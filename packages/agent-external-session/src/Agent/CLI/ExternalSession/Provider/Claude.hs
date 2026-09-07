module Agent.CLI.ExternalSession.Provider.Claude
    ( candidateFromPathClaude
    , discoverClaude
    , findClaudeById
    , readClaude
    ) where

import Agent.CLI.ExternalSession.Content
import Agent.CLI.ExternalSession.JSONL
import Agent.CLI.ExternalSession.Paths
import Agent.CLI.ExternalSession.SQLite
import Agent.CLI.ExternalSession.Types
import Control.Applicative ((<|>))
import Control.Exception.Safe (tryAny)
import Control.Monad (filterM)
import Data.Aeson (Value(..), decodeStrict', encode)
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
    ( IORef
    , modifyIORef'
    , newIORef
    , readIORef
    )
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding
import qualified Data.Vector as Vector
import Database.SQLite3 (Database, SQLData(..))
import System.FilePath (pathSeparator, takeFileName, (</>))

discoverClaude :: ExternalSessionEnv -> FilePath -> IO [ExternalCandidate]
discoverClaude env cwd = do
    canonicalCwd <- canonicalPath cwd
    let projects = env.externalClaudeRoot </> "projects"
        slug = maybe (claudeProjectSlug cwd) claudeProjectSlug canonicalCwd
        direct = projects </> slug
    directPaths <- directoryChildren direct
    paths <-
        if any isClaudeTranscript directPaths
            then pure (filter isClaudeTranscript directPaths)
            else do
                projectDirectories <- directoryChildren projects
                filter isClaudeTranscript . concat
                    <$> traverse directoryChildren projectDirectories
    safePaths <- filterM (isSafeFile projects) paths
    candidates <- mapMaybe id <$> traverse (claudeMetadata env) safePaths
    filterM (\candidate ->
        maybe (pure False) (`samePath` cwd) candidate.candidateCwd)
        candidates

findClaudeById
    :: ExternalSessionEnv
    -> Text
    -> IO (Maybe ExternalCandidate)
findClaudeById env reference = do
    paths <- recursiveFiles
        (env.externalClaudeRoot </> "projects")
        isClaudeTranscript
    firstJustM (claudeMetadata env)
        [ path
        | path <- paths
        , Text.toCaseFold reference
            `Text.isInfixOf`
                Text.toCaseFold (Text.pack (takeFileName path))
        ]

candidateFromPathClaude
    :: ExternalSessionEnv
    -> FilePath
    -> IO (Maybe ExternalCandidate)
candidateFromPathClaude = claudeMetadata

claudeMetadata
    :: ExternalSessionEnv
    -> FilePath
    -> IO (Maybe ExternalCandidate)
claudeMetadata env path
    | not (isClaudeTranscript path) = pure Nothing
    | otherwise = do
        result <- tryAny $
            foldJsonl env path Nothing
                ClaudeMetadataState
                    { metadataCwd = Nothing
                    , metadataSessionId = Text.pack (dropJsonl path)
                    , metadataFirstUser = ""
                    , metadataCustomTitle = ""
                    , metadataAiTitle = ""
                    , metadataSummary = ""
                    , metadataCreated = Nothing
                    , metadataUpdated = Nothing
                    }
                \state record ->
                    pure
                        ( if truthy
                                (externalObjectValue "isSidechain" record)
                            then state
                            else consumeClaudeMetadata record state
                        , JsonlContinue
                        )
        case result of
            Left _ -> pure Nothing
            Right (state, _) -> do
                let title =
                        firstNonEmptyText
                            [ nonEmptyText state.metadataCustomTitle
                            , nonEmptyText state.metadataAiTitle
                            , nonEmptyText state.metadataSummary
                            , nonEmptyText state.metadataFirstUser
                            ]
                Just <$> mkCandidate
                    ExternalClaude
                    "claude-code"
                    state.metadataSessionId
                    path
                    title
                    state.metadataCwd
                    state.metadataCreated
                    state.metadataUpdated

data ClaudeMetadataState = ClaudeMetadataState
    { metadataCwd :: !(Maybe FilePath)
    , metadataSessionId :: !Text
    , metadataFirstUser :: !Text
    , metadataCustomTitle :: !Text
    , metadataAiTitle :: !Text
    , metadataSummary :: !Text
    , metadataCreated :: !(Maybe Value)
    , metadataUpdated :: !(Maybe Value)
    }

consumeClaudeMetadata :: Value -> ClaudeMetadataState -> ClaudeMetadataState
consumeClaudeMetadata record state =
    let recordType = fromMaybe "" (externalTextValue "type" record)
        timestamp = externalObjectValue "timestamp" record
        cwd =
            state.metadataCwd
                <|> (Text.unpack <$> externalTextValue "cwd" record)
        sessionId =
            fromMaybe state.metadataSessionId
                (externalTextValue "sessionId" record
                    >>= nonEmptyText)
        firstUser
            | Text.null state.metadataFirstUser
            , recordType == "user"
            , not (truthy (externalObjectValue "isMeta" record))
            , not (truthy (externalObjectValue "isCompactSummary" record))
            , Just message <- externalObjectValue "message" record
            , Just content <- externalObjectValue "content" message =
                userText (contentText content)
            | otherwise = state.metadataFirstUser
    in state
        { metadataCwd = cwd
        , metadataSessionId = sessionId
        , metadataFirstUser = firstUser
        , metadataCustomTitle =
            if recordType == "custom-title"
                then fromMaybe state.metadataCustomTitle
                    (externalTextValue "customTitle" record)
                else state.metadataCustomTitle
        , metadataAiTitle =
            if recordType == "ai-title"
                then fromMaybe state.metadataAiTitle
                    (externalTextValue "aiTitle" record)
                else state.metadataAiTitle
        , metadataSummary =
            if recordType == "summary"
                then fromMaybe state.metadataSummary
                    (externalTextValue "summary" record)
                else state.metadataSummary
        , metadataCreated = state.metadataCreated <|> timestamp
        , metadataUpdated = timestamp <|> state.metadataUpdated
        }

readClaude
    :: ExternalSessionEnv
    -> ExternalCandidate
    -> Int
    -> IO ExternalSession
readClaude env candidate maxToolChars =
    withTemporaryDatabase
        env.externalScratchDirectory
        "resume-claude-chain.sqlite"
        \database -> do
            initializeClaudeIndex database
            (indexState, counters) <-
                foldJsonl env candidate.candidatePath Nothing
                    (ClaudeIndexState 0 0)
                    \indexState record -> do
                        nextState <-
                            indexClaudeRecord
                                database
                                candidate
                                indexState
                                record
                        pure (nextState, JsonlContinue)
            leaf <- claudeLeaf database
            stateRef <- newIORef ClaudeReadState
                { claudeTurnsFromLeaf = []
                , claudeLastUser = Nothing
                , claudeLastAssistant = Nothing
                , claudeSkipped = indexState.claudeIndexUnindexable
                , claudeOmissions = mempty
                }
            mapM_ (walkClaudeChain database maxToolChars stateRef) leaf
            state <- readIORef stateRef
            let unsafeWarnings =
                    [ warning
                        "unsafe_records_skipped"
                        ("Skipped " <> Text.pack (show state.claudeSkipped)
                            <> " hidden or unsupported record(s).")
                    | state.claudeSkipped > 0
                    ]
                warnings =
                    appendOmissionWarnings state.claudeOmissions
                        (jsonlWarnings counters <> unsafeWarnings)
            pure $
                finaliseSession
                    candidate
                    (reverse state.claudeTurnsFromLeaf)
                    warnings
                    state.claudeLastUser
                    state.claudeLastAssistant

initializeClaudeIndex :: Database -> IO ()
initializeClaudeIndex database = do
    execute database
        "CREATE TABLE claude_messages (uuid TEXT PRIMARY KEY, \
        \sequence INTEGER NOT NULL, parent_uuid TEXT NOT NULL, \
        \sort_time REAL NOT NULL, payload BLOB NOT NULL)"
        []
    execute database
        "CREATE INDEX claude_messages_parent \
        \ON claude_messages (parent_uuid)"
        []
    execute database
        "CREATE TABLE claude_visited (uuid TEXT PRIMARY KEY)"
        []

data ClaudeIndexState = ClaudeIndexState
    { claudeIndexSequence :: !Int
    , claudeIndexUnindexable :: !Int
    }

indexClaudeRecord
    :: Database
    -> ExternalCandidate
    -> ClaudeIndexState
    -> Value
    -> IO ClaudeIndexState
indexClaudeRecord database candidate state record =
    case externalTextValue "uuid" record of
        Just uuid
            | externalTextValue "type" record
                `elem` map Just ["user", "assistant", "system", "attachment"]
            , not (truthy (externalObjectValue "isSidechain" record)) -> do
                let nextSequence = state.claudeIndexSequence + 1
                    parent =
                        firstNonEmptyText
                            [ externalTextValue "parentUuid" record
                            , externalTextValue "logicalParentUuid" record
                            ]
                    sortTime =
                        fromMaybe candidate.candidateSortTime $
                            externalObjectValue "timestamp" record
                                >>= numericTimestampSeconds
                execute database
                    "INSERT INTO claude_messages \
                    \(uuid, sequence, parent_uuid, sort_time, payload) \
                    \VALUES (?, ?, ?, ?, ?) \
                    \ON CONFLICT(uuid) DO UPDATE SET \
                    \sequence = excluded.sequence, \
                    \parent_uuid = excluded.parent_uuid, \
                    \sort_time = excluded.sort_time, \
                    \payload = excluded.payload"
                    [ SQLText uuid
                    , SQLInteger (fromIntegral nextSequence)
                    , SQLText parent
                    , SQLFloat sortTime
                    , SQLBlob (LBS.toStrict (encode record))
                    ]
                pure state
                    { claudeIndexSequence = nextSequence
                    }
        _ -> pure state
  `catchAnyIndex` \_ ->
        pure ClaudeIndexState
            { claudeIndexSequence = state.claudeIndexSequence + 1
            , claudeIndexUnindexable =
                state.claudeIndexUnindexable + 1
            }

catchAnyIndex :: IO value -> (Text -> IO value) -> IO value
catchAnyIndex action handle =
    tryAny action >>= \case
        Left exception -> handle (Text.pack (show exception))
        Right value -> pure value

claudeLeaf :: Database -> IO (Maybe Text)
claudeLeaf database = do
    rows <- queryRows database
        "SELECT messages.uuid FROM claude_messages AS messages \
        \WHERE NOT EXISTS (SELECT 1 FROM claude_messages AS children \
        \WHERE children.parent_uuid = messages.uuid) \
        \ORDER BY messages.sort_time DESC, messages.sequence DESC LIMIT 1"
        []
    case rows of
        ([value] : _) -> pure (nonEmptyText (sqlDataText value))
        _ -> do
            fallback <- queryRows database
                "SELECT uuid FROM claude_messages \
                \ORDER BY sort_time DESC, sequence DESC LIMIT 1"
                []
            pure $ case fallback of
                ([value] : _) -> nonEmptyText (sqlDataText value)
                _ -> Nothing

data ClaudeReadState = ClaudeReadState
    { claudeTurnsFromLeaf :: ![ExternalTurn]
    , claudeLastUser :: !(Maybe Text)
    , claudeLastAssistant :: !(Maybe Text)
    , claudeSkipped :: !Int
    , claudeOmissions :: !ContentOmissions
    }

walkClaudeChain
    :: Database
    -> Int
    -> IORef ClaudeReadState
    -> Text
    -> IO ()
walkClaudeChain database maxToolChars stateRef = go
  where
    go uuid
        | Text.null uuid = pure ()
        | otherwise = do
            execute database
                "INSERT OR IGNORE INTO claude_visited (uuid) VALUES (?)"
                [SQLText uuid]
            changed <- queryRows database "SELECT changes()" []
            case changed of
                ([SQLInteger 1] : _) -> do
                    rows <- queryRows database
                        "SELECT parent_uuid, payload FROM claude_messages \
                        \WHERE uuid = ?"
                        [SQLText uuid]
                    case rows of
                        ([parent, payload] : _) -> do
                            case decodePayload payload of
                                Nothing ->
                                    modifyIORef' stateRef \state ->
                                        state
                                            { claudeSkipped =
                                                state.claudeSkipped + 1
                                            }
                                Just record ->
                                    modifyIORef' stateRef
                                        (addClaudeTurn maxToolChars record)
                            go (sqlDataText parent)
                        _ -> pure ()
                _ -> pure ()

decodePayload :: SQLData -> Maybe Value
decodePayload = \case
    SQLBlob bytes -> decodeStrict' bytes
    SQLText text -> decodeStrict' (Data.Text.Encoding.encodeUtf8 text)
    _ -> Nothing

addClaudeTurn :: Int -> Value -> ClaudeReadState -> ClaudeReadState
addClaudeTurn maxToolChars record state =
    let (turn, skipped, omissions) = claudeTurn maxToolChars record
        turns =
            case turn of
                Just value
                    | length state.claudeTurnsFromLeaf
                        < maxExternalTurns + 1 ->
                            state.claudeTurnsFromLeaf <> [value]
                _ -> state.claudeTurnsFromLeaf
        updateLast role old =
            old <|> do
                value <- turn
                if value.externalTurnRole == role
                        && not (Text.null value.externalTurnText)
                    then Just value.externalTurnText
                    else Nothing
    in state
        { claudeTurnsFromLeaf = turns
        , claudeLastUser = updateLast "user" state.claudeLastUser
        , claudeLastAssistant =
            updateLast "assistant" state.claudeLastAssistant
        , claudeSkipped = state.claudeSkipped + skipped
        , claudeOmissions = state.claudeOmissions <> omissions
        }

claudeTurn
    :: Int
    -> Value
    -> (Maybe ExternalTurn, Int, ContentOmissions)
claudeTurn maxToolChars record
    | truthy (externalObjectValue "isMeta" record)
        || truthy (externalObjectValue "isCompactSummary" record) =
            (Nothing, 1, mempty)
    | role `notElem` ["user", "assistant"] = (Nothing, 1, mempty)
    | otherwise =
        case externalObjectValue "message" record of
            Just message@Object{} ->
                let content = fromMaybe Null
                        (externalObjectValue "content" message)
                    (text, initialOmissions) =
                        contentTextWithOmissions content
                    (calls, results, skipped, resultOmissions) =
                        parseClaudeBlocks maxToolChars content
                in
                    ( inertTurn role text calls results
                    , skipped
                    , initialOmissions <> resultOmissions
                    )
            _ -> (Nothing, 1, mempty)
  where
    role = Text.toLower (fromMaybe "" (externalTextValue "type" record))

parseClaudeBlocks
    :: Int
    -> Value
    -> ( [HistoricalToolCall]
       , [HistoricalToolResult]
       , Int
       , ContentOmissions
       )
parseClaudeBlocks maxToolChars = \case
    Array values ->
        foldl consume ([], [], 0, mempty) (Vector.toList values)
    _ -> ([], [], 0, mempty)
  where
    consume state@(calls, results, skipped, omissions) block =
        case externalTextValue "type" block of
            Just "tool_use" ->
                let call = HistoricalToolCall
                        { historicalCallId =
                            firstNonEmptyText
                                [ externalTextValue "id" block
                                , externalTextValue "tool_use_id" block
                                ]
                        , historicalCallName =
                            fromMaybe "" (externalTextValue "name" block)
                        , historicalCallArguments =
                            jsonPreview maxToolChars $
                                fromMaybe Null
                                    (externalObjectValue "input" block)
                        }
                in (calls <> [call], results, skipped, omissions)
            Just "tool_result" ->
                let (output, resultOmissions) =
                        toolResultContent $
                            fromMaybe Null
                                (externalObjectValue "content" block)
                    result = HistoricalToolResult
                        { historicalResultCallId =
                            fromMaybe ""
                                (externalTextValue "tool_use_id" block)
                        , historicalResultOutput =
                            historicalToolResult maxToolChars output
                        }
                in
                    ( calls
                    , results <> [result]
                    , skipped
                    , omissions <> resultOmissions
                    )
            Just kind
                | kind `elem` ["thinking", "redacted_thinking"] ->
                    (calls, results, skipped + 1, omissions)
            _ -> state

claudeProjectSlug :: FilePath -> FilePath
claudeProjectSlug =
    map (\character -> if character == pathSeparator then '-' else character)

dropJsonl :: FilePath -> FilePath
dropJsonl path =
    fromMaybe (takeFileName path) $
        Text.unpack <$> Text.stripSuffix ".jsonl" (Text.pack (takeFileName path))

isClaudeTranscript :: FilePath -> Bool
isClaudeTranscript =
    Text.isSuffixOf ".jsonl" . Text.pack

truthy :: Maybe Value -> Bool
truthy = \case
    Nothing -> False
    Just Null -> False
    Just (Bool False) -> False
    Just (String value) -> not (Text.null value)
    Just (Array values) -> not (Vector.null values)
    Just (Object values) -> not (null values)
    Just _ -> True

firstNonEmptyText :: [Maybe Text] -> Text
firstNonEmptyText values =
    fromMaybe "" $ firstMaybe
        [ value
        | Just value <- values
        , not (Text.null value)
        ]

nonEmptyText :: Text -> Maybe Text
nonEmptyText value
    | Text.null value = Nothing
    | otherwise = Just value

firstMaybe :: [value] -> Maybe value
firstMaybe [] = Nothing
firstMaybe (value : _) = Just value

firstJustM
    :: (input -> IO (Maybe output))
    -> [input]
    -> IO (Maybe output)
firstJustM _ [] = pure Nothing
firstJustM action (value : values) =
    action value >>= \case
        Just output -> pure (Just output)
        Nothing -> firstJustM action values
