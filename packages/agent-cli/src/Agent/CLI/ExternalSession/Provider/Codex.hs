module Agent.CLI.ExternalSession.Provider.Codex
    ( candidateFromPathCodex
    , discoverCodex
    , findCodexById
    , readCodex
    ) where

import Agent.CLI.ExternalSession.Content
import Agent.CLI.ExternalSession.JSONL
import Agent.CLI.ExternalSession.Paths
import Agent.CLI.ExternalSession.SQLite
import Agent.CLI.ExternalSession.Types
import Control.Applicative ((<|>))
import Control.Exception.Safe (tryAny)
import Control.Monad (filterM, when)
import Data.Aeson (Value(..), decodeStrict', encode)
import qualified Data.Aeson.Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
    ( modifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.List (maximumBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Scientific (fromFloatDigits)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Database.SQLite3 (Database, SQLData(..))
import Data.Scientific (floatingOrInteger)
import qualified Data.Text.Encoding
import Data.Text.Encoding.Error (lenientDecode)
import System.Directory (doesFileExist)
import System.FilePath
    ( isAbsolute
    , takeFileName
    , (</>)
    )
import Text.Read (readMaybe)

discoverCodex :: ExternalSessionEnv -> FilePath -> IO [ExternalCandidate]
discoverCodex env cwd = do
    database <- newestStateDatabase env.externalCodexRoot
    case database of
        Just path -> discoverFromDatabase path >>= \case
            Just candidates -> pure candidates
            Nothing -> discoverFromFiles
        Nothing -> discoverFromFiles
  where
    discoverFromDatabase databasePath = do
        result <- tryAny $
            withReadOnlyDatabase databasePath \database -> do
                columns <- tableColumns database "threads"
                let required =
                        Set.fromList ["id", "rollout_path", "source", "cwd",
                                      "archived"]
                if not (required `Set.isSubsetOf` columns)
                    then pure Nothing
                    else do
                        let fixedColumn name fallback =
                                if name `Set.member` columns then name else fallback
                            titleExpression = fixedColumn "title" "''"
                            firstExpression =
                                fixedColumn "first_user_message" "''"
                            createdExpression =
                                fixedColumn "created_at" "NULL"
                            updatedExpression
                                | "updated_at_ms" `Set.member` columns =
                                    "updated_at_ms"
                                | "updated_at" `Set.member` columns =
                                    "updated_at"
                                | otherwise = "NULL"
                            sql =
                                "SELECT id, rollout_path, "
                                    <> createdExpression <> ", "
                                    <> updatedExpression <> ", source, cwd, "
                                    <> titleExpression <> ", "
                                    <> firstExpression
                                    <> " FROM threads WHERE archived = 0 "
                                    <> "AND source IN ('cli', 'vscode') "
                                    <> "ORDER BY " <> updatedExpression
                                    <> " DESC, id ASC"
                        rows <- queryRows database sql []
                        Just . mapMaybe id <$> traverse
                            (candidateFromDatabaseRow env cwd)
                            rows
        pure (either (const Nothing) id result)
    discoverFromFiles = do
        paths <- recursiveFiles
            (env.externalCodexRoot </> "sessions")
            isCodexTranscript
        candidates <- mapMaybe id <$> traverse (codexMetadata env) paths
        filterM (\candidate ->
            if candidate.candidateSource
                    `notElem` ["codex-cli", "codex-vscode"]
                then pure False
                else maybe (pure False) (`samePath` cwd)
                    candidate.candidateCwd)
            candidates

candidateFromDatabaseRow
    :: ExternalSessionEnv
    -> FilePath
    -> [SQLData]
    -> IO (Maybe ExternalCandidate)
candidateFromDatabaseRow env cwd row =
    case row of
        [sid, rawPath, created, updated, source, rowCwd, title, first] -> do
            cwdMatches <- samePath (Text.unpack (sqlDataText rowCwd)) cwd
            let sourceText = sqlDataText source
            if not cwdMatches || sourceText `notElem` ["cli", "vscode"]
                then pure Nothing
                else do
                    let storedPath = Text.unpack (sqlDataText rawPath)
                        initialPath =
                            if isAbsolute storedPath
                                then storedPath
                                else env.externalCodexRoot </> storedPath
                    plainExists <- doesFileExist initialPath
                    compressedExists <- doesFileExist (initialPath <> ".zst")
                    let rollout
                            | plainExists = initialPath
                            | ".jsonl" `Text.isSuffixOf` Text.pack initialPath
                                && compressedExists = initialPath <> ".zst"
                            | otherwise = initialPath
                    safe <- isSafeFile env.externalCodexRoot rollout
                    if not safe
                        then pure Nothing
                        else Just <$> mkCandidate
                            ExternalCodex
                            ("codex-" <> sourceText)
                            (sqlDataText sid)
                            rollout
                            ( userText $
                                firstNonEmptyText
                                    [nonEmptyText (sqlDataText title),
                                     nonEmptyText (sqlDataText first)]
                            )
                            (Just (Text.unpack (sqlDataText rowCwd)))
                            (sqlDataValue created)
                            (sqlDataValue updated)
        _ -> pure Nothing

newestStateDatabase :: FilePath -> IO (Maybe FilePath)
newestStateDatabase root = do
    direct <- directoryChildren root
    nested <- directoryChildren (root </> "sqlite")
    let numbered = mapMaybe stateNumber (direct <> nested)
    pure $ snd <$> nonEmptyMaximum numbered
  where
    stateNumber path = do
        name <- Text.stripPrefix "state_" (Text.pack (takeFileName path))
        numberText <- Text.stripSuffix ".sqlite" name
        number <- readMaybe (Text.unpack numberText)
        pure (number :: Integer, path)
    nonEmptyMaximum [] = Nothing
    nonEmptyMaximum values = Just (maximumBy (comparing fst) values)

candidateFromPathCodex
    :: ExternalSessionEnv
    -> FilePath
    -> IO (Maybe ExternalCandidate)
candidateFromPathCodex = codexMetadata

findCodexById
    :: ExternalSessionEnv
    -> Text
    -> IO (Maybe ExternalCandidate)
findCodexById env reference = do
    paths <- recursiveFiles env.externalCodexRoot isCodexTranscript
    let matching =
            filter
                (Text.isInfixOf (Text.toCaseFold reference)
                    . Text.toCaseFold
                    . Text.pack
                    . takeFileName)
                paths
    firstJustM (codexMetadata env) matching

codexMetadata
    :: ExternalSessionEnv
    -> FilePath
    -> IO (Maybe ExternalCandidate)
codexMetadata env path
    | not (isCodexTranscript path) = pure Nothing
    | otherwise = do
        result <- tryAny $ readJsonlValues env path (Just 300)
        case result of
            Left _ -> pure Nothing
            Right (records, _) -> do
                let metadata =
                        fromMaybe (Object mempty) $
                            lastMaybe
                                [ payload
                                | record <- records
                                , externalTextValue "type" record
                                    == Just "session_meta"
                                , Just payload@Object{} <-
                                    [externalObjectValue "payload" record]
                                ]
                    firstUser =
                        fromMaybe "" $
                            firstMaybe
                                [ userText text
                                | record <- records
                                , externalTextValue "type" record
                                    == Just "response_item"
                                , Just payload <-
                                    [externalObjectValue "payload" record]
                                , externalTextValue "type" payload
                                    == Just "message"
                                , externalTextValue "role" payload
                                    == Just "user"
                                , Just content <-
                                    [externalObjectValue "content" payload]
                                , let text = contentText content
                                , not (Text.null (userText text))
                                ]
                    fallbackId =
                        Text.pack $
                            dropCodexExtensions (takeFileName path)
                    sessionId =
                        firstNonEmptyText
                            [ externalTextValue "id" metadata
                            , externalTextValue "session_id" metadata
                            , Just fallbackId
                            ]
                    source =
                        fromMaybe "cli" (externalTextValue "source" metadata)
                    metadataCwd =
                        Text.unpack <$> externalTextValue "cwd" metadata
                Just <$> mkCandidate
                    ExternalCodex
                    ("codex-" <> source)
                    sessionId
                    path
                    firstUser
                    metadataCwd
                    (externalObjectValue "timestamp" metadata)
                    Nothing

dropCodexExtensions :: FilePath -> FilePath
dropCodexExtensions name =
    fromMaybe name $
        Text.unpack
            <$> ( Text.stripSuffix ".jsonl.zst" (Text.pack name)
                    <|> Text.stripSuffix ".jsonl" (Text.pack name)
                )

isCodexTranscript :: FilePath -> Bool
isCodexTranscript path =
    ".jsonl" `Text.isSuffixOf` name
        || ".jsonl.zst" `Text.isSuffixOf` name
  where
    name = Text.pack path

readCodex
    :: ExternalSessionEnv
    -> ExternalCandidate
    -> Int
    -> IO ExternalSession
readCodex env candidate maxToolChars = do
    stateRef <- newIORef emptyBoundedTurns
    let boundedSink = TurnSink
            { sinkClear = writeIORef stateRef emptyBoundedTurns
            , sinkAppend = \turn ->
                modifyIORef' stateRef (appendBoundedTurn turn)
            , sinkDropUsers = \number -> do
                state <- readIORef stateRef
                case dropLastUserTurns number state of
                    Nothing -> pure False
                    Just updated -> writeIORef stateRef updated >> pure True
            }
    firstPass <- processCodex env candidate.candidatePath
        maxToolChars boundedSink
    bounded <- readIORef stateRef
    if firstPass.processedNeedsJournal
        then withTemporaryDatabase
            env.externalScratchDirectory
            "resume-codex-turns.sqlite"
            \database -> do
                initializeCodexJournal database
                secondPass <- processCodex env candidate.candidatePath
                    maxToolChars (journalSink database)
                turns <- journalRecent database
                lastUser <- journalLastText database "user"
                lastAssistant <- journalLastText database "assistant"
                pure $
                    finishCodex candidate secondPass turns
                        lastUser lastAssistant
        else
            pure $
                finishCodex
                    candidate
                    firstPass
                    (boundedRecent bounded)
                    (boundedLastText "user" bounded)
                    (boundedLastText "assistant" bounded)

data TurnSink = TurnSink
    { sinkClear :: !(IO ())
    , sinkAppend :: !(ExternalTurn -> IO ())
    , sinkDropUsers :: !(Int -> IO Bool)
    }

data CodexProcessed = CodexProcessed
    { processedJsonl :: !JsonlCounters
    , processedSkipped :: !Int
    , processedOmissions :: !ContentOmissions
    , processedNeedsJournal :: !Bool
    }

processCodex
    :: ExternalSessionEnv
    -> FilePath
    -> Int
    -> TurnSink
    -> IO CodexProcessed
processCodex env path maxToolChars sink = do
    stateRef <- newIORef (0, mempty, False)
    counters <- consumeJsonl env path Nothing \record -> do
        (_, _, needsJournal) <- readIORef stateRef
        let recordType = externalTextValue "type" record
            payload = externalObjectValue "payload" record
        case recordType of
            Just "compacted" ->
                case payload >>= externalObjectValue "replacement_history" of
                    Just (Array values) -> do
                        sink.sinkClear
                        writeIORef stateRef (0, mempty, False)
                        Vector.forM_ values \replacement -> do
                            let (turn, unsafe, replacementOmissions) =
                                    codexTurn maxToolChars replacement
                            modifyIORef' stateRef
                                (\(count, total, _) ->
                                    ( count + if unsafe then 1 else 0
                                    , total <> replacementOmissions
                                    , False
                                    ))
                            mapM_ sink.sinkAppend turn
                    _ -> pure ()
            _
                | needsJournal -> pure ()
            Just "response_item" -> case payload of
                Just payloadValue -> do
                    let (turn, unsafe, recordOmissions) =
                            codexTurn maxToolChars payloadValue
                    modifyIORef' stateRef
                        (\(count, total, required) ->
                            ( count + if unsafe then 1 else 0
                            , total <> recordOmissions
                            , required
                            ))
                    mapM_ sink.sinkAppend turn
                Nothing -> modifyIORef' stateRef
                    (\(count, total, required) ->
                        (count + 1, total, required))
            Just "event_msg"
                | Just event <- payload
                , externalTextValue "type" event
                    == Just "thread_rolled_back" -> do
                    let number = fromMaybe 0 $
                            externalObjectValue "num_turns" event
                                >>= integralValue
                    applied <- sink.sinkDropUsers number
                    when (not applied) $
                        modifyIORef' stateRef
                            (\(count, total, _) ->
                                (count, total, True))
            Just "session_meta" -> pure ()
            Just "event_msg" -> pure ()
            _ -> modifyIORef' stateRef
                (\(count, total, required) ->
                    (count + 1, total, required))
        pure JsonlContinue
    (skipped, omissions, needsJournal) <- readIORef stateRef
    pure CodexProcessed
        { processedJsonl = counters
        , processedSkipped = skipped
        , processedOmissions = omissions
        , processedNeedsJournal = needsJournal
        }

codexTurn
    :: Int
    -> Value
    -> (Maybe ExternalTurn, Bool, ContentOmissions)
codexTurn maxToolChars payload =
    case externalTextValue "type" payload of
        Just "message" ->
            let role = fromMaybe "" (externalTextValue "role" payload)
                (text, omissions) =
                    maybe ("", mempty) contentTextWithOmissions
                        (externalObjectValue "content" payload)
            in (inertTurn role text [] [], role `notElem` ["user", "assistant"],
                omissions)
        Just "local_shell_call" ->
            callOnly "local_shell" ["action"]
        Just "function_call" ->
            callOnlyNamed "function_call" ["arguments", "input"]
        Just "custom_tool_call" ->
            callOnlyNamed "custom_tool_call" ["arguments", "input"]
        Just "computer_call" ->
            callOnly "computer" ["actions", "action"]
        Just "mcp_call" ->
            let callId = protocolCallId payload
                arguments =
                    Object $ KeyMap.fromList
                        [ (key, value)
                        | name <- ["server_label", "name", "arguments"]
                        , Just value <- [externalObjectValue name payload]
                        , let key = Data.Aeson.Key.fromText name
                        ]
                call = HistoricalToolCall callId "mcp_call"
                    (jsonPreview maxToolChars arguments)
                rawResult =
                    firstMaybe
                        [ if key == "error"
                            then Object
                                (KeyMap.singleton
                                    (Data.Aeson.Key.fromText "error")
                                    value)
                            else value
                        | key <- ["result", "output", "error"]
                        , Just value <- [externalObjectValue key payload]
                        , value /= Null
                        ]
                (results, omissions) = case rawResult of
                    Nothing -> ([], mempty)
                    Just value ->
                        let (sanitized, resultOmissions) =
                                toolResultContent value
                        in
                            ( [ HistoricalToolResult callId
                                    (historicalToolResult
                                        maxToolChars sanitized)
                              ]
                            , resultOmissions
                            )
            in (inertTurn "assistant" "" [call] results, False, omissions)
        Just kind
            | kind `elem`
                [ "shell_call"
                , "apply_patch_call"
                , "tool_search_call"
                , "mcp_approval_request"
                , "program"
                ] ->
                    callOnly kind
                        [ "action"
                        , "arguments"
                        , "input"
                        , "command"
                        , "patch"
                        , "request"
                        ]
            | kind `elem`
                [ "function_call_output"
                , "custom_tool_call_output"
                , "computer_call_output"
                , "local_shell_call_output"
                , "shell_call_output"
                , "apply_patch_call_output"
                , "tool_search_output"
                , "mcp_approval_response"
                , "program_output"
                ] ->
                    resultOnly
        _ -> (Nothing, True, mempty)
  where
    callOnly name keys =
        let arguments =
                fromMaybe payload
                    (firstMaybe (mapMaybe (`externalObjectValue` payload) keys))
            call = HistoricalToolCall
                (protocolCallId payload)
                name
                (jsonPreview maxToolChars arguments)
        in (inertTurn "assistant" "" [call] [], False, mempty)
    callOnlyNamed fallback keys =
        callOnly
            (fromMaybe fallback (externalTextValue "name" payload))
            keys
    resultOnly =
        let rawOutput =
                fromMaybe payload $
                    firstMaybe $
                        mapMaybe
                            (`externalObjectValue` payload)
                            ["output", "result", "tools", "response", "content"]
            (sanitized, omissions) = toolResultContent rawOutput
            result = HistoricalToolResult
                (protocolCallId payload)
                (historicalToolResult maxToolChars sanitized)
        in (inertTurn "assistant" "" [] [result], False, omissions)

finishCodex
    :: ExternalCandidate
    -> CodexProcessed
    -> [ExternalTurn]
    -> Maybe Text
    -> Maybe Text
    -> ExternalSession
finishCodex candidate processed turns lastUser lastAssistant =
    let unsafeWarnings =
            [ warning
                "unsafe_records_skipped"
                ("Skipped " <> Text.pack (show processed.processedSkipped)
                    <> " instruction, reasoning, or unsupported record(s).")
            | processed.processedSkipped > 0
            ]
        warnings =
            appendOmissionWarnings processed.processedOmissions
                (jsonlWarnings processed.processedJsonl <> unsafeWarnings)
    in finaliseSession candidate turns warnings lastUser lastAssistant

initializeCodexJournal :: Database -> IO ()
initializeCodexJournal database = do
    execute database
        "CREATE TABLE turns (sequence INTEGER PRIMARY KEY, role TEXT NOT NULL, \
        \text TEXT NOT NULL, payload BLOB NOT NULL)"
        []
    execute database
        "CREATE INDEX turns_role_sequence ON turns (role, sequence)"
        []

journalSink :: Database -> TurnSink
journalSink database = TurnSink
    { sinkClear = execute database "DELETE FROM turns" []
    , sinkAppend = \turn ->
        execute database
            "INSERT INTO turns (role, text, payload) VALUES (?, ?, ?)"
            [ SQLText turn.externalTurnRole
            , SQLText turn.externalTurnText
            , SQLBlob (LBS.toStrict (encode turn))
            ]
    , sinkDropUsers = \number -> do
        when (number > 0) do
            rows <- queryRows database
                "SELECT sequence FROM turns WHERE role = 'user' \
                \ORDER BY sequence DESC LIMIT ?"
                [SQLInteger (fromIntegral number)]
            case reverse rows of
                ([sequence] : _) ->
                    execute database
                        "DELETE FROM turns WHERE sequence >= ?"
                        [sequence]
                _ -> pure ()
        pure True
    }

journalRecent :: Database -> IO [ExternalTurn]
journalRecent database = do
    rows <- queryRows database
        "SELECT payload FROM (SELECT sequence, payload FROM turns \
        \ORDER BY sequence DESC LIMIT ?) ORDER BY sequence ASC"
        [SQLInteger (fromIntegral (maxExternalTurns + 1))]
    pure $
        mapMaybe
            (\case
                [SQLBlob bytes] -> decodeStrict' bytes
                [SQLText text] ->
                    decodeStrict' (Data.Text.Encoding.encodeUtf8 text)
                _ -> Nothing)
            rows

journalLastText :: Database -> Text -> IO (Maybe Text)
journalLastText database role = do
    rows <- queryRows database
        "SELECT text FROM turns WHERE role = ? AND length(text) > 0 \
        \ORDER BY sequence DESC LIMIT 1"
        [SQLText role]
    pure $ case rows of
        ([value] : _) -> nonEmptyText (sqlDataText value)
        _ -> Nothing

integralValue :: Value -> Maybe Int
integralValue (Number number) =
    case floatingOrInteger number of
        Right integer -> Just integer
        Left (_ :: Double) -> Nothing
integralValue _ = Nothing

sqlDataValue :: SQLData -> Maybe Value
sqlDataValue = \case
    SQLInteger integer -> Just (Number (fromIntegral integer))
    SQLFloat number -> Just (Number (fromFloatDigits number))
    SQLText text -> nonEmptyText text >>= Just . String
    SQLBlob bytes -> Just (String (Data.Text.Encoding.decodeUtf8With
        lenientDecode bytes))
    SQLNull -> Nothing

nonEmptyText :: Text -> Maybe Text
nonEmptyText text
    | Text.null text = Nothing
    | otherwise = Just text

firstNonEmptyText :: [Maybe Text] -> Text
firstNonEmptyText values =
    fromMaybe "" $ firstMaybe
        [ value
        | Just value <- values
        , not (Text.null value)
        ]

firstMaybe :: [value] -> Maybe value
firstMaybe [] = Nothing
firstMaybe (value : _) = Just value

lastMaybe :: [value] -> Maybe value
lastMaybe [] = Nothing
lastMaybe values = Just (last values)

firstJustM
    :: (input -> IO (Maybe output))
    -> [input]
    -> IO (Maybe output)
firstJustM _ [] = pure Nothing
firstJustM action (value : values) =
    action value >>= \case
        Just output -> pure (Just output)
        Nothing -> firstJustM action values
