module Agent.CLI.ExternalSession.Provider.Cursor
    ( candidateFromPathCursor
    , discoverCursor
    , findCursorById
    , readCursor
    ) where

import Agent.CLI.ExternalSession.Content
import Agent.CLI.ExternalSession.JSONL
import Agent.CLI.ExternalSession.Paths
import Agent.CLI.ExternalSession.SQLite
import Agent.CLI.ExternalSession.Types
import Control.Applicative ((<|>))
import Control.Exception.Safe (IOException, tryAny, tryIO)
import Control.Monad (filterM, foldM)
import Crypto.Hash (Digest, MD5, hash)
import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.IORef as IORef
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Vector as Vector
import Database.SQLite3 (Database, SQLData(..))
import System.Directory
    ( doesDirectoryExist
    , doesFileExist
    , pathIsSymbolicLink
    )
import System.FilePath
    ( makeRelative
    , splitDirectories
    , takeDirectory
    , takeExtension
    , takeFileName
    , (</>)
    )

discoverCursor :: ExternalSessionEnv -> FilePath -> IO [ExternalCandidate]
discoverCursor env cwd = do
    canonicalCwd <- fromMaybe cwd <$> canonicalPath cwd
    let digest = show
            (hash (encodeUtf8 (Text.pack canonicalCwd)) :: Digest MD5)
        chats = env.externalCursorRoot </> "chats" </> digest
    cliDirectories <- directoryChildren chats
    cli <- mapMaybe id <$> traverse
        (\directory -> cursorCliCandidate env directory (Just cwd))
        cliDirectories
    desktop <- concat <$> traverse (discoverDesktop env cwd)
        env.externalCursorDesktopStores
    transcripts <- discoverCursorTranscripts env cwd
    pure (cli <> desktop <> transcripts)

discoverDesktop
    :: ExternalSessionEnv
    -> FilePath
    -> FilePath
    -> IO [ExternalCandidate]
discoverDesktop _env cwd databasePath = do
    safe <- existingNonSymlinkFile databasePath
    if not safe
        then pure []
        else tryAny
            (withReadOnlyDatabase databasePath \database -> do
                headers <- cursorHeaderValues database
                mapMaybe id <$> traverse (headerCandidate databasePath) headers)
            >>= pure . either (const []) id
  where
    headerCandidate databasePath header
        | truthy (externalObjectValue "isDraft" header) = pure Nothing
        | otherwise = do
            let paths = nestedStrings cursorCwdKeys header
            pathMatches <- or <$> traverse (`samePath` cwd)
                (map Text.unpack paths)
            if null paths || not pathMatches
                then pure Nothing
                else case externalTextValue "composerId" header of
                    Nothing -> pure Nothing
                    Just sessionId
                        | Text.null sessionId -> pure Nothing
                        | otherwise ->
                            Just <$> mkCandidate
                                ExternalCursor
                                "cursor-desktop"
                                sessionId
                                databasePath
                                ( firstNonEmptyText
                                    [ externalTextValue "name" header
                                    , externalTextValue "title" header
                                    ]
                                )
                                (Just cwd)
                                (externalObjectValue "createdAt" header)
                                (externalObjectValue "lastUpdatedAt" header)

cursorHeaderValues :: Database -> IO [Value]
cursorHeaderValues database = do
    columns <- tableColumns database "composerHeaders"
    if Set.fromList ["composerId", "value"] `Set.isSubsetOf` columns
        then do
            let updated =
                    if "lastUpdatedAt" `Set.member` columns
                        then "lastUpdatedAt"
                        else "NULL"
                archived =
                    if "isArchived" `Set.member` columns
                        then "COALESCE(isArchived, 0) = 0"
                        else "1"
                subagent =
                    if "isSubagent" `Set.member` columns
                        then "COALESCE(isSubagent, 0) = 0"
                        else "1"
                sql =
                    "SELECT composerId, " <> updated
                        <> ", value FROM composerHeaders WHERE "
                        <> archived <> " AND " <> subagent
            rows <- queryRows database sql []
            pure $
                [ let decoded = fromMaybe (Object mempty) (decodeJsonish raw)
                  in insertDefaults
                        [ ("composerId", String (sqlDataText sid))
                        , ("lastUpdatedAt", sqlDataToValue rawUpdated)
                        ]
                        decoded
                | [sid, rawUpdated, raw] <- rows
                ]
        else do
            result <- tryAny $
                queryRows database
                    "SELECT value FROM ItemTable \
                    \WHERE key = 'composer.composerHeaders'"
                    []
            pure $ case result of
                Right ([raw] : _) ->
                    case decodeJsonish raw
                        >>= externalObjectValue "allComposers" of
                        Just (Array values) ->
                            [value | value@Object{} <- Vector.toList values]
                        _ -> []
                _ -> []

insertDefaults :: [(Text, Value)] -> Value -> Value
insertDefaults defaults (Object objectValue) =
    Object $ foldl
        (\current (key, value) ->
            KeyMap.insertWith (\_ old -> old) (Key.fromText key) value current)
        objectValue
        defaults
insertDefaults _ _ = Object mempty

discoverCursorTranscripts
    :: ExternalSessionEnv
    -> FilePath
    -> IO [ExternalCandidate]
discoverCursorTranscripts env cwd = do
    let projects = env.externalCursorRoot </> "projects"
    paths <- recursiveFiles projects
        (Text.isSuffixOf ".jsonl" . Text.pack)
    mapMaybe id <$> traverse (cursorTranscriptCandidate env cwd) paths

cursorTranscriptCandidate
    :: ExternalSessionEnv
    -> FilePath
    -> FilePath
    -> IO (Maybe ExternalCandidate)
cursorTranscriptCandidate env cwd path = do
    canonicalRoot <- canonicalPath (env.externalCursorRoot </> "projects")
    canonicalTranscript <- canonicalPath path
    case (canonicalRoot, canonicalTranscript) of
        (Just root, Just transcript) -> do
            let relative = splitDirectories (makeRelative root transcript)
            case relative of
                project : "agent-transcripts" : _session : _ -> do
                    matches <- cursorProjectMatchesCwd env
                        (root </> project) cwd
                    if not matches
                        then pure Nothing
                        else do
                            title <- cursorTranscriptTitle env transcript
                            Just <$> mkCandidate
                                ExternalCursor
                                "cursor-transcript"
                                (Text.pack
                                    (dropJsonl (takeFileName transcript)))
                                transcript
                                (fromMaybe "" title)
                                (Just cwd)
                                Nothing
                                Nothing
                _ -> pure Nothing
        _ -> pure Nothing

cursorTranscriptTitle
    :: ExternalSessionEnv
    -> FilePath
    -> IO (Maybe Text)
cursorTranscriptTitle env path = do
    titleRef <- newIORef Nothing
    _ <- tryAny $
        consumeJsonl env path Nothing \record -> do
            case cursorFirstUserTitle record of
                Just title -> do
                    modifyIORef' titleRef (const (Just title))
                    pure JsonlStop
                Nothing -> pure JsonlContinue
    readIORef titleRef

cursorProjectMatchesCwd
    :: ExternalSessionEnv
    -> FilePath
    -> FilePath
    -> IO Bool
cursorProjectMatchesCwd env projectDirectory cwd = do
    safe <- isSafeDirectory (env.externalCursorRoot </> "projects")
        projectDirectory
    if not safe
        then pure False
        else do
            let metadataPath = projectDirectory </> ".workspace-trusted"
            metadataSafe <- isSafeFile projectDirectory metadataPath
            metadata <-
                if metadataSafe
                    then readJsonFileValue metadataPath
                    else pure Nothing
            case metadata >>= externalTextValue "workspacePath" of
                Just workspace | not (Text.null (Text.strip workspace)) ->
                    samePath (Text.unpack (Text.strip workspace)) cwd
                _ ->
                    pure $
                        Text.toCaseFold
                            (Text.pack (takeFileName projectDirectory))
                            == Text.toCaseFold
                                (Text.pack (cursorProjectSlug cwd))

cursorProjectSlug :: FilePath -> FilePath
cursorProjectSlug =
    trimDashes . map (\character ->
        if isAsciiAlphaNumeric character then character else '-')
  where
    trimDashes = reverse . dropWhile (== '-') . reverse . dropWhile (== '-')
    isAsciiAlphaNumeric character =
        character >= 'A' && character <= 'Z'
            || character >= 'a' && character <= 'z'
            || character >= '0' && character <= '9'

cursorCliCandidate
    :: ExternalSessionEnv
    -> FilePath
    -> Maybe FilePath
    -> IO (Maybe ExternalCandidate)
cursorCliCandidate env =
    cursorCliCandidateWithin (env.externalCursorRoot </> "chats")

cursorCliCandidateWithin
    :: FilePath
    -> FilePath
    -> Maybe FilePath
    -> IO (Maybe ExternalCandidate)
cursorCliCandidateWithin root directory fallbackCwd = do
    safe <- isSafeDirectory root directory
    if not safe
        then pure Nothing
        else do
            let metadataPath = directory </> "meta.json"
            metadataSafe <- isSafeFile directory metadataPath
            metadata <-
                if metadataSafe
                    then readJsonFileValue metadataPath
                    else pure Nothing
            let objectValue = fromMaybe (Object mempty) metadata
                metadataCwd =
                    Text.unpack <$> externalTextValue "cwd" objectValue
            validCwd <- case (fallbackCwd, metadataCwd) of
                (Just expected, Just actual) -> samePath actual expected
                _ -> pure True
            if not validCwd
                then pure Nothing
                else Just <$> mkCandidate
                    ExternalCursor
                    "cursor-cli"
                    ( firstNonEmptyText
                        [ externalTextValue "id" objectValue
                        , Just (Text.pack (takeFileName directory))
                        ]
                    )
                    directory
                    ( firstNonEmptyText
                        [ externalTextValue "name" objectValue
                        , externalTextValue "title" objectValue
                        ]
                    )
                    (metadataCwd <|> fallbackCwd)
                    (externalObjectValue "createdAt" objectValue)
                    (externalObjectValue "updatedAt" objectValue)

candidateFromPathCursor
    :: ExternalSessionEnv
    -> FilePath
    -> IO (Maybe ExternalCandidate)
candidateFromPathCursor _env path
    | takeExtension path == ".jsonl" = do
        safe <- existingNonSymlinkFile path
        if safe
            then Just <$> mkCandidate
                ExternalCursor "cursor-transcript"
                (Text.pack (dropJsonl (takeFileName path)))
                path "" Nothing Nothing Nothing
            else pure Nothing
    | takeFileName path `elem` ["store.db", "meta.json"] =
        cursorCliCandidateWithin
            (takeDirectory path)
            (takeDirectory path)
            Nothing
    | otherwise = do
        directory <- doesDirectoryExist path
        if directory
            then cursorCliCandidateWithin path path Nothing
            else pure Nothing

findCursorById
    :: ExternalSessionEnv
    -> Text
    -> IO (Maybe ExternalCandidate)
findCursorById env reference
    | not (literalPathComponent reference) = pure Nothing
    | otherwise = do
        chatDigests <- directoryChildren (env.externalCursorRoot </> "chats")
        chatDirectories <- concat <$> traverse directoryChildren chatDigests
        cli <- firstJustM
            (\directory -> cursorCliCandidate env directory Nothing)
            [ directory
            | directory <- chatDirectories
            , Text.toCaseFold (Text.pack (takeFileName directory))
                == Text.toCaseFold reference
            ]
        case cli of
            Just value -> pure (Just value)
            Nothing -> do
                desktop <- concat <$> traverse (findDesktop env reference)
                    env.externalCursorDesktopStores
                case desktop of
                    value : _ -> pure (Just value)
                    [] -> do
                        paths <- recursiveFiles
                            (env.externalCursorRoot </> "projects")
                            (\path ->
                                Text.toCaseFold
                                    (Text.pack (dropJsonl (takeFileName path)))
                                    == Text.toCaseFold reference)
                        case paths of
                            path : _ ->
                                Just <$> mkCandidate
                                    ExternalCursor "cursor-transcript"
                                    reference path "" Nothing Nothing Nothing
                            [] -> pure Nothing

findDesktop
    :: ExternalSessionEnv
    -> Text
    -> FilePath
    -> IO [ExternalCandidate]
findDesktop _env reference databasePath = do
    result <- tryAny $
        withReadOnlyDatabase databasePath \database -> do
            headers <- cursorHeaderValues database
            mapMaybe id <$> traverse convert headers
    pure (either (const []) id result)
  where
    convert header =
        case externalTextValue "composerId" header of
            Just sessionId
                | Text.toCaseFold sessionId
                    == Text.toCaseFold reference -> do
                        let paths = nestedStrings cursorCwdKeys header
                        Just <$> mkCandidate
                            ExternalCursor "cursor-desktop" sessionId
                            databasePath
                            ( firstNonEmptyText
                                [ externalTextValue "name" header
                                , externalTextValue "title" header
                                ]
                            )
                            (Text.unpack <$> firstMaybe paths)
                            (externalObjectValue "createdAt" header)
                            (externalObjectValue "lastUpdatedAt" header)
            _ -> pure Nothing

readCursor
    :: ExternalSessionEnv
    -> ExternalCandidate
    -> Int
    -> IO ExternalSession
readCursor env candidate maxToolChars = do
    stateRef <- newIORef (emptyBoundedTurns, mempty)
    warningRef <- newIORef []
    let consume value =
            modifyIORef' stateRef \(bounded, omissions) ->
                let (turns, addedOmissions) =
                        cursorTurns maxToolChars value
                in (foldl (flip appendBoundedTurn) bounded turns,
                    omissions <> addedOmissions)
    transcript <- resolveCursorTranscript env candidate
    case transcript of
        Just path -> do
            counters <- consumeJsonl env path Nothing \value -> do
                consume value
                pure JsonlContinue
            modifyIORef' warningRef (<> jsonlWarnings counters)
        Nothing
            | takeFileName candidate.candidatePath == "state.vscdb" ->
                readDesktopRows candidate consume warningRef
            | otherwise -> do
                directory <- doesDirectoryExist candidate.candidatePath
                if directory
                    then readCursorStore candidate consume warningRef
                    else readDesktopRows candidate consume warningRef
    (bounded, omissions) <- readIORef stateRef
    warnings <- readIORef warningRef
    let turns = boundedRecent bounded
        unavailable =
            [ warning
                "cursor_transcript_unavailable"
                ( "Cursor metadata was found, but no safe text transcript "
                    <> "was recoverable; binary/protobuf content was not inferred."
                )
            | null turns
            ]
    pure $
        finaliseSession candidate turns
            (appendOmissionWarnings omissions (warnings <> unavailable))
            (boundedLastText "user" bounded)
            (boundedLastText "assistant" bounded)

resolveCursorTranscript
    :: ExternalSessionEnv
    -> ExternalCandidate
    -> IO (Maybe FilePath)
resolveCursorTranscript env candidate = do
    let path = candidate.candidatePath
    safeFile <- existingNonSymlinkFile path
    if safeFile && takeExtension path == ".jsonl"
        then pure (Just path)
        else case candidate.candidateCwd of
            Nothing -> pure Nothing
            Just cwd -> cursorTranscriptForSession
                env candidate.candidateSessionId cwd

cursorTranscriptForSession
    :: ExternalSessionEnv
    -> Text
    -> FilePath
    -> IO (Maybe FilePath)
cursorTranscriptForSession env sessionId cwd
    | not (literalPathComponent sessionId) = pure Nothing
    | otherwise = do
        projectDirectories <- directoryChildren
            (env.externalCursorRoot </> "projects")
        matching <- filterM
            (\directory -> cursorProjectMatchesCwd env directory cwd)
            projectDirectories
        firstSafe matching
  where
    firstSafe [] = pure Nothing
    firstSafe (project : rest) = do
        let directory =
                project </> "agent-transcripts" </> Text.unpack sessionId
            transcript =
                directory </> Text.unpack sessionId <> ".jsonl"
        safeDirectory <- isSafeDirectory project directory
        safeTranscript <- isSafeFile directory transcript
        if safeDirectory && safeTranscript
            then pure (Just transcript)
            else firstSafe rest

readCursorStore
    :: ExternalCandidate
    -> (Value -> IO ())
    -> IORef.IORef [ExternalWarning]
    -> IO ()
readCursorStore candidate consume warningRef = do
    let store = candidate.candidatePath </> "store.db"
    safe <- isSafeFile candidate.candidatePath store
    if not safe
        then pure ()
        else do
            result <- tryAny $
                withReadOnlyDatabase store \database -> do
                    columns <- tableColumns database "blobs"
                    let keyColumn = firstMaybe
                            [ name | name <- ["id", "key", "hash"],
                                name `Set.member` columns ]
                        dataColumn = firstMaybe
                            [ name | name <- ["data", "value", "blob"],
                                name `Set.member` columns ]
                    case (keyColumn, dataColumn) of
                        (Just keyName, Just dataName) -> do
                            rows <- queryRows database
                                ( "SELECT \"" <> keyName <> "\", \""
                                    <> dataName <> "\" FROM blobs ORDER BY \""
                                    <> keyName <> "\""
                                )
                                []
                            consumeRows consume rows
                        _ -> pure 0
            case result of
                Left exception ->
                    modifyIORef' warningRef
                        (<> [warning "cursor_store_error"
                            (oneLine 200 (Text.pack (show exception)))])
                Right unavailable -> appendUnavailable
                    "Cursor blob(s)" unavailable warningRef

readDesktopRows
    :: ExternalCandidate
    -> (Value -> IO ())
    -> IORef.IORef [ExternalWarning]
    -> IO ()
readDesktopRows candidate consume warningRef = do
    let prefix = "bubbleId:" <> candidate.candidateSessionId <> ":"
        composer = "composerData:" <> candidate.candidateSessionId
    result <- tryAny $
        withReadOnlyDatabase candidate.candidatePath \database -> do
            rows <- queryRows database
                "SELECT value FROM cursorDiskKV \
                \WHERE key = ? OR substr(key, 1, length(?)) = ? ORDER BY key"
                [SQLText composer, SQLText prefix, SQLText prefix]
            consumeRows consume rows
    case result of
        Left exception ->
            modifyIORef' warningRef
                (<> [warning "cursor_store_error"
                    (oneLine 200 (Text.pack (show exception)))])
        Right unavailable ->
            appendUnavailable "Cursor row(s)" unavailable warningRef

consumeRows :: (Value -> IO ()) -> [[SQLData]] -> IO Int
consumeRows consume = foldM step 0
  where
    step unavailable row =
        case lastMaybe row >>= decodeJsonish of
            Nothing -> pure (unavailable + 1)
            Just value -> consume value >> pure unavailable

appendUnavailable
    :: Text
    -> Int
    -> IORef.IORef [ExternalWarning]
    -> IO ()
appendUnavailable label unavailable warningRef =
    if unavailable <= 0
        then pure ()
        else modifyIORef' warningRef
            (<> [ warning
                    "binary_content_unavailable"
                    ( Text.pack (show unavailable) <> " " <> label
                        <> " were binary, protobuf, or non-JSON and were not inferred."
                    )
                ])

cursorTurns :: Int -> Value -> ([ExternalTurn], ContentOmissions)
cursorTurns maxToolChars root = go [root] [] mempty
  where
    go [] turns omissions = (reverse turns, omissions)
    go (current : pending) turns omissions =
        case current of
            Array values ->
                go (Vector.toList values <> pending) turns omissions
            Object{} ->
                let rawRole = Text.toLower $
                        firstNonEmptyText
                            [ externalTextValue "role" current
                            , externalTextValue "type" current
                            ]
                in if rawRole `elem` ignoredCursorRoles
                    then go pending turns omissions
                    else case normalizeCursorRole rawRole of
                        Just role ->
                            let rawText =
                                    fromMaybe Null $
                                        externalObjectValue "text" current
                                            <|> externalObjectValue "content" current
                                            <|> externalObjectValue "message" current
                                textValue =
                                    case rawText of
                                        Object{} ->
                                            fromMaybe rawText $
                                                externalObjectValue "text" rawText
                                                    <|> externalObjectValue
                                                        "content" rawText
                                        _ -> rawText
                                (text, textOmissions) =
                                    case textValue of
                                        String value -> (value, mempty)
                                        Object{} ->
                                            contentTextWithOmissions textValue
                                        Array{} ->
                                            contentTextWithOmissions textValue
                                        _ -> ("", mempty)
                                (calls, results, resultOmissions) =
                                    cursorCallsAndResults maxToolChars current
                                turn = inertTurn role text calls results
                            in case turn of
                                Just value ->
                                    go pending (value : turns)
                                        (omissions <> textOmissions
                                            <> resultOmissions)
                                Nothing ->
                                    go (cursorChildren current <> pending)
                                        turns
                                        (omissions <> textOmissions
                                            <> resultOmissions)
                        Nothing
                            | rawRole `elem`
                                ["tool", "tool_result", "tool_output"] ->
                                    let rawOutput =
                                            fromMaybe Null $
                                                externalObjectValue "content" current
                                                    <|> externalObjectValue
                                                        "output" current
                                                    <|> externalObjectValue
                                                        "text" current
                                        (output, resultOmissions) =
                                            toolResultContent rawOutput
                                        result = HistoricalToolResult
                                            { historicalResultCallId =
                                                firstNonEmptyText
                                                    [ externalTextValue
                                                        "tool_call_id" current
                                                    , externalTextValue
                                                        "call_id" current
                                                    ]
                                            , historicalResultOutput =
                                                historicalToolResult
                                                    maxToolChars output
                                            }
                                    in case inertTurn
                                        "assistant" "" [] [result] of
                                            Just value ->
                                                go pending (value : turns)
                                                    (omissions
                                                        <> resultOmissions)
                                            Nothing ->
                                                go pending turns
                                                    (omissions
                                                        <> resultOmissions)
                            | otherwise ->
                                go (cursorChildren current <> pending)
                                    turns omissions
            _ -> go pending turns omissions

cursorCallsAndResults
    :: Int
    -> Value
    -> ([HistoricalToolCall], [HistoricalToolResult], ContentOmissions)
cursorCallsAndResults maxToolChars value =
    let blocks = case externalObjectValue "content" value of
            Just (Array values) -> Vector.toList values
            _ -> []
        (blockCalls, blockResults, omissions) =
            foldl consumeBlock ([], [], mempty) blocks
        topCalls = case externalObjectValue "tool_calls" value of
            Just (Array values) -> mapMaybe parseTopCall (Vector.toList values)
            _ -> []
    in (blockCalls <> topCalls, blockResults, omissions)
  where
    consumeBlock state@(calls, results, omissions) block =
        case Text.toLower <$> externalTextValue "type" block of
            Just kind | kind `elem` ["tool_use", "tool_call"] ->
                ( calls <>
                    [ HistoricalToolCall
                        { historicalCallId =
                            firstNonEmptyText
                                [ externalTextValue "id" block
                                , externalTextValue "call_id" block
                                ]
                        , historicalCallName =
                            fromMaybe "tool" (externalTextValue "name" block)
                        , historicalCallArguments =
                            jsonPreview maxToolChars $
                                fromMaybe Null $
                                    externalObjectValue "input" block
                                        <|> externalObjectValue
                                            "arguments" block
                        }
                    ]
                , results
                , omissions
                )
            Just kind | kind `elem` ["tool_result", "tool_output"] ->
                let (output, added) = toolResultContent $
                        fromMaybe Null (externalObjectValue "content" block)
                in
                    ( calls
                    , results <>
                        [ HistoricalToolResult
                            { historicalResultCallId =
                                firstNonEmptyText
                                    [ externalTextValue "tool_use_id" block
                                    , externalTextValue "call_id" block
                                    ]
                            , historicalResultOutput =
                                historicalToolResult maxToolChars output
                            }
                        ]
                    , omissions <> added
                    )
            _ -> state
    parseTopCall raw@Object{} =
        let functionValue =
                case externalObjectValue "function" raw of
                    Just value@Object{} -> value
                    _ -> raw
        in Just HistoricalToolCall
            { historicalCallId =
                firstNonEmptyText
                    [ externalTextValue "id" raw
                    , externalTextValue "call_id" functionValue
                    ]
            , historicalCallName =
                fromMaybe "tool"
                    (externalTextValue "name" functionValue)
            , historicalCallArguments =
                jsonPreview maxToolChars $
                    fromMaybe Null $
                        externalObjectValue "arguments" functionValue
                            <|> externalObjectValue "input" functionValue
            }
    parseTopCall _ = Nothing

cursorFirstUserTitle :: Value -> Maybe Text
cursorFirstUserTitle root =
    firstMaybe
        [ value.externalTurnText
        | value <- fst (cursorTurns 100 root)
        , value.externalTurnRole == "user"
        , not (Text.null value.externalTurnText)
        ]

cursorChildren :: Value -> [Value]
cursorChildren (Object objectValue) =
    [ child
    | (key, child) <- KeyMap.toList objectValue
    , Text.toLower (Key.toText key) `elem` cursorConversationKeys
    , case child of
        Object{} -> True
        Array{} -> True
        _ -> False
    ]
cursorChildren _ = []

nestedStrings :: [Text] -> Value -> [Text]
nestedStrings wanted root = reverse (go [root] [])
  where
    lowered = map Text.toLower wanted
    go [] found = found
    go (current : pending) found = case current of
        Object objectValue ->
            let matches =
                    [ text
                    | (key, String text) <- KeyMap.toList objectValue
                    , Text.toLower (Key.toText key) `elem` lowered
                    ]
                children =
                    [ child
                    | (_, child) <- KeyMap.toList objectValue
                    , case child of
                        Object{} -> True
                        Array{} -> True
                        _ -> False
                    ]
            in go (children <> pending) (reverse matches <> found)
        Array values -> go (Vector.toList values <> pending) found
        _ -> go pending found

normalizeCursorRole :: Text -> Maybe Text
normalizeCursorRole role
    | role `elem` ["human", "user"] = Just "user"
    | role `elem` ["ai", "assistant"] = Just "assistant"
    | otherwise = Nothing

cursorConversationKeys, cursorCwdKeys, ignoredCursorRoles :: [Text]
cursorConversationKeys = ["messages", "turns", "conversation", "bubbles"]
cursorCwdKeys =
    [ "cwd", "fspath", "folderpath", "rootpath", "sourcereporootpath",
      "workspacepath" ]
ignoredCursorRoles =
    [ "system", "developer", "instruction", "instructions", "thinking",
      "reasoning", "redacted_thinking", "signature", "encrypted_content" ]

existingNonSymlinkFile :: FilePath -> IO Bool
existingNonSymlinkFile path = do
    exists <- doesFileExist path
    if not exists
        then pure False
        else tryIO (pathIsSymbolicLink path) >>= \case
            Left (_ :: IOException) -> pure False
            Right symlink -> pure (not symlink)

dropJsonl :: FilePath -> FilePath
dropJsonl name =
    fromMaybe name $
        Text.unpack <$> Text.stripSuffix ".jsonl" (Text.pack name)

sqlDataToValue :: SQLData -> Value
sqlDataToValue = \case
    SQLInteger value -> Number (fromIntegral value)
    SQLFloat value -> Number (realToFrac value)
    SQLText value -> String value
    SQLBlob value -> String (TextEncoding.decodeUtf8With lenientDecode value)
    SQLNull -> Null

truthy :: Maybe Value -> Bool
truthy = \case
    Nothing -> False
    Just Null -> False
    Just (Bool False) -> False
    Just (String value) -> not (Text.null value)
    Just (Array values) -> not (Vector.null values)
    Just (Object values) -> not (KeyMap.null values)
    Just _ -> True

firstNonEmptyText :: [Maybe Text] -> Text
firstNonEmptyText values =
    fromMaybe "" $ firstMaybe
        [ value | Just value <- values, not (Text.null value) ]

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
