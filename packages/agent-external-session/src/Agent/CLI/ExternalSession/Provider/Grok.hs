module Agent.CLI.ExternalSession.Provider.Grok
    ( candidateFromPathGrok
    , discoverGrok
    , findGrokById
    , readGrok
    ) where

import Agent.CLI.ExternalSession.Content
import Agent.CLI.ExternalSession.JSONL
import Agent.CLI.ExternalSession.Paths
import Agent.CLI.ExternalSession.Types
import Control.Applicative ((<|>))
import Control.Monad (filterM)
import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Network.URI (unEscapeString)
import System.FilePath
    ( takeDirectory
    , takeFileName
    , (</>)
    )

discoverGrok :: ExternalSessionEnv -> FilePath -> IO [ExternalCandidate]
discoverGrok env cwd = do
    let sessions = env.externalGrokRoot </> "sessions"
    encodedDirectories <- directoryChildren sessions
    sessionDirectories <- concat <$> traverse directoryChildren encodedDirectories
    mapMaybe id <$> traverse (metadataInDirectory env) sessionDirectories
        >>= filterM (candidateMatchesCwd cwd)

findGrokById
    :: ExternalSessionEnv
    -> Text
    -> IO (Maybe ExternalCandidate)
findGrokById env reference = do
    candidates <- discoverAllGrok env
    pure $
        firstMaybe
            [ candidate
            | candidate <- candidates
            , Text.toCaseFold candidate.candidateSessionId
                == Text.toCaseFold reference
                || Text.toCaseFold
                    (Text.pack (takeFileName candidate.candidatePath))
                    == Text.toCaseFold reference
            ]

discoverAllGrok :: ExternalSessionEnv -> IO [ExternalCandidate]
discoverAllGrok env = do
    let sessions = env.externalGrokRoot </> "sessions"
    encodedDirectories <- directoryChildren sessions
    sessionDirectories <- concat <$> traverse directoryChildren encodedDirectories
    mapMaybe id <$> traverse (metadataInDirectory env) sessionDirectories

metadataInDirectory
    :: ExternalSessionEnv
    -> FilePath
    -> IO (Maybe ExternalCandidate)
metadataInDirectory env directory = do
    safe <- isSafeDirectory (env.externalGrokRoot </> "sessions") directory
    if safe
        then grokMetadata env (directory </> "summary.json")
        else pure Nothing

grokMetadata
    :: ExternalSessionEnv
    -> FilePath
    -> IO (Maybe ExternalCandidate)
grokMetadata env summaryPath = do
    safe <- isSafeFile (env.externalGrokRoot </> "sessions") summaryPath
    if not safe
        then pure Nothing
        else readJsonFileValue summaryPath >>= \case
            Just summary@Object{} -> do
                let info =
                        fromMaybe (Object mempty)
                            (externalObjectValue "info" summary)
                    directory = takeDirectory summaryPath
                    sessionId =
                        firstNonEmptyText
                            [ externalTextValue "id" info
                            , Just (Text.pack (takeFileName directory))
                            ]
                    metadataCwd =
                        externalTextValue "cwd" info
                            <|> Just
                                (Text.pack
                                    (unEscapeString
                                        (takeFileName (takeDirectory directory))))
                    title =
                        fromMaybe "" $
                            externalTextValue "session_summary" summary
                                <|> externalTextValue "generated_title" summary
                Just <$> mkCandidate
                    ExternalGrok
                    "grok-build"
                    sessionId
                    directory
                    title
                    (Text.unpack <$> metadataCwd)
                    (externalObjectValue "created_at" summary)
                    (externalObjectValue "updated_at" summary)
            _ -> pure Nothing

candidateFromPathGrok
    :: ExternalSessionEnv
    -> FilePath
    -> IO (Maybe ExternalCandidate)
candidateFromPathGrok _env path = do
    let summary =
            if takeFileName path == "summary.json"
                then path
                else path </> "summary.json"
        directory = takeDirectory summary
    safe <- isSafeFile directory summary
    if not safe
        then pure Nothing
        else grokMetadataExplicit summary
  where
    grokMetadataExplicit summaryPath =
        readJsonFileValue summaryPath >>= \case
            Just summaryValue@Object{} -> do
                let info =
                        fromMaybe (Object mempty)
                            (externalObjectValue "info" summaryValue)
                    directory = takeDirectory summaryPath
                    sessionId =
                        firstNonEmptyText
                            [ externalTextValue "id" info
                            , Just (Text.pack (takeFileName directory))
                            ]
                    metadataCwd =
                        externalTextValue "cwd" info
                            <|> Just
                                (Text.pack
                                    (unEscapeString
                                        (takeFileName (takeDirectory directory))))
                    title =
                        fromMaybe "" $
                            externalTextValue "session_summary" summaryValue
                                <|> externalTextValue "generated_title" summaryValue
                Just <$> mkCandidate
                    ExternalGrok "grok-build" sessionId directory title
                    (Text.unpack <$> metadataCwd)
                    (externalObjectValue "created_at" summaryValue)
                    (externalObjectValue "updated_at" summaryValue)
            _ -> pure Nothing

readGrok
    :: ExternalSessionEnv
    -> ExternalCandidate
    -> Int
    -> IO ExternalSession
readGrok env candidate maxToolChars = do
    let directory = candidate.candidatePath
        plain = directory </> "chat_history.jsonl"
        compressed = plain <> ".zst"
    plainSafe <- isSafeFile directory plain
    compressedSafe <- isSafeFile directory compressed
    let transcript
            | plainSafe = Just plain
            | compressedSafe = Just compressed
            | otherwise = Nothing
    ((bounded, skipped, omissions), counters) <- case transcript of
        Nothing ->
            pure
                ( (emptyBoundedTurns, 0, mempty)
                , JsonlCounters 0 0 0
                )
        Just path ->
            foldJsonl env path Nothing
                (emptyBoundedTurns, 0, mempty)
                \(current, skippedTotal, totalOmissions) record -> do
                let (turn, skipped, omissions) =
                        grokTurn maxToolChars record
                pure
                    ( ( maybe current (`appendBoundedTurn` current) turn
                    , skippedTotal + skipped
                    , totalOmissions <> omissions
                    )
                    , JsonlContinue
                    )
    let transcriptWarnings =
            [ warning
                "grok_transcript_unavailable"
                ( "The Grok session metadata exists, but a safe "
                    <> "chat_history.jsonl is unavailable."
                )
            | transcript == Nothing
            ]
        unsafeWarnings =
            [ warning
                "unsafe_records_skipped"
                ("Skipped " <> Text.pack (show skipped)
                    <> " hidden or unsupported record(s).")
            | skipped > 0
            ]
        warnings =
            appendOmissionWarnings omissions
                (transcriptWarnings <> jsonlWarnings counters <> unsafeWarnings)
    pure $
        finaliseSession
            candidate
            (boundedRecent bounded)
            warnings
            (boundedLastText "user" bounded)
            (boundedLastText "assistant" bounded)

grokTurn
    :: Int
    -> Value
    -> (Maybe ExternalTurn, Int, ContentOmissions)
grokTurn maxToolChars record =
    let kind = Text.toLower (fromMaybe "" (externalTextValue "type" record))
    in if truthy (externalObjectValue "synthetic_reason" record)
        then (Nothing, 1, mempty)
        else if kind `elem` ["user", "assistant"]
            then
                let (text, omissions) =
                        maybe ("", mempty) contentTextWithOmissions
                            (externalObjectValue "content" record)
                    (calls, invalidCalls) =
                        if kind == "assistant"
                            then parseCalls maxToolChars
                                (externalObjectValue "tool_calls" record)
                            else ([], 0)
                in
                    ( inertTurn kind text calls []
                    , invalidCalls
                    , omissions
                    )
            else if kind `elem` ["tool_call", "backend_tool_call"]
                then
                    ( inertTurn "assistant" "" [callFromRecord kind record] []
                    , 0
                    , mempty
                    )
                else if kind == "tool_result"
                    then
                        let rawOutput =
                                fromMaybe Null $
                                    externalObjectValue "output" record
                                        <|> externalObjectValue "content" record
                            (output, omissions) =
                                toolResultContent rawOutput
                            result = HistoricalToolResult
                                { historicalResultCallId =
                                    firstNonEmptyText
                                        [ externalTextValue "call_id" record
                                        , externalTextValue "tool_call_id" record
                                        ]
                                , historicalResultOutput =
                                    historicalToolResult maxToolChars output
                                }
                        in
                            ( inertTurn "assistant" "" [] [result]
                            , 0
                            , omissions
                            )
                    else (Nothing, 1, mempty)
  where
    callFromRecord fallback recordValue =
        HistoricalToolCall
            { historicalCallId =
                firstNonEmptyText
                    [ externalTextValue "call_id" recordValue
                    , externalTextValue "id" recordValue
                    ]
            , historicalCallName =
                firstNonEmptyText
                    [ externalTextValue "name" recordValue
                    , externalTextValue "tool_name" recordValue
                    , Just fallback
                    ]
            , historicalCallArguments =
                jsonPreview maxToolChars $
                    fromMaybe Null $
                        externalObjectValue "arguments" recordValue
                            <|> externalObjectValue "input" recordValue
            }

parseCalls :: Int -> Maybe Value -> ([HistoricalToolCall], Int)
parseCalls maxToolChars = \case
    Just (Array values) ->
        let parsed = map parseOne (Vector.toList values)
        in (mapMaybe fst parsed, length (filter ((== Nothing) . fst) parsed))
    _ -> ([], 0)
  where
    parseOne :: Value -> (Maybe HistoricalToolCall, Int)
    parseOne record@Object{} =
        ( Just HistoricalToolCall
            { historicalCallId =
                firstNonEmptyText
                    [ externalTextValue "id" record
                    , externalTextValue "call_id" record
                    ]
            , historicalCallName =
                fromMaybe "tool" (externalTextValue "name" record)
            , historicalCallArguments =
                jsonPreview maxToolChars $
                    fromMaybe Null $
                        externalObjectValue "arguments" record
                            <|> externalObjectValue "input" record
            }
        , 0
        )
    parseOne _ = (Nothing, 1)

candidateMatchesCwd :: FilePath -> ExternalCandidate -> IO Bool
candidateMatchesCwd cwd candidate =
    maybe (pure False) (`samePath` cwd) candidate.candidateCwd

truthy :: Maybe Value -> Bool
truthy = \case
    Just Null -> False
    Just (Bool False) -> False
    Just (String text) -> not (Text.null text)
    Just (Array values) -> not (Vector.null values)
    Just (Object value) -> not (KeyMap.null value)
    Just _ -> True
    Nothing -> False

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
