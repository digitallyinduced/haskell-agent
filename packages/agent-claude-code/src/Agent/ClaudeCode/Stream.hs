{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Decode and accumulate Claude Code's Agent-SDK-compatible stream-json
-- stdout protocol.
module Agent.ClaudeCode.Stream
    ( CompletedTurn(..)
    , StreamAccumulator
    , consumeStreamLine
    , decodeStreamLine
    , emptyStreamAccumulator
    , streamAccumulatorError
    ) where

import Agent.Loop
    ( LoopEvent(..)
    , TokenUsage(..)
    , addTokenUsage
    )
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , ToolCallResult(..)
    )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncoding

data CompletedTurn = CompletedTurn
    { sessionId :: !Text
    , assistantText :: !(Maybe Text)
    -- | Per-result main-loop usage. Used only when Claude does not provide a
    -- valid cumulative @modelUsage@ snapshot.
    , tokenUsage :: !TokenUsage
    -- | Running total across all models used by the current child process.
    -- The session layer converts it to a per-turn delta before returning it to
    -- the provider-neutral loop.
    , cumulativeModelUsage :: !(Maybe TokenUsage)
    } deriving (Eq, Show)

data BufferedRecord = BufferedRecord
    { bufferedAssistantChunks :: ![Text]
    , bufferedToolEvents :: ![LoopEvent]
    } deriving (Eq, Show)

data StreamAccumulator = StreamAccumulator
    { bufferedRecords :: !(Map Text BufferedRecord)
    , recordOrderRev :: ![Text]
    , seenRecordIds :: !(Set Text)
    , retractedRecordIds :: !(Set Text)
    , sawSubscriptionInit :: !Bool
    , terminalError :: !(Maybe Text)
    } deriving (Eq, Show)

emptyStreamAccumulator :: StreamAccumulator
emptyStreamAccumulator = StreamAccumulator
    { bufferedRecords = Map.empty
    , recordOrderRev = []
    , seenRecordIds = Set.empty
    , retractedRecordIds = Set.empty
    , sawSubscriptionInit = False
    , terminalError = Nothing
    }

streamAccumulatorError :: StreamAccumulator -> Maybe Text
streamAccumulatorError accumulator =
    accumulator.terminalError

-- | Decode one stdout JSONL record. Syntactically valid unknown records are
-- intentionally preserved as objects and ignored by 'consumeStreamLine'.
decodeStreamLine :: ByteString -> Either Text Aeson.Value
decodeStreamLine bytes =
    case Aeson.eitherDecodeStrict' bytes of
        Left message ->
            Left ("Invalid Claude Code stream JSON: " <> Text.pack message)
        Right value ->
            Right value

consumeStreamLine
    :: StreamAccumulator
    -> ByteString
    -> Either
        Text
        (StreamAccumulator, [LoopEvent], Maybe CompletedTurn)
consumeStreamLine accumulator bytes = do
    value <- decodeStreamLine bytes
    case value of
        Aeson.Object object ->
            pure (consumeObject accumulator object)
        _ ->
            Left "Invalid Claude Code stream record: expected a JSON object."

consumeObject
    :: StreamAccumulator
    -> Aeson.Object
    -> (StreamAccumulator, [LoopEvent], Maybe CompletedTurn)
consumeObject accumulator object =
    case nonEmptyTextAt "uuid" object of
        Just identifier
            | Set.member identifier accumulator.seenRecordIds
                || Set.member identifier accumulator.retractedRecordIds ->
                (accumulator, [], Nothing)
            | otherwise ->
                consumeFreshObject
                    accumulator
                        { seenRecordIds =
                            Set.insert identifier accumulator.seenRecordIds
                        }
                    object
        Nothing ->
            consumeFreshObject accumulator object

consumeFreshObject
    :: StreamAccumulator
    -> Aeson.Object
    -> (StreamAccumulator, [LoopEvent], Maybe CompletedTurn)
consumeFreshObject accumulator object
    | hasParentToolUseId object =
        -- Nested subagent messages share the stdout stream when forwarding is
        -- enabled. They belong to the enclosing Agent tool, not to the
        -- top-level assistant response rendered by this backend.
        (accumulator, [], Nothing)
    | otherwise =
        case textAt "type" object of
            Just "stream_event" ->
                -- Partial messages are deliberately not requested. The host's
                -- append-only renderers cannot retract text or tool output,
                -- while Claude can supersede records during refusal fallback.
                -- Buffer complete records transactionally until the result.
                (accumulator, [], Nothing)
            Just "assistant" ->
                consumeAssistant accumulator object
            Just "user" ->
                consumeUser accumulator object
            Just "system" ->
                consumeSystem accumulator object
            Just "result" ->
                consumeResult accumulator object
            Just "conversation_reset" ->
                ( accumulator
                    { terminalError =
                        Just
                            "Claude Code reset its conversation while a harness session was active."
                    }
                , []
                , Nothing
                )
            Just "control_request" ->
                ( accumulator
                    { terminalError =
                        Just
                            "Claude Code requested interactive protocol input that this backend does not support."
                    }
                , []
                , Nothing
                )
            _ ->
                (accumulator, [], Nothing)

consumeAssistant
    :: StreamAccumulator
    -> Aeson.Object
    -> (StreamAccumulator, [LoopEvent], Maybe CompletedTurn)
consumeAssistant accumulator outer =
    let afterRetractions =
            retractRecords accumulator (stringArrayAt "supersedes" outer)
    in if nonEmptyTextAt "error" outer /= Nothing
        then
            -- Claude follows assistant error frames with an authoritative
            -- terminal result. Their synthetic text must not render as a
            -- normal answer immediately before the structured error.
            (afterRetractions, [], Nothing)
        else
            case objectAt "message" outer >>= valueArrayAt "content" of
                Nothing ->
                    (afterRetractions, [], Nothing)
                Just content ->
                    let textChunks =
                            mapMaybe assistantTextFromValue content
                        toolCalls =
                            mapMaybe toolCallFromValue content
                        contribution = BufferedRecord
                            { bufferedAssistantChunks = textChunks
                            , bufferedToolEvents =
                                map ToolStarted toolCalls
                            }
                    in
                        ( bufferVisibleRecord
                            afterRetractions
                            outer
                            contribution
                        , []
                        , Nothing
                        )

consumeSystem
    :: StreamAccumulator
    -> Aeson.Object
    -> (StreamAccumulator, [LoopEvent], Maybe CompletedTurn)
consumeSystem accumulator object
    | textAt "subtype" object == Just "model_refusal_fallback" =
        ( retractRecords
            accumulator
            (stringArrayAt "retracted_message_uuids" object)
        , []
        , Nothing
        )
    | textAt "subtype" object == Just "init" =
        case nonEmptyTextAt "apiKeySource" object of
            Just "none" ->
                ( accumulator { sawSubscriptionInit = True }
                , []
                , Nothing
                )
            Just source ->
                ( accumulator
                    { terminalError =
                        Just
                            ( "Claude Code selected non-subscription credential source "
                                <> source
                                <> "."
                            )
                    }
                , []
                , Nothing
                )
            Nothing ->
                ( accumulator
                    { terminalError =
                        Just
                            "Claude Code did not identify its credential source."
                    }
                , []
                , Nothing
                )
    | otherwise =
        (accumulator, [], Nothing)

consumeUser
    :: StreamAccumulator
    -> Aeson.Object
    -> (StreamAccumulator, [LoopEvent], Maybe CompletedTurn)
consumeUser accumulator outer =
    case objectAt "message" outer >>= valueArrayAt "content" of
        Nothing ->
            (accumulator, [], Nothing)
        Just content ->
            let results = mapMaybe toolResultFromValue content
                contribution = BufferedRecord
                    { bufferedAssistantChunks = []
                    , bufferedToolEvents =
                        map ToolFinished results
                    }
            in
                ( bufferVisibleRecord accumulator outer contribution
                , []
                , Nothing
                )

consumeResult
    :: StreamAccumulator
    -> Aeson.Object
    -> (StreamAccumulator, [LoopEvent], Maybe CompletedTurn)
consumeResult accumulator object
    | textAt "subtype" object == Just "success"
    , boolAt "is_error" object == Just False
    , not accumulator.sawSubscriptionInit =
        ( accumulator
            { terminalError =
                Just
                    "Claude Code completed before confirming subscription authentication."
            }
        , []
        , Nothing
        )
    | textAt "subtype" object == Just "success"
    , boolAt "is_error" object == Just False
    , Just sessionId <- nonEmptyTextAt "session_id" object =
        let resultText = maybe "" id (textAt "result" object)
            bufferedText =
                canonicalBufferedAssistantText accumulator
            finalText =
                firstNonEmptyText [resultText, bufferedText]
            completed = CompletedTurn
                { sessionId
                , assistantText = finalText
                , tokenUsage =
                    maybe
                        emptyUsage
                        usageFromObject
                        (objectAt "usage" object)
                , cumulativeModelUsage =
                    modelUsageFromResult object
                }
        in
            ( accumulator
            , canonicalBufferedToolEvents accumulator
                <> maybe [] (pure . TextDelta) finalText
            , Just completed
            )
    | textAt "subtype" object == Just "success"
    , boolAt "is_error" object == Just False =
        ( accumulator
            { terminalError =
                Just
                    "Claude Code completed a turn without reporting a session ID."
            }
        , []
        , Nothing
        )
    | otherwise =
        ( accumulator
            { terminalError =
                Just (resultErrorText object)
            }
        , []
        , Nothing
        )

bufferVisibleRecord
    :: StreamAccumulator
    -> Aeson.Object
    -> BufferedRecord
    -> StreamAccumulator
bufferVisibleRecord accumulator outer contribution
    | null contribution.bufferedAssistantChunks
        && null contribution.bufferedToolEvents =
        accumulator
    | otherwise =
        case nonEmptyTextAt "uuid" outer of
            Nothing ->
                accumulator
                    { terminalError =
                        Just
                            "Claude Code emitted a visible record without a wire UUID."
                    }
            Just identifier
                | Set.member identifier accumulator.retractedRecordIds ->
                    accumulator
                | otherwise ->
                    accumulator
                        { bufferedRecords =
                            Map.insert
                                identifier
                                contribution
                                accumulator.bufferedRecords
                        , recordOrderRev =
                            identifier : accumulator.recordOrderRev
                        }

retractRecords :: StreamAccumulator -> [Text] -> StreamAccumulator
retractRecords accumulator identifiers =
    let retracted = Set.fromList identifiers
    in accumulator
        { bufferedRecords =
            Set.foldr
                Map.delete
                accumulator.bufferedRecords
                retracted
        , retractedRecordIds =
            Set.union retracted accumulator.retractedRecordIds
        }

canonicalBufferedAssistantText :: StreamAccumulator -> Text
canonicalBufferedAssistantText accumulator =
    Text.concat
        [ chunk
        | identifier <- reverse accumulator.recordOrderRev
        , Just record <-
            [Map.lookup identifier accumulator.bufferedRecords]
        , chunk <- record.bufferedAssistantChunks
        ]

canonicalBufferedToolEvents :: StreamAccumulator -> [LoopEvent]
canonicalBufferedToolEvents accumulator =
    reverse eventsRev
  where
    orderedEvents =
        [ event
        | identifier <- reverse accumulator.recordOrderRev
        , Just record <-
            [Map.lookup identifier accumulator.bufferedRecords]
        , event <- record.bufferedToolEvents
        ]
    (_, _, eventsRev) =
        foldl' step (Set.empty, Set.empty, []) orderedEvents
    step (started, finished, events) = \case
        ToolStarted call
            | Set.member call.callId started ->
                (started, finished, events)
            | otherwise ->
                ( Set.insert call.callId started
                , finished
                , ToolStarted call : events
                )
        ToolFinished result
            | not (Set.member result.callId started)
                || Set.member result.callId finished ->
                (started, finished, events)
            | otherwise ->
                ( started
                , Set.insert result.callId finished
                , ToolFinished result : events
                )
        _ ->
            (started, finished, events)

assistantTextFromValue :: Aeson.Value -> Maybe Text
assistantTextFromValue = \case
    Aeson.Object content
        | textAt "type" content == Just "text"
        , Just text <- textAt "text" content
        , not (Text.null text) ->
            Just text
    _ ->
        Nothing

toolCallFromValue :: Aeson.Value -> Maybe ToolCall
toolCallFromValue = \case
    Aeson.Object content
        | textAt "type" content == Just "tool_use" -> do
            identifier <- nonEmptyTextAt "id" content
            toolName <- nonEmptyTextAt "name" content
            let inputValue =
                    maybe
                        (Aeson.Object KeyMap.empty)
                        id
                        (KeyMap.lookup "input" content)
            pure ToolCall
                { callId = identifier
                , name = toolName
                , arguments = encodeValue inputValue
                , callKind = FunctionCallKind
                , argumentsEncrypted = False
                }
    _ ->
        Nothing

toolResultFromValue :: Aeson.Value -> Maybe ToolCallResult
toolResultFromValue = \case
    Aeson.Object object
        | textAt "type" object == Just "tool_result" -> do
            identifier <- nonEmptyTextAt "tool_use_id" object
            let rawOutput =
                    maybe "" renderResultContent
                        (KeyMap.lookup "content" object)
                renderedOutput
                    | boolAt "is_error" object == Just True =
                        "Error: " <> rawOutput
                    | otherwise =
                        rawOutput
            pure ToolCallResult
                { callId = identifier
                , output = renderedOutput
                , callKind = FunctionCallKind
                }
    _ ->
        Nothing

usageFromObject :: Aeson.Object -> TokenUsage
usageFromObject object =
    let directInput = intAt "input_tokens" object
        cacheCreation = intAt "cache_creation_input_tokens" object
        cacheRead = intAt "cache_read_input_tokens" object
    in TokenUsage
        { inputTokens = directInput + cacheCreation + cacheRead
        , outputTokens = intAt "output_tokens" object
        , cachedTokens = cacheRead
        }

modelUsageFromResult :: Aeson.Object -> Maybe TokenUsage
modelUsageFromResult object = do
    models <- objectAt "modelUsage" object
    entries <- traverse modelUsageEntry (KeyMap.elems models)
    case entries of
        [] ->
            Nothing
        _ ->
            Just (foldl' addTokenUsage emptyUsage entries)

modelUsageEntry :: Aeson.Value -> Maybe TokenUsage
modelUsageEntry = \case
    Aeson.Object object -> do
        directInput <- nonNegativeIntAt "inputTokens" object
        output <- nonNegativeIntAt "outputTokens" object
        cacheRead <- optionalNonNegativeIntAt "cacheReadInputTokens" object
        cacheCreation <-
            optionalNonNegativeIntAt "cacheCreationInputTokens" object
        pure TokenUsage
            { inputTokens = directInput + cacheCreation + cacheRead
            , outputTokens = output
            , cachedTokens = cacheRead
            }
    _ ->
        Nothing

emptyUsage :: TokenUsage
emptyUsage = TokenUsage
    { inputTokens = 0
    , outputTokens = 0
    , cachedTokens = 0
    }

resultErrorText :: Aeson.Object -> Text
resultErrorText object =
    "Claude Code "
        <> maybe "request failed" id (nonEmptyTextAt "subtype" object)
        <> statusSuffix
        <> ": "
        <> details
  where
    status = intAt "api_error_status" object
    statusSuffix
        | status > 0 =
            " (HTTP " <> Text.pack (show status) <> ")"
        | otherwise =
            ""
    details =
        case stringArrayAt "errors" object of
            first : rest ->
                Text.intercalate "; " (first : rest)
            [] ->
                maybe
                    "request failed"
                    (Text.take 2_000)
                    (nonEmptyTextAt "result" object)

renderResultContent :: Aeson.Value -> Text
renderResultContent = \case
    Aeson.String text -> text
    Aeson.Array values ->
        Text.intercalate "\n"
            (map renderResultContent (toList values))
    Aeson.Object object ->
        case textAt "text" object of
            Just text -> text
            Nothing -> encodeValue (Aeson.Object object)
    Aeson.Null -> ""
    other -> encodeValue other

encodeValue :: Aeson.Value -> Text
encodeValue =
    TextEncoding.decodeUtf8With TextEncoding.lenientDecode
        . LazyByteString.toStrict
        . Aeson.encode

firstNonEmptyText :: [Text] -> Maybe Text
firstNonEmptyText values =
    case filter (not . Text.null) values of
        value : _ -> Just value
        [] -> Nothing

objectAt :: Key.Key -> Aeson.Object -> Maybe Aeson.Object
objectAt key object =
    case KeyMap.lookup key object of
        Just (Aeson.Object nested) -> Just nested
        _ -> Nothing

valueArrayAt :: Key.Key -> Aeson.Object -> Maybe [Aeson.Value]
valueArrayAt key object =
    case KeyMap.lookup key object of
        Just (Aeson.Array values) -> Just (toList values)
        Just (Aeson.String text) ->
            Just
                [ Aeson.object
                    [ "type" Aeson..= ("text" :: Text)
                    , "text" Aeson..= text
                    ]
                ]
        _ -> Nothing

stringArrayAt :: Key.Key -> Aeson.Object -> [Text]
stringArrayAt key object =
    case KeyMap.lookup key object of
        Just (Aeson.Array values) ->
            [text | Aeson.String text <- toList values]
        _ -> []

textAt :: Key.Key -> Aeson.Object -> Maybe Text
textAt key object =
    case KeyMap.lookup key object of
        Just (Aeson.String text) -> Just text
        _ -> Nothing

nonEmptyTextAt :: Key.Key -> Aeson.Object -> Maybe Text
nonEmptyTextAt key object = do
    text <- textAt key object
    let stripped = Text.strip text
    if Text.null stripped then Nothing else Just stripped

boolAt :: Key.Key -> Aeson.Object -> Maybe Bool
boolAt key object =
    case KeyMap.lookup key object of
        Just (Aeson.Bool value) -> Just value
        _ -> Nothing

intAt :: Key.Key -> Aeson.Object -> Int
intAt key object =
    case KeyMap.lookup key object of
        Nothing -> 0
        Just Aeson.Null -> 0
        Just value ->
            case Aeson.fromJSON value of
                Aeson.Success number -> max 0 number
                Aeson.Error _ -> 0

nonNegativeIntAt :: Key.Key -> Aeson.Object -> Maybe Int
nonNegativeIntAt key object = do
    value <- KeyMap.lookup key object
    case Aeson.fromJSON value of
        Aeson.Success number
            | number >= 0 ->
                Just number
        _ ->
            Nothing

optionalNonNegativeIntAt :: Key.Key -> Aeson.Object -> Maybe Int
optionalNonNegativeIntAt key object =
    case KeyMap.lookup key object of
        Nothing ->
            Just 0
        Just Aeson.Null ->
            Just 0
        Just value ->
            case Aeson.fromJSON value of
                Aeson.Success number
                    | number >= 0 ->
                        Just number
                _ ->
                    Nothing

hasParentToolUseId :: Aeson.Object -> Bool
hasParentToolUseId object =
    case KeyMap.lookup "parent_tool_use_id" object of
        Nothing -> False
        Just Aeson.Null -> False
        Just _ -> True
