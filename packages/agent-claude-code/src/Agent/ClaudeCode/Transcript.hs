{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Agent.ClaudeCode.Transcript
    ( ClaudeTranscriptAccumulator
    , ClaudeTranscriptRecord
    , CompletedTurn(..)
    , TranscriptFact(..)
    , TurnAccumulator
    , accumulateClaudeTranscriptRecord
    , applyTranscriptFacts
    , claudeTranscriptAssistantText
    , claudeTranscriptComplete
    , claudeTranscriptSawEndTurn
    , claudeTranscriptText
    , claudeTranscriptTokenUsage
    , claudeTranscriptUsage
    , consumeTranscriptLine
    , decodeClaudeTranscriptLine
    , decodeTranscriptLine
    , emptyClaudeTranscriptAccumulator
    , emptyTurnAccumulator
    , finishTranscriptOnExit
    , parseClaudeTranscriptLine
    , turnAccumulatorError
    ) where

import Agent.Loop
    ( LoopEvent(..)
    , TokenUsage(..)
    , addTokenUsage
    , emptyTokenUsage
    )
import Control.Applicative ((<|>))
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
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncoding

-- | Provider-neutral facts extracted from one Claude Code JSONL record.
-- Unknown records and assistant thinking blocks produce no facts.
data TranscriptFact
    = AssistantTextFact !Text
    | ToolUseFact !ToolCall
    | ToolResultFact !ToolCallResult
    | UsageFact !Text !TokenUsage
    | AssistantEndTurnFact
    | TurnDurationFact
    | TerminalErrorFact !Text
    deriving (Eq, Show)

data CompletedTurn = CompletedTurn
    { assistantText :: !(Maybe Text)
    , tokenUsage :: !TokenUsage
    } deriving (Eq, Show)

data TurnAccumulator = TurnAccumulator
    { assistantChunks :: ![Text]
    , accumulatedUsage :: !TokenUsage
    , seenRecordIds :: !(Set Text)
    , seenUsageMessageIds :: !(Set Text)
    , startedToolIds :: !(Set Text)
    , sawAssistantEndTurn :: !Bool
    , alreadyCompleted :: !Bool
    , terminalError :: !(Maybe Text)
    } deriving (Eq, Show)

-- | Stable, abstract integration types used by the subprocess backend. A
-- decoded JSONL record may contain several facts because one assistant
-- message can include text, a tool use, usage, and a stop reason together.
data ClaudeTranscriptRecord =
    ClaudeTranscriptRecord !(Maybe Text) ![TranscriptFact]
    deriving (Eq, Show)

newtype ClaudeTranscriptAccumulator =
    ClaudeTranscriptAccumulator TurnAccumulator
    deriving (Eq, Show)

emptyTurnAccumulator :: TurnAccumulator
emptyTurnAccumulator = TurnAccumulator
    { assistantChunks = []
    , accumulatedUsage = emptyTokenUsage
    , seenRecordIds = Set.empty
    , seenUsageMessageIds = Set.empty
    , startedToolIds = Set.empty
    , sawAssistantEndTurn = False
    , alreadyCompleted = False
    , terminalError = Nothing
    }

emptyClaudeTranscriptAccumulator :: ClaudeTranscriptAccumulator
emptyClaudeTranscriptAccumulator =
    ClaudeTranscriptAccumulator emptyTurnAccumulator

-- | Decode a JSONL record. Syntactically valid but unrelated Claude Code
-- records are intentionally represented by an empty fact list.
decodeTranscriptLine :: ByteString -> Either Text [TranscriptFact]
decodeTranscriptLine bytes =
    snd <$> decodeTranscriptRecord bytes

decodeTranscriptRecord
    :: ByteString
    -> Either Text (Maybe Text, [TranscriptFact])
decodeTranscriptRecord bytes = do
    value <- case Aeson.eitherDecodeStrict' bytes of
        Left message ->
            Left ("Invalid Claude Code transcript JSON: " <> Text.pack message)
        Right decoded ->
            Right decoded
    case value of
        Aeson.Object object ->
            Right
                ( nonEmptyTextAt "uuid" object
                , factsFromObject object
                )
        _ ->
            Right (Nothing, [])

decodeClaudeTranscriptLine
    :: ByteString
    -> Either Text ClaudeTranscriptRecord
decodeClaudeTranscriptLine = parseClaudeTranscriptLine

parseClaudeTranscriptLine
    :: ByteString
    -> Either Text ClaudeTranscriptRecord
parseClaudeTranscriptLine bytes = do
    (recordId, facts) <- decodeTranscriptRecord bytes
    pure (ClaudeTranscriptRecord recordId facts)

consumeTranscriptLine
    :: TurnAccumulator
    -> ByteString
    -> Either Text (TurnAccumulator, [LoopEvent], Maybe CompletedTurn)
consumeTranscriptLine accumulator bytes =
    applyClaudeTranscriptRecord accumulator
        <$> parseClaudeTranscriptLine bytes

accumulateClaudeTranscriptRecord
    :: ClaudeTranscriptAccumulator
    -> ClaudeTranscriptRecord
    -> (ClaudeTranscriptAccumulator, [LoopEvent])
accumulateClaudeTranscriptRecord
    (ClaudeTranscriptAccumulator accumulator)
    record =
    let (next, events, _completion) =
            applyClaudeTranscriptRecord accumulator record
    in (ClaudeTranscriptAccumulator next, events)

applyClaudeTranscriptRecord
    :: TurnAccumulator
    -> ClaudeTranscriptRecord
    -> (TurnAccumulator, [LoopEvent], Maybe CompletedTurn)
applyClaudeTranscriptRecord accumulator
    (ClaudeTranscriptRecord recordId facts) =
        case recordId of
            Just identifier
                | Set.member identifier accumulator.seenRecordIds ->
                    (accumulator, [], Nothing)
                | otherwise ->
                    applyTranscriptFacts
                        accumulator
                            { seenRecordIds =
                                Set.insert
                                    identifier
                                    accumulator.seenRecordIds
                            }
                        facts
            Nothing ->
                applyTranscriptFacts accumulator facts

claudeTranscriptAssistantText
    :: ClaudeTranscriptAccumulator
    -> Maybe Text
claudeTranscriptAssistantText
    (ClaudeTranscriptAccumulator accumulator) =
    (completedTurn accumulator).assistantText

claudeTranscriptText
    :: ClaudeTranscriptAccumulator
    -> Maybe Text
claudeTranscriptText = claudeTranscriptAssistantText

claudeTranscriptTokenUsage
    :: ClaudeTranscriptAccumulator
    -> TokenUsage
claudeTranscriptTokenUsage
    (ClaudeTranscriptAccumulator accumulator) =
        accumulator.accumulatedUsage

claudeTranscriptUsage
    :: ClaudeTranscriptAccumulator
    -> TokenUsage
claudeTranscriptUsage = claudeTranscriptTokenUsage

claudeTranscriptSawEndTurn
    :: ClaudeTranscriptAccumulator
    -> Bool
claudeTranscriptSawEndTurn
    (ClaudeTranscriptAccumulator accumulator) =
        accumulator.sawAssistantEndTurn

claudeTranscriptComplete
    :: ClaudeTranscriptAccumulator
    -> Bool
claudeTranscriptComplete
    (ClaudeTranscriptAccumulator accumulator) =
        accumulator.alreadyCompleted

turnAccumulatorError :: TurnAccumulator -> Maybe Text
turnAccumulatorError accumulator =
    accumulator.terminalError

-- | Apply facts in transcript order, returning display-only loop events and a
-- terminal result only after Claude records both @end_turn@ and the subsequent
-- @turn_duration@ system record.
applyTranscriptFacts
    :: TurnAccumulator
    -> [TranscriptFact]
    -> (TurnAccumulator, [LoopEvent], Maybe CompletedTurn)
applyTranscriptFacts initial facts =
    applyFacts initial facts
  where
    applyFacts starting recordFacts =
        let (finalAccumulator, reversedEvents, completion) =
                foldl applyOne (starting, [], Nothing) recordFacts
        in (finalAccumulator, reverse reversedEvents, completion)

    applyOne
        (accumulator, events, completion)
        fact =
            case fact of
                AssistantTextFact text ->
                    ( accumulator
                        { assistantChunks =
                            accumulator.assistantChunks <> [text]
                        }
                    , TextDelta text : events
                    , completion
                    )
                ToolUseFact call ->
                    ( accumulator
                        { startedToolIds =
                            Set.insert call.callId accumulator.startedToolIds
                        }
                    , ToolStarted call : events
                    , completion
                    )
                ToolResultFact result
                    | Set.member
                        result.callId
                        accumulator.startedToolIds ->
                        ( accumulator
                        , ToolFinished result : events
                        , completion
                        )
                    | otherwise ->
                        (accumulator, events, completion)
                TerminalErrorFact message ->
                    ( accumulator { terminalError = Just message }
                    , events
                    , completion
                    )
                UsageFact messageId usage
                    | Set.member
                        messageId
                        accumulator.seenUsageMessageIds ->
                        (accumulator, events, completion)
                    | otherwise ->
                        ( accumulator
                            { accumulatedUsage =
                                addTokenUsage
                                    accumulator.accumulatedUsage
                                    usage
                            , seenUsageMessageIds =
                                Set.insert
                                    messageId
                                    accumulator.seenUsageMessageIds
                            }
                        , events
                        , completion
                        )
                AssistantEndTurnFact ->
                    ( accumulator { sawAssistantEndTurn = True }
                    , events
                    , completion
                    )
                TurnDurationFact
                    | accumulator.sawAssistantEndTurn
                    , not accumulator.alreadyCompleted ->
                        let completed = completedTurn accumulator
                        in ( accumulator { alreadyCompleted = True }
                           , events
                           , Just completed
                           )
                    | otherwise ->
                        (accumulator, events, completion)

-- | Accept an @end_turn@ observed immediately before process exit if Claude
-- could not flush the normal @turn_duration@ completion record.
finishTranscriptOnExit :: TurnAccumulator -> Maybe CompletedTurn
finishTranscriptOnExit accumulator
    | accumulator.sawAssistantEndTurn
    , not accumulator.alreadyCompleted =
        Just (completedTurn accumulator)
    | otherwise =
        Nothing

factsFromObject :: Aeson.Object -> [TranscriptFact]
factsFromObject object =
    case textAt "type" object of
        Just "assistant" ->
            assistantFacts object
        Just "user" ->
            userFacts object
        Just "system"
            | textAt "subtype" object == Just "turn_duration" ->
                [TurnDurationFact]
            | textAt "subtype" object == Just "api_error" ->
                apiErrorFacts object
            | textAt "subtype" object == Just "error" ->
                genericErrorFacts object
        Just "error" ->
            genericErrorFacts object
        _ ->
            []

assistantFacts :: Aeson.Object -> [TranscriptFact]
assistantFacts outer =
    case objectAt "message" outer of
        Nothing -> []
        Just message ->
            contentFacts message
                <> usageFacts message
                <> stopFacts message
  where
    contentFacts message =
        case KeyMap.lookup "content" message of
            Just (Aeson.Array values) ->
                concatMap assistantContentFacts (toList values)
            Just (Aeson.String text) ->
                [AssistantTextFact text | not (Text.null text)]
            _ ->
                []
    usageFacts message =
        case (nonEmptyTextAt "id" message, objectAt "usage" message) of
            (Just messageId, Just usageObject) ->
                [UsageFact messageId (usageFromObject usageObject)]
            _ ->
                []
    stopFacts message =
        case textAt "stop_reason" message of
            Just "end_turn" ->
                [AssistantEndTurnFact]
            Just "stop_sequence" ->
                [TerminalErrorFact
                    "Claude Code ended the response with stop_sequence before completing the turn."]
            _ ->
                []

apiErrorFacts :: Aeson.Object -> [TranscriptFact]
apiErrorFacts outer =
    case objectAt "error" outer of
        Nothing -> []
        Just err
            | terminal ->
                [TerminalErrorFact rendered]
            | otherwise ->
                []
          where
            status = intAt "status" err
            retryAttempt = intAt "retryAttempt" outer
            maxRetries = intAt "maxRetries" outer
            terminal =
                status `elem` [401, 403]
                    || (maxRetries > 0 && retryAttempt >= maxRetries)
                    || (maxRetries == 0 && intAt "retryInMs" outer == 0)
            rendered =
                "Claude Code API error"
                    <> (if status > 0
                        then " (HTTP " <> Text.pack (show status) <> ")"
                        else "")
                    <> ": "
                    <> maybe
                        "request failed"
                        (Text.take 2_000)
                        ( nonEmptyTextAt "formatted" err
                            <|> nonEmptyTextAt "message" err
                        )

genericErrorFacts :: Aeson.Object -> [TranscriptFact]
genericErrorFacts object =
    [TerminalErrorFact
        ("Claude Code error: " <> Text.take 2_000 message)]
  where
    message =
        maybe
            "request failed"
            id
            ( (objectAt "error" object >>= \err ->
                    nonEmptyTextAt "formatted" err
                        <|> nonEmptyTextAt "message" err)
                <|> nonEmptyTextAt "message" object
                <|> case KeyMap.lookup "error" object of
                    Just (Aeson.String text)
                        | not (Text.null (Text.strip text)) ->
                            Just (Text.strip text)
                    _ ->
                        Nothing
            )

assistantContentFacts :: Aeson.Value -> [TranscriptFact]
assistantContentFacts = \case
    Aeson.Object content ->
        case textAt "type" content of
            Just "text" ->
                case textAt "text" content of
                    Just text
                        | not (Text.null text) -> [AssistantTextFact text]
                    _ -> []
            Just "tool_use" ->
                maybe [] (pure . ToolUseFact) (toolCallFromObject content)
            -- Deliberately suppress private chain-of-thought.
            Just "thinking" ->
                []
            _ ->
                []
    _ ->
        []

userFacts :: Aeson.Object -> [TranscriptFact]
userFacts outer =
    case objectAt "message" outer >>= KeyMap.lookup "content" of
        Just (Aeson.Array values) ->
            mapMaybe toolResultFromValue (toList values)
        _ ->
            []

toolCallFromObject :: Aeson.Object -> Maybe ToolCall
toolCallFromObject object = do
    identifier <- nonEmptyTextAt "id" object
    toolName <- nonEmptyTextAt "name" object
    let inputValue =
            maybe (Aeson.Object KeyMap.empty) id (KeyMap.lookup "input" object)
    pure ToolCall
        { callId = identifier
        , name = toolName
        , arguments = encodeValue inputValue
        , callKind = FunctionCallKind
        , argumentsEncrypted = False
        }

toolResultFromValue :: Aeson.Value -> Maybe TranscriptFact
toolResultFromValue = \case
    Aeson.Object object
        | textAt "type" object == Just "tool_result" -> do
            identifier <- nonEmptyTextAt "tool_use_id" object
            let rawOutput =
                    maybe "" renderResultContent (KeyMap.lookup "content" object)
                renderedOutput
                    | boolAt "is_error" object == Just True =
                        "Error: " <> rawOutput
                    | otherwise =
                        rawOutput
            pure
                (ToolResultFact ToolCallResult
                    { callId = identifier
                    , output = renderedOutput
                    , callKind = FunctionCallKind
                    })
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

completedTurn :: TurnAccumulator -> CompletedTurn
completedTurn accumulator =
    let text = Text.concat accumulator.assistantChunks
    in CompletedTurn
        { assistantText =
            if Text.null text then Nothing else Just text
        , tokenUsage = accumulator.accumulatedUsage
        }

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

objectAt :: Key.Key -> Aeson.Object -> Maybe Aeson.Object
objectAt key object =
    case KeyMap.lookup key object of
        Just (Aeson.Object nested) -> Just nested
        _ -> Nothing

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
        Just value ->
            case Aeson.fromJSON value of
                Aeson.Success number -> max 0 number
                Aeson.Error _ -> 0
