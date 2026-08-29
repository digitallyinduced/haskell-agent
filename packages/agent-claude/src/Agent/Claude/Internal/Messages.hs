-- | Translate the reusable SDK's typed message stream into the
-- provider-neutral events expected by the harness.
module Agent.Claude.Internal.Messages
    ( CompletedClaudeTurn(..)
    , ClaudeInterpretationError(..)
    , ClaudeLiveEvent(..)
    , ClaudeEventState
    , emptyClaudeEventState
    , claudeEventStateHasActivity
    , interpretClaudeTurn
    , interpretClaudeTurnWithCredentialValidation
    , streamClaudeProgress
    , streamClaudeMessage
    , remainingClaudeEvents
    , assistantMessageItem
    ) where

import Agent.Loop (LoopEvent(..))
import Agent.Loop (NativeAgentStatus(..))
import Agent.Json
    ( RawJson
    , rawJsonBytes
    , rawJsonFromEncoding
    )
import qualified Agent.Json.Decode as Json
import Agent.Responses.Types
    ( FunctionCall(..)
    , FunctionCallOutput(..)
    , ItemStatus(..)
    , MessageContent(..)
    , ResponseContentPart(..)
    , ResponseItem(..)
    , ResponseMessage(..)
    , ResponseRole(..)
    )
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , ToolCallResult(..)
    )
import Claude.Agent.SDK.Types
    ( AssistantMessage(..)
    , ContentBlock(..)
    , Message(..)
    , ResultMessage(..)
    , StreamEvent(..)
    , StreamToolUse(..)
    , SystemMessage(..)
    , Usage(..)
    , ToolResultContent(..)
    , UserMessage(..)
    , QueryMessageScope(..)
    , QueryProgress(..)
    , addUsage
    , emptyUsage
    , messageHasParentToolUseId
    , messageUuid
    , modelUsageToUsage
    )
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

data CompletedClaudeTurn = CompletedClaudeTurn
    { sessionId :: !Text
    , assistantText :: !(Maybe Text)
    -- | Every canonical top-level record in wire order. Completion replays
    -- whatever 'streamClaudeMessage' has not already exposed.
    , liveEvents :: ![ClaudeLiveEvent]
    , tokenUsage :: !Usage
    , cumulativeModelUsage :: !(Maybe Usage)
    -- | Host transcript items for this turn in wire order: assistant text
    -- interleaved with tool calls and outputs, always ending in at least
    -- one assistant message.
    , turnItems :: ![ResponseItem]
    } deriving (Eq, Show)

-- | A completed result can be rejected either because the credential source
-- was not an allowed subscription, or because the stream violated the
-- protocol.  Keeping these cases typed prevents callers from misreporting
-- malformed output as an authentication failure.
data ClaudeInterpretationError
    = ClaudeAuthenticationFailure !Text
    | ClaudeProtocolFailure !Text
    deriving (Eq, Show)

-- | Kind of the most recent streamed delta since the last tool start. The
-- renderer extends the current block for consecutive deltas of one kind, so
-- separate wire blocks need an explicit paragraph break between them.
data LiveDeltaKind
    = LiveTextDelta
    | LiveThinkingDelta
    deriving (Eq, Show)

data ClaudeEventState = ClaudeEventState
    { startedToolCalls :: !(Set Text)
    , startedToolDetails :: !(Map.Map Text ToolCall)
    , finishedToolCalls :: !(Set Text)
    , toolMessageIds :: !(Map.Map Text [Text])
    , warnedUnknownTypes :: !(Set Text)
    , nativeAgentCalls :: !(Map.Map Text ToolCall)
    -- | Wire UUIDs of assistant records whose text or thinking is already
    -- displayed.
    , streamedMessageIds :: !(Set Text)
    , lastDelta :: !(Maybe LiveDeltaKind)
    -- | Set after displayed text was retracted: the attempt has been
    -- discarded, so nothing more is exposed live and completion replays the
    -- surviving records in order.
    , replayAtCompletion :: !Bool
    -- | True once any user-visible response activity has been emitted.
    , emittedActivity :: !Bool
    } deriving (Eq, Show)

emptyClaudeEventState :: ClaudeEventState
emptyClaudeEventState =
    ClaudeEventState
        { startedToolCalls = Set.empty
        , startedToolDetails = Map.empty
        , finishedToolCalls = Set.empty
        , toolMessageIds = Map.empty
        , warnedUnknownTypes = Set.empty
        , nativeAgentCalls = Map.empty
        , streamedMessageIds = Set.empty
        , lastDelta = Nothing
        , replayAtCompletion = False
        , emittedActivity = False
        }

claudeEventStateHasActivity :: ClaudeEventState -> Bool
claudeEventStateHasActivity state =
    state.emittedActivity

-- | Expose completed top-level records as soon as Claude Code emits them.
-- Claude Code writes one assistant record per content block and its final
-- @result@ carries only the last text block, so text and thinking must be
-- shown as they arrive or everything the model said before its last tool
-- call is lost. Tools are exposed incrementally by stable call id; in
-- particular the @Task@ tool remains in flight while a native subagent runs,
-- so buffering it until the final result leaves the UI blank for the entire
-- child-agent lifetime.
streamClaudeMessage
    :: ClaudeEventState
    -> Message
    -> (ClaudeEventState, [LoopEvent])
streamClaudeMessage state message
    | messageHasParentToolUseId message = (state, [])
    | state.replayAtCompletion = (state, [])
    | otherwise =
        let toolEvents = messageLiveEvents message
            (nextState, events) =
                advanceLiveEvents state toolEvents
            messageIds =
                maybe [] (\identifier -> [identifier]) (messageUuid message)
            withIds =
                foldl'
                    (\current toolEvent ->
                        case toolEvent of
                            ClaudeToolStarted call ->
                                current
                                    { toolMessageIds =
                                        Map.insertWith
                                            (<>)
                                            call.callId
                                            messageIds
                                            current.toolMessageIds
                                    }
                            _ -> current)
                    nextState
                    toolEvents
        in appendUnknownWarning withIds message events

-- | Apply the query layer's classification before projecting a live Claude
-- record. Retraction identifiers refer to wire message UUIDs, not tool call
-- IDs, so the ledger keeps both and can remove the corresponding UI block.
streamClaudeProgress
    :: ClaudeEventState
    -> QueryProgress
    -> (ClaudeEventState, [LoopEvent])
streamClaudeProgress state = \case
    QueryMessageObserved QueryTopLevel message ->
        let (next, events) = streamClaudeMessage state message
        in (next, events <> nativeLifecycleEvents next.startedToolDetails events)
    QueryMessageObserved (QueryNested parent) message ->
        nestedNativeEvents state parent message
    QueryMessagesRetracted scope identifiers
        | scope == Nothing || scope == Just QueryTopLevel
        , not state.replayAtCompletion
        , state.emittedActivity
        , any (`Set.member` state.streamedMessageIds) identifiers
            || null identifiers ->
            -- Displayed text has no per-block retraction event. Discard the
            -- whole attempt and replay the surviving records at completion.
            ( emptyClaudeEventState
                { warnedUnknownTypes = state.warnedUnknownTypes
                , replayAtCompletion = True
                }
            , [ResponseAttemptDiscarded]
            )
        | scope == Nothing || scope == Just QueryTopLevel ->
            let calls =
                    [ callId
                    | (callId, messageIds) <-
                        Map.toList state.toolMessageIds
                    , any (`elem` identifiers) messageIds
                    ]
                next =
                    state
                        { startedToolCalls =
                            foldr Set.delete state.startedToolCalls calls
                        , startedToolDetails =
                            foldr Map.delete state.startedToolDetails calls
                        , finishedToolCalls =
                            foldr Set.delete state.finishedToolCalls calls
                        , toolMessageIds =
                            foldr Map.delete state.toolMessageIds calls
                        }
                nativeRetractions =
                    [ NativeAgentFinished callId NativeAgentCancelled
                    | callId <- calls
                    , Just call <- [Map.lookup callId state.startedToolDetails]
                    , isNativeAgentName call.name
                    ]
            in (next, map ToolRetracted calls <> nativeRetractions)
    QueryMessagesRetracted _ _ ->
        (state, [])

nativeLifecycleEvents
    :: Map.Map Text ToolCall
    -> [LoopEvent]
    -> [LoopEvent]
nativeLifecycleEvents startedToolDetails = concatMap \case
    ToolStarted call
        | isNativeAgentName call.name ->
            [NativeAgentStarted
                call.callId
                Nothing
                (nativeAgentLabel call)
                (nativeAgentModel call)]
    ToolFinished result
        | Just call <- Map.lookup result.callId startedToolDetails
        , isNativeAgentName call.name ->
            [NativeAgentFinished
                result.callId
                (if "error:" `Text.isPrefixOf`
                        Text.toLower (Text.stripStart result.output)
                    then NativeAgentFailed
                    else NativeAgentCompleted)]
    _ -> []

nestedNativeEvents
    :: ClaudeEventState
    -> Maybe Text
    -> Message
    -> (ClaudeEventState, [LoopEvent])
nestedNativeEvents state parent message =
    case parent of
        Nothing -> (state, [])
        Just identifier ->
            let tools = messageLiveEvents message
                (nextState, childLifecycle) =
                    foldl' (\(current, events) -> \case
                    ClaudeToolStarted call
                        | isNativeAgentName call.name ->
                            ( current
                                { nativeAgentCalls =
                                    Map.insert
                                        call.callId
                                        call
                                        current.nativeAgentCalls
                                }
                            , events
                                <> [ NativeAgentStarted
                                        call.callId
                                        (Just identifier)
                                        (nativeAgentLabel call)
                                        (nativeAgentModel call)
                                   ]
                            )
                    ClaudeToolFinished result
                        | Just call <- Map.lookup
                            result.callId
                            current.nativeAgentCalls
                        , isNativeAgentName call.name ->
                            ( current
                                { nativeAgentCalls =
                                    Map.delete
                                        result.callId
                                        current.nativeAgentCalls
                                }
                            , events
                                <> [ NativeAgentFinished
                                        result.callId
                                        NativeAgentCompleted
                                   ]
                            )
                    _ -> (current, events))
                        (state, [])
                        tools
                outputEvents = case message of
                    MessageAssistant assistant
                        | assistant.error == Nothing ->
                            [ NativeAgentOutput identifier text
                            | TextBlock{text} <- assistant.content
                            , not (Text.null text)
                            ]
                    MessageUser user ->
                        [ NativeAgentOutput identifier output
                        | ToolResultBlock{content} <- user.content
                        , let output = maybe "" renderResultContent content
                        , not (Text.null output)
                        ]
                    _ -> []
            in (nextState, outputEvents <> childLifecycle)

isNativeAgentName :: Text -> Bool
isNativeAgentName name =
    Text.toLower name `elem` ["agent", "task"]

nativeAgentLabel :: ToolCall -> Text
nativeAgentLabel call =
    fromMaybe call.name (jsonTextField "description" call.arguments)

nativeAgentModel :: ToolCall -> Maybe Text
nativeAgentModel call = jsonTextField "model" call.arguments

jsonTextField :: Text -> Text -> Maybe Text
jsonTextField key raw =
    either (const Nothing) id $
        Json.decodeText
            (Json.object $
                (Json.atKeyOptional key $
                    Json.withType \case
                        Json.VString -> do
                            value <- Text.strip <$> Json.text
                            pure $
                                if Text.null value
                                    then Nothing
                                    else Just value
                        _ -> pure Nothing)
                    >>= pure . (>>= id))
            raw

appendUnknownWarning
    :: ClaudeEventState
    -> Message
    -> [LoopEvent]
    -> (ClaudeEventState, [LoopEvent])
appendUnknownWarning state message events =
    case unknownToolLikeType message of
        Just contentType
            | not (Set.member contentType state.warnedUnknownTypes) ->
                ( state
                    { warnedUnknownTypes =
                        Set.insert contentType state.warnedUnknownTypes
                    }
                , events
                    <> [ WarningRaised
                            ( "Claude Code emitted unsupported tool-like content `"
                                <> contentType
                                <> "`."
                            )
                       ]
                )
        _ -> (state, events)

unknownToolLikeType :: Message -> Maybe Text
unknownToolLikeType = \case
    MessageAssistant assistant ->
        firstNonEmptyText
            [ contentType
            | UnknownContentBlock{contentType = Just contentType}
                <- assistant.content
            , "tool" `Text.isInfixOf` Text.toLower contentType
            ]
    _ -> Nothing

-- | Emit anything not already exposed by 'streamClaudeMessage', in wire
-- order. After a discarded attempt this replays the whole surviving turn.
-- When Claude Code produced no text blocks at all, the result record's text
-- is the only reply and is exposed here.
remainingClaudeEvents
    :: ClaudeEventState
    -> CompletedClaudeTurn
    -> [LoopEvent]
remainingClaudeEvents state completed =
    events
        <> nativeLifecycleEvents next.startedToolDetails events
        <> resultText
  where
    (next, events) =
        advanceLiveEvents
            state { replayAtCompletion = False }
            completed.liveEvents
    resultText =
        [ TextDelta text
        | not (any isTextEvent completed.liveEvents)
        , Just text <- [completed.assistantText]
        ]
    isTextEvent = \case
        ClaudeText{} -> True
        _ -> False

interpretClaudeTurn
    :: [Message]
    -> ResultMessage
    -> Either ClaudeInterpretationError CompletedClaudeTurn
interpretClaudeTurn = interpretClaudeTurnWithCredentialValidation True

interpretClaudeTurnWithCredentialValidation
    :: Bool
    -> [Message]
    -> ResultMessage
    -> Either ClaudeInterpretationError CompletedClaudeTurn
interpretClaudeTurnWithCredentialValidation validateCredential messages result = do
    let visibleMessages =
            filter (not . messageHasParentToolUseId) messages
    if validateCredential
        then validateSubscriptionSource visibleMessages
        else Right ()
    if Text.null (Text.strip result.sessionId)
        then Left
            (ClaudeProtocolFailure
                "Claude Code returned a result without a session id.")
        else Right ()
    let
        liveEvents = concatMap messageLiveEvents visibleMessages
        -- Claude Code's result text is only the last text block; the
        -- canonical records carry everything the model said this turn.
        assistantText =
            firstNonEmptyText
                [ Text.intercalate
                    "\n\n"
                    [text | ClaudeText _ text <- liveEvents]
                , fromMaybe "" result.result
                ]
        canonicalItems = canonicalTurnItems visibleMessages
        turnItems
            | any isAssistantItem canonicalItems = canonicalItems
            | otherwise =
                canonicalItems <> [assistantMessageItem assistantText]
        cumulative =
            case Map.elems result.modelUsage of
                [] -> Nothing
                modelUsages ->
                    Just (foldl' addUsage emptyUsage
                        (map modelUsageToUsage modelUsages))
    pure CompletedClaudeTurn
        { sessionId = result.sessionId
        , assistantText
        , liveEvents
        , tokenUsage = result.usage
        , cumulativeModelUsage = cumulative
        , turnItems
        }

validateSubscriptionSource :: [Message] -> Either ClaudeInterpretationError ()
validateSubscriptionSource messages =
    case
        [ system.apiKeySource
        | MessageSystem system <- messages
        , system.subtype == "init"
        ] of
        [] ->
            Left (ClaudeAuthenticationFailure
                "Claude Code completed before confirming subscription authentication."
                )
        sources
            | Just source <- firstUnexpected sources ->
                Left (ClaudeAuthenticationFailure
                    ( "Claude Code selected non-subscription credential source "
                        <> source
                        <> "."
                    )
                    )
            | Nothing `elem` sources ->
                Left (ClaudeAuthenticationFailure
                    "Claude Code did not identify its credential source."
                    )
            | otherwise ->
                Right ()
  where
    firstUnexpected =
        foldr
            (\source found ->
                case source of
                    Just "none" -> found
                    Just value -> Just value
                    Nothing -> found)
            Nothing

-- | One displayable unit of a canonical Claude record. Text and thinking
-- carry their record's wire UUID so completion can skip records that were
-- already streamed live.
data ClaudeLiveEvent
    = ClaudeToolStarted !ToolCall
    | ClaudeToolFinished !ToolCallResult
    | ClaudeText !(Maybe Text) !Text
    | ClaudeThinking !(Maybe Text) !Text
    deriving (Eq, Show)

messageLiveEvents :: Message -> [ClaudeLiveEvent]
messageLiveEvents = \case
    MessageAssistant assistant
        | assistant.error == Nothing ->
            concatMap (assistantBlockEvents assistant.uuid) assistant.content
    MessageUser user ->
        concatMap userBlockEvents user.content
    MessageStreamEvent stream ->
        streamEventToolEvents stream
    _ ->
        []

-- | One transcript-relevant unit of a canonical record.
data TurnUnit
    = TurnText !Text
    | TurnItem !ResponseItem

messageTurnUnits :: Message -> [TurnUnit]
messageTurnUnits = \case
    MessageAssistant assistant
        | assistant.error == Nothing ->
            concatMap assistantBlockUnits assistant.content
    MessageUser user ->
        concatMap userBlockUnits user.content
    _ -> []
  where
    assistantBlockUnits = \case
        TextBlock{text}
            | Text.null (Text.strip text) -> []
            | otherwise -> [TurnText text]
        ToolUseBlock{toolUseId, name, input} ->
            [TurnItem (functionCallItem toolUseId name input)]
        ServerToolUseBlock{toolUseId, name, input} ->
            [TurnItem (functionCallItem toolUseId name input)]
        _ -> []
    userBlockUnits = \case
        ToolResultBlock{toolUseId, content, isError} ->
            [TurnItem (functionOutputItem toolUseId content isError)]
        ServerToolResultBlock{toolUseId, content} ->
            [TurnItem (functionOutputItem toolUseId content Nothing)]
        _ -> []

-- | Transcript items in wire order. Text between tool calls becomes its own
-- assistant message so a replayed transcript keeps the order the user saw
-- live; consecutive text blocks merge with the same paragraph break the
-- live view inserts. Outputs without a visible call are dropped.
canonicalTurnItems :: [Message] -> [ResponseItem]
canonicalTurnItems messages =
    go Nothing (filter keepUnit units)
  where
    units = concatMap messageTurnUnits messages
    started =
        Set.fromList
            [call.callId | TurnItem (FunctionCallItem call) <- units]
    keepUnit = \case
        TurnItem (FunctionCallOutputItem output) ->
            Set.member output.callId started
        _ -> True
    go pending [] = flush pending []
    go pending (TurnText text : rest) =
        go (Just (maybe text (\previous -> previous <> "\n\n" <> text) pending)) rest
    go pending (TurnItem item : rest) =
        flush pending (item : go Nothing rest)
    flush pending rest =
        maybe rest (\text -> assistantMessageItem (Just text) : rest) pending

isAssistantItem :: ResponseItem -> Bool
isAssistantItem = \case
    MessageItem message -> message.role == RoleAssistant
    _ -> False

assistantMessageItem :: Maybe Text -> ResponseItem
assistantMessageItem assistantText =
    MessageItem ResponseMessage
        { messageId = Nothing
        , content = MessageContentParts
            [ OutputTextPart
                { text = fromMaybe "" assistantText
                , annotations = Nothing
                , logprobs = Nothing
                }
            ]
        , role = RoleAssistant
        , status = Just ItemCompleted
        , phase = Nothing
        , passthrough = Nothing
        }

functionCallItem :: Text -> Text -> RawJson -> ResponseItem
functionCallItem callId name input =
    FunctionCallItem FunctionCall
        { itemId = Nothing
        , callId
        , name
        , namespace = Nothing
        , provider = Just "claude-code"
        , arguments = rawJsonText input
        , encryptedFunctionArgs = Nothing
        , status = Just ItemCompleted
        }

-- | Persist the text projection rather than the wire JSON: structured
-- results (tool references, image reads carrying base64 payloads) are
-- replayed into the transcript view and imported into later prompts as
-- plain text.
functionOutputItem
    :: Text
    -> Maybe ToolResultContent
    -> Maybe Bool
    -> ResponseItem
functionOutputItem callId content isError =
    FunctionCallOutputItem FunctionCallOutput
        { itemId = Nothing
        , callId
        , name = Nothing
        , namespace = Nothing
        , provider = Just "claude-code"
        , output =
            maybe
                (rawJsonFromEncoding Aeson.null_)
                (rawJsonFromEncoding . Aeson.text . renderResultContent)
                content
        , status =
            Just $
                if isError == Just True
                    then ItemIncomplete
                    else ItemCompleted
        }

streamEventToolEvents :: StreamEvent -> [ClaudeLiveEvent]
streamEventToolEvents StreamEvent{streamToolUse = Just toolUse} =
    [ ClaudeToolStarted ToolCall
        { callId = toolUse.toolUseId
        , name = toolUse.name
        , arguments = rawJsonText toolUse.input
        , callKind = FunctionCallKind
        , argumentsEncrypted = False
        }
    ]
streamEventToolEvents _ = []

assistantBlockEvents :: Maybe Text -> ContentBlock -> [ClaudeLiveEvent]
assistantBlockEvents messageId = \case
    TextBlock{text}
        | Text.null (Text.strip text) -> []
        | otherwise -> [ClaudeText messageId text]
    ThinkingBlock{thinking}
        | Text.null (Text.strip thinking) -> []
        | otherwise -> [ClaudeThinking messageId thinking]
    ToolUseBlock{toolUseId, name, input} ->
        [ ClaudeToolStarted ToolCall
            { callId = toolUseId
            , name
            , arguments = rawJsonText input
            , callKind = FunctionCallKind
            , argumentsEncrypted = False
            }
        ]
    ServerToolUseBlock{toolUseId, name, input} ->
        [ ClaudeToolStarted ToolCall
            { callId = toolUseId
            , name
            , arguments = rawJsonText input
            , callKind = FunctionCallKind
            , argumentsEncrypted = False
            }
        ]
    _ ->
        []

userBlockEvents :: ContentBlock -> [ClaudeLiveEvent]
userBlockEvents = \case
    ToolResultBlock{toolUseId, content, isError} ->
        let rawOutput = maybe "" renderResultContent content
            output
                | isError == Just True = "Error: " <> rawOutput
                | otherwise = rawOutput
        in
            [ ClaudeToolFinished ToolCallResult
                { callId = toolUseId
                , output
                , callKind = FunctionCallKind
                }
            ]
    ServerToolResultBlock{toolUseId, content} ->
        let rawOutput = maybe "" renderResultContent content
        in
            [ ClaudeToolFinished ToolCallResult
                { callId = toolUseId
                , output = rawOutput
                , callKind = FunctionCallKind
                }
            ]
    _ ->
        []

-- | Project a batch of live events (one record while streaming, the whole
-- turn at completion) onto loop events, skipping what is already displayed.
-- Text and thinking are skipped per record, judged against the state before
-- the batch, so every block of a multi-block record is emitted together.
advanceLiveEvents
    :: ClaudeEventState
    -> [ClaudeLiveEvent]
    -> (ClaudeEventState, [LoopEvent])
advanceLiveEvents initialState toolEvents =
    let (state, eventsRev) =
            foldl' step (initialState, []) toolEvents
    in (state, reverse eventsRev)
  where
    alreadyStreamed :: Maybe Text -> Bool
    alreadyStreamed =
        maybe False (`Set.member` initialState.streamedMessageIds)

    markStreamed :: Maybe Text -> ClaudeEventState -> ClaudeEventState
    markStreamed messageId state =
        case messageId of
            Nothing -> state
            Just identifier ->
                state
                    { streamedMessageIds =
                        Set.insert identifier state.streamedMessageIds
                    }

    paragraph :: LiveDeltaKind -> ClaudeEventState -> Text -> Text
    paragraph kind state text
        | state.lastDelta == Just kind = "\n\n" <> text
        | otherwise = text

    step
        :: (ClaudeEventState, [LoopEvent])
        -> ClaudeLiveEvent
        -> (ClaudeEventState, [LoopEvent])
    step (state, events) = \case
        ClaudeText messageId text
            | alreadyStreamed messageId -> (state, events)
            | otherwise ->
                ( (markStreamed messageId state)
                    { lastDelta = Just LiveTextDelta
                    , emittedActivity = True
                    }
                , TextDelta (paragraph LiveTextDelta state text) : events
                )
        ClaudeThinking messageId thinking
            | alreadyStreamed messageId -> (state, events)
            | otherwise ->
                ( (markStreamed messageId state)
                    { lastDelta = Just LiveThinkingDelta
                    , emittedActivity = True
                    }
                , ReasoningDelta (paragraph LiveThinkingDelta state thinking)
                    : events
                )
        ClaudeToolStarted call
            | Just previous <- Map.lookup
                call.callId
                state.startedToolDetails ->
                    if previous == call
                        then (state, events)
                        else
                            ( state
                                { startedToolDetails =
                                    Map.insert
                                        call.callId
                                        call
                                        state.startedToolDetails
                                }
                            , ToolUpdated call : events
                            )
            | otherwise ->
                ( state
                    { startedToolCalls =
                        Set.insert call.callId state.startedToolCalls
                    , startedToolDetails =
                        Map.insert
                            call.callId
                            call
                            state.startedToolDetails
                    , lastDelta = Nothing
                    , emittedActivity = True
                    }
                , ToolStarted call : events
                )
        ClaudeToolFinished result
            | not (Set.member result.callId state.startedToolCalls)
                || Set.member result.callId state.finishedToolCalls ->
                (state, events)
            | otherwise ->
                ( state
                    { finishedToolCalls =
                        Set.insert result.callId state.finishedToolCalls
                    , emittedActivity = True
                    }
                , ToolFinished result : events
                )

renderResultContent :: ToolResultContent -> Text
renderResultContent = (.renderedText)

rawJsonText :: RawJson -> Text
rawJsonText = TextEncoding.decodeUtf8 . rawJsonBytes

firstNonEmptyText :: [Text] -> Maybe Text
firstNonEmptyText values =
    case filter (not . Text.null) values of
        value : _ -> Just value
        [] -> Nothing
