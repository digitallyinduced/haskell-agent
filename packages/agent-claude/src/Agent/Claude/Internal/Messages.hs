-- | Translate the reusable SDK's typed message stream into the
-- provider-neutral events expected by the harness.
module Agent.Claude.Internal.Messages
    ( CompletedClaudeTurn(..)
    , ClaudeEventState
    , emptyClaudeEventState
    , claudeEventStateHasActivity
    , interpretClaudeTurn
    , streamClaudeProgress
    , streamClaudeMessage
    , remainingClaudeEvents
    ) where

import Agent.Loop (LoopEvent(..))
import Agent.Loop (NativeAgentStatus(..))
import Agent.Responses.Types
    ( FunctionCall(..)
    , FunctionCallOutput(..)
    , ItemStatus(..)
    , ResponseItem(..)
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
    , SystemMessage(..)
    , Usage(..)
    , UserMessage(..)
    , QueryMessageScope(..)
    , QueryProgress(..)
    , addUsage
    , emptyUsage
    , messageHasParentToolUseId
    , messageUuid
    , modelUsageToUsage
    )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)

data CompletedClaudeTurn = CompletedClaudeTurn
    { sessionId :: !Text
    , assistantText :: !(Maybe Text)
    , events :: ![LoopEvent]
    , tokenUsage :: !Usage
    , cumulativeModelUsage :: !(Maybe Usage)
    , toolItems :: ![ResponseItem]
    } deriving (Eq, Show)

data ClaudeEventState = ClaudeEventState
    { startedToolCalls :: !(Set Text)
    , startedToolDetails :: !(Map.Map Text ToolCall)
    , finishedToolCalls :: !(Set Text)
    , toolMessageIds :: !(Map.Map Text [Text])
    , warnedUnknownTypes :: !(Set Text)
    , nativeAgentCalls :: !(Map.Map Text ToolCall)
    } deriving (Eq, Show)

emptyClaudeEventState :: ClaudeEventState
emptyClaudeEventState =
    ClaudeEventState
        Set.empty Map.empty Set.empty Map.empty Set.empty Map.empty

claudeEventStateHasActivity :: ClaudeEventState -> Bool
claudeEventStateHasActivity state =
    not (Set.null state.startedToolCalls)

-- | Expose completed top-level tool messages as soon as Claude Code emits
-- them. In particular, its @Task@ tool remains in flight while a native
-- subagent runs, so buffering this event until the final result leaves the UI
-- blank for the entire child-agent lifetime.
streamClaudeMessage
    :: ClaudeEventState
    -> Message
    -> (ClaudeEventState, [LoopEvent])
streamClaudeMessage state message
    | messageHasParentToolUseId message = (state, [])
    | otherwise =
        let toolEvents = messageToolEvents message
            (nextState, events) =
                advanceToolEvents state toolEvents
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
        in (next, events <> nativeLifecycleEvents state events)
    QueryMessageObserved (QueryNested parent) message ->
        nestedNativeEvents state parent message
    QueryMessagesRetracted scope identifiers
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

nativeLifecycleEvents :: ClaudeEventState -> [LoopEvent] -> [LoopEvent]
nativeLifecycleEvents state = concatMap \case
    ToolStarted call
        | isNativeAgentName call.name ->
            [NativeAgentStarted
                call.callId
                Nothing
                (nativeAgentLabel call)
                (nativeAgentModel call)]
    ToolFinished result
        | Just call <- Map.lookup result.callId state.startedToolDetails
        , isNativeAgentName call.name ->
            [NativeAgentFinished
                result.callId
                (if "error" `Text.isInfixOf` Text.toLower result.output
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
            let tools = messageToolEvents message
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
jsonTextField key raw = do
    Aeson.Object object <-
        Aeson.decodeStrict (TextEncoding.encodeUtf8 raw)
    Aeson.String value <- KeyMap.lookup (Key.fromText key) object
    let stripped = Text.strip value
    if Text.null stripped then Nothing else Just stripped

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
            | UnknownContentBlock (Aeson.Object object) <- assistant.content
            , Just (Aeson.String contentType) <-
                [KeyMap.lookup "type" object]
            , "tool" `Text.isInfixOf` Text.toLower contentType
            ]
    _ -> Nothing

-- | Emit anything not already exposed by 'streamClaudeMessage'. Assistant
-- text remains completion-buffered because the SDK can supersede messages;
-- tool lifecycle events are safe to expose incrementally by stable call id.
remainingClaudeEvents
    :: ClaudeEventState
    -> CompletedClaudeTurn
    -> [LoopEvent]
remainingClaudeEvents state completed =
    reverse eventsRev <> textEvents
  where
    (_, eventsRev) = foldl' step (state, []) completed.events
    textEvents =
        [event | event@TextDelta{} <- completed.events]
    step (current, events) event = case event of
        ToolStarted call
            | Set.member call.callId current.startedToolCalls ->
                (current, events)
            | otherwise ->
                ( current
                    { startedToolCalls =
                        Set.insert call.callId current.startedToolCalls
                    }
                , event : events
                )
        ToolFinished result
            | Set.member result.callId current.finishedToolCalls ->
                (current, events)
            | otherwise ->
                ( current
                    { finishedToolCalls =
                        Set.insert result.callId current.finishedToolCalls
                    }
                , event : events
                )
        _ -> (current, events)

interpretClaudeTurn
    :: [Message]
    -> ResultMessage
    -> Either Text CompletedClaudeTurn
interpretClaudeTurn messages result = do
    let visibleMessages =
            filter (not . messageHasParentToolUseId) messages
    validateSubscriptionSource visibleMessages
    let
        bufferedAssistantText =
            Text.concat
                [ text
                | MessageAssistant assistant <- visibleMessages
                , assistant.error == Nothing
                , TextBlock{text} <- assistant.content
                ]
        assistantText =
            firstNonEmptyText
                [ maybe "" id result.result
                , bufferedAssistantText
                ]
        toolEvents =
            canonicalToolEvents
                (concatMap messageToolEvents visibleMessages)
        toolItems = canonicalToolItems visibleMessages
        events =
            toolEvents
                <> maybe [] (pure . TextDelta) assistantText
        cumulative =
            case Map.elems result.modelUsage of
                [] -> Nothing
                modelUsages ->
                    Just (foldl' addUsage emptyUsage
                        (map modelUsageToUsage modelUsages))
    pure CompletedClaudeTurn
        { sessionId = result.sessionId
        , assistantText
        , events
        , tokenUsage = result.usage
        , cumulativeModelUsage = cumulative
        , toolItems
        }

validateSubscriptionSource :: [Message] -> Either Text ()
validateSubscriptionSource messages =
    case
        [ system.apiKeySource
        | MessageSystem system <- messages
        , system.subtype == "init"
        ] of
        [] ->
            Left
                "Claude Code completed before confirming subscription authentication."
        sources
            | Just source <- firstUnexpected sources ->
                Left
                    ( "Claude Code selected non-subscription credential source "
                        <> source
                        <> "."
                    )
            | Nothing `elem` sources ->
                Left
                    "Claude Code did not identify its credential source."
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

data ClaudeToolEvent
    = ClaudeToolStarted !ToolCall
    | ClaudeToolFinished !ToolCallResult

messageToolEvents :: Message -> [ClaudeToolEvent]
messageToolEvents = \case
    MessageAssistant assistant
        | assistant.error == Nothing ->
            concatMap assistantBlockEvents assistant.content
    MessageUser user ->
        concatMap userBlockEvents user.content
    MessageStreamEvent stream ->
        streamEventToolEvents stream.event
    _ ->
        []

messageToolItems :: Message -> [ResponseItem]
messageToolItems = \case
    MessageAssistant assistant
        | assistant.error == Nothing ->
            concatMap assistantBlockItems assistant.content
    MessageUser user ->
        concatMap userBlockItems user.content
    _ -> []
  where
    assistantBlockItems = \case
        ToolUseBlock{toolUseId, name, input} ->
            [functionCallItem toolUseId name input]
        ServerToolUseBlock{toolUseId, name, input} ->
            [functionCallItem toolUseId name input]
        _ -> []
    userBlockItems = \case
        ToolResultBlock{toolUseId, content, isError} ->
            [functionOutputItem toolUseId content isError]
        ServerToolResultBlock{toolUseId, content} ->
            [functionOutputItem toolUseId content Nothing]
        _ -> []

canonicalToolItems :: [Message] -> [ResponseItem]
canonicalToolItems messages =
    filter keepItem items
  where
    items = concatMap messageToolItems messages
    started =
        Set.fromList
            [call.callId | FunctionCallItem call <- items]
    keepItem = \case
        FunctionCallOutputItem output ->
            Set.member output.callId started
        _ -> True

functionCallItem :: Text -> Text -> Aeson.Value -> ResponseItem
functionCallItem callId name input =
    FunctionCallItem FunctionCall
        { itemId = Nothing
        , callId
        , name
        , namespace = Nothing
        , arguments = encodeValue input
        , encryptedFunctionArgs = Nothing
        , status = Just ItemCompleted
        , extraFields =
            KeyMap.singleton
                "provider"
                (Aeson.String "claude-code")
        }

functionOutputItem
    :: Text
    -> Maybe Aeson.Value
    -> Maybe Bool
    -> ResponseItem
functionOutputItem callId content isError =
    FunctionCallOutputItem FunctionCallOutput
        { itemId = Nothing
        , callId
        , name = Nothing
        , namespace = Nothing
        , output = fromMaybe Aeson.Null content
        , status =
            Just $
                if isError == Just True
                    then ItemIncomplete
                    else ItemCompleted
        , extraFields =
            KeyMap.singleton
                "provider"
                (Aeson.String "claude-code")
        }

streamEventToolEvents :: Aeson.Value -> [ClaudeToolEvent]
streamEventToolEvents = \case
    Aeson.Object eventObject
        | Just (Aeson.String "content_block_start") <-
            KeyMap.lookup "type" eventObject
        , Just (Aeson.Object block) <-
            KeyMap.lookup "content_block" eventObject
        , Just (Aeson.String blockType) <- KeyMap.lookup "type" block
        , blockType `elem` ["tool_use", "server_tool_use"]
        , Just (Aeson.String toolUseId) <- KeyMap.lookup "id" block
        , Just (Aeson.String name) <- KeyMap.lookup "name" block ->
            [ ClaudeToolStarted ToolCall
                { callId = toolUseId
                , name
                , arguments =
                    encodeValue $
                        fromMaybe
                            (Aeson.Object KeyMap.empty)
                            (KeyMap.lookup "input" block)
                , callKind = FunctionCallKind
                , argumentsEncrypted = False
                }
            ]
    _ -> []

assistantBlockEvents :: ContentBlock -> [ClaudeToolEvent]
assistantBlockEvents = \case
    ToolUseBlock{toolUseId, name, input} ->
        [ ClaudeToolStarted ToolCall
            { callId = toolUseId
            , name
            , arguments = encodeValue input
            , callKind = FunctionCallKind
            , argumentsEncrypted = False
            }
        ]
    ServerToolUseBlock{toolUseId, name, input} ->
        [ ClaudeToolStarted ToolCall
            { callId = toolUseId
            , name
            , arguments = encodeValue input
            , callKind = FunctionCallKind
            , argumentsEncrypted = False
            }
        ]
    _ ->
        []

userBlockEvents :: ContentBlock -> [ClaudeToolEvent]
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

canonicalToolEvents :: [ClaudeToolEvent] -> [LoopEvent]
canonicalToolEvents toolEvents =
    events
  where
    (_, events) = advanceToolEvents emptyClaudeEventState toolEvents

advanceToolEvents
    :: ClaudeEventState
    -> [ClaudeToolEvent]
    -> (ClaudeEventState, [LoopEvent])
advanceToolEvents initialState toolEvents =
    let (state, eventsRev) =
            foldl' step (initialState, []) toolEvents
    in (state, reverse eventsRev)
  where
    step
        :: (ClaudeEventState, [LoopEvent])
        -> ClaudeToolEvent
        -> (ClaudeEventState, [LoopEvent])
    step (state, events) = \case
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
                    }
                , ToolFinished result : events
                )

renderResultContent :: Aeson.Value -> Text
renderResultContent = \case
    Aeson.String text -> text
    Aeson.Array values ->
        Text.intercalate "\n"
            (map renderResultContent (toList values))
    Aeson.Object object ->
        case KeyMap.lookup "text" object of
            Just (Aeson.String text) -> text
            _ -> encodeValue (Aeson.Object object)
    Aeson.Null -> ""
    other -> encodeValue other

encodeValue :: Aeson.Value -> Text
encodeValue =
    TextEncoding.decodeUtf8With lenientDecode
        . LazyByteString.toStrict
        . Aeson.encode

firstNonEmptyText :: [Text] -> Maybe Text
firstNonEmptyText values =
    case filter (not . Text.null) values of
        value : _ -> Just value
        [] -> Nothing
