-- | Message routing and transactional response accumulation.
module Claude.Agent.SDK.Internal.Query
    ( QueryAccumulator
    , emptyQueryAccumulator
    , consumeQueryMessage
    , consumeQueryMessageWithProgress
    , canonicalMessages
    ) where

import Claude.Agent.SDK.Errors (ClaudeSDKError(..))
import Claude.Agent.SDK.Types
    ( AssistantMessage(..)
    , ContentBlock(..)
    , Message(..)
    , MessageOrigin(..)
    , QueryMessageScope(..)
    , QueryProgress(..)
    , ResultMessage(..)
    , SystemMessage(..)
    , UserMessage(..)
    , messageHasParentToolUseId
    , messageParentToolUseId
    , messageUuid
    )
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

data MessageScope
    = TopLevelScope
    | NestedScope !(Maybe Text)
    deriving (Eq, Ord, Show)

data BufferedMessage = BufferedMessage
    { identifier :: !(Maybe Text)
    , scope :: !MessageScope
    , message :: !Message
    } deriving (Eq, Show)

data MessageBuffer = MessageBuffer
    { messagesRev :: ![BufferedMessage]
    , seenIds :: !(Set (MessageScope, Text))
    , retractedIds :: !(Set (MessageScope, Text))
    , globallyRetractedIds :: !(Set Text)
    } deriving (Eq, Show)

data QueryAccumulator = QueryAccumulator
    { ownBuffer :: !MessageBuffer
    , foreignBuffer :: !(Maybe MessageBuffer)
    , progressSeenIds :: !(Set (MessageScope, Text))
    } deriving (Eq, Show)

emptyQueryAccumulator :: QueryAccumulator
emptyQueryAccumulator = QueryAccumulator
    { ownBuffer = emptyMessageBuffer
    , foreignBuffer = Nothing
    , progressSeenIds = Set.empty
    }

emptyMessageBuffer :: MessageBuffer
emptyMessageBuffer = MessageBuffer
    { messagesRev = []
    , seenIds = Set.empty
    , retractedIds = Set.empty
    , globallyRetractedIds = Set.empty
    }

-- | Consume one parsed SDK message. A successful human result returns the
-- canonical message sequence after all known retractions have been applied.
-- Autonomous/background turns are isolated and discarded so their result
-- cannot terminate the query submitted by this client.
consumeQueryMessage
    :: QueryAccumulator
    -> Message
    -> Either
        ClaudeSDKError
        (QueryAccumulator, Maybe ([Message], ResultMessage))
consumeQueryMessage accumulator message =
    case accumulator.foreignBuffer of
        Just foreignMessages ->
            consumeForeignMessage accumulator foreignMessages message
        Nothing
            | messageHasParentToolUseId message -> do
                next <-
                    consumeBufferedMessage accumulator.ownBuffer message
                Right
                    ( accumulator { ownBuffer = next }
                    , Nothing
                    )
            | beginsForeignTurn message -> do
                foreignMessages <-
                    consumeBufferedMessage emptyMessageBuffer message
                Right
                    ( accumulator
                        { foreignBuffer = Just foreignMessages }
                    , Nothing
                    )
            | MessageResult result <- message
            , not (isHumanOrigin result.origin) ->
                -- A current Claude Code process can emit autonomous turns on
                -- the same stream. A detached result does not answer the
                -- prompt this query submitted.
                Right (accumulator, Nothing)
            | otherwise ->
                consumeOwnMessage accumulator message

-- | Consume a message and report live progress only when it belongs to the
-- submitted human turn. Autonomous/background records remain transactional
-- and invisible to the observer.
consumeQueryMessageWithProgress
    :: QueryAccumulator
    -> Message
    -> Either
        ClaudeSDKError
        ( QueryAccumulator
        , [QueryProgress]
        , Maybe ([Message], ResultMessage)
        )
consumeQueryMessageWithProgress accumulator message = do
    (next, completed) <- consumeQueryMessage accumulator message
    let progress = observedProgress accumulator message
        nextWithProgress = next
            { progressSeenIds =
                case progress of
                    [] -> accumulator.progressSeenIds
                    _ -> maybe
                        accumulator.progressSeenIds
                        (\identifier ->
                            Set.insert
                                (messageScope message, identifier)
                                accumulator.progressSeenIds)
                        (messageUuid message)
            }
    pure (nextWithProgress, progress, completed)

observedProgress :: QueryAccumulator -> Message -> [QueryProgress]
observedProgress accumulator message
    | not (belongsToOwnTurn accumulator message) = []
    | not (messageWouldBeObserved accumulator message) =
        retractionsFor message
    | otherwise =
        retractionsFor message
            <> [QueryMessageObserved (publicScope message) message]

belongsToOwnTurn :: QueryAccumulator -> Message -> Bool
belongsToOwnTurn accumulator message =
    case accumulator.foreignBuffer of
        Nothing ->
            messageHasParentToolUseId message
                || not (beginsForeignTurn message)
                    && not (isForeignResult message)
        Just _
            | messageHasParentToolUseId message -> False
            | MessageSystem SystemMessage{subtype = "init"} <- message -> True
            | MessageUser UserMessage{origin} <- message ->
                isExplicitHumanOrigin origin
            | MessageResult result <- message ->
                isHumanOrigin result.origin
            | otherwise -> False
  where
    isForeignResult = \case
        MessageResult result -> not (isHumanOrigin result.origin)
        _ -> False

messageWouldBeObserved :: QueryAccumulator -> Message -> Bool
messageWouldBeObserved accumulator message =
    case message of
        MessageResult{} -> messageHasParentToolUseId message
        MessageConversationReset{} -> False
        MessageControlRequest{} -> False
        MessageAssistant AssistantMessage{error = Just _} -> False
        MessageStreamEvent{} -> not (alreadySeen accumulator.ownBuffer message)
        _ -> not (alreadySeen accumulator.ownBuffer message)
  where
    alreadySeen current candidate =
        case messageUuid candidate of
            Nothing -> False
            Just identifier ->
                Set.member
                    (messageScope candidate, identifier)
                    accumulator.progressSeenIds
                    || Set.member identifier current.globallyRetractedIds
                    || Set.member
                        (messageScope candidate, identifier)
                        current.retractedIds

retractionsFor :: Message -> [QueryProgress]
retractionsFor = \case
    MessageAssistant assistant
        | not (null assistant.supersedes) ->
            [ QueryMessagesRetracted
                (Just (publicScope (MessageAssistant assistant)))
                assistant.supersedes
            ]
    MessageSystem system
        | system.subtype == "model_refusal_fallback"
        , not (null system.retractedMessageUuids) ->
            [QueryMessagesRetracted Nothing system.retractedMessageUuids]
    _ -> []

publicScope :: Message -> QueryMessageScope
publicScope message = case messageScope message of
    TopLevelScope -> QueryTopLevel
    NestedScope parent -> QueryNested parent

consumeOwnMessage
    :: QueryAccumulator
    -> Message
    -> Either
        ClaudeSDKError
        (QueryAccumulator, Maybe ([Message], ResultMessage))
consumeOwnMessage accumulator message =
    case message of
        MessageConversationReset _ ->
            Left $
                CLIProtocolError
                    "Claude Code reset the conversation while a query was active."
        MessageControlRequest _ ->
            Left $
                CLIProtocolError
                    "Claude Code requested interactive protocol input that this client does not support."
        MessageResult result
            | result.isError || result.subtype /= "success" ->
                Left ResultError
                    { subtype = result.subtype
                    , apiErrorStatus = result.apiErrorStatus
                    , errors = result.errors
                    , result = result.result
                    }
            | otherwise ->
                let finalMessages =
                        canonicalMessages accumulator
                            <> [MessageResult result]
                in Right
                    ( accumulator
                    , Just (finalMessages, result)
                    )
        _ -> do
            next <- consumeBufferedMessage accumulator.ownBuffer message
            Right
                ( accumulator { ownBuffer = next }
                , Nothing
                )

consumeForeignMessage
    :: QueryAccumulator
    -> MessageBuffer
    -> Message
    -> Either
        ClaudeSDKError
        (QueryAccumulator, Maybe ([Message], ResultMessage))
consumeForeignMessage accumulator foreignMessages message
    | messageHasParentToolUseId message = do
        next <- consumeBufferedMessage foreignMessages message
        Right
            ( accumulator { foreignBuffer = Just next }
            , Nothing
            )
    | otherwise =
        case message of
            MessageConversationReset _ ->
                Left $
                    CLIProtocolError
                        "Claude Code reset the conversation while a query was active."
            MessageControlRequest _ ->
                Left $
                    CLIProtocolError
                        "Claude Code requested interactive protocol input that this client does not support."
            MessageResult result
                | isHumanOrigin result.origin ->
                    consumeOwnMessage
                        accumulator { foreignBuffer = Nothing }
                        message
                | otherwise ->
                    Right
                        ( accumulator { foreignBuffer = Nothing }
                        , Nothing
                        )
            MessageSystem SystemMessage{subtype = "init"} -> do
                next <- consumeBufferedMessage accumulator.ownBuffer message
                Right
                    ( accumulator
                        { ownBuffer = next
                        , foreignBuffer = Just foreignMessages
                        }
                    , Nothing
                    )
            MessageUser UserMessage{origin}
                | isExplicitHumanOrigin origin -> do
                    next <-
                        consumeBufferedMessage accumulator.ownBuffer message
                    Right
                        ( accumulator
                            { ownBuffer = next
                            , foreignBuffer = Just foreignMessages
                            }
                        , Nothing
                        )
            _ -> do
                next <- consumeBufferedMessage foreignMessages message
                Right
                    ( accumulator { foreignBuffer = Just next }
                    , Nothing
                    )

consumeBufferedMessage
    :: MessageBuffer
    -> Message
    -> Either ClaudeSDKError MessageBuffer
consumeBufferedMessage buffer message =
    case message of
        MessageAssistant assistant ->
            let retracted =
                    retractMessages
                        (messageScope message)
                        buffer
                        assistant.supersedes
            in case assistant.error of
                Just _ ->
                    Right (markMessageSeen retracted message)
                Nothing ->
                    bufferRetractableMessage retracted message
        MessageSystem system
            | system.subtype == "model_refusal_fallback" ->
                Right $
                    bufferMessage
                        (retractMessagesGlobally
                            buffer
                            system.retractedMessageUuids)
                        message
        MessageUser _ ->
            bufferRetractableMessage buffer message
        MessageStreamEvent _ ->
            -- Partial stream events are not canonical response records and
            -- cannot be safely associated with later UUID retractions. The
            -- low-level 'receiveMessage' API still exposes them to callers
            -- that explicitly implement live partial-message handling.
            Right buffer
        _ ->
            Right (bufferMessage buffer message)

canonicalMessages :: QueryAccumulator -> [Message]
canonicalMessages accumulator =
    [ buffered.message
    | buffered <- reverse accumulator.ownBuffer.messagesRev
    ]

bufferRetractableMessage
    :: MessageBuffer
    -> Message
    -> Either ClaudeSDKError MessageBuffer
bufferRetractableMessage buffer message
    | messageHasVisibleContent message
    , messageUuid message == Nothing =
        Left $
            CLIProtocolError
                "Claude Code emitted a visible message without a wire UUID."
    | otherwise =
        Right (bufferMessage buffer message)

messageHasVisibleContent :: Message -> Bool
messageHasVisibleContent = \case
    MessageAssistant AssistantMessage{content} ->
        any visibleBlock content
    MessageUser UserMessage{content} ->
        any visibleBlock content
    _ ->
        False
  where
    visibleBlock = \case
        TextBlock{} -> True
        ToolUseBlock{} -> True
        ToolResultBlock{} -> True
        ServerToolUseBlock{} -> True
        ServerToolResultBlock{} -> True
        ThinkingBlock{} -> False
        UnknownContentBlock{} -> False

bufferMessage :: MessageBuffer -> Message -> MessageBuffer
bufferMessage buffer message =
    let scope = messageScope message
    in case messageUuid message of
        Just identifier
            | Set.member (scope, identifier) buffer.seenIds
                || Set.member
                    (scope, identifier)
                    buffer.retractedIds
                || Set.member
                    identifier
                    buffer.globallyRetractedIds ->
                buffer
            | otherwise ->
                buffer
                    { messagesRev =
                        BufferedMessage
                            { identifier = Just identifier
                            , scope
                            , message
                            }
                            : buffer.messagesRev
                    , seenIds =
                        Set.insert
                            (scope, identifier)
                            buffer.seenIds
                    }
        Nothing ->
            buffer
                { messagesRev =
                    BufferedMessage
                        { identifier = Nothing
                        , scope
                        , message
                        }
                        : buffer.messagesRev
                }

markMessageSeen :: MessageBuffer -> Message -> MessageBuffer
markMessageSeen buffer message =
    case messageUuid message of
        Nothing -> buffer
        Just identifier ->
            buffer
                { seenIds =
                    Set.insert
                        (messageScope message, identifier)
                        buffer.seenIds
                }

retractMessages
    :: MessageScope
    -> MessageBuffer
    -> [Text]
    -> MessageBuffer
retractMessages scope buffer identifiers =
    let retracted =
            Set.fromList
                [(scope, identifier) | identifier <- identifiers]
    in buffer
        { messagesRev =
            filter
                (\buffered ->
                    case buffered.identifier of
                        Nothing -> True
                        Just identifier ->
                            not
                                (Set.member
                                    (buffered.scope, identifier)
                                    retracted))
                buffer.messagesRev
        , retractedIds =
            Set.union retracted buffer.retractedIds
        }

retractMessagesGlobally
    :: MessageBuffer
    -> [Text]
    -> MessageBuffer
retractMessagesGlobally buffer identifiers =
    let retracted = Set.fromList identifiers
    in buffer
        { messagesRev =
            filter
                (\buffered ->
                    case buffered.identifier of
                        Nothing -> True
                        Just identifier ->
                            not (Set.member identifier retracted))
                buffer.messagesRev
        , globallyRetractedIds =
            Set.union retracted buffer.globallyRetractedIds
        }

messageScope :: Message -> MessageScope
messageScope message
    | messageHasParentToolUseId message =
        NestedScope (messageParentToolUseId message)
    | otherwise =
        TopLevelScope

beginsForeignTurn :: Message -> Bool
beginsForeignTurn = \case
    MessageUser UserMessage{origin} ->
        not (isHumanOrigin origin)
    _ ->
        False

isHumanOrigin :: Maybe MessageOrigin -> Bool
isHumanOrigin = \case
    Nothing -> True
    Just origin -> origin.kind == "human"

isExplicitHumanOrigin :: Maybe MessageOrigin -> Bool
isExplicitHumanOrigin = \case
    Just origin -> origin.kind == "human"
    Nothing -> False
