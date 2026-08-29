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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
    , foreignRoutes :: !(Map RouteKey MessageBuffer)
    , currentForeignRoute :: !(Maybe RouteKey)
    , toolOwners :: !(Map Text RouteOwner)
    , progressSeenIds :: !(Set (MessageScope, Text))
    } deriving (Eq, Show)

data RouteKey = RouteKey
    { routeKind :: !Text
    , routeIdentifiers :: !(Map Text Text)
    } deriving (Eq, Ord, Show)

data RouteOwner
    = OwnRoute
    | ForeignRoute !RouteKey
    deriving (Eq, Ord, Show)

data RouteDecision
    = RouteOwn
    | RouteForeign !RouteKey
    | RouteHidden
    deriving (Eq, Show)

emptyQueryAccumulator :: QueryAccumulator
emptyQueryAccumulator = QueryAccumulator
    { ownBuffer = emptyMessageBuffer
    , foreignRoutes = Map.empty
    , currentForeignRoute = Nothing
    , toolOwners = Map.empty
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
    case message of
        MessageConversationReset _
            | not (messageHasParentToolUseId message) ->
                Right
                    ( resetQueryAccumulator accumulator
                    , Nothing
                    )
        _ ->
            case routeMessage accumulator message of
                RouteOwn ->
                    consumeOwnMessage accumulator message
                RouteForeign key ->
                    consumeForeignMessage accumulator key message
                RouteHidden ->
                    Right (accumulator, Nothing)

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
    let progress = case message of
            MessageConversationReset reset
                | not (messageHasParentToolUseId message) ->
                    [QueryConversationReset reset]
            _ ->
                observedProgress accumulator message
        nextWithProgress = next
            { progressSeenIds = applyProgressSeen
                next.progressSeenIds
                progress
            }
    pure (nextWithProgress, progress, completed)

observedProgress :: QueryAccumulator -> Message -> [QueryProgress]
observedProgress accumulator message
    | not (belongsToOwnTurn accumulator message) = []
    | not (messageWouldBeObserved accumulator message) =
        retractionsFor accumulator message
    | otherwise =
        retractionsFor accumulator message
            <> [QueryMessageObserved (publicScope message) message]

applyProgressSeen
    :: Set (MessageScope, Text)
    -> [QueryProgress]
    -> Set (MessageScope, Text)
applyProgressSeen = foldl' step
  where
    step seen = \case
        QueryMessageObserved _ message ->
            maybe seen
                (\identifier ->
                    Set.insert (messageScope message, identifier) seen)
                (messageUuid message)
        QueryMessagesRetracted scope identifiers ->
            case scope of
                Nothing ->
                    Set.filter
                        (\(_, identifier) -> identifier `notElem` identifiers)
                        seen
                Just public ->
                    let internal = case public of
                            QueryTopLevel -> TopLevelScope
                            QueryNested parent -> NestedScope parent
                    in foldl'
                        (\current identifier ->
                            Set.delete (internal, identifier) current)
                        seen
                        identifiers
        QueryConversationReset{} ->
            Set.empty

belongsToOwnTurn :: QueryAccumulator -> Message -> Bool
belongsToOwnTurn accumulator message =
    routeMessage accumulator message == RouteOwn

messageWouldBeObserved :: QueryAccumulator -> Message -> Bool
messageWouldBeObserved accumulator message =
    case message of
        MessageResult{} -> messageHasParentToolUseId message
        MessageConversationReset{} -> False
        MessageControlRequest{} -> False
        MessageUnknown{} -> False
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

retractionsFor :: QueryAccumulator -> Message -> [QueryProgress]
retractionsFor accumulator = \case
    MessageAssistant assistant
        | let identifiers =
                filter
                    (\identifier ->
                        Set.member
                            ( messageScope (MessageAssistant assistant)
                            , identifier
                            )
                            accumulator.progressSeenIds)
                    assistant.supersedes
        , not (null identifiers) ->
            [ QueryMessagesRetracted
                (Just (publicScope (MessageAssistant assistant)))
                identifiers
            ]
    MessageSystem system
        | let identifiers =
                filter
                    (\identifier ->
                        any
                            ((== identifier) . snd)
                            accumulator.progressSeenIds)
                    system.retractedMessageUuids
        , system.subtype == "model_refusal_fallback"
        , not (null identifiers) ->
            [QueryMessagesRetracted Nothing identifiers]
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
        MessageResult result
            | result.hasParentToolUseId ->
                bufferForRoute OwnRoute accumulator message
        MessageConversationReset _
            | messageHasParentToolUseId message ->
                bufferForRoute OwnRoute accumulator message
            | otherwise ->
                Right (resetQueryAccumulator accumulator, Nothing)
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
                ( registerToolOwners OwnRoute message $
                    accumulator
                        { ownBuffer = next
                        , currentForeignRoute = Nothing
                        }
                , Nothing
                )

consumeForeignMessage
    :: QueryAccumulator
    -> RouteKey
    -> Message
    -> Either
        ClaudeSDKError
        (QueryAccumulator, Maybe ([Message], ResultMessage))
consumeForeignMessage accumulator key message =
    case message of
        MessageResult result
            | not result.hasParentToolUseId ->
                Right
                    ( removeForeignRoute key accumulator
                    , Nothing
                    )
        _ ->
            bufferForRoute (ForeignRoute key) accumulator message

bufferForRoute
    :: RouteOwner
    -> QueryAccumulator
    -> Message
    -> Either
        ClaudeSDKError
        (QueryAccumulator, Maybe ([Message], ResultMessage))
bufferForRoute owner accumulator message = do
    let oldBuffer = case owner of
            OwnRoute -> accumulator.ownBuffer
            ForeignRoute key ->
                Map.findWithDefault emptyMessageBuffer
                    key
                    accumulator.foreignRoutes
    next <- consumeBufferedMessage oldBuffer message
    let routed = case owner of
            OwnRoute ->
                accumulator
                    { ownBuffer = next
                    , currentForeignRoute = Nothing
                    }
            ForeignRoute key ->
                accumulator
                    { foreignRoutes =
                        Map.insert key next accumulator.foreignRoutes
                    , currentForeignRoute = Just key
                    }
    Right (registerToolOwners owner message routed, Nothing)

routeMessage :: QueryAccumulator -> Message -> RouteDecision
routeMessage accumulator message
    | messageHasParentToolUseId message =
        case
            messageParentToolUseId message
                >>= (`Map.lookup` accumulator.toolOwners)
        of
            Just OwnRoute -> RouteOwn
            Just (ForeignRoute key) -> RouteForeign key
            Nothing -> RouteHidden
    | Just origin <- messageOrigin message =
        if origin.kind == "human"
            then RouteOwn
            else
                maybe RouteHidden RouteForeign
                    (resolveForeignRoute accumulator origin)
    | MessageSystem SystemMessage{subtype = "init"} <- message =
        RouteOwn
    | MessageConversationReset{} <- message =
        RouteOwn
    | otherwise =
        case accumulator.currentForeignRoute of
            Just key
                | Map.member key accumulator.foreignRoutes ->
                    RouteForeign key
            _
                | Map.null accumulator.foreignRoutes ->
                    RouteOwn
                | otherwise ->
                    -- Without an origin or a known parent, assigning this
                    -- record to either the human query or one of several
                    -- autonomous routes could leak unrelated output.
                    RouteHidden

messageOrigin :: Message -> Maybe MessageOrigin
messageOrigin = \case
    MessageUser user -> user.origin
    MessageResult result -> result.origin
    _ -> Nothing

resolveForeignRoute
    :: QueryAccumulator
    -> MessageOrigin
    -> Maybe RouteKey
resolveForeignRoute accumulator origin =
    let requested = RouteKey origin.kind origin.identifiers
        compatible =
            [ existing
            | existing <- Map.keys accumulator.foreignRoutes
            , routesCompatible requested existing
            ]
    in if Map.member requested accumulator.foreignRoutes
        then Just requested
        else case compatible of
            [existing] -> Just existing
            [] -> Just requested
            _ -> Nothing

routesCompatible :: RouteKey -> RouteKey -> Bool
routesCompatible left right =
    left.routeKind == right.routeKind
        && not (Map.null common)
        && all
            (\field -> Map.lookup field left.routeIdentifiers
                == Map.lookup field right.routeIdentifiers)
            (Map.keys common)
  where
    common =
        Map.intersection
            left.routeIdentifiers
            right.routeIdentifiers

registerToolOwners
    :: RouteOwner
    -> Message
    -> QueryAccumulator
    -> QueryAccumulator
registerToolOwners owner message accumulator =
    accumulator
        { toolOwners =
            foldl'
                (\owners identifier -> Map.insert identifier owner owners)
                accumulator.toolOwners
                (messageToolUseIds message)
        }

messageToolUseIds :: Message -> [Text]
messageToolUseIds = \case
    MessageAssistant AssistantMessage{content} ->
        [ identifier
        | block <- content
        , identifier <- case block of
            ToolUseBlock{toolUseId} -> [toolUseId]
            ServerToolUseBlock{toolUseId} -> [toolUseId]
            _ -> []
        ]
    _ -> []

removeForeignRoute :: RouteKey -> QueryAccumulator -> QueryAccumulator
removeForeignRoute key accumulator =
    accumulator
        { foreignRoutes = Map.delete key accumulator.foreignRoutes
        , currentForeignRoute =
            if accumulator.currentForeignRoute == Just key
                then Nothing
                else accumulator.currentForeignRoute
        , toolOwners =
            Map.filter (/= ForeignRoute key) accumulator.toolOwners
        }

resetQueryAccumulator :: QueryAccumulator -> QueryAccumulator
resetQueryAccumulator _ = emptyQueryAccumulator

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
