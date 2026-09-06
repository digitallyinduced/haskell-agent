-- | Message routing and transactional response accumulation.
module Claude.Agent.SDK.Internal.Query
    ( QueryAccumulator
    , emptyQueryAccumulator
    , consumeQueryMessage
    , consumeQueryMessageWithProgress
    , canonicalMessages
    ) where

import Agent.Json (rawJsonBytes)
import Claude.Agent.SDK.Errors (ClaudeSDKError(..))
import Claude.Agent.SDK.Types
    ( AssistantMessage(..)
    , ContentBlock(..)
    , Message(..)
    , MessageOrigin(..)
    , QueryMessageScope(..)
    , QueryProgress(..)
    , ResultMessage(..)
    , StreamEvent(..)
    , SystemMessage(..)
    , Usage(..)
    , UserMessage(..)
    , messageHasParentToolUseId
    , messageParentToolUseId
    , messageUuid
    )
import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

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
    , usageStreams :: !(Map MessageScope UsageStream)
    } deriving (Eq, Show)

-- A delta contains cumulative counters for one API request, not increments.
-- Keep the raw input components separate until the stream has finished.
data UsagePatch = UsagePatch
    { directInput :: !(Maybe Int)
    , cacheCreation :: !(Maybe Int)
    , cacheRead :: !(Maybe Int)
    , output :: !(Maybe Int)
    } deriving (Eq, Show)

data UsageStream = UsageStream
    { apiMessageId :: !Text
    , counters :: !UsagePatch
    , finalStopReason :: !(Maybe Text)
    , finalOutputSeen :: !Bool
    } deriving (Eq, Show)

data UsageEvent
    = UsageStart !Text !UsagePatch
    | UsageDelta !UsagePatch !(Maybe Text)
    | UsageStop
    | OtherUsageEvent

data QueryAccumulator = QueryAccumulator
    { ownBuffer :: !MessageBuffer
    , foreignRoutes :: !(Map RouteKey MessageBuffer)
    , currentForeignRoute :: !(Maybe RouteKey)
    , toolOwners :: !(Map Text RouteOwner)
    , progressSeenIds :: !(Set (MessageScope, Text))
    } deriving (Eq, Show)

data RouteKey = RouteKey
    { routeKind :: !Text
    , routeIdentity :: !RouteIdentity
    } deriving (Eq, Ord, Show)

data RouteIdentity
    = SenderTaskRoute !Text
    | FromSessionRoute !Text
    | FromRoute !Text
    | ServerRoute !Text
    | StartingUserRoute !Text
    | KindOnlyRoute
    deriving (Eq, Ord, Show)

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
    , usageStreams = Map.empty
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
                    (resolveForeignRoute accumulator message origin)
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
    -> Message
    -> MessageOrigin
    -> Maybe RouteKey
resolveForeignRoute accumulator message origin =
    let requested = RouteKey origin.kind (originRouteIdentity message origin)
        sameKind =
            [ existing
            | existing <- Map.keys accumulator.foreignRoutes
            , existing.routeKind == requested.routeKind
            ]
    in if Map.member requested accumulator.foreignRoutes
        then Just requested
        else
            if isForeignTurnStart message
                    || requested.routeIdentity /= KindOnlyRoute
                then Just requested
                else case sameKind of
                    [existing] -> Just existing
                    _ -> Nothing

originRouteIdentity :: Message -> MessageOrigin -> RouteIdentity
originRouteIdentity message origin =
    maybe
        (maybe KindOnlyRoute StartingUserRoute
            (if isForeignTurnStart message then messageUuid message else Nothing))
        id
        ( SenderTaskRoute <$> origin.senderTaskId
            <|> FromSessionRoute <$> origin.fromSession
            <|> FromRoute <$> origin.from
            <|> ServerRoute <$> origin.server
        )

isForeignTurnStart :: Message -> Bool
isForeignTurnStart = \case
    MessageUser{} -> True
    _ -> False

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
                    -- A streamed content block is emitted before its final
                    -- message_delta usage. Never expose that provisional
                    -- count as the canonical response's measured usage.
                    bufferRetractableMessage retracted $
                        MessageAssistant assistant
                            { usage =
                                if assistant.stopReason == Nothing
                                    || Map.member (messageScope message) retracted.usageStreams
                                    then Nothing
                                    else assistant.usage
                            }
        MessageSystem system
            | system.subtype == "model_refusal_fallback" ->
                Right $
                    bufferMessage
                        (retractMessagesGlobally
                            buffer
                            system.retractedMessageUuids)
                        message
        MessageSystem system
            | system.subtype == "compact_boundary" ->
                Right $ bufferMessage
                    buffer
                        { usageStreams =
                            Map.delete (messageScope message) buffer.usageStreams
                        }
                    message
        MessageUser _ ->
            bufferRetractableMessage buffer message
        MessageStreamEvent event ->
            -- Partial stream events are not canonical response records and
            -- cannot be safely associated with later UUID retractions. The
            -- low-level 'receiveMessage' API still exposes them to callers
            -- that explicitly implement live partial-message handling.
            -- Usage alone can be reconciled by API message id and scope;
            -- never retain partial text or resurrect retracted messages.
            Right (consumeUsageEvent (messageScope message) event buffer)
        _ ->
            Right (bufferMessage buffer message)

consumeUsageEvent :: MessageScope -> StreamEvent -> MessageBuffer -> MessageBuffer
consumeUsageEvent scope event buffer =
    case Aeson.decodeStrict' (rawJsonBytes event.event)
            >>= Aeson.parseMaybe parseUsageEvent of
        Nothing -> clearStream
        Just (UsageStart identifier counters) ->
            buffer { usageStreams = Map.insert scope
                (UsageStream identifier counters Nothing False)
                buffer.usageStreams }
        Just (UsageDelta patch stopReason) ->
            buffer { usageStreams = Map.adjust
                (\stream -> stream
                    { counters = mergeUsagePatch stream.counters patch
                    , finalStopReason = stopReason <|> stream.finalStopReason
                    , finalOutputSeen = stream.finalOutputSeen || isJust patch.output
                    })
                scope buffer.usageStreams }
        Just UsageStop ->
            case Map.lookup scope buffer.usageStreams of
                Just stream
                    | stream.finalOutputSeen
                    , Just stopReason <- stream.finalStopReason
                    , Just usage <- completedUsage stream.counters ->
                        clearStream
                            { messagesRev = map (complete stream.apiMessageId stopReason usage)
                                buffer.messagesRev
                            }
                _ -> clearStream
        Just OtherUsageEvent -> buffer
  where
    clearStream = buffer
        { usageStreams = Map.delete scope buffer.usageStreams }
    complete :: Text -> Text -> Usage -> BufferedMessage -> BufferedMessage
    complete identifier stopReason usage buffered =
        case buffered.message of
            MessageAssistant assistant
                | buffered.scope == scope
                , assistant.messageId == Just identifier ->
                    buffered { message = MessageAssistant assistant
                        { usage = Just usage, stopReason = Just stopReason } }
            _ -> buffered

parseUsageEvent :: Aeson.Value -> Aeson.Parser UsageEvent
parseUsageEvent = Aeson.withObject "stream usage event" \object -> do
    eventType <- object Aeson..: "type" :: Aeson.Parser Text
    case eventType of
        "message_start" -> do
            message <- object Aeson..: "message"
            Aeson.withObject "stream message" (\fields -> do
                identifier <- fields Aeson..: "id"
                if Text.null (Text.strip identifier)
                    then fail "empty API message id"
                    else UsageStart identifier
                        <$> (fields Aeson..: "usage" >>= parseUsagePatch)) message
        "message_delta" -> do
            patch <- object Aeson..: "usage" >>= parseUsagePatch
            delta <- object Aeson..: "delta"
            stopReason <- Aeson.withObject "message delta"
                (Aeson..:? "stop_reason") delta
            pure $ UsageDelta patch
                (stopReason >>= \reason ->
                    if Text.null (Text.strip reason) then Nothing else Just reason)
        "message_stop" -> pure UsageStop
        _ -> pure OtherUsageEvent

parseUsagePatch :: Aeson.Value -> Aeson.Parser UsagePatch
parseUsagePatch = Aeson.withObject "usage counters" \object ->
    UsagePatch
        <$> counter object "input_tokens"
        <*> counter object "cache_creation_input_tokens"
        <*> counter object "cache_read_input_tokens"
        <*> counter object "output_tokens"
  where
    counter object key = do
        value <- object Aeson..:? key
        case value of
            Just number | number < 0 -> fail "negative usage counter"
            _ -> pure value

mergeUsagePatch :: UsagePatch -> UsagePatch -> UsagePatch
mergeUsagePatch previous current = UsagePatch
    { directInput = positive current.directInput <|> previous.directInput
    , cacheCreation = positive current.cacheCreation <|> previous.cacheCreation
    , cacheRead = positive current.cacheRead <|> previous.cacheRead
    , output = current.output <|> previous.output
    }
  where
    -- Claude's message_delta may carry zero placeholders for unchanged
    -- input/cache components. Output is cumulative and may validly be zero.
    positive (Just number) | number > 0 = Just number
    positive _ = Nothing

completedUsage :: UsagePatch -> Maybe Usage
completedUsage counters
    | total > 0
    , total <= toInteger (maxBound :: Int)
    , Just outputTokens <- counters.output =
        Just Usage
            { inputTokens = fromInteger total
            , cachedTokens = fromMaybe 0 counters.cacheRead
            , outputTokens
            }
    | otherwise = Nothing
  where
    total = sum $ map (toInteger . fromMaybe 0)
        [counters.directInput, counters.cacheCreation, counters.cacheRead]

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
