-- | Named wire types and strict SSE decoding for the public agent-server API.
module Agent.Server.Client.Protocol
    ( AgentServerCreateSessionRequest (..)
    , AgentServerSession (..)
    , AgentServerCreateTurnRequest (..)
    , AgentServerTurnStatus (..)
    , AgentServerTurn (..)
    , AgentServerTurnList (..)
    , AgentServerTurnCompletion (..)
    , AgentServerTurnOutput (..)
    , AgentServerTurnResult (..)
    , AgentServerHumanRequestKind (..)
    , AgentServerHumanRequest (..)
    , AgentServerRequestList (..)
    , AgentServerResolveRequest (..)
    , AgentServerEventPayload (..)
    , AgentServerEvent (..)
    , AgentServerReplayReset (..)
    , AgentServerSseMessage (..)
    , AgentServerHistoryTurn (..)
    , AgentServerHistoryItem (..)
    , AgentServerHistory (..)
    , AgentServerErrorBody (..)
    , AgentServerErrorEnvelope (..)
    , parseAgentServerSseFrame
    )
where

import Control.Monad (foldM, when)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Bifunctor qualified as Bifunctor
import Data.ByteString qualified as ByteString
import Data.Foldable (forM_)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Time.Clock (UTCTime)
import Data.UUID qualified as UUID
import Text.Read (readMaybe)

data AgentServerCreateSessionRequest = AgentServerCreateSessionRequest
    { createSessionModel :: !(Maybe Text)
    , createSessionCwd :: !(Maybe FilePath)
    , createSessionEffort :: !(Maybe Text)
    , createSessionTitle :: !(Maybe Text)
    }
    deriving (Eq, Show)

instance ToJSON AgentServerCreateSessionRequest where
    toJSON request =
        object
            [ "model" .= request.createSessionModel
            , "cwd" .= request.createSessionCwd
            , "effort" .= request.createSessionEffort
            , "title" .= request.createSessionTitle
            ]

newtype AgentServerSession = AgentServerSession
    { agentServerSessionId :: Text
    }
    deriving (Eq, Show)

instance FromJSON AgentServerSession where
    parseJSON = withObject "AgentServerSession" \value ->
        AgentServerSession
            <$> (value .: "id" >>= parseSessionIdentifier)

data AgentServerCreateTurnRequest = AgentServerCreateTurnRequest
    { createTurnClientRequestId :: !Text
    , createTurnInput :: !Text
    }
    deriving (Eq, Show)

instance ToJSON AgentServerCreateTurnRequest where
    toJSON request =
        object
            [ "clientRequestId" .= request.createTurnClientRequestId
            , "input" .= request.createTurnInput
            ]

data AgentServerTurnStatus
    = AgentServerTurnQueued
    | AgentServerTurnRunning
    | AgentServerTurnWaitingForInput
    | AgentServerTurnCompleted
    | AgentServerTurnFailed
    | AgentServerTurnCancelled
    deriving (Eq, Show)

data AgentServerTurn = AgentServerTurn
    { agentServerTurnId :: !Text
    , agentServerTurnSessionId :: !Text
    , agentServerTurnClientRequestId :: !Text
    , agentServerTurnStatus :: !AgentServerTurnStatus
    , agentServerTurnCreatedAt :: !UTCTime
    , agentServerTurnStartedAt :: !(Maybe UTCTime)
    , agentServerTurnFinishedAt :: !(Maybe UTCTime)
    , agentServerTurnError :: !(Maybe Text)
    }
    deriving (Eq, Show)

instance FromJSON AgentServerTurn where
    parseJSON = withObject "AgentServerTurn" parseTurn

newtype AgentServerTurnList = AgentServerTurnList
    { agentServerTurns :: [AgentServerTurn]
    }
    deriving (Eq, Show)

instance FromJSON AgentServerTurnList where
    parseJSON = withObject "AgentServerTurnList" \value ->
        AgentServerTurnList <$> value .: "data"

data AgentServerTurnCompletion
    = AgentServerTurnComplete
    | AgentServerTurnIncomplete !Text !(Maybe Int)
    deriving (Eq, Show)

data AgentServerTurnOutput = AgentServerTurnOutput
    { agentServerOutputResponseId :: !Text
    , agentServerOutputAssistantText :: !(Maybe Text)
    , agentServerOutputAssistantTextTruncated :: !Bool
    , agentServerOutputCompletion :: !AgentServerTurnCompletion
    }
    deriving (Eq, Show)

instance FromJSON AgentServerTurnOutput where
    parseJSON = withObject "AgentServerTurnOutput" \value ->
        AgentServerTurnOutput
            <$> value .: "responseId"
            <*> value .: "assistantText"
            <*> value .: "assistantTextTruncated"
            <*> (value .: "completion" >>= parseCompletion)

data AgentServerTurnResult = AgentServerTurnResult
    { agentServerResultTurn :: !AgentServerTurn
    , agentServerResultOutput :: !(Maybe AgentServerTurnOutput)
    }
    deriving (Eq, Show)

instance FromJSON AgentServerTurnResult where
    parseJSON = withObject "AgentServerTurnResult" \value ->
        AgentServerTurnResult
            <$> value .: "turn"
            <*> value .: "output"

data AgentServerHumanRequestKind
    = AgentServerToolApproval
    | AgentServerRootAccess
    | AgentServerPlanEnter
    | AgentServerPlanExit
    | AgentServerPlanQuestion
    deriving (Eq, Show)

data AgentServerHumanRequest = AgentServerHumanRequest
    { agentServerRequestId :: !Text
    , agentServerRequestTurnId :: !Text
    , agentServerRequestSessionId :: !Text
    , agentServerRequestKind :: !AgentServerHumanRequestKind
    , agentServerRequestPrompt :: !Text
    , agentServerRequestOptions :: ![Text]
    , agentServerRequestCreatedAt :: !UTCTime
    }
    deriving (Eq, Show)

instance FromJSON AgentServerHumanRequest where
    parseJSON = withObject "AgentServerHumanRequest" parseHumanRequest

newtype AgentServerRequestList = AgentServerRequestList
    { agentServerRequests :: [AgentServerHumanRequest]
    }
    deriving (Eq, Show)

instance FromJSON AgentServerRequestList where
    parseJSON = withObject "AgentServerRequestList" \value ->
        AgentServerRequestList <$> value .: "data"

data AgentServerResolveRequest = AgentServerResolveRequest
    { resolveRequestDecision :: !Text
    , resolveRequestValue :: !(Maybe Text)
    }
    deriving (Eq, Show)

instance ToJSON AgentServerResolveRequest where
    toJSON request =
        object
            [ "decision" .= request.resolveRequestDecision
            , "value" .= request.resolveRequestValue
            ]

data AgentServerEventPayload
    = AgentServerResponseTextDelta !Text
    | AgentServerAgentTurnFinished
        !(Maybe Text)
        !Bool
        !AgentServerTurnCompletion
    | AgentServerToolStarted !Text !Text
    | AgentServerToolFinished !Text
    | AgentServerRequestCreated !AgentServerHumanRequest
    | AgentServerRequestResolved !Text
    | AgentServerTurnQueuedEvent
    | AgentServerTurnStartedEvent
    | AgentServerTurnCompletedEvent
    | AgentServerTurnFailedEvent !(Maybe Text)
    | AgentServerTurnCancelledEvent
    | AgentServerOtherEvent !Text
    deriving (Eq, Show)

data AgentServerEvent = AgentServerEvent
    { agentServerEventId :: !Int64
    , agentServerEventType :: !Text
    , agentServerEventTurnId :: !(Maybe Text)
    , agentServerEventSessionId :: !(Maybe Text)
    , agentServerEventPayload :: !AgentServerEventPayload
    , agentServerEventAt :: !UTCTime
    }
    deriving (Eq, Show)

instance FromJSON AgentServerEvent where
    parseJSON = withObject "AgentServerEvent" \value -> do
        eventId <- value .: "id"
        when (eventId < 1) (fail "event id must be positive")
        eventType <- value .: "type"
        eventTurnId <-
            traverse
                (parseIdentifier "event turn id")
                =<< value .:? "turnId"
        eventSessionId <-
            traverse
                parseSessionIdentifier
                =<< value .:? "sessionId"
        eventData <- value .: "data"
        eventPayload <- parseEventPayload eventType eventData
        eventAt <- value .: "at"
        pure
            AgentServerEvent
                { agentServerEventId = eventId
                , agentServerEventType = eventType
                , agentServerEventTurnId = eventTurnId
                , agentServerEventSessionId = eventSessionId
                , agentServerEventPayload = eventPayload
                , agentServerEventAt = eventAt
                }

newtype AgentServerReplayReset = AgentServerReplayReset
    { replayResetReason :: Text
    }
    deriving (Eq, Show)

instance FromJSON AgentServerReplayReset where
    parseJSON = withObject "AgentServerReplayReset" \value ->
        AgentServerReplayReset <$> value .: "reason"

data AgentServerSseMessage
    = AgentServerSseEvent !AgentServerEvent
    | AgentServerSseReplayReset !AgentServerReplayReset
    deriving (Eq, Show)

data AgentServerHistoryTurn = AgentServerHistoryTurn
    { historyTurnUserText :: !Text
    , historyTurnAssistantText :: !(Maybe Text)
    , historyTurnError :: !(Maybe Text)
    , historyTurnProjectionTruncated :: !Bool
    }
    deriving (Eq, Show)

instance FromJSON AgentServerHistoryTurn where
    parseJSON = withObject "AgentServerHistoryTurn" \value ->
        AgentServerHistoryTurn
            <$> value .: "userText"
            <*> value .:? "assistantText"
            <*> value .:? "error"
            <*> value .:? "projectionTruncated" .!= False

data AgentServerHistoryItem = AgentServerHistoryItem
    { historyItemIndex :: !Int64
    , historyItemTurn :: !AgentServerHistoryTurn
    }
    deriving (Eq, Show)

instance FromJSON AgentServerHistoryItem where
    parseJSON = withObject "AgentServerHistoryItem" \value ->
        AgentServerHistoryItem
            <$> value .: "index"
            <*> value .: "turn"

newtype AgentServerHistory = AgentServerHistory
    { agentServerHistoryItems :: [AgentServerHistoryItem]
    }
    deriving (Eq, Show)

instance FromJSON AgentServerHistory where
    parseJSON = withObject "AgentServerHistory" \value ->
        AgentServerHistory <$> value .: "data"

data AgentServerErrorBody = AgentServerErrorBody
    { agentServerErrorCode :: !Text
    , agentServerErrorMessage :: !Text
    , agentServerErrorRequestId :: !(Maybe Text)
    }
    deriving (Eq, Show)

instance FromJSON AgentServerErrorBody where
    parseJSON = withObject "AgentServerErrorBody" \value ->
        AgentServerErrorBody
            <$> value .: "code"
            <*> value .: "message"
            <*> value .: "requestId"

newtype AgentServerErrorEnvelope = AgentServerErrorEnvelope
    { agentServerError :: AgentServerErrorBody
    }
    deriving (Eq, Show)

instance FromJSON AgentServerErrorEnvelope where
    parseJSON = withObject "AgentServerErrorEnvelope" \value ->
        AgentServerErrorEnvelope <$> value .: "error"

{- | Decode one complete SSE block (without the terminating blank line).
Heartbeat/comment-only blocks intentionally produce 'Nothing'.
-}
parseAgentServerSseFrame ::
    ByteString.ByteString ->
    Either Text (Maybe AgentServerSseMessage)
parseAgentServerSseFrame rawFrame = do
    fields <- foldM parseLine (Nothing, Nothing, []) normalizedLines
    case fields of
        (_, _, []) -> Right Nothing
        (eventName, sseId, reversedData) -> do
            let eventType = fromMaybe "message" eventName
                payload = ByteString.intercalate "\n" (reverse reversedData)
            if eventType == "replay.reset"
                then do
                    reset <-
                        Bifunctor.first
                            Text.pack
                            (eitherDecodeStrict' payload)
                    pure (Just (AgentServerSseReplayReset reset))
                else do
                    event <-
                        Bifunctor.first
                            Text.pack
                            (eitherDecodeStrict' payload)
                    when (event.agentServerEventType /= eventType) $
                        Left "SSE event type does not match its JSON envelope"
                    forM_ sseId \identifier ->
                        when (identifier /= event.agentServerEventId) $
                            Left "SSE event id does not match its JSON envelope"
                    pure (Just (AgentServerSseEvent event))
  where
    normalizedLines =
        map stripCarriageReturn (ByteString.split 10 rawFrame)

    parseLine fields line
        | ByteString.null line = Right fields
        | ByteString.head line == 58 = Right fields
        | otherwise =
            case ByteString.break (== 58) line of
                (name, rest) ->
                    let value = stripOneSpace (ByteString.drop 1 rest)
                     in case name of
                            "event" -> do
                                decoded <- decodeUtf8Field "SSE event name" value
                                pure
                                    ( Just decoded
                                    , secondOf fields
                                    , thirdOf fields
                                    )
                            "id" -> do
                                identifier <- parseEventId value
                                pure
                                    ( firstOf fields
                                    , Just identifier
                                    , thirdOf fields
                                    )
                            "data" ->
                                pure
                                    ( firstOf fields
                                    , secondOf fields
                                    , value : thirdOf fields
                                    )
                            _ -> Right fields

parseTurn :: Object -> Parser AgentServerTurn
parseTurn value =
    AgentServerTurn
        <$> (value .: "id" >>= parseIdentifier "turn id")
        <*> (value .: "sessionId" >>= parseSessionIdentifier)
        <*> ( value .: "clientRequestId"
                >>= parseIdentifier "client request id"
            )
        <*> (value .: "status" >>= parseTurnStatus)
        <*> value .: "createdAt"
        <*> value .: "startedAt"
        <*> value .: "finishedAt"
        <*> value .: "error"

parseEventPayload :: Text -> Value -> Parser AgentServerEventPayload
parseEventPayload eventType eventData =
    case eventType of
        "response.text.delta" ->
            withObject
                "ResponseTextDelta"
                (\value -> AgentServerResponseTextDelta <$> value .: "text")
                eventData
        "agent.turn.finished" ->
            withObject
                "AgentTurnFinished"
                ( \value ->
                    AgentServerAgentTurnFinished
                        <$> value .: "assistantText"
                        <*> value .: "assistantTextTruncated"
                        <*> (value .: "completion" >>= parseCompletion)
                )
                eventData
        "tool.started" ->
            withObject
                "ToolStarted"
                ( \value ->
                    AgentServerToolStarted
                        <$> value .: "callId"
                        <*> value .: "name"
                )
                eventData
        "tool.finished" ->
            withObject
                "ToolFinished"
                (\value -> AgentServerToolFinished <$> value .: "callId")
                eventData
        "request.created" ->
            withObject
                "RequestCreated"
                ( \value ->
                    AgentServerRequestCreated
                        <$> (value .: "request" >>= parseJSON)
                )
                eventData
        "request.resolved" ->
            withObject
                "RequestResolved"
                ( \value ->
                    AgentServerRequestResolved
                        <$> ( value .: "requestId"
                                >>= parseIdentifier "resolved request id"
                            )
                )
                eventData
        "turn.queued" -> pure AgentServerTurnQueuedEvent
        "turn.started" -> pure AgentServerTurnStartedEvent
        "turn.completed" -> pure AgentServerTurnCompletedEvent
        "turn.failed" ->
            withObject
                "TurnFailed"
                (\value -> AgentServerTurnFailedEvent <$> value .:? "error")
                eventData
        "turn.cancelled" -> pure AgentServerTurnCancelledEvent
        _ -> pure (AgentServerOtherEvent eventType)

parseHumanRequest :: Object -> Parser AgentServerHumanRequest
parseHumanRequest value =
    AgentServerHumanRequest
        <$> (value .: "id" >>= parseIdentifier "request id")
        <*> (value .: "turnId" >>= parseIdentifier "request turn id")
        <*> (value .: "sessionId" >>= parseSessionIdentifier)
        <*> (value .: "kind" >>= parseHumanRequestKind)
        <*> value .: "prompt"
        <*> value .: "options"
        <*> value .: "createdAt"

parseTurnStatus :: Text -> Parser AgentServerTurnStatus
parseTurnStatus = \case
    "queued" -> pure AgentServerTurnQueued
    "running" -> pure AgentServerTurnRunning
    "waiting_for_input" -> pure AgentServerTurnWaitingForInput
    "completed" -> pure AgentServerTurnCompleted
    "failed" -> pure AgentServerTurnFailed
    "cancelled" -> pure AgentServerTurnCancelled
    other ->
        fail ("unknown agent-server turn status: " <> Text.unpack other)

parseHumanRequestKind :: Text -> Parser AgentServerHumanRequestKind
parseHumanRequestKind = \case
    "tool_approval" -> pure AgentServerToolApproval
    "root_access" -> pure AgentServerRootAccess
    "plan_enter" -> pure AgentServerPlanEnter
    "plan_exit" -> pure AgentServerPlanExit
    "plan_question" -> pure AgentServerPlanQuestion
    other ->
        fail ("unknown agent-server request kind: " <> Text.unpack other)

parseCompletion :: Value -> Parser AgentServerTurnCompletion
parseCompletion = withObject "AgentServerTurnCompletion" \value ->
    value .: "status" >>= \case
        ("completed" :: Text) -> pure AgentServerTurnComplete
        "incomplete" ->
            AgentServerTurnIncomplete
                <$> value .: "reason"
                <*> value .:? "reasoningTokens"
        other ->
            fail
                ( "unknown agent-server completion status: "
                    <> Text.unpack other
                )

parseIdentifier :: Text -> Text -> Parser Text
parseIdentifier label raw =
    case UUID.fromText raw of
        Nothing -> fail (Text.unpack label <> " must be a UUID")
        Just value -> pure (UUID.toText value)

-- Session ids predate the UUID wire identifiers. They are opaque path
-- components, and the server intentionally keeps accepting older allocator
-- formats so persisted sessions remain resumable.
parseSessionIdentifier :: Text -> Parser Text
parseSessionIdentifier raw
    | Text.null raw = fail "session id must not be empty"
    | raw == "." || raw == ".." =
        fail "session id must be a single path component"
    | Text.any
        ( \character ->
            character == '/'
                || character == '\\'
                || character == '\NUL'
        )
        raw =
        fail "session id must be a single path component"
    | otherwise = pure raw

parseEventId :: ByteString.ByteString -> Either Text Int64
parseEventId bytes =
    case readMaybe (Text.unpack decoded) of
        Just value
            | value >= 1 -> Right value
        _ -> Left "SSE event id must be a positive integer"
  where
    decoded =
        TextEncoding.decodeUtf8With (\_ _ -> Just '\xfffd') bytes

decodeUtf8Field ::
    Text ->
    ByteString.ByteString ->
    Either Text Text
decodeUtf8Field label =
    Bifunctor.first
        (const (label <> " is not valid UTF-8"))
        . TextEncoding.decodeUtf8'

stripCarriageReturn :: ByteString.ByteString -> ByteString.ByteString
stripCarriageReturn value =
    case ByteString.unsnoc value of
        Just (prefix, 13) -> prefix
        _ -> value

stripOneSpace :: ByteString.ByteString -> ByteString.ByteString
stripOneSpace value =
    fromMaybe value (ByteString.stripPrefix " " value)

firstOf :: (a, b, c) -> a
firstOf (value, _, _) = value

secondOf :: (a, b, c) -> b
secondOf (_, value, _) = value

thirdOf :: (a, b, c) -> c
thirdOf (_, _, value) = value
