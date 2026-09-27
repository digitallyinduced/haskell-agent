-- | Agent-server execution with stable submission identifiers and exact
-- session/turn/request correlation. The caller supplies an admitted client;
-- this module never discovers credentials or chooses a gateway.
module Agent.Telegram.Connector.Server
    ( ServerTransport (..)
    , serverTransport
    , serverSessionBackend
    , serverSessionBackendWith
    , ServerTurnState (..)
    , ServerTurnSnapshot (..)
    , reconcileServerTurn
    , reconcileServerTurnWith
    , resolveServerRequest
    , resolveServerRequestWith
    , cancelServerTurn
    , cancelServerTurnWith
    , completedServerResponse
    ) where

import Agent.Server.Client
import Agent.Telegram.Connector.Session
import Control.Monad (unless)
import Data.Aeson (withObject, (.:), (.:?))
import Data.Aeson.Types (parseMaybe)
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as Text

-- | A transport seam, not an application orchestration interface. Production
-- callers use 'serverTransport'; tests inject bounded deterministic responses.
data ServerTransport = ServerTransport
    { transportCreateTurn :: Text -> AgentServerCreateTurnRequest -> IO (Either AgentServerClientError AgentServerTurn)
    , transportListTurns :: Text -> IO (Either AgentServerClientError AgentServerTurnList)
    , transportGetTurn :: Text -> IO (Either AgentServerClientError AgentServerTurn)
    , transportGetResult :: Text -> IO (Either AgentServerClientError AgentServerTurnResult)
    , transportListRequests :: Text -> IO (Either AgentServerClientError AgentServerRequestList)
    , transportResolveRequest :: Text -> AgentServerResolveRequest -> IO (Either AgentServerClientError AgentServerHumanRequest)
    , transportCancelTurn :: Text -> IO (Either AgentServerClientError AgentServerTurn)
    }

serverTransport :: AgentServerClient -> ServerTransport
serverTransport client = ServerTransport
    { transportCreateTurn = createAgentServerTurn client
    , transportListTurns = listAgentServerTurns client
    , transportGetTurn = getAgentServerTurn client
    , transportGetResult = getAgentServerTurnResult client
    , transportListRequests = listAgentServerRequestsForTurn client
    , transportResolveRequest = resolveAgentServerRequest client
    , transportCancelTurn = cancelAgentServerTurn client
    }

-- | Construct the ready-to-use remote session backend. The text limit is an
-- application publication policy, not a truncation target: longer responses
-- fail validation instead of silently changing the agent's answer.
serverSessionBackend :: AgentServerClient -> Int -> SessionBackend
serverSessionBackend = serverSessionBackendWith . serverTransport

serverSessionBackendWith :: ServerTransport -> Int -> SessionBackend
serverSessionBackendWith transport maximumLength = SessionBackend
    { submitExecution = \execution -> reconcile execution execution.executionTurnId
    , inspectExecution = \execution identifier -> reconcile execution (Just identifier)
    , cancelExecution = \execution identifier ->
        fmap (first sessionFailure) $
            cancelServerTurnWith transport execution.executionSessionId execution.executionRequestId identifier
    , respondToRequest = \execution identifier request value ->
        case parseMaybe (withObject "Session response" \fields ->
                AgentServerResolveRequest <$> fields .: "decision" <*> fields .:? "value") value of
            Nothing -> pure (Left (SessionRejected "Invalid session response"))
            Just response -> fmap (first sessionFailure) $
                resolveServerRequestExpected transport execution.executionSessionId execution.executionRequestId
                    identifier request.humanRequestId (Just request) response
    }
  where
    reconcile execution identifier =
        fmap (first sessionFailure . fmap (toSnapshot execution)) $
            reconcileServerTurnWith transport maximumLength execution.executionSessionId
                (AgentServerCreateTurnRequest execution.executionRequestId execution.executionPrompt [] [])
                identifier
    toSnapshot execution snapshot = SessionSnapshot
        { snapshotSessionId = execution.executionSessionId
        , snapshotRequestId = execution.executionRequestId
        , snapshotTurnId = snapshot.snapshotTurnIdentifier
        , snapshotStatus = case snapshot.snapshotTurnState of
            ServerTurnActive -> SessionRunning
            ServerTurnWaiting requests -> SessionWaiting (map toHumanRequest requests)
            ServerTurnCompleted content -> SessionCompleted content
            ServerTurnFailed -> SessionFailed "Agent execution failed"
            ServerTurnCancelled -> SessionCancelled
        }

toHumanRequest :: AgentServerHumanRequest -> HumanRequest
toHumanRequest request = HumanRequest
    { humanRequestId = request.agentServerRequestId
    , humanRequestKind = case request.agentServerRequestKind of
        AgentServerToolApproval -> "AgentServerToolApproval"
        AgentServerRootAccess -> "AgentServerRootAccess"
        AgentServerPlanEnter -> "AgentServerPlanEnter"
        AgentServerPlanExit -> "AgentServerPlanExit"
        AgentServerPlanQuestion -> "AgentServerPlanQuestion"
    , humanRequestPrompt = request.agentServerRequestPrompt
    , humanRequestOptions = request.agentServerRequestOptions
    }

-- Do not propagate remote diagnostics, prompts or bearer-bearing exceptions
-- through the connector's user-visible failure channel.
sessionFailure :: AgentServerClientError -> SessionFailure
sessionFailure = \case
    AgentServerTransportError _ -> SessionUnavailable
    AgentServerCredentialError _ -> SessionUnavailable
    AgentServerHttpError status _ _
        | status == 408 || status == 429 || status >= 500 -> SessionUnavailable
        | otherwise -> SessionRejected "Agent server rejected the operation"
    AgentServerDecodeError _ -> SessionRejected "Agent server returned an invalid response"
    AgentServerProtocolError _ -> SessionRejected "Agent server response failed protocol validation"

data ServerTurnState
    = ServerTurnActive
    | ServerTurnWaiting ![AgentServerHumanRequest]
    | ServerTurnCompleted !Text
    | ServerTurnFailed
    | ServerTurnCancelled
    deriving (Eq, Show)

data ServerTurnSnapshot = ServerTurnSnapshot
    { snapshotTurnIdentifier :: !Text
    , snapshotTurnState :: !ServerTurnState
    } deriving (Eq, Show)

-- | Reconcile one durable execution. Persist the returned identifier before
-- the next iteration. A missing local turn identifier is not evidence that
-- submission failed: first recover by the stable client request identifier.
-- A bounded listing can omit older work; resubmission still uses exactly the
-- same identifier and relies on the server's idempotency constraint.
--
-- Session creation must be committed separately before calling this function.
-- Recreating an unrecorded session after an uncertain turn submission would
-- bypass the server's session-scoped idempotency boundary.
reconcileServerTurn
    :: AgentServerClient
    -> Int
    -> Text
    -> AgentServerCreateTurnRequest
    -> Maybe Text
    -> IO (Either AgentServerClientError ServerTurnSnapshot)
reconcileServerTurn = reconcileServerTurnWith . serverTransport

reconcileServerTurnWith
    :: ServerTransport
    -> Int
    -> Text
    -> AgentServerCreateTurnRequest
    -> Maybe Text
    -> IO (Either AgentServerClientError ServerTurnSnapshot)
reconcileServerTurnWith transport maximumLength session request knownTurn =
    case knownTurn of
        Just identifier -> fetchResult identifier
        Nothing -> transport.transportListTurns session >>= \case
            Left problem -> pure (Left problem)
            Right listing ->
                case filter (\turn -> turn.agentServerTurnClientRequestId == request.createTurnClientRequestId) listing.agentServerTurns of
                    [] -> transport.transportCreateTurn session request >>= \case
                        Left problem -> pure (Left problem)
                        Right turn -> adopt turn
                    [turn] -> adopt turn
                    _ -> pure (protocolFailure "duplicate agent-server request identifiers")
  where
    validate = validateTurn session request.createTurnClientRequestId
    adopt turn = case validate Nothing turn of
        Left problem -> pure (Left problem)
        Right () -> pure (Right (ServerTurnSnapshot turn.agentServerTurnId ServerTurnActive))
    fetchResult identifier = transport.transportGetResult identifier >>= \case
        Left problem -> pure (Left problem)
        Right result -> case validate (Just identifier) result.agentServerResultTurn of
            Left problem -> pure (Left problem)
            Right () -> classify identifier result
    classify identifier result = case result.agentServerResultTurn.agentServerTurnStatus of
        AgentServerTurnQueued -> snapshot identifier ServerTurnActive
        AgentServerTurnRunning -> snapshot identifier ServerTurnActive
        AgentServerTurnFailed -> snapshot identifier ServerTurnFailed
        AgentServerTurnCancelled -> snapshot identifier ServerTurnCancelled
        AgentServerTurnCompleted ->
            pure $ ServerTurnSnapshot identifier . ServerTurnCompleted
                <$> completedServerResponse maximumLength result
        AgentServerTurnWaitingForInput ->
            transport.transportListRequests identifier >>= \case
                Left problem -> pure (Left problem)
                Right listing -> pure do
                    mapM_ (validateHumanRequest session identifier) listing.agentServerRequests
                    pure (ServerTurnSnapshot identifier (ServerTurnWaiting listing.agentServerRequests))
    snapshot identifier state = pure (Right (ServerTurnSnapshot identifier state))

completedServerResponse :: Int -> AgentServerTurnResult -> Either AgentServerClientError Text
completedServerResponse maximumLength result
    | result.agentServerResultTurn.agentServerTurnStatus /= AgentServerTurnCompleted =
        protocolFailure "agent-server turn is not completed"
    | otherwise = case result.agentServerResultOutput of
        Just output
            | output.agentServerOutputCompletion == AgentServerTurnComplete
            , not output.agentServerOutputAssistantTextTruncated
            , Just content <- output.agentServerOutputAssistantText
            , not (Text.null (Text.strip content))
            , Text.length content <= maximumLength
            , not (Text.any (== '\0') content) -> Right content
        _ -> protocolFailure "agent-server response is absent, incomplete, or exceeds the output limit"

-- | Re-fetch the outstanding request before submitting a decision. The
-- connector must already have consumed and authorized its one-use callback.
-- Never resolve an identifier supplied directly by Telegram callback data.
resolveServerRequest
    :: AgentServerClient -> Text -> Text -> Text -> Text
    -> AgentServerResolveRequest -> IO (Either AgentServerClientError ())
resolveServerRequest = resolveServerRequestWith . serverTransport

resolveServerRequestWith
    :: ServerTransport -> Text -> Text -> Text -> Text
    -> AgentServerResolveRequest -> IO (Either AgentServerClientError ())
resolveServerRequestWith transport session clientRequest turnIdentifier requestIdentifier response =
    resolveServerRequestExpected transport session clientRequest turnIdentifier requestIdentifier Nothing response

resolveServerRequestExpected
    :: ServerTransport -> Text -> Text -> Text -> Text -> Maybe HumanRequest
    -> AgentServerResolveRequest -> IO (Either AgentServerClientError ())
resolveServerRequestExpected transport session clientRequest turnIdentifier requestIdentifier expected response =
    transport.transportGetTurn turnIdentifier >>= \case
        Left problem -> pure (Left problem)
        Right turn -> case validateTurn session clientRequest (Just turnIdentifier) turn of
            Left problem -> pure (Left problem)
            Right ()
                | turn.agentServerTurnStatus /= AgentServerTurnWaitingForInput ->
                    pure (protocolFailure "agent-server request is no longer awaiting input")
                | otherwise -> transport.transportListRequests turnIdentifier >>= \case
                    Left problem -> pure (Left problem)
                    Right listing -> case filter (\request -> request.agentServerRequestId == requestIdentifier) listing.agentServerRequests of
                        [request] -> case validateDecision request of
                            Left problem -> pure (Left problem)
                            Right () -> transport.transportResolveRequest requestIdentifier response >>= \case
                                Left problem -> pure (Left problem)
                                Right resolved -> pure do
                                    validateHumanRequest session turnIdentifier resolved
                                    unless (resolved == request) (protocolFailure "agent-server resolved a different request")
                        _ -> pure (protocolFailure "agent-server request is absent or ambiguous")
  where
    validateDecision request = do
        validateHumanRequest session turnIdentifier request
        unless (maybe True (== toHumanRequest request) expected)
            (protocolFailure "agent-server request changed after presentation")
        unless (response.resolveRequestDecision `elem` request.agentServerRequestOptions)
            (protocolFailure "agent-server decision is not an offered option")
        unless (maybe True (\value -> Text.length value <= 16000 && not (Text.any (== '\0') value)) response.resolveRequestValue)
            (protocolFailure "agent-server response value is invalid")

cancelServerTurn
    :: AgentServerClient -> Text -> Text -> Text
    -> IO (Either AgentServerClientError ())
cancelServerTurn = cancelServerTurnWith . serverTransport

cancelServerTurnWith
    :: ServerTransport -> Text -> Text -> Text
    -> IO (Either AgentServerClientError ())
cancelServerTurnWith transport session clientRequest identifier =
    transport.transportGetTurn identifier >>= \case
        Left problem -> pure (Left problem)
        Right turn -> case validateTurn session clientRequest (Just identifier) turn of
            Left problem -> pure (Left problem)
            Right () -> case turn.agentServerTurnStatus of
                AgentServerTurnCompleted -> pure (Right ())
                AgentServerTurnFailed -> pure (Right ())
                AgentServerTurnCancelled -> pure (Right ())
                _ -> transport.transportCancelTurn identifier >>= \case
                    Left problem -> pure (Left problem)
                    Right cancelled -> pure (validateTurn session clientRequest (Just identifier) cancelled)

validateTurn :: Text -> Text -> Maybe Text -> AgentServerTurn -> Either AgentServerClientError ()
validateTurn session clientRequest identifier turn =
    unless
        ( turn.agentServerTurnSessionId == session
            && turn.agentServerTurnClientRequestId == clientRequest
            && maybe True (== turn.agentServerTurnId) identifier
            && not (Text.null turn.agentServerTurnId)
        )
        (protocolFailure "agent-server returned a mismatched turn")

validateHumanRequest :: Text -> Text -> AgentServerHumanRequest -> Either AgentServerClientError ()
validateHumanRequest session turn request = do
    unless
        ( request.agentServerRequestSessionId == session
            && request.agentServerRequestTurnId == turn
            && not (Text.null request.agentServerRequestId)
        )
        (protocolFailure "agent-server returned a mismatched human request")
    unless
        (all safeText (request.agentServerRequestId : request.agentServerRequestPrompt : request.agentServerRequestOptions))
        (protocolFailure "agent-server returned unsafe human request text")
  where
    safeText value = Text.length value <= 16000 && not (Text.any (== '\0') value)

protocolFailure :: Text -> Either AgentServerClientError value
protocolFailure = Left . AgentServerProtocolError
