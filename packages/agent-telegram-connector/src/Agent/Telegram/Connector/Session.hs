-- | Session execution protocol shared by local and remote Telegram connectors.
--
-- The application resolves and authorizes the session before constructing an
-- execution. The request identifier is durable and MUST NOT change on retry.
module Agent.Telegram.Connector.Session where

import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as Text

data SessionExecution = SessionExecution
    { executionRequestId :: !Text
    , executionSessionId :: !Text
    , executionPrompt :: !Text
    , executionTurnId :: !(Maybe Text)
    } deriving (Eq, Show)

data HumanRequest = HumanRequest
    { humanRequestId :: !Text
    , humanRequestKind :: !Text
    , humanRequestPrompt :: !Text
    , humanRequestOptions :: ![Text]
    } deriving (Eq, Show)

data SessionStatus
    = SessionRunning
    | SessionWaiting ![HumanRequest]
    | SessionCompleted !Text
    | SessionFailed !Text
    | SessionCancelled
    deriving (Eq, Show)

data SessionSnapshot = SessionSnapshot
    { snapshotSessionId :: !Text
    , snapshotRequestId :: !Text
    , snapshotTurnId :: !Text
    , snapshotStatus :: !SessionStatus
    } deriving (Eq, Show)

data SessionFailure
    = SessionUnavailable
    | SessionRejected !Text
    deriving (Eq, Show)

-- | Backend operations are session operations, never Telegram operations.
-- Submission must reconcile the stable request ID before creating new work.
-- Only complete, non-truncated output may become 'SessionCompleted'.
data SessionBackend = SessionBackend
    { submitExecution :: SessionExecution -> IO (Either SessionFailure SessionSnapshot)
    , inspectExecution :: SessionExecution -> Text -> IO (Either SessionFailure SessionSnapshot)
    , cancelExecution :: SessionExecution -> Text -> IO (Either SessionFailure ())
    , respondToRequest :: SessionExecution -> Text -> HumanRequest -> Value -> IO (Either SessionFailure ())
    }

data ExecutionTransition
    = ExecutionRunning !Text
    | ExecutionWaiting !Text ![HumanRequest]
    | ExecutionCompleted !Text !Text
    | ExecutionFailed !Text
    | ExecutionCancelled
    | ExecutionRetry
    deriving (Eq, Show)

-- | Exactly one submission/reconciliation step. Persist the resulting transition
-- before another step. Completed response publication and completion marking
-- must be one transaction in the application store.
advanceSessionExecution :: SessionBackend -> SessionExecution -> IO ExecutionTransition
advanceSessionExecution backend execution =
    if any Text.null [execution.executionRequestId, execution.executionSessionId]
        then pure (ExecutionFailed "Missing session execution identity")
        else do
            outcome <- case execution.executionTurnId of
                Nothing -> backend.submitExecution execution
                Just turn -> backend.inspectExecution execution turn
            pure $ case outcome of
                Left SessionUnavailable -> ExecutionRetry
                Left (SessionRejected problem) -> ExecutionFailed problem
                Right snapshot -> validateSessionSnapshot execution snapshot

validateSessionSnapshot :: SessionExecution -> SessionSnapshot -> ExecutionTransition
validateSessionSnapshot execution snapshot
    | snapshot.snapshotSessionId /= execution.executionSessionId
        || snapshot.snapshotRequestId /= execution.executionRequestId
        || Text.null snapshot.snapshotTurnId
        || maybe False (/= snapshot.snapshotTurnId) execution.executionTurnId =
            ExecutionFailed "Session backend returned an unrelated execution"
    | otherwise = case snapshot.snapshotStatus of
        SessionRunning -> ExecutionRunning snapshot.snapshotTurnId
        SessionWaiting requests -> ExecutionWaiting snapshot.snapshotTurnId requests
        SessionCompleted content
            | Text.any (== '\0') content ->
                ExecutionFailed "Session output contains an invalid character"
            | otherwise -> ExecutionCompleted snapshot.snapshotTurnId content
        SessionFailed problem -> ExecutionFailed problem
        SessionCancelled -> ExecutionCancelled

-- | Invoke only after atomically claiming an unexpired callback associated with
-- the current owner, chat and binding revision. Reconcile the remote request
-- again: a persisted button is not evidence that an approval remains pending.
respondToSessionRequest
    :: SessionBackend -> SessionExecution -> HumanRequest -> Value
    -> IO (Either SessionFailure ())
respondToSessionRequest backend execution expected response =
    case execution.executionTurnId of
        Nothing -> pure (Left (SessionRejected "No active session execution"))
        Just turn -> backend.inspectExecution execution turn >>= \case
            Left failure -> pure (Left failure)
            Right snapshot -> case validateSessionSnapshot execution snapshot of
                ExecutionWaiting _ requests | expected `elem` requests ->
                    backend.respondToRequest execution turn expected response
                _ -> pure (Left (SessionRejected "The human request is no longer pending"))

cancelSessionExecution :: SessionBackend -> SessionExecution -> IO (Either SessionFailure ())
cancelSessionExecution backend execution = case execution.executionTurnId of
    Nothing -> pure (Left (SessionRejected "No active session execution"))
    Just turn -> backend.inspectExecution execution turn >>= \case
        Left failure -> pure (Left failure)
        Right snapshot -> case validateSessionSnapshot execution snapshot of
            ExecutionRunning _ -> backend.cancelExecution execution turn
            ExecutionWaiting _ _ -> backend.cancelExecution execution turn
            _ -> pure (Left (SessionRejected "The session execution is no longer active"))
