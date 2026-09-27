-- | Durable adapter for the standalone managed-process session backend.
--
-- A process launch has no idempotency key. Record admission before launching,
-- cache terminal output before returning it to Telegram, and never relaunch an
-- interrupted admission. The shared connector performs identity validation and
-- terminal-state interpretation for this backend and the HTTP backend alike.
module Agent.Telegram.Session.Local (localSessionBackend) where

import Agent.Telegram.Connector.Session
import Agent.FileRetry (writeLazyFileAtomically)
import Control.Exception.Safe (tryAny)
import Data.Aeson
import qualified Data.ByteString.Lazy as Bytes
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (doesFileExist)
import System.FileLock (SharedExclusive(Exclusive), withFileLock)
import System.OsPath (unsafeEncodeUtf)

data LocalCheckpoint = LocalCheckpoint
    { checkpointSession :: !Text
    , checkpointRequest :: !Text
    , checkpointPrompt :: !Text
    , checkpointStatus :: !Text
    , checkpointOutput :: !Text
    }

instance ToJSON LocalCheckpoint where
    toJSON checkpoint = object
        [ "session" .= checkpoint.checkpointSession
        , "request" .= checkpoint.checkpointRequest
        , "prompt" .= checkpoint.checkpointPrompt
        , "status" .= checkpoint.checkpointStatus
        , "output" .= checkpoint.checkpointOutput
        ]

instance FromJSON LocalCheckpoint where
    parseJSON = withObject "Local session checkpoint" \value ->
        LocalCheckpoint <$> value .: "session" <*> value .: "request"
            <*> value .: "prompt" <*> value .: "status" <*> value .: "output"

-- | The caller owns a private checkpoint directory and supplies the exact
-- session launch, cancellation flag and cancellation operation. Human requests
-- remain served by the standalone bridge while the synchronous launch runs.
-- Lock files and terminal checkpoints are retained so a restart cannot mistake
-- previously executed work for a new admission.
localSessionBackend
    :: FilePath
    -> IO Bool
    -> IO ()
    -> IO (Either Text Text)
    -> SessionBackend
localSessionBackend checkpointPath isCancelled cancel launch = SessionBackend
    { submitExecution = \execution ->
        withFileLock (checkpointPath <> ".lock") Exclusive \_ -> do
            exists <- doesFileExist checkpointPath
            if exists then restore execution else do
                cancelled <- isCancelled
                if cancelled then pure (Right (snapshot execution SessionCancelled)) else do
                    save execution "started" ""
                    result <- tryAny launch
                    stopped <- isCancelled
                    case result of
                        _ | stopped -> do
                            save execution "cancelled" ""
                            pure (Right (snapshot execution SessionCancelled))
                        Right (Right content) -> do
                            save execution "completed" content
                            pure (Right (snapshot execution (SessionCompleted content)))
                        Right (Left problem) -> do
                            save execution "failed" problem
                            pure (Right (snapshot execution (SessionFailed problem)))
                        Left _ -> do
                            -- Never store raw exceptions: they may contain credentials.
                            save execution "failed" interruptedMessage
                            pure (Right (snapshot execution (SessionFailed interruptedMessage)))
    , inspectExecution = \execution turn ->
        if turn /= execution.executionRequestId
            then pure (Left (SessionRejected "Local execution identity mismatch"))
            else withFileLock (checkpointPath <> ".lock") Exclusive \_ -> restore execution
    , cancelExecution = \execution turn ->
        if turn /= execution.executionRequestId
            then pure (Left (SessionRejected "Local execution identity mismatch"))
            else cancel >> pure (Right ())
    , respondToRequest = \_ _ _ _ ->
        pure (Left (SessionRejected "Local human requests are served by the active Telegram bridge"))
    }
  where
    snapshot :: SessionExecution -> SessionStatus -> SessionSnapshot
    snapshot execution status = SessionSnapshot
        execution.executionSessionId execution.executionRequestId
        execution.executionRequestId status
    save :: SessionExecution -> Text -> Text -> IO ()
    save execution status content =
        writeLazyFileAtomically (unsafeEncodeUtf checkpointPath) 0o600 $ encode $
            LocalCheckpoint execution.executionSessionId execution.executionRequestId
                execution.executionPrompt status content
    restore :: SessionExecution -> IO (Either SessionFailure SessionSnapshot)
    restore execution = do
        exists <- doesFileExist checkpointPath
        if not exists then pure (Left SessionUnavailable) else do
            decoded <- (eitherDecode <$> Bytes.readFile checkpointPath)
                :: IO (Either String LocalCheckpoint)
            pure $ case decoded of
                Left _ -> Left (SessionRejected "Invalid local execution checkpoint")
                Right checkpoint
                    | checkpoint.checkpointSession /= execution.executionSessionId
                        || checkpoint.checkpointRequest /= execution.executionRequestId
                        || checkpoint.checkpointPrompt /= execution.executionPrompt ->
                            Left (SessionRejected "Local execution checkpoint identity mismatch")
                    | otherwise -> Right $ snapshot execution $
                        case checkpoint.checkpointStatus of
                            "completed" -> SessionCompleted checkpoint.checkpointOutput
                            "cancelled" -> SessionCancelled
                            "failed" -> SessionFailed checkpoint.checkpointOutput
                            _ -> SessionFailed interruptedMessage
    interruptedMessage =
        Text.unwords
            [ "A previous local execution was interrupted and its outcome is uncertain."
            , "It was not submitted again. Inspect the session before requesting new work."
            ]
