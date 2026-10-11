{-# LANGUAGE RankNTypes #-}

-- | Recovery support for hosts retaining a provider's native transcript.
-- This does not run model/tool iterations; it supplies replay checkpoints and
-- bounded connection replacement around an existing loop.
module Agent.Loop.Recovery
    ( ReplayState
    , newReplayState
    , readResumeState
    , prepareReplayAttempt
    , commitReplayResponse
    , ConnectionOutcome(..)
    , ConnectionRecovery(..)
    , runRecoveringConnections
    ) where

import qualified Control.Exception as Exception
import Data.IORef

-- | Single-owner state. Record the resume point before submitting a request,
-- so a transport exception can recover even when no response callback ran.
data ReplayState state item = ReplayState
    { resumeState :: !(IORef state)
    , committedItems :: !(IORef [item])
    }

newReplayState :: state -> IO (ReplayState state item)
newReplayState initial = ReplayState <$> newIORef initial <*> newIORef []

readResumeState :: ReplayState state item -> IO state
readResumeState replay = readIORef replay.resumeState

-- | A request with no continuation already carries its full input. A
-- continuation carries only the new suffix. The host chooses whether its
-- saved state can replay the whole transcript or must retain a continuation.
prepareReplayAttempt
    :: ReplayState state item
    -> Bool
    -> [item]
    -> ([item] -> state)
    -> IO [item]
prepareReplayAttempt replay continues input buildResume = do
    committed <- readIORef replay.committedItems
    let fullInput = (if continues then committed else []) <> input
    writeIORef replay.resumeState (buildResume fullInput)
    pure fullInput

commitReplayResponse
    :: ReplayState state item
    -> ([item] -> [item])
    -> [item]
    -> [item]
    -> IO ()
commitReplayResponse replay normalize input output =
    writeIORef replay.committedItems (normalize (input <> output))

data ConnectionOutcome reason result
    = ConnectionFinished result
    | ReplaceConnection reason

data ConnectionRecovery reason error = ConnectionRecovery
    { replacementLimit :: !Int
    , classifyException :: Exception.SomeException -> Maybe reason
    , exhaustedError :: reason -> error
    -- | One-based replacement number. The host supplies logging and delay.
    , beforeReplacement :: Int -> reason -> IO ()
    }

-- | The source retains ownership of credential failover: an attempt's Left
-- may make it select another account before returning. The replacement budget
-- is shared across every such attempt and is never reset by progress.
--
-- Unclassified exceptions are rethrown with their original identity, including
-- asynchronous timeout exceptions. Classification must recognize only known
-- transport failures, never asynchronous cancellation.
runRecoveringConnections
    :: ConnectionRecovery reason error
    -> (forall a. (connection -> IO (Either error a)) -> IO (Either error a))
    -> (IO Bool -> connection -> IO (Either error (ConnectionOutcome reason result)))
    -> IO (Either error result)
runRecoveringConnections config withConnection attempt = do
    remaining <- newIORef (max 0 config.replacementLimit)
    let available = (> 0) <$> readIORef remaining
        connect number = do
            outcome <- Exception.try (withConnection (attempt available))
            case outcome of
                Right (Right (ConnectionFinished result)) -> pure (Right result)
                Right (Right (ReplaceConnection reason)) -> replace number reason
                Right (Left err) -> pure (Left err)
                Left exception -> case config.classifyException exception of
                    Just reason -> replace number reason
                    Nothing -> Exception.throwIO exception
        replace number reason = do
            permitted <- available
            if permitted
                then do
                    modifyIORef' remaining (subtract 1)
                    config.beforeReplacement (number + 1) reason
                    connect (number + 1)
                else pure (Left (config.exhaustedError reason))
    connect 0
