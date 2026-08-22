module Agent.Retry
    ( ExceptionRetry(..)
    , handleSyncExceptions
    , retryingBeforeCommit
    ) where

import qualified Control.Exception.Safe as Safe
import Control.Retry (RetryPolicyM, retrying)
import Data.IORef (newIORef, readIORef, writeIORef)

-- | How a synchronous exception raised before an operation commits should be
-- represented. Both constructors return the error as data; only
-- 'RetryException' asks the supplied retry policy for another attempt.
data ExceptionRetry error
    = RetryException !error
    | StopException !error
    deriving (Eq, Show)

-- | Normalize any synchronous exception from an action into its ordinary
-- error channel. Asynchronous cancellation remains an exception.
handleSyncExceptions
    :: (Safe.SomeException -> error)
    -> IO (Either error value)
    -> IO (Either error value)
handleSyncExceptions classify action =
    Safe.tryAny action >>= \case
        Left exception -> pure (Left (classify exception))
        Right result -> pure result

-- | Retry synchronous exceptions only until the action crosses its explicit
-- commit boundary.
--
-- Exceptions raised before @markCommitted@ are normalized with @classify@ and
-- returned as ordinary errors when the policy stops. Exceptions raised after
-- the boundary are rethrown unchanged because replaying the action could
-- duplicate externally visible effects. 'Safe.tryAny' intentionally leaves
-- asynchronous cancellation alone.
retryingBeforeCommit
    :: RetryPolicyM IO
    -> (Safe.SomeException -> ExceptionRetry error)
    -> ((IO () -> IO (Either error value)))
    -> IO (Either error value)
retryingBeforeCommit policy classify action = do
    committedRef <- newIORef False
    outcome <- retrying policy shouldRetry \_retryStatus -> do
        writeIORef committedRef False
        Safe.tryAny (action (writeIORef committedRef True)) >>= \case
            Right result -> pure (Right result)
            Left exception -> do
                committed <- readIORef committedRef
                if committed
                    then Safe.throwIO exception
                    else pure (Left (classify exception))
    pure $ case outcome of
        Right result -> result
        Left (RetryException err) -> Left err
        Left (StopException err) -> Left err
  where
    shouldRetry _retryStatus = \case
        Left RetryException{} -> pure True
        _ -> pure False
