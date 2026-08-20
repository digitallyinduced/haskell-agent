-- | Soft-cancel signal shared by the agent loop and long-running tools.
--
-- Soft-cancel asks the current turn to stop and return to the REPL without
-- tearing the process down. The CLI maps the first Ctrl-C (and Esc) onto
-- this flag; a second Ctrl-C still raises 'UserInterrupt' for full exit.
module Agent.Cancel
    ( CancelFlag
    , newCancelFlag
    , requestCancel
    , isCancelled
    , waitCancel
    , resetCancel
    ) where

import Control.Concurrent.STM
    ( TMVar
    , atomically
    , isEmptyTMVar
    , newEmptyTMVarIO
    , readTMVar
    , tryPutTMVar
    , tryTakeTMVar
    )

-- | One-shot cancel latch for a single agent turn.
newtype CancelFlag = CancelFlag (TMVar ())

newCancelFlag :: IO CancelFlag
newCancelFlag = CancelFlag <$> newEmptyTMVarIO

-- | Signal cancel. Idempotent: later calls are no-ops once latched.
requestCancel :: CancelFlag -> IO ()
requestCancel (CancelFlag var) = atomically do
    _ <- tryPutTMVar var ()
    pure ()

isCancelled :: CancelFlag -> IO Bool
isCancelled (CancelFlag var) = atomically (not <$> isEmptyTMVar var)

-- | Block until cancel is requested.
waitCancel :: CancelFlag -> IO ()
waitCancel (CancelFlag var) = atomically (readTMVar var)

-- | Clear a previous cancel so the flag can be reused for another turn.
resetCancel :: CancelFlag -> IO ()
resetCancel (CancelFlag var) = atomically do
    _ <- tryTakeTMVar var
    pure ()
