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
    , requestTurnYield
    , isTurnYieldRequested
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
data CancelFlag = CancelFlag (TMVar ()) (TMVar ())

newCancelFlag :: IO CancelFlag
newCancelFlag = CancelFlag <$> newEmptyTMVarIO <*> newEmptyTMVarIO

-- | Signal cancel. Idempotent: later calls are no-ops once latched.
requestCancel :: CancelFlag -> IO ()
requestCancel (CancelFlag var _) = atomically do
    _ <- tryPutTMVar var ()
    pure ()

isCancelled :: CancelFlag -> IO Bool
isCancelled (CancelFlag var _) = atomically (not <$> isEmptyTMVar var)

-- | Block until cancel is requested.
waitCancel :: CancelFlag -> IO ()
waitCancel (CancelFlag var _) = atomically (readTMVar var)

-- | Clear a previous cancel so the flag can be reused for another turn.
resetCancel :: CancelFlag -> IO ()
resetCancel (CancelFlag var yielded) = atomically do
    _ <- tryTakeTMVar var
    _ <- tryTakeTMVar yielded
    pure ()

-- | Stop after the current tool batch has finished, retaining its results.
-- Unlike cancellation this never interrupts an executing tool.
requestTurnYield :: CancelFlag -> IO ()
requestTurnYield (CancelFlag _ yielded) = atomically do
    _ <- tryPutTMVar yielded ()
    pure ()

isTurnYieldRequested :: CancelFlag -> IO Bool
isTurnYieldRequested (CancelFlag _ yielded) =
    atomically (not <$> isEmptyTMVar yielded)
