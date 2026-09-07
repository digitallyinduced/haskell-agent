-- | Serialized ownership of a turn's optional activity marker. A turn can
-- start before its durable session exists; persistence may install the marker
-- later, but never after the turn has ended.
module Agent.CLI.Session.Activity
    ( TurnActivity
    , newTurnActivity
    , beginTurnActivity
    , acquireTurnActivity
    , endTurnActivity
    ) where

import Control.Concurrent.MVar
    ( MVar
    , modifyMVarMasked
    , modifyMVarMasked_
    , newMVar
    )
import Data.Maybe (isJust)

data ActivityState lock
    = Inactive
    | Active !(Maybe lock)

newtype TurnActivity lock = TurnActivity (MVar (ActivityState lock))

newTurnActivity :: IO (TurnActivity lock)
newTurnActivity = TurnActivity <$> newMVar Inactive

beginTurnActivity :: TurnActivity lock -> IO ()
beginTurnActivity (TurnActivity state) =
    modifyMVarMasked_ state \case
        Inactive -> pure (Active Nothing)
        active -> pure active

-- | Acquire at most one marker for an active turn. The acquisition is inside
-- the same critical section as cleanup, so ending a turn waits for any marker
-- being acquired and releases it before publishing the inactive state.
acquireTurnActivity :: TurnActivity lock -> IO (Maybe lock) -> IO Bool
acquireTurnActivity (TurnActivity state) acquire =
    modifyMVarMasked state \case
        Active Nothing -> do
            lock <- acquire
            pure (Active lock, isJust lock)
        current -> pure (current, False)

endTurnActivity :: TurnActivity lock -> (lock -> IO ()) -> IO ()
endTurnActivity (TurnActivity state) release =
    modifyMVarMasked_ state \case
        Inactive -> pure Inactive
        Active lock -> do
            mapM_ release lock
            pure Inactive
