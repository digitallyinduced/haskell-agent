-- | Cooperative resource ownership shared by tool workers in one runtime.
-- This is not a filesystem sandbox or a cross-process lock service.
module Agent.Tools.ResourceArbiter
    ( ToolResourceArbiter
    , ToolResourceArbiterError(..)
    , newToolResourceArbiter
    , withToolResources
    , closeToolResourceArbiter
    , waitToolResourceArbiter
    , toolResourceArbiterCounts
    ) where

import Agent.Tools.Scheduling
    ( ToolResourceClaim, ToolSchedulingPlan(..), schedulingPlansConflict )
import Control.Concurrent.STM
    ( STM, TVar, atomically, check, modifyTVar', newTVarIO, readTVar
    , throwSTM, writeTVar
    )
import Control.Exception.Safe (Exception, bracket)
import Control.Monad (when)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data ToolResourceArbiter = ToolResourceArbiter
    { state :: !(TVar ArbiterState)
    , capacity :: !Int
    }

data ArbiterState = ArbiterState
    { closed :: !Bool
    , nextTicket :: !Integer
    , claims :: !(Map Integer Claim)
    }

data Claim = Claim
    { resources :: ![ToolResourceClaim]
    , running :: !Bool
    }

data ToolResourceArbiterError
    = ToolResourceArbiterClosed
    | ToolResourceArbiterFull
    deriving (Eq, Show)

instance Exception ToolResourceArbiterError

-- | Bound both active claims and waiters. Full admission fails immediately,
-- rather than leaving unbounded requests waiting outside a bounded queue.
newToolResourceArbiter :: Int -> IO ToolResourceArbiter
newToolResourceArbiter capacity
    | capacity <= 0 = ioError (userError "resource arbiter capacity must be positive")
    | otherwise =
        ToolResourceArbiter <$> newTVarIO (ArbiterState False 0 Map.empty)
            <*> pure capacity

-- | Acquire the complete claim set atomically. Conflicting requests retain
-- admission order, but unrelated work may pass a blocked request. Cancellation
-- removes either a waiter or an active lease. The action must encompass the
-- actual worker lifetime, including joining any structured children.
--
-- Do not call recursively while holding conflicting resources. In particular,
-- collaboration wait/spawn handlers must not acquire a global exclusive lease.
withToolResources :: ToolResourceArbiter -> [ToolResourceClaim] -> IO a -> IO a
withToolResources arbiter resources action =
    bracket
        (atomically (admit arbiter resources))
        (\ticket -> atomically $
            modifyTVar' arbiter.state \current ->
                current { claims = Map.delete ticket current.claims })
        \ticket -> do
            atomically (acquire arbiter ticket resources)
            action

admit :: ToolResourceArbiter -> [ToolResourceClaim] -> STM Integer
admit arbiter resources = do
    current <- readTVar arbiter.state
    when current.closed (throwSTM ToolResourceArbiterClosed)
    when (Map.size current.claims >= arbiter.capacity)
        (throwSTM ToolResourceArbiterFull)
    let ticket = current.nextTicket
    writeTVar arbiter.state current
        { nextTicket = ticket + 1
        , claims = Map.insert ticket (Claim resources False) current.claims
        }
    pure ticket

acquire :: ToolResourceArbiter -> Integer -> [ToolResourceClaim] -> STM ()
acquire arbiter ticket resources = do
    current <- readTVar arbiter.state
    when current.closed (throwSTM ToolResourceArbiterClosed)
    check $ not $ Map.foldrWithKey
        (\other claim conflict ->
            conflict || (other /= ticket && (claim.running || other < ticket)
                && schedulingPlansConflict
                    (ToolResourceClaims resources)
                    (ToolResourceClaims claim.resources)))
        False current.claims
    writeTVar arbiter.state current
        { claims = Map.adjust (\claim -> claim { running = True })
            ticket current.claims
        }

-- | Reject new work and wake blocked admissions. Does not revoke resources
-- from running work: the owning runtime must cancel/join its workers.
closeToolResourceArbiter :: ToolResourceArbiter -> IO ()
closeToolResourceArbiter arbiter =
    atomically $ modifyTVar' arbiter.state \current -> current { closed = True }

waitToolResourceArbiter :: ToolResourceArbiter -> IO ()
waitToolResourceArbiter arbiter =
    atomically $ readTVar arbiter.state >>= check . Map.null . (.claims)

-- | Consistent diagnostic snapshot: (waiting, running).
toolResourceArbiterCounts :: ToolResourceArbiter -> STM (Int, Int)
toolResourceArbiterCounts arbiter = do
    current <- readTVar arbiter.state
    let active = Map.size (Map.filter (.running) current.claims)
    pure (Map.size current.claims - active, active)
