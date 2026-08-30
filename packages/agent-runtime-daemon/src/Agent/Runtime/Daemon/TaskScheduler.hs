module Agent.Runtime.Daemon.TaskScheduler
    ( TaskIdentity(..)
    , selectRunnableTasks
    ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

-- | Stable scheduling identity. Tasks belonging to the same persisted session
-- are serialized; fresh sessions do not conflict with one another.
data TaskIdentity = TaskIdentity
    { taskIdentityId :: !Text
    , taskIdentitySessionId :: !(Maybe Text)
    } deriving (Eq, Show)

-- | Select at most @capacity@ tasks in queue order. A task blocked by its
-- session does not prevent an unrelated session later in the queue from
-- running, while later work for the same session cannot overtake it.
selectRunnableTasks
    :: Int
    -> Set Text
    -> [(TaskIdentity, task)]
    -> ([(TaskIdentity, task)], [(TaskIdentity, task)])
selectRunnableTasks rawCapacity activeSessions =
    go (max 0 rawCapacity) activeSessions [] []
  where
    go _ _ selected deferred [] =
        (reverse selected, reverse deferred)
    go 0 _ selected deferred remaining =
        (reverse selected, reverse deferred <> remaining)
    go capacity sessions selected deferred (candidate@(identity, _) : rest) =
        case identity.taskIdentitySessionId of
            Just sessionId
                | Set.member sessionId sessions ->
                    go capacity sessions selected (candidate : deferred) rest
                | otherwise ->
                    go
                        (capacity - 1)
                        (Set.insert sessionId sessions)
                        (candidate : selected)
                        deferred
                        rest
            Nothing ->
                go
                    (capacity - 1)
                    sessions
                    (candidate : selected)
                    deferred
                    rest
