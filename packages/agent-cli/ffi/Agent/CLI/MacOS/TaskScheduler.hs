module Agent.CLI.MacOS.TaskScheduler
    ( TaskIdentity(..)
    , selectRunnableTasks
    ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

-- | Stable scheduling identity for one native task. A missing session id is a
-- fresh session and therefore does not conflict with another queued task.
data TaskIdentity = TaskIdentity
    { taskIdentityId :: !Text
    , taskIdentitySessionId :: !(Maybe Text)
    } deriving (Eq, Show)

-- | Select at most @capacity@ tasks in queue order while leaving tasks for an
-- already-active (or newly-selected) session queued. Skipping a blocked task
-- lets unrelated sessions make progress without allowing a later turn for the
-- same session to overtake it.
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
