module Agent.Runtime.Daemon.Task
    ( TaskId (..)
    , TaskStatus (..)
    , DurableTask (..)
    , isActive
    , interruptActive
    ) where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

newtype TaskId = TaskId { unTaskId :: Text }
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, FromJSONKey, ToJSON, ToJSONKey)

data TaskStatus
    = TaskQueued
    | TaskRunning
    | TaskCompleted
    | TaskFailed
    | TaskCancelled
    | TaskInterrupted
    deriving stock (Eq, Show, Generic)

instance ToJSON TaskStatus
instance FromJSON TaskStatus

data DurableTask = DurableTask
    { taskId :: TaskId
    , status :: TaskStatus
    , description :: Text
    , updatedAt :: UTCTime
    , logTail :: [Text]
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON DurableTask
instance FromJSON DurableTask

isActive :: DurableTask -> Bool
isActive DurableTask {status} = status == TaskQueued || status == TaskRunning

interruptActive :: UTCTime -> DurableTask -> DurableTask
interruptActive now task@DurableTask {}
    | isActive task = task {status = TaskInterrupted, updatedAt = now}
    | otherwise = task
