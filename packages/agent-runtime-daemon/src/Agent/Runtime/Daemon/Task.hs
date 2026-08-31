module Agent.Runtime.Daemon.Task
    ( TaskId (..)
    , TaskStatus (..)
    , DurableTask (..)
    , isActive
    , interruptActive
    ) where

import Data.Aeson
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
    , sessionId :: Maybe Text
    , status :: TaskStatus
    , description :: Text
    , workingDirectory :: FilePath
    , provider :: Maybe Text
    , model :: Maybe Text
    , effort :: Maybe Text
    , worktree :: Bool
    , attempt :: Int
    , updatedAt :: UTCTime
    , logTail :: [Text]
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON DurableTask where
    toJSON task =
        object
            [ "taskId" .= task.taskId
            , "sessionId" .= task.sessionId
            , "status" .= task.status
            , "description" .= task.description
            , "workingDirectory" .= task.workingDirectory
            , "provider" .= task.provider
            , "model" .= task.model
            , "effort" .= task.effort
            , "worktree" .= task.worktree
            , "attempt" .= task.attempt
            , "updatedAt" .= task.updatedAt
            , "logTail" .= task.logTail
            ]

-- Defaults keep snapshots written by the daemon foundation readable.
instance FromJSON DurableTask where
    parseJSON = withObject "DurableTask" $ \value ->
        DurableTask
            <$> value .: "taskId"
            <*> value .:? "sessionId"
            <*> value .: "status"
            <*> value .: "description"
            <*> value .:? "workingDirectory" .!= "."
            <*> value .:? "provider"
            <*> value .:? "model"
            <*> value .:? "effort"
            <*> value .:? "worktree" .!= False
            <*> value .:? "attempt" .!= 1
            <*> value .: "updatedAt"
            <*> value .: "logTail"

isActive :: DurableTask -> Bool
isActive DurableTask {status} = status == TaskQueued || status == TaskRunning

interruptActive :: UTCTime -> DurableTask -> DurableTask
interruptActive now task@DurableTask {}
    | isActive task = task {status = TaskInterrupted, updatedAt = now}
    | otherwise = task
