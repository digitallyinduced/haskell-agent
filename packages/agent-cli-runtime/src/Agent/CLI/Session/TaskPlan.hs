-- | Conversion between runtime and persisted task-plan representations.
module Agent.CLI.Session.TaskPlan
    ( fromStoredTaskPlan
    , toStoredTaskPlanItem
    , taskPlanTransferJson
    ) where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Agent.Store.Postgres.Session as Store
import Agent.Tools.TaskPlan
    ( CurrentTaskPlan(..)
    , TaskPlan(..)
    , TaskPlanItem(..)
    , TaskPlanStatus(..)
    )

fromStoredTaskPlan :: Store.SessionTaskPlan -> CurrentTaskPlan
fromStoredTaskPlan stored = CurrentTaskPlan
    { currentTaskPlanRevision = stored.sessionTaskPlanRevision
    , currentTaskPlanValue = TaskPlan
        { taskPlanExplanation = stored.sessionTaskPlanExplanation
        , taskPlanItems = map fromStoredTaskPlanItem stored.sessionTaskPlanItems
        }
    }

fromStoredTaskPlanItem :: Store.SessionTaskPlanItem -> TaskPlanItem
fromStoredTaskPlanItem stored = TaskPlanItem
    { taskPlanStep = stored.sessionTaskPlanItemStep
    , taskPlanStatus = case stored.sessionTaskPlanItemStatus of
        Store.SessionTaskPlanPending -> TaskPlanPending
        Store.SessionTaskPlanInProgress -> TaskPlanInProgress
        Store.SessionTaskPlanCompleted -> TaskPlanCompleted
    }

toStoredTaskPlanItem :: TaskPlanItem -> Store.SessionTaskPlanItem
toStoredTaskPlanItem item = Store.SessionTaskPlanItem
    { Store.sessionTaskPlanItemStep = item.taskPlanStep
    , Store.sessionTaskPlanItemStatus = case item.taskPlanStatus of
        TaskPlanPending -> Store.SessionTaskPlanPending
        TaskPlanInProgress -> Store.SessionTaskPlanInProgress
        TaskPlanCompleted -> Store.SessionTaskPlanCompleted
    }

taskPlanTransferJson :: TaskPlan -> Aeson.Value
taskPlanTransferJson plan = Aeson.object
    [ "explanation" Aeson..= plan.taskPlanExplanation
    , "plan" Aeson..= map itemJson plan.taskPlanItems
    ]
  where
    itemJson item = Aeson.object
        [ "step" Aeson..= item.taskPlanStep
        , "status" Aeson..= case item.taskPlanStatus of
            TaskPlanPending -> ("pending" :: Text)
            TaskPlanInProgress -> "in_progress"
            TaskPlanCompleted -> "completed"
        ]
