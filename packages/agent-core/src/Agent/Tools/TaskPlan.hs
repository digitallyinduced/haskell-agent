-- | Durable, session-scoped progress-plan state.
--
-- The current plan is authoritative state rather than something reconstructed
-- from model history.  Persistence hooks are deliberately small so callers can
-- back the state with a database without coupling agent-core to a store.
module Agent.Tools.TaskPlan
    ( TaskPlanStatus(..)
    , TaskPlanItem(..)
    , TaskPlan(..)
    , CurrentTaskPlan(..)
    , TaskPlanInputItem(..)
    , TaskPlanUpdate(..)
    , TaskPlanHooks(..)
    , TaskPlanEnv
    , TaskPlanReminder
    , taskPlanReminderText
    , taskPlanUpdateDecoder
    , validateTaskPlanUpdate
    , renderTaskPlanUpdate
    , newTaskPlanEnv
    , readTaskPlan
    , replaceTaskPlan
    , clearTaskPlan
    , resetTaskPlanState
    , taskPlanContextText
    , currentTaskPlanContextText
    , takeTaskPlanReminder
    , restoreTaskPlanReminder
    , isTaskPlanContextText
    , taskPlanContextMarker
    ) where

import qualified Agent.Json.Decode as Json
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception.Safe (mask)
import Data.Int (Int64)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text

data TaskPlanStatus
    = TaskPlanPending
    | TaskPlanInProgress
    | TaskPlanCompleted
    deriving (Eq, Show)

data TaskPlanItem = TaskPlanItem
    { taskPlanStep :: !Text
    , taskPlanStatus :: !TaskPlanStatus
    } deriving (Eq, Show)

data TaskPlan = TaskPlan
    { taskPlanExplanation :: !(Maybe Text)
    , taskPlanItems :: ![TaskPlanItem]
    } deriving (Eq, Show)

data CurrentTaskPlan = CurrentTaskPlan
    { currentTaskPlanRevision :: !Int64
    , currentTaskPlanValue :: !TaskPlan
    } deriving (Eq, Show)

-- | Wire representation.  Status remains textual until validation so invalid
-- calls retain the established, useful validation error.
data TaskPlanInputItem = TaskPlanInputItem
    { taskPlanInputStep :: !Text
    , taskPlanInputStatus :: !Text
    } deriving (Eq, Show)

data TaskPlanUpdate = TaskPlanUpdate
    { taskPlanUpdateExplanation :: !(Maybe Text)
    , taskPlanUpdateItems :: ![TaskPlanInputItem]
    } deriving (Eq, Show)

data TaskPlanHooks = TaskPlanHooks
    { taskPlanPersistReplace :: !(TaskPlan -> IO (Either Text Int64))
    , taskPlanPersistClear :: !(IO (Either Text ()))
    }

data TaskPlanReminderState
    = TaskPlanReminderInactive
    | TaskPlanReminderPending
    | TaskPlanReminderConsumed

data TaskPlanMemory
    = TaskPlanEmpty
    | TaskPlanPresent !CurrentTaskPlan !TaskPlanReminderState

newtype TaskPlanReminder = TaskPlanReminder CurrentTaskPlan
    deriving (Eq, Show)

data TaskPlanEnv = TaskPlanEnv
    { taskPlanMemoryRef :: !(IORef TaskPlanMemory)
    , taskPlanHooks :: !(Maybe TaskPlanHooks)
    , taskPlanMutationLock :: !(MVar ())
    }

taskPlanUpdateDecoder :: Json.Decoder TaskPlanUpdate
taskPlanUpdateDecoder = Json.object $
    TaskPlanUpdate
        <$> optionalNonEmptyText "explanation"
        <*> Json.atKey "plan" (Json.list inputItemDecoder)
  where
    inputItemDecoder = Json.object $
        TaskPlanInputItem
            <$> Json.atKey "step" Json.text
            <*> Json.atKey "status" Json.text

optionalNonEmptyText :: Text -> Json.FieldsDecoder (Maybe Text)
optionalNonEmptyText key =
    fmap (>>= nonEmpty) (Json.optionalKey key Json.text)
  where
    nonEmpty value
        | Text.null value = Nothing
        | otherwise = Just value

validateTaskPlanUpdate :: TaskPlanUpdate -> Either Text TaskPlan
validateTaskPlanUpdate update = do
    items <- traverse validateItem update.taskPlanUpdateItems
    if length (filter (\item -> item.taskPlanStatus == TaskPlanInProgress) items) > 1
        then Left "At most one step can be in_progress at a time."
        else Right TaskPlan
            { taskPlanExplanation = update.taskPlanUpdateExplanation
            , taskPlanItems = items
            }
  where
    validateItem :: TaskPlanInputItem -> Either Text TaskPlanItem
    validateItem item = TaskPlanItem item.taskPlanInputStep
        <$> case item.taskPlanInputStatus of
            "pending" -> Right TaskPlanPending
            "in_progress" -> Right TaskPlanInProgress
            "completed" -> Right TaskPlanCompleted
            _ -> Left "Each plan status must be pending, in_progress, or completed."

renderTaskPlanUpdate :: TaskPlan -> Text
renderTaskPlanUpdate plan =
    header <> Text.unlines (map renderItem plan.taskPlanItems)
  where
    header = case plan.taskPlanExplanation of
        Nothing -> "Plan updated:\n"
        Just explanation -> explanation <> "\nPlan updated:\n"
    renderItem :: TaskPlanItem -> Text
    renderItem item =
        "- [" <> renderStatus item.taskPlanStatus <> "] " <> item.taskPlanStep

-- | Construct an environment.  A supplied initial plan is considered resumed
-- durable state and therefore produces one reminder when next requested.
newTaskPlanEnv
    :: Maybe CurrentTaskPlan
    -> Maybe TaskPlanHooks
    -> IO TaskPlanEnv
newTaskPlanEnv initial hooks = do
    memoryRef <- newIORef (resumeTaskPlanMemory initial)
    mutationLock <- newMVar ()
    pure TaskPlanEnv
        { taskPlanMemoryRef = memoryRef
        , taskPlanHooks = hooks
        , taskPlanMutationLock = mutationLock
        }

readTaskPlan :: TaskPlanEnv -> IO (Maybe CurrentTaskPlan)
readTaskPlan env =
    currentTaskPlan <$> readIORef env.taskPlanMemoryRef

-- | Persist first, then publish in memory.  Async exceptions may interrupt the
-- persistence operation, but cannot land between its success and publication.
replaceTaskPlan
    :: TaskPlanEnv
    -> TaskPlan
    -> IO (Either Text CurrentTaskPlan)
replaceTaskPlan env plan =
    mask \restore -> modifyMVar env.taskPlanMutationLock \lock -> do
        persisted <- restore $ case env.taskPlanHooks of
            Just hooks -> hooks.taskPlanPersistReplace plan
            Nothing -> do
                current <- readTaskPlan env
                pure $ Right $ maybe 1 (\value -> value.currentTaskPlanRevision + 1) current
        case persisted of
            Left err -> pure (lock, Left err)
            Right revision -> do
                let current = CurrentTaskPlan revision plan
                writeIORef env.taskPlanMemoryRef
                    (publishTaskPlanMemory current)
                pure (lock, Right current)

-- | Clear durable state before clearing the in-memory projection.
clearTaskPlan :: TaskPlanEnv -> IO (Either Text ())
clearTaskPlan env =
    mask \restore -> modifyMVar env.taskPlanMutationLock \lock -> do
        persisted <- restore $ case env.taskPlanHooks of
            Just hooks -> hooks.taskPlanPersistClear
            Nothing -> pure (Right ())
        case persisted of
            Left err -> pure (lock, Left err)
            Right () -> do
                writeIORef env.taskPlanMemoryRef TaskPlanEmpty
                pure (lock, Right ())

-- | Replace only the in-memory projection after the host switches the
-- persistence hooks to a different session. The supplied value must already
-- reflect that session's authoritative database state.
resetTaskPlanState
    :: TaskPlanEnv
    -> Maybe CurrentTaskPlan
    -> IO ()
resetTaskPlanState env current =
    modifyMVar env.taskPlanMutationLock \lock -> do
        writeIORef env.taskPlanMemoryRef (resumeTaskPlanMemory current)
        pure (lock, ())

taskPlanContextMarker :: Text
taskPlanContextMarker = "<task-plan-state"

taskPlanContextLimit :: Int
taskPlanContextLimit = 12000

taskPlanContextText :: CurrentTaskPlan -> Text
taskPlanContextText current =
    boundContext $
        "<task-plan-state revision=\""
            <> Text.pack (show current.currentTaskPlanRevision)
            <> "\">\n"
            <> "This is the authoritative current task plan. Continue from it; do not reconstruct a plan from earlier messages.\n"
            <> explanation
            <> Text.concat (map renderItem plan.taskPlanItems)
            <> "</task-plan-state>"
  where
    plan = current.currentTaskPlanValue
    explanation = case plan.taskPlanExplanation of
        Nothing -> ""
        Just value -> "Explanation: " <> neutralize value <> "\n"
    renderItem :: TaskPlanItem -> Text
    renderItem item =
        "- [" <> renderStatus item.taskPlanStatus <> "] "
            <> neutralize item.taskPlanStep <> "\n"

currentTaskPlanContextText :: TaskPlanEnv -> IO (Maybe Text)
currentTaskPlanContextText env =
    fmap taskPlanContextText <$> readTaskPlan env

taskPlanReminderText :: TaskPlanReminder -> Text
taskPlanReminderText (TaskPlanReminder current) =
    taskPlanContextText current

-- | Consume the one-shot reminder used when a runtime resumes persisted state.
takeTaskPlanReminder :: TaskPlanEnv -> IO (Maybe TaskPlanReminder)
takeTaskPlanReminder env =
    atomicModifyIORef' env.taskPlanMemoryRef takeTaskPlanReminderStep

-- | Requeue a consumed resume reminder when prompt preparation fails before
-- the corresponding input can become part of canonical history.  The token
-- identifies the plan that was consumed, so a delayed failure cannot requeue
-- a replacement plan.
restoreTaskPlanReminder :: TaskPlanEnv -> TaskPlanReminder -> IO ()
restoreTaskPlanReminder env reminder =
    atomicModifyIORef' env.taskPlanMemoryRef \memory ->
        (restoreTaskPlanReminderStep reminder memory, ())

resumeTaskPlanMemory :: Maybe CurrentTaskPlan -> TaskPlanMemory
resumeTaskPlanMemory = \case
    Nothing -> TaskPlanEmpty
    Just current -> TaskPlanPresent current TaskPlanReminderPending

publishTaskPlanMemory :: CurrentTaskPlan -> TaskPlanMemory
publishTaskPlanMemory current =
    TaskPlanPresent current TaskPlanReminderInactive

currentTaskPlan :: TaskPlanMemory -> Maybe CurrentTaskPlan
currentTaskPlan = \case
    TaskPlanEmpty -> Nothing
    TaskPlanPresent current _ -> Just current

takeTaskPlanReminderStep
    :: TaskPlanMemory
    -> (TaskPlanMemory, Maybe TaskPlanReminder)
takeTaskPlanReminderStep = \case
    TaskPlanPresent current TaskPlanReminderPending ->
        ( TaskPlanPresent current TaskPlanReminderConsumed
        , Just (TaskPlanReminder current)
        )
    memory -> (memory, Nothing)

restoreTaskPlanReminderStep
    :: TaskPlanReminder
    -> TaskPlanMemory
    -> TaskPlanMemory
restoreTaskPlanReminderStep (TaskPlanReminder consumed) = \case
    TaskPlanPresent current TaskPlanReminderConsumed
        | current == consumed ->
            TaskPlanPresent current TaskPlanReminderPending
    memory -> memory

isTaskPlanContextText :: Text -> Bool
isTaskPlanContextText =
    Text.isPrefixOf taskPlanContextMarker . Text.stripStart

renderStatus :: TaskPlanStatus -> Text
renderStatus = \case
    TaskPlanPending -> "pending"
    TaskPlanInProgress -> "in_progress"
    TaskPlanCompleted -> "completed"

-- Prevent user-controlled text from closing or opening our context element.
neutralize :: Text -> Text
neutralize = Text.replace ">" "›" . Text.replace "<" "‹"

boundContext :: Text -> Text
boundContext text
    | Text.length text <= taskPlanContextLimit = text
    | otherwise =
        Text.take (taskPlanContextLimit - Text.length suffix) text <> suffix
  where
    suffix = "\n[task plan context truncated]\n</task-plan-state>"
