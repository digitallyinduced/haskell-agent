-- | Session-local Grok goal state and the @update_goal@ tool.
--
-- The upstream goal harness can run an adversarial completion classifier.
-- This implementation deliberately does not claim that behavior: completion
-- is accepted directly and the tool response says that classifier
-- verification is disabled.
module Agent.GrokBuild.Dialect.Goal
    ( GoalRuntime
    , GoalSnapshot(..)
    , GoalStatus(..)
    , newGoalRuntime
    , updateGoalTool
    , activateGoal
    , pauseGoal
    , resumeGoal
    , clearGoal
    , readGoal
    , formatGoalSnapshot
    ) where

import Agent.GrokBuild.Dialect.Common (jsonTool)
import qualified Agent.Json.Decode as Json
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch (typedTool)
import Agent.GrokBuild.Dialect.Json (optionalBool, optionalText)
import Agent.Tools.Types (AppTool, ToolExecutionPolicy(..))
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , newMVar
    , readMVar
    )
import Data.Aeson (object, (.=))
import qualified Data.Aeson.Text as Aeson
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText

data GoalStatus
    = GoalActive
    | GoalUserPaused
    | GoalBlocked
    | GoalComplete
    deriving (Eq, Show)

data GoalSnapshot = GoalSnapshot
    { goalObjective :: !Text
    , goalTokenBudget :: !(Maybe Int)
    , goalStatus :: !GoalStatus
    , goalProgress :: ![Text]
    , goalBlockedReason :: !(Maybe Text)
    } deriving (Eq, Show)

newtype GoalRuntime = GoalRuntime (MVar (Maybe GoalSnapshot))

newGoalRuntime :: IO GoalRuntime
newGoalRuntime = GoalRuntime <$> newMVar Nothing

activateGoal
    :: GoalRuntime
    -> Text
    -> Maybe Int
    -> IO (Either Text GoalSnapshot)
activateGoal (GoalRuntime state) objective budget
    | Text.null stripped =
        pure (Left "goal objective must be non-empty")
    | maybe False (<= 0) budget =
        pure (Left "goal token budget must be a positive integer")
    | otherwise =
        modifyMVar state \_ -> do
            let snapshot = GoalSnapshot
                    { goalObjective = stripped
                    , goalTokenBudget = budget
                    , goalStatus = GoalActive
                    , goalProgress = []
                    , goalBlockedReason = Nothing
                    }
            pure (Just snapshot, Right snapshot)
  where
    stripped = Text.strip objective

pauseGoal :: GoalRuntime -> IO (Either Text GoalSnapshot)
pauseGoal (GoalRuntime state) =
    modifyMVar state \case
        Nothing ->
            pure (Nothing, Left "No active goal to pause.")
        Just goal
            | goal.goalStatus == GoalActive -> do
                let updated = goal
                        { goalStatus = GoalUserPaused
                        , goalBlockedReason = Nothing
                        }
                pure (Just updated, Right updated)
            | otherwise ->
                pure
                    ( Just goal
                    , Left
                        ("Goal is not active; current status is "
                            <> goalStatusName goal.goalStatus <> ".")
                    )

resumeGoal :: GoalRuntime -> IO (Either Text GoalSnapshot)
resumeGoal (GoalRuntime state) =
    modifyMVar state \case
        Nothing ->
            pure (Nothing, Left "No goal to resume.")
        Just goal
            | goal.goalStatus `elem` [GoalUserPaused, GoalBlocked] -> do
                let updated = goal
                        { goalStatus = GoalActive
                        , goalBlockedReason = Nothing
                        }
                pure (Just updated, Right updated)
            | otherwise ->
                pure
                    ( Just goal
                    , Left
                        ("Goal is not paused; current status is "
                            <> goalStatusName goal.goalStatus <> ".")
                    )

clearGoal :: GoalRuntime -> IO Bool
clearGoal (GoalRuntime state) =
    modifyMVar state \current ->
        pure (Nothing, maybe False (const True) current)

readGoal :: GoalRuntime -> IO (Maybe GoalSnapshot)
readGoal (GoalRuntime state) = readMVar state

formatGoalSnapshot :: GoalSnapshot -> Text
formatGoalSnapshot goal =
    Text.unlines $
        [ "objective: " <> goal.goalObjective
        , "status: " <> goalStatusName goal.goalStatus
        ]
        <> maybe
            []
            (\budget ->
                [ "advisory_token_budget: "
                    <> Text.pack (show budget)
                    <> " (not hard-enforced)"
                ])
            goal.goalTokenBudget
        <> case goal.goalProgress of
            [] -> []
            progress ->
                "progress:" : map ("- " <>) progress
        <> maybe []
            (\reason -> ["blocked_reason: " <> reason])
            goal.goalBlockedReason

data UpdateGoalArgs = UpdateGoalArgs
    { completed :: !(Maybe Bool)
    , message :: !(Maybe Text)
    , blockedReason :: !(Maybe Text)
    }

updateGoalArgsDecoder :: Json.Decoder UpdateGoalArgs
updateGoalArgsDecoder = Json.object $
    UpdateGoalArgs
        <$> optionalBool "completed"
        <*> optionalText "message"
        <*> optionalText "blocked_reason"

updateGoalTool :: GoalRuntime -> AppTool
updateGoalTool runtime =
    jsonTool
        "update_goal"
        "Report progress on the active goal. Use the parameters to log a status message, mark the goal completed, or flag that you're blocked."
        [ PropertySchema "completed" PropertyBoolean False $ Just
            "Set to true ONLY when the goal is fully achieved. This ends goal mode. Use together with message to include a completion summary."
        , PropertySchema "message" PropertyString False $ Just
            "Optional short message logged as progress. Use with completed=true for a completion summary."
        , PropertySchema "blocked_reason" PropertyString False $ Just
            "Set only when truly stuck after 3+ consecutive failed attempts at the same problem. This pauses the goal as blocked."
        ]
        True
        TurnSequential
        (typedTool "update_goal" updateGoalArgsDecoder (runUpdateGoal runtime))

runUpdateGoal
    :: GoalRuntime
    -> UpdateGoalArgs
    -> IO (Either Text Text)
runUpdateGoal (GoalRuntime state) args =
    modifyMVar state \case
        Nothing ->
            pure
                ( Nothing
                , Left
                    "goal_update_harness_disabled: No active goal to update. Start one with /goal <objective>."
                )
        Just goal
            | goal.goalStatus /= GoalActive ->
                pure
                    ( Just goal
                    , Left
                        ("goal_update_non_active: Goal is not active; current status is "
                            <> goalStatusName goal.goalStatus <> ".")
                    )
            | args.completed == Just True
            , Just reason <- nonBlank args.blockedReason ->
                pure
                    ( Just goal
                    , Left
                        ("goal_update_invalid: completed=true conflicts with blocked_reason: "
                            <> reason)
                    )
            | Just reason <- nonBlank args.blockedReason -> do
                let progress = appendProgress goal.goalProgress args.message
                    updated = goal
                        { goalStatus = GoalBlocked
                        , goalProgress = progress
                        , goalBlockedReason = Just reason
                        }
                    summary =
                        "Goal paused as blocked: " <> reason
                            <> maybe "" ("\nProgress: " <>) (nonBlank args.message)
                pure (Just updated, Right (successOutput summary))
            | args.completed == Just True -> do
                let updated = goal
                        { goalStatus = GoalComplete
                        , goalProgress =
                            appendProgress goal.goalProgress args.message
                        , goalBlockedReason = Nothing
                        }
                    summary =
                        "Goal marked complete (automatic classifier verification is disabled in this host)."
                            <> maybe "" ("\nSummary: " <>) (nonBlank args.message)
                pure (Just updated, Right (successOutput summary))
            | Just note <- nonBlank args.message -> do
                let updated = goal
                        { goalProgress = goal.goalProgress <> [note] }
                pure
                    ( Just updated
                    , Right (successOutput ("Progress recorded: " <> note))
                    )
            | otherwise ->
                pure
                    ( Just goal
                    , Left
                        "goal_update_invalid: Provide message, blocked_reason, or completed=true."
                    )

appendProgress :: [Text] -> Maybe Text -> [Text]
appendProgress progress message =
    progress <> catMaybes [nonBlank message]

nonBlank :: Maybe Text -> Maybe Text
nonBlank = (>>= keep)
  where
    keep value =
        let stripped = Text.strip value
        in if Text.null stripped then Nothing else Just stripped

successOutput :: Text -> Text
successOutput summary =
    LazyText.toStrict $ Aeson.encodeToLazyText $ object
        [ "success" .= True
        , "summary" .= summary
        ]

goalStatusName :: GoalStatus -> Text
goalStatusName = \case
    GoalActive -> "active"
    GoalUserPaused -> "user_paused"
    GoalBlocked -> "blocked"
    GoalComplete -> "complete"
