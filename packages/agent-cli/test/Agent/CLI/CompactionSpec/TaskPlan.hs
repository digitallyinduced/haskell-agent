module Agent.CLI.CompactionSpec.TaskPlan (spec) where

import Agent.CLI.Compaction
    ( CompactOutcome(..)
    , decorateCompactOutcomeWithTaskPlan
    , decorateCompactOutcomeWithTaskPlanWithin
    , runProviderCompactWith
    )
import Agent.CLI.CompactionSpec.Fixtures
import Agent.OpenAI.Compaction
    ( compactionTriggerItem, estimateRequestTokensWithItems
    , summarizationPrompt, userTextItem )
import Agent.Provider (Provider(..))
import Agent.Responses.Types
import Agent.Tools.TaskPlan
import Data.IORef
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = do
    describe "task-plan compaction context" do
        it "removes generated task plans before remote compaction" do
            let stale =
                    taskPlanContextText $
                        CurrentTaskPlan 8 $
                            TaskPlan Nothing
                                [TaskPlanItem "stale" TaskPlanInProgress]
                history =
                    [ taskPlanMessage RoleDeveloper stale
                    , userTextItem stale
                    , userTextItem "retained"
                    ]
            params <- testRequestState defaultResponseCreateParams
            transcript <- newIORef history
            requests <- newIORef []
            result <-
                runProviderCompactWith
                    (Just \request -> do
                        modifyIORef' requests (<> [request])
                        pure (Right remoteCompactionResponse))
                    (const (pure ()))
                    OpenAIProvider
                    Nothing
                    params
                    transcript
                    Nothing
            result `shouldSatisfy` either (const False) (const True)
            map requestItems <$> readIORef requests
                `shouldReturn`
                    [ [ userTextItem "retained"
                      , compactionTriggerItem
                      ]
                    ]

        it "removes generated task plans before local summarization" do
            let stale =
                    taskPlanContextText $
                        CurrentTaskPlan 8 $
                            TaskPlan Nothing
                                [TaskPlanItem "stale" TaskPlanInProgress]
                history =
                    [ taskPlanMessage RoleDeveloper stale
                    , userTextItem stale
                    , userTextItem "retained"
                    ]
                focus = Just "focus on the remaining work"
            params <- testRequestState defaultResponseCreateParams
            transcript <- newIORef history
            requests <- newIORef []
            result <-
                runProviderCompactWith
                    (Just \request -> do
                        modifyIORef' requests (<> [request])
                        pure (Right (summaryResponse "local summary")))
                    (const (pure ()))
                    OpenAIProvider
                    Nothing
                    params
                    transcript
                    focus
            result `shouldSatisfy` either (const False) (const True)
            map requestItems <$> readIORef requests
                `shouldReturn`
                    [ [ userTextItem "retained"
                      , userTextItem (summarizationPrompt focus)
                      ]
                    ]

        it "replaces stale generated copies from authoritative state" do
            let stale =
                    taskPlanContextText $
                        CurrentTaskPlan 8 $
                            TaskPlan Nothing
                                [TaskPlanItem "stale" TaskPlanPending]
                plan = TaskPlan
                    (Just "continue here")
                    [TaskPlanItem "current" TaskPlanInProgress]
                current = CurrentTaskPlan 1 plan
                outcome = CompactOutcome
                    { compactBeforeTokens = 20
                    , compactAfterTokens = 10
                    , compactHistory =
                        [userTextItem stale, userTextItem "retained"]
                    , compactSummary = "summary"
                    }
            env <- newTaskPlanEnv Nothing Nothing
            replaceTaskPlan env plan `shouldReturn` Right current
            decorated <-
                decorateCompactOutcomeWithTaskPlan (Just env) outcome
            filter responseItemHasTaskPlan decorated.compactHistory
                `shouldSatisfy` \case
                    [MessageItem message] ->
                        message.role == RoleDeveloper
                            && responseMessageHasText
                                (taskPlanContextText current)
                                message
                    _ -> False

        it "does not reconstruct a plan from pre-compaction history" do
            let stale =
                    taskPlanContextText $
                        CurrentTaskPlan 8 $
                            TaskPlan Nothing
                                [TaskPlanItem "stale" TaskPlanInProgress]
                outcome = CompactOutcome
                    { compactBeforeTokens = 20
                    , compactAfterTokens = 10
                    , compactHistory =
                        [userTextItem stale, userTextItem "retained"]
                    , compactSummary = "summary"
                    }
            env <- newTaskPlanEnv Nothing Nothing
            decorated <-
                decorateCompactOutcomeWithTaskPlan (Just env) outcome
            decorated.compactHistory
                `shouldBe` [userTextItem "retained"]

        it "rejects a generated plan that cannot fit the compacted request" do
            let plan = TaskPlan Nothing
                    [TaskPlanItem "current" TaskPlanInProgress]
                outcome = CompactOutcome
                    { compactBeforeTokens = 20
                    , compactAfterTokens = 1
                    , compactHistory = []
                    , compactSummary = "summary"
                    }
                rawLimit =
                    estimateRequestTokensWithItems
                        defaultResponseCreateParams
                        outcome.compactHistory
            env <- newTaskPlanEnv Nothing Nothing
            _ <- replaceTaskPlan env plan
            result <-
                decorateCompactOutcomeWithTaskPlanWithin
                    rawLimit
                    defaultResponseCreateParams
                    (Just env)
                    outcome
            result `shouldSatisfy` \case
                Left message ->
                    "authoritative task plan does not fit"
                        `Text.isInfixOf` message
                Right _ -> False
