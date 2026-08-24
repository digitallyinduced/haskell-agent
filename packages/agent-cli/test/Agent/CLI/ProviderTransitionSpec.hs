module Agent.CLI.ProviderTransitionSpec (spec) where

import Agent.CLI.Models (ModelTarget(..))
import Agent.CLI.Options (CliOptions(..), defaultCliOptions, isOneShot)
import Agent.CLI.ProviderTransition
import Agent.Dialect (DialectId(..))
import Agent.Provider (BillingMode(..), Provider(..))
import Agent.Tools.PlanMode (PlanModeState(..))
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = do
    describe "applyProviderTransition" do
        it "preserves one-shot invocation mode without requiring persistence" do
            let options = defaultCliOptions
                    { optPrompt = Just "hello"
                    , optProvider = Just XAIProvider
                    }
                transitioned = applyProviderTransition options
                    (transition Nothing Nothing)
            isOneShot transitioned `shouldBe` True
            transitioned.optPrompt `shouldBe` Just "hello"
            transitioned.optResume `shouldBe` Nothing
            transitioned.optProvider `shouldBe` Just OpenAIProvider
            transitioned.optModel `shouldBe` Just "gpt-5.6-sol"

        it "uses a persisted session when one exists" do
            let transitioned = applyProviderTransition defaultCliOptions
                    (transition (Just "session-1") Nothing)
            transitioned.optResume `shouldBe` Just "session-1"

    describe "setPendingExitAfter" do
        it "preserves the plan state while changing exit behavior" do
            let pending = PendingTurn
                    { pendingPromptText = "make a plan"
                    , pendingInputs = []
                    , pendingExitAfter = False
                    , pendingPlanState = PlanActive
                    }
                updated = setPendingExitAfter True pending
            updated.pendingExitAfter `shouldBe` True
            updated.pendingPlanState `shouldBe` PlanActive

transition
    :: Maybe Text
    -> Maybe PendingTurn
    -> ProviderTransition
transition sessionId pending = ProviderTransition
    { transitionTarget = ModelTarget
        { targetProvider = OpenAIProvider
        , targetConnectionId = "openai"
        , targetModelId = "gpt-5.6-sol"
        , targetWireModelId = "gpt-5.6-sol"
        , targetDialect = CodexDialect
        }
    , transitionAccountSelectionId = Nothing
    , transitionAccountId = Nothing
    , transitionSessionId = sessionId
    , transitionPendingTurn = pending
    , transitionUnavailableProviders = [XAIProvider]
    , transitionCause = AutomaticFallback
    , transitionAutomaticBilling = Just SubscriptionBilled
    }
