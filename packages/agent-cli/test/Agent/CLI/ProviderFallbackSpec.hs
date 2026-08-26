module Agent.CLI.ProviderFallbackSpec (spec) where

import Agent.CLI.ModelConfig
    ( ModelCatalog
    , decodeModelConfig
    , packagedModelCatalogPath
    )
import Agent.CLI.Models (ModelOption(..), ModelTarget(..))
import Agent.CLI.ProviderFallback
    ( allowsAutomaticBillingFallback
    , automaticCooldownRetryDelay
    , automaticRetryCountdownText
    , fallbackCandidates
    , ProviderRecoveryPreference(..)
    , providerRecoveryPreference
    , rankedModels
    )
import Agent.Error
    ( ApiError(..)
    , ErrorType(..)
    , credentialsExhausted
    )
import Agent.Provider (BillingMode(..), Provider(..))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), addUTCTime)
import Test.Hspec

spec :: Spec
spec = do
    catalog <- runIO readPackagedCatalog
    describe "allowsAutomaticBillingFallback" do
        it "blocks subscription-to-API-credit fallback" do
            allowsAutomaticBillingFallback
                SubscriptionBilled ApiBilled
                `shouldBe` False

        it "allows fallback between subscription accounts" do
            allowsAutomaticBillingFallback
                SubscriptionBilled SubscriptionBilled
                `shouldBe` True

        it "does not restrict a session already using API credits" do
            allowsAutomaticBillingFallback
                ApiBilled ApiBilled
                `shouldBe` True

    describe "rankedModels" do
        it "puts the frontier OpenAI model first" do
            fmap
                (\model ->
                    ( model.modelTarget.targetProvider
                    , model.modelTarget.targetModelId
                    ))
                (safeHead (rankedModels catalog))
                `shouldBe` Just (OpenAIProvider, "gpt-5.6-sol")

    describe "fallbackCandidates" do
        let exhausted =
                credentialsExhausted
                    (UTCTime (fromGregorian 2026 8 20) 0)

        it "selects the best model for each other provider" do
            map
                (\model ->
                    ( model.modelTarget.targetProvider
                    , model.modelTarget.targetModelId
                    ))
                (fallbackCandidates catalog Set.empty XAIProvider exhausted)
                `shouldBe`
                    [ (OpenAIProvider, "gpt-5.6-sol")
                    , (OpenRouterProvider, "stealth/ox-alpha")
                    ]

        it "falls back from OpenAI to the best configured alternatives" do
            map
                (\model ->
                    ( model.modelTarget.targetProvider
                    , model.modelTarget.targetModelId
                    ))
                (fallbackCandidates catalog Set.empty OpenAIProvider exhausted)
                `shouldBe`
                    [ (XAIProvider, "grok-4.6")
                    , (OpenRouterProvider, "stealth/ox-alpha")
                    ]

        it "never automatically enters or leaves the Claude Code provider" do
            fallbackCandidates catalog Set.empty ClaudeCodeProvider exhausted
                `shouldBe` []
            map (.modelTarget.targetProvider)
                (fallbackCandidates catalog Set.empty OpenAIProvider exhausted)
                `shouldSatisfy` (ClaudeCodeProvider `notElem`)

        it "skips providers already found unavailable" do
            map (.modelTarget.targetProvider)
                (fallbackCandidates catalog (Set.singleton OpenAIProvider) XAIProvider exhausted)
                `shouldBe` [OpenRouterProvider]

        it "accepts direct usage-limit errors from every provider" do
            fallbackCandidates catalog Set.empty OpenRouterProvider
                (ProviderError UsageLimitReached "quota exhausted" (Just 3600))
                `shouldSatisfy` (not . null)

        it "accepts other definitive account and billing exhaustion errors" do
            map
                (not . null . fallbackCandidates catalog Set.empty XAIProvider)
                [ ProviderError UsageBalanceExhausted "balance exhausted" Nothing
                , ProviderError QuotaExceeded "quota exhausted" Nothing
                , ProviderError UsageNotIncluded "not included" Nothing
                , ProviderError BillingError "billing unavailable" Nothing
                ]
                `shouldBe` replicate 4 True

        it "does not switch for transient capacity failures" do
            fallbackCandidates catalog Set.empty XAIProvider
                (ProviderError OverloadedError "busy" (Just 30))
                `shouldBe` []

        it "can continue past a replacement provider with rejected auth" do
            map (.modelTarget.targetProvider)
                (fallbackCandidates catalog (Set.singleton XAIProvider) OpenAIProvider
                    (ProviderError AuthenticationError "rejected" Nothing))
                `shouldBe` [OpenRouterProvider]
            map (.modelTarget.targetProvider)
                (fallbackCandidates catalog (Set.singleton XAIProvider) OpenAIProvider
                    (CredentialError "credential file is invalid"))
                `shouldBe` [OpenRouterProvider]

    describe "automaticCooldownRetryDelay" do
        let now = UTCTime (fromGregorian 2026 8 21) 0

        it "waits through a brief credential cooldown" do
            automaticCooldownRetryDelay now
                (credentialsExhausted (addUTCTime 60 now))
                `shouldBe` Just 60

        it "retries immediately when the reset time has just passed" do
            automaticCooldownRetryDelay now
                (credentialsExhausted (addUTCTime (-1) now))
                `shouldBe` Just 0

        it "returns control for a long cooldown" do
            automaticCooldownRetryDelay now
                (credentialsExhausted (addUTCTime 121 now))
                `shouldBe` Nothing

        it "does not retry authentication failures as cooldowns" do
            automaticCooldownRetryDelay now
                (ProviderError AuthenticationError "expired" Nothing)
                `shouldBe` Nothing

    describe "providerRecoveryPreference" do
        let now = UTCTime (fromGregorian 2026 8 21) 0

        it "retries a transient all-account cooldown before provider fallback" do
            providerRecoveryPreference True now
                (credentialsExhausted (addUTCTime 60 now))
                `shouldBe` RetryCurrentProviderAfter 60

        it "falls back for a genuine long usage-window exhaustion" do
            providerRecoveryPreference True now
                (credentialsExhausted (addUTCTime 3600 now))
                `shouldBe` TryProviderFallback

        it "does not repeat the cooldown retry after its one retry allowance" do
            providerRecoveryPreference False now
                (credentialsExhausted (addUTCTime 60 now))
                `shouldBe` TryProviderFallback

    describe "automaticRetryCountdownText" do
        it "shows the remaining wait in seconds" do
            automaticRetryCountdownText 60
                `shouldBe`
                    "Provider temporarily unavailable; retrying automatically in 60s · Esc to cancel"

        it "never displays a negative countdown" do
            automaticRetryCountdownText (-1)
                `shouldSatisfy` Text.isInfixOf "in 0s"

safeHead :: [a] -> Maybe a
safeHead = \case
    [] -> Nothing
    value : _ -> Just value

readPackagedCatalog :: IO ModelCatalog
readPackagedCatalog = do
    path <- packagedModelCatalogPath
    bytes <- LBS.readFile path
    case decodeModelConfig "models.default.json" bytes of
        Left err -> fail (Text.unpack err)
        Right catalog -> pure catalog
