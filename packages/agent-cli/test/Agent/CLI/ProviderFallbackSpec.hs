module Agent.CLI.ProviderFallbackSpec (spec) where

import Agent.Runtime.ModelConfig
    ( ModelCatalog
    , decodeModelConfig
    , packagedModelCatalogPath
    )
import Agent.CLI.AccountSelection (SelectedAccount(..))
import Agent.Runtime.Models (ModelOption(..), ModelTarget(..), rawModelOption)
import Agent.CLI.ProviderFallback
    ( allowsAutomaticBillingFallback
    , automaticCooldownRetryDelay
    , automaticRetryCountdownText
    , fallbackCandidates
    , ProviderRecoveryPreference(..)
    , providerRecoveryPreference
    , rankedModels
    , selectAutomaticProviderCandidateWith
    )
import Agent.Dialect (DialectId(..))
import Agent.Error
    ( ApiError(..)
    , ErrorType(..)
    , credentialsExhausted
    )
import Agent.Provider (BillingMode(..), Provider(..))
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), addUTCTime)
import Test.Hspec

spec :: Spec
spec = do
    catalog <- runIO readPackagedCatalog
    describe "selectAutomaticProviderCandidateWith" do
        let openai = rawModelOption OpenAIProvider "gpt-6-astra"
            grok = rawModelOption XAIProvider "grok-4.6"
            gemini = rawModelOption GeminiProvider "gemini-3.7-flash"
            ignoreSkipped _ _ = pure ()

        it "does no resolution, validation, or reporting for no candidates" do
            let unexpected = fail "unexpected candidate effect"
            selectAutomaticProviderCandidateWith
                (const unexpected) (const (unexpected :: IO (Either Text.Text ())))
                (\_ _ -> unexpected) OpenAIProvider Set.empty []
                `shouldReturn` Nothing

        it "resolves before validating and returns the selected account unchanged" do
            events <- newIORef ([] :: [Text.Text])
            let resolved = openai
                    { modelTarget = openai.modelTarget { targetDialect = GrokBuildDialect } }
                account = SelectedAccount OpenAIProvider "selection-id"
                    "account-id" SubscriptionBilled "selected account"
                resolve choice = do
                    choice `shouldBe` openai
                    modifyIORef' events (<> ["resolve"])
                    pure resolved
                validate choice = do
                    choice `shouldBe` resolved
                    modifyIORef' events (<> ["validate"])
                    pure (Right (Just account))
                unavailable = Set.singleton GeminiProvider
            selectAutomaticProviderCandidateWith resolve validate
                (\_ _ -> expectationFailure "successful candidate was skipped")
                OpenAIProvider unavailable [openai, grok]
                `shouldReturn` Just (resolved, Just account, unavailable)
            readIORef events `shouldReturn` ["resolve", "validate"]

        it "filters every remaining model of a rejected provider and retains prior failures" do
            events <- newIORef ([] :: [Text.Text])
            let resolve choice = do
                    modifyIORef' events (<> ["resolve " <> choice.modelTarget.targetModelId])
                    pure choice
                validate choice = do
                    modifyIORef' events (<> ["validate " <> choice.modelTarget.targetModelId])
                    pure $ if choice.modelTarget.targetProvider == XAIProvider
                        then Left "no usable account"
                        else Right (Nothing :: Maybe SelectedAccount)
                skipped provider err = do
                    provider `shouldBe` XAIProvider
                    err `shouldBe` "no usable account"
                    modifyIORef' events (<> ["skip"])
            selectAutomaticProviderCandidateWith resolve validate skipped
                OpenAIProvider (Set.singleton OpenRouterProvider)
                [grok, rawModelOption XAIProvider "another-grok", gemini, openai]
                `shouldReturn` Just
                    ( gemini, Nothing
                    , Set.fromList [OpenRouterProvider, XAIProvider, OpenAIProvider]
                    )
            readIORef events `shouldReturn`
                [ "resolve grok-4.6", "validate grok-4.6", "skip"
                , "resolve gemini-3.7-flash", "validate gemini-3.7-flash"
                ]

        it "keeps the current provider available after an earlier rejection" do
            let validate choice = pure $
                    if choice.modelTarget.targetProvider == XAIProvider
                        then Left "unavailable"
                        else Right ()
            selectAutomaticProviderCandidateWith pure validate ignoreSkipped
                OpenAIProvider Set.empty [grok, openai]
                `shouldReturn` Just (openai, (), Set.singleton XAIProvider)

        it "returns no selection after exhausting providers and reports each once" do
            skipped <- newIORef []
            selectAutomaticProviderCandidateWith pure
                (const (pure (Left "unavailable" :: Either Text.Text ())))
                (\provider err -> modifyIORef' skipped (<> [(provider, err)]))
                OpenAIProvider Set.empty
                [grok, gemini, rawModelOption XAIProvider "another-grok"]
                `shouldReturn` Nothing
            readIORef skipped `shouldReturn`
                [(XAIProvider, "unavailable"), (GeminiProvider, "unavailable")]

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
                `shouldBe` Just (OpenAIProvider, "gpt-6-astra")

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
                (fallbackCandidates catalog Set.empty XAIProvider
                    "grok-4.6" exhausted)
                `shouldBe`
                    [ (OpenAIProvider, "gpt-6-astra")
                    , (GeminiProvider, "gemini-3.7-flash")
                    , (OpenRouterProvider, "stealth/ox-alpha")
                    ]

        it "falls back from OpenAI to the best configured alternatives" do
            map
                (\model ->
                    ( model.modelTarget.targetProvider
                    , model.modelTarget.targetModelId
                    ))
                (fallbackCandidates catalog Set.empty OpenAIProvider
                    "gpt-5.6-sol" exhausted)
                `shouldBe`
                    [ (XAIProvider, "grok-4.6")
                    , (GeminiProvider, "gemini-3.7-flash")
                    , (OpenRouterProvider, "stealth/ox-alpha")
                    ]

        it "never automatically enters or leaves the Claude Code provider" do
            fallbackCandidates catalog Set.empty ClaudeCodeProvider
                "sonnet" exhausted
                `shouldBe` []
            map (.modelTarget.targetProvider)
                (fallbackCandidates catalog Set.empty OpenAIProvider
                    "gpt-5.6-sol" exhausted)
                `shouldSatisfy` (ClaudeCodeProvider `notElem`)

        it "skips providers already found unavailable" do
            map (.modelTarget.targetProvider)
                (fallbackCandidates catalog (Set.singleton OpenAIProvider)
                    XAIProvider "grok-4.6" exhausted)
                `shouldBe` [GeminiProvider, OpenRouterProvider]

        it "accepts direct usage-limit errors from every provider" do
            fallbackCandidates catalog Set.empty OpenRouterProvider
                "stealth/ox-alpha"
                (ProviderError UsageLimitReached "quota exhausted" (Just 3600))
                `shouldSatisfy` (not . null)
            fallbackCandidates catalog Set.empty GeminiProvider
                "gemini-3.7-flash"
                (ProviderError UsageLimitReached "quota exhausted" (Just 3600))
                `shouldSatisfy` (not . null)

        it "accepts other definitive account and billing exhaustion errors" do
            map
                (not . null . fallbackCandidates catalog Set.empty XAIProvider
                    "grok-4.6")
                [ ProviderError UsageBalanceExhausted "balance exhausted" Nothing
                , ProviderError QuotaExceeded "quota exhausted" Nothing
                , ProviderError UsageNotIncluded "not included" Nothing
                , ProviderError BillingError "billing unavailable" Nothing
                ]
                `shouldBe` replicate 4 True

        it "does not switch for transient capacity failures" do
            fallbackCandidates catalog Set.empty XAIProvider "grok-4.6"
                (ProviderError OverloadedError "busy" (Just 30))
                `shouldBe` []

        it "can continue past a replacement provider with rejected auth" do
            map (.modelTarget.targetProvider)
                (fallbackCandidates catalog (Set.singleton XAIProvider)
                    OpenAIProvider "gpt-5.6-sol"
                    (ProviderError AuthenticationError "rejected" Nothing))
                `shouldBe` [GeminiProvider, OpenRouterProvider]
            map (.modelTarget.targetProvider)
                (fallbackCandidates catalog (Set.singleton XAIProvider)
                    OpenAIProvider "gpt-5.6-sol"
                    (CredentialError "credential file is invalid"))
                `shouldBe` [GeminiProvider, OpenRouterProvider]

        it "steps down through same-provider models on model access errors" do
            map (.modelTarget.targetModelId)
                (fallbackCandidates catalog Set.empty OpenAIProvider
                    "gpt-6-astra"
                    (ProviderError PermissionError "not available" Nothing))
                `shouldBe`
                    [ "gpt-5.6-sol"
                    , "gpt-5.6-terra"
                    , "gpt-5.6-luna"
                    , "grok-4.6"
                    , "gemini-3.7-flash"
                    , "stealth/ox-alpha"
                    ]
            map (.modelTarget.targetModelId)
                (fallbackCandidates catalog Set.empty OpenAIProvider
                    "gpt-5.6-sol"
                    (ProviderError PermissionError "not available" Nothing))
                `shouldBe`
                    [ "gpt-5.6-terra"
                    , "gpt-5.6-luna"
                    , "grok-4.6"
                    , "gemini-3.7-flash"
                    , "stealth/ox-alpha"
                    ]
            map (.modelTarget.targetModelId)
                (fallbackCandidates catalog Set.empty OpenAIProvider
                    "gpt-5.6-terra"
                    (ProviderError UsageNotIncluded "not included" Nothing))
                `shouldBe`
                    [ "gpt-5.6-luna"
                    , "grok-4.6"
                    , "gemini-3.7-flash"
                    , "stealth/ox-alpha"
                    ]

        it "does not retry the same provider for untyped HTTP permission failures" do
            map (.modelTarget.targetProvider)
                (fallbackCandidates catalog Set.empty OpenAIProvider
                    "gpt-5.6-sol" (HttpError 403 "forbidden"))
                `shouldBe`
                    [XAIProvider, GeminiProvider, OpenRouterProvider]

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
