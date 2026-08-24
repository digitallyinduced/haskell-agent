module Agent.CLI.AccountSelectionSpec (spec) where

import Agent.CLI.AccountSelection
import Agent.CLI.CredentialStore (ManagedAuthKind(..))
import Agent.CLI.Login
    ( AccountBilling(..)
    , AccountUsage(..)
    , LoginAccount(..)
    , UsageState(..)
    , UsageWindow(..)
    )
import Agent.Provider (BillingMode(..), Provider(..))
import Data.Text (Text)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Test.Hspec

spec :: Spec
spec = describe "account selection" do
    it "ranks provider-neutral candidates without changing equal-capacity order" do
        fmap (.candidateSelectionId)
            (selectCandidates Nothing
                [candidate "first" "account-1" (Just 50)
                , candidate "second" "account-2" (Just 50)
                ])
            `shouldBe` Just "first"

    it "ignores candidates with unverifiable or exhausted capacity" do
        fmap (.candidateSelectionId)
            (selectCandidates Nothing
                [candidate "unknown" "account-0" Nothing
                ,candidate "empty" "account-1" (Just 0)
                ,candidate "usable" "account-2" (Just 1)
                ])
            `shouldBe` Just "usable"

    it "prefers the remembered usable project account" do
        fmap (.selectedAccountId)
            (selectAccount
                (Just ("external:openai:second", "account-2"))
                [subscriptionAccount "first" "account-1" 10
                , subscriptionAccount "second" "account-2" 80
                ])
            `shouldBe` Just "account-2"

    it "uses the account with the most remaining capacity otherwise" do
        fmap (.selectedAccountId)
            (selectAccount
                (Just ("external:openai:exhausted", "account-0"))
                [subscriptionAccount "exhausted" "account-0" 100
                , subscriptionAccount "busy" "account-1" 80
                , subscriptionAccount "free" "account-2" 20
                ])
            `shouldBe` Just "account-2"

    it "excludes accounts whose usage could not be checked" do
        selectAccount Nothing
            [ (subscriptionAccount "unknown" "account-1" 20)
                { loginUsage = UsageUnavailable "offline" }
            ]
            `shouldBe` Nothing

    it "keeps discovery order for equal-capacity login accounts" do
        fmap (.selectedSelectionId)
            (selectAccount Nothing
                [ subscriptionAccount "z-source" "account-z" 50
                , subscriptionAccount "a-source" "account-a" 50
                ])
            `shouldBe` Just "external:openai:z-source"

    it "does not usage-rank Claude Code CLI authentication" do
        providerSupportsUsageAccountSelection ClaudeCodeProvider
            `shouldBe` False

    it "keeps a verified OpenRouter free-tier key usable at zero credits" do
        accountCapacity freeTierAccount `shouldBe` Just 1

    it "still rejects a paid OpenRouter key with zero remaining credits" do
        accountCapacity
            (freeTierAccount
                { loginUsage = UsageAvailable AccountUsage
                    { usagePlan = Nothing
                    , usageWindows = []
                    , creditsRemaining = Just "$0.0"
                    , creditsUsed = Just "$1.0"
                    }
                })
            `shouldBe` Nothing

candidate :: Text -> Text -> Maybe Double -> AccountCandidate
candidate selectionId accountId capacity = AccountCandidate
    { candidateProvider = OpenAIProvider
    , candidateSelectionId = selectionId
    , candidateAccountId = accountId
    , candidateBillingMode = SubscriptionBilled
    , candidateLabel = accountId
    , candidateCapacity = capacity
    }

subscriptionAccount
    :: Text
    -> Text
    -> Int
    -> LoginAccount
subscriptionAccount source accountId used = LoginAccount
    { loginManagedId = Nothing
    , loginProvider = OpenAIProvider
    , loginAccountId = accountId
    , loginLabel = accountId
    , loginBilling = SubscriptionBilling Nothing
    , loginSource = source
    , loginUsage = UsageAvailable AccountUsage
        { usagePlan = Nothing
        , usageWindows =
            [ UsageWindow
                { windowName = "primary"
                , usedPercent = used
                , windowSeconds = 3600
                , resetsAt = posixSecondsToUTCTime 0
                }
            ]
        , creditsRemaining = Nothing
        , creditsUsed = Nothing
        }
    , loginAccessToken = "redacted"
    , loginAuthKind = ManagedBearerToken
    , loginSecretPayload = "redacted"
    , loginEnabled = True
    }

freeTierAccount :: LoginAccount
freeTierAccount =
    (subscriptionAccount "openrouter-env" "openrouter-key" 0)
        { loginProvider = OpenRouterProvider
        , loginBilling = ApiCreditsBilling
        , loginUsage = UsageAvailable AccountUsage
            { usagePlan = Just "free tier"
            , usageWindows = []
            , creditsRemaining = Just "$0.0"
            , creditsUsed = Just "$0.0"
            }
        }
