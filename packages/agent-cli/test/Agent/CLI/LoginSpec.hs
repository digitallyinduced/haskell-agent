module Agent.CLI.LoginSpec (spec) where

import Agent.CLI.Login
import Agent.CLI.CredentialStore (ManagedAuthKind(..))
import Agent.CLI.Picker (PickerKey(..))
import Agent.Provider (Provider(..))
import Data.Either (isLeft)
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..))
import Test.Hspec

spec :: Spec
spec = do
    describe "applyLoginKey" do
        it "moves and wraps the selected credential" do
            let state = initialLoginState [openai, grok]
            applyLoginKey PickerKeyUp state
                `shouldBe` Right state { loginIndex = 1 }
            applyLoginKey PickerKeyDown state
                `shouldBe` Right state { loginIndex = 1 }

        it "refreshes the selected credential on enter or r" do
            let state = (initialLoginState [openai, grok]) { loginIndex = 1 }
            applyLoginKey PickerKeyConfirm state
                `shouldBe` Left (LoginRefresh 1)
            applyLoginKey (PickerKeyChar 'r') state
                `shouldBe` Left (LoginRefresh 1)

        it "opens add, toggle, and delete actions" do
            let state = initialLoginState [openai]
            applyLoginKey (PickerKeyChar 'a') state
                `shouldBe` Left LoginAdd
            applyLoginKey (PickerKeyChar 'e') state
                `shouldBe` Left (LoginToggle 0)
            applyLoginKey (PickerKeyChar 'd') state
                `shouldBe` Left (LoginDelete 0)
            applyLoginKey (PickerKeyChar 'i') state
                `shouldBe` Left (LoginImport 0)

        it "closes an empty dashboard instead of refreshing" do
            applyLoginKey PickerKeyConfirm (initialLoginState [])
                `shouldSatisfy` isLeft

    describe "renderLoginFrame" do
        it "shows providers, billing modes, sources, and usage" do
            let body = renderLoginFrame False $
                    initialLoginState
                        [ openai
                            { loginUsage =
                                UsageAvailable AccountUsage
                                    { usagePlan = Just "plus"
                                    , usageWindows =
                                        [ UsageWindow
                                            { windowName = "primary"
                                            , usedPercent = 31
                                            , windowSeconds = 18000
                                            , resetsAt = epoch
                                            }
                                        ]
                                    , creditsRemaining = Nothing
                                    , creditsUsed = Nothing
                                    }
                            }
                        , openRouter
                        ]
            body `shouldSatisfy` Text.isInfixOf "openai"
            body `shouldSatisfy` Text.isInfixOf "subscription"
            body `shouldSatisfy` Text.isInfixOf "31% used"
            body `shouldSatisfy` Text.isInfixOf "openrouter"
            body `shouldSatisfy` Text.isInfixOf "API credits"
            body `shouldSatisfy` Text.isInfixOf "environment"

        it "shows the OpenAI account email in the account label" do
            let body = renderLoginFrame False $
                    initialLoginState
                        [openai { loginLabel = "person@example.com" }]
            body `shouldSatisfy` Text.isInfixOf "person@example.com"

        it "shows the Grok account email in the account label" do
            let body = renderLoginFrame False $
                    initialLoginState
                        [grok { loginLabel = "person@example.com" }]
            body `shouldSatisfy` Text.isInfixOf "person@example.com"

        it "does not render access tokens" do
            renderLoginFrame False (initialLoginState [openai])
                `shouldSatisfy` (not . Text.isInfixOf "secret-openai")

        it "shows unchecked usage as an in-progress refresh" do
            renderLoginFrame False (initialLoginState [openai])
                `shouldSatisfy` Text.isInfixOf "checking usage"

        it "rounds OpenRouter credit amounts to cents" do
            formatCurrencyAmount 5.317721499 `shouldBe` "$5.32"
            formatCurrencyAmount 3369.682278501 `shouldBe` "$3369.68"
            let account = openRouter
                    { loginUsage =
                        UsageAvailable AccountUsage
                            { usagePlan = Nothing
                            , usageWindows = []
                            , creditsRemaining = Just "$5.32"
                            , creditsUsed = Just "$3369.68"
                            }
                    }
                body = renderLoginFrame False (initialLoginState [account])
            body `shouldSatisfy` Text.isInfixOf "credits remaining $5.32"
            body `shouldSatisfy` Text.isInfixOf "used $3369.68"

openai :: LoginAccount
openai = LoginAccount
    { loginManagedId = Nothing
    , loginProvider = OpenAIProvider
    , loginAccountId = "acc-openai"
    , loginLabel = "ChatGPT"
    , loginBilling = SubscriptionBilling Nothing
    , loginSource = "~/.codex/auth.json"
    , loginUsage = UsageNotChecked
    , loginAccessToken = "secret-openai"
    , loginAuthKind = ManagedBearerToken
    , loginSecretPayload = "secret-openai"
    , loginEnabled = True
    }

grok :: LoginAccount
grok = LoginAccount
    { loginManagedId = Nothing
    , loginProvider = XAIProvider
    , loginAccountId = "acc-grok"
    , loginLabel = "Grok"
    , loginBilling = SubscriptionBilling Nothing
    , loginSource = "~/.grok/auth.json"
    , loginUsage = UsageNotChecked
    , loginAccessToken = "secret-grok"
    , loginAuthKind = ManagedBearerToken
    , loginSecretPayload = "secret-grok"
    , loginEnabled = True
    }

openRouter :: LoginAccount
openRouter = LoginAccount
    { loginManagedId = Nothing
    , loginProvider = OpenRouterProvider
    , loginAccountId = "openrouter"
    , loginLabel = "OpenRouter"
    , loginBilling = ApiCreditsBilling
    , loginSource = "environment"
    , loginUsage = UsageNotChecked
    , loginAccessToken = "secret-openrouter"
    , loginAuthKind = ManagedBearerToken
    , loginSecretPayload = "secret-openrouter"
    , loginEnabled = True
    }

epoch :: UTCTime
epoch = UTCTime (fromGregorian 2026 1 1) 0
