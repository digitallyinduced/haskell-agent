module Agent.CLI.LoginSpec (spec) where

import Agent.CLI.Login
import Agent.CLI.CredentialStore (ManagedAuthKind(..))
import Agent.CLI.Picker (PickerKey(..))
import Agent.Provider (Provider(..))
import Data.Either (isLeft)
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), addUTCTime)
import qualified System.Timeout as Timeout
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

        it "opens provider, gateway, toggle, and delete actions" do
            let state = initialLoginState [openai]
            applyLoginKey (PickerKeyChar 'a') state
                `shouldBe` Left LoginAdd
            applyLoginKey (PickerKeyChar 'g') state
                `shouldBe` Left LoginGateway
            applyLoginKey (PickerKeyChar 'e') state
                `shouldBe` Left (LoginToggle 0)
            applyLoginKey (PickerKeyChar 'd') state
                `shouldBe` Left (LoginDelete 0)
            applyLoginKey (PickerKeyChar 'i') state
                `shouldBe` Left (LoginImport 0)

        it "closes an empty dashboard instead of refreshing" do
            applyLoginKey PickerKeyConfirm (initialLoginState [])
                `shouldSatisfy` isLeft

    describe "launchBrowserCommand" do
        it "does not wait for a foreground browser process to exit" do
            result <- Timeout.timeout 1_000_000
                (launchBrowserCommand "sleep" "2")
            result `shouldBe` Just True

    describe "gateway login flow selection" do
        it "uses browser OAuth only on a local macOS session" do
            selectGatewayLoginFlow "darwin" False
                `shouldBe` GatewayBrowserOAuth
            selectGatewayLoginFlow "darwin" True
                `shouldBe` GatewayDeviceFlow
            selectGatewayLoginFlow "linux" False
                `shouldBe` GatewayDeviceFlow

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

        it "shows gateway routing status instead of a usage spinner" do
            renderLoginFrame False (initialLoginState [gateway])
                `shouldSatisfy` Text.isInfixOf "gateway connected"

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

    describe "fullscreen login dashboard" do
        it "offers account connection before any credentials exist" do
            let rows = loginDashboardRows []
                labels = map fst rows
                rendered = Text.unlines
                    [label <> " " <> description | (label, description) <- rows]
            labels `shouldSatisfy` any (Text.isInfixOf "Connect account")
            labels `shouldSatisfy` any (Text.isInfixOf "platform gateway")
            labels `shouldSatisfy` not . any (Text.isInfixOf "Refresh")
            rendered `shouldSatisfy` Text.isInfixOf "Google Gemini"

        it "shows a saved gateway with reconnect and disconnect controls" do
            let dashboardLabels =
                    map fst (loginDashboardRows [gateway])
                actionLabels =
                    map fst (loginAccountActionRows gateway)
            dashboardLabels
                `shouldSatisfy` any (Text.isInfixOf "Reconnect platform gateway")
            dashboardLabels
                `shouldSatisfy` not . any (Text.isInfixOf "Refresh all")
            actionLabels
                `shouldSatisfy` any (Text.isInfixOf "Disconnect gateway")
            actionLabels
                `shouldSatisfy` not . any (Text.isInfixOf "Refresh usage")
            loginAccountDetail gateway
                `shouldSatisfy`
                    Text.isInfixOf
                        "Local accounts are unavailable until the gateway is disconnected."

        it "offers usage refresh and opens each discovered account" do
            let rows = loginDashboardRows [openai, openRouter]
                labels = map fst rows
                rendered = Text.unlines
                    [label <> " " <> description | (label, description) <- rows]
            labels `shouldSatisfy` any (Text.isInfixOf "Refresh all")
            rendered `shouldSatisfy` Text.isInfixOf "openai"
            rendered `shouldSatisfy` Text.isInfixOf "openrouter"
            rendered `shouldSatisfy` Text.isInfixOf "acc-openai"
            rendered `shouldSatisfy` not . Text.isInfixOf "secret-openai"
            rendered `shouldSatisfy` not . Text.isInfixOf "secret-openrouter"
            let hostileRows =
                    loginDashboardRows
                        [openai { loginLabel = "ChatGPT\ESC]0;unsafe" }]
                hostileRendered = Text.unlines
                    [label <> " " <> description
                    | (label, description) <- hostileRows
                    ]
            hostileRendered `shouldSatisfy` not . Text.any (== '\ESC')

        it "offers managed-account controls without an import action" do
            let managed = openai
                    { loginManagedId = Just "managed-openai"
                    , loginSource = "managed"
                    }
                labels = map fst (loginAccountActionRows managed)
            labels `shouldSatisfy` any (Text.isInfixOf "Refresh usage")
            labels `shouldSatisfy` any (Text.isInfixOf "Disable credential")
            labels `shouldSatisfy` any (Text.isInfixOf "Disconnect credential")
            labels `shouldSatisfy` not . any (Text.isInfixOf "Import")

        it "keeps external accounts read-only but importable" do
            let labels = map fst (loginAccountActionRows openai)
            labels `shouldSatisfy` any (Text.isInfixOf "Import credential")
            labels `shouldSatisfy` not . any (Text.isInfixOf "Disconnect")
            labels `shouldSatisfy` not . any (Text.isInfixOf "Disable")

        it "renders detailed usage without exposing credential secrets" do
            let detail = loginAccountDetail openai
                    { loginLabel = "person@example.com"
                    , loginUsage =
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
            detail `shouldSatisfy` Text.isInfixOf "person@example.com"
            detail `shouldSatisfy` Text.isInfixOf "31% used"
            detail `shouldSatisfy` Text.isInfixOf "Resets"
            detail `shouldSatisfy` not . Text.isInfixOf "secret-openai"

    describe "device authorization polling" do
        it "enforces the provider interval after a pending check" do
            let initial = initialDevicePollSchedule epoch 5 60
                pending = advanceDevicePollSchedule False epoch initial
            devicePollReadiness epoch initial
                `shouldBe` DevicePollReady
            devicePollReadiness epoch pending
                `shouldBe` DevicePollWait 5
            devicePollReadiness (addUTCTime 4 epoch) pending
                `shouldBe` DevicePollWait 1
            devicePollReadiness (addUTCTime 5 epoch) pending
                `shouldBe` DevicePollReady

        it "adds five seconds after slow_down and enforces expiry" do
            let initial = initialDevicePollSchedule epoch 5 60
                slowedDownOnce =
                    advanceDevicePollSchedule True epoch initial
                slowedDownTwice =
                    advanceDevicePollSchedule
                        True
                        (addUTCTime 10 epoch)
                        slowedDownOnce
            devicePollReadiness (addUTCTime 9 epoch) slowedDownOnce
                `shouldBe` DevicePollWait 1
            devicePollReadiness (addUTCTime 10 epoch) slowedDownOnce
                `shouldBe` DevicePollReady
            devicePollReadiness (addUTCTime 24 epoch) slowedDownTwice
                `shouldBe` DevicePollWait 1
            devicePollReadiness (addUTCTime 25 epoch) slowedDownTwice
                `shouldBe` DevicePollReady
            devicePollReadiness (addUTCTime 60 epoch) slowedDownTwice
                `shouldBe` DevicePollExpired

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

gateway :: LoginAccount
gateway = LoginAccount
    { loginManagedId = Nothing
    , loginProvider = OpenAIProvider
    , loginAccountId = "https://gateway.example"
    , loginLabel = "Gateway · https://gateway.example"
    , loginBilling = SubscriptionBilling Nothing
    , loginSource = "gateway"
    , loginUsage = UsageNotChecked
    , loginAccessToken = ""
    , loginAuthKind = ManagedBearerToken
    , loginSecretPayload = ""
    , loginEnabled = True
    }

epoch :: UTCTime
epoch = UTCTime (fromGregorian 2026 1 1) 0
