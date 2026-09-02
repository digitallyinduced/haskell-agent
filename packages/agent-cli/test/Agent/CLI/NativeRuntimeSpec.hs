module Agent.CLI.NativeRuntimeSpec (spec) where

import Agent.CLI.NativeRuntime
    ( NativeInteractionMode(..)
    , NativeSessionTarget(..)
    , NativeShellMode(..)
    , NativeTurnRequest(..)
    , nativeTurnOptions
    )
import Agent.CLI.Options
    ( CliOptions(..)
    , ScreenMode(..)
    )
import Agent.Provider (Provider(..))
import Agent.ReasoningEffort (ReasoningEffort(..))
import Agent.TUI.Motion (MotionMode(..))
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec

spec :: Spec
spec = describe "nativeTurnOptions" do
    it "lowers a new typed turn without enabling native-only capabilities" do
        let cwd = unsafeEncodeUtf "/tmp/project"
            request = NativeTurnRequest
                { nativeTurnPrompt = "fix the tests"
                , nativeTurnSession = NativeNewSession
                , nativeTurnProvider = Just OpenAIProvider
                , nativeTurnModel = Just "gpt-5"
                , nativeTurnCwd = cwd
                , nativeTurnEffort = Just EffortHigh
                , nativeTurnInteractionMode = NativeAsk
                , nativeTurnShellMode = NativeShellNone
                }
        options <- shouldReturnRight (nativeTurnOptions request)
        options.optPrompt `shouldBe` Just "fix the tests"
        options.optResume `shouldBe` Nothing
        options.optProvider `shouldBe` Just OpenAIProvider
        options.optModel `shouldBe` Just "gpt-5"
        options.optCwd `shouldBe` Just cwd
        options.optEffort `shouldBe` Just EffortHigh
        options.optSaveSession `shouldBe` True
        options.optNoYolo `shouldBe` True
        options.optYolo `shouldBe` False
        options.optWorktree `shouldBe` False
        options.optComputerUse `shouldBe` False
        options.optGhci `shouldBe` False
        options.optBash `shouldBe` False
        options.optScreenMode `shouldBe` ScreenMinimal
        options.optMotionMode `shouldBe` MotionOff

    it "maps resume and shell mode without parsing argv" do
        let request = baseRequest
                { nativeTurnSession = NativeResumeSession "session-123"
                , nativeTurnShellMode = NativeShellBoth
                }
        options <- shouldReturnRight (nativeTurnOptions request)
        options.optResume `shouldBe` Just "session-123"
        options.optGhci `shouldBe` True
        options.optBash `shouldBe` True

    it "rejects an empty resume id before runtime admission" do
        nativeTurnOptions
            (baseRequest
                { nativeTurnSession = NativeResumeSession "   " }
            )
            `shouldBe` Left "native resume session id must not be empty"

    it "does not expose auto-approval through typed turns" do
        nativeTurnOptions
            (baseRequest
                { nativeTurnInteractionMode = NativeYolo }
            )
            `shouldBe` Left "typed native turns do not support auto-approval"

baseRequest :: NativeTurnRequest
baseRequest = NativeTurnRequest
    { nativeTurnPrompt = "hello"
    , nativeTurnSession = NativeNewSession
    , nativeTurnProvider = Nothing
    , nativeTurnModel = Nothing
    , nativeTurnCwd = unsafeEncodeUtf "/tmp/project"
    , nativeTurnEffort = Nothing
    , nativeTurnInteractionMode = NativeAsk
    , nativeTurnShellMode = NativeShellNone
    }

shouldReturnRight :: (Show err) => Either err value -> IO value
shouldReturnRight = \case
    Left err -> expectationFailure ("expected Right, got Left " <> show err)
        >> error "unreachable"
    Right value -> pure value
