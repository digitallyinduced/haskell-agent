module Agent.CLI.NativeRuntimeSpec (spec) where

import Agent.CLI.NativeRuntime
    ( NativeDiscoveryContext(..)
    , NativeInteractionMode(..)
    , NativeSessionTarget(..)
    , NativeShellMode(..)
    , NativeTurnRequest(..)
    , NativeWorkspaceDiscovery(..)
    , nativeLoadsHostWorkspaceContext
    , nativePreparedDiscovery
    , nativeTurnOptions
    )
import Agent.CLI.Options
    ( CliOptions(..)
    , ScreenMode(..)
    )
import Agent.Provider (Provider(..))
import Agent.CLI.Project (defaultProjectSettings)
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
                , nativeTurnImages = []
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

    it "requires prepared discovery whenever host discovery is disabled" do
        let root = unsafeEncodeUtf "/prepared/root"
            prepared = NativeDiscoveryContext
                { nativeDiscoveryHome = root
                , nativeDiscoveryProjectRoot = root
                , nativeDiscoveryCatalogRoot = root
                , nativeDiscoveryProjectSettings = defaultProjectSettings
                , nativeDiscoveryGitBranch = ""
                , nativeDiscoveryOperatingSystem = "TestOS"
                , nativeDiscoveryShell = "/bin/test-shell"
                }
        nativeLoadsHostWorkspaceContext DiscoverHostWorkspace
            `shouldBe` True
        case nativePreparedDiscovery DiscoverHostWorkspace of
            Nothing -> pure ()
            Just _ ->
                expectationFailure
                    "host discovery unexpectedly carried prepared context"
        let discovery = UsePreparedWorkspace prepared
        nativeLoadsHostWorkspaceContext discovery `shouldBe` False
        (.nativeDiscoveryProjectRoot)
            <$> nativePreparedDiscovery discovery
            `shouldBe` Just root
        (.nativeDiscoveryHome)
            <$> nativePreparedDiscovery discovery
            `shouldBe` Just root
        (.nativeDiscoveryOperatingSystem)
            <$> nativePreparedDiscovery discovery
            `shouldBe` Just "TestOS"
        (.nativeDiscoveryShell)
            <$> nativePreparedDiscovery discovery
            `shouldBe` Just "/bin/test-shell"

baseRequest :: NativeTurnRequest
baseRequest = NativeTurnRequest
    { nativeTurnPrompt = "hello"
    , nativeTurnImages = []
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
