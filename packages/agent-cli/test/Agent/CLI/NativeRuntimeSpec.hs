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
    , applyNativeStartupPolicy
    )
import Agent.CLI.Options
    ( CliOptions(..)
    , ScreenMode(..)
    , defaultCliOptions
    )
import Agent.Runtime.StartupPolicy
import Control.Monad (forM_)
import Agent.Provider (Provider(..))
import Agent.Loop (ImageAttachment(..))
import Agent.CLI.Project (defaultProjectSettings)
import Agent.ReasoningEffort (ReasoningEffort(..))
import Agent.TUI.Motion (MotionMode(..))
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec

spec :: Spec
spec = describe "nativeTurnOptions" do
    it "preserves legacy host startup options exactly" do
        applyNativeStartupPolicy hostNativeStartupPolicy
            (unsafeEncodeUtf "/admitted") conflictingOptions
            `shouldBe` conflictingOptions

    it "enforces every restricted startup setting against conflicting options" do
        let cwd = unsafeEncodeUtf "/admitted"
            actual = applyNativeStartupPolicy restrictedNativeStartupPolicy cwd
                conflictingOptions
            expected = conflictingOptions
                { optCwd = Just cwd
                , optWorktree = False
                , optYolo = False
                , optNoYolo = True
                , optPromptFile = Nothing
                , optManagedTurnFile = Nothing
                , optAgentsMd = False
                , optSkills = False
                , optComputerUse = False
                , optCodeMode = False
                }
        actual `shouldBe` expected
        applyNativeStartupPolicy restrictedNativeStartupPolicy cwd actual
            `shouldBe` actual

    it "can exclude workspace context without changing execution facilities" do
        let policy = hostNativeStartupPolicy
                { nativeContextSources = SuppliedContextOnly }
        applyNativeStartupPolicy policy (unsafeEncodeUtf "/admitted") conflictingOptions
            `shouldBe` conflictingOptions { optAgentsMd = False, optSkills = False }

    it "does not re-enable caller-disabled context under host policy" do
        let options = defaultCliOptions { optAgentsMd = False, optSkills = False }
        applyNativeStartupPolicy hostNativeStartupPolicy (unsafeEncodeUtf "/admitted") options
            `shouldBe` options

    it "accepts image-only typed requests without introducing startup input files" do
        let request = baseRequest
                { nativeTurnPrompt = ""
                , nativeTurnImages = [ImageAttachment "image/png" "image bytes"]
                }
        lowered <- shouldReturnRight (nativeTurnOptions request)
        forM_ [hostNativeStartupPolicy, restrictedNativeStartupPolicy] \policy -> do
            let options = applyNativeStartupPolicy policy request.nativeTurnCwd lowered
            options.optPrompt `shouldBe` Just ""
            options.optPromptFile `shouldBe` Nothing
            options.optManagedTurnFile `shouldBe` Nothing

    forM_ [hostNativeStartupPolicy, restrictedNativeStartupPolicy] \policy ->
        forM_ [NativeNewSession, NativeResumeSession "existing"] \session ->
            forM_ [NativeAsk, NativePlan, NativeYolo] \interaction ->
                forM_ [NativeShellNone, NativeShellGhci, NativeShellBash, NativeShellBoth] \shell ->
                    it ("preserves typed turn fields under " <> show (policy, session, interaction, shell)) do
                        let request = baseRequest
                                { nativeTurnSession = session
                                , nativeTurnInteractionMode = interaction
                                , nativeTurnShellMode = shell
                                }
                        lowered <- shouldReturnRight (nativeTurnOptions request)
                        let options = applyNativeStartupPolicy policy request.nativeTurnCwd lowered
                        options.optPrompt `shouldBe` lowered.optPrompt
                        options.optResume `shouldBe` lowered.optResume
                        options.optGhci `shouldBe` lowered.optGhci
                        options.optBash `shouldBe` lowered.optBash
                        options.optCwd `shouldBe` Just request.nativeTurnCwd
                        options.optYolo `shouldBe` False
                        options.optNoYolo `shouldBe` True
                        options.optWorktree `shouldBe` False
                        options.optPromptFile `shouldBe` Nothing
                        options.optManagedTurnFile `shouldBe` Nothing
                        options.optComputerUse `shouldBe` False

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

    it "accepts explicit auto-approval without leaking it into CLI flags" do
        options <- shouldReturnRight $
            nativeTurnOptions
                (baseRequest
                    { nativeTurnInteractionMode = NativeYolo }
                )
        options.optYolo `shouldBe` False
        options.optNoYolo `shouldBe` True

    it "rejects an invalid resume in auto-approval mode under either policy" do
        forM_ [hostNativeStartupPolicy, restrictedNativeStartupPolicy] \policy -> do
            let request = baseRequest
                    { nativeTurnInteractionMode = NativeYolo
                    , nativeTurnSession = NativeResumeSession " "
                    }
            (applyNativeStartupPolicy policy request.nativeTurnCwd <$> nativeTurnOptions request)
                `shouldBe` Left "native resume session id must not be empty"

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

conflictingOptions :: CliOptions
conflictingOptions = defaultCliOptions
    { optCwd = Just (unsafeEncodeUtf "/untrusted")
    , optWorktree = True
    , optYolo = True
    , optNoYolo = False
    , optPromptFile = Just (unsafeEncodeUtf "/untrusted/prompt")
    , optManagedTurnFile = Just (unsafeEncodeUtf "/untrusted/turn")
    , optAgentsMd = True
    , optSkills = True
    , optComputerUse = True
    , optCodeMode = True
    , optPrompt = Just "preserve supplied input"
    , optResume = Just "preserve-session"
    }

shouldReturnRight :: (Show err) => Either err value -> IO value
shouldReturnRight = \case
    Left err -> expectationFailure ("expected Right, got Left " <> show err)
        >> error "unreachable"
    Right value -> pure value
