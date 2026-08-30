module Agent.CLI.TUIBridgeSpec (spec) where

import Agent.CLI.AgentViewport (AgentEntry(..), AgentTarget(..))
import Agent.CLI.Interrupt (CtrlCDecision(..))
import Agent.CLI.TUI.App
    ( emitUiEvent
    , loadSyntaxHighlighterForRuntime
    , newFullscreenInputBuffer
    , newFullscreenRuntime
    , newFullscreenRuntimeWithSyntaxLoader
    , setFullscreenSessionActions
    )
import Agent.CLI.TUI.Bridge
import Agent.CLI.TUI.Types
    ( AppEvent(..)
    , AppEventMailbox(..)
    , FullscreenRuntime(..)
    , FullscreenSessionActions(..)
    , PendingAppEvent(..)
    , SyntaxHighlighterState(..)
    )
import Agent.TUI.Model
import Agent.Loop (LoopEvent(..), emptyTurnOutput)
import Agent.Provider (Provider(XAIProvider))
import Agent.Subagents (SubagentId(..))
import Agent.ToolDispatch (functionToolCall)
import Agent.TUI.Motion (MotionMode(..))
import Control.Concurrent.STM (readTVarIO)
import Control.Exception.Safe (throwString)
import Control.Monad (replicateM_)
import Data.Foldable (toList)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Text as Text
import System.Timeout (timeout)
import qualified Graphics.Vty as V
import Test.Hspec

spec :: Spec
spec = describe "fullscreen TUI bridge" do
    it "follows retained output events but not draft-only events" do
        eventFollows (UiSystemMessage "copied") `shouldBe` True
        eventFollows (UiErrorMessage "failed") `shouldBe` True
        eventFollows
            (UiRetryCountdown "Provider unavailable.\n" 60000 ".")
            `shouldBe` True
        eventFollows (UiSetDraft "draft" 5) `shouldBe` False

    it "starts and clears native terminal progress" do
        let running = reduceUi (UiLoop TurnStarted) initialUiState
            toolCall =
                functionToolCall
                    "tool-1"
                    "run_terminal_cmd"
                    "{\"command\":\"sleep 1\"}"
            continuing =
                reduceUi
                    (UiLoop
                        (TurnFinished
                            (emptyTurnOutput "r1" [toolCall] Nothing)))
                    running
            finished =
                reduceUi
                    (UiLoop
                        (TurnFinished
                            (emptyTurnOutput "r2" [] Nothing)))
                    running
        nativeProgressSignal False (UiLoop TurnStarted) running
            `shouldBe` Just True
        nativeProgressSignal
            False
            (UiLoop (TurnFinished (emptyTurnOutput "r1" [] Nothing)))
            finished
            `shouldBe` Just False
        nativeProgressSignal
            False
            (UiLoop (TurnFinished (emptyTurnOutput "r1" [toolCall] Nothing)))
            continuing
            `shouldBe` Just True
        nativeProgressSignal False (UiTurnEnded BlockCancelled) running
            `shouldBe` Just False

    it "coalesces adjacent streaming updates without merging boundaries" do
        mergeUiEvents
            (UiLoop (TextDelta "hel"))
            (UiLoop (TextDelta "lo"))
            `shouldBe` Just (UiLoop (TextDelta "hello"))
        mergeUiEvents
            (UiLoop (ReasoningDelta "look "))
            (UiLoop (ReasoningDelta "here"))
            `shouldBe` Just (UiLoop (ReasoningDelta "look here"))
        mergeUiEvents
            (UiLoop (ActivityUpdated "connecting"))
            (UiLoop (ActivityUpdated "streaming"))
            `shouldBe` Just (UiLoop (ActivityUpdated "streaming"))
        mergeUiEvents
            (UiLoop (TextDelta "answer"))
            (UiLoop
                (ToolStarted
                    (functionToolCall "tool-1" "read_file" "{}")))
            `shouldBe` Nothing
        mergeUiEvents
            (UiLoop (ReasoningDelta "thought"))
            (UiLoop (TextDelta "answer"))
            `shouldBe` Nothing

    it "bounds the in-memory prompt history while preserving newest-first order" do
        let oversized =
                [ "prompt-" <> Text.pack (show index)
                | index <- [1 .. fullscreenHistoryLimit + 10]
                ]
            trimmed = trimHistory oversized
            pushed = pushHistory "latest" trimmed
        length trimmed `shouldBe` fullscreenHistoryLimit
        trimmed `shouldBe` take fullscreenHistoryLimit oversized
        length pushed `shouldBe` fullscreenHistoryLimit
        take 2 pushed `shouldBe` ["latest", "prompt-1"]

    it "does not block producers when Brick is not draining events" do
        input <- newFullscreenInputBuffer
        runtime <- newFullscreenRuntime
            input
            (pure ())
            (const (pure ()))
            (pure WarnExit)
            (const (pure True))
            (const (pure ()))
            (const (pure ()))
            (pure (AgentRoot, []))
            (const (pure ()))
            (pure ())
            (const (throwString "syntax timing failed"))
            MotionFull
            False
            initialUiState
        completed <- timeout 2000000 $
            replicateM_ 2000 do
                emitUiEvent runtime (UiLoop (TextDelta "x"))
                emitUiEvent runtime (UiLoop TurnStarted)
        completed `shouldBe` Just ()

    it "rebinds provider-specific actions without replacing the runtime" do
        calls <- newIORef ([] :: [String])
        input <- newFullscreenInputBuffer
        runtime <- newFullscreenRuntime
            input
            (modifyIORef' calls (<> ["old cancel"]))
            (const (pure ()))
            (pure WarnExit)
            (const (pure True))
            (const (pure ()))
            (const (pure ()))
            (pure (AgentRoot, []))
            (const (pure ()))
            (pure ())
            (const (pure ()))
            MotionFull
            False
            initialUiState
        runtime.runtimeCancel
        setFullscreenSessionActions
            runtime
            (Just XAIProvider)
            (modifyIORef' calls (<> ["new cancel"]))
            (const (modifyIORef' calls (<> ["new steer"])))
            (const (modifyIORef' calls (<> ["new btw"])))
            (modifyIORef' calls (<> ["new recap"]))
            (const (modifyIORef' calls (<> ["new effort"])))
            (pure SoftCancel)
            (pure (AgentRoot, []))
            (const (modifyIORef' calls (<> ["new agent"])))
        runtime.runtimeCancel
        runtime.runtimeSteer "guidance"
        runtime.runtimeBtw "question"
        runtime.runtimeRecap
        runtime.runtimeRestartEffort "high"
        runtime.runtimeAgentSelect AgentRoot
        decision <- runtime.runtimeCtrlC
        actions <- readIORef runtime.runtimeSessionActions
        actions.sessionProvider `shouldBe` Just XAIProvider
        readIORef calls `shouldReturn`
            ["old cancel", "new cancel", "new steer", "new btw", "new recap", "new effort", "new agent"]
        decision `shouldBe` SoftCancel

    it "defers syntax loading until the runtime starts it" do
        input <- newFullscreenInputBuffer
        loaderCalled <- newIORef False
        durationRef <- newIORef Nothing
        runtime <- newFullscreenRuntimeWithSyntaxLoader
            (writeIORef loaderCalled True >> pure (Left "unavailable"))
            input
            (pure ())
            (const (pure ()))
            (pure WarnExit)
            (const (pure True))
            (const (pure ()))
            (const (pure ()))
            (pure (AgentRoot, []))
            (const (pure ()))
            (pure ())
            (writeIORef durationRef . Just)
            MotionFull
            False
            initialUiState
        readIORef loaderCalled `shouldReturn` False
        loadSyntaxHighlighterForRuntime runtime
        readIORef loaderCalled `shouldReturn` True
        readIORef durationRef >>= (`shouldSatisfy` maybe False (>= 0))
        hasPendingUnavailableSyntax runtime `shouldReturn` True

    it "contains unexpected syntax loader and timing failures" do
        input <- newFullscreenInputBuffer
        runtime <- newFullscreenRuntimeWithSyntaxLoader
            (throwString "syntax loader failed")
            input
            (pure ())
            (const (pure ()))
            (pure WarnExit)
            (const (pure True))
            (const (pure ()))
            (const (pure ()))
            (pure (AgentRoot, []))
            (const (pure ()))
            (pure ())
            (const (pure ()))
            MotionFull
            False
            initialUiState
        loadSyntaxHighlighterForRuntime runtime
        hasPendingUnavailableSyntax runtime `shouldReturn` True

    it "moves through history and restores the original draft" do
        historyMove 1 ["new", "old"] Nothing "draft" ""
            `shouldBe` ("new", Just 0, "draft")
        historyMove 1 ["new", "old"] (Just 0) "new" "draft"
            `shouldBe` ("old", Just 1, "draft")
        historyMove (-1) ["new", "old"] (Just 0) "new" "draft"
            `shouldBe` ("draft", Nothing, "draft")

    it "recognizes send-now keys without stealing ordinary Enter or Tab" do
        isSendNowKey (V.EvKey V.KEnter [V.MCtrl]) `shouldBe` True
        isSendNowKey (V.EvKey (V.KChar 'o') [V.MCtrl]) `shouldBe` True
        isSendNowKey (V.EvKey V.KEnter [V.MShift, V.MCtrl])
            `shouldBe` False
        isSendNowKey (V.EvKey V.KEnter []) `shouldBe` False
        isSendNowKey (V.EvKey (V.KChar '\t') []) `shouldBe` False

    it "falls back to root when the selected agent disappears" do
        let child = AgentChild (SubagentId "child")
            other = AgentChild (SubagentId "other")
            root =
                AgentEntry
                    { agentTarget = AgentRoot
                    , agentPath = "/root"
                    , agentStatus = "active"
                    , agentModel = Nothing
                    , agentSteps = []
                    , agentTranscript = []
                    , agentConversation = initialUiState
                    }
        normalizeAgentSelection child [root] `shouldBe` AgentRoot
        normalizeAgentSelection AgentRoot [root] `shouldBe` AgentRoot
        reconcileAgentSelection [AgentRoot] child
            `shouldBe` AgentRoot
        reconcileAgentSelection [AgentRoot] AgentRoot
            `shouldBe` AgentRoot
        reconcileAgentSelection [AgentRoot, other] other
            `shouldBe` other

hasPendingUnavailableSyntax :: FullscreenRuntime -> IO Bool
hasPendingUnavailableSyntax runtime = do
    let AppEventMailbox pendingRef = runtime.runtimeMailbox
    pending <- readTVarIO pendingRef
    syntaxState <- readIORef runtime.runtimeSyntaxHighlighter
    pure case (toList pending, syntaxState) of
        ( [PendingEvent AppSyntaxHighlighterChanged]
            , SyntaxHighlighterActive _ Nothing
            ) -> True
        _ -> False
