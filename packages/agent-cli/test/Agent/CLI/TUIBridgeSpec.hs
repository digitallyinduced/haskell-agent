module Agent.CLI.TUIBridgeSpec (spec) where

import Agent.CLI.AgentViewport
    ( AgentEntry(..)
    , AgentStep(..)
    , AgentStepState(..)
    , AgentTarget(..)
    )
import Agent.CLI.Command (ReplAction(ReplCopyPath))
import Agent.CLI.Dictation (DictationTarget(..))
import Agent.CLI.Interrupt (CtrlCDecision(..))
import Agent.CLI.TUI.App
    ( appEventLogicalBytes
    , emitUiEvent
    , enqueueAppEvent
    , loadSyntaxHighlighterForRuntime
    , newFullscreenInputBuffer
    , newFullscreenRuntime
    , newFullscreenRuntimeWithSyntaxLoader
    , setFullscreenSessionActions
    )
import Agent.CLI.TUI.Bridge
import Agent.CLI.TUI.ImagePreview (TuiImagePreview(..))
import Agent.CLI.TUI.Types
    ( AppEvent(..)
    , AppEventMailbox(..)
    , AppEventMailboxState(..)
    , FullscreenRuntime(..)
    , FullscreenSessionActions(..)
    , PendingAppEvent(..)
    , PendingUiEvent(..)
    , SyntaxHighlighterState(..)
    )
import Agent.TUI.Model
import Agent.TUI.Presentation
    ( TodoDisplayLine(..)
    , TodoDisplayStatus(..)
    )
import Agent.Loop (ImageAttachment(..), LoopEvent(..), emptyTurnOutput)
import Agent.Provider (Provider(XAIProvider))
import Agent.Subagents (SubagentId(..))
import Agent.ToolDispatch (ToolCall(..), functionToolCall)
import Agent.TUI.Motion (MotionMode(..))
import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.STM (readTVarIO)
import Control.Exception.Safe (throwString)
import Control.Monad (replicateM_)
import qualified Data.ByteString as BS
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

    it "coalesces keyed snapshots without crossing lifecycle barriers" do
        runtime <- newBridgeTestRuntime
        emitUiEvent runtime (UiLoop (ToolOutputUpdated "c1" "old"))
        emitUiEvent runtime (UiLoop (TextDelta "text"))
        emitUiEvent runtime (UiLoop (ToolOutputUpdated "c1" "latest"))
        emitUiEvent runtime (UiLoop TurnStarted)
        emitUiEvent runtime (UiLoop (ToolOutputUpdated "c1" "next"))
        emitUiEvent runtime (UiLoop (ToolOutputUpdated "c1" "newest"))
        let AppEventMailbox stateRef = runtime.runtimeMailbox
        pending <- (.mailboxPendingEvents) <$> readTVarIO stateRef
        [ output
            | PendingUi
                (PendingExactUi
                    (UiLoop (ToolOutputUpdated "c1" output))) <-
                toList pending
            ]
            `shouldBe` ["latest", "newest"]
        let newestFollowsBoundary =
                case dropWhile (not . isTurnStarted) (toList pending) of
                    PendingUi (PendingExactUi (UiLoop TurnStarted)) : rest ->
                        any isNewestOutput rest
                    _ -> False
        newestFollowsBoundary `shouldBe` True

    it "coalesces live tool arguments and lets the canonical call replace them" do
        runtime <- newBridgeTestRuntime
        let call arguments =
                functionToolCall "c1" "apply_patch" arguments
        emitUiEvent runtime
            (UiLoop (ToolArgumentsUpdated (call "old")))
        emitUiEvent runtime (UiLoop (TextDelta "text"))
        emitUiEvent runtime
            (UiLoop (ToolArgumentsUpdated (call "latest")))
        emitUiEvent runtime (UiLoop TurnStarted)
        emitUiEvent runtime
            (UiLoop (ToolArgumentsUpdated (call "next")))
        emitUiEvent runtime
            (UiLoop (ToolUpdated (call "canonical")))
        let AppEventMailbox stateRef = runtime.runtimeMailbox
        pending <- (.mailboxPendingEvents) <$> readTVarIO stateRef
        [ arguments
            | PendingUi
                (PendingExactUi
                    (UiLoop (ToolArgumentsUpdated toolCall))) <-
                toList pending
            , let arguments = toolCall.arguments
            ]
            `shouldBe` ["latest"]
        [ arguments
            | PendingUi
                (PendingExactUi (UiLoop (ToolUpdated toolCall))) <-
                toList pending
            , let arguments = toolCall.arguments
            ]
            `shouldBe` ["canonical"]

    it "backpressures a single streaming mailbox node by payload bytes" do
        runtime <- newBridgeTestRuntime
        let exactBudgetText =
                Text.replicate
                    ((16 * 1024 * 1024 - 64) `div` 4)
                    "x"
        emitUiEvent runtime (UiLoop (TextDelta exactBudgetText))
        withAsync
            (emitUiEvent runtime (UiLoop (TextDelta "blocked")))
            \publishing -> do
                timeout 100000 (wait publishing)
                    `shouldReturn` Nothing
                let AppEventMailbox stateRef = runtime.runtimeMailbox
                state <- readTVarIO stateRef
                state.mailboxPendingBytes
                    `shouldBe` 16 * 1024 * 1024
                state.mailboxHighWaterCount `shouldBe` 1

    it "accounts and backpressures tool lifecycle payloads" do
        runtime <- newBridgeTestRuntime
        let arguments = Text.replicate (3 * 1024 * 1024) "x"
            started callId =
                UiLoop
                    (ToolStarted
                        (functionToolCall callId "shell_command" arguments))
        appEventLogicalBytes (AppUi (started "first"))
            `shouldSatisfy` (> 8 * 1024 * 1024)
        emitUiEvent runtime (started "first")
        withAsync (emitUiEvent runtime (started "second")) \publishing -> do
            timeout 100000 (wait publishing)
                `shouldReturn` Nothing
            let AppEventMailbox stateRef = runtime.runtimeMailbox
            state <- readTVarIO stateRef
            state.mailboxPendingCount `shouldBe` 1

    it "accounts retained conversation state in agent snapshots" do
        let body = Text.replicate (1024 * 1024) "x"
            conversation =
                reduceUi (UiAssistantHistory body) initialUiState
            entry = AgentEntry
                { agentTarget = AgentRoot
                , agentPath = "/root"
                , agentStatus = "active"
                , agentModel = Nothing
                , agentSteps = []
                , agentTranscript = []
                , agentConversation = conversation
                }
        appEventLogicalBytes (AppAgentSnapshot AgentRoot [entry])
            `shouldSatisfy` (>= 4 * 1024 * 1024)

    it "accounts provider-controlled snapshot target identifiers" do
        let target = AgentNative (Text.replicate (1024 * 1024) "x")
        appEventLogicalBytes (AppAgentSnapshot target [])
            `shouldSatisfy` (>= 4 * 1024 * 1024)

    it "accounts structural overhead in agent steps and todo rows" do
        let entry conversation steps =
                AgentEntry
                    { agentTarget = AgentRoot
                    , agentPath = "/root"
                    , agentStatus = "active"
                    , agentModel = Nothing
                    , agentSteps = steps
                    , agentTranscript = []
                    , agentConversation = conversation
                    }
            baseline =
                appEventLogicalBytes $
                    AppAgentSnapshot
                        AgentRoot
                        [entry initialUiState []]
            withStep =
                appEventLogicalBytes $
                    AppAgentSnapshot
                        AgentRoot
                        [ entry
                            initialUiState
                            [AgentStep AgentStepInfo "" Nothing]
                        ]
            withTodo =
                appEventLogicalBytes $
                    AppAgentSnapshot
                        AgentRoot
                        [ entry
                            initialUiState
                                { uiTodos =
                                    [ TodoDisplayLine
                                        TodoDisplayPending
                                        ""
                                    ]
                                }
                            []
                        ]
        withStep - baseline `shouldSatisfy` (>= 128)
        withTodo - baseline `shouldSatisfy` (>= 128)

    it "admits one indivisible event larger than the mailbox byte budget" do
        runtime <- newBridgeTestRuntime
        let oversizedOutput =
                Text.replicate ((16 * 1024 * 1024 `div` 4) + 1) "x"
        timeout 2000000
            (emitUiEvent
                runtime
                (UiLoop (ToolOutputUpdated "oversized" oversizedOutput)))
            `shouldReturn` Just ()
        -- Replacing that sole keyed snapshot must not compare the new
        -- oversized value against the ordinary byte budget and deadlock.
        timeout 2000000
            (emitUiEvent
                runtime
                (UiLoop (ToolOutputUpdated "oversized" oversizedOutput)))
            `shouldReturn` Just ()
        let AppEventMailbox stateRef = runtime.runtimeMailbox
        state <- readTVarIO stateRef
        state.mailboxPendingCount `shouldBe` 1

    it "accounts and backpressures encoded image events by payload bytes" do
        runtime <- newBridgeTestRuntime
        let encoded = BS.replicate (10 * 1024 * 1024) 0
            attachment = ImageAttachment
                { imageMime = "image/png"
                , imageBytes = encoded
                }
            preview = TuiImagePreview
                { previewMime = "image/png"
                , previewBytes = BS.length encoded
                , previewSourceWidth = 1
                , previewSourceHeight = 1
                , previewSample =
                    error "mailbox accounting forced the lazy preview sample"
                , previewKittyAttachment = attachment
                }
        appEventLogicalBytes (AppSetImagePreviews [(attachment, preview)])
            `shouldSatisfy` (> BS.length encoded)
        appEventLogicalBytes (AppCommitImagePreviews [(attachment, preview)])
            `shouldSatisfy` (> BS.length encoded)
        enqueueAppEvent runtime (AppToolImage "first" preview)
        withAsync
            (enqueueAppEvent runtime (AppToolImage "second" preview))
            \publishing -> do
                timeout 100000 (wait publishing)
                    `shouldReturn` Nothing
                let AppEventMailbox stateRef = runtime.runtimeMailbox
                state <- readTVarIO stateRef
                state.mailboxPendingBytes
                    `shouldSatisfy` (>= BS.length encoded)

    it "rebinds provider actions and forwards steering paste provenance" do
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
            (Just (DirectDictation XAIProvider))
            (modifyIORef' calls (<> ["new cancel"]))
            (\pasted _ -> do
                modifyIORef' calls (<> ["new steer"])
                pasted `shouldBe` True
                pure (Right ()))
            (const (modifyIORef' calls (<> ["new btw"])))
            (const (modifyIORef' calls (<> ["new immediate command"])))
            (modifyIORef' calls (<> ["new recap"]))
            (const (modifyIORef' calls (<> ["new effort"])))
            (pure SoftCancel)
            (pure (AgentRoot, []))
            (const (modifyIORef' calls (<> ["new agent"])))
        runtime.runtimeCancel
        _ <- runtime.runtimeSteer True "guidance"
        runtime.runtimeBtw "question"
        runtime.runtimeImmediateCommand ReplCopyPath
        runtime.runtimeRecap
        runtime.runtimeRestartEffort "high"
        runtime.runtimeAgentSelect AgentRoot
        decision <- runtime.runtimeCtrlC
        actions <- readIORef runtime.runtimeSessionActions
        case actions.sessionDictationTarget of
            Just (DirectDictation provider) ->
                provider `shouldBe` XAIProvider
            _ ->
                expectationFailure
                    "expected direct xAI dictation target"
        readIORef calls `shouldReturn`
            [ "old cancel"
            , "new cancel"
            , "new steer"
            , "new btw"
            , "new immediate command"
            , "new recap"
            , "new effort"
            , "new agent"
            ]
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
    pending <- (.mailboxPendingEvents) <$> readTVarIO pendingRef
    syntaxState <- readIORef runtime.runtimeSyntaxHighlighter
    pure case (toList pending, syntaxState) of
        ( [PendingEvent AppSyntaxHighlighterChanged]
            , SyntaxHighlighterActive _ Nothing
            ) -> True
        _ -> False

newBridgeTestRuntime :: IO FullscreenRuntime
newBridgeTestRuntime = do
    input <- newFullscreenInputBuffer
    newFullscreenRuntime
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

isTurnStarted :: PendingAppEvent -> Bool
isTurnStarted = \case
    PendingUi (PendingExactUi (UiLoop TurnStarted)) -> True
    _ -> False

isNewestOutput :: PendingAppEvent -> Bool
isNewestOutput = \case
    PendingUi
        (PendingExactUi
            (UiLoop (ToolOutputUpdated "c1" "newest"))) -> True
    _ -> False
