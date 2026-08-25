module Agent.CLI.TUIAppSpec (spec) where

import Agent.CLI.AgentViewport (AgentEntry(..), AgentTarget(..))
import Agent.CLI.Input (terminalTextWidth)
import Agent.CLI.Interrupt (CtrlCDecision(..))
import Agent.CLI.TUI.App
    ( applyStoredFullscreenWindowTitle
    , applyTextPromptEdit
    , advanceCompletionFlashes
    , agentEntryWindow
    , agentPaneEntryLimit
    , agentPaneVisible
    , completionFlashTransitions
    , conversationScrollbarRenderer
    , choiceRowColumns
    , choiceClosesOnUiTransition
    , elapsedMillisSince
    , externalUrlCommand
    , fullscreenBounds
    , fullscreenVtyConfig
    , fullscreenSurface
    , mergeConversationView
    , newFullscreenInputBuffer
    , newFullscreenRuntime
    , withTrackedVtyBuilder
    , wrapFullscreenKeyboardVty
    , motionDemandFor
    , motionDemandForTerminalFocus
    , motionModeForTerminalFocus
    , lambdaArtWidget
    , quickStartRows
    , quickStartVisible
    , nativeProgressKeepaliveDue
    , nextMotionSchedule
    , onboardingVisibleRowIndices
    , maskedSecretText
    , normalizeTextOverlayInsertion
    , repositoryHeaderText
    , resumeSearchCursorColumn
    , selectedAgentConversation
    , setFullscreenWindowTitle
    , syntaxLanguagesForBlocks
    , textOverlayDisplayText
    , turnCompletionRequiresRedraw
    , uiEventRestartsMotionSchedule
    )
import Agent.CLI.TUI.Types
    ( ChoiceOverlay(..)
    , ChoicePresentation(..)
    , FullscreenRuntime(..)
    , Name(..)
    , TerminalFocus(..)
    , TextInputMode(..)
    , TextOverlay(..)
    )
import Agent.CLI.Terminal
    ( kittyKeyboardDisambiguatePush
    , kittyKeyboardPop
    )
import Agent.Loop (LoopEvent(..), emptyTurnOutput)
import Brick
    ( VScrollbarRenderer(..)
    , Widget
    , hLimit
    , renderWidget
    , txt
    , vLimit
    )
import qualified Brick.Types as B
import Agent.Subagents (SubagentId(..))
import Agent.ToolDispatch
    ( ToolCallKind(..)
    , ToolCallResult(..)
    , functionToolCall
    )
import Agent.TUI.Model
import Agent.TUI.Presentation
    ( TodoDisplayLine(..)
    , TodoDisplayStatus(..)
    )
import Agent.TUI.Motion
import Control.Concurrent.STM (newTChanIO)
import qualified Data.ByteString as ByteString
import Data.Foldable (find, toList)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Output.Mock as VMock
import Test.Hspec

spec :: Spec
spec = do
    describe "on-demand syntax loading" do
        it "requests grammars used by fenced file paths" do
            let conversation =
                    reduceUi
                        (UiAssistantHistory
                            "```src/Agent/Syntax.hs\nmain = pure ()\n```\n\
                            \```python\nprint('hello')\n```")
                        initialUiState
            syntaxLanguagesForBlocks (toList conversation.uiBlocks)
                `shouldBe` Set.fromList ["haskell", "python"]

    describe "externalUrlCommand" do
        it "opens HTTP(S) URLs without passing through a shell" do
            let url = "https://github.com/digitallyinduced/haskell-agent"
            externalUrlCommand url
                `shouldSatisfy`
                    maybe False ((== [Text.unpack url]) . snd)

        it "rejects unsafe or malformed destinations" do
            externalUrlCommand "file:///tmp/report" `shouldBe` Nothing
            externalUrlCommand "javascript:alert(1)" `shouldBe` Nothing
            externalUrlCommand "https://example.com/a b" `shouldBe` Nothing
            externalUrlCommand "https://example.com/\nowned" `shouldBe` Nothing
            externalUrlCommand
                ("https://example.com/" <> Text.replicate 4096 "a")
                `shouldBe` Nothing

    describe "secret text overlay" do
        it "renders only fixed-width masking glyphs" do
            maskedSecretText "top-secret-123"
                `shouldBe` Text.replicate 14 "•"
            let overlay = TextOverlay
                    { textTitle = "Secret requested by agent"
                    , textBody = "Enter an API key"
                    , textDraft = "top-secret-123"
                    , textCursor = 14
                    , textInputMode = TextInputSecret
                    }
            textOverlayDisplayText overlay
                `shouldBe` Text.replicate 14 "•"
            textOverlayDisplayText overlay
                `shouldNotSatisfy` Text.isInfixOf "secret"

        it "preserves plain overlays and keeps secret pastes single-line" do
            let value = "first\nsecond\rthird"
            normalizeTextOverlayInsertion TextInputPlain value
                `shouldBe` value
            normalizeTextOverlayInsertion TextInputSecret value
                `shouldBe` "first"

    describe "text overlay grapheme editing" do
        it "moves across a ZWJ emoji as one visible glyph" do
            let emoji = Text.pack ['\x1f469', '\x200d', '\x1f4bb']
                overlay = textOverlay ("a" <> emoji <> "b") 4
                movedLeft =
                    applyTextPromptEdit
                        (V.EvKey V.KLeft [])
                        overlay
                movedRight =
                    movedLeft >>=
                        applyTextPromptEdit
                            (V.EvKey V.KRight [])
            (.textCursor) <$> movedLeft `shouldBe` Just 1
            (.textCursor) <$> movedRight `shouldBe` Just 4

        it "deletes a ZWJ emoji without exposing internal code points" do
            let emoji = Text.pack ['\x1f469', '\x200d', '\x1f4bb']
                beforeEmoji = textOverlay ("a" <> emoji <> "b") 1
                afterEmoji = textOverlay ("a" <> emoji <> "b") 4
            (fmap
                (\overlay -> (overlay.textDraft, overlay.textCursor))
                (applyTextPromptEdit
                    (V.EvKey V.KDel [])
                    beforeEmoji))
                `shouldBe` Just ("ab", 1)
            (fmap
                (\overlay -> (overlay.textDraft, overlay.textCursor))
                (applyTextPromptEdit
                    (V.EvKey V.KBS [])
                    afterEmoji))
                `shouldBe` Just ("ab", 1)

        it "normalizes stale interior cursors before editing" do
            let emoji = Text.pack ['\x1f469', '\x200d', '\x1f4bb']
                interior = textOverlay ("a" <> emoji <> "b") 3
            (fmap
                (\overlay -> (overlay.textDraft, overlay.textCursor))
                (applyTextPromptEdit
                    (V.EvKey V.KLeft [])
                    interior))
                `shouldBe` Just ("a" <> emoji <> "b", 0)
            (fmap
                (\overlay -> (overlay.textDraft, overlay.textCursor))
                (applyTextPromptEdit
                    (V.EvKey V.KDel [])
                    interior))
                `shouldBe` Just ("ab", 1)

        it "keeps the cursor after insertions that merge with following text" do
            let regionalU = '\x1f1fa'
                regionalS = Text.singleton '\x1f1f8'
                insertedFlag =
                    applyTextPromptEdit
                        (V.EvKey (V.KChar regionalU) [])
                        (textOverlay regionalS 0)
                typedAfter =
                    insertedFlag >>=
                        applyTextPromptEdit
                            (V.EvKey (V.KChar 'x') [])
            (fmap
                (\overlay -> (overlay.textDraft, overlay.textCursor))
                insertedFlag)
                `shouldBe` Just (Text.singleton regionalU <> regionalS, 2)
            (fmap
                (\overlay -> (overlay.textDraft, overlay.textCursor))
                typedAfter)
                `shouldBe` Just
                    (Text.singleton regionalU <> regionalS <> "x", 3)

    describe "choice overlay lifecycle" do
        it "closes a running-turn choice on success or cancellation" do
            let running =
                    reduceUi (UiLoop TurnStarted) initialUiState
                finished =
                    reduceUi
                        (UiLoop
                            (TurnFinished
                                (emptyTurnOutput
                                    "response-1"
                                    []
                                    Nothing)))
                        running
                cancelled =
                    reduceUi (UiTurnEnded BlockCancelled) running
            choiceClosesOnUiTransition
                running
                finished
                (choiceOverlay True)
                `shouldBe` True
            choiceClosesOnUiTransition
                running
                cancelled
                (choiceOverlay True)
                `shouldBe` True

        it "preserves ordinary choices and continuing tool rounds" do
            let running =
                    reduceUi (UiLoop TurnStarted) initialUiState
                call =
                    functionToolCall
                        "tool-1"
                        "shell_command"
                        "{\"command\":\"true\"}"
                continuing =
                    reduceUi
                        (UiLoop
                            (TurnFinished
                                (emptyTurnOutput
                                    "response-1"
                                    [call]
                                    Nothing)))
                        running
            choiceClosesOnUiTransition
                running
                (reduceUi (UiTurnEnded BlockCancelled) running)
                (choiceOverlay False)
                `shouldBe` False
            choiceClosesOnUiTransition
                running
                continuing
                (choiceOverlay True)
                `shouldBe` False

    describe "prompt model refresh" do
        it "preserves the live draft and cursor across a provider restart" do
            let before =
                    reduceUi
                        (UiSetDraft "half typed prompt" 7)
                        initialUiState
                after =
                    reduceUi
                        (UiSetPrompt
                            before.uiPrompt
                                { promptModel = "gpt-5.6-sol"
                                })
                        before
            after.uiDraft `shouldBe` "half typed prompt"
            after.uiCursor `shouldBe` 7
            after.uiPrompt.promptModel `shouldBe` "gpt-5.6-sol"

    describe "fullscreenVtyConfig" do
        it "maps enhanced-keyboard sequences before Vty decodes them" do
            let mappings = V.configInputMap fullscreenVtyConfig
            mappings `shouldContain`
                [ ( Nothing
                  , "\ESC[27;2;13~"
                  , V.EvKey V.KEnter [V.MShift]
                  )
                , ( Nothing
                  , "\ESC[13;2u"
                  , V.EvKey V.KEnter [V.MShift]
                  )
                ]
            mapM_
                (\mapping -> mappings `shouldContain` [mapping])
                [ ( Nothing
                  , "\ESC[118;5u"
                  , V.EvKey (V.KChar 'v') [V.MCtrl]
                  )
                , ( Nothing
                  , "\ESC[99;5u"
                  , V.EvKey (V.KChar 'c') [V.MCtrl]
                  )
                , ( Nothing
                  , "\ESC[99:67:67;5:1u"
                  , V.EvKey (V.KChar 'c') [V.MCtrl]
                  )
                , ( Nothing
                  , "\ESC[118;9u"
                  , V.EvKey (V.KChar 'v') [V.MMeta]
                  )
                , ( Nothing
                  , "\ESC[118:86:86;9:1u"
                  , V.EvKey (V.KChar 'v') [V.MMeta]
                  )
                , ( Nothing
                  , "\ESC[114;5u"
                  , V.EvKey (V.KChar 'r') [V.MCtrl]
                  )
                , ( Nothing
                  , "\ESC[114:82:82;5:1u"
                  , V.EvKey (V.KChar 'r') [V.MCtrl]
                  )
                ]

    describe "fullscreen keyboard protocol lifecycle" do
        it "pushes Cmd+V reporting and pops it before Vty shutdown" do
            events <- newIORef
                ([] :: [Either ByteString.ByteString ()])
            (_, output) <- VMock.mockTerminal (80, 24)
            vty <- mockVty
                output
                    { V.outputByteBuffer =
                        \bytes ->
                            modifyIORef' events (<> [Left bytes])
                    }
                (modifyIORef' events (<> [Right ()]))
                ((Right () `elem`) <$> readIORef events)
            let push =
                    TextEncoding.encodeUtf8
                        kittyKeyboardDisambiguatePush
                pop = TextEncoding.encodeUtf8 kittyKeyboardPop
            wrapped <- wrapFullscreenKeyboardVty True vty
            readIORef events `shouldReturn` [Left push]

            V.shutdown wrapped
            readIORef events
                `shouldReturn` [Left push, Left pop, Right ()]

            V.shutdown wrapped
            readIORef events
                `shouldReturn` [Left push, Left pop, Right ()]

        it "leaves unsupported terminals in legacy keyboard mode" do
            events <- newIORef
                ([] :: [Either ByteString.ByteString ()])
            (_, output) <- VMock.mockTerminal (80, 24)
            vty <- mockVty
                output
                    { V.outputByteBuffer =
                        \bytes ->
                            modifyIORef' events (<> [Left bytes])
                    }
                (modifyIORef' events (<> [Right ()]))
                ((Right () `elem`) <$> readIORef events)
            wrapped <- wrapFullscreenKeyboardVty False vty
            readIORef events `shouldReturn` []

            V.shutdown wrapped
            readIORef events `shouldReturn` [Right ()]

    describe "fullscreen window title" do
        it "replays the stored session title through Vty output" do
            titles <- newIORef ([] :: [String])
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
                (const (pure ()))
                MotionFull
                False
                initialUiState
            (_, output) <- VMock.mockTerminal (80, 24)
            setFullscreenWindowTitle runtime "New session"
            readIORef runtime.runtimeWindowTitle
                `shouldReturn` Just "New session"
            applyStoredFullscreenWindowTitle
                runtime
                output
                    { V.setOutputWindowTitle =
                        \title -> modifyIORef' titles (<> [title])
                    }
            readIORef titles `shouldReturn` ["New session"]

    describe "fullscreen Vty ownership" do
        it "shuts down the rebuilt Vty when exit follows suspension" do
            shutdowns <- newIORef ([] :: [String])
            useInitial <- newIORef True
            (_, output) <- VMock.mockTerminal (80, 24)
            initialVty <- mockVty
                output
                (modifyIORef' shutdowns (<> ["initial"]))
                (pure False)
            resumedVty <- mockVty
                output
                (modifyIORef' shutdowns (<> ["resumed"]))
                (pure False)
            let makeVty = do
                    initial <- readIORef useInitial
                    writeIORef useInitial False
                    pure (if initial then initialVty else resumedVty)

            (withTrackedVtyBuilder makeVty \buildVty -> do
                first <- buildVty
                V.shutdown first
                _ <- buildVty
                ioError (userError "forced exit"))
                `shouldThrow` anyIOException

            readIORef shutdowns
                `shouldReturn` ["initial", "resumed"]

    describe "repositoryHeaderText" do
        it "puts the git state before the full checkout path" do
            repositoryHeaderText
                "detached"
                "~/digitallyinduced/haskell-agent"
                `shouldBe`
                    "detached  ~/digitallyinduced/haskell-agent"

        it "still renders a path when git state is unavailable" do
            repositoryHeaderText "" "~/scratch"
                `shouldBe` "~/scratch"

    describe "bounded custom rendering" do
        it "crops the empty-conversation art to tiny render contexts" do
            let image =
                    V.picImage $
                        renderWidget Nothing [lambdaArtWidget True 0] (5, 3)
            V.imageWidth image `shouldSatisfy` (<= 5)
            V.imageHeight image `shouldSatisfy` (<= 3)

        it "sweeps the empty-conversation sheen over time" do
            let rendered elapsed =
                    show $
                        renderWidget Nothing [lambdaArtWidget True elapsed] (42, 21)
            rendered 0 `shouldNotBe` rendered 400

        it "shows quick-start actions only when the empty pane has room" do
            quickStartVisible 100 30 `shouldBe` True
            quickStartVisible 47 30 `shouldBe` False
            quickStartVisible 100 19 `shouldBe` False

        it "surfaces the existing high-value startup commands" do
            quickStartRows
                `shouldBe`
                    [ (QuickStartWorktree, "New worktree", "/worktree")
                    , (QuickStartResume, "Resume session", "/resume")
                    , (QuickStartCommands, "Browse commands", "/")
                    , (QuickStartModel, "Manage models", "/model")
                    ]

        it "paints an exact terminal-sized backing surface" do
            let image =
                    V.picImage $
                        renderWidget
                            Nothing
                            [ ( fullscreenSurface $
                                    vLimit 1 $
                                        hLimit 20 $
                                            txt "content that exceeds the terminal"
                              ) :: Widget ()
                            ]
                            (8, 4)
            V.imageWidth image `shouldBe` 8
            V.imageHeight image `shouldBe` 4

        it "crops oversized overlay layers to the terminal" do
            let oversized :: Widget ()
                oversized =
                    B.Widget B.Greedy B.Greedy $
                        pure
                            B.emptyResult
                                { B.image =
                                    V.charFill V.defAttr 'x' (20 :: Int) 10
                                }
                image =
                    V.picImage $
                        renderWidget
                            Nothing
                            [fullscreenBounds oversized]
                            (8, 4)
            V.imageWidth image `shouldBe` 8
            V.imageHeight image `shouldBe` 4

    describe "resume search cursor" do
        it "uses terminal cells for wide and combining characters" do
            resumeSearchCursorColumn "search: " "漢"
                `shouldBe` 10
            resumeSearchCursorColumn "search: " "e\x0301"
                `shouldBe` 9

    describe "choice row layout" do
        it "keeps long labels and details separated inside the row width" do
            let (label, detail) =
                    choiceRowColumns
                        57
                        "  openrouter · stealth/ox-alpha · generic-responses"
                        "default · frontier · free · coding"
            terminalTextWidth label
                + 2
                + terminalTextWidth detail
                `shouldSatisfy` (<= 57)
            label `shouldSatisfy` Text.isSuffixOf "…"
            detail `shouldSatisfy` Text.isSuffixOf "…"

        it "preserves both columns when they already fit" do
            choiceRowColumns 40 "› model" "default"
                `shouldBe` ("› model", "default")

    describe "onboarding layout" do
        it "uses the complete 18-row surface when it fits" do
            onboardingVisibleRowIndices 18 0 3
                `shouldBe` [0 .. 17]

        it "keeps every setup path in a short terminal" do
            onboardingVisibleRowIndices 3 1 3
                `shouldBe` [8, 9, 10]

        it "keeps the selected setup path when only one row fits" do
            onboardingVisibleRowIndices 1 1 3
                `shouldBe` [9]

    describe "Agents pane layout" do
        it "hides below the responsive breakpoint and without children" do
            agentPaneVisible 71 20 [rootEntry, childEntry 1]
                `shouldBe` False
            agentPaneVisible 72 20 [rootEntry, childEntry 1]
                `shouldBe` True
            agentPaneVisible 120 9 [rootEntry, childEntry 1]
                `shouldBe` False
            agentPaneVisible 120 20 [rootEntry]
                `shouldBe` False

        it "centers the selected row and reports hidden rows on both sides" do
            let entries = rootEntry : map childEntry [1 .. 6]
                selected = AgentChild (SubagentId "agent-4")
                (above, shown, below) =
                    agentEntryWindow 3 selected entries
            above `shouldBe` 3
            map (.agentTarget) shown
                `shouldBe`
                    [ AgentChild (SubagentId "agent-3")
                    , selected
                    , AgentChild (SubagentId "agent-5")
                    ]
            below `shouldBe` 1

        it "reserves height for truncation indicators and pane chrome" do
            let availableHeight = 15
                entries = rootEntry : map childEntry [1 .. 20]
                selected = AgentChild (SubagentId "agent-10")
                entryLimit = agentPaneEntryLimit availableHeight
                (above, shown, below) =
                    agentEntryWindow entryLimit selected entries
                indicatorRows =
                    fromEnum (above > 0) + fromEnum (below > 0)
                renderedRows =
                    length shown + indicatorRows + 5
            entryLimit `shouldBe` 8
            renderedRows `shouldSatisfy` (<= availableHeight)

        it "uses a clicked child as the conversation view and root as the main view" do
            let child =
                    (childEntry 1)
                        { agentTranscript =
                            ["user: investigate", "assistant: finished"]
                        }
            selectedAgentConversation child.agentTarget [rootEntry, child]
                `shouldBe` Just child
            selectedAgentConversation AgentRoot [rootEntry, child]
                `shouldBe` Nothing

        it "preserves child block selection and expansion across snapshots" do
            let replay =
                    foldl
                        (flip reduceUi)
                        initialUiState
                        [ UiUserSubmitted "investigate"
                        , UiLoop TurnStarted
                        , UiLoop (ReasoningDelta "compare paths")
                        , UiAssistantHistory "done"
                        ]
            case find
                    ((== BlockThinking) . (.blockKind))
                    replay.uiBlocks of
                Nothing ->
                    expectationFailure "expected a reasoning block"
                Just reasoning -> do
                    let previous =
                            reduceUi
                                (UiActivateBlock reasoning.blockId)
                                replay
                        merged = mergeConversationView previous replay
                    merged.uiSelectedBlock
                        `shouldBe` Just reasoning.blockId
                    fmap (.blockExpanded)
                        (find
                            ((== reasoning.blockId) . (.blockId))
                            merged.uiBlocks)
                        `shouldBe` Just True
                    mergeConversationView previous initialUiState
                        `shouldBe`
                            initialUiState { uiTodos = previous.uiTodos }

        it "keeps a child's live todo list across empty snapshot refreshes" do
            let todoCall =
                    functionToolCall
                        "todo-1"
                        "todo_write"
                        "{\"todos\":[{\"id\":\"1\",\"content\":\"Keep this list\"}]}"
                previous =
                    foldl
                        (flip reduceUi)
                        initialUiState
                        [ UiLoop TurnStarted
                        , UiLoop (ToolStarted todoCall)
                        , UiLoop
                            (ToolFinished ToolCallResult
                                { callId = "todo-1"
                                , output = "- [in_progress] 1: Keep this list"
                                , callKind = FunctionCallKind
                                })
                        ]
                merged = mergeConversationView previous initialUiState
                updated =
                    mergeConversationView
                        previous
                        (initialUiState
                            { uiTodos =
                                [ TodoDisplayLine
                                    TodoDisplayCompleted
                                    "Keep this list"
                                ]
                            })
            map (.todoLineText) (visibleTodoList merged)
                `shouldBe` ["Keep this list"]
            map (.todoLineStatus) updated.uiTodos
                `shouldBe` [TodoDisplayCompleted]

    describe "conversation scrollbar" do
        it "uses a visible trough that repaints old thumb cells" do
            let renderCell widget =
                    V.picImage $
                        renderWidget Nothing
                            [hLimit 1 (vLimit 1 widget)]
                            (1, 1)
            renderCell
                (conversationScrollbarRenderer @()).renderVScrollbarTrough
                `shouldBe` V.char V.defAttr '│'
            renderCell
                (conversationScrollbarRenderer @()).renderVScrollbar
                `shouldBe` V.char V.defAttr '┃'

    describe "motion demand" do
        it "distinguishes foreground, waiting, background, and static modes" do
            let idle =
                    reduceUi
                        (UiUserSubmitted "done")
                        initialUiState
                running =
                    reduceUi (UiLoop TurnStarted) idle
            motionDemandFor MotionFull False False False running
                `shouldBe` MotionFast
            motionDemandFor MotionFull True False False running
                `shouldBe` MotionSlow
            motionDemandFor MotionFull False True False idle
                `shouldBe` MotionSlow
            motionDemandFor MotionFull False False False idle
                `shouldBe` MotionNone
            motionDemandFor MotionFull False False False initialUiState
                `shouldBe` MotionSlow
            motionDemandFor MotionReduced False False False initialUiState
                `shouldBe` MotionNone
            motionDemandFor MotionReduced False False False running
                `shouldBe` MotionSlow
            motionDemandFor MotionOff False False False running
                `shouldBe` MotionSlow

        it "keeps semantic countdown updates active in every motion mode" do
            let countdown =
                    reduceUi
                        (UiRetryCountdown
                            "Provider unavailable.\n"
                            60000
                            ", or choose another provider.")
                        initialUiState
            motionDemandFor MotionFull False False False countdown
                `shouldBe` MotionSlow
            motionDemandFor MotionReduced False False False countdown
                `shouldBe` MotionSlow
            motionDemandFor MotionOff False False False countdown
                `shouldBe` MotionSlow

        it "suppresses cosmetic motion and slows cadence while unfocused" do
            let idle =
                    reduceUi
                        (UiUserSubmitted "done")
                        initialUiState
                running =
                    reduceUi (UiLoop TurnStarted) idle
            motionDemandForTerminalFocus
                TerminalFocused
                MotionFull
                False
                False
                False
                running
                `shouldBe` MotionFast
            motionDemandForTerminalFocus
                TerminalUnfocused
                MotionFull
                False
                True
                True
                idle
                `shouldBe` MotionNone
            motionDemandForTerminalFocus
                TerminalUnfocused
                MotionFull
                False
                False
                False
                running
                `shouldBe` MotionSlow
            motionModeForTerminalFocus TerminalFocused MotionFull
                `shouldBe` MotionFull
            motionModeForTerminalFocus TerminalFocusUnknown MotionReduced
                `shouldBe` MotionReduced
            motionModeForTerminalFocus TerminalUnfocused MotionFull
                `shouldBe` MotionOff

        it "bumps the scheduler generation on demand or timer boundaries" do
            nextMotionSchedule
                False
                MotionSlow
                160000
                (MotionSlow, 160000, 4)
                `shouldBe` (MotionSlow, 160000, 4)
            nextMotionSchedule
                True
                MotionSlow
                160000
                (MotionSlow, 160000, 4)
                `shouldBe` (MotionSlow, 160000, 5)
            nextMotionSchedule
                False
                MotionFast
                80000
                (MotionSlow, 160000, 4)
                `shouldBe` (MotionFast, 80000, 5)
            nextMotionSchedule
                False
                MotionSlow
                400000
                (MotionSlow, 500000, 4)
                `shouldBe` (MotionSlow, 400000, 5)

        it "requests one unfocused redraw when a running turn becomes idle" do
            let running = reduceUi (UiLoop TurnStarted) initialUiState
                finished =
                    reduceUi
                        (UiLoop
                            (TurnFinished
                                (emptyTurnOutput "response-1" [] Nothing)))
                        running
                continuing =
                    reduceUi
                        (UiLoop
                            (TurnFinished
                                (emptyTurnOutput
                                    "response-1"
                                    [functionToolCall "call-1" "read_file" "{}"]
                                    Nothing)))
                        running
            turnCompletionRequiresRedraw running finished `shouldBe` True
            turnCompletionRequiresRedraw running continuing `shouldBe` False
            turnCompletionRequiresRedraw finished finished `shouldBe` False

        it "retains sub-millisecond time across clock samples" do
            elapsedMillisSince 1000000 1499999
                `shouldBe` (0, 1000000)
            elapsedMillisSince 1234567 3234999
                `shouldBe` (2, 3234567)
            elapsedMillisSince 4000000 3000000
                `shouldBe` (0, 4000000)

        it "restarts cadence when turn, notice, and promoted-input timers start" do
            let idle =
                    reduceUi
                        (UiUserSubmitted "done")
                        initialUiState
                turnStarted =
                    reduceUi (UiLoop TurnStarted) idle
                turnFinished =
                    reduceUi
                        (UiLoop
                            (TurnFinished
                                (emptyTurnOutput
                                    "response-1"
                                    []
                                    Nothing)))
                        turnStarted
                notice =
                    reduceUi
                        (UiSetNotice
                            (Just (successNotice "saved")))
                        idle
                warning =
                    reduceUi
                        (UiLoop (WarningRaised "Codex usage is low"))
                        turnStarted
                promoted =
                    reduceUi (UiInputPromoted "urgent") turnStarted
            uiEventRestartsMotionSchedule
                (UiLoop TurnStarted)
                idle
                turnStarted
                Map.empty
                `shouldBe` True
            uiEventRestartsMotionSchedule
                (UiLoop
                    (TurnFinished
                        (emptyTurnOutput "response-1" [] Nothing)))
                turnStarted
                turnFinished
                Map.empty
                `shouldBe` True
            uiEventRestartsMotionSchedule
                (UiSetNotice (Just (successNotice "saved")))
                idle
                notice
                Map.empty
                `shouldBe` True
            uiEventRestartsMotionSchedule
                (UiLoop (WarningRaised "Codex usage is low"))
                turnStarted
                warning
                Map.empty
                `shouldBe` True
            uiEventRestartsMotionSchedule
                (UiInputPromoted "urgent")
                turnStarted
                promoted
                Map.empty
                `shouldBe` True
            uiEventRestartsMotionSchedule
                (UiLoop (ActivityUpdated "still working"))
                turnStarted
                (reduceUi
                    (UiLoop (ActivityUpdated "still working"))
                    turnStarted)
                Map.empty
                `shouldBe` False

        it "refreshes native progress only after each five-second bucket" do
            let running =
                    advanceUiTime 5000 $
                        reduceUi (UiLoop TurnStarted) initialUiState
            nativeProgressKeepaliveDue False 0 running
                `shouldBe` True
            nativeProgressKeepaliveDue False 1 running
                `shouldBe` False
            nativeProgressKeepaliveDue True 0 running
                `shouldBe` False

        it "self-schedules completion flashes but disables them in off mode" do
            let idle =
                    reduceUi
                        (UiUserSubmitted "done")
                        initialUiState
            motionDemandFor MotionFull False False True idle
                `shouldBe` MotionFast
            motionDemandFor MotionReduced False False True idle
                `shouldBe` MotionSlow
            motionDemandFor MotionOff False False True idle
                `shouldBe` MotionNone

    describe "completion flashes" do
        it "detects only live-to-terminal block transitions" do
            let call =
                    functionToolCall
                        "tool-1"
                        "run_terminal_cmd"
                        "{\"command\":\"true\"}"
                running =
                    reduceUi
                        (UiLoop (ToolStarted call))
                        (reduceUi (UiLoop TurnStarted) initialUiState)
                completed =
                    reduceUi
                        (UiLoop
                            (ToolFinished
                                ToolCallResult
                                    { callId = "tool-1"
                                    , output = "exit: 0"
                                    , callKind = FunctionCallKind
                                    }))
                        running
            completionFlashTransitions running completed
                `shouldBe` [BlockId 1]
            completionFlashTransitions completed completed
                `shouldBe` []

        it "ignores assistant streams and unsuccessful terminal states" do
            let assistantRunning =
                    reduceUi
                        (UiLoop (TextDelta "answer"))
                        (reduceUi (UiLoop TurnStarted) initialUiState)
                assistantComplete =
                    reduceUi
                        (UiLoop
                            (TurnFinished
                                (emptyTurnOutput
                                    "response-1"
                                    []
                                    (Just "answer"))))
                        assistantRunning
                call =
                    functionToolCall
                        "tool-2"
                        "run_terminal_cmd"
                        "{\"command\":\"false\"}"
                toolRunning =
                    reduceUi
                        (UiLoop (ToolStarted call))
                        (reduceUi (UiLoop TurnStarted) initialUiState)
                toolFailed =
                    reduceUi
                        (UiLoop
                            (ToolFinished
                                ToolCallResult
                                    { callId = "tool-2"
                                    , output = "Error: failed"
                                    , callKind = FunctionCallKind
                                    }))
                        toolRunning
            completionFlashTransitions
                assistantRunning
                assistantComplete
                `shouldBe` []
            completionFlashTransitions toolRunning toolFailed
                `shouldBe` []

        it "expires completion flashes from elapsed milliseconds" do
            let active = Map.singleton (BlockId 7) 400
            advanceCompletionFlashes 399 active
                `shouldBe` Map.singleton (BlockId 7) 1
            advanceCompletionFlashes 400 active
                `shouldBe` Map.empty

mockVty :: V.Output -> IO () -> IO Bool -> IO V.Vty
mockVty output shutdownAction isShutdownAction = do
    channel <- newTChanIO
    let input = V.Input
            { V.eventChannel = channel
            , V.shutdownInput = pure ()
            , V.restoreInputState = pure ()
            , V.inputLogMsg = const (pure ())
            }
    pure V.Vty
        { V.update = const (pure ())
        , V.nextEvent = pure (V.EvKey V.KEsc [])
        , V.nextEventNonblocking = pure Nothing
        , V.inputIface = input
        , V.outputIface = output
        , V.refresh = pure ()
        , V.shutdown = shutdownAction
        , V.isShutdown = isShutdownAction
        }

choiceOverlay :: Bool -> ChoiceOverlay
choiceOverlay closeOnTurnEnd = ChoiceOverlay
    { choicePresentation = ChoiceDialog
    , choiceTitle = "choice"
    , choiceBody = ""
    , choiceIndex = 0
    , choiceRows = [("one", "")]
    , choiceCloseOnTurnEnd = closeOnTurnEnd
    }

textOverlay :: Text -> Int -> TextOverlay
textOverlay draft cursor = TextOverlay
    { textTitle = "prompt"
    , textBody = ""
    , textDraft = draft
    , textCursor = cursor
    , textInputMode = TextInputPlain
    }

rootEntry :: AgentEntry
rootEntry = AgentEntry
    { agentTarget = AgentRoot
    , agentPath = "/root"
    , agentStatus = "active"
    , agentModel = Nothing
    , agentSteps = []
    , agentTranscript = []
    , agentConversation = initialUiState
    }

childEntry :: Int -> AgentEntry
childEntry index = AgentEntry
    { agentTarget = AgentChild (SubagentId name)
    , agentPath = "/root/" <> name
    , agentStatus = "running"
    , agentModel = Just "gpt-5.6-luna"
    , agentSteps = []
    , agentTranscript = []
    , agentConversation = initialUiState
    }
  where
    name :: Text
    name = "agent-" <> Text.pack (show index)
