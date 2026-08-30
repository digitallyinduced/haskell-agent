module Agent.CLI.TUIAppSpec (spec) where

import Agent.CLI.AgentViewport
    ( AgentEntry(..)
    , AgentStep(..)
    , AgentStepState(..)
    , AgentTarget(..)
    )
import Agent.CLI.Input (ReplLine(..), terminalTextWidth)
import Agent.CLI.Interrupt (CtrlCDecision(..))
import Agent.CLI.TUI.App
    ( applyStoredFullscreenWindowTitle
    , applyMetaConsoleEdit
    , applyTextPromptEdit
    , advanceCompletionFlashes
    , agentEntryWindow
    , agentPaneEntryLimit
    , agentPaneVisible
    , backgroundActivityText
    , completionFlashTransitions
    , completionRequiresRedraw
    , conversationScrollbarRenderer
    , choiceRowColumns
    , choiceClosesOnUiTransition
    , elapsedMillisSince
    , externalUrlCommand
    , launchExternalUrlCommand
    , fullscreenBounds
    , fullscreenVtyConfig
    , fullscreenSurface
    , fullscreenApp
    , initialFullscreenAppState
    , isMetaConsoleToggle
    , mergeConversationView
    , newFullscreenInputBuffer
    , newFullscreenRuntime
    , withTrackedVtyBuilder
    , wrapFullscreenKeyboardVty
    , wrapMarkdownLinkCursorVty
    , motionDemandFor
    , motionDemandForTerminalFocus
    , motionModeForTerminalFocus
    , lambdaArtWidget
    , quickStartRows
    , quickStartVisible
    , quickStartWideVisible
    , startupCapabilityLines
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
    , toolImageBlockId
    , turnCompletionRequiresRedraw
    , uiEventRestartsMotionSchedule
    )
import Agent.CLI.WindowTitle (oscWindowTitleBytes)
import Agent.CLI.TUI.Types
    ( AppEvent(..)
    , AppState(..)
    , ChoiceOverlay(..)
    , ChoicePresentation(..)
    , FullscreenInput(..)
    , choiceVisibleRows
    , selectedChoiceIndex
    , FullscreenRuntime(..)
    , HistoryCommit(..)
    , MetaConsoleOverlay(..)
    , Name(..)
    , TerminalFocus(..)
    , TextInputMode(..)
    , TextOverlay(..)
    )
import Agent.CLI.TUI.History
    ( HistoryCursor(..)
    , HistoryDirection(..)
    , HistoryGeneration(..)
    , HistoryPage(..)
    , HistoryRequest(..)
    , HistoryTurn(..)
    , applyHistoryPage
    , emptyHistoryWindow
    )
import Agent.CLI.TUI.ImagePreview
    ( NativePreviewPlacement(..)
    , TuiImagePreview(..)
    )
import Agent.CLI.Terminal
    ( TerminalCapabilities(..)
    , TerminalKind(..)
    , kittyKeyboardDisambiguatePush
    , kittyKeyboardPop
    )
import Agent.Loop (ImageAttachment(..), LoopEvent(..), emptyTurnOutput)
import Brick
    ( App(..)
    , BrickEvent(..)
    , VScrollbarRenderer(..)
    , Widget
    , customMain
    , halt
    , hLimit
    , renderWidget
    , txt
    , vLimit
    )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Types as B
import Agent.Subagents (SubagentId(..))
import Agent.ToolDispatch
    ( ToolCallKind(..)
    , ToolCallResult(..)
    , customToolCall
    , functionToolCall
    )
import Agent.TUI.Model
import Agent.TUI.Presentation
    ( TodoDisplayLine(..)
    , TodoDisplayStatus(..)
    )
import Agent.TUI.Motion
import Control.Concurrent.STM
    ( atomically
    , newEmptyTMVarIO
    , newTChanIO
    , retry
    )
import Control.Monad (replicateM_)
import qualified Data.ByteString as ByteString
import Data.Foldable (find, toList)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Output.Mock as VMock
import qualified Agent.CLI.TUI.Composer as Composer
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = do
    describe "toolImageBlockId" do
        let started callId name =
                reduceUi
                    (UiLoop (ToolStarted (functionToolCall callId name "{}")))
            finished callId =
                reduceUi
                    (UiLoop
                        (ToolFinished
                            (ToolCallResult callId "done" FunctionCallKind)))
            firstBlock = BlockId initialUiState.uiNextBlockId
            secondBlock = BlockId (initialUiState.uiNextBlockId + 1)

        it "attaches to the block of the originating call" do
            let ui =
                    started "call-2" "show_image" $
                        started "call-1" "shell_command" initialUiState
            toolImageBlockId "call-1" ui `shouldBe` Just firstBlock
            toolImageBlockId "call-2" ui `shouldBe` Just secondBlock

        it "falls back to the newest running tool block for nested code-mode calls" do
            let ui =
                    started "exec-1" "exec" $
                        finished "call-1" $
                            started "call-1" "shell_command" initialUiState
            toolImageBlockId "code-mode:1:show_image" ui
                `shouldBe` Just secondBlock

        it "ignores unknown calls once no tool is running" do
            let ui =
                    finished "call-1" $
                        started "call-1" "shell_command" initialUiState
            toolImageBlockId "code-mode:1:show_image" ui `shouldBe` Nothing
            toolImageBlockId "code-mode:1:show_image" initialUiState
                `shouldBe` Nothing

    describe "background activity status" do
        it "names a running agent and its current step" do
            backgroundActivityText
                [ rootEntry
                , (childEntry 1)
                    { agentSteps =
                        [ AgentStep AgentStepCompleted "Read files" Nothing
                        , AgentStep AgentStepRunning "Running focused tests" Nothing
                        ]
                    }
                ]
                `shouldBe` "Background · agent-1 — Running focused tests"

        it "summarizes multiple active agents and ignores finished agents" do
            backgroundActivityText
                [ rootEntry
                , (childEntry 1)
                    { agentSteps = [AgentStep AgentStepRunning "Reading logs" Nothing] }
                , (childEntry 2)
                    { agentSteps = [AgentStep AgentStepRunning "Reproducing bug" Nothing] }
                , (childEntry 3)
                    { agentSteps = [AgentStep AgentStepRunning "Waiting" Nothing] }
                , (childEntry 4) { agentStatus = "done" }
                ]
                `shouldBe` "3 agents · agent-1 — Reading logs; agent-2 — Reproducing bug …"

        it "falls back to the agent name before its first step arrives" do
            backgroundActivityText [rootEntry, childEntry 1]
                `shouldBe` "Background · agent-1"

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

        it "requests the JavaScript grammar for exec source" do
            let conversation =
                    reduceUi
                        (UiLoop
                            (ToolStarted
                                (customToolCall
                                    "exec-1"
                                    "exec"
                                    "const answer = 42;")))
                        initialUiState
            syntaxLanguagesForBlocks (toList conversation.uiBlocks)
                `shouldBe` Set.singleton "javascript"

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

        it "does not block on a long-running URL opener" do
            result <- timeout 1_000_000
                (launchExternalUrlCommand ("sleep", ["2"]))
            result `shouldBe` Just True

        it "reports an opener that exits unsuccessfully" do
            launchExternalUrlCommand ("false", []) `shouldReturn` False

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

    describe "Meta Console" do
        it "recognizes Command/Meta+K and Alt+K without stealing plain K" do
            isMetaConsoleToggle
                (V.EvKey (V.KChar 'k') [V.MMeta])
                `shouldBe` True
            isMetaConsoleToggle
                (V.EvKey (V.KChar 'k') [V.MAlt])
                `shouldBe` True
            isMetaConsoleToggle
                (V.EvKey (V.KChar 'k') [])
                `shouldBe` False
            isMetaConsoleToggle
                (V.EvKey (V.KChar 'k') [V.MCtrl])
                `shouldBe` False

        it "edits multi-code-point glyphs as one grapheme" do
            let emoji = Text.pack ['\x1f469', '\x200d', '\x1f4bb']
                overlay = MetaConsoleOverlay
                    { metaConsoleDraft = "a" <> emoji <> "b"
                    , metaConsoleCursor = 4
                    }
                edited =
                    applyMetaConsoleEdit
                        (V.EvKey V.KBS [])
                        overlay
            (fmap
                (\current ->
                    ( current.metaConsoleDraft
                    , current.metaConsoleCursor
                    ))
                edited)
                `shouldBe` Just ("ab", 1)

        it "queues a private request during a turn without changing the composer draft" do
            (state, inputs) <- runMetaConsoleSubmission True
            state.appUi.uiDraft `shouldBe` "unfinished composer draft"
            (() <$ state.appMetaConsole) `shouldBe` Nothing
            map
                (\input ->
                    ( input.fullscreenInputLine
                    , input.fullscreenInputQueued
                    , input.fullscreenInputDisplay
                    ))
                inputs
                `shouldBe`
                    [ ( ReplMeta "connect my Grok account"
                      , True
                      , Nothing
                      )
                    ]

        it "submits immediately at an idle REPL boundary" do
            (_, inputs) <- runMetaConsoleSubmission False
            map (.fullscreenInputQueued) inputs `shouldBe` [False]

        it "keeps the request open when the prompt queue is full" do
            let running = reduceUi (UiLoop TurnStarted) initialUiState
            runtime <- newScriptRuntime running
            atomically $
                replicateM_ Composer.fullscreenInputCountLimit do
                    result <- Composer.appendFullscreenInput
                        runtime.runtimeInput
                        FullscreenInput
                            { fullscreenInputLine = ReplEof
                            , fullscreenInputQueued = True
                            , fullscreenInputDisplay = Nothing
                            }
                    case result of
                        Left message -> error (Text.unpack message)
                        Right () -> pure ()
            let request = "connect my Grok account"
                initialState =
                    initialFullscreenAppState runtime [] AgentRoot [] 0
                script =
                    [ FullscreenScriptVty
                        (V.EvKey (V.KChar 'k') [V.MMeta])
                    , FullscreenScriptVty
                        (V.EvPaste (encoded request))
                    , FullscreenScriptVty
                        (V.EvKey V.KEnter [])
                    , FullscreenScriptHalt
                    ]
            (_, finalState) <-
                runFullscreenScriptWithState initialState script
            (.metaConsoleDraft) <$> finalState.appMetaConsole
                `shouldBe` Just request
            (.noticeText) <$> finalState.appUi.uiNotice
                `shouldBe`
                    Just
                        "Prompt queue is full; wait for a queued prompt to be consumed."
            inputs <- atomically $
                Composer.readFullscreenInputs runtime.runtimeInput
            Seq.length inputs
                `shouldBe` Composer.fullscreenInputCountLimit

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

    describe "searchable choice rows" do
        let searchable = (choiceOverlay False)
                { choiceSearch = True
                , choiceQuery = "claude"
                , choiceRows =
                    [ ("gpt-5.6", "Vendor: OpenAI")
                    , ("anthropic/claude-sonnet", "Anthropic")
                    , ("google/gemini", "Google")
                    ]
                }
        it "matches title and detail case-insensitively" do
            choiceVisibleRows searchable
                `shouldBe` [(1, ("anthropic/claude-sonnet", "Anthropic"))]
            choiceVisibleRows searchable { choiceQuery = "VENDOR" }
                `shouldBe` [(0, ("gpt-5.6", "Vendor: OpenAI"))]
        it "returns the source index after filtering" do
            selectedChoiceIndex searchable
                `shouldBe` Just 1
        it "returns no selection when a query has no matches" do
            selectedChoiceIndex
                searchable { choiceQuery = "missing" }
                `shouldBe` Nothing

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
        it "maps the Kitty-encoded Esc key so its payload cannot leak" do
            let mappings = V.configInputMap fullscreenVtyConfig
            mapM_
                (\body -> mappings `shouldContain`
                    [(Nothing, "\ESC[" <> body, V.EvKey V.KEsc [])])
                [ "27u"
                , "27;1u"
                , "27;1:1u"
                , "27;2u"
                , "27;5:1u"
                , "27;65u"
                , "27;256:1u"
                ]

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
                  , "\ESC[107;9u"
                  , V.EvKey (V.KChar 'k') [V.MMeta]
                  )
                , ( Nothing
                  , "\ESC[107:75:75;9:1u"
                  , V.EvKey (V.KChar 'k') [V.MMeta]
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

    describe "fullscreen link cursor lifecycle" do
        it "restores the Ghostty cursor before Vty shutdown" do
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
            let terminal = TerminalCapabilities
                    { terminalKind = TerminalGhostty
                    , terminalIsTty = True
                    , terminalInsideTmux = False
                    , terminalInlineImages = True
                    , terminalNativeProgress = True
                    , terminalNotifications = True
                    , terminalSemanticPrompts = True
                    , terminalOsc52Clipboard = True
                    , terminalSynchronizedOutput = True
                    , terminalKittyKeyboard = True
                    }
                reset = TextEncoding.encodeUtf8 "\ESC]22;default\ESC\\"
            wrapped <- wrapMarkdownLinkCursorVty terminal vty
            V.shutdown wrapped
            readIORef events `shouldReturn` [Left reset, Right ()]

    describe "fullscreen window title" do
        it "replays the stored session title as UTF-8 OSC bytes" do
            titles <- newIORef ([] :: [ByteString.ByteString])
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
            let title = "⠋ New session"
            setFullscreenWindowTitle runtime title
            readIORef runtime.runtimeWindowTitle
                `shouldReturn` Just title
            applyStoredFullscreenWindowTitle
                runtime
                output
                    { V.outputByteBuffer =
                        \bytes -> modifyIORef' titles (<> [bytes])
                    }
            actual <- readIORef titles
            actual `shouldBe` [oscWindowTitleBytes title]
            actual
                `shouldSatisfy`
                    any (ByteString.isInfixOf (ByteString.pack [0xE2, 0xA0, 0x8B]))

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
            quickStartVisible 100 21 `shouldBe` False

        it "uses the two-column dashboard only in wide render contexts" do
            quickStartWideVisible 140 35 `shouldBe` True
            quickStartWideVisible 103 35 `shouldBe` False
            quickStartWideVisible 140 28 `shouldBe` False

        it "surfaces the existing high-value startup commands" do
            quickStartRows
                `shouldBe`
                    [ (QuickStartWorktree, "New worktree", "/worktree")
                    , (QuickStartResume, "Resume session", "/resume")
                    , (QuickStartCommands, "Browse commands", "/")
                    , (QuickStartModel, "Manage models", "/model")
                    ]

        it "packs capability names into bounded startup rows" do
            let lines' =
                    startupCapabilityLines
                        20
                        2
                        [ "read_file"
                        , "grep"
                        , "shell_command"
                        , "apply_patch"
                        ]
            lines'
                `shouldBe`
                    [ "read_file · grep"
                    , "shell_command · … +1"
                    ]
            map terminalTextWidth lines'
                `shouldSatisfy` all (<= 20)

        it "summarizes omitted capabilities and handles an empty catalog" do
            startupCapabilityLines
                20
                1
                ["read_file", "grep", "shell_command", "apply_patch"]
                `shouldBe` ["read_file · … +3"]
            startupCapabilityLines 20 2 []
                `shouldBe` ["none"]

        it "renders untrusted capability names without terminal controls" do
            startupCapabilityLines 40 1 ["  safe\nname\ESC[31m  "]
                `shouldBe` ["safe↵name␛[31m"]

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

    describe "history replacement viewport" do
        it "shows the new durable tail immediately while focused" do
            timeout 2_000_000
                (fst <$> replacementAfterHistoryReplacement ReplaceWhileFocused)
                `shouldReturn` Just True

        it "shows the new durable tail immediately when focus returns" do
            timeout 2_000_000
                (fst <$> replacementAfterHistoryReplacement ReplaceWhileHidden)
                `shouldReturn` Just True

        it "reflows native placements after focus returns" do
            timeout 2_000_000
                (snd <$> replacementAfterHistoryReplacement ReplaceWhileHidden)
                `shouldReturn` Just True

        it "shows the durable tail without relying on a focus-gained event" do
            timeout 2_000_000
                (replacementLeavesDurableTailVisible ReplaceWhileHiddenNoFocus)
                `shouldReturn` Just True

        it "keeps focused tail-following through content shrink" do
            timeout 2_000_000 (replacementPreservesFollow True)
                `shouldReturn` Just True

        it "keeps paused scrollback paused through content shrink" do
            timeout 2_000_000 (replacementPreservesFollow False)
                `shouldReturn` Just True

    describe "submitted image history retention" do
        it "remaps a live preview onto its committed durable block" do
            timeout 2_000_000 committedPreviewKeys
                `shouldReturn` Just [BlockId (-1)]

        it "clears previews when history is reset" do
            timeout 2_000_000 resetPreviewState
                `shouldReturn` Just ([], [], 1)

        it "prunes previews when their history turn is evicted" do
            timeout 2_000_000 evictedPreviewKeys
                `shouldReturn` Just []

    describe "unfocused terminal recovery" do
        it "treats paste input as proof that focus returned" do
            timeout 2_000_000 unfocusedPasteRendersDraft
                `shouldReturn` Just True

        it "renders a blocking text prompt without a focus-gained event" do
            timeout 2_000_000 unfocusedTextPromptIsVisible
                `shouldReturn` Just True

        it "renders a submitted prompt without a focus-gained event" do
            timeout 2_000_000 unfocusedSubmissionIsVisible
                `shouldReturn` Just True

        it "renders reset durable history without a focus-gained event" do
            timeout 2_000_000 unfocusedHistoryResetIsVisible
                `shouldReturn` Just True

        it "halts when the worker stops while unfocused" do
            timeout 2_000_000 unfocusedStopHalts
                `shouldReturn` Just ()

        it "runs a suspension requested while unfocused" do
            timeout 2_000_000 unfocusedSuspendRuns
                `shouldReturn` Just True

        it "keeps high-frequency streaming redraws throttled" do
            timeout 2_000_000 unfocusedStreamingRemainsThrottled
                `shouldReturn` Just True

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

        it "requests an unfocused redraw when any child agent finishes" do
            let runningChild = childEntry 1
                sibling = childEntry 2
                finishedChild =
                    runningChild { agentStatus = "completed" }
                stillStreaming =
                    runningChild
                        { agentTranscript = ["assistant: still working"] }
            completionRequiresRedraw
                initialUiState
                [rootEntry, runningChild, sibling]
                initialUiState
                [rootEntry, finishedChild, sibling]
                `shouldBe` True
            completionRequiresRedraw
                initialUiState
                [rootEntry, runningChild, sibling]
                initialUiState
                [rootEntry, stillStreaming, sibling]
                `shouldBe` False
            completionRequiresRedraw
                initialUiState
                [rootEntry, runningChild, sibling]
                initialUiState
                [rootEntry, sibling]
                `shouldBe` True

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

data FullscreenScriptEvent
    = FullscreenScriptApp !AppEvent
    | FullscreenScriptVty !V.Event
    | FullscreenScriptHalt

data ReplacementScenario
    = ReplaceWhileFocused
    | ReplaceWhileHidden
    | ReplaceWhileHiddenNoFocus

runMetaConsoleSubmission :: Bool -> IO (AppState, [FullscreenInput])
runMetaConsoleSubmission running = do
    let draft = "unfinished composer draft"
        baseUi = reduceUi (UiSetDraft draft (Text.length draft)) initialUiState
        ui =
            if running
                then reduceUi (UiLoop TurnStarted) baseUi
                else baseUi
    runtime <- newScriptRuntime ui
    let request = "connect my Grok account"
        initialState =
            initialFullscreenAppState runtime [] AgentRoot [] 0
        script =
            [ FullscreenScriptVty
                (V.EvKey (V.KChar 'k') [V.MMeta])
            , FullscreenScriptVty
                (V.EvPaste (encoded request))
            , FullscreenScriptVty
                (V.EvKey V.KEnter [])
            , FullscreenScriptHalt
            ]
    (_, finalState) <-
        runFullscreenScriptWithState initialState script
    inputs <-
        toList <$>
            atomically
                (Composer.readFullscreenInputs runtime.runtimeInput)
    pure (finalState, inputs)

replacementLeavesDurableTailVisible :: ReplacementScenario -> IO Bool
replacementLeavesDurableTailVisible scenario =
    fst <$> replacementAfterHistoryReplacement scenario

replacementAfterHistoryReplacement :: ReplacementScenario -> IO (Bool, Bool)
replacementAfterHistoryReplacement scenario = do
    let liveTranscript =
            Text.unlines (replicate 200 "live transcript line")
    runtime <- newScriptRuntime
        (initialUiState { uiFollow = True })
    let
        durableTail = "FINAL DURABLE ANSWER"
        durableBlock = UiBlock
            { blockId = BlockId 1000
            , blockKind = BlockAssistant
            , blockTitle = "Assistant"
            , blockBody =
                Text.unlines
                    (replicate 35 "durable transcript line"
                        <> [durableTail])
            , blockTimestamp = ""
            , blockDetail = ""
            , blockState = BlockComplete
            , blockExpanded = False
            , blockCallId = Nothing
            }
        durableTurn = HistoryTurn
            { historyTurnCursor = HistoryCursor 0
            , historyTurnBlocks = Seq.singleton durableBlock
            }
        initialState =
            initialFullscreenAppState runtime [] AgentRoot [] 0

    -- A single custom-event channel makes each focus/history sequence
    -- deterministic while still running the real Brick event handler.
    let commit = FullscreenScriptApp
            (AppHistoryCommitted
                (HistoryGeneration 0)
                durableTurn
                HistoryCommitAppend)
        beginLive =
            [ FullscreenScriptApp AppHistoryLiveStarted
            , FullscreenScriptApp
                (AppUi (UiAssistantHistory liveTranscript))
            ]
        script = beginLive <> case scenario of
            ReplaceWhileFocused ->
                [ commit
                , FullscreenScriptApp AppStop
                ]
            ReplaceWhileHidden ->
                [ FullscreenScriptVty V.EvLostFocus
                , commit
                -- This later hidden event discards the commit's pending
                -- scroll request, so focus gain must reassert it.
                , FullscreenScriptApp AppConversationReflow
                , FullscreenScriptVty V.EvGainedFocus
                , FullscreenScriptApp AppStop
                ]
            ReplaceWhileHiddenNoFocus ->
                [ FullscreenScriptVty V.EvLostFocus
                , commit
                -- Some terminal tabs omit focus gain; the durable replacement
                -- must still reach the terminal's backing screen.
                , FullscreenScriptApp AppConversationReflow
                , FullscreenScriptHalt
                ]
    (rendered, finalState) <- runFullscreenScriptWithState initialState script
    pure
        ( encoded durableTail `ByteString.isInfixOf` rendered
        , finalState.appConversationReflowQueued
        )

replacementPreservesFollow :: Bool -> IO Bool
replacementPreservesFollow follow = do
    let liveTranscript =
            Text.unlines (replicate 200 "scrollback transcript line")
    runtime <- newScriptRuntime
        (initialUiState { uiFollow = follow })
    let durableTurn = HistoryTurn
            { historyTurnCursor = HistoryCursor 0
            , historyTurnBlocks =
                Seq.singleton
                    (markerBlock
                        (BlockId 1000)
                        (Text.unlines
                            (replicate 12 "short durable transcript line")))
            }
        initialState =
            initialFullscreenAppState runtime [] AgentRoot [] 0
        script =
            [ FullscreenScriptApp AppHistoryLiveStarted
            , FullscreenScriptApp
                (AppUi (UiAssistantHistory liveTranscript))
            , FullscreenScriptApp
                (AppHistoryCommitted
                    (HistoryGeneration 0)
                    durableTurn
                    HistoryCommitAppend)
            , FullscreenScriptHalt
            ]
    (_, finalState) <- runFullscreenScriptWithState initialState script
    pure (finalState.appUi.uiFollow == follow)

committedPreviewKeys :: IO [BlockId]
committedPreviewKeys = do
    runtime <- newScriptRuntime initialUiState
    let liveUi = reduceUi (UiUserSubmitted "question") initialUiState
        initialState =
            (initialFullscreenAppState runtime [] AgentRoot [] 0)
                { appUi = liveUi
                , appHistoryLiveStart = Just 0
                , appSubmittedImagePreviews =
                    Map.singleton (BlockId 0) [historyPreview 1]
                }
        durableTurn = HistoryTurn
            { historyTurnCursor = HistoryCursor 0
            , historyTurnBlocks =
                Seq.singleton (markerBlock (BlockId 0) "question")
            }
    (_, finalState) <- runFullscreenScriptWithState
        initialState
        [ FullscreenScriptApp
            (AppHistoryCommitted
                (HistoryGeneration 0)
                durableTurn
                HistoryCommitAppend)
        , FullscreenScriptHalt
        ]
    pure (Map.keys finalState.appSubmittedImagePreviews)

resetPreviewState :: IO ([BlockId], [NativePreviewPlacement], Int)
resetPreviewState = do
    runtime <- newScriptRuntime initialUiState
    writeIORef runtime.runtimeSubmittedImagePlacements
        [historyPlacement (historyPreview 1)]
    let initialState =
            (initialFullscreenAppState runtime [] AgentRoot [] 0)
                { appSubmittedImagePreviews =
                    Map.singleton (BlockId 0) [historyPreview 1]
                }
        generation = HistoryGeneration 1
        page = HistoryPage
            { historyPageGeneration = generation
            , historyPageDirection = HistoryNewer
            , historyPageTurns = Seq.empty
            , historyPageGenerationStart = HistoryCursor 0
            , historyPageTotalTurns = 0
            , historyPageHasOlder = False
            , historyPageHasNewer = False
            }
    (_, finalState) <- runFullscreenScriptWithState
        initialState
        [ FullscreenScriptApp (AppHistoryReset page)
        , FullscreenScriptHalt
        ]
    placements <- readIORef runtime.runtimeSubmittedImagePlacements
    revision <- readIORef runtime.runtimeImagePreviewRevision
    pure
        ( Map.keys finalState.appSubmittedImagePreviews
        , placements
        , revision
        )

evictedPreviewKeys :: IO [BlockId]
evictedPreviewKeys = do
    runtime <- newScriptRuntime initialUiState
    let generation = HistoryGeneration 1
        initialWindow = emptyHistoryWindow generation 2 20 1_000_000
        existingPage = historyTestPage
            generation
            HistoryNewer
            [ historyTestTurn 2 (BlockId (-1))
            , historyTestTurn 3 (BlockId (-2))
            ]
        existingWindow =
            either (error . show) id
                (applyHistoryPage existingPage initialWindow)
        initialState =
            (initialFullscreenAppState runtime [] AgentRoot [] 0)
                { appHistoryWindow = existingWindow
                , appNextHistoryBlockId = -3
                , appSubmittedImagePreviews =
                    Map.singleton (BlockId (-2)) [historyPreview 1]
                }
        request = HistoryRequest
            { historyRequestGeneration = generation
            , historyRequestDirection = HistoryOlder
            , historyRequestCursor = Just (HistoryCursor 2)
            }
        olderPage =
            historyTestPage
                generation
                HistoryOlder
                [historyTestTurn 1 (BlockId 100)]
    (_, finalState) <- runFullscreenScriptWithState
        initialState
        [ FullscreenScriptApp
            (AppHistoryLoaded request (Right olderPage))
        , FullscreenScriptHalt
        ]
    pure (Map.keys finalState.appSubmittedImagePreviews)

historyTestPage
    :: HistoryGeneration
    -> HistoryDirection
    -> [HistoryTurn]
    -> HistoryPage
historyTestPage generation direction turns =
    HistoryPage
        { historyPageGeneration = generation
        , historyPageDirection = direction
        , historyPageTurns = Seq.fromList turns
        , historyPageGenerationStart = HistoryCursor 0
        , historyPageTotalTurns = fromIntegral (length turns)
        , historyPageHasOlder = direction == HistoryNewer
        , historyPageHasNewer = direction == HistoryOlder
        }

historyTestTurn :: Int -> BlockId -> HistoryTurn
historyTestTurn cursor blockId =
    HistoryTurn
        { historyTurnCursor = HistoryCursor (fromIntegral cursor)
        , historyTurnBlocks =
            Seq.singleton
                (markerBlock blockId ("turn " <> Text.pack (show cursor)))
        }

historyPreview :: Int -> TuiImagePreview
historyPreview bytes =
    TuiImagePreview
        { previewMime = "image/png"
        , previewBytes = bytes
        , previewSourceWidth = 1
        , previewSourceHeight = 1
        , previewSample = error "history test forced ANSI preview"
        , previewKittyAttachment = ImageAttachment "image/png" ""
        }

historyPlacement :: TuiImagePreview -> NativePreviewPlacement
historyPlacement preview =
    NativePreviewPlacement
        { nativePreviewImageId = 1
        , nativePreviewRow = 0
        , nativePreviewColumn = 0
        , nativePreviewColumns = 1
        , nativePreviewRows = 1
        , nativePreviewAttachment = preview.previewKittyAttachment
        }

unfocusedPasteRendersDraft :: IO Bool
unfocusedPasteRendersDraft = do
    runtime <- newScriptRuntime initialUiState
    let marker = "IMPLICIT_FOCUS_DRAFT"
        initialState =
            initialFullscreenAppState runtime [] AgentRoot [] 0
        script =
            [ FullscreenScriptVty V.EvLostFocus
            , FullscreenScriptVty (V.EvPaste (encoded marker))
            , FullscreenScriptHalt
            ]
    rendered <- runFullscreenScript initialState script
    pure $ encoded marker `ByteString.isInfixOf` rendered

unfocusedTextPromptIsVisible :: IO Bool
unfocusedTextPromptIsVisible = do
    runtime <- newScriptRuntime initialUiState
    reply <- newEmptyTMVarIO
    let marker = "HIDDEN_TEXT_PROMPT_MARKER"
        initialState =
            initialFullscreenAppState runtime [] AgentRoot [] 0
        script =
            [ FullscreenScriptVty V.EvLostFocus
            , FullscreenScriptApp
                (AppAskText
                    TextInputPlain
                    "Hidden text prompt"
                    marker
                    ""
                    reply)
            , FullscreenScriptHalt
            ]
    rendered <- runFullscreenScript initialState script
    pure $ encoded marker `ByteString.isInfixOf` rendered

unfocusedSubmissionIsVisible :: IO Bool
unfocusedSubmissionIsVisible = do
    runtime <- newScriptRuntime initialUiState
    let marker = "HIDDEN_SUBMITTED_PROMPT"
        initialState =
            initialFullscreenAppState runtime [] AgentRoot [] 0
        script =
            [ FullscreenScriptVty V.EvLostFocus
            , FullscreenScriptApp (AppUi (UiUserSubmitted marker))
            , FullscreenScriptHalt
            ]
    rendered <- runFullscreenScript initialState script
    pure $ encoded marker `ByteString.isInfixOf` rendered

unfocusedHistoryResetIsVisible :: IO Bool
unfocusedHistoryResetIsVisible = do
    runtime <- newScriptRuntime (initialUiState { uiFollow = True })
    let marker = "HIDDEN_RESET_HISTORY"
        liveTranscript =
            Text.unlines (replicate 200 "reset live transcript line")
        generation = HistoryGeneration 1
        cursor = HistoryCursor 0
        durableTurn = HistoryTurn
            { historyTurnCursor = cursor
            , historyTurnBlocks =
                Seq.singleton
                    (markerBlock
                        (BlockId 1001)
                        (Text.unlines
                            (replicate 35 "reset durable transcript line"
                                <> [marker])))
            }
        page = HistoryPage
            { historyPageGeneration = generation
            , historyPageDirection = HistoryNewer
            , historyPageTurns = Seq.singleton durableTurn
            , historyPageGenerationStart = cursor
            , historyPageTotalTurns = 1
            , historyPageHasOlder = False
            , historyPageHasNewer = False
            }
        initialState =
            initialFullscreenAppState runtime [] AgentRoot [] 0
        script =
            [ FullscreenScriptApp
                (AppUi (UiAssistantHistory liveTranscript))
            , FullscreenScriptVty V.EvLostFocus
            , FullscreenScriptApp (AppHistoryReset page)
            , FullscreenScriptApp AppConversationReflow
            , FullscreenScriptHalt
            ]
    rendered <- runFullscreenScript initialState script
    pure $ encoded marker `ByteString.isInfixOf` rendered

unfocusedStopHalts :: IO ()
unfocusedStopHalts = do
    runtime <- newScriptRuntime initialUiState
    let initialState =
            initialFullscreenAppState runtime [] AgentRoot [] 0
    _ <- runFullscreenScript
        initialState
        [ FullscreenScriptVty V.EvLostFocus
        , FullscreenScriptApp AppStop
        ]
    pure ()

unfocusedSuspendRuns :: IO Bool
unfocusedSuspendRuns = do
    runtime <- newScriptRuntime initialUiState
    actionRan <- newIORef False
    reply <- newEmptyTMVarIO
    let initialState =
            initialFullscreenAppState runtime [] AgentRoot [] 0
    _ <- runFullscreenScript
        initialState
        [ FullscreenScriptVty V.EvLostFocus
        , FullscreenScriptApp
            (AppSuspend (writeIORef actionRan True) reply)
        , FullscreenScriptApp AppStop
        ]
    readIORef actionRan

unfocusedStreamingRemainsThrottled :: IO Bool
unfocusedStreamingRemainsThrottled = do
    runtime <- newScriptRuntime initialUiState
    let marker = "HIDDEN_STREAMING_DELTA"
        initialState =
            initialFullscreenAppState runtime [] AgentRoot [] 0
        script =
            [ FullscreenScriptVty V.EvLostFocus
            , FullscreenScriptApp (AppUi (UiLoop (TextDelta marker)))
            , FullscreenScriptHalt
            ]
    rendered <- runFullscreenScript initialState script
    pure $ not (encoded marker `ByteString.isInfixOf` rendered)

newScriptRuntime :: UiState -> IO FullscreenRuntime
newScriptRuntime ui = do
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
        ui

runFullscreenScript
    :: AppState
    -> [FullscreenScriptEvent]
    -> IO ByteString.ByteString
runFullscreenScript initialState script =
    fst <$> runFullscreenScriptWithState initialState script

runFullscreenScriptWithState
    :: AppState
    -> [FullscreenScriptEvent]
    -> IO (ByteString.ByteString, AppState)
runFullscreenScriptWithState initialState script = do
    let scriptedApp = App
            { appDraw = fullscreenApp.appDraw
            , appChooseCursor = fullscreenApp.appChooseCursor
            , appHandleEvent = \case
                AppEvent (FullscreenScriptApp event) ->
                    fullscreenApp.appHandleEvent (AppEvent event)
                AppEvent (FullscreenScriptVty event) ->
                    fullscreenApp.appHandleEvent (VtyEvent event)
                AppEvent FullscreenScriptHalt ->
                    halt
                VtyEvent event ->
                    fullscreenApp.appHandleEvent (VtyEvent event)
                MouseDown name button modifiers location ->
                    fullscreenApp.appHandleEvent
                        (MouseDown name button modifiers location)
                MouseUp name button location ->
                    fullscreenApp.appHandleEvent
                        (MouseUp name button location)
            , appStartEvent = fullscreenApp.appStartEvent
            , appAttrMap = fullscreenApp.appAttrMap
            }
    events <- newBChan (max 1 (length script))
    mapM_ (writeBChan events) script
    let bounds = (80, 24)
    (_, mockOutput) <- VMock.mockTerminal bounds
    outputBytes <- newIORef ByteString.empty
    let output = mockOutput
            { V.outputByteBuffer = \bytes ->
                modifyIORef' outputBytes (<> bytes)
            }
    context <- V.mkDisplayContext output output bounds
    internalEvents <- newTChanIO
    let vty = V.Vty
            { V.update = V.outputPicture context
            , V.nextEvent = atomically retry
            , V.nextEventNonblocking = pure Nothing
            , V.inputIface = V.Input
                { V.eventChannel = internalEvents
                , V.shutdownInput = pure ()
                , V.restoreInputState = pure ()
                , V.inputLogMsg = const (pure ())
                }
            , V.outputIface = output
            , V.refresh = pure ()
            , V.shutdown = pure ()
            , V.isShutdown = pure False
            }
    finalState <-
        customMain vty (pure vty) (Just events) scriptedApp initialState
    rendered <- readIORef outputBytes
    pure (rendered, finalState)

markerBlock :: BlockId -> Text -> UiBlock
markerBlock blockId body = UiBlock
    { blockId
    , blockKind = BlockAssistant
    , blockTitle = "Assistant"
    , blockBody = body
    , blockTimestamp = ""
    , blockDetail = ""
    , blockState = BlockComplete
    , blockExpanded = False
    , blockCallId = Nothing
    }

encoded :: Text -> ByteString.ByteString
encoded = TextEncoding.encodeUtf8

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
    , choiceSearch = False
    , choiceQuery = ""
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
