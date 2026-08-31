module Agent.CLI.TUIPropertySpec (spec) where

import Agent.CLI.AgentViewport
    ( AgentEntry(..)
    , AgentTarget(..)
    )
import Agent.CLI.Input (terminalTextWidth)
import Agent.CLI.Interrupt (CtrlCDecision(..))
import Agent.CLI.TUI.App
    ( drawApp
    , initialFullscreenAppState
    , newFullscreenInputBuffer
    , newFullscreenRuntimeWithSyntaxLoader
    )
import Agent.CLI.TUI.Types
    ( AppState(..)
    , ChoiceOverlay(..)
    , ChoicePresentation(..)
    , TextInputMode(..)
    , TextOverlay(..)
    )
import Agent.Loop (LoopEvent(..))
import Agent.Subagents (SubagentId(..))
import Agent.ToolDispatch
    ( ToolCallKind(..)
    , ToolCallResult(..)
    , functionToolCall
    )
import Agent.TUI.Model
    ( BlockId(..)
    , BlockState(..)
    , Focus(..)
    , PermissionOverlay(..)
    , PromptLimitStatus(..)
    , PromptState(..)
    , RetryCountdown(..)
    , UiBlock(..)
    , UiEvent(..)
    , UiState(..)
    , initialUiState
    , reduceUi
    , visibleTodoList
    , timestampNewMessageBlocks
    , warningNotice
    )
import Agent.TUI.Motion (MotionMode(..))
import qualified Agent.TUI.Theme as Theme
import Brick (renderWidget)
import Control.Monad (forM)
import Data.Foldable (toList)
import Data.IORef (readIORef)
import Data.List (find, nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Output as VOutput
import Graphics.Vty.Output.Mock (mockTerminal)
import Graphics.Vty.PictureToSpans (displayOpsForPic)
import Graphics.Vty.Span (SpanOp(..))
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
    ( Arbitrary(..)
    , Gen
    , Property
    , chooseInt
    , conjoin
    , counterexample
    , elements
    , frequency
    , ioProperty
    , shrinkList
    , sized
    , vectorOf
    , (===)
    )

newtype RenderTrace = RenderTrace [TuiAction]
    deriving (Show)

data TuiAction
    = Resize !Int !Int
    | Submit !Text
    | Assistant !Text
    | SystemMessage !Text
    | ErrorMessage !Text
    | SetDraft !Text !Int
    | QueueInput !Text
    | StartQueuedInput
    | SetRepository !Text !Text !Text
    | SetNotice !Text
    | ClearNotice
    | MoveSelection !Int
    | ToggleSelected
    | SetFocus !Focus
    | ShowPermission !Text
    | MovePermission !Int
    | HidePermission
    | SetRetryCountdown !Text !Int !Text
    | SetFollow !Bool
    | EndTurn !BlockState
    | RestartTurn
    | ClearConversation
    | ShowChoice !ChoicePresentation !Text !Text ![(Text, Text)] !Int
    | ShowText !TextInputMode !Text !Text !Text !Int
    | CloseAppOverlay
    | SetAgents !Int !Text
    deriving (Show)

data RenderHarness = RenderHarness
    { harnessApp :: !AppState
    , harnessSize :: !(Int, Int)
    }

spec :: Spec
spec = do
    baseState <- runIO makeBaseState

    describe "generated fullscreen rendering" do
        modifyMaxSuccess (const 300) $
            prop "keeps every generated frame and cursor inside the terminal" $
                renderTraceProperty baseState

        modifyMaxSuccess (const 100) $
            prop "keeps Vty's incremental assumed frame equal to a clean frame" $
                incrementalTraceProperty baseState

        it "covers the terminal after a tall timestamped conversation is cleared" do
            let populated =
                    foldl
                        (flip applyAction)
                        (RenderHarness baseState (80, 40))
                        (concat
                            [ [ Submit ("prompt " <> Text.pack (show index))
                              , Assistant
                                    (Text.replicate 8
                                        ("long answer " <> Text.pack (show index) <> " "))
                              ]
                            | index <- [1 :: Int .. 20]
                            ])
                cleared =
                    applyAction
                        (Resize 43 17)
                        (applyAction ClearConversation populated)
            assertFrame cleared

        it "keeps narrow composer status borders intact" do
            let app = baseState
                    { appUi =
                        baseState.appUi
                            { uiDraft =
                                "wide 漢字 emoji 🚀 combining é and a very long draft that wraps"
                            , uiCursor = 66
                            , uiPrompt =
                                baseState.appUi.uiPrompt
                                    { promptModel = "gpt-5.6-sol"
                                    , promptEffort = "medium"
                                    , promptMode = "ask"
                                    , promptAccount =
                                        "marc@digitallyinduced.com"
                                    , promptLimitStatus =
                                        Just PromptLimitStatus
                                            { promptLimitText =
                                                "5h limit left: 71%"
                                            , promptLimitWarning = False
                                            }
                                    }
                            }
                    }
                size = (43, 17)
                picture =
                    renderWidget
                        (Just Theme.monochrome)
                        (drawApp app)
                        size
                rows = pictureRows picture size
            case find (Text.isInfixOf "gpt-5.6-sol") rows of
                Nothing ->
                    expectationFailure "composer status row was not rendered"
                Just row -> do
                    Text.take 1 row `shouldBe` "╰"
                    Text.stripEnd row `shouldSatisfy` Text.isSuffixOf "╯"

        it "renders reasoning summaries as Markdown instead of literal markers" do
            let reasoningUi =
                    reduceUi
                        (UiLoop
                            (ReasoningDelta
                                "**Inspecting dependencies**\n\n**Planning the fix**"))
                        (reduceUi (UiLoop TurnStarted) baseState.appUi)
                app = baseState { appUi = reasoningUi }
                size = (80, 20)
                rows =
                    pictureRows
                        (renderWidget
                            (Just Theme.monochrome)
                            (drawApp app)
                            size)
                        size
                rendered = Text.unlines rows
            rendered `shouldSatisfy` Text.isInfixOf "Inspecting dependencies"
            rendered `shouldSatisfy` Text.isInfixOf "Planning the fix"
            rendered `shouldSatisfy` (not . Text.isInfixOf "**")

        it "renders inline Markdown in thought blocks" do
            let app = baseState
                    { appUi =
                        reduceUi
                            (UiLoop
                                (ReasoningDelta
                                    "Inspect `AppState` before continuing."))
                            baseState.appUi
                    }
                size = (80, 24)
                rendered =
                    Text.unlines $
                        pictureRows
                            (renderWidget
                                (Just Theme.monochrome)
                                (drawApp app)
                                size)
                            size
            rendered `shouldSatisfy`
                Text.isInfixOf "Inspect AppState before continuing."
            rendered `shouldNotSatisfy` Text.isInfixOf "`AppState`"

        it "distinguishes completed inspection and action blocks" do
            let inspectCall =
                    functionToolCall
                        "inspect-1"
                        "read_file"
                        "{\"target_file\":\"flake.nix\"}"
                actionCall =
                    functionToolCall
                        "action-1"
                        "shell_command"
                        "{\"command\":\"pwd\"}"
                finish callId output =
                    ToolCallResult
                        { callId
                        , output
                        , callKind = FunctionCallKind
                        }
                ui =
                    foldl
                        (flip reduceUi)
                        baseState.appUi
                        [ UiLoop TurnStarted
                        , UiLoop (ToolStarted inspectCall)
                        , UiLoop (ToolFinished (finish "inspect-1" "contents"))
                        , UiLoop (ToolStarted actionCall)
                        , UiLoop (ToolFinished (finish "action-1" "checkout"))
                        ]
                app = baseState { appUi = ui }
                size = (80, 24)
                rendered =
                    Text.unlines $
                        pictureRows
                            (renderWidget
                                (Just Theme.monochrome)
                                (drawApp app)
                                size)
                            size
            case toList ui.uiBlocks of
                [inspectBlock, actionBlock] -> do
                    rendered `shouldSatisfy`
                        Text.isInfixOf ("◇ " <> inspectBlock.blockTitle)
                    rendered `shouldSatisfy`
                        Text.isInfixOf ("◆ " <> actionBlock.blockTitle)
                _ -> expectationFailure "expected inspection and action blocks"

        it "keeps live todos to one row each so the prompt stays visible" do
            let todoCall =
                    functionToolCall
                        "todo-1"
                        "todo_write"
                        "{\"todos\":[{\"id\":\"1\",\"content\":\"Keep this list\"}]}"
                longItem =
                    "Investigate a very long remaining task that would wrap \
                    \across many columns if the panel used wrapping text"
                todoResult = ToolCallResult
                    { callId = "todo-1"
                    , output =
                        "- [completed] 1: Find and clone repos\n\
                        \- [in_progress] 2: "
                            <> longItem
                    , callKind = FunctionCallKind
                    }
                ui =
                    reduceUi
                        (UiLoop (ToolFinished todoResult))
                        (reduceUi
                            (UiLoop (ToolStarted todoCall))
                            (reduceUi (UiLoop TurnStarted) baseState.appUi))
                app = baseState { appUi = ui }
                size = (40, 16)
                rows =
                    pictureRows
                        (renderWidget
                            (Just Theme.monochrome)
                            (drawApp app)
                            size)
                        size
                rendered = Text.unlines rows
            length (filter (Text.isInfixOf "Find and clone repos") rows)
                `shouldBe` 1
            length (filter (Text.isInfixOf "Investigate a very long") rows)
                `shouldBe` 1
            rendered `shouldSatisfy` Text.isInfixOf "Thinking"
            rendered `shouldSatisfy` Text.isInfixOf "Type guidance"
            visibleTodoList ui
                `shouldSatisfy` (not . null)

        it "renders a selected child with the retained conversation renderer" do
            let target = AgentChild (SubagentId "renderer")
                call =
                    functionToolCall
                        "call-1"
                        "shell_command"
                        "{\"command\":\"printf rich-child\"}"
                conversation =
                    foldl
                        (flip reduceUi)
                        initialUiState
                        [ UiUserSubmitted "Investigate the renderer"
                        , UiLoop TurnStarted
                        , UiLoop
                            (ReasoningDelta
                                "Compare the retained block paths")
                        , UiLoop (ToolStarted call)
                        , UiLoop
                            (ToolFinished ToolCallResult
                                { callId = "call-1"
                                , output = "tool output"
                                , callKind = FunctionCallKind
                                })
                        , UiAssistantHistory
                            "## Result\n\nMarkdown **kept**."
                        , UiTurnEnded BlockComplete
                        ]
                child =
                    AgentEntry
                        { agentTarget = target
                        , agentPath = "/root/renderer"
                        , agentStatus = "done"
                        , agentModel = Just "gpt-5.6-luna"
                        , agentSteps = []
                        , agentTranscript = []
                        , agentConversation = conversation
                        }
                app =
                    baseState
                        { appAgentSelected = target
                        , appAgentEntries = [rootEntry, child]
                        }
                size = (120, 40)
                frame =
                    Text.unlines $
                        pictureRows
                            (renderWidget
                                (Just Theme.monochrome)
                                (drawApp app)
                                size)
                            size
            frame `shouldSatisfy` Text.isInfixOf "Viewing /root/renderer"
            frame `shouldSatisfy` Text.isInfixOf "Investigate the renderer"
            frame `shouldSatisfy`
                Text.isInfixOf "Compare the retained block paths"
            frame `shouldSatisfy` Text.isInfixOf "rich-child"
            frame `shouldSatisfy` Text.isInfixOf "tool output"
            frame `shouldSatisfy` Text.isInfixOf "Markdown kept."

        it "keeps variation selectors from crossing adjacent choice widgets" do
            let label = Text.replicate 33 "a" <> "✓"
                detail = Text.singleton '\xfe0f' <> "detail"
                harness =
                    applyAction
                        (ShowChoice
                            ChoiceOnboarding
                            "Sign in"
                            ""
                            [(label, detail)]
                            0)
                        (RenderHarness baseState (80, 24))
            assertFrame harness

        it "shows a selected child's live todo list above its transcript" do
            let target = AgentChild (SubagentId "reviewer")
                todoCall =
                    functionToolCall
                        "todo-1"
                        "todo_write"
                        "{\"todos\":[{\"id\":\"1\",\"content\":\"Review Model.hs\"}]}"
                todoResult = ToolCallResult
                    { callId = "todo-1"
                    , output = "- [in_progress] 1: Review Model.hs"
                    , callKind = FunctionCallKind
                    }
                conversation =
                    foldl
                        (flip reduceUi)
                        initialUiState
                        [ UiLoop TurnStarted
                        , UiLoop (ToolStarted todoCall)
                        , UiLoop (ToolFinished todoResult)
                        ]
                child =
                    AgentEntry
                        { agentTarget = target
                        , agentPath = "/root/reviewer"
                        , agentStatus = "running"
                        , agentModel = Just "gpt-5.6-luna"
                        , agentSteps = []
                        , agentTranscript = []
                        , agentConversation = conversation
                        }
                app =
                    baseState
                        { appAgentSelected = target
                        , appAgentEntries = [rootEntry, child]
                        }
                size = (120, 32)
                frame =
                    Text.unlines $
                        pictureRows
                            (renderWidget
                                (Just Theme.monochrome)
                                (drawApp app)
                                size)
                            size
            frame `shouldSatisfy` Text.isInfixOf "Viewing /root/reviewer"
            length (filter (Text.isInfixOf "Review Model.hs") (Text.lines frame))
                `shouldBe` 1
            frame `shouldNotSatisfy` Text.isInfixOf "todo_write"

renderTraceProperty :: AppState -> RenderTrace -> Property
renderTraceProperty baseState (RenderTrace actions) =
    conjoin $
        zipWith
            (\index harness ->
                counterexample
                    ("after action " <> show index <> ": "
                        <> show (actions !! (index - 1)))
                    (frameProperties "generated frame" harness))
            [1 :: Int ..]
            (traceHarnesses baseState actions)

incrementalTraceProperty :: AppState -> RenderTrace -> Property
incrementalTraceProperty baseState (RenderTrace actions) =
    ioProperty do
        let harnesses = traceHarnesses baseState actions
        (_, mockOutput) <- mockTerminal (80, 24)
        let output =
                mockOutput
                    { VOutput.outputByteBuffer = const (pure ())
                    }
        results <-
            forM (zip [1 :: Int ..] harnesses) \(index, harness) -> do
                let app = harness.harnessApp
                    size = harness.harnessSize
                    picture =
                        renderWidget
                            (Just Theme.monochrome)
                            (drawApp app)
                            size
                    cleanOps = displayOpsForPic picture size
                output.setDisplayBounds size
                context <- VOutput.displayContext output size
                VOutput.outputPicture context picture
                assumed <- readIORef output.assumedStateRef
                pure $
                    counterexample
                        ("Vty assumed-state mismatch after action "
                            <> show index)
                        (assumed.prevOutputOps === Just cleanOps)
        pure (conjoin results)

traceHarnesses :: AppState -> [TuiAction] -> [RenderHarness]
traceHarnesses baseState =
    drop 1 . scanl (flip applyAction) initialHarness
  where
    initialHarness = RenderHarness baseState (80, 24)

frameProperties :: String -> RenderHarness -> Property
frameProperties label harness =
    conjoin
        [ counterexample (label <> ": composed width")
            (V.imageWidth composed === width)
        , counterexample (label <> ": composed height")
            (V.imageHeight composed === height)
        , counterexample (label <> ": top-level layer bounds")
            (all layerFits layers === True)
        , counterexample
            (label <> ": Vty text spans match terminal widths: "
                <> show spanMismatches)
            (null spanMismatches === True)
        , counterexample (label <> ": cursor bounds")
            (cursorFits width height picture.picCursor === True)
        , counterexample (label <> ": deterministic clean redraw")
            (picture === cleanRedraw)
        , uiStateProperties app.appUi
        ]
  where
    app = harness.harnessApp
    size@(width, height) = harness.harnessSize
    widgets = drawApp app
    picture = renderWidget (Just Theme.monochrome) widgets size
    cleanRedraw = renderWidget (Just Theme.monochrome) widgets size
    composed = V.picImage picture
    layers =
        [ V.picImage $
            renderWidget (Just Theme.monochrome) [widget] size
        | widget <- widgets
        ]
    spanMismatches =
        concatMap (spanWidthMismatches . toList)
            (toList (displayOpsForPic picture size))
    layerFits image =
        V.imageWidth image <= width
            && V.imageHeight image <= height

spanWidthMismatches :: [SpanOp] -> [(Int, Int, Text)]
spanWidthMismatches =
    mapMaybe \case
        TextSpan{textSpanOutputWidth, textSpanText} ->
            let text = LazyText.toStrict textSpanText
                terminalWidth = terminalTextWidth text
            in if textSpanOutputWidth == terminalWidth
                then Nothing
                else Just
                    (textSpanOutputWidth, terminalWidth, text)
        Skip _ -> Nothing
        RowEnd _ -> Nothing

uiStateProperties :: UiState -> Property
uiStateProperties state =
    conjoin
        [ counterexample "block IDs are unique"
            (length blockIds === length (nub blockIds))
        , counterexample "block index map matches retained order"
            (state.uiBlockIndices === expectedIndices)
        , counterexample "next block ID is after every retained block"
            ((state.uiNextBlockId > maximum (0 : blockIds)) === True)
        , counterexample "draft cursor is in bounds"
            ((state.uiCursor >= 0
                && state.uiCursor <= Text.length state.uiDraft)
                === True)
        , counterexample "selected block ID and index agree"
            (selectionIsValid state === True)
        , counterexample "permission selection is in bounds"
            ((maybe True
                (\permission ->
                    permission.permissionIndex >= 0
                        && permission.permissionIndex < 4)
                state.uiPermission)
                === True)
        , counterexample "retry countdown is non-negative"
            ((maybe True
                ((>= 0) . (.retryCountdownRemainingMillis))
                state.uiRetryCountdown)
                === True)
        ]
  where
    blocks = toList state.uiBlocks
    blockIds = map (blockNumber . (.blockId)) blocks
    expectedIndices =
        Map.fromList
            [ (block.blockId, index)
            | (index, block) <- zip [0 :: Int ..] blocks
            ]

selectionIsValid :: UiState -> Bool
selectionIsValid state =
    case (state.uiSelectedBlock, state.uiSelectedBlockIndex) of
        (Nothing, Nothing) -> True
        (Just ident, Just index) ->
            index >= 0
                && index < Seq.length state.uiBlocks
                && maybe False ((== ident) . (.blockId))
                    (Seq.lookup index state.uiBlocks)
                && Map.lookup ident state.uiBlockIndices == Just index
        _ -> False

blockNumber :: BlockId -> Int
blockNumber (BlockId number) = number

cursorFits :: Int -> Int -> V.Cursor -> Bool
cursorFits width height = \case
    V.NoCursor -> True
    V.PositionOnly _ column row -> inBounds column row
    V.Cursor column row -> inBounds column row
    V.AbsoluteCursor column row -> inBounds column row
  where
    inBounds column row =
        column >= 0
            && column < width
            && row >= 0
            && row < height

pictureRows :: V.Picture -> (Int, Int) -> [Text]
pictureRows picture size =
    map
        (Text.concat . map spanText . toList)
        (toList (displayOpsForPic picture size))
  where
    spanText = \case
        TextSpan _ _ _ text -> LazyText.toStrict text
        Skip width -> Text.replicate width " "
        RowEnd width -> Text.replicate width " "

applyAction :: TuiAction -> RenderHarness -> RenderHarness
applyAction action harness =
    case action of
        Resize width height ->
            harness
                { harnessSize =
                    (max 1 width, max 1 height)
                }
        Submit text -> applyUi (UiUserSubmitted text)
        Assistant text -> applyUi (UiAssistantHistory text)
        SystemMessage text -> applyUi (UiSystemMessage text)
        ErrorMessage text -> applyUi (UiErrorMessage text)
        SetDraft text cursor -> applyUi (UiSetDraft text cursor)
        QueueInput text -> applyUi (UiInputQueued text)
        StartQueuedInput -> applyUi UiQueuedInputStarted
        SetRepository branch cwd workspace ->
            applyUi (UiSetRepository branch cwd workspace)
        SetNotice text -> applyUi (UiSetNotice (Just (warningNotice text)))
        ClearNotice -> applyUi (UiSetNotice Nothing)
        MoveSelection amount -> applyUi (UiMoveSelection amount)
        ToggleSelected -> applyUi UiToggleSelected
        SetFocus focus -> applyUi (UiFocusChanged focus)
        ShowPermission summary -> applyUi (UiPermissionShown summary)
        MovePermission amount -> applyUi (UiPermissionMoved amount)
        HidePermission -> applyUi UiPermissionHidden
        SetRetryCountdown prefix remaining suffix ->
            applyUi (UiRetryCountdown prefix remaining suffix)
        SetFollow follow -> applyUi (UiSetFollow follow)
        EndTurn blockState -> applyUi (UiTurnEnded blockState)
        RestartTurn -> applyUi UiTurnRestarted
        ClearConversation -> applyUi UiConversationCleared
        ShowChoice presentation title body rows rawIndex ->
            let index = rawIndex `mod` length rows
            in harness
                { harnessApp =
                    harness.harnessApp
                        { appChoice =
                            Just ChoiceOverlay
                                { choicePresentation = presentation
                                , choiceTitle = title
                                , choiceBody = body
                                , choiceIndex = index
                                , choiceRows = rows
                                , choiceSearch = False
                                , choiceQuery = ""
                                , choiceAdjustments = Nothing
                                , choiceAdjustmentIndices = []
                                , choiceCloseOnTurnEnd = False
                                }
                        , appTextPrompt = Nothing
                        }
                }
        ShowText mode title body draft rawCursor ->
            harness
                { harnessApp =
                    harness.harnessApp
                        { appChoice = Nothing
                        , appTextPrompt =
                            Just TextOverlay
                                { textTitle = title
                                , textBody = body
                                , textDraft = draft
                                , textCursor =
                                    max 0
                                        (min (Text.length draft) rawCursor)
                                , textInputMode = mode
                                }
                        }
                }
        CloseAppOverlay ->
            harness
                { harnessApp =
                    harness.harnessApp
                        { appChoice = Nothing
                        , appTextPrompt = Nothing
                        }
                }
        SetAgents rawCount transcript ->
            let count = max 0 (min 8 rawCount)
                children = map (childEntry transcript) [1 .. count]
                selected = case reverse children of
                    [] -> AgentRoot
                    child : _ -> child.agentTarget
            in harness
                { harnessApp =
                    harness.harnessApp
                        { appAgentEntries = rootEntry : children
                        , appAgentSelected = selected
                        }
                }
  where
    applyUi event =
        let app = harness.harnessApp
            previous = app.appUi
            next =
                timestampNewMessageBlocks
                    (Seq.length previous.uiBlocks)
                    "11:58 PM"
                    (reduceUi event previous)
        in harness
            { harnessApp = app { appUi = next }
            }

rootEntry :: AgentEntry
rootEntry = AgentEntry
    { agentTarget = AgentRoot
    , agentPath = "/root"
    , agentStatus = "running"
    , agentModel = Nothing
    , agentSteps = []
    , agentTranscript = []
    , agentConversation = initialUiState
    }

childEntry :: Text -> Int -> AgentEntry
childEntry transcript index = AgentEntry
    { agentTarget = AgentChild (SubagentId ("agent-" <> Text.pack (show index)))
    , agentPath = "/root/task_" <> Text.pack (show index)
    , agentStatus = if even index then "running" else "done"
    , agentModel = Just "gpt-5.6-luna"
    , agentSteps = []
    , agentTranscript =
        [ "user: " <> transcript
        , "assistant: " <> Text.reverse transcript
        ]
    , agentConversation = initialUiState
    }

makeBaseState :: IO AppState
makeBaseState = do
    input <- newFullscreenInputBuffer
    runtime <-
        newFullscreenRuntimeWithSyntaxLoader
            (pure (Left "disabled in renderer property tests"))
            input
            (pure ())
            (const (pure ()))
            (pure WarnExit)
            (const (pure True))
            (const (pure ()))
            (const (pure ()))
            (pure (AgentRoot, [rootEntry]))
            (const (pure ()))
            (pure ())
            (const (pure ()))
            MotionOff
            False
            initialUiState
    pure $
        initialFullscreenAppState
            runtime
            []
            AgentRoot
            [rootEntry]
            0

instance Arbitrary RenderTrace where
    arbitrary = sized \size -> do
        count <- chooseInt (1, max 1 (min 30 (size + 1)))
        RenderTrace <$> vectorOf count genAction

    shrink (RenderTrace actions) =
        RenderTrace <$> shrinkList (const []) actions

genAction :: Gen TuiAction
genAction =
    frequency
        [ (8, uncurry Resize <$> genTerminalSize)
        , (7, Submit <$> genBodyText)
        , (7, Assistant <$> genBodyText)
        , (3, SystemMessage <$> genBodyText)
        , (3, ErrorMessage <$> genBodyText)
        , (7, SetDraft <$> genBodyText <*> genCursor)
        , (3, QueueInput <$> genBodyText)
        , (1, pure StartQueuedInput)
        , (2, SetRepository <$> genLineText <*> genLineText <*> genLineText)
        , (2, SetNotice <$> genLineText)
        , (1, pure ClearNotice)
        , (2, MoveSelection <$> chooseInt (-20, 20))
        , (1, pure ToggleSelected)
        , (2, SetFocus <$> elements [FocusComposer, FocusScrollback])
        , (2, ShowPermission <$> genLineText)
        , (1, MovePermission <$> chooseInt (-20, 20))
        , (1, pure HidePermission)
        , (2, SetRetryCountdown
                <$> genLineText
                <*> chooseInt (-1000, 100000)
                <*> genLineText)
        , (1, SetFollow <$> arbitrary)
        , (1, EndTurn <$> elements
                [ BlockComplete
                , BlockFailed
                , BlockCancelled
                , BlockDenied
                ])
        , (1, pure RestartTurn)
        , (2, pure ClearConversation)
        , (3, ShowChoice
                <$> elements [ChoiceDialog, ChoiceOnboarding]
                <*> genLineText
                <*> genBodyText
                <*> genChoiceRows
                <*> genCursor)
        , (3, ShowText
                <$> elements [TextInputPlain, TextInputSecret]
                <*> genLineText
                <*> genBodyText
                <*> genBodyText
                <*> genCursor)
        , (2, pure CloseAppOverlay)
        , (3, SetAgents <$> chooseInt (0, 8) <*> genBodyText)
        ]

genTerminalSize :: Gen (Int, Int)
genTerminalSize =
    frequency
        [ (5, elements
            [ (1, 1)
            , (2, 3)
            , (9, 10)
            , (39, 9)
            , (40, 10)
            , (41, 11)
            , (71, 24)
            , (72, 10)
            , (72, 24)
            , (73, 25)
            , (79, 23)
            , (80, 24)
            , (81, 25)
            , (120, 40)
            ])
        , (1, (,) <$> chooseInt (1, 180) <*> chooseInt (1, 60))
        ]

genChoiceRows :: Gen [(Text, Text)]
genChoiceRows = do
    count <- chooseInt (1, 6)
    vectorOf count ((,) <$> genLineText <*> genLineText)

genCursor :: Gen Int
genCursor = chooseInt (-20, 240)

genLineText :: Gen Text
genLineText = Text.filter (/= '\n') <$> genBodyText

genBodyText :: Gen Text
genBodyText = do
    length' <-
        frequency
            [ (4, elements
                [ 0, 1, 2, 8, 9, 10
                , 39, 40, 41
                , 71, 72, 73
                , 79, 80, 81
                , 120, 200
                ])
            , (1, chooseInt (0, 220))
            ]
    Text.pack <$> vectorOf length' genDisplayChar

genDisplayChar :: Gen Char
genDisplayChar =
    frequency
        [ (20, elements ['a' .. 'z'])
        , (5, elements ['0' .. '9'])
        , (5, elements [' ', ' ', ' ', '\n'])
        , (2, elements ['`', '*', '_', '#', '|', '[', ']', '(', ')'])
        , (3, elements ['漢', '界', '語'])
        , (2, elements ['é', 'ø', 'ß'])
        , (2, elements ['🙂', '🚀', '✓', '✕'])
        , (1, elements ['\x0301', '\xFE0F'])
        ]

assertFrame :: RenderHarness -> Expectation
assertFrame harness = do
    V.imageWidth composed `shouldBe` width
    V.imageHeight composed `shouldBe` height
    mapM_
        (\image -> do
            V.imageWidth image `shouldSatisfy` (<= width)
            V.imageHeight image `shouldSatisfy` (<= height))
        layers
    concatMap (spanWidthMismatches . toList)
        (toList (displayOpsForPic picture size))
        `shouldBe` []
    cursorFits width height picture.picCursor `shouldBe` True
  where
    app = harness.harnessApp
    size@(width, height) = harness.harnessSize
    widgets = drawApp app
    picture = renderWidget (Just Theme.monochrome) widgets size
    composed = V.picImage picture
    layers =
        [ V.picImage $
            renderWidget (Just Theme.monochrome) [widget] size
        | widget <- widgets
        ]
