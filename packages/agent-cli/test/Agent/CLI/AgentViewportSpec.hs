module Agent.CLI.AgentViewportSpec (spec) where

import Agent.CLI.AgentViewport
import Agent.Json (rawJsonFromEncoding)
import Agent.CLI.Picker (PickerKey(..))
import Agent.Responses.Types
import Agent.Subagents (SubagentId(..), SubagentStatus(..))
import Agent.Tools.TaskPlan
    ( CurrentTaskPlan(..)
    , TaskPlan(..)
    , TaskPlanItem(..)
    , TaskPlanStatus(..)
    , taskPlanContextText
    )
import Agent.TUI.Model
    ( BlockKind(..)
    , BlockState(..)
    , UiBlock(..)
    , UiEvent(..)
    , UiState(..)
    , initialUiState
    , reduceUi
    )
import Agent.Loop (LoopEvent(..))
import Agent.ToolDispatch
    ( ToolCallKind(..)
    , ToolCallResult(..)
    , functionToolCall
    )
import qualified Data.Aeson as Aeson
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = do
    describe "renderAgentTree" do
        it "stays hidden until a child agent exists" do
            renderAgentTree False AgentRoot [rootEntry] `shouldBe` ""

        it "renders a deterministic filesystem hierarchy" do
            let entries =
                    [ child "beta" "/root/beta" "running"
                    , child "gamma" "/root/alpha/gamma" "done"
                    , rootEntry
                    , child "alpha" "/root/alpha" "running"
                    ]
            renderAgentTree False (AgentChild (SubagentId "gamma")) entries
                `shouldBe`
                    Text.intercalate "\n"
                        [ "agents"
                        , "  ▾ root  ● active"
                        , "  ├─ alpha  ● running · gpt-5.6-luna"
                        , "› │  └─ gamma  ✓ done · gpt-5.6-luna"
                        , "  └─ beta  ● running · gpt-5.6-luna"
                        , "  viewing /root/alpha/gamma · /agents to switch"
                        ]

        it "computes nested sibling guides across the hierarchy" do
            let entries =
                    [ rootEntry
                    , child "alpha" "/root/alpha" "running"
                    , child "one" "/root/alpha/one" "running"
                    , child "deep" "/root/alpha/one/deep" "done"
                    , child "two" "/root/alpha/two" "done"
                    , child "beta" "/root/beta" "running"
                    , child "leaf" "/root/beta/leaf" "done"
                    ]
            renderAgentTree False AgentRoot entries
                `shouldBe`
                    Text.intercalate "\n"
                        [ "agents"
                        , "› ▾ root  ● active"
                        , "  ├─ alpha  ● running · gpt-5.6-luna"
                        , "  │  ├─ one  ● running · gpt-5.6-luna"
                        , "  │  │  └─ deep  ✓ done · gpt-5.6-luna"
                        , "  │  └─ two  ✓ done · gpt-5.6-luna"
                        , "  └─ beta  ● running · gpt-5.6-luna"
                        , "     └─ leaf  ✓ done · gpt-5.6-luna"
                        , "  viewing /root · /agents to switch"
                        ]

        it "uses the model instead of redundant status text in narrow panes" do
            let entries = [rootEntry, child "alpha" "/root/alpha" "running"]
            agentEntryTreeLabelWithGlyphModel "●" entries 1 (entries !! 1)
                `shouldBe` "└─ alpha  ● gpt-5.6-luna"

    describe "agent viewport selection" do
        let entries =
                [ rootEntry
                , child "alpha" "/root/alpha" "running"
                , child "beta" "/root/beta" "done"
                ]
            state = initialAgentViewportState AgentRoot entries

        it "wraps through agents with arrow keys" do
            up <- rightState (applyAgentViewportKey PickerKeyUp state)
            up.viewportIndex `shouldBe` 2
            down <- rightState (applyAgentViewportKey PickerKeyDown up)
            down.viewportIndex `shouldBe` 0

        it "confirms the currently previewed agent and preserves cancel" do
            moved <- rightState (applyAgentViewportKey PickerKeyDown state)
            applyAgentViewportKey PickerKeyConfirm moved
                `shouldBe` Left (Just (AgentChild (SubagentId "alpha")))
            applyAgentViewportKey PickerKeyCancel moved
                `shouldBe` Left Nothing

        it "selects clicked targets and preserves them across live refreshes" do
            let selected =
                    selectAgentTarget
                        (AgentChild (SubagentId "alpha"))
                        state
                refreshed =
                    refreshAgentViewportState
                        [ rootEntry
                        , child "alpha" "/root/alpha" "done"
                        , child "gamma" "/root/alpha/gamma" "running"
                        ]
                        selected
            (.agentTarget) <$> selectedAgentEntry refreshed
                `shouldBe` Just (AgentChild (SubagentId "alpha"))
            (.agentStatus) <$> selectedAgentEntry refreshed
                `shouldBe` Just "done"

        it "selects provider-native agents without changing host child identity" do
            let native =
                    AgentEntry
                        { agentTarget = AgentNative "toolu-native"
                        , agentPath = "/native/explore"
                        , agentStatus = "running"
                        , agentModel = Just "claude-sonnet"
                        , agentSteps = []
                        , agentTranscript = ["assistant: inspecting"]
                        , agentConversation = initialUiState
                        }
                mixed = entries <> [native]
                selected =
                    selectAgentTarget (AgentNative "toolu-native")
                        (initialAgentViewportState AgentRoot mixed)
            (.agentTarget) <$> selectedAgentEntry selected
                `shouldBe` Just (AgentNative "toolu-native")
            lookupAgentEntry
                (AgentChild (SubagentId "alpha"))
                mixed
                `shouldSatisfy` maybe False ((== "/root/alpha") . (.agentPath))

        it "renders provider-native agents in the shared hierarchy" do
            let native =
                    AgentEntry
                        { agentTarget = AgentNative "toolu-native"
                        , agentPath = "/native/explore"
                        , agentStatus = "cancelled"
                        , agentModel = Just "claude-sonnet"
                        , agentSteps = []
                        , agentTranscript = ["assistant: partial result"]
                        , agentConversation = initialUiState
                        }
                rendered =
                    renderAgentTree
                        False
                        (AgentNative "toolu-native")
                        (entries <> [native])
            rendered `shouldSatisfy` Text.isInfixOf "explore  ■ cancelled"
            rendered `shouldSatisfy`
                Text.isInfixOf "viewing /native/explore"

        it "renders hierarchy and transcript panes" do
            let frame = renderAgentViewportFrameFor False 10 70 state
            frame `shouldSatisfy` Text.isInfixOf "hierarchy"
            frame `shouldSatisfy` Text.isInfixOf "transcript · /root"
            frame `shouldSatisfy` Text.isInfixOf "assistant: ready"

        it "keeps the selected child transcript visible after the picker" do
            let selected = AgentChild (SubagentId "alpha")
                panel = renderAgentViewportPanelFor False 70 selected entries
            panel `shouldSatisfy` Text.isInfixOf "transcript · /root/alpha"
            panel `shouldSatisfy` Text.isInfixOf "assistant: working"
            panel `shouldSatisfy` Text.isInfixOf "input routes to /root"

        it "shows a selected child's live todos in the transcript pane" do
            let todoCall =
                    functionToolCall
                        "todo-1"
                        "todo_write"
                        "{\"todos\":[{\"id\":\"1\",\"content\":\"Review Model.hs\"}]}"
                conversation =
                    foldl
                        (flip reduceUi)
                        initialUiState
                        [ UiLoop TurnStarted
                        , UiLoop (ToolStarted todoCall)
                        , UiLoop
                            (ToolFinished ToolCallResult
                                { callId = "todo-1"
                                , output = "- [in_progress] 1: Review Model.hs"
                                , callKind = FunctionCallKind
                                })
                        ]
                selected = AgentChild (SubagentId "alpha")
                withTodos =
                    map
                        (\entry ->
                            if entry.agentTarget == selected
                                then
                                    entry
                                        { agentConversation = conversation
                                        , agentTranscript =
                                            replicate 8 "assistant: filler"
                                                <> ["assistant: working"]
                                        }
                                else entry)
                        entries
                panel = renderAgentViewportPanelFor False 70 selected withTodos
            panel `shouldSatisfy` Text.isInfixOf "Review Model.hs"
            panel `shouldSatisfy` Text.isInfixOf "transcript · /root/alpha"
            panel `shouldSatisfy` Text.isInfixOf "assistant: working"

    describe "formatAgentStatus" do
        it "uses compact status labels" do
            map formatAgentStatus
                [ Pending
                , Running
                , Completed (Just "ok")
                , Errored "boom"
                , Interrupted
                ]
                `shouldBe` ["pending", "running", "done", "error", "interrupted"]

    describe "responseItemPreviewLines" do
        it "keeps the first line and only the requested transcript tail" do
            let items =
                    [ messageItem RoleUser "request"
                    , messageItem RoleAssistant "one\ntwo\nthree\nfour"
                    ]
            responseItemPreviewLines 2 items
                `shouldBe`
                    [ "user: request"
                    , "           three"
                    , "           four"
                    ]

        it "uses only the first line when no transcript tail is requested" do
            responseItemPreviewLines 0
                [ messageItem RoleUser "request"
                , messageItem RoleAssistant "answer"
                ]
                `shouldBe` ["user: request"]

        it "hides generated task-plan context but keeps ordinary developer text" do
            let context =
                    taskPlanContextText $
                        CurrentTaskPlan 2 $
                            TaskPlan Nothing
                                [TaskPlanItem "implement" TaskPlanInProgress]
                items =
                    [ messageItem RoleDeveloper context
                    , messageItem RoleUser context
                    , messageItem RoleDeveloper "ordinary instruction"
                    ]
            responseItemLines items
                `shouldBe` ["developer: ordinary instruction"]
            responseItemPreviewLines 4 items
                `shouldBe` ["developer: ordinary instruction"]

    describe "responseItemsToUiState" do
        it "replays child messages, reasoning, and tools into retained blocks" do
            let ui =
                    responseItemsToUiState False
                        [ agentMessageItem "Investigate the renderer"
                        , reasoningItem "Compare both paths" "private detail"
                        , functionCallItem
                            "call-1"
                            "shell_command"
                            "{\"command\":\"printf done\"}"
                            (Just ItemCompleted)
                        , functionOutputItem
                            "call-1"
                            (Just ItemCompleted)
                        , messageItem
                            RoleAssistant
                            "Finished with **Markdown** intact."
                        ]
                blocks = toList ui.uiBlocks
            map (.blockKind) blocks
                `shouldBe`
                    [ BlockUser
                    , BlockThinking
                    , BlockShell
                    , BlockAssistant
                    ]
            map (.blockState) blocks
                `shouldBe`
                    [ BlockComplete
                    , BlockComplete
                    , BlockComplete
                    , BlockComplete
                    ]
            map (.blockBody) blocks
                `shouldBe`
                    [ "Investigate the renderer"
                    , "Compare both paths"
                    , "ok"
                    , "Finished with **Markdown** intact."
                    ]

        it "shows raw reasoning only when explicitly enabled" do
            let items = [reasoningItem "Visible summary" "private detail"]
                body visible =
                    (.blockBody)
                        <$> atMay
                            (toList
                                (responseItemsToUiState visible items).uiBlocks)
                            0
            body False `shouldBe` Just "Visible summary"
            body True
                `shouldBe` Just "Visible summary\nprivate detail"

        it "shows workspace-relative tool titles when hydrating a child transcript" do
            let workspace = "/Users/marc/.haskell-agent/worktrees/haskell-agent/wt"
                ui =
                    responseItemsToUiStateRelative False workspace
                        [ functionCallItem
                            "call-1"
                            "read_file"
                            ("{\"target_file\":\""
                                <> workspace
                                <> "/nix/modules/telegram.nix\"}")
                            (Just ItemCompleted)
                        ]
            map (.blockTitle) (toList ui.uiBlocks)
                `shouldBe` ["Read"]
            map (.blockDetail) (toList ui.uiBlocks)
                `shouldBe` ["nix/modules/telegram.nix"]
            map (.agentStepTitle)
                (responseItemStepPreviewsRelative workspace 1
                    [ functionCallItem
                        "call-1"
                        "search_replace"
                        ("{\"file_path\":\""
                            <> workspace
                            <> "/nix/modules/telegram.nix\"}")
                        (Just ItemCompleted)
                    ])
                `shouldBe` ["Edited nix/modules/telegram.nix"]

        it "keeps a hydrated background shell running across reasoning items" do
            let ui =
                    responseItemsToUiState False
                        [ functionCallItem
                            "shell-1"
                            "shell_command"
                            "{\"command\":\"slow\"}"
                            (Just ItemCompleted)
                        , functionOutputText
                            "shell-1"
                            "Process still running.\nsession_id: 6\nfirst\n"
                        , reasoningItem "Waiting for the command" "private detail"
                        ]
                blocks = toList ui.uiBlocks
            map (.blockState) blocks
                `shouldBe` [BlockRunning, BlockComplete]
            ui.uiShellProcesses `shouldNotBe` mempty

    describe "responseItemStepPreviews" do
        it "coalesces tool calls with their outputs and returns newest first" do
            let steps =
                    responseItemStepPreviews 2
                        [ messageItem RoleUser "fix it"
                        , functionCallItem
                            "call-1"
                            "shell_command"
                            "{\"command\":\"cabal test\"}"
                            Nothing
                        , functionOutputItem
                            "call-1"
                            (Just ItemCompleted)
                        , messageItem
                            RoleAssistant
                            "Updated the retry policy\nFocused tests pass"
                        ]
            case steps of
                [latest, tool] -> do
                    map (.agentStepState) steps
                        `shouldBe` [AgentStepCompleted, AgentStepCompleted]
                    latest.agentStepTitle
                        `shouldBe` "Updated the retry policy"
                    latest.agentStepDetail
                        `shouldBe` Just "Focused tests pass"
                    tool.agentStepTitle
                        `shouldSatisfy` Text.isInfixOf "cabal test"
                _ -> expectationFailure
                    ("expected exactly two semantic steps, got " <> show steps)

        it "keeps a call running until its matching output arrives" do
            let preview status =
                    responseItemStepPreviews 1
                        [ functionCallItem
                            "call-1"
                            "shell_command"
                            "{\"command\":\"sleep 5\"}"
                            (Just status)
                        ]
            map
                (map (.agentStepState) . preview)
                [ItemInProgress, ItemCompleted]
                `shouldBe`
                    [ [AgentStepRunning]
                    , [AgentStepRunning]
                    ]

        it "treats production tool outputs without status as neutral" do
            responseItemStepPreviews 1
                [ functionCallItem
                    "call-1"
                    "shell_command"
                    "{\"command\":\"false\"}"
                    (Just ItemCompleted)
                , functionOutputItem "call-1" Nothing
                ]
                `shouldSatisfy`
                    \case
                        [step] ->
                            step.agentStepState == AgentStepInfo
                                && step.agentStepDetail == Just "finished"
                        _ -> False

        it "keeps the newest status when a call has multiple outputs" do
            responseItemStepPreviews 1
                [ functionCallItem
                    "call-1"
                    "shell_command"
                    "{\"command\":\"cabal test\"}"
                    (Just ItemCompleted)
                , functionOutputItem
                    "call-1"
                    (Just ItemIncomplete)
                , functionOutputItem
                    "call-1"
                    (Just ItemCompleted)
                ]
                `shouldSatisfy`
                    \case
                        [step] ->
                            step.agentStepState == AgentStepCompleted
                        _ -> False

        it "adds live and terminal lifecycle steps for subagents" do
            agentStepsForStatus 2 Running []
                `shouldBe`
                    [AgentStep AgentStepRunning "Working…" Nothing]
            agentStepsForStatus 2 Running
                [ functionCallItem
                    "call-1"
                    "shell_command"
                    "{\"command\":\"sleep 5\"}"
                    (Just ItemCompleted)
                ]
                `shouldSatisfy`
                    \case
                        step : _ ->
                            step.agentStepState == AgentStepRunning
                                && "sleep 5"
                                    `Text.isInfixOf` step.agentStepTitle
                        _ -> False
            agentStepsForStatus 2 (Errored "websocket disconnected") []
                `shouldBe`
                    [ AgentStep
                        AgentStepFailed
                        "Agent failed"
                        (Just "websocket disconnected")
                    ]

rootEntry :: AgentEntry
rootEntry =
    AgentEntry
        { agentTarget = AgentRoot
        , agentPath = "/root"
        , agentStatus = "active"
        , agentModel = Nothing
        , agentSteps = []
        , agentTranscript = ["user: hello", "assistant: ready"]
        , agentConversation = initialUiState
        }

child :: Text -> Text -> Text -> AgentEntry
child agentId path status =
    AgentEntry
        { agentTarget = AgentChild (SubagentId agentId)
        , agentPath = path
        , agentStatus = status
        , agentModel = Just "gpt-5.6-luna"
        , agentSteps = []
        , agentTranscript = ["assistant: working"]
        , agentConversation = initialUiState
        }

messageItem :: ResponseRole -> Text -> ResponseItem
messageItem role text = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentText text
    , role
    , status = Nothing
    , phase = Nothing
    , passthrough = Nothing
    }

functionCallItem
    :: Text
    -> Text
    -> Text
    -> Maybe ItemStatus
    -> ResponseItem
functionCallItem callId name arguments status =
    FunctionCallItem FunctionCall
        { itemId = Nothing
        , callId
        , name
        , namespace = Nothing
        , provider = Nothing
        , arguments
        , encryptedFunctionArgs = Nothing
        , status
        , async = Nothing
        }

functionOutputItem :: Text -> Maybe ItemStatus -> ResponseItem
functionOutputItem callId status =
    FunctionCallOutputItem FunctionCallOutput
        { itemId = Nothing
        , callId
        , name = Nothing
        , namespace = Nothing
        , provider = Nothing
        , output = rawJsonFromEncoding (Aeson.toEncoding ("ok" :: Text))
        , status
        , async = Nothing
        }

functionOutputText :: Text -> Text -> ResponseItem
functionOutputText callId output =
    FunctionCallOutputItem FunctionCallOutput
        { itemId = Nothing
        , callId
        , name = Nothing
        , namespace = Nothing
        , provider = Nothing
        , output = rawJsonFromEncoding (Aeson.toEncoding output)
        , status = Just ItemCompleted
        , async = Nothing
        }

agentMessageItem :: Text -> ResponseItem
agentMessageItem text =
    AgentMessageItem ResponseAgentMessage
        { messageId = Nothing
        , author = Nothing
        , recipient = Nothing
        , content = [InputTextPart text Nothing]
        , passthrough = Nothing
        }

reasoningItem :: Text -> Text -> ResponseItem
reasoningItem summary raw =
    ReasoningItemValue ReasoningItem
        { itemId = Nothing
        , summary =
            [ ReasoningSummaryPart
                { partType = "summary_text"
                , text = Just summary
                }
            ]
        , content =
            Just
                [ ReasoningTextPart
                    { text = raw
                    }
                ]
        , encryptedContent = Nothing
        , status = Just ItemCompleted
        }

atMay :: [a] -> Int -> Maybe a
atMay values index
    | index < 0 = Nothing
    | otherwise =
        case drop index values of
            value : _ -> Just value
            [] -> Nothing

rightState :: Either (Maybe AgentTarget) AgentViewportState -> IO AgentViewportState
rightState result = case result of
    Right state -> pure state
    Left selected -> fail ("expected viewport state, got " <> show selected)
