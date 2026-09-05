module Agent.CLI.RenderSpec (spec) where

import Agent.CLI.Input (terminalTextWidth)
import Agent.CLI.Render
import Agent.CLI.Style
    ( glyphInspect
    , glyphTool
    , motionGlyphSet
    , roleInspectName
    , roleToolArrow
    , roleToolDetail
    , roleToolName
    , roleToolPath
    )
import Agent.Error (ApiError(..), ErrorType(..), credentialsExhausted)
import Agent.Loop
    ( LoopError(..)
    , LoopEvent(..)
    , TokenUsage(..)
    , TurnCompletion(..)
    , TurnOutput(..)
    , emptyTokenUsage
    , emptyTurnOutput
    )
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , ToolCallResult(..)
    , customToolCall
    , functionToolCall
    )
import Agent.TextBuffer
    ( emptyTextBuffer
    , textBufferToText
    )
import Agent.Telemetry (TurnTelemetry(..))
import Agent.TUI.Motion (MotionMode(..), foregroundIndicator)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (asyncThreadId, poll)
import Control.Concurrent.MVar (newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Exception (finally)
import Control.Monad (forM_)
import Data.IORef (newIORef, readIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Calendar (fromGregorian)
import Data.Maybe (fromMaybe, isJust)
import Data.Time.Clock (UTCTime(..), addUTCTime, diffUTCTime, getCurrentTime)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (BufferMode(..), Handle, hClose, hSetBuffering, openTempFile)
import Test.Hspec

spec :: Spec
spec = do
    describe "RenderState transitions" do
        it "starts a fresh turn while retaining immutable defaults" do
            let started =
                    beginRenderTurn
                        (UTCTime (fromGregorian 2026 1 2) 0)
                        emptyRenderState
            stateStartedAt started `shouldSatisfy` (/= Nothing)
            stateActivity started `shouldBe` "Thinking…"
            stateToolCalls started `shouldBe` mempty

        it "preserves an already-running thinking spinner across tool rounds" do
            let visible =
                    emptyRenderState
                        { stateThinkingVisible = True
                        , statePrintedText = True
                        }
                started =
                    beginRenderTurn
                        (UTCTime (fromGregorian 2026 1 2) 0)
                        visible
            stateThinkingVisible started `shouldBe` True
            statePrintedText started `shouldBe` False

        it "preserves last tok/s across a new generation" do
            let started =
                    beginRenderTurn
                        (UTCTime (fromGregorian 2026 1 2) 0)
                        emptyRenderState
                            { stateLastTokensPerSecond = Just 42 }
            stateLastTokensPerSecond started `shouldBe` Just 42

        it "records provider output tokens per second" do
            let startedAt = UTCTime (fromGregorian 2026 1 2) 0
                started = beginRenderTurn startedAt emptyRenderState
                turn =
                    (emptyTurnOutput "r1" [] (Just "hi"))
                        { tokenUsage =
                            TokenUsage
                                { inputTokens = 10
                                , outputTokens = 80
                                , cachedTokens = 0
                                }
                        }
                recorded =
                    recordRenderTurnRate (addUTCTime 2 startedAt) turn started
            stateLastTokensPerSecond recorded `shouldBe` Just 40

        it "does not estimate a completed rate without provider usage" do
            let startedAt = UTCTime (fromGregorian 2026 1 2) 0
                started =
                    countGenerationChars "abcdefghijklmnop" $
                        beginRenderTurn startedAt $
                            emptyRenderState
                                { stateLastTokensPerSecond = Just 42 }
                turn = emptyTurnOutput "r1" [] (Just "abcdefghijklmnop")
                recorded =
                    recordRenderTurnRate (addUTCTime 2 startedAt) turn started
            stateLastTokensPerSecond recorded `shouldBe` Nothing

        it "keeps the turn timer when a generation restarts" do
            let startedAt = UTCTime (fromGregorian 2026 1 2) 0
                started = beginRenderTurn startedAt $
                    emptyRenderState
                        { stateGenerationChars = 16
                        , stateLastTokensPerSecond = Just 12
                        }
                restartedAt = addUTCTime 3 startedAt
                restarted = resetRenderGeneration restartedAt started
                turn =
                    (emptyTurnOutput "r1" [] (Just "hi"))
                        { tokenUsage =
                            TokenUsage
                                { inputTokens = 10
                                , outputTokens = 80
                                , cachedTokens = 0
                                }
                        }
                recorded =
                    recordRenderTurnRate (addUTCTime 2 restartedAt) turn restarted
            stateStartedAt restarted `shouldBe` stateStartedAt started
            restarted.stateGenerationChars `shouldBe` 0
            restarted.stateGenerationStartedAt `shouldBe` Just restartedAt
            stateLastTokensPerSecond recorded `shouldBe` Just 40

        it "drops a previous conversation's saved rate" do
            let started =
                    beginRenderTurn
                        (UTCTime (fromGregorian 2026 1 2) 0)
                        emptyRenderState
                            { stateLastTokensPerSecond = Just 42
                            , stateGenerationChars = 16
                            }
                cleared = clearRenderTokenRate started
            stateLastTokensPerSecond cleared `shouldBe` Nothing
            cleared.stateGenerationChars `shouldBe` 0
            cleared.stateGenerationStartedAt `shouldBe` Nothing
            stateStartedAt cleared `shouldBe` stateStartedAt started

        it "keeps spinner elapsed time across ResponseRestarted" do
            withRenderConfig True False \config _handle _path -> do
                renderEvent config TurnStarted
                before <- readIORef config.renderState
                threadDelay 1_100_000
                renderEvent config (ResponseRestarted "retry")
                after <- readIORef config.renderState
                now <- getCurrentTime
                let started = fromMaybe now after.stateStartedAt
                    elapsed = realToFrac (diffUTCTime now started) :: Double
                stateStartedAt after `shouldBe` stateStartedAt before
                after.stateGenerationStartedAt
                    `shouldNotBe` after.stateStartedAt
                elapsed `shouldSatisfy` (>= 1.0)

        it "streams markdown through a pure state transition" do
            let (state1, first) = streamMarkdown "hello\n" emptyRenderState
                (state2, second) = streamMarkdown "world\n" state1
            statePrintedText state2 `shouldBe` False
            first <> second `shouldSatisfy` Text.isInfixOf "hello"
            first <> second `shouldSatisfy` Text.isInfixOf "world"

        it "streams tables without optional outer pipes" do
            let input =
                    "Name | Footprint\n\
                    \--- | ---:\n\
                    \WebKit | 27.7 GiB\n\
                    \Control Center | 4.6 GiB\n\
                    \after\n"
                (_state, output) = streamMarkdown input emptyRenderState
            output `shouldSatisfy` Text.isInfixOf "Name"
            output `shouldSatisfy` Text.isInfixOf "Control Center"
            output `shouldSatisfy` Text.isInfixOf "─"
            output `shouldSatisfy` (not . Text.isInfixOf "|")

        it "constrains streamed tables to the terminal width" do
            let input =
                    "Product | Description | Difference\n\
                    \--- | --- | ---:\n\
                    \Codex | Compact and fast | 18%\n\
                    \Other | Polished borders | 24%\n\
                    \after\n"
                (_state, output) =
                    streamMarkdownAtWidth 24 input emptyRenderState
                rows =
                    filter (not . Text.null . Text.strip) $
                        Text.lines (stripTerminalControls output)
            map terminalTextWidth rows `shouldSatisfy` all (<= 24)
            output `shouldSatisfy` Text.isInfixOf "Codex"
            output `shouldSatisfy` Text.isInfixOf "24%"
            output `shouldSatisfy` Text.isInfixOf "after"

        it "streams ordinary prose before its newline" do
            let (state1, first) = streamMarkdown "hello" emptyRenderState
                (_state2, second) = streamMarkdown " world" state1
            first `shouldBe` ""
            first <> second `shouldSatisfy` Text.isInfixOf "hello world"

        it "keeps lowercase multiword table headers reclassifiable" do
            let (state1, first) =
                    streamMarkdown "first name " emptyRenderState
                (_state2, second) =
                    streamMarkdown
                        "| age\n--- | ---:\nalice smith | 42\nafter\n"
                        state1
                output = first <> second
            first `shouldBe` ""
            output `shouldSatisfy` Text.isInfixOf "first name"
            output `shouldSatisfy` Text.isInfixOf "alice smith"
            output `shouldSatisfy` Text.isInfixOf "─"
            output `shouldSatisfy` (not . Text.isInfixOf "|")

    describe "summarizeToolCall" do
        it "uses English verbs and argument highlights" do
            summarizeToolCall (functionToolCall "c1" "read_file" "{\"target_file\":\"src/A.hs\"}")
                `shouldBe` "Read src/A.hs"
            summarizeToolCall (functionToolCall "c2" "shell_command" "{\"command\":\"ls -l\"}")
                `shouldBe` "$ ls -l"
            summarizeToolCall (functionToolCall "c2b" "write_stdin" "{\"session_id\":7}")
                `shouldBe` "Continued session 7"
            summarizeToolCall (functionToolCall "c3" "run_terminal_cmd" "{\"command\":\"git status\"}")
                `shouldBe` "$ git status"
            summarizeToolCall (functionToolCall "c3b" "run_ghci" "{\"expression\":\"1 + 1\"}")
                `shouldBe` "$ 1 + 1"
            summarizeToolCall (functionToolCall "c3c" "list_dir" "{\"target_directory\":\"packages\"}")
                `shouldBe` "Listed packages"
            summarizeToolCall (functionToolCall "c3d" "grep" "{\"pattern\":\"foo\"}")
                `shouldBe` "Searched foo"
            summarizeToolCall
                (functionToolCall
                    "c3e"
                    "wait_commands_or_subagents"
                    "{\"task_ids\":[\"t1\"]}")
                `shouldBe` "Waited"

        it "pulls the first path out of an apply_patch body" do
            let patch = "*** Begin Patch\n*** Update File: src/Foo.hs\n@@\n-a\n+b\n*** End Patch"
            summarizeToolCall (customToolCall "c4" "apply_patch" patch)
                `shouldBe` "Edited src/Foo.hs"

        it "renders namespaced collaboration tools with useful details" do
            summarizeToolCall
                (functionToolCall "c5" "collaboration.spawn_agent"
                    "{\"task_name\":\"reviewer\",\"message\":\"review this\"}")
                `shouldBe` "Spawned agent reviewer"
            summarizeToolCall
                (functionToolCall "c6" "collaboration.send_message"
                    "{\"target\":\"reviewer\",\"message\":\"status?\"}")
                `shouldBe` "Sent message to reviewer"
            summarizeToolCall
                (functionToolCall "c7" "collaboration.wait_agent"
                    "{\"timeout_ms\":30000}")
                `shouldBe` "Waited for agent updates"

        it "uses the redacted computer-action summary for privileged calls" do
            summarizeToolCall
                ( (functionToolCall
                    "computer-1"
                    "computer"
                    "{\"actions\":[{\"type\":\"type\",\"text\":\"secret\"}]}")
                    { callKind = ComputerCallKind
                    }
                )
                `shouldBe` "Computer: type 6 characters"

    describe "truncateToolOutput" do
        it "keeps the first line and marks empty output" do
            truncateToolOutput "Exit code: 0\nhello"
                `shouldSatisfy` ("Exit code: 0" `Text.isInfixOf`)
            truncateToolOutput "   "
                `shouldSatisfy` ("(empty)" `Text.isSuffixOf`)

        it "caps long multi-line output" do
            let out = Text.unlines (map (Text.pack . show) [1 :: Int .. 12])
            truncateToolOutput out `shouldSatisfy` Text.isInfixOf "… 4 more"

    describe "formatToolOutput" do
        it "renders structured collaboration results as readable text" do
            let spawn = functionToolCall "c1" "collaboration.spawn_agent" "{}"
                wait = functionToolCall "c2" "collaboration.wait_agent" "{}"
                agents = functionToolCall "c3" "collaboration.list_agents" "{}"
            formatToolOutput spawn
                "{\"nickname\":null,\"task_name\":\"/root/reviewer\"}"
                `shouldBe` "Agent: /root/reviewer"
            formatToolOutput wait
                "{\"message\":\"agent updates: reviewer=completed\",\"timed_out\":false}"
                `shouldBe` "agent updates: reviewer=completed"
            formatToolOutput agents
                "{\"agents\":[{\"agent_name\":\"/root/reviewer\",\
                \\"agent_id\":\"agent-1\",\"agent_status\":\"running\"}]}"
                `shouldBe` "/root/reviewer · running"

        it "falls back to the original output when JSON is malformed" do
            let call = functionToolCall "c1" "collaboration.spawn_agent" "{}"
            formatToolOutput call "not json" `shouldBe` "not json"

        it "renders todo_write output as a glyph checklist" do
            formatToolOutput
                (functionToolCall "c1" "todo_write" "{}")
                "- [completed] 1: Find and clone repos\n\
                \- [pending] 2: Investigate Codex"
                `shouldBe`
                    "✓ Find and clone repos\n\
                    \□ Investigate Codex"

    describe "formatElapsed" do
        it "formats seconds and minutes" do
            formatElapsed 0.4 `shouldBe` "0.4s"
            formatElapsed 12.4 `shouldBe` "12.4s"
            formatElapsed 80 `shouldBe` "1m20s"

    describe "formatActivityLine" do
        it "joins spinner, activity, and elapsed" do
            formatActivityLine False "⠋" "Thinking…" 1.2 Nothing
                `shouldBe` "⠋ Thinking…  1.2s"
            formatActivityLine False "⠋" "Writing…" 1.2 (Just 42)
                `shouldBe` "⠋ Writing…  1.2s · 42 ◈/s"

    describe "formatToolStarted" do
        it "renders English verbs for known tools" do
            formatToolStarted False (functionToolCall "c1" "read_file" "{\"target_file\":\"src/A.hs\"}")
                `shouldBe` "◇ Read src/A.hs"
            formatToolStarted False (functionToolCall "c2" "run_terminal_cmd" "{\"command\":\"git status\"}")
                `shouldBe` "◆ $ git status"
            formatToolStarted False (functionToolCall "c3" "search_replace" "{\"file_path\":\"src/A.hs\"}")
                `shouldBe` "◆ Edited src/A.hs"
            formatToolStartedRelative
                False
                "/Users/marc/.haskell-agent/worktrees/haskell-agent/wt"
                (functionToolCall
                    "c3b"
                    "search_replace"
                    "{\"file_path\":\"/Users/marc/.haskell-agent/worktrees/haskell-agent/wt/nix/modules/telegram.nix\"}")
                `shouldBe` "◆ Edited nix/modules/telegram.nix"
            formatToolStarted False
                (functionToolCall
                    "c4"
                    "wait_commands_or_subagents"
                    "{\"task_ids\":[\"t1\"]}")
                `shouldBe` "◆ Waited"

        it "keeps inspection labels and paths in subdued gray" do
            formatToolStarted True
                (functionToolCall
                    "inspect-read"
                    "read_file"
                    "{\"target_file\":\"src/A.hs\"}")
                `shouldBe`
                    roleToolArrow True glyphInspect
                        <> roleInspectName True "Read"
                        <> " "
                        <> roleToolDetail True "src/A.hs"
            formatToolStarted True
                (functionToolCall
                    "action-edit"
                    "search_replace"
                    "{\"file_path\":\"src/A.hs\"}")
                `shouldBe`
                    roleToolArrow True glyphTool
                        <> roleToolName True "Edited"
                        <> " "
                        <> roleToolPath True "src/A.hs"

        it "keeps unknown tool names" do
            formatToolStarted False (functionToolCall "c5" "custom_tool" "{\"x\":1}")
                `shouldBe` "◆ custom_tool"

        it "renders Claude Code built-ins with host chrome" do
            formatToolStarted False (functionToolCall "c10" "Bash" "{\"command\":\"git status\"}")
                `shouldBe` "◆ $ git status"
            formatToolStarted False (functionToolCall "c11" "Read" "{\"file_path\":\"src/A.hs\"}")
                `shouldBe` "◇ Read src/A.hs"
            formatToolStarted False (functionToolCall "c12" "Edit" "{\"file_path\":\"src/A.hs\"}")
                `shouldBe` "◆ Edited src/A.hs"
            formatToolStarted False
                (functionToolCall "c13" "Write" "{\"file_path\":\"src/B.hs\",\"content\":\"x\"}")
                `shouldBe` "◆ Wrote src/B.hs"
            formatToolStarted False
                (functionToolCall "c14" "WebFetch" "{\"url\":\"https://example.com\"}")
                `shouldBe` "◇ Fetched https://example.com"
            formatToolStarted False
                (functionToolCall "c15" "mcp__playwright__browser_click" "{}")
                `shouldBe` "◆ playwright: browser_click"
            formatToolBody False
                (functionToolCall "c13" "Write" "{\"file_path\":\"src/B.hs\",\"content\":\"x\"}")
                `shouldBe` "  write src/B.hs\n  +x"

        it "keeps todo_write and update_plan on their wire names" do
            formatToolStarted False
                (functionToolCall
                    "c8"
                    "todo_write"
                    "{\"todos\":[{\"id\":\"1\",\"content\":\"Find repos\"}]}")
                `shouldBe` "◆ todo_write"
            formatToolStarted False
                (functionToolCall "c9" "update_plan" "{\"plan\":[]}")
                `shouldBe` "◆ update_plan"

        it "renders learned-skill mutations as visible learning activity" do
            formatToolStarted False
                (functionToolCall
                    "c6"
                    "skill_create"
                    "{\"scope\":\"user\",\"slug\":\"post-task-review\"}")
                `shouldBe` "◆ Learned user/post-task-review"
            formatToolStarted False
                (functionToolCall
                    "c7"
                    "skill_update"
                    "{\"scope\":\"repository\",\"slug\":\"postgres-sessions\"}")
                `shouldBe`
                    "◆ Updated skill repository/postgres-sessions"

    describe "formatSearchReplaceDiff" do
        it "renders a compact unified diff" do
            let args =
                    "{\"file_path\":\"src/A.hs\",\"old_string\":\"foo\",\"new_string\":\"bar\"}"
                diff = formatSearchReplaceDiff False args
            diff `shouldSatisfy` Text.isInfixOf "-foo"
            diff `shouldSatisfy` Text.isInfixOf "+bar"

        it "labels create and delete" do
            formatSearchReplaceDiff False
                "{\"file_path\":\"src/New.hs\",\"old_string\":\"\",\"new_string\":\"hi\"}"
                `shouldSatisfy` Text.isInfixOf "create src/New.hs"
            formatSearchReplaceDiff False
                "{\"file_path\":\"src/Old.hs\",\"old_string\":\"bye\",\"new_string\":\"\"}"
                `shouldSatisfy` Text.isInfixOf "delete src/Old.hs"

    describe "formatToolBody" do
        it "renders compact multi-file apply_patch diffs" do
            let patch =
                    "*** Begin Patch\n\
                    \*** Update File: src/A.hs\n\
                    \@@\n\
                    \-old\n\
                    \+new\n\
                    \*** Add File: src/B.hs\n\
                    \+created\n\
                    \*** End Patch"
            formatToolBody False (customToolCall "patch" "apply_patch" patch)
                `shouldBe`
                    "  -old\n\
                    \  +new\n\
                    \  create src/B.hs\n\
                    \  +created"

        it "does not dump todo_write arguments into linear chrome" do
            formatToolBody False
                (functionToolCall
                    "c1"
                    "todo_write"
                    "{\"todos\":[{\"id\":\"1\",\"content\":\"Find and clone repos\"},\
                    \{\"id\":\"2\",\"content\":\"Investigate Codex\"}]}")
                `shouldBe` ""

    describe "formatLoopError" do
        it "explains a max-turn stop" do
            formatLoopError (LoopMaxTurns TurnOutput
                { responseId = "r"
                , toolCalls = []
                , assistantText = Just "almost"
                , tokenUsage = emptyTokenUsage
                , providerTelemetry = Nothing
                , completion = TurnCompleted
                })
                `shouldSatisfy` (/= "")

        it "reports incomplete output and hidden reasoning usage" do
            let rendered = formatLoopError (LoopIncomplete TurnOutput
                    { responseId = "r"
                    , toolCalls = []
                    , assistantText = Nothing
                    , tokenUsage = emptyTokenUsage
                        { outputTokens = 32768 }
                    , providerTelemetry = Nothing
                    , completion = TurnIncomplete
                        { incompleteReason = "max_output_tokens"
                        , incompleteReasoningTokens = Just 32000
                        }
                    })
            rendered `shouldSatisfy`
                Text.isInfixOf "max_output_tokens"
            rendered `shouldSatisfy`
                Text.isInfixOf "32000 reasoning tokens"
            rendered `shouldSatisfy` Text.isInfixOf "/retry"

        it "renders exhausted credentials as actionable user-facing text" do
            let now = UTCTime (fromGregorian 2026 8 22) 0
                retryAt = addUTCTime (5 * 86400 + 21 * 3600) now
                rendered =
                    formatLoopErrorAt now
                        (LoopTransport (credentialsExhausted retryAt))
            rendered `shouldSatisfy`
                Text.isInfixOf
                    "All accounts for this provider are temporarily unavailable"
            rendered `shouldSatisfy` Text.isInfixOf "Try again in 5d 21h"
            rendered `shouldSatisfy`
                Text.isInfixOf "choose another provider with /model"
            rendered `shouldNotSatisfy`
                Text.isInfixOf "CredentialsExhausted"

        it "persists absolute retry guidance without UI chrome" do
            let retryAt =
                    addUTCTime (5 * 86400 + 21 * 3600)
                        (UTCTime (fromGregorian 2026 8 22) 0)
                rendered =
                    formatLoopErrorPersistedAt
                        (UTCTime (fromGregorian 2026 8 22) 0)
                        (LoopTransport (credentialsExhausted retryAt))
            rendered `shouldSatisfy`
                Text.isInfixOf "2026-08-27 21:00:00 UTC"
            rendered `shouldNotSatisfy` Text.isInfixOf "Try again in"
            rendered `shouldNotSatisfy` Text.isPrefixOf "✗"

        it "persists provider retry intervals as absolute timestamps" do
            let rendered =
                    formatLoopErrorPersistedAt
                        (UTCTime (fromGregorian 2026 8 22) 0)
                        (LoopTransport
                            (ProviderError RateLimitError
                                "slow down"
                                (Just 120)))
            rendered `shouldSatisfy`
                Text.isInfixOf "2026-08-22 00:02:00 UTC"
            rendered `shouldNotSatisfy` Text.isInfixOf "Try again in"

        it "renders typed provider errors without constructor syntax" do
            let rendered =
                    formatLoopError
                        (LoopTransport
                            (ProviderError ContextWindowExceeded
                                "context too long"
                                Nothing))
            rendered `shouldSatisfy`
                Text.isInfixOf "conversation is too long"
            rendered `shouldSatisfy` Text.isInfixOf "/compact"
            rendered `shouldNotSatisfy`
                Text.isInfixOf "ProviderError"

        it "makes a mid-response transport stop explicit" do
            let rendered =
                    formatLoopError
                        (LoopTransportAfterOutput
                            (ConnectionError
                                "WebSocket receive error: ParseException \"not enough bytes\""))
            rendered `shouldSatisfy`
                Text.isInfixOf "Response interrupted after partial output"
            rendered `shouldSatisfy`
                Text.isInfixOf "nothing is still running"
            rendered `shouldSatisfy`
                Text.isInfixOf "Send \"continue\" to continue the task"

        it "explains an incomplete provider response" do
            formatLoopError LoopNoResponseId
                `shouldSatisfy`
                    Text.isInfixOf "Provider returned an incomplete response"

        it "renders unexpected synchronous exceptions as retryable turn errors" do
            let rendered =
                    formatLoopError
                        (LoopUnexpected "user error (disk failed)")
            rendered `shouldSatisfy`
                Text.isInfixOf
                    "Unexpected agent error: user error (disk failed)"
            rendered `shouldSatisfy` Text.isInfixOf "Retry the message."

    describe "renderEvent" do
        it "prints compact provider telemetry after a completed turn" do
            withRenderConfig False False \config handle path -> do
                renderEvent config
                    (TurnFinished
                        (emptyTurnOutput "r1" [] Nothing)
                            { providerTelemetry =
                                Just TurnTelemetry
                                    { telemetryDurationMs = Just 1250
                                    , telemetryApiDurationMs = Just 1100
                                    , telemetryCostUsd = Just 0.0125
                                    , telemetryStopReason = Just "end_turn"
                                    , telemetryProviderTurns = Just 2
                                    , telemetryModels = Map.empty
                                    , telemetryStructuredOutput = Nothing
                                    }
                            })
                hClose handle
                body <- Text.readFile path
                body `shouldSatisfy`
                    Text.isInfixOf
                        "$0.0125 · 1.2s · 2 provider turns · stop end_turn"

        it "prints canonical streamed tool metadata once" do
            withRenderConfig False False \config handle path -> do
                let early = functionToolCall "c1" "shell_command" ""
                    canonical =
                        functionToolCall
                            "c1"
                            "shell_command"
                            "{\"command\":\"git status\"}"
                renderEvent config (ToolStarted early)
                renderEvent config (ToolUpdated canonical)
                renderEvent config (ToolStarted canonical)
                hClose handle
                body <- Text.readFile path
                Text.count "◆ $ git status" body `shouldBe` 1
                calls <- stateToolCalls <$> readIORef config.renderState
                calls `shouldBe` Map.singleton "c1" canonical

        it "keeps concurrent tool lines intact" do
            withRenderConfig False False \config handle path -> do
                let events =
                        [ ToolStarted (functionToolCall cid "list_dir" args)
                        | i <- [1 :: Int .. 80]
                        , let cid = Text.pack (show i)
                              args = "{\"target_directory\":\"packages/agent-" <> cid <> "\"}"
                        ]
                dones <- mapM (\event -> do
                    done <- newEmptyMVar
                    _ <- forkIO (renderEvent config event >> putMVar done ())
                    pure done) events
                mapM_ takeMVar dones
                hClose handle
                body <- Text.readFile path
                let lines_ = filter (not . Text.null) (Text.lines body)
                length lines_ `shouldBe` 80
                lines_ `shouldMatchList`
                    [ "◇ Listed packages/agent-" <> Text.pack (show i)
                    | i <- [1 :: Int .. 80]
                    ]

        it "keeps a live thinking status after the first tool" do
            withRenderConfig True False \config handle path -> do
                renderEvent config TurnStarted
                visible <- stateThinkingVisible <$> readIORef config.renderState
                visible `shouldBe` True
                renderEvent config (ToolStarted (functionToolCall "c1" "list_dir" "{\"target_directory\":\".\"}"))
                visibleAfter <- stateThinkingVisible <$> readIORef config.renderState
                visibleAfter `shouldBe` True
                activity <- stateActivity <$> readIORef config.renderState
                activity `shouldBe` "Listed ."
                hClose handle
                body <- Text.readFile path
                body `shouldSatisfy` (Text.isInfixOf "Thinking…")
                body `shouldSatisfy` ("◇ Listed ." `Text.isInfixOf`)

        it "reuses the live spinner when a tool round starts another turn" do
            withRenderConfig True False \config _handle _path -> do
                renderEvent config TurnStarted
                first <- readIORef config.renderThinkingSpinner
                renderEvent config TurnStarted
                second <- readIORef config.renderThinkingSpinner
                fmap asyncThreadId second `shouldBe` fmap asyncThreadId first

        it "joins the spinner worker before clearing its owner" do
            withRenderConfig True False \config _handle _path -> do
                renderEvent config TurnStarted
                worker <-
                    readIORef config.renderThinkingSpinner
                        >>= maybe
                            (expectationFailure "spinner worker was not started"
                                >> fail "missing spinner worker")
                            pure
                clearThinking config
                (isJust <$> readIORef config.renderThinkingSpinner)
                    `shouldReturn` False
                poll worker >>= (`shouldSatisfy` isJust)

        it "commits buffered reasoning before a continuation TurnStarted" do
            withRenderConfig True False \config handle path -> do
                renderEvent config TurnStarted
                renderEvent config (ReasoningDelta "inspect the loop")
                renderEvent config TurnStarted
                buffered <-
                    textBufferToText . stateReasoningBuffer
                        <$> readIORef config.renderState
                buffered `shouldBe` ""
                hClose handle
                body <- Text.readFile path
                body `shouldSatisfy` Text.isInfixOf "inspect the loop"
                body `shouldSatisfy` Text.isInfixOf "Thought for"

        it "shows retry activity on the live thinking status" do
            withRenderConfig True False \config handle path -> do
                renderEvent config TurnStarted
                renderEvent config
                    (ActivityUpdated
                        "Codex server error; retrying in 5s (attempt 1)…")
                activity <- stateActivity <$> readIORef config.renderState
                activity `shouldBe`
                    "Codex server error; retrying in 5s (attempt 1)…"
                hClose handle
                body <- Text.readFile path
                body `shouldSatisfy`
                    Text.isInfixOf
                        "Codex server error; retrying in 5s (attempt 1)…"

        it "prints provider warnings without replacing live activity" do
            withRenderConfig True False \config handle path -> do
                renderEvent config TurnStarted
                renderEvent config
                    (WarningRaised
                        "Codex usage is low: primary 8% left.")
                activity <- stateActivity <$> readIORef config.renderState
                activity `shouldBe` "Thinking…"
                hClose handle
                body <- Text.readFile path
                body `shouldSatisfy`
                    Text.isInfixOf
                        "Codex usage is low: primary 8% left."

        it "closes a partial Markdown stream before an automatic retry" do
            withRenderConfig False True \config handle path -> do
                let message =
                        "Connection interrupted the response; restarting automatically."
                renderEvent config TurnStarted
                renderEvent config
                    (TextDelta "partial\n```haskell\nunfinished")
                renderEvent config (ResponseRestarted message)
                renderEvent config (TextDelta "# Complete\n")
                renderEvent config (TurnFinished TurnOutput
                    { responseId = "r1"
                    , toolCalls = []
                    , assistantText = Just "Complete"
                    , tokenUsage = emptyTokenUsage
                    , providerTelemetry = Nothing
                    , completion = TurnCompleted
                    })
                activity <- stateActivity <$> readIORef config.renderState
                activity `shouldBe` "Retrying response…"
                hClose handle
                body <- stripTerminalControls <$> Text.readFile path
                body `shouldSatisfy` Text.isInfixOf "partial"
                body `shouldSatisfy` Text.isInfixOf message
                body `shouldSatisfy` Text.isInfixOf "Complete"
                body `shouldSatisfy` (not . Text.isInfixOf "# Complete")
                let partialIndex =
                        Text.length (fst (Text.breakOn "partial" body))
                    warningIndex =
                        Text.length (fst (Text.breakOn message body))
                    completeIndex =
                        Text.length (fst (Text.breakOn "Complete" body))
                partialIndex `shouldSatisfy` (< warningIndex)
                warningIndex `shouldSatisfy` (< completeIndex)

        it "buffers reasoning summaries and commits one thinking block" do
            withRenderConfig True False \config handle path -> do
                renderEvent config TurnStarted
                renderEvent config (ReasoningDelta "secret plan")
                renderEvent config (TurnFinished TurnOutput
                    { responseId = "r1"
                    , toolCalls = []
                    , assistantText = Nothing
                    , tokenUsage = emptyTokenUsage
                    , providerTelemetry = Nothing
                    , completion = TurnCompleted
                    })
                hClose handle
                body <- Text.readFile path
                body `shouldSatisfy` (Text.isInfixOf "Thinking")
                body `shouldSatisfy` (Text.isInfixOf "secret plan")
                body `shouldSatisfy` (Text.isInfixOf "Thought for")
                body `shouldSatisfy` (not . Text.isInfixOf "\ESC7")
                body `shouldSatisfy` (not . Text.isInfixOf "\ESC8")

        it "accumulates many small reasoning deltas without losing order" do
            withRenderConfig True False \config handle path -> do
                let chunks = replicate 5000 "x"
                mapM_ (renderEvent config . ReasoningDelta) chunks
                buffered <- stateReasoningBuffer <$> readIORef config.renderState
                textBufferToText buffered
                    `shouldBe` Text.replicate 5000 "x"
                renderEvent config (TurnFinished TurnOutput
                    { responseId = "r1"
                    , toolCalls = []
                    , assistantText = Nothing
                    , tokenUsage = emptyTokenUsage
                    , providerTelemetry = Nothing
                    , completion = TurnCompleted
                    })
                (stateReasoningBuffer <$> readIORef config.renderState)
                    `shouldReturn` emptyTextBuffer
                hClose handle
                body <- Text.readFile path
                body `shouldSatisfy` Text.isInfixOf "Thought for"

        it "commits the thinking block before the first tool" do
            withRenderConfig True False \config handle path -> do
                renderEvent config TurnStarted
                renderEvent config (ReasoningDelta "look at src")
                renderEvent config
                    (ToolStarted (functionToolCall "c1" "list_dir" "{\"target_directory\":\".\"}"))
                hClose handle
                body <- Text.readFile path
                let thoughtIdx = Text.length (fst (Text.breakOn "Thought for" body))
                    toolIdx = Text.length (fst (Text.breakOn "Listed" body))
                thoughtIdx `shouldSatisfy` (< toolIdx)
                body `shouldSatisfy` (Text.isInfixOf "look at src")

        it "hides reasoning when thinking chrome is off" do
            withRenderConfig False False \config handle path -> do
                renderEvent config TurnStarted
                renderEvent config (ReasoningDelta "secret plan")
                renderEvent config (TurnFinished TurnOutput
                    { responseId = "r1"
                    , toolCalls = []
                    , assistantText = Nothing
                    , tokenUsage = emptyTokenUsage
                    , providerTelemetry = Nothing
                    , completion = TurnCompleted
                    })
                hClose handle
                body <- Text.readFile path
                body `shouldSatisfy` (not . Text.isInfixOf "secret plan")
                body `shouldSatisfy` (not . Text.isInfixOf "Thought for")

        it "styles inline markdown while streaming append-only in color mode" do
            withRenderConfig False True \config handle path -> do
                renderEvent config (TextDelta "say **")
                renderEvent config (TextDelta "hello")
                renderEvent config (TextDelta "** there")
                live <- stateLiveActive <$> readIORef config.renderState
                live `shouldBe` True
                printed <- statePrintedText <$> readIORef config.renderState
                printed `shouldBe` True
                renderEvent config (TurnFinished TurnOutput
                    { responseId = "r1"
                    , toolCalls = []
                    , assistantText = Nothing
                    , tokenUsage = emptyTokenUsage
                    , providerTelemetry = Nothing
                    , completion = TurnCompleted
                    })
                hClose handle
                body <- Text.readFile path
                body `shouldSatisfy` Text.isInfixOf "hello"
                body `shouldSatisfy` Text.isInfixOf "\ESC["
                body `shouldSatisfy` (not . Text.isInfixOf "48;")
                body `shouldSatisfy` (not . Text.isInfixOf "**")
                (stateLiveActive <$> readIORef config.renderState)
                    `shouldReturn` False

        it "does not repaint earlier content across deltas" do
            withRenderConfig False True \config handle path -> do
                renderEvent config (TextDelta "first ")
                renderEvent config (TextDelta "second")
                hClose handle
                body <- Text.readFile path
                body `shouldSatisfy` Text.isInfixOf "first "
                body `shouldSatisfy` Text.isInfixOf "second"
                Text.count "first " body `shouldBe` 1
                body `shouldSatisfy` (not . Text.isInfixOf "\ESC7")
                body `shouldSatisfy` (not . Text.isInfixOf "\ESC8")

        it "flushes an unmatched inline delimiter literally at turn end" do
            withRenderConfig False True \config handle path -> do
                renderEvent config (TextDelta "say **unfinished")
                renderEvent config (TurnFinished TurnOutput
                    { responseId = "r1"
                    , toolCalls = []
                    , assistantText = Nothing
                    , tokenUsage = emptyTokenUsage
                    , providerTelemetry = Nothing
                    , completion = TurnCompleted
                    })
                hClose handle
                body <- Text.readFile path
                body `shouldSatisfy` Text.isInfixOf "**unfinished"

        it "renders streamed block Markdown independently of chunk boundaries" do
            let samples =
                    [ "# Heading\n"
                    , "- one\n  - two\n"
                    , ">compact **quote**\n"
                    , "[x **important**](https://example.com/a_(b)) \
                        \and **bold *italic* `code`**\n"
                    , "```haskell\nmain = pure ()\n```\n"
                    , "| Key | Value |\n| --- | --- |\n| snake_case | `code` |\n"
                    , "Key | Value\n--- | ---:\nsnake_case | `code`\n"
                    ]
            forM_ samples \source -> do
                let expected = stripTerminalControls
                        (renderAssistantText True source)
                withRenderConfig False True \config handle path -> do
                    mapM_
                        (renderEvent config . TextDelta . Text.singleton)
                        (Text.unpack source)
                    renderEvent config (TurnFinished TurnOutput
                        { responseId = "r1"
                        , toolCalls = []
                        , assistantText = Nothing
                        , tokenUsage = emptyTokenUsage
                        , providerTelemetry = Nothing
                        , completion = TurnCompleted
                        })
                    hClose handle
                    actual <- stripTerminalControls <$> Text.readFile path
                    actual `shouldBe` expected

        it "preserves emoji graphemes split across streaming deltas" do
            let womanTechnologist =
                    Text.pack ['\x1f469', '\x200d', '\x1f4bb']
                keycapOne =
                    Text.pack ['1', '\xfe0f', '\x20e3']
                usFlag =
                    Text.pack ['\x1f1fa', '\x1f1f8']
                source =
                    "status "
                        <> womanTechnologist
                        <> " "
                        <> keycapOne
                        <> " "
                        <> usFlag
                expected =
                    stripTerminalControls
                        (renderAssistantText True source)
            withRenderConfig False True \config handle path -> do
                mapM_
                    (renderEvent config . TextDelta . Text.singleton)
                    (Text.unpack source)
                renderEvent config (TurnFinished TurnOutput
                    { responseId = "r1"
                    , toolCalls = []
                    , assistantText = Nothing
                    , tokenUsage = emptyTokenUsage
                    , providerTelemetry = Nothing
                    , completion = TurnCompleted
                    })
                hClose handle
                actual <- stripTerminalControls <$> Text.readFile path
                actual `shouldBe` expected

        it "does not expose fence or table markers while streaming" do
            withRenderConfig False True \config handle path -> do
                mapM_ (renderEvent config . TextDelta)
                    [ "```haskell\nmain = "
                    , "pure ()\n```\n"
                    , "| name | value |\n| --- | --- |\n"
                    , "| snake_case | **bold** |\n"
                    ]
                renderEvent config (TurnFinished TurnOutput
                    { responseId = "r1"
                    , toolCalls = []
                    , assistantText = Nothing
                    , tokenUsage = emptyTokenUsage
                    , providerTelemetry = Nothing
                    , completion = TurnCompleted
                    })
                hClose handle
                body <- stripTerminalControls <$> Text.readFile path
                body `shouldSatisfy` Text.isInfixOf "main = pure ()"
                body `shouldSatisfy` Text.isInfixOf "snake_case"
                body `shouldSatisfy` Text.isInfixOf "bold"
                body `shouldSatisfy` (not . Text.isInfixOf "```")
                body `shouldSatisfy` (not . Text.isInfixOf "**bold**")

        it "flushes pre-tool assistant prose before tool lines" do
            withRenderConfig False True \config handle path -> do
                let call = functionToolCall "c1" "list_dir" "{\"target_directory\":\".\"}"
                renderEvent config (TextDelta "checking `src`")
                renderEvent config (TurnFinished TurnOutput
                    { responseId = "r1"
                    , toolCalls = [call]
                    , assistantText = Nothing
                    , tokenUsage = emptyTokenUsage
                    , providerTelemetry = Nothing
                    , completion = TurnCompleted
                    })
                hClose handle
                body <- Text.readFile path
                body `shouldSatisfy` Text.isInfixOf "src"
                body `shouldSatisfy` Text.isInfixOf "\ESC["

        it "paints assistantText on TurnFinished when no deltas arrived" do
            withRenderConfig False True \config handle path -> do
                renderEvent config (TurnFinished TurnOutput
                    { responseId = "r1"
                    , toolCalls = []
                    , assistantText = Just "see `file.txt`"
                    , tokenUsage = emptyTokenUsage
                    , providerTelemetry = Nothing
                    , completion = TurnCompleted
                    })
                hClose handle
                body <- Text.readFile path
                body `shouldSatisfy` Text.isInfixOf "file.txt"
                body `shouldSatisfy` Text.isInfixOf "\ESC["
                body `shouldSatisfy` (not . Text.isInfixOf "`file.txt`")

        it "styles tool chrome when color is on" do
            withRenderConfig False True \config handle path -> do
                let call = functionToolCall "c1" "list_dir" "{\"target_directory\":\".\"}"
                renderEvent config (ToolStarted call)
                renderEvent config (ToolFinished ToolCallResult
                    { callId = "c1"
                    , output = "ok\nmore"
                    , callKind = FunctionCallKind
                    })
                hClose handle
                body <- Text.readFile path
                body `shouldSatisfy` Text.isInfixOf "Listed"
                body `shouldSatisfy` Text.isInfixOf "\ESC["
                body `shouldSatisfy` Text.isInfixOf "ok"

        it "suppresses todo_write from linear scrollback" do
            withRenderConfig False False \config handle path -> do
                let call =
                        functionToolCall
                            "c1"
                            "todo_write"
                            "{\"todos\":[{\"id\":\"1\",\"content\":\"Find repos\"}]}"
                renderEvent config (ToolStarted call)
                renderEvent config (ToolFinished ToolCallResult
                    { callId = "c1"
                    , output =
                        "- [completed] 1: Find and clone Grok Build and Codex repos\n\
                        \- [pending] 2: Investigate Codex"
                    , callKind = FunctionCallKind
                    })
                hClose handle
                body <- Text.readFile path
                body `shouldNotSatisfy` Text.isInfixOf "todo_write"
                body `shouldNotSatisfy` Text.isInfixOf "Find and clone"
                body `shouldNotSatisfy` Text.isInfixOf "[completed]"

        it "keeps thinking plain when color is off" do
            withRenderConfig True False \config handle path -> do
                renderEvent config TurnStarted
                hClose handle
                body <- Text.readFile path
                body `shouldSatisfy` (Text.isInfixOf "Thinking…")
                body `shouldSatisfy` (not . Text.isInfixOf "\ESC]9;4;")

        it "emits Ghostty OSC 9;4 while thinking and clears it after" do
            withRenderConfigNative True False True \config handle path -> do
                renderEvent config TurnStarted
                renderEvent config (ReasoningDelta "plan")
                clearThinking config
                hClose handle
                body <- Text.readFile path
                body `shouldSatisfy` containsOsc9 3
                body `shouldSatisfy` containsOsc9 0

        it "keeps reduced and off thinking static without native animation" do
            mapM_
                (\mode ->
                    withRenderConfigNativeMode
                        True
                        False
                        True
                        mode
                        \config handle path -> do
                            renderEvent config TurnStarted
                            hClose handle
                            body <- Text.readFile path
                            body `shouldSatisfy`
                                Text.isInfixOf
                                    (foregroundIndicator
                                        motionGlyphSet
                                        mode
                                        0
                                        <> " Thinking…")
                            body `shouldSatisfy` (not . containsOsc9 3))
                [MotionReduced, MotionOff]

    describe "formatThinkingBlock" do
        it "headers a live preview and a finished duration" do
            formatThinkingBlock False True 1.2 "secret plan"
                `shouldSatisfy` Text.isInfixOf "Thinking"
            let finished = formatThinkingBlock False False 1.2 "secret plan"
            finished `shouldSatisfy` Text.isInfixOf "Thought for 1.2s"
            finished `shouldSatisfy` Text.isInfixOf "secret plan"

        it "truncates live preview to three wrapped lines" do
            let raw = Text.unlines (replicate 8 "word")
                live = formatThinkingBlock False True 0.4 raw
            Text.count "word" live `shouldBe` 3
            live `shouldSatisfy` Text.isInfixOf "more"

    describe "wrapThinkingLines" do
        it "wraps at the thoughts width" do
            wrapThinkingLines 8 "hello world friends"
                `shouldBe` ["hello", "world", "friends"]

        it "splits an overlong token" do
            wrapThinkingLines 4 "abcdefgh"
                `shouldBe` ["abcd", "efgh"]

containsOsc9 :: Int -> Text.Text -> Bool
containsOsc9 state body =
    let raw = "\ESC]9;4;" <> Text.pack (show state) <> "\BEL"
        wrapped = "\ESCPtmux;\ESC" <> raw <> "\ESC\\"
    in raw `Text.isInfixOf` body || wrapped `Text.isInfixOf` body

withRenderConfig
    :: Bool
    -> Bool
    -> (RenderConfig -> Handle -> FilePath -> IO ())
    -> IO ()
withRenderConfig showThinking color =
    withRenderConfigNative showThinking color False

withRenderConfigNative
    :: Bool
    -> Bool
    -> Bool
    -> (RenderConfig -> Handle -> FilePath -> IO ())
    -> IO ()
withRenderConfigNative showThinking color native =
    withRenderConfigNativeMode
        showThinking
        color
        native
        MotionFull

withRenderConfigNativeMode
    :: Bool
    -> Bool
    -> Bool
    -> MotionMode
    -> (RenderConfig -> Handle -> FilePath -> IO ())
    -> IO ()
withRenderConfigNativeMode showThinking color native motionMode action = do
    spinner <- newIORef Nothing
    modelRef <- newIORef "test-model"
    renderStateRef <- newIORef emptyRenderState
    lock <- newMVar ()
    tmp <- getTemporaryDirectory
    (path, handle) <- openTempFile tmp "agent-render-spec"
    flip finally (removeFile path) do
        hSetBuffering handle NoBuffering
        let config = RenderConfig
                { renderShowThinking = showThinking
                , renderThinkingSpinner = spinner
                , renderState = renderStateRef
                , renderColor = color
                , renderLock = lock
                , renderStdout = handle
                , renderStderr = handle
                , renderModel = readIORef modelRef
                , renderNativeProgress = native
                , renderMotionMode = motionMode
                , renderWorkspace = ""
                }
        action config handle path
        clearThinking config

stripTerminalControls :: Text.Text -> Text.Text
stripTerminalControls = go
  where
    go text =
        case Text.break (== '\ESC') text of
            (before, rest)
                | Text.null rest -> before
                | Just after <- Text.stripPrefix "\ESC[" rest ->
                    before <> go (dropCsi after)
                | Just after <- Text.stripPrefix "\ESC]" rest ->
                    before <> go (dropOsc after)
                | otherwise ->
                    before <> go (Text.drop 1 rest)

    dropCsi =
        Text.drop 1
            . Text.dropWhile
                (\character ->
                    not (character >= '@' && character <= '~'))

    dropOsc text =
        case Text.break (\character -> character == '\BEL' || character == '\ESC')
                text of
            (_, rest)
                | Text.null rest -> ""
                | Text.isPrefixOf "\BEL" rest -> Text.drop 1 rest
                | Text.isPrefixOf "\ESC\\" rest -> Text.drop 2 rest
                | otherwise -> dropOsc (Text.drop 1 rest)
