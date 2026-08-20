module Agent.CLI.RenderSpec (spec) where

import Agent.CLI.Render
import Agent.Loop (LoopError(..), LoopEvent(..), TurnOutput(..), emptyTokenUsage)
import Agent.ToolDispatch
    ( ToolCallKind(..)
    , ToolCallResult(..)
    , customToolCall
    , functionToolCall
    )
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Exception (finally)
import Data.IORef (newIORef, readIORef)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (BufferMode(..), Handle, hClose, hSetBuffering, openTempFile)
import Test.Hspec

spec :: Spec
spec = do
    describe "summarizeToolCall" do
        it "includes JSON argument highlights" do
            summarizeToolCall (functionToolCall "c1" "read_file" "{\"target_file\":\"src/A.hs\"}")
                `shouldBe` "read_file src/A.hs"
            summarizeToolCall (functionToolCall "c2" "shell_command" "{\"command\":\"ls -l\"}")
                `shouldBe` "shell_command ls -l"
            summarizeToolCall (functionToolCall "c3" "run_terminal_cmd" "{\"command\":\"git status\"}")
                `shouldBe` "run_terminal_cmd git status"
            summarizeToolCall (functionToolCall "c3b" "run_ghci" "{\"expression\":\"1 + 1\"}")
                `shouldBe` "run_ghci 1 + 1"

        it "pulls the first path out of an apply_patch body" do
            let patch = "*** Begin Patch\n*** Update File: src/Foo.hs\n@@\n-a\n+b\n*** End Patch"
            summarizeToolCall (customToolCall "c4" "apply_patch" patch)
                `shouldBe` "apply_patch src/Foo.hs"

    describe "truncateToolOutput" do
        it "keeps the first line and marks empty output" do
            truncateToolOutput "Exit code: 0\nhello"
                `shouldSatisfy` ("Exit code: 0" `Text.isInfixOf`)
            truncateToolOutput "   "
                `shouldSatisfy` ("(empty)" `Text.isSuffixOf`)

        it "caps long multi-line output" do
            let out = Text.unlines (map (Text.pack . show) [1 :: Int .. 12])
            truncateToolOutput out `shouldSatisfy` Text.isInfixOf "… 4 more"

    describe "formatElapsed" do
        it "formats seconds and minutes" do
            formatElapsed 0.4 `shouldBe` "0.4s"
            formatElapsed 12.4 `shouldBe` "12.4s"
            formatElapsed 80 `shouldBe` "1m20s"

    describe "formatActivityLine" do
        it "joins spinner, activity, and elapsed" do
            formatActivityLine False "⠋" "thinking…" 1.2
                `shouldBe` "⠋ thinking…  1.2s"

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

    describe "formatLoopError" do
        it "explains a max-turn stop" do
            formatLoopError (LoopMaxTurns TurnOutput
                { responseId = "r"
                , toolCalls = []
                , assistantText = Just "almost"
                , tokenUsage = emptyTokenUsage
                })
                `shouldSatisfy` (/= "")

    describe "renderEvent" do
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
                    [ "◆ list_dir packages/agent-" <> Text.pack (show i)
                    | i <- [1 :: Int .. 80]
                    ]

        it "keeps a live thinking status after the first tool" do
            withRenderConfig True False \config handle path -> do
                renderEvent config TurnStarted
                visible <- readIORef config.renderThinkingVisible
                visible `shouldBe` True
                renderEvent config (ToolStarted (functionToolCall "c1" "list_dir" "{\"target_directory\":\".\"}"))
                visibleAfter <- readIORef config.renderThinkingVisible
                visibleAfter `shouldBe` True
                activity <- readIORef config.renderActivityRef
                activity `shouldBe` "list_dir ."
                hClose handle
                body <- Text.readFile path
                body `shouldSatisfy` (Text.isInfixOf "thinking…")
                body `shouldSatisfy` ("◆ list_dir ." `Text.isInfixOf`)

        it "ignores reasoning deltas" do
            withRenderConfig True False \config handle path -> do
                renderEvent config TurnStarted
                renderEvent config (ReasoningDelta "secret plan")
                hClose handle
                body <- Text.readFile path
                body `shouldSatisfy` (Text.isInfixOf "thinking…")

        it "streams styled TextDelta live in color mode" do
            withRenderConfig False True \config handle path -> do
                renderEvent config (TextDelta "see `file.txt`")
                buffered <- readIORef config.renderTextBuffer
                buffered `shouldBe` "see `file.txt`"
                live <- readIORef config.renderLiveActive
                live `shouldBe` True
                printed <- readIORef config.renderPrintedText
                printed `shouldBe` True
                renderEvent config (TurnFinished TurnOutput
                    { responseId = "r1"
                    , toolCalls = []
                    , assistantText = Nothing
                    , tokenUsage = emptyTokenUsage
                    })
                hClose handle
                body <- Text.readFile path
                body `shouldSatisfy` Text.isInfixOf "file.txt"
                body `shouldSatisfy` Text.isInfixOf "\ESC["
                body `shouldSatisfy` Text.isInfixOf "\ESC[48;2;0;43;54m"
                body `shouldSatisfy` (not . Text.isInfixOf "`file.txt`")
                readIORef config.renderLiveActive `shouldReturn` False

        it "restyles growing markdown across deltas" do
            withRenderConfig False True \config handle path -> do
                renderEvent config (TextDelta "see `fi")
                renderEvent config (TextDelta "le.txt`")
                hClose handle
                body <- Text.readFile path
                -- Second delta should restore saved cursor and redraw; final
                -- body styles the closed code span (no raw backticks left).
                body `shouldSatisfy` Text.isInfixOf "file.txt"
                body `shouldSatisfy` (not . Text.isInfixOf "`file.txt`")
                body `shouldSatisfy` Text.isInfixOf "\ESC["
                body `shouldSatisfy` Text.isInfixOf "\ESC7"  -- DECSC
                body `shouldSatisfy` Text.isInfixOf "\ESC8"  -- DECRC

        it "flushes pre-tool assistant prose before tool lines" do
            withRenderConfig False True \config handle path -> do
                let call = functionToolCall "c1" "list_dir" "{\"target_directory\":\".\"}"
                renderEvent config (TextDelta "checking `src`")
                renderEvent config (TurnFinished TurnOutput
                    { responseId = "r1"
                    , toolCalls = [call]
                    , assistantText = Nothing
                    , tokenUsage = emptyTokenUsage
                    })
                hClose handle
                body <- Text.readFile path
                body `shouldSatisfy` Text.isInfixOf "src"
                body `shouldSatisfy` Text.isInfixOf "\ESC["
                readIORef config.renderTextBuffer `shouldReturn` ""

        it "paints assistantText on TurnFinished when no deltas arrived" do
            withRenderConfig False True \config handle path -> do
                renderEvent config (TurnFinished TurnOutput
                    { responseId = "r1"
                    , toolCalls = []
                    , assistantText = Just "see `file.txt`"
                    , tokenUsage = emptyTokenUsage
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
                body `shouldSatisfy` Text.isInfixOf "list_dir"
                body `shouldSatisfy` Text.isInfixOf "\ESC["
                body `shouldSatisfy` Text.isInfixOf "ok"

        it "keeps thinking plain when color is off" do
            withRenderConfig True False \config handle path -> do
                renderEvent config TurnStarted
                hClose handle
                body <- Text.readFile path
                body `shouldSatisfy` (Text.isInfixOf "thinking…")

    describe "visibleDisplayRows" do
        it "counts soft-wrapped rows without ANSI" do
            visibleDisplayRows 5 "abcdefghij" `shouldBe` 2
            visibleDisplayRows 10 "abcdefghij" `shouldBe` 1

        it "ignores SGR when measuring width" do
            let painted = "\ESC[1mabcdefghij\ESC[0m"
            visibleDisplayRows 5 painted `shouldBe` 2

withRenderConfig
    :: Bool
    -> Bool
    -> (RenderConfig -> Handle -> FilePath -> IO ())
    -> IO ()
withRenderConfig showThinking color action = do
    printed <- newIORef False
    thinking <- newIORef False
    spinner <- newIORef Nothing
    modelRef <- newIORef "test-model"
    activityRef <- newIORef "thinking…"
    startedAt <- newIORef Nothing
    textBuffer <- newIORef ""
    liveActive <- newIORef False
    lock <- newMVar ()
    tmp <- getTemporaryDirectory
    (path, handle) <- openTempFile tmp "agent-render-spec"
    flip finally (removeFile path) do
        hSetBuffering handle NoBuffering
        let config = RenderConfig
                { renderShowThinking = showThinking
                , renderThinkingVisible = thinking
                , renderThinkingSpinner = spinner
                , renderColor = color
                , renderPrintedText = printed
                , renderTextBuffer = textBuffer
                , renderLiveActive = liveActive
                , renderLock = lock
                , renderStdout = handle
                , renderStderr = handle
                , renderModelRef = modelRef
                , renderActivityRef = activityRef
                , renderStartedAt = startedAt
                }
        action config handle path
        clearThinking config
