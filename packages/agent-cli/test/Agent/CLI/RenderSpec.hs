module Agent.CLI.RenderSpec (spec) where

import Agent.CLI.Render
import Agent.Loop (LoopError(..), LoopEvent(..), TurnOutput(..))
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
            summarizeToolCall (functionToolCall "c3b" "run_ghci" "{"expression":"1 + 1"}")
                `shouldBe` "run_ghci 1 + 1"

        it "pulls the first path out of an apply_patch body" do
            let patch = "*** Begin Patch\n*** Update File: src/Foo.hs\n@@\n-a\n+b\n*** End Patch"
            summarizeToolCall (customToolCall "c4" "apply_patch" patch)
                `shouldBe` "apply_patch src/Foo.hs"

    describe "truncateToolOutput" do
        it "keeps the first line and marks empty output" do
            truncateToolOutput "Exit code: 0\nhello" `shouldBe` "Exit code: 0"
            truncateToolOutput "   " `shouldBe` "(empty)"

    describe "formatLoopError" do
        it "explains a max-turn stop" do
            formatLoopError (LoopMaxTurns TurnOutput
                { responseId = "r"
                , toolCalls = []
                , assistantText = Just "almost"
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
                    [ "→ list_dir packages/agent-" <> Text.pack (show i)
                    | i <- [1 :: Int .. 80]
                    ]

        it "shows a static thinking status until the first tool or text" do
            withRenderConfig True False \config handle path -> do
                renderEvent config TurnStarted
                visible <- readIORef config.renderThinkingVisible
                visible `shouldBe` True
                renderEvent config (ToolStarted (functionToolCall "c1" "list_dir" "{\"target_directory\":\".\"}"))
                visibleAfter <- readIORef config.renderThinkingVisible
                visibleAfter `shouldBe` False
                hClose handle
                body <- Text.readFile path
                body `shouldSatisfy` ("thinking…" `Text.isPrefixOf`)
                -- Clear uses CR + erase, so the tool summary shares the buffer
                -- line with the status text rather than starting a new line.
                body `shouldSatisfy` ("→ list_dir ." `Text.isInfixOf`)

        it "ignores reasoning deltas" do
            withRenderConfig True False \config handle path -> do
                renderEvent config TurnStarted
                renderEvent config (ReasoningDelta "secret plan")
                hClose handle
                body <- Text.readFile path
                body `shouldBe` "thinking…"

        it "buffers colored TextDelta until TurnFinished" do
            withRenderConfig False True \config handle path -> do
                renderEvent config (TextDelta "see `file.txt`")
                buffered <- readIORef config.renderTextBuffer
                buffered `shouldBe` "see `file.txt`"
                renderEvent config (TurnFinished TurnOutput
                    { responseId = "r1"
                    , toolCalls = []
                    , assistantText = Nothing
                    })
                hClose handle
                body <- Text.readFile path
                body `shouldSatisfy` Text.isInfixOf "file.txt"
                body `shouldSatisfy` Text.isInfixOf "\ESC["
                body `shouldSatisfy` (not . Text.isInfixOf "`file.txt`")

        it "flushes pre-tool assistant prose before tool lines" do
            withRenderConfig False True \config handle path -> do
                let call = functionToolCall "c1" "list_dir" "{\"target_directory\":\".\"}"
                renderEvent config (TextDelta "checking `src`")
                renderEvent config (TurnFinished TurnOutput
                    { responseId = "r1"
                    , toolCalls = [call]
                    , assistantText = Nothing
                    })
                hClose handle
                body <- Text.readFile path
                body `shouldSatisfy` Text.isInfixOf "src"
                body `shouldSatisfy` Text.isInfixOf "\ESC["
                readIORef config.renderTextBuffer `shouldReturn` ""

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
                body `shouldBe` "thinking…"

withRenderConfig
    :: Bool
    -> Bool
    -> (RenderConfig -> Handle -> FilePath -> IO ())
    -> IO ()
withRenderConfig showThinking color action = do
    printed <- newIORef False
    thinking <- newIORef False
    textBuffer <- newIORef ""
    lock <- newMVar ()
    tmp <- getTemporaryDirectory
    (path, handle) <- openTempFile tmp "agent-render-spec"
    flip finally (removeFile path) do
        hSetBuffering handle NoBuffering
        let config = RenderConfig
                { renderShowThinking = showThinking
                , renderThinkingVisible = thinking
                , renderColor = color
                , renderPrintedText = printed
                , renderTextBuffer = textBuffer
                , renderLock = lock
                , renderStdout = handle
                , renderStderr = handle
                }
        action config handle path
