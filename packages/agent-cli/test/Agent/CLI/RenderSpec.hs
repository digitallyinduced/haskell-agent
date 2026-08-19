module Agent.CLI.RenderSpec (spec) where

import Agent.CLI.Render
import Agent.Loop (LoopError(..), LoopEvent(..), TurnOutput(..))
import Agent.ToolDispatch (customToolCall, functionToolCall)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Exception (finally)
import Data.IORef (newIORef)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (BufferMode(..), hClose, hSetBuffering, openTempFile)
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
            printed <- newIORef False
            lock <- newMVar ()
            tmp <- getTemporaryDirectory
            (path, handle) <- openTempFile tmp "agent-render-spec"
            flip finally (removeFile path) do
                hSetBuffering handle NoBuffering
                let config = RenderConfig
                        { renderShowReasoning = False
                        , renderPrintedText = printed
                        , renderLock = lock
                        , renderStdout = handle
                        , renderStderr = handle
                        }
                    events =
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
