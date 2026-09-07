module Agent.CLI.AgentSessionsWaitGraphSpec (spec) where

import Agent.CLI.AgentSessions.WaitGraph (withSessionWaitEdge)
import Agent.OsPath (unsafeToFilePath)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (cancel, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception.Safe (bracket)
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.OsPath (OsPath, unsafeEncodeUtf)
import System.IO.Error (tryIOError)
import System.Posix.IO (closeFd, createPipe)
import System.Posix.IO.ByteString (fdRead, fdWrite)
import System.Posix.Process (forkProcess, getProcessStatus)
import System.Posix.Signals (sigKILL, signalProcess)
import System.Posix.Temp (mkdtemp)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.AgentSessions.WaitGraph" do
    it "rejects self waits without running the action" $
        withRoot \root ->
            withSessionWaitEdge root "a" "a" (expectationFailure "ran")
                `shouldReturn` Left "cannot wait for the current agent session"

    it "rejects indirect cycles and permits unrelated waits" $
        withRoot \root -> do
            result <- withSessionWaitEdge root "a" "b" $
                withSessionWaitEdge root "b" "c" do
                    withSessionWaitEdge root "c" "a" (pure ())
                        `shouldReturn` Left "circular agent session wait rejected"
                    withSessionWaitEdge root "c" "d" (pure ())
                        `shouldReturn` Right ()
            result `shouldBe` Right (Right ())

    it "retains every simultaneous outgoing edge from a session" $
        withRoot \root -> do
            result <- withSessionWaitEdge root "a" "b" $
                withSessionWaitEdge root "a" "c" do
                    withSessionWaitEdge root "b" "a" (pure ())
                        `shouldReturn` Left "circular agent session wait rejected"
                    withSessionWaitEdge root "c" "a" (pure ())
                        `shouldReturn` Left "circular agent session wait rejected"
            result `shouldBe` Right (Right ())

    it "removes edges on cancellation without cancelling other waits" $
        withRoot \root -> do
            ready <- newEmptyMVar
            never <- newEmptyMVar
            withAsync
                (withSessionWaitEdge root "a" "b"
                    (putMVar ready () >> takeMVar never))
                \worker -> do
                    takeMVar ready
                    withSessionWaitEdge root "b" "a" (pure ())
                        `shouldReturn` Left "circular agent session wait rejected"
                    cancel worker
                    withSessionWaitEdge root "b" "a" (pure ())
                        `shouldReturn` Right ()

    it "prunes unlocked crash remnants before detecting cycles" $
        withRoot \root -> do
            -- Initialize the private graph directory with the public API.
            withSessionWaitEdge root "a" "b" (pure ())
                `shouldReturn` Right ()
            -- A crashed registration has metadata but no live advisory lease.
            let directory = unsafeToFilePath root FilePath.</> ".agent-session-waits"
                    FilePath.</> "edge-crashed"
            Directory.createDirectory directory
            writeFile (directory FilePath.</> "edge.json") "[\"a\",\"b\"]"
            withSessionWaitEdge root "b" "a" (pure ())
                `shouldReturn` Right ()
            Directory.doesDirectoryExist directory `shouldReturn` False

    it "detects cross-process cycles and recovers after a process crash" $
        withRoot \root ->
            bracket createPipe (\(readFd, writeFd) -> closeFd readFd >> closeFd writeFd)
                \(readFd, writeFd) -> do
                    let child = do
                            _ <- withSessionWaitEdge root "a" "b" do
                                _ <- fdWrite writeFd "r"
                                threadDelay 30000000
                            pure ()
                        stop pid = do
                            tryIOError (getProcessStatus False False pid) >>= \case
                                Right Nothing -> do
                                    _ <- tryIOError (signalProcess sigKILL pid)
                                    _ <- getProcessStatus True False pid
                                    pure ()
                                _ -> pure ()
                    bracket (forkProcess child) stop \pid -> do
                        timeout 5000000 (fdRead readFd 1) `shouldReturn` Just "r"
                        withSessionWaitEdge root "b" "a" (pure ())
                            `shouldReturn` Left "circular agent session wait rejected"
                        signalProcess sigKILL pid
                        _ <- getProcessStatus True False pid
                        withSessionWaitEdge root "b" "a" (pure ())
                            `shouldReturn` Right ()

withRoot :: (OsPath -> IO a) -> IO a
withRoot action = do
    temporary <- Directory.getTemporaryDirectory
    bracket
        (mkdtemp (temporary FilePath.</> "agent-wait-graph-"))
        Directory.removeDirectoryRecursive
        (action . unsafeEncodeUtf)
