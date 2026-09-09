module Agent.CLI.SessionThreadsSpec (spec) where

import Agent.CLI.Session.Threads
import Agent.CLI.SessionLock
    ( acquireSessionLock
    , adjustSessionInboxPending
    , releaseSessionLock
    )
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar, tryTakeMVar )
import Control.Exception.Safe (bracket, finally)
import Control.Monad (forM, forM_)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.OsPath (unsafeEncodeUtf, (</>))
import System.Posix.Temp (mkdtemp)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "shared session thread manager" do
    it "rejects a duplicate running turn without executing its action" $
        withManager ["session"] \manager -> do
            started <- newEmptyMVar
            release <- newEmptyMVar
            duplicate <- newEmptyMVar
            launchSessionThread manager "session"
                (putMVar started () >> takeMVar release >> pure (Right ()))
                `shouldReturn` Right "started session session"
            withinDeadline (takeMVar started)
            launchSessionThread manager "session"
                (putMVar duplicate () >> pure (Right ()))
                `shouldReturn` Left "session session is already running"
            tryTakeMVar duplicate `shouldReturn` Nothing
            putMVar release ()
            waitForStatus manager "session" "completed"

    it "cancels and joins every active worker before close returns" $
        withManager ["one", "two"] \manager -> do
            stopped <- forM ["one", "two"] \sessionId -> do
                started <- newEmptyMVar
                release <- newEmptyMVar
                finished <- newEmptyMVar
                launchSessionThread manager sessionId
                    ((putMVar started () >> takeMVar release >> pure (Right ()))
                        `finally` putMVar finished ())
                    `shouldReturn` Right ("started session " <> sessionId)
                withinDeadline (takeMVar started)
                pure finished
            closeSessionThreadManager manager
            forM_ stopped \finished ->
                tryTakeMVar finished `shouldReturn` Just ()

    it "rejects launches after close without running their actions" $
        withManager [] \manager -> do
            ran <- newEmptyMVar
            closeSessionThreadManager manager
            launchSessionThread manager "later"
                (putMVar ran () >> pure (Right ()))
                `shouldReturn` Left "agent session manager is closed"
            tryTakeMVar ran `shouldReturn` Nothing

    it "retains completion and allows another turn for the same session" $
        withManager ["session"] \manager -> do
            launchSessionThread manager "session" (pure (Right ()))
                `shouldReturn` Right "started session session"
            waitForStatus manager "session" "completed"
            sessionThreadStatus manager "session" `shouldReturn` "completed"
            launchSessionThread manager "session" (pure (Left "second turn"))
                `shouldReturn` Right "started session session"
            waitForStatus manager "session" "failed (second turn)"

    it "retains provider failure and permits a successful retry" $
        withManager ["session"] \manager -> do
            launchSessionThread manager "session" (pure (Left "provider failed"))
                `shouldReturn` Right "started session session"
            waitForStatus manager "session" "failed (provider failed)"
            sessionThreadStatus manager "session"
                `shouldReturn` "failed (provider failed)"
            launchSessionThread manager "session" (pure (Right ()))
                `shouldReturn` Right "started session session"
            waitForStatus manager "session" "completed"

    it "waits for accepted inbox messages on an otherwise idle open session" $ do
        tmp <- Directory.getTemporaryDirectory
        bracket
            (mkdtemp (tmp FilePath.</> "ha-inbox-wait"))
            Directory.removeDirectoryRecursive
            \root -> do
                Directory.createDirectory (root FilePath.</> "session")
                bracket
                    (newSessionThreadManager (unsafeEncodeUtf root))
                    closeSessionThreadManager
                    \manager -> do
                        let sessionDir =
                                unsafeEncodeUtf root </> unsafeEncodeUtf "session"
                        Right lock <- acquireSessionLock sessionDir "session"
                        flip finally (releaseSessionLock lock) do
                            _ <- adjustSessionInboxPending sessionDir 1
                            wait <- prepareSessionThreadWait manager "session"
                            timeout 50000 wait `shouldReturn` Nothing
                            _ <- adjustSessionInboxPending sessionDir (-1)
                            timeout 500000 wait `shouldReturn` Just "idle"

-- Bound only interruptible handshakes, never wrap masked resource release in
-- nested timeouts. The manager's bracket owns cancellation and joining.
withinDeadline :: IO a -> IO a
withinDeadline action =
    timeout 5000000 action >>= maybe (fail "session worker handshake timed out") pure

waitForStatus :: SessionThreadManager -> Text -> Text -> IO ()
waitForStatus manager sessionId expected = withinDeadline loop
  where
    loop = do
        status <- sessionThreadStatus manager sessionId
        if status == expected
            then pure ()
            else threadDelay 1000 >> loop

withManager :: [Text] -> (SessionThreadManager -> IO a) -> IO a
withManager sessionIds action = do
    tmp <- Directory.getTemporaryDirectory
    bracket
        (mkdtemp (tmp FilePath.</> "ha-shared-threads"))
        Directory.removeDirectoryRecursive
        \root -> do
            forM_ sessionIds \sessionId ->
                Directory.createDirectory (root FilePath.</> Text.unpack sessionId)
            bracket
                (newSessionThreadManager (unsafeEncodeUtf root))
                closeSessionThreadManager
                action
