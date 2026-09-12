module Agent.Runtime.SessionInboxSpec (spec) where

import Agent.Runtime.Session.Inbox
import Agent.Runtime.SessionLock
    ( acquireSessionLock
    , adjustSessionInboxPending
    , releaseSessionLock
    , sessionInboxPending
    )
import Control.Concurrent.STM (atomically)
import Control.Exception.Safe (bracket, finally)
import Control.Monad (void)
import Data.Bits ((.&.))
import Data.IORef (newIORef, readIORef, writeIORef)
import System.Directory
    ( createDirectory, getTemporaryDirectory, removeDirectoryRecursive, removeFile )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.OsPath (unsafeEncodeUtf)
import System.Posix.Files (createSymbolicLink, fileMode, getFileStatus)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "CLI session inbox" do
    it "delivers a message to the owning process" $
        withDirectory \directory -> do
            inbox <- newSessionInbox
            let sessionDir = unsafeEncodeUtf directory
            withSessionInboxServerAt directory inbox "session" sessionDir do
                deliverSessionInboxMessageAt directory "session" "please continue"
                    `shouldReturn` Right ()
                atomically (takeInboxMessage inbox)
                    `shouldReturn` "please continue"
                sessionInboxPending sessionDir `shouldReturn` 1
                releaseInboxPending inbox
                sessionInboxPending sessionDir `shouldReturn` 0

    it "rejects an empty message before touching the owner queue" $
        withDirectory \directory -> do
            inbox <- newSessionInbox
            let sessionDir = unsafeEncodeUtf directory
            withSessionInboxServerAt directory inbox "session" sessionDir do
                deliverSessionInboxMessageAt directory "session" "   "
                    `shouldReturn` Left
                        "send_agent_session_message requires a non-empty message"
                timeout 20000 (atomically (takeInboxMessage inbox))
                    `shouldReturn` Nothing

    it "uses private socket permissions and refuses a competing owner" $
        withDirectory \directory -> do
            inbox <- newSessionInbox
            let sessionDir = unsafeEncodeUtf directory
            withSessionInboxServerAt directory inbox "session" sessionDir do
                status <- getFileStatus (inboxSocketPath directory "session")
                fileMode status .&. 0o777 `shouldBe` 0o600
                other <- newSessionInbox
                withSessionInboxServerAt directory other "session" sessionDir
                    (pure ())
                    `shouldThrow` anyIOException

    it "rejects symbolic-link directories rather than changing their permissions" $
        withDirectory \directory -> do
            let destination = directory </> "destination"
                symbolic = directory </> "symbolic"
            createDirectory destination
            createSymbolicLink destination symbolic
            inbox <- newSessionInbox
            withSessionInboxServerAt
                symbolic inbox "session" (unsafeEncodeUtf destination) (pure ())
                `shouldThrow` anyIOException

    it "reports unavailable when no owner is listening" $
        withDirectory \directory ->
            deliverSessionInboxMessageAt directory "missing" "hello"
                `shouldReturn` Left inboxUnavailableError

    it "falls back without an inbox when the endpoint cannot be opened" $
        withDirectory \directory -> do
            inbox <- newSessionInbox
            entered <- newIORef False
            withInboxDirectory (directory </> replicate 150 'x') $
                withOptionalSessionInboxServer
                    inbox "session" (unsafeEncodeUtf directory) do
                        writeIORef entered True
            readIORef entered `shouldReturn` True

    it "keeps pending count aligned with accepted and released messages" $
        withDirectory \directory -> do
            let sessionDir = unsafeEncodeUtf directory
            Right lock <- acquireSessionLock sessionDir "session"
            flip finally (releaseSessionLock lock) do
                adjustSessionInboxPending sessionDir 2
                    `shouldReturn` 2
                sessionInboxPending sessionDir `shouldReturn` 2
                adjustSessionInboxPending sessionDir (-1)
                    `shouldReturn` 1
                void (adjustSessionInboxPending sessionDir (-5))
                sessionInboxPending sessionDir `shouldReturn` 0

withDirectory :: (FilePath -> IO a) -> IO a
withDirectory action = do
    temporary <- getTemporaryDirectory
    bracket (do
        (path, handle) <- openTempFile temporary "i"
        hClose handle
        removeFile path
        createDirectory path
        pure path) removeDirectoryRecursive action

withInboxDirectory :: FilePath -> IO a -> IO a
withInboxDirectory directory action =
    bracket
        (lookupEnv variable <* setEnv variable directory)
        (maybe (unsetEnv variable) (setEnv variable))
        (const action)
  where
    variable = "HASKELL_AGENT_INBOX_DIRECTORY"
