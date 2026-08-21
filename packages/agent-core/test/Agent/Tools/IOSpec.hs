module Agent.Tools.IOSpec (spec) where

import Agent.Cancel (requestCancel)
import Agent.FileRetry
    ( appendLazyFileRetryingOpen
    , retryOnFileBusy
    , writeLazyFileAtomically
    )
import Agent.OsPath (fromFilePath)
import Agent.Tools.IO
    ( CommandResult(..)
    , RunningCommand(..)
    , readTextFile
    , resolveUnderCwd
    , runShellCommand
    , startShellCommand
    , writeTextFile
    )
import Agent.Tools.Types (ToolEnv(..), defaultToolEnv)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.MVar (readMVar)
import Control.Exception.Safe (bracket, tryIO)
import Control.Monad (replicateM)
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Either (isLeft, isRight)
import Data.IORef
import Data.List (sort)
import qualified Data.Text as Text
import System.Directory
    ( canonicalizePath
    , createDirectory
    , createDirectoryLink
    , getTemporaryDirectory
    , listDirectory
    , removeDirectoryRecursive
    )
import System.FilePath ((</>))
import System.IO (IOMode(..), hClose, openFile)
import System.IO.Error (alreadyInUseErrorType, mkIOError)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

spec :: Spec
spec = describe "Agent.Tools.IO" do
    it "retries only resource-busy IO exceptions" do
        attempts <- newIORef (0 :: Int)
        retryOnFileBusy do
            attempt <- atomicModifyIORef' attempts \current ->
                let next = current + 1
                in (next, next)
            if attempt < 3
                then ioError (mkIOError alreadyInUseErrorType "busy" Nothing Nothing)
                else pure ()
        readIORef attempts `shouldReturn` 3

    it "does not retry unrelated IO exceptions" do
        attempts <- newIORef (0 :: Int)
        result <- tryIO do
            retryOnFileBusy do
                modifyIORef' attempts (+ 1)
                ioError (userError "not retryable") :: IO ()
        result `shouldSatisfy` isLeft
        readIORef attempts `shouldReturn` 1

    it "atomically replaces a file from concurrent writers" do
        withTempDir checkConcurrentAtomicWrites

    it "appends each concurrent payload exactly once" do
        withTempDir checkConcurrentAppends

    it "retries a write after a transient GHC file lock" do
        withTempDir \dir -> do
            let path = dir </> "held.txt"
            writeFile path "old\n"
            h <- openFile path AppendMode
            _ <- forkIO do
                threadDelay 5000
                hClose h
            writeTextFile (fromFilePath path) "new\n" `shouldReturn` Right ()
            readTextFile (fromFilePath path) `shouldReturn` Right "new\n"

    it "gives up when the GHC file lock is held for the whole retry window" do
        withTempDir \dir -> do
            let path = dir </> "held.txt"
            writeFile path "old\n"
            bracket (openFile path ReadMode) hClose \_ -> do
                result <- writeTextFile (fromFilePath path) "new\n"
                result `shouldSatisfy` isLeft
                result `shouldSatisfy` either (Text.isInfixOf "resource busy") (const False)

    it "lets several threads write the same file without a lock error" do
        withTempDir \dir -> do
            let path = dir </> "race.txt"
            writeFile path "start\n"
            vars <- replicateM 8 newEmptyMVar
            mapM_
                (\(i, var) -> forkIO $
                    writeTextFile (fromFilePath path) (Text.pack (show i) <> "\n") >>= putMVar var)
                (zip [1 :: Int ..] vars)
            results <- mapM takeMVar vars
            results `shouldBe` replicate 8 (Right ())

    it "reads the same file from many threads at once" do
        withTempDir \dir -> do
            let path = dir </> "shared.txt"
                body = Text.replicate 2000 "concurrent-read\n"
            writeTextFile (fromFilePath path) body `shouldReturn` Right ()
            vars <- replicateM 32 newEmptyMVar
            mapM_ (\var -> forkIO $ readTextFile (fromFilePath path) >>= putMVar var) vars
            results <- mapM takeMVar vars
            results `shouldBe` replicate 32 (Right body)

    it "rejects missing descendants below a symlink that escapes cwd" do
        withTempDir \dir -> do
            let workspace = dir </> "workspace"
                outside = dir </> "outside"
            createDirectory workspace
            createDirectory outside
            createDirectoryLink outside (workspace </> "link")
            env <- defaultToolEnv (fromFilePath workspace)
            result <- resolveUnderCwd env
                (fromFilePath ("link" </> "missing" </> "file.txt"))
            result `shouldSatisfy` isLeft

    it "preserves parent-segment semantics after a directory symlink" do
        withTempDir \dir -> do
            let workspace = dir </> "workspace"
                targetParent = workspace </> "a"
                target = targetParent </> "b"
            createDirectory workspace
            createDirectory targetParent
            createDirectory target
            createDirectoryLink target (workspace </> "link")
            env <- defaultToolEnv (fromFilePath workspace)
            result <- resolveUnderCwd env
                (fromFilePath ("link" </> ".." </> "file.txt"))
            canonicalParent <- canonicalizePath targetParent
            result `shouldBe` Right
                (fromFilePath (canonicalParent </> "file.txt"))

    it "cancels a long-running shell command via toolCancel" do
        withTempDir \dir -> do
            let osDir = fromFilePath dir
            env@ToolEnv{toolCancel} <- defaultToolEnv osDir
            done <- newEmptyMVar
            _ <- forkIO do
                result <- runShellCommand env osDir "sleep 30" 60000
                putMVar done result
            threadDelay 100000
            requestCancel toolCancel
            CommandResult{commandCancelled, commandTimedOut} <- takeMVar done
            commandCancelled `shouldBe` True
            commandTimedOut `shouldBe` False

    it "collects both output streams from a background shell command" do
        withTempDir checkBackgroundOutput

checkBackgroundOutput dir = do
    let osDir = fromFilePath dir
    env <- defaultToolEnv osDir
    Right running <-
        startShellCommand env osDir "printf stdout; printf stderr >&2"
    result <- readMVar running.runningResult
    result.commandExitCode `shouldBe` Just 0
    result.commandStdout `shouldBe` "stdout"
    result.commandStderr `shouldBe` "stderr"

checkConcurrentAtomicWrites :: FilePath -> IO ()
checkConcurrentAtomicWrites dir = do
    let path = fromFilePath (dir </> "state.json")
        payloads = map (LBS8.pack . show) [1 :: Int .. 16]
    vars <- replicateM (length payloads) newEmptyMVar
    mapM_ (startAtomicWrite path) (zip payloads vars)
    results <- mapM takeMVar vars
    results `shouldSatisfy` all isRight
    final <- LBS8.readFile (dir </> "state.json")
    final `shouldSatisfy` (`elem` payloads)
    leftovers <- filter (Text.isInfixOf ".tmp" . Text.pack)
        <$> listDirectory dir
    leftovers `shouldBe` []

startAtomicWrite path (payload, var) =
    forkIO $
        tryIO (writeLazyFileAtomically path 0o600 payload) >>= putMVar var

checkConcurrentAppends :: FilePath -> IO ()
checkConcurrentAppends dir = do
    let file = dir </> "transcript.jsonl"
        path = fromFilePath file
        payloads = map (LBS8.pack . (<> "\n") . show) [1 :: Int .. 16]
    writeFile file ""
    vars <- replicateM (length payloads) newEmptyMVar
    mapM_ (startAppend path) (zip payloads vars)
    results <- mapM takeMVar vars
    results `shouldSatisfy` all isRight
    linesWritten <- sort . LBS8.lines <$> LBS8.readFile file
    linesWritten `shouldBe` sort (map LBS8.init payloads)

startAppend path (payload, var) =
    forkIO $
        tryIO (appendLazyFileRetryingOpen path payload) >>= putMVar var

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
    tmp <- getTemporaryDirectory
    bracket
        (mkdtemp (tmp </> "agent-io-XXXXXX"))
        removeDirectoryRecursive
        action
