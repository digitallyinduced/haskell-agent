module Agent.Tools.IOSpec (spec) where

import Agent.Tools.IO (readTextFile, writeTextFile)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception.Safe (bracket)
import Control.Monad (replicateM)
import Data.Either (isLeft)
import qualified Data.Text as Text
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO (IOMode(..), hClose, openFile)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

spec :: Spec
spec = describe "Agent.Tools.IO" do
    it "retries a write after a transient GHC file lock" do
        withTempDir \dir -> do
            let path = dir </> "held.txt"
            writeFile path "old\n"
            h <- openFile path AppendMode
            _ <- forkIO do
                threadDelay 5000
                hClose h
            writeTextFile path "new\n" `shouldReturn` Right ()
            readTextFile path `shouldReturn` Right "new\n"

    it "gives up when the GHC file lock is held for the whole retry window" do
        withTempDir \dir -> do
            let path = dir </> "held.txt"
            writeFile path "old\n"
            bracket (openFile path ReadMode) hClose \_ -> do
                result <- writeTextFile path "new\n"
                result `shouldSatisfy` isLeft
                result `shouldSatisfy` either (Text.isInfixOf "resource busy") (const False)

    it "lets several threads write the same file without a lock error" do
        withTempDir \dir -> do
            let path = dir </> "race.txt"
            writeFile path "start\n"
            vars <- replicateM 8 newEmptyMVar
            mapM_
                (\(i, var) -> forkIO $
                    writeTextFile path (Text.pack (show i) <> "\n") >>= putMVar var)
                (zip [1 :: Int ..] vars)
            results <- mapM takeMVar vars
            results `shouldBe` replicate 8 (Right ())

    it "reads the same file from many threads at once" do
        withTempDir \dir -> do
            let path = dir </> "shared.txt"
                body = Text.replicate 2000 "concurrent-read\n"
            writeTextFile path body `shouldReturn` Right ()
            vars <- replicateM 32 newEmptyMVar
            mapM_ (\var -> forkIO $ readTextFile path >>= putMVar var) vars
            results <- mapM takeMVar vars
            results `shouldBe` replicate 32 (Right body)

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
    tmp <- getTemporaryDirectory
    bracket
        (mkdtemp (tmp </> "agent-io-XXXXXX"))
        removeDirectoryRecursive
        action
