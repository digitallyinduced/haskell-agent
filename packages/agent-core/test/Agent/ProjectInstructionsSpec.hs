module Agent.ProjectInstructionsSpec (spec) where

import Agent.ProjectInstructions
import System.OsPath (unsafeEncodeUtf)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception.Safe (bracket)
import System.Directory
    ( createDirectoryIfMissing
    , getTemporaryDirectory
    , removeDirectoryRecursive
    )
import System.FilePath ((</>))
import System.IO (IOMode(AppendMode), hClose, openFile)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

fromFilePath = unsafeEncodeUtf

spec :: Spec
spec = describe "Agent.ProjectInstructions" do
    describe "discoverProjectInstructions" do
        it "loads root to cwd files with deeper last" do
            withTempDir \dir -> do
                createDirectoryIfMissing True (dir </> ".git")
                createDirectoryIfMissing True (dir </> "pkg" </> "src")
                writeFile (dir </> "AGENTS.md") "root rules\n"
                writeFile (dir </> "pkg" </> "AGENTS.md") "pkg rules\n"
                writeFile (dir </> "pkg" </> "src" </> "AGENTS.md") "src rules\n"
                loaded <- discoverProjectInstructions defaultDiscoverOptions
                    (fromFilePath (dir </> "pkg" </> "src"))
                map (.instructionContent) (loadedInstructionFiles loaded)
                    `shouldBe` ["root rules\n", "pkg rules\n", "src rules\n"]

        it "prefers AGENTS.override.md in a directory" do
            withTempDir \dir -> do
                createDirectoryIfMissing True (dir </> ".git")
                writeFile (dir </> "AGENTS.md") "base\n"
                writeFile (dir </> "AGENTS.override.md") "override\n"
                loaded <- discoverProjectInstructions defaultDiscoverOptions (fromFilePath dir)
                map (.instructionContent) loaded.loadedProject `shouldBe` ["override\n"]

        it "waits for a transient lock instead of dropping instructions" do
            withTempDir checkLockedInstructions

        it "loads a global home file before project files" do
            withTempDir \dir -> do
                createDirectoryIfMissing True (dir </> ".git")
                createDirectoryIfMissing True (dir </> "home")
                writeFile (dir </> "home" </> "AGENTS.md") "global\n"
                writeFile (dir </> "AGENTS.md") "project\n"
                let options = defaultDiscoverOptions
                        { discoverGlobalDir = Just (fromFilePath (dir </> "home")) }
                loaded <- discoverProjectInstructions options (fromFilePath dir)
                fmap (.instructionContent) loaded.loadedGlobal `shouldBe` Just "global\n"
                map (.instructionContent) loaded.loadedProject `shouldBe` ["project\n"]

        it "falls back to cwd only when no root marker exists" do
            withTempDir \dir -> do
                createDirectoryIfMissing True (dir </> "nested")
                writeFile (dir </> "AGENTS.md") "outside\n"
                writeFile (dir </> "nested" </> "AGENTS.md") "nested\n"
                loaded <- discoverProjectInstructions defaultDiscoverOptions
                    (fromFilePath (dir </> "nested"))
                map (.instructionContent) loaded.loadedProject `shouldBe` ["nested\n"]

        it "skips empty files and truncates to the byte budget" do
            withTempDir \dir -> do
                createDirectoryIfMissing True (dir </> ".git")
                createDirectoryIfMissing True (dir </> "nested")
                writeFile (dir </> "AGENTS.md") "   \n"
                writeFile (dir </> "nested" </> "AGENTS.md") "abcdefghij"
                let options = defaultDiscoverOptions { discoverMaxBytes = 4 }
                loaded <- discoverProjectInstructions options (fromFilePath (dir </> "nested"))
                map (.instructionContent) loaded.loadedProject `shouldBe` ["abcd"]

        it "counts UTF-8 bytes rather than Unicode code points" do
            withTempDir \dir -> do
                createDirectoryIfMissing True (dir </> ".git")
                writeFile (dir </> "AGENTS.md") "ééa"
                let options = defaultDiscoverOptions { discoverMaxBytes = 3 }
                loaded <- discoverProjectInstructions options (fromFilePath dir)
                map (.instructionContent) loaded.loadedProject `shouldBe` ["é"]

        it "drops an incomplete trailing UTF-8 code point" do
            withTempDir \dir -> do
                createDirectoryIfMissing True (dir </> ".git")
                writeFile (dir </> "AGENTS.md") "a😀b"
                let options = defaultDiscoverOptions { discoverMaxBytes = 4 }
                loaded <- discoverProjectInstructions options (fromFilePath dir)
                map (.instructionContent) loaded.loadedProject `shouldBe` ["a"]

        it "disables discovery when max bytes is zero" do
            withTempDir \dir -> do
                createDirectoryIfMissing True (dir </> ".git")
                writeFile (dir </> "AGENTS.md") "rules\n"
                let options = defaultDiscoverOptions { discoverMaxBytes = 0 }
                loaded <- discoverProjectInstructions options (fromFilePath dir)
                loadedInstructionFiles loaded `shouldBe` []

checkLockedInstructions :: FilePath -> IO ()
checkLockedInstructions dir = do
    createDirectoryIfMissing True (dir </> ".git")
    let path = dir </> "AGENTS.md"
    writeFile path "locked rules\n"
    handle <- openFile path AppendMode
    _ <- forkIO do
        threadDelay 5000
        hClose handle
    loaded <- discoverProjectInstructions defaultDiscoverOptions (fromFilePath dir)
    map (.instructionContent) loaded.loadedProject `shouldBe` ["locked rules\n"]

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
    tmp <- getTemporaryDirectory
    bracket
        (mkdtemp (tmp </> "agent-agents-md-XXXXXX"))
        removeDirectoryRecursive
        action
