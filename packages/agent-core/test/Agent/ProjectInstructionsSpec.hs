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
    describe "nonEmptyInstructionContent" do
        it "keeps original non-empty content and rejects whitespace-only files" do
            nonEmptyInstructionContent
                (InstructionFile (fromFilePath "/repo/AGENTS.md") " rules \n")
                `shouldBe` Just " rules \n"
            nonEmptyInstructionContent
                (InstructionFile (fromFilePath "/repo/EMPTY.md") " \t\n")
                `shouldBe` Nothing

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

        it "loads Grok-compatible filenames and sorted project rules" do
            withTempDir \dir -> do
                createDirectoryIfMissing True (dir </> ".git")
                createDirectoryIfMissing True (dir </> ".claude")
                createDirectoryIfMissing True (dir </> ".grok" </> "rules")
                createDirectoryIfMissing True (dir </> ".cursor" </> "rules")
                writeFile (dir </> "AGENTS.md") "agents\n"
                writeFile (dir </> "Claude.md") "claude\n"
                writeFile (dir </> "AGENT.md") "agent\n"
                writeFile (dir </> ".claude" </> "CLAUDE.local.md") "claude local\n"
                writeFile (dir </> ".grok" </> "rules" </> "b.md") "grok b\n"
                writeFile (dir </> ".grok" </> "rules" </> "a.md") "grok a\n"
                writeFile (dir </> ".grok" </> "rules" </> "ignored.txt") "ignored\n"
                writeFile (dir </> ".cursor" </> "rules" </> "c.md") "cursor c\n"
                loaded <- discoverProjectInstructions defaultDiscoverOptions
                    (fromFilePath dir)
                map (.instructionContent) loaded.loadedProject `shouldBe`
                    [ "agents\n"
                    , "claude\n"
                    , "agent\n"
                    , "claude local\n"
                    , "grok a\n"
                    , "grok b\n"
                    , "cursor c\n"
                    ]

        it "keeps Codex discovery narrow when the global home is .codex" do
            withTempDir \dir -> do
                let home = dir </> ".codex"
                createDirectoryIfMissing True (dir </> ".git")
                createDirectoryIfMissing True home
                createDirectoryIfMissing True (dir </> ".grok" </> "rules")
                writeFile (home </> "AGENTS.md") "global agents\n"
                writeFile (home </> "Claude.md") "global claude\n"
                writeFile (dir </> "AGENTS.md") "project agents\n"
                writeFile (dir </> "Claude.md") "project claude\n"
                writeFile (dir </> ".grok" </> "rules" </> "rule.md") "rule\n"
                let options = defaultDiscoverOptions
                        { discoverGlobalDir = Just (fromFilePath home) }
                loaded <- discoverProjectInstructions options (fromFilePath dir)
                map (.instructionContent) (loadedInstructionFiles loaded)
                    `shouldBe` ["global agents\n", "project agents\n"]

        it "loads Grok, Claude, and Cursor home instructions before project files" do
            withTempDir \dir -> do
                let home = dir </> "home"
                    grokHome = home </> ".grok"
                createDirectoryIfMissing True (dir </> ".git")
                createDirectoryIfMissing True (grokHome </> "rules")
                createDirectoryIfMissing True (home </> ".claude" </> "rules")
                createDirectoryIfMissing True (home </> ".cursor" </> "rules")
                writeFile (grokHome </> "AGENTS.md") "grok agents\n"
                writeFile (grokHome </> "rules" </> "a.md") "grok rule\n"
                writeFile (home </> ".claude" </> "Claude.md") "claude agents\n"
                writeFile (home </> ".claude" </> "rules" </> "b.md") "claude rule\n"
                writeFile (home </> ".cursor" </> "rules" </> "c.md") "cursor rule\n"
                writeFile (dir </> "AGENTS.md") "project\n"
                let options = defaultDiscoverOptions
                        { discoverGlobalDir = Just (fromFilePath grokHome) }
                loaded <- discoverProjectInstructions options (fromFilePath dir)
                map (.instructionContent) (loadedInstructionFiles loaded)
                    `shouldBe`
                        [ "grok agents\n"
                        , "grok rule\n"
                        , "claude agents\n"
                        , "claude rule\n"
                        , "cursor rule\n"
                        , "project\n"
                        ]

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
