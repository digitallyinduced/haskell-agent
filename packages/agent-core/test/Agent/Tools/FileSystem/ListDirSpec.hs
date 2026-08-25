module Agent.Tools.FileSystem.ListDirSpec (spec) where

import Agent.OsPath (fromText, toText)
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , ToolDispatchConfig(..)
    , dispatchToolCall
    , functionToolCall
    )
import Agent.Tools.FileSystem.ListDir
    ( DirNode(..)
    , capNodes
    , listDirTool
    , renderTree
    )
import Agent.Tools.Types (AppTool(..), defaultToolEnv)
import Control.Exception.Safe (bracket)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory
    ( createDirectory
    , getTemporaryDirectory
    , removeDirectoryRecursive
    )
import System.FilePath ((</>))
import System.OsPath (unsafeEncodeUtf)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

spec :: Spec
spec = do
    describe "capNodes" do
        it "keeps a stub for an oversized directory and later siblings" do
            let packages =
                    DirectoryNode
                        (fromText "packages")
                        (map (FileNode . fromText . Text.pack . show) [1 .. 50 :: Int])
                tree =
                    [ FileNode (fromText "LICENSE")
                    , packages
                    , FileNode (fromText "patches")
                    , FileNode (fromText "scripts")
                    , FileNode (fromText "tests")
                    ]
                (shown, truncated) = capNodes 10 tree
            truncated `shouldBe` True
            map nodeName shown
                `shouldBe` ["LICENSE", "packages", "patches", "scripts", "tests"]
            case shown of
                _ : packagesNode : _ ->
                    childCount packagesNode `shouldSatisfy` (< 50)
                _ -> expectationFailure "expected packages to remain in the listing"

        it "includes a directory name even when no children fit" do
            let tree =
                    [ DirectoryNode
                        (fromText "packages")
                        [FileNode (fromText "a"), FileNode (fromText "b")]
                    ]
                (shown, truncated) = capNodes 1 tree
            truncated `shouldBe` True
            map nodeName shown `shouldBe` ["packages"]
            case shown of
                [packagesNode] -> childCount packagesNode `shouldBe` 0
                _ -> expectationFailure "expected only the packages stub"

        it "does not mark a fully visible tree as truncated" do
            let tree =
                    [ FileNode (fromText "a")
                    , DirectoryNode (fromText "docs") [FileNode (fromText "readme")]
                    ]
            capNodes 10 tree `shouldBe` (tree, False)

    describe "renderTree" do
        it "does not insert blank lines after nested directories" do
            let tree =
                    [ DirectoryNode
                        (fromText "docs")
                        [ DirectoryNode
                            (fromText "evals")
                            [FileNode (fromText "ghci-vs-bash.md")]
                        , FileNode (fromText "ghostty.md")
                        , FileNode (fromText "nixos.md")
                        ]
                    , FileNode (fromText "flake.lock")
                    ]
            renderTree 0 tree
                `shouldBe` Text.unlines
                    [ "- docs/"
                    , "  - evals/"
                    , "    - ghci-vs-bash.md"
                    , "  - ghostty.md"
                    , "  - nixos.md"
                    , "- flake.lock"
                    ]

    describe "listDirTool" do
        it "renders nested directories without extra blank lines" do
            withTempDir \dir -> do
                let workspace = dir </> "workspace"
                    docs = workspace </> "docs"
                    evals = docs </> "evals"
                createDirectory workspace
                createDirectory docs
                createDirectory evals
                writeFile (evals </> "ghci-vs-bash.md") "ok\n"
                writeFile (docs </> "ghostty.md") "ok\n"
                writeFile (workspace </> "flake.lock") "{}\n"
                env <- defaultToolEnv (unsafeEncodeUtf workspace)
                result <-
                    dispatchToolCall testConfig
                        [(listDirTool env).appToolHandler]
                        (functionToolCall "list-1" "list_dir"
                            "{\"target_directory\":\".\"}")
                result.output
                    `shouldBe` Text.unlines
                        [ "Directory listing for .:"
                        , "- docs/"
                        , "  - evals/"
                        , "    - ghci-vs-bash.md"
                        , "  - ghostty.md"
                        , "- flake.lock"
                        ]

nodeName :: DirNode -> Text
nodeName = \case
    FileNode name -> toText name
    DirectoryNode name _ -> toText name

childCount :: DirNode -> Int
childCount = \case
    FileNode _ -> 0
    DirectoryNode _ children -> length children

testConfig :: ToolDispatchConfig
testConfig = ToolDispatchConfig
    { toolDispatchUnknownTool = \name -> "unknown:" <> name
    , toolDispatchFormatResult = either ("ERR " <>) id
    , toolDispatchFormatException = \name _ -> "EX " <> name
    , toolDispatchOnException = \_ _ -> pure ()
    , toolDispatchOnOutput = \_ _ -> pure ()
    }

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
    base <- getTemporaryDirectory
    bracket
        (mkdtemp (base </> "agent-core-listdir-"))
        removeDirectoryRecursive
        action
