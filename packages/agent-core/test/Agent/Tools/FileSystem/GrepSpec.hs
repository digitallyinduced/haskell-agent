module Agent.Tools.FileSystem.GrepSpec (spec) where

import Agent.ToolDispatch
    ( ToolCallResult(..)
    , ToolDispatchConfig(..)
    , dispatchToolCall
    , functionToolCall
    )
import Agent.Tools.FileSystem.Grep (grepTool)
import Agent.Tools.Types (AppTool(..), defaultToolEnv)
import Control.Exception.Safe (bracket)
import qualified Data.Text as Text
import System.Directory
    ( createDirectory
    , findExecutable
    , getTemporaryDirectory
    , removeDirectoryRecursive
    )
import System.FilePath ((</>))
import System.OsPath (unsafeEncodeUtf)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

spec :: Spec
spec = describe "grepTool" do
    it "renders matches with workspace-relative paths" do
        findExecutable "rg" >>= \case
            Nothing -> pendingWith "rg is not installed"
            Just _ -> withTempDir \dir -> do
                let workspace = dir </> "workspace"
                    sourceDir = workspace </> "src"
                createDirectory workspace
                createDirectory sourceDir
                writeFile (sourceDir </> "Example.hs") "needle\n"
                env <- defaultToolEnv (unsafeEncodeUtf workspace)
                result <- dispatchToolCall testConfig
                    [(grepTool env).appToolHandler]
                    (functionToolCall "grep-1" "grep"
                        "{\"pattern\":\"needle\"}")
                result.output `shouldBe` Text.intercalate "\n"
                    [ "<workspace_result>"
                    , "src/Example.hs"
                    , "1:needle"
                    , "</workspace_result>"
                    ]
    it "bounds truncated output without retaining all matching lines" do
        findExecutable "rg" >>= \case
            Nothing -> pendingWith "rg is not installed"
            Just _ -> withTempDir \dir -> do
                let workspace = dir </> "workspace"
                createDirectory workspace
                writeFile (workspace </> "many.txt")
                    (unlines (replicate 10000 "needle"))
                env <- defaultToolEnv (unsafeEncodeUtf workspace)
                result <- dispatchToolCall testConfig
                    [(grepTool env).appToolHandler]
                    (functionToolCall "grep-2" "grep"
                        "{\"pattern\":\"needle\",\"head_limit\":2}")
                result.output `shouldContain`
                    "[at least 2 lines; output truncated]"
                result.output `shouldContain` "many.txt"

    it "reports an invalid regular expression without leaking a child process" do
        findExecutable "rg" >>= \case
            Nothing -> pendingWith "rg is not installed"
            Just _ -> withTempDir \dir -> do
                let workspace = dir </> "workspace"
                createDirectory workspace
                writeFile (workspace </> "one.txt") "needle\n"
                env <- defaultToolEnv (unsafeEncodeUtf workspace)
                result <- dispatchToolCall testConfig
                    [(grepTool env).appToolHandler]
                    (functionToolCall "grep-3" "grep"
                        "{\"pattern\":\"[\"}")
                result.output `shouldContain` "ERR"

testConfig :: ToolDispatchConfig
testConfig = ToolDispatchConfig
    { toolDispatchUnknownTool = \name -> "unknown:" <> name
    , toolDispatchFormatResult = either ("ERR " <>) id
    , toolDispatchFormatException = \name _ -> "EX " <> name
    , toolDispatchOnException = \_ _ -> pure ()
    , toolDispatchOnOutput = \_ _ -> pure ()
    , toolDispatchFinalizeOutput = \_call output -> pure output
    }

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
    base <- getTemporaryDirectory
    bracket
        (mkdtemp (base </> "agent-core-grep-"))
        removeDirectoryRecursive
        action
