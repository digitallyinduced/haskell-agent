module Agent.Tools.FileSystem.GrepSpec (spec) where

import Agent.ToolDispatch
    ( ToolCallResult(..)
    , ToolDispatchConfig(..)
    , dispatchToolCall
    , functionToolCall
    )
import Agent.ToolDSL (PropertySchema(..))
import Agent.Tools.FileSystem.Grep (grepTool)
import Agent.Tools.Types
    ( AppTool(..)
    , ToolSchema(..)
    , defaultToolEnv
    )
import Control.Concurrent
    ( newEmptyMVar
    , takeMVar
    , threadDelay
    , tryPutMVar
    )
import Control.Concurrent.Async (cancel, wait, waitCatch, withAsync)
import Control.Exception.Safe (bracket)
import Control.Monad (void)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as Text
import System.Directory
    ( Permissions(..)
    , createDirectory
    , createDirectoryLink
    , doesFileExist
    , findExecutable
    , getPermissions
    , getTemporaryDirectory
    , removeDirectoryRecursive
    , setPermissions
    )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath (searchPathSeparator, (</>))
import System.OsPath (unsafeEncodeUtf)
import System.Posix.Temp (mkdtemp)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "grepTool" do
    it "advertises only the Codex grep contract" do
        env <- defaultToolEnv (unsafeEncodeUtf ".")
        case (grepTool env).appToolSchema of
            JsonFunctionSchema properties ->
                map (.propertyName) properties `shouldBe`
                    [ "pattern", "path", "glob", "-B", "-A", "-C", "-i"
                    , "type", "head_limit", "multiline"
                    ]
            schema -> expectationFailure
                ("unexpected grep schema: " <> show schema)

    it "rejects the retired output_mode argument" do
        env <- defaultToolEnv (unsafeEncodeUtf ".")
        result <- dispatchToolCall testConfig
            [(grepTool env).appToolHandler]
            (functionToolCall "grep-retired" "grep"
                "{\"pattern\":\"needle\",\"output_mode\":\"count\"}")
        result.output `shouldSatisfy`
            Text.isInfixOf "output_mode is not supported"

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
                result.output `shouldSatisfy`
                    Text.isInfixOf "[at least 2 lines; output truncated]"
                result.output `shouldSatisfy` Text.isInfixOf "many.txt"

    it "leniently decodes invalid UTF-8 in matching output" do
        findExecutable "rg" >>= \case
            Nothing -> pendingWith "rg is not installed"
            Just _ -> withTempDir \dir -> do
                let workspace = dir </> "workspace"
                createDirectory workspace
                BS.writeFile (workspace </> "invalid.txt")
                    (BS.pack [110, 101, 101, 100, 108, 101, 0xc3, 0x28, 10])
                env <- defaultToolEnv (unsafeEncodeUtf workspace)
                result <- dispatchToolCall testConfig
                    [(grepTool env).appToolHandler]
                    (functionToolCall "grep-utf8" "grep"
                        "{\"pattern\":\"needle\"}")
                result.output `shouldSatisfy` Text.isInfixOf "invalid.txt"

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
                result.output `shouldSatisfy` Text.isInfixOf "ERR"
    it "preserves no-match and context behavior" do
        findExecutable "rg" >>= \case
            Nothing -> pendingWith "rg is not installed"
            Just _ -> withTempDir \dir -> do
                let workspace = dir </> "workspace"
                    source = workspace </> "context.txt"
                createDirectory workspace
                writeFile source "before\nneedle\nafter\n"
                env <- defaultToolEnv (unsafeEncodeUtf workspace)
                noMatch <- dispatchToolCall testConfig
                    [(grepTool env).appToolHandler]
                    (functionToolCall "grep-4" "grep"
                        "{\"pattern\":\"absent\"}")
                noMatch.output `shouldBe` "No matches found."
                context <- dispatchToolCall testConfig
                    [(grepTool env).appToolHandler]
                    (functionToolCall "grep-5" "grep"
                        "{\"pattern\":\"needle\",\"-C\":1}")
                context.output `shouldSatisfy` Text.isInfixOf "before"
                context.output `shouldSatisfy` Text.isInfixOf "needle"
                context.output `shouldSatisfy` Text.isInfixOf "after"

    it "clamps non-positive head limits to one output line" do
        findExecutable "rg" >>= \case
            Nothing -> pendingWith "rg is not installed"
            Just _ -> withTempDir \dir -> do
                let workspace = dir </> "workspace"
                createDirectory workspace
                writeFile (workspace </> "many.txt") "needle\nneedle\n"
                env <- defaultToolEnv (unsafeEncodeUtf workspace)
                result <- dispatchToolCall testConfig
                    [(grepTool env).appToolHandler]
                    (functionToolCall "grep-limit" "grep"
                        "{\"pattern\":\"needle\",\"head_limit\":0}")
                result.output `shouldSatisfy`
                    Text.isInfixOf "[at least 1 lines; output truncated]"

    it "rejects an explicitly searched symlink that escapes the workspace" do
        findExecutable "rg" >>= \case
            Nothing -> pendingWith "rg is not installed"
            Just _ -> withTempDir \dir -> do
                let workspace = dir </> "workspace"
                    outside = dir </> "outside"
                    link = workspace </> "escape"
                createDirectory workspace
                createDirectory outside
                writeFile (outside </> "secret.txt") "needle\n"
                createDirectoryLink outside link
                env <- defaultToolEnv (unsafeEncodeUtf workspace)
                result <- dispatchToolCall testConfig
                    [(grepTool env).appToolHandler]
                    (functionToolCall "grep-escape" "grep" . Text.pack $
                        "{\"pattern\":\"needle\",\"path\":" <> show link <> "}")
                result.output `shouldSatisfy` Text.isPrefixOf "ERR "
                result.output `shouldNotSatisfy` Text.isInfixOf "secret.txt"

    it "preserves partial matches when rg later reports an error" do
        withTempDir \dir -> do
            let workspace = dir </> "workspace"
            createDirectory workspace
            withFakeRg dir
                "#!/bin/sh\n\
                \printf 'partial.txt\\n1:needle\\n'\n\
                \printf 'fatal diagnostic\\n' >&2\n\
                \exit 2\n"
                do
                    env <- defaultToolEnv (unsafeEncodeUtf workspace)
                    result <- dispatchToolCall testConfig
                        [(grepTool env).appToolHandler]
                        (functionToolCall "grep-partial" "grep"
                            "{\"pattern\":\"needle\"}")
                    result.output `shouldSatisfy` Text.isPrefixOf "ERR "
                    result.output `shouldSatisfy` Text.isInfixOf "partial.txt"
                    result.output `shouldSatisfy`
                        Text.isInfixOf "fatal diagnostic"

    it "publishes matching output while rg is still running" do
        withTempDir \dir -> do
            let workspace = dir </> "workspace"
                release = dir </> "release"
            createDirectory workspace
            withFakeRg dir
                ("#!/bin/sh\n\
                \printf 'first.txt\\n1:needle\\n'\n\
                \while [ ! -f " <> shellQuote release <> " ]; do\n\
                \  sleep 0.01\n\
                \done\n\
                \printf 'second.txt\\n1:needle\\n'\n")
                do
                    env <- defaultToolEnv (unsafeEncodeUtf workspace)
                    progress <- newEmptyMVar
                    let config = testConfig
                            { toolDispatchOnOutput = \_ output ->
                                void (tryPutMVar progress output)
                            }
                        action = dispatchToolCall config
                            [(grepTool env).appToolHandler]
                            (functionToolCall "grep-streaming" "grep"
                                "{\"pattern\":\"needle\"}")
                    withAsync action \worker -> do
                        first <- timeout 3000000 (takeMVar progress)
                        first `shouldSatisfy` maybe False
                            (Text.isInfixOf "first.txt")
                        first `shouldSatisfy` maybe False
                            (not . Text.isInfixOf "second.txt")
                        writeFile release ""
                        result <- wait worker
                        result.output `shouldSatisfy`
                            Text.isInfixOf "second.txt"

    it "terminates and joins rg when the grep call is cancelled" do
        withTempDir \dir -> do
            let workspace = dir </> "workspace"
                started = dir </> "started"
            createDirectory workspace
            withFakeRg dir
                ("#!/bin/sh\n\
                \printf started > " <> shellQuote started <> "\n\
                \trap 'exit 0' TERM\n\
                \while :; do sleep 1; done\n")
                do
                    env <- defaultToolEnv (unsafeEncodeUtf workspace)
                    let action = dispatchToolCall testConfig
                            [(grepTool env).appToolHandler]
                            (functionToolCall "grep-cancel" "grep"
                                "{\"pattern\":\"needle\"}")
                    withAsync action \worker -> do
                        waitForFile started `shouldReturn` True
                        cancel worker
                        joined <- timeout 3000000 (waitCatch worker)
                        joined `shouldSatisfy` isJust

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

withFakeRg :: FilePath -> String -> IO a -> IO a
withFakeRg dir script action = do
    let binDir = dir </> "bin"
        rgPath = binDir </> "rg"
    createDirectory binDir
    writeFile rgPath script
    permissions <- getPermissions rgPath
    setPermissions rgPath permissions { executable = True }
    oldPath <- lookupEnv "PATH"
    bracket
        (setEnv "PATH"
            (binDir <> [searchPathSeparator] <> fromMaybe "" oldPath))
        (const $ maybe (unsetEnv "PATH") (setEnv "PATH") oldPath)
        (const action)

waitForFile :: FilePath -> IO Bool
waitForFile path = go (100 :: Int)
  where
    go 0 = pure False
    go remaining = do
        exists <- doesFileExist path
        if exists
            then pure True
            else threadDelay 10000 >> go (remaining - 1)

shellQuote :: String -> String
shellQuote value = "'" <> concatMap escape value <> "'"
  where
    escape '\'' = "'\\''"
    escape char = [char]
