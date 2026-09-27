module Agent.Tools.EnvironmentSpec (spec) where

import Agent.Cancel (requestCancel, resetCancel)
import Agent.OsPath (unsafeToFilePath)
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , ToolDispatchConfig(..)
    , customToolCall
    , dispatchToolCall
    )
import Agent.Tools.Environment
    ( newEnvironmentToolWithNix
    , restoreShellEnvironment
    )
import Agent.Tools.IO
    ( CommandResult(..)
    , RunningCommand(..)
    , runShellCommand
    , startShellCommandWithInput
    , stopShellCommand
    , writeShellCommandInput
    )
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , ShellEnvironment(..)
    , ToolEnv(..)
    , ToolExecutionPolicy(..)
    , ToolSchema(..)
    , defaultToolEnv
    , setToolSessionTmp
    )
import Control.Exception.Safe (bracket)
import Control.Concurrent.Async (withAsync, wait)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import Data.IORef (readIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory
    ( canonicalizePath
    , createDirectory
    , doesFileExist
    , getTemporaryDirectory
    , listDirectory
    , removeDirectoryRecursive
    )
import System.FilePath ((</>), takeDirectory, takeExtension)
import System.OsPath (unsafeEncodeUtf)
import System.Posix.Files (setFileMode)
import System.Posix.Temp (mkdtemp)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "Agent.Tools.Environment" do
    it "advertises raw Nix input, mutation approval, and sequential execution" do
        withEnvironment \_ _ _ tool -> do
            tool.appToolName `shouldBe` "set_environment"
            case tool.appToolSchema of
                FreeformGrammarSchema syntax _ -> syntax `shouldBe` "lark"
                _ -> expectationFailure "expected a freeform tool rather than JSON"
            case tool.appToolApproval of
                AlwaysPrompt -> pure ()
                _ -> expectationFailure "environment changes must require mutation approval"
            tool.appToolExecution `shouldBe` TurnSequential

    it "preserves plain Nix source verbatim and activates subsequent shell commands" do
        withEnvironment \directory _ env tool -> do
            output <- runTool tool firstExpression
            output `shouldNotSatisfy` Text.isPrefixOf "ERR "
            active <- requireActive env
            let revision = takeDirectory (unsafeToFilePath active.environmentProfile)
            files <- listDirectory revision
            sources <- traverse (Text.readFile . (revision </>))
                [name | name <- files, takeExtension name == ".nix", name /= "flake.nix"]
            sources `shouldContain` [firstExpression]
            result <- runShellCommand env env.toolCwd
                "printf '%s|%s' \"$AGENT_ENVIRONMENT_TEST_VALUE\" \"$TMPDIR\"" 5000
            result.commandExitCode `shouldBe` Just 0
            result.commandStdout `shouldBe` "revision-one|" <> Text.pack directory
            doesFileExist (directory </> "must-not-be-created") `shouldReturn` False

    it "replaces rather than layers the previous revision" do
        withEnvironment \_ _ env tool -> do
            runTool tool firstExpression
            previous <- requireActive env
            runTool tool secondExpression
            current <- requireActive env
            current.environmentProfile `shouldNotBe` previous.environmentProfile
            shellValue env `shouldReturn` "revision-two"

    it "serializes separate parent and child tools and publishes one shared revision" do
        withEnvironment \directory executable parent parentTool -> do
            freshChild <- defaultToolEnv parent.toolCwd
            let child = freshChild
                    { toolSessionTmp = parent.toolSessionTmp
                    , toolShellEnvironment = parent.toolShellEnvironment
                    , toolShellEnvironmentLock = parent.toolShellEnvironmentLock
                    }
            childTool <- newEnvironmentToolWithNix child executable
            parentEntered <- newEmptyMVar
            releaseParent <- newEmptyMVar
            childStarted <- newEmptyMVar
            childEntered <- newEmptyMVar
            let parentOutput output =
                    if output == "Building candidate Nix environment…"
                        then putMVar parentEntered () >> takeMVar releaseParent
                        else pure ()
                childOutput output =
                    if output == "Building candidate Nix environment…"
                        then putMVar childEntered ()
                        else pure ()
            completed <- timeout 10000000 $
                withAsync (runToolWithOutput parentOutput parentTool firstExpression) \parentCall -> do
                    takeMVar parentEntered
                    withAsync (putMVar childStarted () >> runToolWithOutput childOutput childTool secondExpression) \childCall -> do
                        takeMVar childStarted
                        timeout 100000 (readMVar childEntered) `shouldReturn` Nothing
                        putMVar releaseParent ()
                        wait parentCall >>= (`shouldNotSatisfy` Text.isPrefixOf "ERR ")
                        wait childCall >>= (`shouldNotSatisfy` Text.isPrefixOf "ERR ")
            completed `shouldBe` Just ()
            shellValue parent `shouldReturn` "revision-two"
            shellValue child `shouldReturn` "revision-two"
            parentActive <- requireActive parent
            childActive <- requireActive child
            childActive.environmentProfile `shouldBe` parentActive.environmentProfile
            resumed <- defaultToolEnv parent.toolCwd
            setToolSessionTmp resumed (Just (unsafeEncodeUtf directory))
            restoreShellEnvironment resumed
            restored <- requireActive resumed
            restored.environmentProfile `shouldBe` parentActive.environmentProfile
            shellValue resumed `shouldReturn` "revision-two"

    it "shares parent activation with an existing child and preserves it after child failure" do
        withEnvironment \_ executable parent parentTool -> do
            freshChild <- defaultToolEnv parent.toolCwd
            let child = freshChild
                    { toolSessionTmp = parent.toolSessionTmp
                    , toolShellEnvironment = parent.toolShellEnvironment
                    , toolShellEnvironmentLock = parent.toolShellEnvironmentLock
                    }
            childTool <- newEnvironmentToolWithNix child executable
            runTool parentTool firstExpression
            shellValue child `shouldReturn` "revision-one"
            output <- runTool childTool "{ pkgs }: throw \"ENVIRONMENT_TEST_FAILURE\""
            output `shouldSatisfy` Text.isPrefixOf "ERR "
            shellValue parent `shouldReturn` "revision-one"
            shellValue child `shouldReturn` "revision-one"

    it "leaves the active revision intact after a failed realization" do
        withEnvironment \_ _ env tool -> do
            runTool tool firstExpression
            previous <- requireActive env
            output <- runTool tool "{ pkgs }: throw \"ENVIRONMENT_TEST_FAILURE\""
            output `shouldSatisfy` Text.isInfixOf "simulated realization failure"
            current <- requireActive env
            current.environmentProfile `shouldBe` previous.environmentProfile
            shellValue env `shouldReturn` "revision-one"

    it "keeps an existing managed process on its original environment" do
        withEnvironment \_ _ env tool -> do
            runTool tool firstExpression
            let start = startShellCommandWithInput env env.toolCwd
                    "IFS= read -r ignored; printf '%s' \"$AGENT_ENVIRONMENT_TEST_VALUE\""
                    >>= either (fail . Text.unpack) pure
            bracket start stopShellCommand \running -> do
                runTool tool secondExpression
                writeShellCommandInput running "continue\n" `shouldReturn` Right ()
                completed <- timeout 5000000 (readMVar running.runningResult)
                fmap (.commandExitCode) completed `shouldBe` Just (Just 0)
                fmap (.commandStdout) completed `shouldBe` Just "revision-one"
                shellValue env `shouldReturn` "revision-two"

    it "does not activate a cancelled candidate" do
        withEnvironment \_ _ env tool -> do
            runTool tool firstExpression
            previous <- requireActive env
            requestCancel env.toolCancel
            runTool tool secondExpression
            resetCancel env.toolCancel
            current <- requireActive env
            current.environmentProfile `shouldBe` previous.environmentProfile
            shellValue env `shouldReturn` "revision-one"

    it "restores the activated revision when the same session is resumed" do
        withEnvironment \directory _ env tool -> do
            runTool tool firstExpression
            previous <- requireActive env
            resumed <- defaultToolEnv env.toolCwd
            setToolSessionTmp resumed (Just (unsafeEncodeUtf directory))
            restoreShellEnvironment resumed
            restored <- requireActive resumed
            restored.environmentProfile `shouldBe` previous.environmentProfile
            shellValue resumed `shouldReturn` "revision-one"

    it "does not share an environment with another session or after a session reset" do
        withEnvironment \directory _ env tool -> do
            runTool tool firstExpression
            let otherDirectory = directory </> "other-session"
            createDirectory otherDirectory
            other <- defaultToolEnv env.toolCwd
            setToolSessionTmp other (Just (unsafeEncodeUtf otherDirectory))
            restoreShellEnvironment other
            shellValue other `shouldReturn` "unset"
            setToolSessionTmp env (Just (unsafeEncodeUtf otherDirectory))
            shellValue env `shouldReturn` "unset"

    it "rejects empty input without creating an active environment" do
        withEnvironment \_ _ env tool -> do
            output <- runTool tool " \n "
            output `shouldSatisfy` Text.isPrefixOf "ERR "
            readIORef env.toolShellEnvironment >>= (`shouldSatisfy` (not . isJust))

    it "requires a session directory without falling back to shared temporary storage" do
        withEnvironment \_ _ env tool -> do
            setToolSessionTmp env Nothing
            output <- runTool tool firstExpression
            output `shouldSatisfy` Text.isPrefixOf "ERR "
            readIORef env.toolShellEnvironment >>= (`shouldSatisfy` (not . isJust))

firstExpression :: Text
firstExpression = Text.unlines
    [ "{ pkgs }: pkgs.mkShell {"
    , "  AGENT_ENVIRONMENT_TEST_VALUE = \"revision-one\";"
    , "  # $(touch must-not-be-created) ' quotation remains data"
    , "}"
    ]

secondExpression :: Text
secondExpression =
    "{ pkgs }: pkgs.mkShell { AGENT_ENVIRONMENT_TEST_VALUE = \"revision-two\"; }"

requireActive :: ToolEnv -> IO ShellEnvironment
requireActive env =
    readIORef env.toolShellEnvironment >>= \case
        Just active -> pure active
        Nothing -> fail "expected an activated shell environment"

shellValue :: ToolEnv -> IO Text
shellValue env = do
    result <- runShellCommand env env.toolCwd
        "printf '%s' \"${AGENT_ENVIRONMENT_TEST_VALUE-unset}\"" 5000
    result.commandExitCode `shouldBe` Just 0
    pure result.commandStdout

withEnvironment :: (FilePath -> FilePath -> ToolEnv -> AppTool -> IO a) -> IO a
withEnvironment action = do
    root <- getTemporaryDirectory
    bracket
        (mkdtemp (root </> "agent-environment-test-") >>= canonicalizePath)
        removeDirectoryRecursive
        \workspace -> do
            let directory = workspace </> "session environment's directory"
                executable = directory </> "nix-test-executable"
            createDirectory directory
            Text.writeFile executable simulatedNix
            setFileMode executable 0o700
            env <- defaultToolEnv (unsafeEncodeUtf directory)
            setToolSessionTmp env (Just (unsafeEncodeUtf directory))
            tool <- newEnvironmentToolWithNix env executable
            action directory executable env tool

-- Exercise the real process path without network downloads or global PATH
-- mutations. The executable recognizes the Nix develop protocol and stores
-- a deterministic value in each profile, not a real Nix derivation.
simulatedNix :: Text
simulatedNix = Text.unlines
    [ "#!/bin/sh"
    , "set -eu"
    , "profile=''"
    , "source_directory=''"
    , "installable=''"
    , "while [ \"$#\" -gt 0 ]; do"
    , "  case \"$1\" in"
    , "    --profile) profile=$2; shift 2 ;;"
    , "    path:*) source_directory=${1#path:}; source_directory=${source_directory%#*}; shift ;;"
    , "    --command|-c) shift; break ;;"
    , "    /*) installable=$1; shift ;;"
    , "    *) shift ;;"
    , "  esac"
    , "done"
    , "if [ -n \"$profile\" ]; then"
    , "  source_text=''"
    , "  for expression in \"$source_directory\"/*.nix; do"
    , "    case \"$expression\" in */flake.nix) continue ;; esac"
    , "    source_text=\"$source_text$(cat \"$expression\")\""
    , "  done"
    , "  case \"$source_text\" in"
    , "    *ENVIRONMENT_TEST_FAILURE*) printf 'simulated realization failure\\n' >&2; exit 1 ;;"
    , "    *revision-two*) value=revision-two ;;"
    , "    *) value=revision-one ;;"
    , "  esac"
    , "  printf '%s' \"$value\" > \"$profile\""
    , "else"
    , "  AGENT_ENVIRONMENT_TEST_VALUE=$(cat \"$installable\")"
    , "  export AGENT_ENVIRONMENT_TEST_VALUE"
    , "fi"
    , "exec \"$@\""
    ]

runTool :: AppTool -> Text -> IO Text
runTool = runToolWithOutput (const (pure ()))

runToolWithOutput :: (Text -> IO ()) -> AppTool -> Text -> IO Text
runToolWithOutput onOutput tool source = do
    result <- dispatchToolCall
        testDispatchConfig { toolDispatchOnOutput = \_ output -> onOutput output }
        [tool.appToolHandler]
        (customToolCall "environment-1" "set_environment" source)
    pure result.output

testDispatchConfig :: ToolDispatchConfig
testDispatchConfig = ToolDispatchConfig
    { toolDispatchUnknownTool = \name -> "unknown:" <> name
    , toolDispatchFormatResult = either ("ERR " <>) id
    , toolDispatchFormatException = \name exception -> "EX " <> name <> ": " <> Text.pack (show exception)
    , toolDispatchOnException = \_ _ -> pure ()
    , toolDispatchOnOutput = \_ _ -> pure ()
    , toolDispatchFinalizeOutput = \_ output -> pure output
    }
