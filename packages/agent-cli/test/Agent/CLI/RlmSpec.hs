module Agent.CLI.RlmSpec (spec) where

import Agent.CLI.Rlm
import Agent.Loop (LoopResult(..), TokenUsage(..))
import Agent.Subagents
    ( beginRootTurn
    , closeSubagentRegistry
    , defaultSubagentConfig
    , newSubagentRegistry
    )
import Agent.Subagents.TaskPath (taskPathRoot)
import Agent.Tools.MultiAgents (MultiAgentContext(..))
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (bracket)
import Data.Text qualified as Text
import System.Directory
    ( doesFileExist
    , getTemporaryDirectory
    , removeDirectoryRecursive
    )
import System.FilePath ((</>))
import System.OsPath (unsafeEncodeUtf)
import System.Posix.Temp (mkdtemp)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.Rlm" do
    it "dispatches mailbox requests through the in-process subagent registry" do
        withTempDirectory \directory -> do
            let cwd = unsafeEncodeUtf directory
                mailboxPath = directory </> "mailbox"
                mailbox = unsafeEncodeUtf mailboxPath
            registry <-
                newSubagentRegistry defaultSubagentConfig cwd
                    (\_ _ _ _ ->
                        pure $ Right LoopResult
                            { finalResponseId = "worker-response"
                            , finalText = Just "worker answer"
                            , turnsUsed = 2
                            , tokenUsage = TokenUsage 11 7 3
                            })
                    (\_ _ -> pure ())
            rootTurn <- beginRootTurn registry
            let context = MultiAgentContext
                    { multiRegistry = registry
                    , multiSelfId = Nothing
                    , multiDepth = 0
                    , multiTaskPath = taskPathRoot
                    , multiRootTurnId = pure (Just rootTurn)
                    , multiResumeFromDisk = Nothing
                    , multiCreateWorktree = Nothing
                    , multiPrepareSpawn = Nothing
                    , multiSendToRoot = Nothing
                    , multiSpawnModelGuidance = Nothing
                    }
            runtime <- newRlmRuntime RlmConfig
                { rlmMailbox = mailbox
                , rlmContext = context
                , rlmModel = Nothing
                , rlmEffort = Nothing
                , rlmMaxCalls = 2
                , rlmParallelism = 2
                , rlmWorkerTimeoutSeconds = 10
                , rlmPrepareWorker = \_ _ -> pure mempty
                }
            let request = mailboxPath </> "request.req"
                response = mailboxPath </> "request.resp"
            writeFile request "readonly\ninspect the project"
            received <- timeout 3000000 (waitForFile response)
            closeRlmRuntime runtime
            closeSubagentRegistry registry
            case received of
                Nothing -> expectationFailure "timed out waiting for RLM response"
                Just body -> do
                    body `shouldSatisfy` Text.isInfixOf "\"worker answer\""
                    body `shouldSatisfy` Text.isInfixOf "\"input\":11"
                    body `shouldSatisfy` Text.isInfixOf "\"turns_used\":2"

waitForFile :: FilePath -> IO Text.Text
waitForFile path = do
    exists <- doesFileExist path
    if exists
        then Text.pack <$> readFile path
        else threadDelay 10000 >> waitForFile path

withTempDirectory :: (FilePath -> IO a) -> IO a
withTempDirectory action = do
    root <- getTemporaryDirectory
    bracket
        (mkdtemp (root </> "agent-cli-rlm-"))
        removeDirectoryRecursive
        action
