module Agent.GrokBuild.RuntimeSpec (spec) where

import Agent.GrokBuild.Dialect.Goal
import Agent.GrokBuild.Dialect.Runtime
    ( GrokCodingTools(..)
    , newGrokCodingTools
    )
import Agent.GrokBuild.Dialect.Scheduler
import Agent.GrokBuild.Dialect.Shell
    ( closeGrokSession
    , newGrokSession
    , readTaskOutput
    , startBackground
    , stopGrokBackgroundTasks
    )
import Agent.GrokBuild.Dialect.Task
    ( filterGrokToolsForType
    , runtimeSubagentType
    )
import Agent.GrokBuild.Dialect.Workflow
import Agent.InterAgentMessage (interAgentMessagePayload)
import Agent.Loop
    ( LoopResult(..)
    , defaultLoopDispatch
    , emptyTokenUsage
    )
import Agent.Subagents
    ( SubagentId(..)
    , SubagentStatus(..)
    , closeSubagentRegistry
    , defaultSubagentConfig
    , newSubagentRegistry
    )
import Agent.Subagents.TaskPath (taskPathRoot)
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , dispatchToolCall
    , functionToolCall
    , noArgsTool
    )
import Agent.ToolDSL (PropertySchema(..))
import Agent.Tools.MultiAgents (MultiAgentContext(..))
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , ToolSchema(..)
    , defaultToolEnv
    , jsonToolParameters
    )
import Control.Concurrent
    ( newChan
    , readChan
    , threadDelay
    , writeChan
    )
import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.MVar
    ( newEmptyMVar
    , takeMVar
    , tryPutMVar
    )
import Control.Exception.Safe (bracket)
import Control.Monad (forM_, replicateM, replicateM_)
import Data.IORef
    ( IORef
    , newIORef
    , readIORef
    , writeIORef
    )
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
    ( UTCTime(..)
    , addUTCTime
    , fromGregorian
    )
import System.IO.Temp (withSystemTempDirectory)
import System.OsPath (unsafeEncodeUtf)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = do
    describe "plan-mode quiescence" do
        it "stops retained Grok commands without closing the shell session" do
            withSystemTempDirectory "grok-quiesce" \dir -> do
                env <- defaultToolEnv (unsafeEncodeUtf dir)
                session <- newGrokSession env
                startBackground session "sleep 30"
                    `shouldReturn` Right
                        "Command moved to background.\n\
                        \task_id: t1\n\
                        \Use get_command_or_subagent_output to read output. Do not poll in a loop."
                stopGrokBackgroundTasks session
                readTaskOutput session "t1" Nothing
                    `shouldReturn` "Unknown task_id: t1"
                startBackground session "printf resumed"
                    `shouldReturn` Right
                        "Command moved to background.\n\
                        \task_id: t2\n\
                        \Use get_command_or_subagent_output to read output. Do not poll in a loop."
                closeGrokSession session

    describe "scheduler tools" do
        it "advertises the upstream public schema without recurring" do
            runtime <- testScheduler (pure fixedTime) (\_ -> pure (Right testAgent))
            let parameters =
                    fromMaybe []
                        (jsonToolParameters (schedulerCreateTool runtime))
                names = map (.propertyName) parameters
            names `shouldBe`
                [ "task_id"
                , "interval"
                , "prompt"
                , "durable"
                , "foreground"
                , "fire_immediately"
                ]
            names `shouldNotContain` ["recurring"]
            closeSchedulerRuntime runtime

        it "parses compact intervals and clamps to 60 seconds" do
            parseSchedulerInterval "5m" `shouldBe` Right 300
            parseSchedulerInterval " 2h " `shouldBe` Right 7200
            parseSchedulerInterval "30s" `shouldBe` Right 60
            parseSchedulerInterval "0m"
                `shouldSatisfy` either (Text.isInfixOf "greater than 0") (const False)
            parseSchedulerInterval "5x"
                `shouldSatisfy` either (Text.isInfixOf "suffix") (const False)

        it "fires immediately once and exposes truthful list/delete state" do
            fired <- newEmptyMVar
            bracket
                (testScheduler
                    (pure fixedTime)
                    (\fire -> do
                        _ <- tryPutMVar fired fire
                        pure (Right testAgent)))
                closeSchedulerRuntime
                \runtime -> do
                    created <- call
                        [schedulerCreateTool runtime]
                        "scheduler_create"
                        "{\"interval\":\"30s\",\"prompt\":\"check deploy\",\
                        \\"fire_immediately\":true}"
                    created.output `shouldSatisfy`
                        Text.isInfixOf "\"humanSchedule\":\"every 1 minute\""
                    timeout 1000000 (takeMVar fired)
                        `shouldReturn` Just
                            (ScheduledFire
                                { scheduledFireTaskId = "sched-1"
                                , scheduledFirePrompt = "check deploy"
                                , scheduledFireIntervalSeconds = 60
                                })
                    snapshots <- listScheduledTasks runtime
                    map (.scheduledTaskId) snapshots `shouldBe` ["sched-1"]
                    map (.scheduledTaskCreatedAt) snapshots
                        `shouldBe` [fixedTime]
                    listed <- call
                        [schedulerListTool runtime]
                        "scheduler_list"
                        "{}"
                    listed.output `shouldSatisfy`
                        Text.isInfixOf "\"intervalHuman\":\"every 1 minute\""
                    listed.output `shouldSatisfy`
                        Text.isInfixOf
                            "\"createdAt\":\"2026-08-24T00:00:00Z\""
                    deleted <- call
                        [schedulerDeleteTool runtime]
                        "scheduler_delete"
                        "{\"id\":\"sched-1\"}"
                    deleted.output `shouldSatisfy`
                        Text.isInfixOf "\"success\":true"
                    listScheduledTasks runtime `shouldReturn` []

        it "rejects unsupported durable and foreground scheduling" do
            bracket
                (testScheduler (pure fixedTime) (\_ -> pure (Right testAgent)))
                closeSchedulerRuntime
                \runtime -> do
                    durable <- call
                        [schedulerCreateTool runtime]
                        "scheduler_create"
                        "{\"interval\":\"5m\",\"prompt\":\"x\",\"durable\":true}"
                    durable.output `shouldSatisfy`
                        Text.isInfixOf "scheduler_durability_unsupported"
                    foreground <- call
                        [schedulerCreateTool runtime]
                        "scheduler_create"
                        "{\"interval\":\"5m\",\"prompt\":\"x\",\"foreground\":true}"
                    foreground.output `shouldSatisfy`
                        Text.isInfixOf "scheduler_foreground_unsupported"

        it "does not overlap a recurring fire while its prior child is active" do
            clock <- newIORef fixedTime
            active <- newIORef True
            fireCount <- newIORef (0 :: Int)
            bracket
                (newSchedulerRuntimeWithFireStatus
                    (readIORef clock)
                    (\_ -> do
                        count <- readIORef fireCount
                        writeIORef fireCount (count + 1)
                        pure (Right testAgent))
                    (const (readIORef active)))
                closeSchedulerRuntime
                \runtime -> do
                    _ <- call
                        [schedulerCreateTool runtime]
                        "scheduler_create"
                        "{\"interval\":\"60s\",\"prompt\":\"slow task\",\
                        \\"fire_immediately\":true}"
                    waitForCount fireCount 1
                    writeIORef clock (addUTCTime 60 fixedTime)
                    _ <- call
                        [schedulerCreateTool runtime]
                        "scheduler_create"
                        "{\"interval\":\"1d\",\
                        \\"prompt\":\"wake scheduler\"}"
                    threadDelay 50000
                    readIORef fireCount `shouldReturn` 1
                    writeIORef active False
                    waitForCount fireCount 2

        it "parks due scheduler fires while plan mode is active" do
            fired <- newEmptyMVar
            bracket
                (testScheduler
                    (pure fixedTime)
                    (\fire -> do
                        _ <- tryPutMVar fired fire
                        pure (Right testAgent)))
                closeSchedulerRuntime
                \runtime -> do
                    setSchedulerPaused runtime True
                    _ <- call
                        [schedulerCreateTool runtime]
                        "scheduler_create"
                        "{\"interval\":\"60s\",\"prompt\":\"mutate later\",\
                        \\"fire_immediately\":true}"
                    timeout 100000 (takeMVar fired)
                        `shouldReturn` Nothing
                    setSchedulerPaused runtime False
                    timeout 1000000 (takeMVar fired)
                        `shouldReturn` Just
                            ScheduledFire
                                { scheduledFireTaskId = "sched-1"
                                , scheduledFirePrompt = "mutate later"
                                , scheduledFireIntervalSeconds = 60
                                }

        it "launches distinct due tasks concurrently with a bound of eight" do
            clock <- newIORef fixedTime
            started <- newChan
            release <- newChan
            bracket
                (testScheduler
                    (readIORef clock)
                    (\fire -> do
                        writeChan started fire.scheduledFireTaskId
                        readChan release
                        pure (Right testAgent)))
                closeSchedulerRuntime
                \runtime -> do
                    forM_ [1 .. (9 :: Int)] \index -> do
                        result <- call
                            [schedulerCreateTool runtime]
                            "scheduler_create"
                            ("{\"interval\":\"60s\",\"prompt\":\"task "
                                <> Text.pack (show index)
                                <> "\"}")
                        result.output `shouldSatisfy`
                            Text.isInfixOf "\"updated\":false"
                    writeIORef clock (addUTCTime 60 fixedTime)
                    _ <- call
                        [schedulerCreateTool runtime]
                        "scheduler_create"
                        "{\"interval\":\"1d\",\"prompt\":\"wake scheduler\"}"
                    firstWave <-
                        replicateM 8
                            (timeout 1000000 (readChan started))
                    firstWave `shouldSatisfy` all (/= Nothing)
                    timeout 100000 (readChan started)
                        `shouldReturn` Nothing
                    writeChan release ()
                    ninthStarted <-
                        timeout 1000000 (readChan started)
                    ninthStarted `shouldSatisfy` (/= Nothing)
                    replicateM_ 8 (writeChan release ())

        it "enforces the 50-task limit and releases expired task slots" do
            nowRef <- newIORef fixedTime
            bracket
                (testScheduler
                    (readIORef nowRef)
                    (\_ -> pure (Right testAgent)))
                closeSchedulerRuntime
                \runtime -> do
                    forM_ [1 .. (50 :: Int)] \_ -> do
                        result <- call
                            [schedulerCreateTool runtime]
                            "scheduler_create"
                            "{\"interval\":\"5m\",\"prompt\":\"check\"}"
                        result.output `shouldSatisfy`
                            Text.isInfixOf "\"updated\":false"
                    overflow <- call
                        [schedulerCreateTool runtime]
                        "scheduler_create"
                        "{\"interval\":\"5m\",\"prompt\":\"overflow\"}"
                    overflow.output `shouldSatisfy`
                        Text.isInfixOf "maximum of 50 scheduled tasks reached"
                    writeIORef
                        nowRef
                        (addUTCTime (8 * 24 * 60 * 60) fixedTime)
                    listScheduledTasks runtime `shouldReturn` []
                    replacement <- call
                        [schedulerCreateTool runtime]
                        "scheduler_create"
                        "{\"interval\":\"5m\",\"prompt\":\"replacement\"}"
                    replacement.output `shouldSatisfy`
                        Text.isInfixOf "\"id\":\"sched-51\""

    describe "goal runtime" do
        it "tracks progress, blocking, resumption, and honest completion" do
            goals <- newGoalRuntime
            activateGoal goals "ship the widget" (Just 1000)
                `shouldReturn` Right GoalSnapshot
                    { goalObjective = "ship the widget"
                    , goalTokenBudget = Just 1000
                    , goalStatus = GoalActive
                    , goalProgress = []
                    , goalBlockedReason = Nothing
                    }
            progress <- call
                [updateGoalTool goals]
                "update_goal"
                "{\"message\":\"implemented core\"}"
            progress.output `shouldSatisfy`
                Text.isInfixOf "Progress recorded"
            blocked <- call
                [updateGoalTool goals]
                "update_goal"
                "{\"blocked_reason\":\"CI unavailable\"}"
            blocked.output `shouldSatisfy` Text.isInfixOf "blocked"
            fmap (.goalStatus) <$> readGoal goals
                `shouldReturn` Just GoalBlocked
            _ <- resumeGoal goals
            completed <- call
                [updateGoalTool goals]
                "update_goal"
                "{\"completed\":true,\"message\":\"verified locally\"}"
            completed.output `shouldSatisfy`
                Text.isInfixOf "classifier verification is disabled"
            fmap (.goalStatus) <$> readGoal goals
                `shouldReturn` Just GoalComplete
            clearGoal goals `shouldReturn` True
            readGoal goals `shouldReturn` Nothing

        it "rejects updates outside goal mode" do
            goals <- newGoalRuntime
            result <- call
                [updateGoalTool goals]
                "update_goal"
                "{\"message\":\"orphan\"}"
            result.output `shouldSatisfy`
                Text.isInfixOf "goal_update_harness_disabled"

    describe "workflow runtime" do
        it "launches uniquely named tracked deep-research runs" do
            withRegistry \registry -> do
                specs <- newIORef Map.empty
                let ctx = rootContext registry
                runtime <-
                    newWorkflowRuntime
                        (unsafeEncodeUtf "/tmp")
                        ctx
                        specs
                first <- call
                    [workflowTool runtime]
                    "workflow"
                    "{\"name\":\"deep-research\",\
                    \\"args\":{\"query\":\"Haskell effects\"}}"
                first.output `shouldSatisfy`
                    Text.isInfixOf "\"name\":\"deep-research\""
                second <- call
                    [workflowTool runtime]
                    "workflow"
                    "{\"name\":\"deep-research\",\
                    \\"args\":\"GHC optimization\"}"
                second.output `shouldSatisfy`
                    Text.isInfixOf "\"name\":\"deep-research-2\""
                runs <- workflowRunSnapshots runtime
                map (.workflowRunName) runs
                    `shouldMatchList`
                        ["deep-research", "deep-research-2"]
                map (.workflowRunId) runs
                    `shouldMatchList` ["wf_1", "wf_2"]

        it "probes workflow run statuses concurrently and preserves run order" do
            withRegistry \registry -> do
                specs <- newIORef Map.empty
                runtime <-
                    newWorkflowRuntime
                        (unsafeEncodeUtf "/tmp")
                        (rootContext registry)
                        specs
                forM_
                    [ "first research"
                    , "second research"
                    ]
                    \query -> do
                        _ <- call
                            [workflowTool runtime]
                            "workflow"
                            ("{\"name\":\"deep-research\",\"args\":{\"query\":\""
                                <> query
                                <> "\"}}")
                        pure ()
                started <- newChan
                release <- newChan
                let readStatus agentId = do
                        writeChan started agentId
                        readChan release
                        pure Running
                withAsync
                    (workflowRunSnapshotsWith readStatus runtime)
                    \snapshotsAsync -> do
                        firstStarted <-
                            timeout 1000000 (readChan started)
                        secondStarted <-
                            timeout 1000000 (readChan started)
                        firstStarted `shouldSatisfy` (/= Nothing)
                        secondStarted `shouldSatisfy` (/= Nothing)
                        replicateM_ 2 (writeChan release ())
                        snapshots <- wait snapshotsAsync
                        map (.workflowRunId) snapshots
                            `shouldBe` ["wf_1", "wf_2"]

        it "returns stable errors for unsupported workflow sources" do
            withRegistry \registry -> do
                specs <- newIORef Map.empty
                runtime <-
                    newWorkflowRuntime
                        (unsafeEncodeUtf "/tmp")
                        (rootContext registry)
                        specs
                inline <- call
                    [workflowTool runtime]
                    "workflow"
                    "{\"script\":\"let meta = #{};\"}"
                inline.output `shouldSatisfy`
                    Text.isInfixOf "workflow_inline_unsupported"
                resumed <- call
                    [workflowTool runtime]
                    "workflow"
                    "{\"resume_from_run_id\":\"wf_1\"}"
                resumed.output `shouldSatisfy`
                    Text.isInfixOf "workflow_resume_unsupported"
                workflowRunSnapshots runtime `shouldReturn` []

    describe "runtime capability gating" do
        it "advertises root-only tools only for a root multi-agent context" do
            withTempDir \dir ->
                withRegistry \registry -> do
                    env <- defaultToolEnv (unsafeEncodeUtf dir)
                    rootTypes <- newIORef Map.empty
                    root <- newGrokCodingTools
                        env
                        Nothing
                        (Just (rootContext registry))
                        rootTypes
                    let rootNames = map (.appToolName) root.grokAppTools
                    map (`elem` rootNames)
                        [ "scheduler_create"
                        , "scheduler_delete"
                        , "scheduler_list"
                        , "workflow"
                        , "update_goal"
                        ]
                        `shouldBe` replicate 5 True
                    root.grokClose

                    childTypes <- newIORef Map.empty
                    child <- newGrokCodingTools
                        env
                        Nothing
                        (Just (childContext registry))
                        childTypes
                    let childNames = map (.appToolName) child.grokAppTools
                    map (`elem` childNames)
                        [ "scheduler_create"
                        , "scheduler_delete"
                        , "scheduler_list"
                        , "workflow"
                        , "update_goal"
                        ]
                        `shouldBe` replicate 5 False
                    child.grokClose

        it "defensively removes root-only tools from general children" do
            let names =
                    map (.appToolName) $
                        filterGrokToolsForType
                            "general-purpose"
                            [ fake "read_file"
                            , fake "workflow"
                            , fake "scheduler_create"
                            , fake "update_goal"
                            ]
            names `shouldBe` ["read_file"]

        it "prevents runtime-owned children from delegating again" do
            let names =
                    map (.appToolName) $
                        filterGrokToolsForType
                            runtimeSubagentType
                            [ fake "read_file"
                            , fake "task"
                            , fake "workflow"
                            , fake "scheduler_create"
                            ]
            names `shouldBe` ["read_file"]

testScheduler
    :: IO UTCTime
    -> (ScheduledFire -> IO (Either Text SubagentId))
    -> IO SchedulerRuntime
testScheduler = newSchedulerRuntimeWithFire

testAgent :: SubagentId
testAgent = SubagentId "agent-test"

waitForCount :: IORef Int -> Int -> IO ()
waitForCount ref expected = go (200 :: Int)
  where
    go 0 =
        expectationFailure
            ("scheduler fire count did not reach " <> show expected)
    go remaining = do
        count <- readIORef ref
        if count >= expected
            then pure ()
            else threadDelay 10000 >> go (remaining - 1)

fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2026 8 24) 0

call :: [AppTool] -> Text -> Text -> IO ToolCallResult
call tools name arguments =
    dispatchToolCall
        defaultLoopDispatch
        (map (.appToolHandler) tools)
        (functionToolCall "call-1" name arguments)

rootContext registry = MultiAgentContext
    registry
    (unsafeEncodeUtf "/tmp")
    Nothing
    0
    taskPathRoot
    (pure Nothing)
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

childContext registry = MultiAgentContext
    registry
    (unsafeEncodeUtf "/tmp")
    (Just (SubagentId "agent-parent"))
    1
    taskPathRoot
    (pure Nothing)
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

withRegistry action =
    bracket
        (newSubagentRegistry
            defaultSubagentConfig
            (unsafeEncodeUtf "/tmp")
            (\_ _ prompt _ ->
                pure $ Right LoopResult
                    { finalResponseId = "response"
                    , finalText =
                        Just
                            ("done:"
                                <> interAgentMessagePayload prompt)
                    , turnsUsed = 1
                    , tokenUsage = emptyTokenUsage
                    })
            (\_ _ -> pure ()))
        closeSubagentRegistry
        action

fake :: Text -> AppTool
fake name = AppTool
    { appToolName = name
    , appToolDescription = name
    , appToolSchema = JsonFunctionSchema []
    , appToolHandler = noArgsTool name (pure (Right "ok"))
    , appToolApproval = AlwaysReadOnly
    , appToolExecution = ParallelSafe
    , appToolResourceClaims = Nothing
    }

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "agent-grok-runtime"
