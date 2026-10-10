module Agent.Subagents.ChildSpec (spec) where

import Agent.Cancel (newCancelFlag, requestCancel)
import Agent.Loop
    ( Backend(..)
    , BackendResult(..)
    , BackendSnapshot(..)
    , BackendStateStore(..)
    , LoopConfig(..)
    , LoopError(..)
    , LoopResult(LoopResult, finalText, turnsUsed)
    , TokenUsage(..)
    , TurnInput(..)
    , TurnOutput(..)
    , defaultLoopDispatch
    , emptyBackendSnapshot
    , emptyTurnOutput
    , initialBackendSnapshot
    , runLoopInputs
    )
import Agent.Loop.SteeringInputs
    ( SteeringInputs
    , awaitUserSteeringAfter
    , commitSteeringInputs
    , enqueueBackgroundCompletion
    , newSteeringInputs
    , readSteeringInputs
    , reserveSteeringInputs
    )
import Agent.Subagents
    ( ChildBackgroundTasks(..)
    , ChildTurn(..)
    , SubagentId(..)
    , SubagentRegistry
    , SubagentSpawnEnv(..)
    , closeSubagentRegistry
    , defaultSubagentConfig
    , getPreviousResponseId
    , newSubagentRegistry
    , restoreSubagent
    , runChildTurn
    , runChildWithBackgroundTasks
    )
import Agent.Subagents.TestItems (functionCallItem, reasoningItem, userItem)
import Agent.Tools.Types
    ( BackgroundTaskHooks(..)
    , BackgroundTaskNotice(..)
    , ToolApproval(..)
    , mkToolRegistry
    )
import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar)
import Control.Exception.Safe (bracket)
import Control.Monad (void)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import System.OsPath (unsafeEncodeUtf)
import System.Timeout (timeout)
import Test.Hspec

-- | Background commands registered by the test, keyed by name. The value says
-- whether the command resumes its child when it completes.
type Commands = TVar (Map.Map Text Bool)

startCommand :: Commands -> Text -> Bool -> IO ()
startCommand commands key autoResume =
    atomically (modifyTVar' commands (Map.insert key autoResume))

removeCommand :: Commands -> Text -> IO ()
removeCommand commands key = atomically (modifyTVar' commands (Map.delete key))

backgroundTasks :: Commands -> ChildBackgroundTasks
backgroundTasks commands = ChildBackgroundTasks
    { installBackgroundTaskHooks = \_ -> pure ()
    , awaitingBackgroundResume = or <$> readTVar commands
    }

backgroundChildConfig :: SteeringInputs -> IORef [[TurnInput]] -> IO LoopConfig
backgroundChildConfig steering requests = do
    cancel <- newCancelFlag
    snapshot <- newIORef emptyBackendSnapshot
    registry <- either (fail . show) pure (mkToolRegistry [])
    pure LoopConfig
        { loopBackend = Backend \current _ inputs _ -> do
            modifyIORef' requests (<> [inputs])
            pure $ Right BackendResult
                { backendOutput =
                    (emptyTurnOutput "response" [] (Just "finished"))
                        { tokenUsage = TokenUsage 3 2 0 }
                , backendState = current
                }
        , loopBackendState = BackendStateStore
            { readBackendState = readIORef snapshot
            , commitBackendState = \current -> writeIORef snapshot current >> pure current
            }
        , loopTools = registry
        , loopReadTools = Nothing
        , loopDispatch = defaultLoopDispatch
        , loopMaxTurns = 5
        , loopOnEvent = const (pure ())
        , loopApprove = const (fail "unexpected tool request")
        , loopReadSteering = reserveSteeringInputs steering
        , loopWaitSteering = atomically . awaitUserSteeringAfter steering
        , loopCommitSteering = commitSteeringInputs steering
        , loopCloseSteering = pure []
        , loopInterrupt = pure ()
        , loopCancel = cancel
        , loopSubagents = Nothing
        }

-- | A child whose provider records every request and answers it with @done@.
answeringChild :: IORef [[TurnInput]] -> IORef BackendSnapshot -> IO ChildTurn
answeringChild requests transcript = do
    tools <- either (fail . show) pure (mkToolRegistry [])
    pure ChildTurn
        { childBackend = Backend \current _ inputs _ -> do
            modifyIORef' requests (<> [inputs])
            pure $ Right BackendResult
                { backendOutput = emptyTurnOutput "answer" [] (Just "done")
                , backendState = current
                }
        , childTools = tools
        , childDispatch = defaultLoopDispatch
        , childApproval = const (pure (ToolApprovalDenied "unexpected tool request"))
        , childMaxTurns = 5
        , childOnEvent = const (pure ())
        , childTranscript = transcript
        , childBackgroundTasks = Nothing
        }

withChild :: (SubagentRegistry -> SubagentSpawnEnv -> IO a) -> IO a
withChild action =
    bracket
        (newSubagentRegistry
            defaultSubagentConfig
            (unsafeEncodeUtf ".")
            (\_ _ _ _ -> pure (Left LoopNoResponseId))
            (\_ _ -> pure ()))
        closeSubagentRegistry
        \registry -> do
            let agentId = SubagentId "agent-child"
            restoreSubagent registry agentId Nothing 1 Nothing Nothing
                `shouldReturn` Right agentId
            cancel <- newCancelFlag
            action registry SubagentSpawnEnv
                { subId = agentId
                , subDepth = 1
                , subParentId = Nothing
                , subCwd = unsafeEncodeUtf "."
                , subCancel = cancel
                , subRootTurnId = Nothing
                }

spec :: Spec
spec = describe "Agent.Subagents.Child" do
    describe "runChildTurn" do
        it "records the response id a follow-up continues from" do
            withChild \registry env -> do
                requests <- newIORef []
                transcript <- newIORef emptyBackendSnapshot
                child <- answeringChild requests transcript
                result <- runChildTurn registry env child \config ->
                    runLoopInputs config Nothing [UserMessage "task"]
                fmap (.finalText) result `shouldBe` Right (Just "done")
                getPreviousResponseId registry env.subId
                    `shouldReturn` Just "answer"

        it "drops tool calls a failed turn left without output" do
            withChild \registry env -> do
                let complete = [userItem "task"]
                requests <- newIORef []
                transcript <- newIORef $ initialBackendSnapshot
                    (complete <> [reasoningItem, functionCallItem "live"])
                child <- answeringChild requests transcript
                result <- runChildTurn registry env child \_ ->
                    pure (Left (LoopCancelled []))
                fmap (.finalText) result `shouldBe` Left (LoopCancelled [])
                (.backendItems) <$> readIORef transcript `shouldReturn` complete
                getPreviousResponseId registry env.subId
                    `shouldReturn` Nothing

        it "runs the child under the spawn's cancellation flag" do
            withChild \registry env -> do
                requests <- newIORef []
                transcript <- newIORef emptyBackendSnapshot
                child <- answeringChild requests transcript
                requestCancel env.subCancel
                result <- runChildTurn registry env child \config ->
                    runLoopInputs config Nothing [UserMessage "task"]
                fmap (.finalText) result `shouldBe` Left (LoopCancelled [])
                readIORef requests `shouldReturn` []

        it "resumes the child with a completion delivered through its hooks" do
            withChild \registry env -> do
                requests <- newIORef []
                transcript <- newIORef emptyBackendSnapshot
                commands <- newTVarIO (Map.singleton "command" True)
                installed <- newEmptyMVar
                firstRequest <- newEmptyMVar
                base <- answeringChild requests transcript
                let Backend submit = base.childBackend
                    child = base
                        { childBackend = Backend \current previous inputs onEvent -> do
                            result <- submit current previous inputs onEvent
                            void (tryPutMVar firstRequest ())
                            pure result
                        , childBackgroundTasks = Just ChildBackgroundTasks
                            { installBackgroundTaskHooks = putMVar installed
                            , awaitingBackgroundResume = or <$> readTVar commands
                            }
                        }
                withAsync
                    (runChildTurn registry env child \config ->
                        runLoopInputs config Nothing [UserMessage "task"])
                    \worker -> do
                        hooks <- takeMVar installed
                        takeMVar firstRequest
                        hooks.backgroundTaskCompleted
                            (BackgroundTaskNotice "command" "completed")
                            `shouldReturn` True
                        removeCommand commands "command"
                        result <- timeout 1000000 (wait worker)
                        fmap (fmap (.turnsUsed)) result `shouldBe` Just (Right 2)
                readIORef requests `shouldReturn`
                    [[UserMessage "task"], [UserMessage "completed"]]

    describe "owned background command continuation" do
        it "keeps the child alive after its reply and resumes with completion exactly once" do
            commands <- newTVarIO Map.empty
            steering <- newSteeringInputs
            requests <- newIORef []
            config <- backgroundChildConfig steering requests
            returned <- newEmptyMVar
            startCommand commands "command" True
            let runFirst current = do
                    result <- runLoopInputs current Nothing [UserMessage "initial"]
                    putMVar returned ()
                    pure result
                tasks = Just (backgroundTasks commands)
            withAsync (runChildWithBackgroundTasks tasks steering config runFirst) \worker -> do
                takeMVar returned
                timeout 20000 (wait worker) `shouldReturn` Nothing
                readIORef requests `shouldReturn` [[UserMessage "initial"]]
                enqueueBackgroundCompletion steering "command" (UserMessage "completed")
                    `shouldReturn` Right True
                removeCommand commands "command"
                timeout 1000000 (wait worker) `shouldReturn`
                    Just (Right (LoopResult "response" (Just "finished") 2 (TokenUsage 6 4 0)))
            readIORef requests `shouldReturn`
                [[UserMessage "initial"], [UserMessage "completed"]]
            readSteeringInputs steering `shouldReturn` []

        it "observes completion queued before checking whether the child can finish" do
            commands <- newTVarIO Map.empty
            steering <- newSteeringInputs
            requests <- newIORef []
            config <- backgroundChildConfig steering requests
            let runFirst current = do
                    result <- runLoopInputs current Nothing [UserMessage "initial"]
                    enqueueBackgroundCompletion steering "command" (UserMessage "completed")
                        `shouldReturn` Right True
                    pure result
            result <-
                runChildWithBackgroundTasks
                    (Just (backgroundTasks commands)) steering config runFirst
            fmap (.turnsUsed) result `shouldBe` Right 2
            readIORef requests `shouldReturn`
                [[UserMessage "initial"], [UserMessage "completed"]]

        it "cancels a child suspended on an owned command without another model request" do
            commands <- newTVarIO Map.empty
            steering <- newSteeringInputs
            requests <- newIORef []
            config <- backgroundChildConfig steering requests
            returned <- newEmptyMVar
            startCommand commands "command" True
            let runFirst current = do
                    result <- runLoopInputs current Nothing [UserMessage "initial"]
                    putMVar returned ()
                    pure result
                tasks = Just (backgroundTasks commands)
            withAsync (runChildWithBackgroundTasks tasks steering config runFirst) \worker -> do
                takeMVar returned
                requestCancel config.loopCancel
                timeout 1000000 (wait worker) `shouldReturn`
                    Just (Left (LoopCancelled []))
            readIORef requests `shouldReturn` [[UserMessage "initial"]]

        it "does not wait for tasks that require explicit observation" do
            commands <- newTVarIO Map.empty
            steering <- newSteeringInputs
            requests <- newIORef []
            config <- backgroundChildConfig steering requests
            startCommand commands "observation" False
            result <- timeout 1000000 $
                runChildWithBackgroundTasks
                    (Just (backgroundTasks commands)) steering config
                    (\current -> runLoopInputs current Nothing [UserMessage "initial"])
            fmap (fmap (.turnsUsed)) result `shouldBe` Just (Right 1)

        it "finishes without a model request when an owned command is explicitly removed" do
            commands <- newTVarIO Map.empty
            steering <- newSteeringInputs
            requests <- newIORef []
            config <- backgroundChildConfig steering requests
            returned <- newEmptyMVar
            startCommand commands "command" True
            let runFirst current = do
                    result <- runLoopInputs current Nothing [UserMessage "initial"]
                    putMVar returned ()
                    pure result
                tasks = Just (backgroundTasks commands)
            withAsync (runChildWithBackgroundTasks tasks steering config runFirst) \worker -> do
                takeMVar returned
                timeout 20000 (wait worker) `shouldReturn` Nothing
                removeCommand commands "command"
                result <- timeout 1000000 (wait worker)
                fmap (fmap (.turnsUsed)) result `shouldBe` Just (Right 1)
            readIORef requests `shouldReturn` [[UserMessage "initial"]]

        it "does not reset the model turn budget when a completion resumes the child" do
            commands <- newTVarIO Map.empty
            steering <- newSteeringInputs
            requests <- newIORef []
            initialConfig <- backgroundChildConfig steering requests
            let config = initialConfig { loopMaxTurns = 2 }
                runFirst current = do
                    result <- runLoopInputs current Nothing [UserMessage "initial"]
                    enqueueBackgroundCompletion steering "first" (UserMessage "completed")
                        `shouldReturn` Right True
                    pure result
            startCommand commands "second" True
            result <- timeout 1000000 $
                runChildWithBackgroundTasks
                    (Just (backgroundTasks commands)) steering config runFirst
            case result of
                Just (Left (LoopMaxTurns _)) -> pure ()
                other -> expectationFailure ("expected exhausted turn budget, got " <> show other)
            readIORef requests `shouldReturn`
                [[UserMessage "initial"], [UserMessage "completed"]]

        it "finishes after its reply when the host has no background commands" do
            steering <- newSteeringInputs
            requests <- newIORef []
            config <- backgroundChildConfig steering requests
            result <- timeout 1000000 $
                runChildWithBackgroundTasks Nothing steering config
                    (\current -> runLoopInputs current Nothing [UserMessage "initial"])
            fmap (fmap (.turnsUsed)) result `shouldBe` Just (Right 1)
