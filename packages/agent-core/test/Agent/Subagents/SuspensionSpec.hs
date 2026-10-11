module Agent.Subagents.SuspensionSpec (spec) where

import Agent.Cancel (requestTurnYield)
import Agent.InterAgentMessage (InterAgentMessage(..), plainInterAgentContent)
import Agent.Loop
import Agent.Loop.Fixtures
import Agent.ToolDispatch (functionToolCall)
import Agent.Subagents
import Agent.Subagents.TaskPath (joinTaskPath, taskPathRoot)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)
import Control.Exception.Safe (bracket)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import System.OsPath (unsafeEncodeUtf)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "durable subagent suspension" do
    it "persists an active normal child suspension and publishes the suspended status" $
        withRegistry \registry -> do
            now <- getCurrentTime
            let deadline = addUTCTime 3600 now
            settled <- newEmptyMVar
            runnerReturned <- newIORef False
            setSubagentSuspensionPersistence registry acceptedStorage
                { persistSuspension = \_ _ _ -> do
                    readIORef runnerReturned `shouldReturn` True
                    pure (Right ())
                }
            setSubagentOnSettled registry (\_ status -> putMVar settled status)
            setSubagentRunner registry \env _ _ _ -> do
                submissions <- newIORef []
                backend <- scriptedBackend submissions []
                config <- testConfig backend
                transcript <- newIORef emptyBackendSnapshot
                let child = ChildTurn
                        { childBackend = backend
                        , childTools = config.loopTools
                        , childDispatch = config.loopDispatch
                        , childApproval = config.loopApprove
                        , childMaxTurns = 5
                        , childOnEvent = const (pure ())
                        , childTranscript = transcript
                        , childBackgroundTasks = Nothing
                        }
                result <- runChildTurn registry env child \_ -> do
                    requestSubagentSuspension registry env.subId deadline "waiting"
                        `shouldReturn` Right ()
                    pure (Left (LoopCancelled []))
                writeIORef runnerReturned True
                pure result
            spawned <- spawnSubagent registry Nothing 0 "find invoice" Nothing
            spawned `shouldSatisfy` either (const False) (const True)
            timeout 1000000 (takeMVar settled)
                `shouldReturn` Just (Suspended deadline "waiting")

    it "yields after recording the suspend tool result without another provider call" do
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [Right $ emptyTurnOutput "suspend-response"
                [functionToolCall "suspend-call" "suspend" "{}"] Nothing]
        config <- testConfig backend
        let suspend = noArgsAppTool "suspend" do
                requestTurnYield config.loopCancel
                pure (Right "persisted")
        execution <- runLoopInputsDetailed
            config { loopTools = registryFromTools [suspend] }
            Nothing [UserMessage "wait for the invoice"]
        execution.executionResult
            `shouldBe` Left (LoopCancelled [functionResult "suspend-call" "persisted"])
        length <$> readIORef submissions `shouldReturn` 1

    it "restores a suspended agent without consuming an execution slot" $
        withRegistry \registry -> do
            now <- getCurrentTime
            restoreSuspended registry "agent-1" (addUTCTime 60 now)
            restoreSuspended registry "agent-2" (addUTCTime 60 now)
            resumeDueSubagents registry now `shouldReturn` []
            getStatus registry (SubagentId "agent-1")
                `shouldReturn` Suspended (addUTCTime 60 now) "waiting"

    it "shares the execution cap across restored nesting levels" $
        withRegistry \registry -> do
            now <- getCurrentTime
            restoreSuspended registry "agent-parent" now
            parentPath <- either (fail . show) pure (joinTaskPath taskPathRoot "agent_parent")
            childPath <- either (fail . show) pure (joinTaskPath parentPath "nested")
            restoreSubagentAtStatus registry (SubagentId "agent-nested")
                (Just (SubagentId "agent-parent")) childPath 2 Nothing Nothing
                (Suspended now "waiting") `shouldReturn` Right (SubagentId "agent-nested")
            resumed <- resumeDueSubagents registry now
            length [() | (_, Right _) <- resumed] `shouldBe` 1
            again <- resumeDueSubagents registry now
            length [() | (_, Right _) <- again] `shouldBe` 0

    it "admits a due suspension only once and respects shared capacity" $
        withRegistry \registry -> do
            now <- getCurrentTime
            resumes <- newIORef (0 :: Int)
            setSubagentSuspensionPersistence registry
                acceptedStorage
                    { persistSuspensionResume = \_ _ ->
                        modifyIORef' resumes (+ 1) >> pure (Right ())
                    }
            restoreSuspended registry "agent-1" now
            restoreSuspended registry "agent-2" now
            first <- resumeDueSubagents registry now
            length [() | (_, Right _) <- first] `shouldBe` 1
            second <- resumeDueSubagents registry now
            length [() | (_, Right _) <- second] `shouldBe` 0
            readIORef resumes `shouldReturn` 1

    it "does not execute when the durable resume handoff fails" $
        withRegistry \registry -> do
            now <- getCurrentTime
            restoreSuspended registry "agent-1" now
            setSubagentSuspensionPersistence registry
                acceptedStorage { persistSuspensionResume = \_ _ -> pure (Left "storage unavailable") }
            resumeDueSubagents registry now
                `shouldReturn` [(SubagentId "agent-1", Left "storage unavailable")]
            getStatus registry (SubagentId "agent-1") `shouldReturn` Suspended now "waiting"

    it "allows an explicit follow-up before the deadline" $
        withRegistry \registry -> do
            now <- getCurrentTime
            wake <- newIORef Nothing
            setSubagentSuspensionPersistence registry acceptedStorage
                { persistSuspensionResume = \_ message -> writeIORef wake (Just message.messageContent) >> pure (Right ()) }
            restoreSuspended registry "agent-1" (addUTCTime 3600 now)
            sendInput registry (SubagentId "agent-1") "The document arrived." False
                `shouldReturn` Right "queued"
            readIORef wake `shouldReturn` Just (plainInterAgentContent "The document arrived.")
            resumeDueSubagents registry (addUTCTime 7200 now) `shouldReturn` []

    it "cancels a suspended deadline rather than reviving it on the next tick" $
        withRegistry \registry -> do
            now <- getCurrentTime
            restoreSuspended registry "agent-1" now
            interruptSubagent registry (SubagentId "agent-1")
                `shouldReturn` Right (Suspended now "waiting")
            resumeDueSubagents registry now `shouldReturn` []
            getStatus registry (SubagentId "agent-1") `shouldReturn` Interrupted

    it "preserves durable suspension metadata during process shutdown" $
        withRegistry \registry -> do
            now <- getCurrentTime
            let deadline = addUTCTime 3600 now
            durable <- newIORef (Just (deadline, "waiting" :: Text))
            setSubagentSuspensionPersistence registry acceptedStorage
                { persistSuspensionCancellation = \_ -> writeIORef durable Nothing >> pure (Right ()) }
            restoreSuspended registry "agent-1" deadline
            stopSubagentRegistry registry
            readIORef durable `shouldReturn` Just (deadline, "waiting")
            withRegistry \recovered -> do
                restoreSuspended recovered "agent-1" deadline
                getStatus recovered (SubagentId "agent-1") `shouldReturn` Suspended deadline "waiting"
                resumeDueSubagents recovered now `shouldReturn` []

    it "rejects a suspension without configured durable storage" $
        bracket createRegistry stopSubagentRegistry \registry -> do
            now <- getCurrentTime
            requestSubagentSuspension registry (SubagentId "agent-1") (addUTCTime 60 now) "waiting"
                `shouldReturn` Left "Durable suspension storage is not configured."

acceptedStorage :: SuspensionPersistence
acceptedStorage = SuspensionPersistence
    { persistSuspension = \_ _ _ -> pure (Right ())
    , persistSuspensionResume = \_ _ -> pure (Right ())
    , persistSuspensionCancellation = \_ -> pure (Right ())
    }

createRegistry :: IO SubagentRegistry
createRegistry = do
    gate <- newEmptyMVar
    newSubagentRegistry defaultSubagentConfig { maxConcurrent = 1 }
        (unsafeEncodeUtf ".")
        (\_ _ _ _ -> takeMVar gate >> pure (Left LoopNoResponseId))
        (\_ _ -> pure ())

withRegistry :: (SubagentRegistry -> IO a) -> IO a
withRegistry = bracket
    (do
        registry <- createRegistry
        setSubagentSuspensionPersistence registry acceptedStorage
        pure registry)
    stopSubagentRegistry

restoreSuspended :: SubagentRegistry -> Text -> UTCTime -> IO ()
restoreSuspended registry name deadline = do
    path <- either (fail . show) pure (joinTaskPath taskPathRoot (Text.replace "-" "_" name))
    restoreSubagentAtStatus registry (SubagentId name) Nothing path 1 Nothing Nothing
        (Suspended deadline "waiting") `shouldReturn` Right (SubagentId name)
