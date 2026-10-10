module Agent.Subagents.LoopSpec (spec) where

import Agent.Cancel (waitCancel)
import Agent.Error (ApiError(..))
import Agent.InterAgentMessage (plainInterAgentContent)
import Agent.Loop
    ( Backend(..)
    , BackendResult(..)
    , LoopConfig(..)
    , LoopError(..)
    , LoopResult(..)
    , TurnInput(..)
    , emptyTokenUsage
    , emptyTurnOutput
    , runLoopInputs
    )
import Agent.Loop.Fixtures (testConfig)
import Agent.Loop.SteeringInputs
    ( commitSteeringInputs
    , enqueueSteeringInputs
    , newSteeringInputs
    , readSteeringInputs
    )
import Agent.Subagents
    ( RootSubagents
    , SubagentId
    , SubagentNotices(..)
    , SubagentRegistry
    , SubagentSpawnEnv(..)
    , SubagentStatus(..)
    , SubagentTurnEnd(..)
    , closeSubagentRegistry
    , currentRootTurn
    , defaultSubagentConfig
    , getStatus
    , newRootSubagents
    , newSubagentRegistry
    , spawnSubagentAtForTurn
    , subagentLoop
    )
import Agent.Subagents.TaskPath (taskPathRoot)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Exception.Safe (bracket)
import Control.Monad (void)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import System.OsPath (unsafeEncodeUtf)
import System.Timeout (timeout)
import Test.Hspec

-- | A registry whose children answer once the test releases them.
withRoot
    :: SubagentNotices
    -> (SubagentRegistry -> RootSubagents -> MVar () -> IO a)
    -> IO a
withRoot notices action = do
    release <- newEmptyMVar
    bracket
        (newSubagentRegistry
            defaultSubagentConfig
            (unsafeEncodeUtf ".")
            (\env _ _ _ ->
                race (waitCancel env.subCancel) (readMVar release) >>= \case
                    Left () -> pure (Left (LoopCancelled []))
                    Right () -> pure $ Right LoopResult
                        { finalResponseId = "child"
                        , finalText = Just "child-answer"
                        , turnsUsed = 1
                        , tokenUsage = emptyTokenUsage
                        })
            (\_ _ -> pure ()))
        closeSubagentRegistry
        \registry -> do
            root <- newRootSubagents notices registry
            action registry root release

spawnChild :: SubagentRegistry -> RootSubagents -> IO SubagentId
spawnChild registry root = do
    turn <- currentRootTurn root
    spawnSubagentAtForTurn registry turn Nothing taskPathRoot 0 "lookup"
        (plainInterAgentContent "find it") Nothing
        >>= either (fail . Text.unpack) (pure . fst)

-- | A root backend that follows a script of answers, one per request, and
-- records the inputs of every request.
scriptedRoot
    :: IORef [[TurnInput]]
    -> [[TurnInput] -> IO (Either ApiError Text)]
    -> IO Backend
scriptedRoot requests steps = do
    remaining <- newIORef steps
    pure $ Backend \current _ inputs _ -> do
        modifyIORef' requests (<> [inputs])
        step <- atomicModifyIORef' remaining \case
            next : rest -> (rest, next)
            [] -> ([], \_ -> pure (Left (ConnectionError "script exhausted")))
        step inputs >>= \case
            Left err -> pure (Left err)
            Right text -> pure $ Right BackendResult
                { backendOutput =
                    emptyTurnOutput ("root-" <> text) [] (Just text)
                , backendState = current
                }

rootConfig :: SubagentTurnEnd -> RootSubagents -> Backend -> IO LoopConfig
rootConfig turnEnd root backend = do
    config <- testConfig backend
    pure config { loopSubagents = Just (subagentLoop turnEnd root) }

mentions :: Text -> [TurnInput] -> Bool
mentions needle = any \case
    UserMessage text -> needle `Text.isInfixOf` text
    _ -> False

awaitStatus :: SubagentRegistry -> SubagentId -> SubagentStatus -> IO ()
awaitStatus registry agentId expected = do
    reached <- timeout 2000000 poll
    reached `shouldBe` Just ()
  where
    poll = do
        status <- getStatus registry agentId
        if status == expected
            then pure ()
            else threadDelay 1000 >> poll

spec :: Spec
spec = describe "Agent.Subagents.Loop" do
    it "keeps children running after the root answers and reports them in the next turn" do
        withRoot NoticesToLoop \registry root release -> do
            requests <- newIORef []
            child <- newIORef Nothing
            backend <- scriptedRoot requests
                [ \_ -> do
                    spawnChild registry root >>= writeIORef child . Just
                    pure (Right "spawned")
                , \_ -> pure (Right "next")
                ]
            config <- rootConfig KeepChildrenRunning root backend
            fmap (.finalText) <$> runLoopInputs config Nothing [UserMessage "start"]
                `shouldReturn` Right (Just "spawned")
            currentRootTurn root `shouldReturn` Nothing
            Just agentId <- readIORef child
            getStatus registry agentId `shouldReturn` Running
            putMVar release ()
            awaitStatus registry agentId (Completed (Just "child-answer"))
            void (runLoopInputs config Nothing [UserMessage "again"])
            secondTurn <- (!! 1) <$> readIORef requests
            secondTurn `shouldSatisfy` mentions "child-answer"

    it "waits for running children before the root answers" do
        withRoot NoticesToLoop \registry root release -> do
            requests <- newIORef []
            backend <- scriptedRoot requests
                [ \_ -> do
                    void (spawnChild registry root)
                    void $ forkIO do
                        threadDelay 50000
                        putMVar release ()
                    pure (Right "waiting")
                , \inputs -> pure $ Right
                    (if mentions "child-answer" inputs then "done" else "missing")
                ]
            config <- rootConfig (AwaitChildren 5000) root backend
            fmap (.finalText) <$> runLoopInputs config Nothing [UserMessage "start"]
                `shouldReturn` Right (Just "done")
            length <$> readIORef requests `shouldReturn` 2

    it "interrupts children that outlast the wait and tells the root" do
        withRoot NoticesToLoop \registry root _ -> do
            requests <- newIORef []
            child <- newIORef Nothing
            backend <- scriptedRoot requests
                [ \_ -> do
                    spawnChild registry root >>= writeIORef child . Just
                    pure (Right "waiting")
                , \inputs -> pure $ Right
                    (if mentions "interrupted" inputs then "partial" else "missing")
                ]
            config <- rootConfig (AwaitChildren 50) root backend
            fmap (.finalText) <$> runLoopInputs config Nothing [UserMessage "start"]
                `shouldReturn` Right (Just "partial")
            Just agentId <- readIORef child
            getStatus registry agentId `shouldReturn` Interrupted

    it "interrupts children when the root answers under InterruptChildren" do
        withRoot NoticesToLoop \registry root _ -> do
            requests <- newIORef []
            child <- newIORef Nothing
            backend <- scriptedRoot requests
                [ \_ -> do
                    spawnChild registry root >>= writeIORef child . Just
                    pure (Right "answered")
                ]
            config <- rootConfig InterruptChildren root backend
            fmap (.finalText) <$> runLoopInputs config Nothing [UserMessage "start"]
                `shouldReturn` Right (Just "answered")
            Just agentId <- readIORef child
            getStatus registry agentId `shouldReturn` Interrupted

    it "interrupts children when the root turn fails" do
        withRoot NoticesToLoop \registry root _ -> do
            requests <- newIORef []
            child <- newIORef Nothing
            backend <- scriptedRoot requests
                [ \_ -> do
                    spawnChild registry root >>= writeIORef child . Just
                    pure (Left (ConnectionError "offline"))
                ]
            config <- rootConfig KeepChildrenRunning root backend
            result <- runLoopInputs config Nothing [UserMessage "start"]
            either (const True) (const False) result `shouldBe` True
            currentRootTurn root `shouldReturn` Nothing
            Just agentId <- readIORef child
            getStatus registry agentId `shouldReturn` Interrupted

    it "acknowledges host guidance and child reports separately" do
        withRoot NoticesToLoop \registry root release -> do
            requests <- newIORef []
            steering <- newSteeringInputs
            backend <- scriptedRoot requests
                [ \_ -> do
                    agentId <- spawnChild registry root
                    putMVar release ()
                    awaitStatus registry agentId (Completed (Just "child-answer"))
                    enqueueSteeringInputs steering [UserMessage "user guidance"]
                        `shouldReturn` Right ()
                    pure (Right "spawned")
                , \_ -> pure (Right "first")
                , \_ -> pure (Right "second")
                ]
            base <- rootConfig KeepChildrenRunning root backend
            let config = base
                    { loopReadSteering = readSteeringInputs steering
                    , loopCommitSteering = commitSteeringInputs steering
                    }
            void (runLoopInputs config Nothing [UserMessage "start"])
            readSteeringInputs steering `shouldReturn` []
            void (runLoopInputs config Nothing [UserMessage "again"])
            turns <- readIORef requests
            turns `shouldSatisfy` \case
                [_, delivered, again] ->
                    mentions "user guidance" delivered
                        && mentions "child-answer" delivered
                        && not (mentions "child-answer" again)
                _ -> False

    it "leaves notices to the host with NoticesToHost" do
        withRoot NoticesToHost \registry root release -> do
            requests <- newIORef []
            child <- newIORef Nothing
            backend <- scriptedRoot requests
                [ \_ -> do
                    spawnChild registry root >>= writeIORef child . Just
                    putMVar release ()
                    pure (Right "spawned")
                , \_ -> pure (Right "next")
                ]
            config <- rootConfig KeepChildrenRunning root backend
            void (runLoopInputs config Nothing [UserMessage "start"])
            Just agentId <- readIORef child
            awaitStatus registry agentId (Completed (Just "child-answer"))
            void (runLoopInputs config Nothing [UserMessage "again"])
            secondTurn <- (!! 1) <$> readIORef requests
            secondTurn `shouldNotSatisfy` mentions "child-answer"
