module Agent.SubagentsSpec (spec) where

import Agent.Cancel (isCancelled, waitCancel)
import Agent.InterAgentMessage
import Agent.Loop (LoopError(..), LoopResult(..), emptyTokenUsage)
import System.OsPath (unsafeEncodeUtf)
import Agent.Subagents
import Agent.Subagents.TaskPath
    ( TaskPath
    , parseTaskPath
    , taskPathRoot
    , taskPathText
    )
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception.Safe (finally, uninterruptibleMask_)
import Control.Monad (unless)
import Data.IORef
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as TextRead
import GHC.Conc (BlockReason(..), ThreadStatus(..), threadStatus)
import System.Timeout (timeout)
import Test.Hspec

messagePayload :: InterAgentMessage -> Text
messagePayload message = case message.messageContent of
    PlainInterAgentContent text -> text
    EncryptedInterAgentContent text -> text

completedResult :: Text -> Either LoopError LoopResult
completedResult text = Right LoopResult
    { finalResponseId = text
    , finalText = Just text
    , turnsUsed = 1
    , tokenUsage = emptyTokenUsage
    }

runNestedRouting
    :: SubagentRegistry
    -> TMVar ()
    -> TMVar ()
    -> TMVar SubagentId
    -> TMVar InterAgentMessage
    -> SubagentSpawnEnv
    -> InterAgentMessage
    -> IO (Either LoopError LoopResult)
runNestedRouting registry parentRelease childRelease childSpawned noticeSeen env prompt
    | env.subDepth == 1 = runParent
    | otherwise = do
        atomically (takeTMVar childRelease)
        pure (completedResult "leaf-final")
  where
    runParent =
        case prompt.messageType of
            NewTaskMessage -> startChild
            _ -> do
                atomically (putTMVar noticeSeen prompt)
                pure (completedResult "parent-final")
    startChild = do
        parentPath <- fromMaybe taskPathRoot <$> getTaskPath registry env.subId
        Right (child, _) <-
            spawnSubagentAt registry (Just env.subId) parentPath env.subDepth
                "leaf" (plainInterAgentContent "leaf") Nothing
        atomically (putTMVar childSpawned child)
        atomically (takeTMVar parentRelease)
        pure (completedResult "parent-first")

blockingRunner started cleanedUp _ _ _ _ =
    (atomically (putTMVar started ()) >> atomically retry)
        `finally` atomically (putTMVar cleanedUp ())

fromFilePath = unsafeEncodeUtf

spec :: Spec
spec = describe "Agent.Subagents" do
    it "spawns a child, waits for completion, and returns final text" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ prompt _ -> pure $ Right LoopResult
                { finalResponseId = "child"
                , finalText = Just ("done:" <> messagePayload prompt)
                , turnsUsed = 1
                , tokenUsage = emptyTokenUsage
                })
            (\_ _ -> pure ())
        Right agentId <- spawnSubagent registry Nothing 0 "hello" Nothing
        (statuses, timedOut) <- waitSubagents registry [agentId] 15000
        timedOut `shouldBe` False
        Map.lookup agentId statuses `shouldBe` Just (Completed (Just "done:hello"))

    it "does not launch a prepared supervisor after registry shutdown" do
        entered <- newEmptyMVar
        release <- newEmptyMVar
        ran <- newIORef False
        registry <- shutdownRaceRegistry ran
        spawning <- Async.async $
            spawnSubagentWithCwdPrepared registry (fromFilePath "/tmp")
                (blockPreparation entered release)
                Nothing 0 "task" Nothing
        takeMVar entered
        closeSubagentRegistry registry
        putMVar release ()
        Async.wait spawning `shouldReturn`
            Left "Subagent closed before its supervisor started."
        readIORef ran `shouldReturn` False

    it "releases a prepared lease when the supervisor cannot start" do
        entered <- newEmptyMVar
        release <- newEmptyMVar
        releases <- newIORef (0 :: Int)
        ran <- newIORef False
        registry <- shutdownRaceRegistry ran
        spawning <- Async.async $
            spawnSubagentWithCwdPrepared registry (fromFilePath "/tmp")
                (\_ -> do
                    putMVar entered ()
                    takeMVar release
                    pure $ subagentLease $
                        atomicModifyIORef' releases \n -> (n + 1, ()))
                Nothing 0 "task" Nothing
        takeMVar entered
        closeSubagentRegistry registry
        putMVar release ()
        Async.wait spawning `shouldReturn`
            Left "Subagent closed before its supervisor started."
        readIORef releases `shouldReturn` 1
        readIORef ran `shouldReturn` False

    it "rolls back prepared admission when spawning is cancelled" do
        entered <- newEmptyMVar
        release <- newEmptyMVar
        ran <- newIORef False
        registry <- shutdownRaceRegistry ran
        spawning <- Async.async $
            spawnSubagentWithCwdPrepared registry (fromFilePath "/tmp")
                (blockPreparation entered release)
                Nothing 0 "cancelled" Nothing
        takeMVar entered
        Async.cancel spawning
        _ <- Async.waitCatch spawning
        Right _ <- spawnSubagent registry Nothing 0 "next" Nothing
        closeSubagentRegistry registry

    it "releases a prepared lease when cancellation wins before supervisor ownership" do
        preparationEntered <- newEmptyMVar
        finishPreparation <- newEmptyMVar
        firstCleanupStarted <- newEmptyMVar
        finishFirstCleanup <- newEmptyMVar
        leaseReleases <- newIORef (0 :: Int)
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ prompt _ -> pure (completedResult (messagePayload prompt)))
            (\_ _ -> pure ())
        Right first <- spawnSubagentWithCwdPrepared registry (fromFilePath "/tmp")
            (\_ -> pure $ subagentLease $
                uninterruptibleMask_ $
                    putMVar firstCleanupStarted () >> takeMVar finishFirstCleanup)
            Nothing 0 "first" Nothing

        Async.withAsync
            (spawnSubagentWithCwdPrepared registry (fromFilePath "/tmp")
                (\_ -> do
                    putMVar preparationEntered ()
                    takeMVar finishPreparation
                    pure $ subagentLease $
                        atomicModifyIORef' leaseReleases \n -> (n + 1, ()))
                Nothing 0 "cancelled-after-preparation" Nothing)
            \spawning -> do
                takeMVar preparationEntered
                Async.withAsync (closeSubagent registry first) \closing -> do
                    takeMVar firstCleanupStarted
                    closingStatus <- Async.poll closing
                    closingStatus `shouldSatisfy` isNothing
                    putMVar finishPreparation ()
                    waitForMVarBlock "spawn" spawning
                    Async.cancel spawning
                    _ <- Async.waitCatch spawning
                    putMVar finishFirstCleanup ()
                    _ <- Async.wait closing
                    readIORef leaseReleases `shouldReturn` 1
        closeSubagentRegistry registry

    it "releases pending capacity exactly once when an agent closes during preparation" do
        preparedId <- newEmptyMVar
        release <- newEmptyMVar
        let config = defaultSubagentConfig { maxConcurrent = 1 }
        registry <- newSubagentRegistry config (fromFilePath "/tmp")
            (\_ _ _ _ -> atomically retry)
            (\_ _ -> pure ())
        spawning <- Async.async $
            spawnSubagentWithCwdPrepared registry (fromFilePath "/tmp")
                (\agentId -> do
                    putMVar preparedId agentId
                    takeMVar release
                    pure mempty)
                Nothing 0 "pending" Nothing
        agentId <- takeMVar preparedId
        closeSubagent registry agentId `shouldReturn` Right Pending
        putMVar release ()
        Async.wait spawning `shouldReturn`
            Left "Subagent closed before its supervisor started."

        Right _ <- spawnSubagent registry Nothing 0 "replacement" Nothing
        extra <- spawnSubagent registry Nothing 0 "extra" Nothing
        extra `shouldSatisfy` \case
            Left err -> "Concurrent subagent limit" `Text.isInfixOf` err
            Right _ -> False
        closeSubagentRegistry registry

    it "rejects a prepared spawn after its root turn is aborted" do
        preparedId <- newEmptyMVar
        release <- newEmptyMVar
        leaseReleases <- newIORef (0 :: Int)
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ _ _ -> atomically retry)
            (\_ _ -> pure ())
        rootTurnId <- beginRootTurn registry
        spawning <- Async.async $
            spawnSubagentWithCwdPreparedForTurn
                registry (Just rootTurnId) (fromFilePath "/tmp")
                (\agentId -> do
                    putMVar preparedId agentId
                    takeMVar release
                    pure $ subagentLease $
                        atomicModifyIORef' leaseReleases \n -> (n + 1, ()))
                Nothing 0 "aborted" Nothing
        agentId <- takeMVar preparedId

        abortRootTurn registry rootTurnId
        getStatus registry agentId `shouldReturn` Interrupted
        putMVar release ()
        Async.wait spawning `shouldReturn`
            Left "Subagent closed before its supervisor started."
        readIORef leaseReleases `shouldReturn` 1
        getStatus registry agentId `shouldReturn` NotFound
        closeSubagentRegistry registry

    it "preserves replacement paths when an old spawn rolls back" do
        entered <- newEmptyMVar
        release <- newEmptyMVar
        ran <- newIORef False
        registry <- shutdownRaceRegistry ran
        oldSpawn <- Async.async $
            spawnPreparedAt registry entered release
        takeMVar entered
        resetSubagentRegistry registry
        Right (replacement, _) <-
            spawnSubagentAt registry Nothing taskPathRoot 0 "worker"
                (plainInterAgentContent "new") Nothing
        putMVar release ()
        _ <- Async.wait oldSpawn
        resolveAgentTarget registry taskPathRoot "worker"
            `shouldReturn` Right replacement
        closeSubagentRegistry registry

    it "rejects spawn past maxDepth" do
        let config = defaultSubagentConfig { maxDepth = Just 1 }
        registry <- newSubagentRegistry config (fromFilePath "/tmp")
            (\_ _ _ _ -> pure $ Right LoopResult
                { finalResponseId = "x"
                , finalText = Just "ok"
                , turnsUsed = 1
                , tokenUsage = emptyTokenUsage
                })
            (\_ _ -> pure ())
        Right child <- spawnSubagent registry Nothing 0 "one" Nothing
        _ <- waitSubagents registry [child] 15000
        result <- spawnSubagent registry (Just child) 1 "two" Nothing
        result `shouldBe` Left "Agent depth limit reached. Solve the task yourself."

    it "releases maxConcurrent capacity when agents finish" do
        gate <- newTVarIO False
        let config = defaultSubagentConfig { maxConcurrent = 1 }
        registry <- newSubagentRegistry config (fromFilePath "/tmp")
            (\_ _ _ _ -> do
                atomically $ readTVar gate >>= \ready -> unless ready retry
                pure $ Right LoopResult
                    { finalResponseId = "x"
                    , finalText = Just "ok"
                    , turnsUsed = 1
                    , tokenUsage = emptyTokenUsage
                    })
            (\_ _ -> pure ())
        Right first <- spawnSubagent registry Nothing 0 "a" Nothing
        second <- spawnSubagent registry Nothing 0 "b" Nothing
        second `shouldSatisfy` \case
            Left err -> "Concurrent subagent limit" `Text.isInfixOf` err
            Right _ -> False
        atomically $ writeTVar gate True
        _ <- waitSubagents registry [first] 15000
        Right _ <- spawnSubagent registry Nothing 0 "c" Nothing
        pure ()

    it "supports nested spawn when depth is unlimited" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ _ _ -> pure $ Left LoopNoResponseId)
            (\_ _ -> pure ())
        setSubagentRunner registry \env _previous prompt _ ->
            if env.subDepth == 1
                then do
                    Right gid <- spawnSubagent registry (Just env.subId) env.subDepth "nested" Nothing
                    _ <- waitSubagentsFrom registry (Just env.subId) [gid] 15000
                    pure $ Right LoopResult
                        { finalResponseId = "child"
                        , finalText = Just ("parent-of-" <> gid.unSubagentId)
                        , turnsUsed = 1
                        , tokenUsage = emptyTokenUsage
                        }
                else
                    pure $ Right LoopResult
                        { finalResponseId = "grand"
                        , finalText = Just ("leaf:" <> messagePayload prompt)
                        , turnsUsed = 1
                        , tokenUsage = emptyTokenUsage
                        }
        Right child <- spawnSubagent registry Nothing 0 "root-task" Nothing
        (statuses, timedOut) <- waitSubagents registry [child] 20000
        timedOut `shouldBe` False
        case Map.lookup child statuses of
            Just (Completed (Just text)) ->
                text `shouldSatisfy` Text.isPrefixOf "parent-of-agent-"
            other -> expectationFailure ("unexpected status: " <> show other)

    it "derives nested parent path and depth from the registry" do
        seen <- newIORef ([] :: [(Text, Int, Maybe SubagentId)])
        let config = defaultSubagentConfig { maxDepth = Just 2 }
        registry <- newSubagentRegistry config (fromFilePath "/tmp")
            (\env _ prompt _ -> do
                atomicModifyIORef' seen \xs ->
                    (xs <> [(messagePayload prompt, env.subDepth, env.subParentId)], ())
                pure $ Right LoopResult
                    { finalResponseId = "done"
                    , finalText = Just (messagePayload prompt)
                    , turnsUsed = 1
                    , tokenUsage = emptyTokenUsage
                    })
            (\_ _ -> pure ())
        Right parent <- spawnSubagent registry Nothing 0 "parent" Nothing
        _ <- waitSubagents registry [parent] 15000
        Just parentPath <- getTaskPath registry parent
        Right (child, childPath) <-
            spawnSubagentAt registry (Just parent) taskPathRoot 99 "nested"
                (plainInterAgentContent "child") Nothing
        _ <- waitSubagents registry [child] 15000
        taskPathText childPath
            `shouldBe` taskPathText parentPath <> "/nested"
        observations <- readIORef seen
        observations `shouldContain` [("child", 2, Just parent)]

    it "rejects descendants after the parent is closed" do
        gate <- newTVarIO False
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ _ _ -> do
                atomically $ readTVar gate >>= \ready -> unless ready retry
                pure (completedResult "parent"))
            (\_ _ -> pure ())
        Right (parent, parentPath) <-
            spawnSubagentAt registry Nothing taskPathRoot 0 "parent"
                (plainInterAgentContent "parent") Nothing
        _ <- closeSubagent registry parent
        result <- spawnSubagentAt registry (Just parent) parentPath 1 "child"
            (plainInterAgentContent "child") Nothing
        result `shouldBe` Left "Parent subagent is closed or missing."

    it "waitSubagents returns when any target finishes" do
        gate <- newTVarIO False
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ prompt _ -> case messagePayload prompt of
                "fast" -> pure $ Right LoopResult
                    { finalResponseId = "fast"
                    , finalText = Just "done-fast"
                    , turnsUsed = 1
                    , tokenUsage = emptyTokenUsage
                    }
                _ -> do
                    atomically $ readTVar gate >>= \ready -> unless ready retry
                    pure $ Right LoopResult
                        { finalResponseId = "slow"
                        , finalText = Just "done-slow"
                        , turnsUsed = 1
                        , tokenUsage = emptyTokenUsage
                        })
            (\_ _ -> pure ())
        Right fast <- spawnSubagent registry Nothing 0 "fast" Nothing
        Right slow <- spawnSubagent registry Nothing 0 "slow" Nothing
        (statuses, timedOut) <- waitSubagents registry [fast, slow] 15000
        timedOut `shouldBe` False
        Map.lookup fast statuses `shouldBe` Just (Completed (Just "done-fast"))
        -- Slow may still be running; wait only required any final.
        Map.lookup slow statuses `shouldSatisfy` \case
            Just Running -> True
            Just Pending -> True
            Just (Completed _) -> True
            _ -> False
        atomically $ writeTVar gate True
        _ <- waitSubagents registry [slow] 15000
        pure ()

    it "untargeted waits exclude the calling agent" do
        parentGate <- newTVarIO False
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ prompt _ -> case messagePayload prompt of
                "parent" -> do
                    atomically $ readTVar parentGate >>= \ready -> unless ready retry
                    pure (completedResult "parent")
                other -> pure (completedResult other))
            (\_ _ -> pure ())
        Right parent <- spawnSubagent registry Nothing 0 "parent" Nothing
        Right child <- spawnSubagent registry Nothing 0 "child" Nothing
        (statuses, timedOut) <- waitAnyLive registry (Just parent) 15000
        timedOut `shouldBe` False
        Map.lookup child statuses `shouldBe` Just (Completed (Just "child"))
        Map.member parent statuses `shouldBe` False
        atomically $ writeTVar parentGate True
        _ <- waitSubagents registry [parent] 15000
        pure ()

    it "untargeted waits consume completions" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ prompt _ -> pure (completedResult (messagePayload prompt)))
            (\_ _ -> pure ())
        Right child <- spawnSubagent registry Nothing 0 "one" Nothing
        (first, firstTimedOut) <- waitAnyLive registry Nothing 15000
        firstTimedOut `shouldBe` False
        Map.lookup child first `shouldBe` Just (Completed (Just "one"))
        repeated <- timeout 100000 (waitAnyLive registry Nothing 15000)
        repeated `shouldBe` Nothing
        Right _ <- sendInput registry child "two" False
        (second, secondTimedOut) <- waitAnyLive registry Nothing 15000
        secondTimedOut `shouldBe` False
        Map.lookup child second `shouldBe` Just (Completed (Just "two"))

    it "passes previous response id on send_input follow-ups" do
        seen <- newIORef ([] :: [Maybe Text])
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ previous prompt _ -> do
                atomicModifyIORef' seen \xs -> (xs <> [previous], ())
                pure $ Right LoopResult
                    { finalResponseId = "resp-" <> messagePayload prompt
                    , finalText = Just (messagePayload prompt)
                    , turnsUsed = 1
                    , tokenUsage = emptyTokenUsage
                    })
            (\_ _ -> pure ())
        Right agentId <- spawnSubagent registry Nothing 0 "one" Nothing
        _ <- waitSubagents registry [agentId] 15000
        Right _ <- sendInput registry agentId "two" False
        _ <- waitSubagents registry [agentId] 15000
        history <- readIORef seen
        history `shouldBe` [Nothing, Just "resp-one"]

    it "does not lose a follow-up that interrupts the active turn" do
        started <- newEmptyTMVarIO
        seen <- newIORef ([] :: [Text])
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\env _ prompt _ -> do
                let payload = messagePayload prompt
                atomicModifyIORef' seen \xs -> (xs <> [payload], ())
                if payload == "first"
                    then do
                        atomically $ putTMVar started ()
                        waitCancel env.subCancel
                        pure (Left (LoopCancelled []))
                    else pure (completedResult payload))
            (\_ _ -> pure ())
        Right agentId <- spawnSubagent registry Nothing 0 "first" Nothing
        atomically $ takeTMVar started
        Right _ <- sendInput registry agentId "second" True
        (statuses, timedOut) <- waitSubagents registry [agentId] 15000
        timedOut `shouldBe` False
        Map.lookup agentId statuses `shouldBe` Just (Completed (Just "second"))
        readIORef seen `shouldReturn` ["first", "second"]
        closeSubagentRegistry registry

    it "does not leave Running when send_input admission fails" do
        gate <- newTVarIO False
        let config = defaultSubagentConfig { maxConcurrent = 1 }
        registry <- newSubagentRegistry config (fromFilePath "/tmp")
            (\_ _ prompt _ -> case messagePayload prompt of
                "hold" -> do
                    atomically $ readTVar gate >>= \ready -> unless ready retry
                    pure $ Right LoopResult
                        { finalResponseId = "hold"
                        , finalText = Just "holding"
                        , turnsUsed = 1
                        , tokenUsage = emptyTokenUsage
                        }
                other -> pure $ Right LoopResult
                    { finalResponseId = "done"
                    , finalText = Just other
                    , turnsUsed = 1
                    , tokenUsage = emptyTokenUsage
                    })
            (\_ _ -> pure ())
        Right idle <- spawnSubagent registry Nothing 0 "idle" Nothing
        _ <- waitSubagents registry [idle] 15000
        Right holder <- spawnSubagent registry Nothing 0 "hold" Nothing
        -- The completed idle agent no longer owns a slot, while holder owns the
        -- only active slot.
        result <- sendInput registry idle "follow-up" False
        result `shouldSatisfy` \case
            Left err -> "Concurrent subagent limit" `Text.isInfixOf` err
            Right _ -> False
        status <- getStatus registry idle
        status `shouldBe` Completed (Just "idle")
        atomically $ writeTVar gate True
        _ <- waitSubagents registry [holder] 15000
        pure ()

    it "invokes onComplete when a child finishes" do
        notices <- newIORef ([] :: [(SubagentId, SubagentStatus)])
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ prompt _ -> pure $ Right LoopResult
                { finalResponseId = "c"
                , finalText = Just (messagePayload prompt)
                , turnsUsed = 1
                , tokenUsage = emptyTokenUsage
                })
            (\_ _ -> pure ())
        setSubagentOnComplete registry \agentId status ->
            atomicModifyIORef' notices \xs -> (xs <> [(agentId, status)], ())
        Right agentId <- spawnSubagent registry Nothing 0 "notify-me" Nothing
        threadDelay 100000
        seen <- readIORef notices
        seen `shouldBe` [(agentId, Completed (Just "notify-me"))]

    it "routes nested completion to the direct parent" do
        parentRelease <- newEmptyTMVarIO
        childRelease <- newEmptyTMVarIO
        childSpawned <- newEmptyTMVarIO
        noticeSeen <- newEmptyTMVarIO
        rootNotices <- newIORef ([] :: [SubagentId])
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ _ _ -> pure (Left LoopNoResponseId))
            (\_ _ -> pure ())
        setSubagentOnComplete registry \agentId _ ->
            atomicModifyIORef' rootNotices \xs -> (xs <> [agentId], ())
        setSubagentRunner registry \env _ prompt _ ->
            runNestedRouting
                registry parentRelease childRelease childSpawned noticeSeen
                env prompt
        Right parent <- spawnSubagent registry Nothing 0 "parent" Nothing
        child <- atomically (takeTMVar childSpawned)
        atomically (putTMVar childRelease ())
        _ <- waitSubagents registry [child] 15000
        atomically (putTMVar parentRelease ())
        delivered <- timeout 1000000 (atomically (takeTMVar noticeSeen))
        delivered `shouldSatisfy` \case
            Just message ->
                message.messageAuthor /= "/root"
                    && message.messageType == QueuedMessage
                    && child.unSubagentId `Text.isInfixOf` messagePayload message
            Nothing -> False
        _ <- waitSubagents registry [parent] 15000
        notices <- readIORef rootNotices
        notices `shouldNotContain` [child]

    it "allows completion callbacks to queue a follow-up without deadlocking" do
        prompts <- newIORef ([] :: [Text])
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ prompt _ -> do
                let payload = messagePayload prompt
                atomicModifyIORef' prompts \seen -> (seen <> [payload], ())
                pure $ Right LoopResult
                    { finalResponseId = "response-" <> payload
                    , finalText = Just payload
                    , turnsUsed = 1
                    , tokenUsage = emptyTokenUsage
                    })
            (\_ _ -> pure ())
        setSubagentOnComplete registry \agentId -> \case
            Completed (Just "first") -> do
                _ <- sendInput registry agentId "second" False
                pure ()
            _ -> pure ()
        Right agentId <- spawnSubagent registry Nothing 0 "first" Nothing
        (statuses, timedOut) <- waitSubagents registry [agentId] 15000
        timedOut `shouldBe` False
        Map.lookup agentId statuses
            `shouldBe` Just (Completed (Just "second"))
        readIORef prompts `shouldReturn` ["first", "second"]

    it "cancels and joins a running supervisor when the registry closes" do
        started <- newEmptyTMVarIO
        cleanedUp <- newEmptyTMVarIO
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (blockingRunner started cleanedUp)
            (\_ _ -> pure ())
        Right _ <- spawnSubagent registry Nothing 0 "wait" Nothing
        atomically $ takeTMVar started
        closeSubagentRegistry registry
        atomically $ readTMVar cleanedUp

    it "restores a missing agent id into the registry" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ previous prompt _ -> pure $ Right LoopResult
                { finalResponseId = fromMaybe "resp" previous
                , finalText = Just (messagePayload prompt)
                , turnsUsed = 1
                , tokenUsage = emptyTokenUsage
                })
            (\_ _ -> pure ())
        let agentId = SubagentId "agent-restored-1"
        Right _ <- restoreSubagent registry agentId Nothing 1 Nothing (Just "prev-1")
        status <- getStatus registry agentId
        status `shouldBe` Completed Nothing
        previous <- getPreviousResponseId registry agentId
        previous `shouldBe` Just "prev-1"
        Right _ <- sendInput registry agentId "follow" False
        (statuses, timedOut) <- waitSubagents registry [agentId] 15000
        timedOut `shouldBe` False
        Map.lookup agentId statuses `shouldBe` Just (Completed (Just "follow"))

    it "indexes restored paths and advances the restored id index" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ prompt _ -> pure $ Right LoopResult
                { finalResponseId = "restored"
                , finalText = Just (messagePayload prompt)
                , turnsUsed = 1
                , tokenUsage = emptyTokenUsage
                })
            (\_ _ -> pure ())
        let restored = SubagentId "agent-restored-1000000"
        Right _ <- restoreSubagent registry restored Nothing 1 Nothing Nothing
        Just restoredPath <- getTaskPath registry restored
        taskPathText restoredPath `shouldBe` "/root/aagentrestored1000000"
        resolveAgentTarget registry taskPathRoot "aagentrestored1000000"
            `shouldReturn` Right restored
        Right spawned <- spawnSubagent registry Nothing 0 "next" Nothing
        let suffix = snd (Text.breakOnEnd "-" spawned.unSubagentId)
        case TextRead.decimal suffix of
            Right (index, rest) -> do
                rest `shouldBe` ""
                index `shouldSatisfy` (> (1000000 :: Int))
            Left err -> expectationFailure err

    it "does not reopen an agent after the registry is closed" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ prompt _ -> pure $ Right LoopResult
                { finalResponseId = "closed"
                , finalText = Just (messagePayload prompt)
                , turnsUsed = 1
                , tokenUsage = emptyTokenUsage
                })
            (\_ _ -> pure ())
        Right agentId <- spawnSubagent registry Nothing 0 "first" Nothing
        _ <- waitSubagents registry [agentId] 15000
        closeSubagentRegistry registry
        restored <- restoreSubagent registry agentId Nothing 1 Nothing Nothing
        restored `shouldBe` Left "Subagent registry is closed."
        getStatus registry agentId `shouldReturn` Closed
        queueMessage registry agentId "late"
            `shouldReturn` Left "Subagent registry is closed."
        sendInput registry agentId "late" False
            `shouldReturn` Left "Subagent registry is closed."
    it "restores persisted lifecycle status and marks abandoned work interrupted" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ _ _ -> pure $ Left LoopNoResponseId)
            (\_ _ -> pure ())
        let interruptedId = SubagentId "agent-restored-running"
            erroredId = SubagentId "agent-restored-error"
        Right _ <- restoreSubagentAtStatus registry interruptedId Nothing
            taskPathRoot 1 Nothing (Just "prev-running") Running
        getStatus registry interruptedId `shouldReturn` Interrupted
        Right _ <- restoreSubagentAtStatus registry erroredId Nothing
            taskPathRoot 1 Nothing (Just "prev-error") (Errored "boom")
        getStatus registry erroredId `shouldReturn` Errored "boom"
    it "reopens a closed agent via restoreSubagent" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ prompt _ -> pure $ Right LoopResult
                { finalResponseId = "r"
                , finalText = Just (messagePayload prompt)
                , turnsUsed = 1
                , tokenUsage = emptyTokenUsage
                })
            (\_ _ -> pure ())
        Right agentId <- spawnSubagent registry Nothing 0 "first" Nothing
        _ <- waitSubagents registry [agentId] 15000
        _ <- closeSubagent registry agentId
        status0 <- getStatus registry agentId
        status0 `shouldBe` Closed
        Right _ <- restoreSubagent registry agentId Nothing 1 Nothing (Just "prev")
        status1 <- getStatus registry agentId
        status1 `shouldBe` Completed Nothing
        previous <- getPreviousResponseId registry agentId
        previous `shouldBe` Just "prev"
        Right _ <- sendInput registry agentId "after-restore" False
        (statuses, timedOut) <- waitSubagents registry [agentId] 15000
        timedOut `shouldBe` False
        Map.lookup agentId statuses
            `shouldBe` Just (Completed (Just "after-restore"))

    it "keeps subagent-owned resources across completion and releases them on close" do
        releases <- newIORef (0 :: Int)
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ prompt _ -> pure (completedResult (messagePayload prompt)))
            (\_ _ -> pure ())
        Right agentId <- spawnSubagentWithCwdPrepared registry (fromFilePath "/tmp")
            (\_ -> pure $ subagentLease $
                atomicModifyIORef' releases \n -> (n + 1, ()))
            Nothing 0 "first" Nothing
        _ <- waitSubagents registry [agentId] 15000
        readIORef releases `shouldReturn` 0
        Right _ <- sendInput registry agentId "second" False
        _ <- waitSubagents registry [agentId] 15000
        readIORef releases `shouldReturn` 0
        _ <- closeSubagent registry agentId
        readIORef releases `shouldReturn` 1
        closeSubagentRegistry registry
        readIORef releases `shouldReturn` 1

    it "releases open subagent-owned resources when the registry closes" do
        released <- newIORef False
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ prompt _ -> pure (completedResult (messagePayload prompt)))
            (\_ _ -> pure ())
        Right _ <- spawnSubagentWithCwdPrepared registry (fromFilePath "/tmp")
            (\_ -> pure $ subagentLease (writeIORef released True))
            Nothing 0 "first" Nothing
        closeSubagentRegistry registry
        readIORef released `shouldReturn` True

    it "releases composed subagent leases in reverse acquisition order" do
        released <- newIORef ([] :: [Int])
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ prompt _ -> pure (completedResult (messagePayload prompt)))
            (\_ _ -> pure ())
        Right agentId <- spawnSubagentWithCwdPrepared registry (fromFilePath "/tmp")
            (\_ -> pure $
                subagentLease
                    (atomicModifyIORef' released \xs -> (xs <> [1], ()))
                    <> subagentLease
                        (atomicModifyIORef' released \xs -> (xs <> [2], ())))
            Nothing 0 "first" Nothing
        _ <- closeSubagent registry agentId
        readIORef released `shouldReturn` [2, 1]
        closeSubagentRegistry registry

    it "releases nested subagent resources before their parent's resources" do
        released <- newIORef ([] :: [Text])
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ prompt _ -> pure (completedResult (messagePayload prompt)))
            (\_ _ -> pure ())
        Right parent <- spawnSubagentWithCwdPrepared registry (fromFilePath "/tmp")
            (\_ -> pure $ subagentLease $
                atomicModifyIORef' released \xs -> (xs <> ["parent"], ()))
            Nothing 0 "parent" Nothing
        _ <- waitSubagents registry [parent] 15000
        Right child <- spawnSubagentWithCwdPrepared registry (fromFilePath "/tmp")
            (\_ -> pure $ subagentLease $
                atomicModifyIORef' released \xs -> (xs <> ["child"], ()))
            (Just parent) 1 "child" Nothing
        _ <- waitSubagents registry [child] 15000
        _ <- closeSubagent registry parent
        readIORef released `shouldReturn` ["child", "parent"]
        closeSubagentRegistry registry

    it "does not let sendInput resurrect a closed agent" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ prompt _ -> pure $ Right LoopResult
                { finalResponseId = "r"
                , finalText = Just (messagePayload prompt)
                , turnsUsed = 1
                , tokenUsage = emptyTokenUsage
                })
            (\_ _ -> pure ())
        Right agentId <- spawnSubagent registry Nothing 0 "first" Nothing
        _ <- waitSubagents registry [agentId] 15000
        _ <- closeSubagent registry agentId
        sendInput registry agentId "should-not-run" False
            `shouldReturn` Left "agent is closed"
        getStatus registry agentId `shouldReturn` Closed

    it "reset cancels old supervisors and reopens the registry" do
        started <- newEmptyMVar
        notices <- newIORef ([] :: [SubagentId])
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ prompt _ ->
                let payload = messagePayload prompt
                in if payload == "old"
                    then putMVar started () >> atomically retry
                    else pure $ Right LoopResult
                        { finalResponseId = "new"
                        , finalText = Just payload
                        , turnsUsed = 1
                        , tokenUsage = emptyTokenUsage
                        })
            (\_ _ -> pure ())
        setSubagentOnComplete registry \agentId _ ->
            atomicModifyIORef' notices \ids -> (ids <> [agentId], ())
        Right old <- spawnSubagent registry Nothing 0 "old" Nothing
        takeMVar started
        resetSubagentRegistry registry
        getStatus registry old `shouldReturn` NotFound
        readIORef notices `shouldReturn` []
        Right fresh <- spawnSubagent registry Nothing 0 "fresh" Nothing
        (statuses, timedOut) <- waitSubagents registry [fresh] 15000
        timedOut `shouldBe` False
        Map.lookup fresh statuses
            `shouldBe` Just (Completed (Just "fresh"))

    it "rejects spawning a descendant under a closed parent" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ prompt _ -> pure $ Right LoopResult
                { finalResponseId = "r"
                , finalText = Just (messagePayload prompt)
                , turnsUsed = 1
                , tokenUsage = emptyTokenUsage
                })
            (\_ _ -> pure ())
        Right parent <- spawnSubagent registry Nothing 0 "parent" Nothing
        _ <- waitSubagents registry [parent] 15000
        _ <- closeSubagent registry parent
        result <- spawnSubagent registry (Just parent) 1 "child" Nothing
        result `shouldBe` Left "Parent subagent is closed or missing."

    it "restored agents receive a fresh cancellation flag" do
        seenCancelled <- newIORef ([] :: [Bool])
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\env _ prompt _ -> do
                cancelled <- isCancelled env.subCancel
                atomicModifyIORef' seenCancelled \xs -> (xs <> [cancelled], ())
                pure (completedResult (messagePayload prompt)))
            (\_ _ -> pure ())
        Right agentId <- spawnSubagent registry Nothing 0 "first" Nothing
        _ <- waitSubagents registry [agentId] 15000
        _ <- closeSubagent registry agentId
        Right _ <- restoreSubagent registry agentId Nothing 1 Nothing (Just "prev")
        Right _ <- sendInput registry agentId "second" False
        _ <- waitSubagents registry [agentId] 15000
        readIORef seenCancelled `shouldReturn` [False, False]

    it "restores canonical task topology" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ prompt _ -> pure (completedResult (messagePayload prompt)))
            (\_ _ -> pure ())
        let agentId = SubagentId "agent-restored-topology"
            parentId = SubagentId "agent-parent"
        Right path <- pure (parseTaskPath "/root/research/worker")
        Right _ <- restoreSubagentAt
            registry agentId (Just parentId) path 2 Nothing (Just "prev")
        getTaskPath registry agentId `shouldReturn` Just path
        resolveAgentTarget registry taskPathRoot "/root/research/worker"
            `shouldReturn` Right agentId
        identity <- getSubagentIdentity registry agentId
        identity `shouldBe` Just (SubagentIdentity (Just parentId) 2 path)

    it "spawns at a task path and resolves relative targets" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ prompt _ -> pure $ Right LoopResult
                { finalResponseId = "c"
                , finalText = Just (messagePayload prompt)
                , turnsUsed = 1
                , tokenUsage = emptyTokenUsage
                })
            (\_ _ -> pure ())
        Right (agentId, path) <-
            spawnSubagentAt registry Nothing taskPathRoot 0 "worker"
                (plainInterAgentContent "do it") Nothing
        taskPathText path `shouldBe` "/root/worker"
        resolved <- resolveAgentTarget registry taskPathRoot "worker"
        resolved `shouldBe` Right agentId
        agents <- listAgents registry (Just "/root")
        map (\(p, _, _) -> taskPathText p) agents `shouldContain` ["/root/worker"]
        closeSubagentRegistry registry

    it "queueMessage does not kick an idle agent" do
        started <- newIORef (0 :: Int)
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ prompt _ -> do
                atomicModifyIORef' started \n -> (n + 1, ())
                pure $ Right LoopResult
                    { finalResponseId = "c"
                    , finalText = Just (messagePayload prompt)
                    , turnsUsed = 1
                    , tokenUsage = emptyTokenUsage
                    })
            (\_ _ -> pure ())
        Right agentId <- spawnSubagent registry Nothing 0 "one" Nothing
        _ <- waitSubagents registry [agentId] 15000
        before <- readIORef started
        Right _ <- queueMessage registry agentId "queued-only"
        threadDelay 50000
        after <- readIORef started
        after `shouldBe` before
        status <- getStatus registry agentId
        status `shouldBe` Completed (Just "one")

    it "interrupts active descendants and keeps the registry reusable" do
        started <- newEmptyTMVarIO
        blocker <- newEmptyTMVarIO
        notices <- newIORef ([] :: [(SubagentId, SubagentStatus)])
        let config = defaultSubagentConfig { maxConcurrent = 1 }
        registry <- newSubagentRegistry config (fromFilePath "/tmp")
            (\_ _ _ _ -> do
                atomically $ putTMVar started ()
                atomically $ takeTMVar blocker
                pure $ Right LoopResult
                    { finalResponseId = "late"
                    , finalText = Just "late"
                    , turnsUsed = 1
                    , tokenUsage = emptyTokenUsage
                    })
            (\_ _ -> pure ())
        setSubagentOnComplete registry \agentId status ->
            atomicModifyIORef' notices \xs -> (xs <> [(agentId, status)], ())
        Right active <- spawnSubagent registry Nothing 0 "active" Nothing
        atomically $ takeTMVar started

        interruptActiveSubagents registry

        getStatus registry active `shouldReturn` Interrupted
        readIORef notices `shouldReturn` []
        replacement <- spawnSubagent registry Nothing 0 "replacement" Nothing
        replacement `shouldSatisfy` \case
            Right _ -> True
            Left _ -> False
        closeSubagentRegistry registry

    it "aborts only descendants owned by the failed root turn" do
        ownedStarted <- newEmptyTMVarIO
        unrelatedStarted <- newEmptyTMVarIO
        unrelatedGate <- newEmptyTMVarIO
        registry <- newSubagentRegistry
            (defaultSubagentConfig { maxConcurrent = 2 })
            (fromFilePath "/tmp")
            (\env _ prompt _ -> case messagePayload prompt of
                "owned" -> do
                    atomically $ putTMVar ownedStarted env.subRootTurnId
                    atomically retry
                _ -> do
                    atomically $ putTMVar unrelatedStarted env.subRootTurnId
                    atomically $ takeTMVar unrelatedGate
                    pure $ Right LoopResult
                        { finalResponseId = "unrelated"
                        , finalText = Just "unrelated"
                        , turnsUsed = 1
                        , tokenUsage = emptyTokenUsage
                        })
            (\_ _ -> pure ())
        ownedTurn <- beginRootTurn registry
        unrelatedTurn <- beginRootTurn registry
        Right owned <- spawnSubagentWithCwdForTurn registry (Just ownedTurn)
            (fromFilePath "/tmp") Nothing 0 "owned" Nothing
        Right unrelated <- spawnSubagentWithCwdForTurn registry (Just unrelatedTurn)
            (fromFilePath "/tmp") Nothing 0 "unrelated" Nothing
        atomically (takeTMVar ownedStarted) `shouldReturn` Just ownedTurn
        atomically (takeTMVar unrelatedStarted) `shouldReturn` Just unrelatedTurn

        abortRootTurn registry ownedTurn

        getStatus registry owned `shouldReturn` Interrupted
        getStatus registry unrelated `shouldReturn` Running
        atomically $ putTMVar unrelatedGate ()
        (statuses, timedOut) <- waitSubagents registry [unrelated] 15000
        timedOut `shouldBe` False
        Map.lookup unrelated statuses
            `shouldBe` Just (Completed (Just "unrelated"))
        rejected <- spawnSubagentWithCwdForTurn registry (Just ownedTurn)
            (fromFilePath "/tmp") Nothing 0 "too-late" Nothing
        rejected `shouldBe` Left "Root turn was aborted."
        closeSubagentRegistry registry

    it "can restart an interrupted agent in a later root turn" do
        started <- newEmptyTMVarIO
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ prompt _ ->
                if messagePayload prompt == "first"
                    then atomically (putTMVar started ()) >> atomically retry
                    else pure $ Right LoopResult
                        { finalResponseId = "second"
                        , finalText = Just (messagePayload prompt)
                        , turnsUsed = 1
                        , tokenUsage = emptyTokenUsage
                        })
            (\_ _ -> pure ())
        firstTurn <- beginRootTurn registry
        secondTurn <- beginRootTurn registry
        Right agentId <- spawnSubagentWithCwdForTurn registry (Just firstTurn)
            (fromFilePath "/tmp") Nothing 0 "first" Nothing
        atomically $ takeTMVar started
        abortRootTurn registry firstTurn
        getStatus registry agentId `shouldReturn` Interrupted
        Right _ <- sendInputMessageForTurn registry (Just secondTurn)
            taskPathRoot agentId (plainInterAgentContent "second") False
        (statuses, timedOut) <- waitSubagents registry [agentId] 15000
        timedOut `shouldBe` False
        Map.lookup agentId statuses
            `shouldBe` Just (Completed (Just "second"))
        closeSubagentRegistry registry

    it "removes failed-turn follow-ups queued behind unrelated work" do
        started <- newEmptyTMVarIO
        gate <- newEmptyTMVarIO
        seen <- newIORef ([] :: [Text])
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ prompt _ -> do
                atomicModifyIORef' seen \messages ->
                    (messages <> [messagePayload prompt], ())
                atomically $ putTMVar started ()
                atomically $ takeTMVar gate
                pure $ Right LoopResult
                    { finalResponseId = messagePayload prompt
                    , finalText = Just (messagePayload prompt)
                    , turnsUsed = 1
                    , tokenUsage = emptyTokenUsage
                    })
            (\_ _ -> pure ())
        originalTurn <- beginRootTurn registry
        failedTurn <- beginRootTurn registry
        Right agentId <- spawnSubagentWithCwdForTurn registry (Just originalTurn)
            (fromFilePath "/tmp") Nothing 0 "original" Nothing
        atomically $ takeTMVar started
        Right _ <- sendInputMessageForTurn registry (Just failedTurn)
            taskPathRoot agentId (plainInterAgentContent "failed-follow-up") False

        abortRootTurn registry failedTurn
        atomically $ putTMVar gate ()
        _ <- waitSubagents registry [agentId] 15000

        readIORef seen `shouldReturn` ["original"]
        getStatus registry agentId
            `shouldReturn` Completed (Just "original")
        closeSubagentRegistry registry

shutdownRaceRegistry :: IORef Bool -> IO SubagentRegistry
shutdownRaceRegistry ran =
    newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
        (\_ _ _ _ -> writeIORef ran True >> pure (Left LoopNoResponseId))
        (\_ _ -> pure ())

blockPreparation :: MVar () -> MVar () -> SubagentId -> IO SubagentLease
blockPreparation entered release _ =
    putMVar entered () >> takeMVar release >> pure mempty

waitForMVarBlock :: String -> Async.Async a -> IO ()
waitForMVarBlock label running = go (1000 :: Int)
  where
    go 0 = expectationFailure (label <> " thread did not block on an MVar")
    go attempts =
        threadStatus (Async.asyncThreadId running) >>= \case
            ThreadBlocked BlockedOnMVar -> pure ()
            ThreadFinished -> expectationFailure (label <> " thread finished before cancellation")
            ThreadDied -> expectationFailure (label <> " thread died before cancellation")
            _ -> threadDelay 1000 >> go (attempts - 1)

spawnPreparedAt
    :: SubagentRegistry
    -> MVar ()
    -> MVar ()
    -> IO (Either Text (SubagentId, TaskPath))
spawnPreparedAt registry entered release =
    spawnSubagentAtWithCwdPrepared registry (fromFilePath "/tmp")
        (blockPreparation entered release)
        Nothing taskPathRoot 0 "worker"
        (plainInterAgentContent "old") Nothing
