{-# LANGUAGE RankNTypes, OverloadedStrings, OverloadedRecordDot, BlockArguments #-}
module Agent.Subagents.RecoverySpec (spec) where

import Agent.Subagents
import Agent.Subagents.Runtime (RuntimeHandle (..), RuntimeHooks (..))
import Agent.Subagents.Runtime.Recovery
import Agent.Subagents.TaskPath (parseTaskPath)
import qualified Agent.Loop as Loop
import Agent.InterAgentMessage (renderInterAgentMessage)
import Control.Concurrent.MVar
import Control.Concurrent (myThreadId)
import Control.Exception (bracket, finally, onException)
import Data.IORef
import Data.List (find, delete)
import Data.Text (Text)
import qualified Data.Text as Text
import System.OsPath (unsafeEncodeUtf)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "durable tree recovery" do
    it "guards initialization, child execution, settlement and recovery with the cached scope" $
        withHandle \unscoped -> do
            active <- newIORef []
            executing <- newIORef []
            observed <- newIORef ([] :: [Text])
            notified <- newEmptyMVar
            (config, _) <- configuration [snapshot "child" Pending]
            let operation action = bracket
                    (myThreadId >>= \thread -> atomicModifyIORef' active (\threads -> (thread : threads, thread)))
                    (\thread -> atomicModifyIORef' active (\threads -> (delete thread threads, ())))
                    (const action)
                execution action = bracket
                    (myThreadId >>= \thread -> atomicModifyIORef' executing (\threads -> (thread : threads, thread)))
                    (\thread -> atomicModifyIORef' executing (\threads -> (delete thread threads, ())))
                    (const action)
                checkExecution label = do
                    thread <- myThreadId
                    threads <- readIORef executing
                    thread `shouldSatisfy` (`elem` threads)
                    operations <- readIORef active
                    thread `shouldSatisfy` (`notElem` operations)
                    atomicModifyIORef' observed (\labels -> (label : labels, ()))
                checkGuard label = do
                    thread <- myThreadId
                    threads <- readIORef active
                    thread `shouldSatisfy` (`elem` threads)
                    atomicModifyIORef' observed (\labels -> (label : labels, ()))
                handle = unscoped { withOperation = operation, withExecution = execution } :: RuntimeHandle
                store = config.store
                    { transaction = \action -> action store
                    , persistRunning = \_ _ -> checkGuard "running"
                    , persistStatus = \_ _ -> checkGuard "status"
                    , settle = \_ _ -> checkGuard "settle"
                    }
                tested = config
                    { store
                    , withOperation = \_ -> fail "must use cached handle scope"
                    , withExecution = \_ -> fail "must use cached execution scope"
                    , executionAllowed = checkExecution "allowed" >> pure True
                    , runner = \_ _ _ _ _ -> checkExecution "runner" >> pure (Left Loop.LoopNoResponseId)
                    , notify = \_ _ -> checkGuard "notify" >> tryPutMVar notified () >> pure ()
                    }
                hooks = durableRuntimeHooks tested
            hooks.initialize handle
            timeout 1000000 (takeMVar notified) `shouldReturn` Just ()
            hooks.pumpRunnable handle `shouldReturn` True
            labels <- readIORef observed
            mapM_ (\label -> labels `shouldSatisfy` elem label)
                ["allowed", "running", "runner", "status", "settle", "notify"]

    it "joins a running child before acknowledging its durable cancellation" $
        withHandle \unscoped -> do
            (config, rows) <- configuration [snapshot "child" Pending]
            executions <- newIORef (0 :: Int)
            uncertain <- newIORef (0 :: Int)
            started <- newEmptyMVar
            finished <- newEmptyMVar
            blocker <- newEmptyMVar
            let execution action = bracket
                    (atomicModifyIORef' executions (\n -> (n + 1, ())))
                    (\_ -> atomicModifyIORef' executions (\n -> (n - 1, ())))
                    (const action)
                operation action = action `onException` atomicModifyIORef' uncertain (\n -> (n + 1, ()))
                handle = unscoped { withExecution = execution, withOperation = operation } :: RuntimeHandle
                tested = config
                    { runner = \_ _ _ _ _ ->
                        (putMVar started () >> readMVar blocker >> pure (Left Loop.LoopNoResponseId))
                            `finally` putMVar finished ()
                    }
                hooks = durableRuntimeHooks tested
            hooks.initialize handle
            timeout 1000000 (takeMVar started) `shouldReturn` Just ()
            modifyIORef' rows (map \row -> row { cancellationRequested = True })
            timeout 1000000 (hooks.pumpRunnable handle) `shouldReturn` Just True
            tryTakeMVar finished `shouldReturn` Just ()
            readIORef executions `shouldReturn` 0
            readIORef uncertain `shouldReturn` 0
            getStatus handle.registry (SubagentId "child") `shouldReturn` Closed
            persisted <- readIORef rows
            map (\row -> (row.status, row.cancellationRequested)) persisted `shouldBe` [(Closed, False)]

    it "honors cancellation after restart without executing the interrupted task" $
        withHandle \handle -> do
            let child = (snapshot "child" Running) { cancellationRequested = True }
            (config, rows) <- configuration [child]
            executions <- newIORef (0 :: Int)
            let hooks = durableRuntimeHooks config
                    { runner = \_ _ _ _ _ -> modifyIORef' executions (+ 1) >> pure (Left Loop.LoopNoResponseId) }
            hooks.initialize handle
            readIORef executions `shouldReturn` 0
            getStatus handle.registry (SubagentId "child") `shouldReturn` Closed
            persisted <- readIORef rows
            map (.status) persisted `shouldBe` [Closed]

    it "does not reconcile a completion over an outstanding cancellation" $
        withHandle \handle -> do
            (config, rows) <- configuration [snapshot "child" (Completed Nothing)]
            restoreStoredSubagent config.store config.cwd handle (SubagentId "child") `shouldReturn` Right ()
            writeIORef rows [(snapshot "child" Running) { cancellationRequested = True }]
            reconcileStoredSubagents config handle
            persisted <- readIORef rows
            map (\row -> (row.status, row.cancellationRequested)) persisted `shouldBe` [(Running, True)]

    it "leaves work durable when the execution limit is reached" $
        withHandle \handle -> do
            (config, rows) <- configuration [snapshot "first" Pending, snapshot "second" Running]
            started <- newEmptyMVar
            release <- newEmptyMVar
            admitted <- newIORef ([] :: [SubagentId])
            let tested = config
                    { runner = \_ environment _ _ _ -> do
                        putMVar started environment.subId
                        readMVar release
                        pure (Left Loop.LoopNoResponseId)
                    , store = config.store
                        { persistRunning = \agent _ -> modifyIORef' admitted (<> [agent]) }
                    }
            (durableRuntimeHooks tested).initialize handle
            timeout 1000000 (takeMVar started) `shouldReturn` Just (SubagentId "first")
            getStatus handle.registry (SubagentId "second") `shouldReturn` Interrupted
            readIORef admitted `shouldReturn` [SubagentId "first"]
            persisted <- readIORef rows
            map (.status) persisted `shouldBe` [Pending, Running]

    it "resumes completed work with the host wake instruction" $
        withHandle \handle -> do
            let child = (snapshot "child" (Completed Nothing)) { wakeRequested = True }
            (config, _) <- configuration [child]
            admitted <- newEmptyMVar
            release <- newEmptyMVar
            let tested = config
                    { runner = \_ _ _ message _ -> do
                        putMVar admitted (renderInterAgentMessage message)
                        readMVar release
                        pure (Left Loop.LoopNoResponseId)
                    }
            (durableRuntimeHooks tested).initialize handle
            result <- timeout 1000000 (takeMVar admitted)
            result `shouldSatisfy` maybe False (Text.isInfixOf config.wakeMessage)

    it "restores running work as interrupted without retaining a phantom worker" $
        withHandle \handle -> do
            (config, _) <- configuration [snapshot "child" Running]
            restoreStoredSubagent config.store config.cwd handle (SubagentId "child") `shouldReturn` Right ()
            getStatus handle.registry (SubagentId "child") `shouldReturn` Interrupted

    it "refuses permanently closed persisted children" $
        withHandle \handle -> do
            (config, _) <- configuration [snapshot "child" Closed]
            result <- restoreStoredSubagent config.store config.cwd handle (SubagentId "child")
            result `shouldBe` Left "Unknown or permanently closed subagent"
            getStatus handle.registry (SubagentId "child") `shouldReturn` NotFound

    it "reconciles status and domain settlement transactionally before notifying" $
        withHandle \handle -> do
            (config, rows) <- configuration [snapshot "child" (Completed (Just "done"))]
            restoreStoredSubagent config.store config.cwd handle (SubagentId "child") `shouldReturn` Right ()
            writeIORef rows [snapshot "child" Running]
            events <- newIORef ([] :: [Text])
            let record event = modifyIORef' events (<> [event])
                store = config.store
                    { transaction = \action -> record "begin" >> action store `finally` record "commit"
                    , lockSnapshot = \_ -> record "lock"
                    , persistStatus = \agent status -> record "status" >> config.store.persistStatus agent status
                    , settle = \agent status -> record "settle" >> config.store.settle agent status
                    }
                tested = config
                    { store
                    , notify = \_ _ -> record "notify"
                    }
            reconcileStoredSubagents tested handle
            readIORef events `shouldReturn` ["begin", "lock", "status", "settle", "commit", "notify"]
            writeIORef events []
            reconcileStoredSubagents tested handle
            readIORef events `shouldReturn` []

    it "settles a stored status once when its settlement has not committed" $
        withHandle \handle -> do
            (config, rows) <- configuration [snapshot "child" (Completed Nothing)]
            restoreStoredSubagent config.store config.cwd handle (SubagentId "child") `shouldReturn` Right ()
            transactions <- newIORef (0 :: Int)
            let store = config.store
                    { transaction = \action -> modifyIORef' transactions (+ 1) >> action store }
                tested = config { store }
            reconcileStoredSubagents tested handle
            reconcileStoredSubagents tested handle
            readIORef transactions `shouldReturn` 1
            map (.settled) <$> readIORef rows `shouldReturn` [True]

    it "reads the status again after locking rather than notifying from stale inventory" $
        withHandle \handle -> do
            (config, rows) <- configuration [snapshot "child" (Completed Nothing)]
            restoreStoredSubagent config.store config.cwd handle (SubagentId "child") `shouldReturn` Right ()
            writeIORef rows [snapshot "child" Running]
            notices <- newIORef (0 :: Int)
            let store = config.store
                    { transaction = \action -> action store
                    , lockSnapshot = \_ -> writeIORef rows [snapshot "child" (Completed Nothing)] }
                tested = config
                    { store
                    , notify = \_ _ -> modifyIORef' notices (+ 1)
                    }
            reconcileStoredSubagents tested handle
            readIORef notices `shouldReturn` 0

    it "uses transaction-bound callbacks for reconciliation and cancellation commits" $
        withHandle \handle -> do
            (config, rows) <- configuration [snapshot "child" (Completed Nothing)]
            restoreStoredSubagent config.store config.cwd handle (SubagentId "child") `shouldReturn` Right ()
            writeIORef rows [snapshot "child" Running]
            let unbound = config.store
                    { lockSnapshot = \_ -> fail "unbound lock"
                    , loadSnapshot = \_ -> fail "unbound snapshot read"
                    , persistStatus = \_ _ -> fail "unbound status write"
                    , settle = \_ _ -> fail "unbound settlement"
                    }
                tested = config { store = unbound }
            reconcileStoredSubagents tested handle
            map (.status) <$> readIORef rows `shouldReturn` [Completed Nothing]
            modifyIORef' rows (map \row -> row { cancellationRequested = True })
            (durableRuntimeHooks tested).pumpRunnable handle `shouldReturn` True
            map (.status) <$> readIORef rows `shouldReturn` [Closed]

    it "rolls back a status write when transaction-bound settlement fails" $
        withHandle \handle -> do
            (config, rows) <- configuration [snapshot "child" (Completed Nothing)]
            restoreStoredSubagent config.store config.cwd handle (SubagentId "child") `shouldReturn` Right ()
            writeIORef rows [snapshot "child" Running]
            notices <- newIORef (0 :: Int)
            let bound = config.store { settle = \_ _ -> fail "settlement failed" }
                store = config.store
                    { transaction = \action -> do
                        previous <- readIORef rows
                        action bound `onException` writeIORef rows previous
                    }
                tested = config { store, notify = \_ _ -> modifyIORef' notices (+ 1) }
            reconcileStoredSubagents tested handle `shouldThrow` anyIOException
            map (.status) <$> readIORef rows `shouldReturn` [Running]
            readIORef notices `shouldReturn` 0

    it "cancels dormant children on access loss and persists their terminal state" $
        withHandle \handle -> do
            (config, rows) <- configuration [snapshot "child" Interrupted]
            let hooks = durableRuntimeHooks config
            hooks.initialize handle
            let disabled = durableRuntimeHooks config { executionAllowed = pure False }
            disabled.pumpRunnable handle `shouldReturn` False
            getStatus handle.registry (SubagentId "child") `shouldReturn` Closed
            persisted <- readIORef rows
            map (.status) persisted `shouldBe` [Closed]

    it "rejects unauthorized initialization before restoring any identity" $
        withHandle \handle -> do
            (config, _) <- configuration [snapshot "child" Interrupted]
            let hooks = durableRuntimeHooks config { executionAllowed = pure False }
            hooks.initialize handle `shouldThrow` anyIOException
            getStatus handle.registry (SubagentId "child") `shouldReturn` NotFound

snapshot :: Text -> SubagentStatus -> SubagentSnapshot
snapshot identifier status = SubagentSnapshot
    { agentId = SubagentId identifier
    , parentId = Nothing
    , taskPath = either (error . Text.unpack) id (parseTaskPath ("/root/" <> identifier))
    , depth = 1
    , status
    , initialMessage = "Recover this task"
    , wakeRequested = False
    , cancellationRequested = False
    , settled = False
    }

configuration :: [SubagentSnapshot] -> IO (RecoveryConfig, IORef [SubagentSnapshot])
configuration initial = do
    rows <- newIORef initial
    let store = RecoveryStore
            { loadSnapshots = readIORef rows
            , loadSnapshot = \agent -> find (\row -> row.agentId == agent) <$> readIORef rows
            , transaction = \action -> action store
            , lockSnapshot = \_ -> pure ()
            , persistStatus = \agent status -> modifyIORef' rows $
                map \row -> if row.agentId == agent then row
                    { status
                    , cancellationRequested = status /= Closed && row.cancellationRequested
                    , settled = False
                    } else row
            , persistRunning = \_ _ -> pure ()
            , persistSuspension = \_ _ _ -> pure (Right ())
            , persistResume = \_ _ -> pure (Right ())
            , settle = \agent _ -> modifyIORef' rows $
                map \row -> if row.agentId == agent then row { settled = True } else row
            }
    pure (RecoveryConfig
        { store
        , withOwnership = fmap Just
        , withOperation = id
        , withExecution = id
        , executionAllowed = pure True
        , runner = \_ _ _ _ _ -> pure (Left Loop.LoopNoResponseId)
        , notify = \_ _ -> pure ()
        , cwd = unsafeEncodeUtf "."
        , wakeMessage = "Read pending input"
        }, rows)

withHandle :: (RuntimeHandle -> IO a) -> IO a
withHandle action = bracket
    (newSubagentRegistry (SubagentConfig 1 (Just 4) (Just 12)) (unsafeEncodeUtf ".")
        (\_ _ _ _ -> pure (Left Loop.LoopNoResponseId)) (\_ _ -> pure ()))
    closeSubagentRegistry
    \registry -> do
        root <- newRootSubagents NoticesToLoop registry
        action RuntimeHandle { registry, root, withOperation = id, withExecution = id }
