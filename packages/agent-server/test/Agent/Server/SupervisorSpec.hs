module Agent.Server.SupervisorSpec (spec) where

import Agent.Server.Runtime.TurnStore (takeRotatingPendingBatch)
import Agent.Server.Supervisor
import Agent.Server.Types
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (
    async,
    asyncThreadId,
    cancel,
    wait,
    waitCatch,
    withAsync,
 )
import Control.Concurrent.MVar (
    MVar,
    newEmptyMVar,
    putMVar,
    takeMVar,
    tryPutMVar,
 )
import Control.Concurrent.STM (
    atomically,
    modifyTVar',
    newTVarIO,
    readTBQueue,
    readTVar,
    writeTVar,
 )
import Control.Exception.Safe (bracket, finally)
import Control.Monad (forM_, void)
import Data.Aeson (object)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "turn supervisor" do
    it "rotates bounded durable cleanup retry batches" do
        let entries :: Map.Map Int Int
            entries =
                Map.fromList
                    [ (1, 1)
                    , (2, 2)
                    , (3, 3)
                    , (4, 4)
                    , (5, 5)
                    ]
        pending <- newTVarIO entries
        cursor <- newTVarIO Nothing
        takeRotatingPendingBatch 4 pending cursor
            `shouldReturn` [1, 2, 3, 4]
        takeRotatingPendingBatch 4 pending cursor
            `shouldReturn` [5, 1, 2, 3]
        atomically (modifyTVar' pending (Map.delete 3))
        takeRotatingPendingBatch 4 pending cursor
            `shouldReturn` [4, 5, 1, 2]

    it "enforces one active turn per session" do
        release <- newEmptyMVar
        let runner _ _ = takeMVar release >> pure (Right testOutput)
        withSupervisor runner \supervisor -> do
            first <- submitTurn supervisor (turnSpec "session-a")
            first `shouldSatisfy` isRight
            submitTurn supervisor (turnSpec "session-a")
                `shouldReturn` Left SubmitSessionBusy
            putMVar release ()

    it "scopes the active-session lock to the exact access boundary" do
        firstStarted <- newEmptyMVar
        secondStarted <- newEmptyMVar
        release <- newEmptyMVar
        let tenantA = testBoundary tenantAId (Just "gateway")
            tenantB = testBoundary tenantBId (Just "gateway")
            runner _ turn = do
                if turn.turnSpecBoundary == tenantA
                    then putMVar firstStarted ()
                    else putMVar secondStarted ()
                takeMVar release
                pure (Right testOutput)
        withSupervisorConfig
            defaultConfig
                { supervisorMaxConcurrentTurns = 2
                , supervisorMaxConcurrentTurnsPerTenant = 2
                }
            runner
            \supervisor -> do
                first <-
                    submitTurn
                        supervisor
                        (turnSpecFor tenantA "same-session")
                first `shouldSatisfy` isRight
                second <-
                    submitTurn
                        supervisor
                        (turnSpecFor tenantB "same-session")
                second `shouldSatisfy` isRight
                takeWithin firstStarted
                takeWithin secondStarted
                sessionHasActiveTurn supervisor tenantA "same-session"
                    `shouldReturn` True
                sessionHasActiveTurn supervisor tenantB "same-session"
                    `shouldReturn` True
                putMVar release ()
                putMVar release ()

    it "atomically excludes turn admission for a session mutation" do
        entered <- newEmptyMVar
        release <- newEmptyMVar
        withSupervisor (\_ _ -> pure (Right testOutput)) \supervisor -> do
            mutation <-
                async $
                    withSessionMutation
                        supervisor
                        localAccessBoundary
                        "session-a"
                        (putMVar entered () >> takeMVar release)
            takeWithin entered
            submitTurn supervisor (turnSpec "session-a")
                `shouldReturn` Left SubmitSessionBusy
            putMVar release ()
            wait mutation `shouldReturn` Right ()

    it "waits to clean up behind a transient session reservation" do
        entered <- newEmptyMVar
        release <- newEmptyMVar
        cleaned <- newEmptyMVar
        withSupervisor (\_ _ -> pure (Right testOutput)) \supervisor -> do
            mutation <-
                async $
                    withSessionMutation
                        supervisor
                        localAccessBoundary
                        "session-a"
                        (putMVar entered () >> takeMVar release)
            takeWithin entered
            cleanup <-
                async $
                    withSessionCleanup
                        supervisor
                        localAccessBoundary
                        "session-a"
                        (putMVar cleaned ())
            timeout 50000 (takeMVar cleaned) `shouldReturn` Nothing
            putMVar release ()
            wait mutation `shouldReturn` Right ()
            wait cleanup `shouldReturn` Right ()

    it "admits a durable winner after a losing local mutation unwinds" do
        entered <- newEmptyMVar
        release <- newEmptyMVar
        withSupervisor (\_ _ -> pure (Right testOutput)) \supervisor -> do
            mutation <-
                async $
                    withSessionMutation
                        supervisor
                        localAccessBoundary
                        "session-a"
                        (putMVar entered () >> takeMVar release)
            takeWithin entered
            createdAt <- getCurrentTime
            let spec' = turnSpec "session-a"
                reserved =
                    TurnRecord
                        { turnRecordId =
                            TurnId
                                "01999999-2222-7222-8222-222222222222"
                        , turnRecordSessionId = spec'.turnSpecSessionId
                        , turnRecordClientRequestId =
                            spec'.turnSpecClientRequestId
                        , turnRecordBoundary = spec'.turnSpecBoundary
                        , turnRecordStatus = TurnQueued
                        , turnRecordCreatedAt = createdAt
                        , turnRecordStartedAt = Nothing
                        , turnRecordFinishedAt = Nothing
                        , turnRecordError = Nothing
                        }
            admission <-
                async $
                    submitReservedTurnChecked
                        supervisor
                        spec'
                        reserved
                        (pure (Right () :: Either Text ()))
            timeout 50000 (wait admission) `shouldReturn` Nothing
            putMVar release ()
            wait mutation `shouldReturn` Right ()
            admitted <- wait admission
            admitted `shouldSatisfy` isRight

    it "holds the session reservation across checked turn validation" do
        entered <- newEmptyMVar
        release <- newEmptyMVar
        withSupervisor (\_ _ -> pure (Right testOutput)) \supervisor -> do
            admission <-
                async $
                    submitTurnChecked
                        supervisor
                        (turnSpec "session-a")
                        ( putMVar entered ()
                            >> takeMVar release
                            >> pure (Right () :: Either Text ())
                        )
            takeWithin entered
            withSessionMutation
                supervisor
                localAccessBoundary
                "session-a"
                (pure ())
                `shouldReturn` Left SessionMutationBusy
            putMVar release ()
            result <- wait admission
            result `shouldSatisfy` isRight

    it "bounds a stuck in-band cancel hook and joins the worker" do
        started <- newEmptyMVar
        never <- newEmptyMVar
        let runner control _ = do
                _ <- control.turnControlRegisterCancel (takeMVar never)
                putMVar started ()
                takeMVar never
                pure (Right testOutput)
        withSupervisor runner \supervisor -> do
            submitted <- submitTurn supervisor (turnSpec "session-a")
            turn <- case submitted of
                Left err ->
                    expectationFailure ("turn rejected: " <> show err)
                        >> fail "unreachable"
                Right accepted -> pure accepted
            takeWithin started
            cancelled <-
                timeout
                    (2 * 1000 * 1000)
                    ( cancelTurn
                        supervisor
                        localAccessBoundary
                        turn.turnRecordId
                    )
            case cancelled of
                Just (Right record) ->
                    record.turnRecordStatus `shouldBe` TurnCancelled
                other ->
                    expectationFailure
                        ("cancellation did not finish: " <> show other)
            sessionHasActiveTurn
                supervisor
                localAccessBoundary
                "session-a"
                `shouldReturn` False

    it "makes an interrupted cancellation retryable" do
        started <- newEmptyMVar
        terminalEntered <- newEmptyMVar
        never <- newEmptyMVar
        attempts <- newIORef (0 :: Int)
        let runner _ _ =
                putMVar started ()
                    >> takeMVar never
                    >> pure (Right testOutput)
            persistence =
                inMemoryTurnPersistence
                    { turnPersistenceStarted = \_ _ -> pure (Right ())
                    , turnPersistenceTerminal = \record finishedAt outcome -> do
                        attempt <-
                            atomicModifyIORef' attempts \count ->
                                let next = count + 1
                                 in (next, next)
                        if attempt == 1
                            then putMVar terminalEntered () >> takeMVar never
                            else
                                pure
                                    ( Right
                                        ( testTerminalRecord
                                            finishedAt
                                            outcome
                                            record
                                        )
                                    )
                    , turnPersistenceShouldCancel = \_ ->
                        pure (Right False)
                    }
        bracket
            ( newSupervisorWithBoundaryGuardAndPersistence
                defaultConfig
                (\_ action -> Right <$> action)
                persistence
                runner
            )
            closeSupervisor
            \supervisor -> do
                submitted <-
                    submitTurn supervisor (turnSpec "session-a")
                        >>= expectRight
                takeWithin started
                firstCancellation <-
                    async $
                        cancelTurn
                            supervisor
                            localAccessBoundary
                            submitted.turnRecordId
                takeWithin terminalEntered
                cancel firstCancellation
                firstResult <- waitCatch firstCancellation
                firstResult `shouldSatisfy` \case
                    Left _ -> True
                    Right _ -> False
                retried <-
                    cancelTurn
                        supervisor
                        localAccessBoundary
                        submitted.turnRecordId
                retried `shouldSatisfy` \case
                    Right record ->
                        record.turnRecordStatus == TurnCancelled
                    Left _ -> False
                readIORef attempts `shouldReturn` 2
                sessionHasActiveTurn
                    supervisor
                    localAccessBoundary
                    "session-a"
                    `shouldReturn` False

    it "preserves the incumbent cancellation task when a duplicate loses" do
        never <- newEmptyMVar
        let turnId = TurnId "01999999-3333-7333-8333-333333333333"
        withAsync (takeMVar never) \incumbent ->
            withAsync (takeMVar never) \duplicate -> do
                let registered = Map.singleton turnId incumbent
                    afterDuplicate =
                        deleteCancellationTaskIfOwned
                            turnId
                            (asyncThreadId duplicate)
                            registered
                    afterIncumbent =
                        deleteCancellationTaskIfOwned
                            turnId
                            (asyncThreadId incumbent)
                            registered
                Map.member turnId afterDuplicate `shouldBe` True
                Map.member turnId afterIncumbent `shouldBe` False

    it "cancels active workers concurrently during shutdown" do
        firstStarted <- newEmptyMVar
        secondStarted <- newEmptyMVar
        firstCancelled <- newEmptyMVar
        secondCancelled <- newEmptyMVar
        releaseFirst <- newEmptyMVar
        releaseSecond <- newEmptyMVar
        never <- newEmptyMVar
        let runner _ turn = do
                let (started, cancelled, release)
                        | turn.turnSpecSessionId == "session-a" =
                            (firstStarted, firstCancelled, releaseFirst)
                        | otherwise =
                            (secondStarted, secondCancelled, releaseSecond)
                (putMVar started () >> takeMVar never)
                    `finally` (putMVar cancelled () >> takeMVar release)
        withSupervisorConfig
            defaultConfig
                { supervisorMaxConcurrentTurns = 2
                , supervisorMaxConcurrentTurnsPerTenant = 2
                }
            runner
            \supervisor -> do
                void (submitTurn supervisor (turnSpec "session-a"))
                void (submitTurn supervisor (turnSpec "session-b"))
                takeWithin firstStarted
                takeWithin secondStarted

                withAsync (closeSupervisor supervisor) \shutdown ->
                    finally
                        ( do
                            cancelledTogether <-
                                timeout 500_000 do
                                    takeMVar firstCancelled
                                    takeMVar secondCancelled
                            putMVar releaseFirst ()
                            putMVar releaseSecond ()
                            wait shutdown

                            cancelledTogether `shouldBe` Just ()
                        )
                        ( do
                            void (tryPutMVar releaseFirst ())
                            void (tryPutMVar releaseSecond ())
                        )

    it "persists active turns as cancelled during graceful shutdown" do
        started <- newEmptyMVar
        never <- newEmptyMVar
        terminal <- newEmptyMVar
        let runner _ _ =
                putMVar started ()
                    >> takeMVar never
                    >> pure (Right testOutput)
            persistence =
                inMemoryTurnPersistence
                    { turnPersistenceStarted = \_ _ -> pure (Right ())
                    , turnPersistenceTerminal = \record finishedAt outcome -> do
                        putMVar terminal (record.turnRecordId, outcome)
                        pure
                            ( Right
                                ( testTerminalRecord
                                    finishedAt
                                    outcome
                                    record
                                )
                            )
                    , turnPersistenceShouldCancel = \_ ->
                        pure (Right False)
                    }
        bracket
            ( newSupervisorWithBoundaryGuardAndPersistence
                defaultConfig
                (\_ action -> Right <$> action)
                persistence
                runner
            )
            closeSupervisor
            \supervisor -> do
                submitted <-
                    submitTurn supervisor (turnSpec "session-a")
                        >>= expectRight
                takeWithin started
                closeSupervisor supervisor
                takeWithin terminal
                    `shouldReturn` (submitted.turnRecordId, TurnWasCancelled)

    it "observes durable cancellation requested by another instance" do
        started <- newEmptyMVar
        never <- newEmptyMVar
        terminal <- newEmptyMVar
        requested <- newIORef False
        let runner _ _ =
                putMVar started ()
                    >> takeMVar never
                    >> pure (Right testOutput)
            persistence =
                inMemoryTurnPersistence
                    { turnPersistenceStarted = \_ _ -> pure (Right ())
                    , turnPersistenceTerminal = \record finishedAt outcome -> do
                        putMVar terminal outcome
                        pure
                            ( Right
                                ( testTerminalRecord
                                    finishedAt
                                    outcome
                                    record
                                )
                            )
                    , turnPersistenceShouldCancel = \_ ->
                        Right <$> readIORef requested
                    }
        bracket
            ( newSupervisorWithBoundaryGuardAndPersistence
                defaultConfig
                (\_ action -> Right <$> action)
                persistence
                runner
            )
            closeSupervisor
            \supervisor -> do
                submitted <-
                    submitTurn supervisor (turnSpec "session-a")
                        >>= expectRight
                takeWithin started
                atomicModifyIORef' requested (const (True, ()))
                takeWithin terminal `shouldReturn` TurnWasCancelled
                fmap
                    (.turnRecordStatus)
                    ( awaitTurnStatus
                        supervisor
                        submitted.turnRecordId
                        TurnCancelled
                    )
                    `shouldReturn` TurnCancelled

    it "stops a worker when its durable owner fence cannot be confirmed" do
        started <- newEmptyMVar
        never <- newEmptyMVar
        cancellationDelivered <- newEmptyMVar
        terminal <- newEmptyMVar
        failControlCheck <- newIORef False
        let runner control _ = do
                _ <-
                    control.turnControlRegisterCancel
                        (void (tryPutMVar cancellationDelivered ()))
                putMVar started ()
                _ <- takeMVar never
                pure (Right testOutput)
            persistence =
                inMemoryTurnPersistence
                    { turnPersistenceStarted = \_ _ -> pure (Right ())
                    , turnPersistenceTerminal = \record finishedAt outcome -> do
                        putMVar terminal outcome
                        pure
                            ( Right
                                ( testTerminalRecord
                                    finishedAt
                                    outcome
                                    record
                                )
                            )
                    , turnPersistenceShouldCancel = \_ -> do
                        failing <- readIORef failControlCheck
                        pure
                            ( if failing
                                then Left "owner lease unavailable"
                                else Right False
                            )
                    }
        bracket
            ( newSupervisorWithBoundaryGuardAndPersistence
                defaultConfig
                (\_ action -> Right <$> action)
                persistence
                runner
            )
            closeSupervisor
            \supervisor -> do
                submitted <-
                    submitTurn supervisor (turnSpec "session-a")
                        >>= expectRight
                takeWithin started
                atomicModifyIORef' failControlCheck (const (True, ()))
                takeWithin cancellationDelivered
                takeWithin terminal `shouldReturn` TurnWasCancelled
                fmap
                    (.turnRecordStatus)
                    ( awaitTurnStatus
                        supervisor
                        submitted.turnRecordId
                        TurnCancelled
                    )
                    `shouldReturn` TurnCancelled

    it "bounds concurrent durable cancellation checks" do
        never <- newEmptyMVar
        concurrency <- newIORef (0 :: Int, 0 :: Int)
        let runner _ _ =
                takeMVar never >> pure (Right testOutput)
            persistence =
                inMemoryTurnPersistence
                    { turnPersistenceShouldCancel = \_ -> do
                        atomicModifyIORef' concurrency \(active, peak) ->
                            let active' = active + 1
                             in ((active', max peak active'), ())
                        threadDelay 50000
                        atomicModifyIORef' concurrency \(active, peak) ->
                            ((active - 1, peak), ())
                        pure (Right False)
                    }
            config =
                defaultConfig
                    { supervisorMaxQueuedTurns = 20
                    , supervisorMaxQueuedTurnsPerTenant = 20
                    }
        bracket
            ( newSupervisorWithBoundaryGuardAndPersistence
                config
                (\_ action -> Right <$> action)
                persistence
                runner
            )
            closeSupervisor
            \supervisor -> do
                forM_
                    [ "session-01"
                    , "session-02"
                    , "session-03"
                    , "session-04"
                    , "session-05"
                    , "session-06"
                    , "session-07"
                    , "session-08"
                    , "session-09"
                    , "session-10"
                    , "session-11"
                    , "session-12"
                    ]
                    \sessionId ->
                        void $
                            submitTurn supervisor (turnSpec sessionId)
                                >>= expectRight
                threadDelay 400000
                (_, peak) <- readIORef concurrency
                peak `shouldSatisfy` \value ->
                    value > 0 && value <= 4

    it "retries terminal persistence before exposing completion" do
        attempts <- newIORef (0 :: Int)
        persisted <- newEmptyMVar
        let persistence =
                inMemoryTurnPersistence
                    { turnPersistenceStarted = \_ _ -> pure (Right ())
                    , turnPersistenceTerminal = \record finishedAt outcome -> do
                        attempt <-
                            atomicModifyIORef' attempts \count ->
                                let next = count + 1
                                 in (next, next)
                        if attempt < 3
                            then pure (Left "database unavailable")
                            else do
                                putMVar persisted ()
                                pure
                                    ( Right
                                        ( testTerminalRecord
                                            finishedAt
                                            outcome
                                            record
                                        )
                                    )
                    , turnPersistenceShouldCancel = \_ ->
                        pure (Right False)
                    }
        bracket
            ( newSupervisorWithBoundaryGuardAndPersistence
                defaultConfig
                (\_ action -> Right <$> action)
                persistence
                (\_ _ -> pure (Right testOutput))
            )
            closeSupervisor
            \supervisor -> do
                submitted <-
                    submitTurn supervisor (turnSpec "session-a")
                        >>= expectRight
                takeWithin persisted
                completed <-
                    timeout
                        (2 * 1000 * 1000)
                        (awaitCompleted supervisor submitted.turnRecordId)
                completed `shouldSatisfy` (/= Nothing)
                readIORef attempts `shouldReturn` 3

    it "keeps queued turns from blocking work in another session" do
        firstStarted <- newEmptyMVar
        releaseFirst <- newEmptyMVar
        secondStarted <- newEmptyMVar
        let runner _ spec
                | spec.turnSpecSessionId == "session-a" = do
                    void (tryPutMVar firstStarted ())
                    takeMVar releaseFirst
                    pure (Right testOutput)
                | otherwise = do
                    void (tryPutMVar secondStarted ())
                    pure (Right testOutput)
        withSupervisorConfig
            defaultConfig
                { supervisorMaxConcurrentTurns = 2
                , supervisorMaxConcurrentTurnsPerTenant = 2
                }
            runner
            \supervisor -> do
                void (submitTurn supervisor (turnSpec "session-a"))
                takeWithin firstStarted
                void (submitTurn supervisor (turnSpec "session-b"))
                takeWithin secondStarted
                putMVar releaseFirst ()

    it "waits for and resolves structured human input" do
        resolved <- newEmptyMVar
        let runner control _ = do
                answer <-
                    control.turnControlRequestInput
                        HumanRequestSpec
                            { humanRequestSpecKind = PlanExitRequest
                            , humanRequestSpecPrompt = "Approve?"
                            , humanRequestSpecOptions =
                                ["approve", "request_changes", "cancel"]
                            }
                putMVar resolved answer
                pure (Right testOutput)
        withSupervisor runner \supervisor -> do
            void (submitTurn supervisor (turnSpec "session-a"))
            request <- awaitRequest supervisor
            resolveHumanRequest
                supervisor
                localAccessBoundary
                request.humanRequestId
                HumanResponse
                    { humanResponseDecision = "request_changes"
                    , humanResponseValue = Just "add tests"
                    }
                `shouldReturn` Right request
            takeWithin resolved
                `shouldReturn` Right
                    HumanResponse
                        { humanResponseDecision = "request_changes"
                        , humanResponseValue = Just "add tests"
                        }

    it "rejects human input too large for the public transport" do
        result <- newEmptyMVar
        let runner control _ = do
                answer <-
                    control.turnControlRequestInput
                        HumanRequestSpec
                            { humanRequestSpecKind = PlanQuestionRequest
                            , humanRequestSpecPrompt = "Choose"
                            , humanRequestSpecOptions =
                                replicate
                                    100
                                    (Text.replicate (16 * 1024) "x")
                            }
                putMVar result answer
                pure (Right testOutput)
        withSupervisor runner \supervisor -> do
            void (submitTurn supervisor (turnSpec "session-a"))
            takeWithin result
                `shouldReturn` Left "human request exceeds the public size limit"
            listHumanRequests supervisor localAccessBoundary Nothing
                `shouldReturn` Right []

    it "bounds merged human request pages without truncating a turn lookup" do
        now <- getCurrentTime
        let durableTurnId =
                TurnId "01999999-3333-7333-8333-333333333333"
            durableRequests =
                [ HumanRequest
                    { humanRequestId =
                        RequestId
                            ( "01999999-4444-7444-8444-"
                                <> Text.justifyRight
                                    12
                                    '0'
                                    (Text.pack (show index))
                            )
                    , humanRequestTurnId = durableTurnId
                    , humanRequestSessionId = "durable-session"
                    , humanRequestBoundary = localAccessBoundary
                    , humanRequestKind = ToolApprovalRequest
                    , humanRequestPrompt = "Approve durable request?"
                    , humanRequestOptions = ["approve", "cancel"]
                    , humanRequestCreatedAt = addUTCTime (-3600) now
                    }
                | index <- [1 .. 201 :: Int]
                ]
            persistence =
                inMemoryTurnPersistence
                    { turnPersistenceListHumanRequests =
                        \boundary turnId ->
                            pure . Right $
                                [ request
                                | request <- durableRequests
                                , request.humanRequestBoundary == boundary
                                , maybe
                                    True
                                    (== request.humanRequestTurnId)
                                    turnId
                                ]
                    }
            runner control _ = do
                void $
                    control.turnControlRequestInput
                        HumanRequestSpec
                            { humanRequestSpecKind = PlanQuestionRequest
                            , humanRequestSpecPrompt = "Choose"
                            , humanRequestSpecOptions = ["continue"]
                            }
                pure (Right testOutput)
        bracket
            ( newSupervisorWithBoundaryGuardAndPersistence
                defaultConfig
                (\_ action -> Right <$> action)
                persistence
                runner
            )
            closeSupervisor
            \supervisor -> do
                localTurn <-
                    submitTurn supervisor (turnSpec "session-a")
                        >>= expectRight
                localRequest <-
                    awaitRequestForTurn
                        supervisor
                        localTurn.turnRecordId
                globalRequests <-
                    listHumanRequests
                        supervisor
                        localAccessBoundary
                        Nothing
                        >>= expectRight
                length globalRequests `shouldBe` 200
                localRequest `elem` globalRequests `shouldBe` True
                exactRequests <-
                    listHumanRequests
                        supervisor
                        localAccessBoundary
                        (Just durableTurnId)
                        >>= expectRight
                exactRequests `shouldMatchList` durableRequests

    it "hands human input across supervisor instances" do
        sharedRequests <- newTVarIO Map.empty
        resolved <- newEmptyMVar
        let persistence =
                inMemoryTurnPersistence
                    { turnPersistenceCreateHumanRequest = \_ request -> do
                        atomically $
                            modifyTVar'
                                sharedRequests
                                (Map.insert request.humanRequestId (request, Nothing))
                        pure (Right ())
                    , turnPersistenceListHumanRequests = \boundary turnId -> do
                        requests <- atomically (readTVar sharedRequests)
                        pure . Right $
                            [ request
                            | (request, response) <- Map.elems requests
                            , request.humanRequestBoundary == boundary
                            , maybe
                                True
                                (== request.humanRequestTurnId)
                                turnId
                            , response == Nothing
                            ]
                    , turnPersistenceResolveHumanRequest =
                        \boundary requestId response ->
                            atomically do
                                requests <- readTVar sharedRequests
                                case Map.lookup requestId requests of
                                    Just (request, Nothing)
                                        | request.humanRequestBoundary
                                            == boundary -> do
                                            writeTVar
                                                sharedRequests
                                                ( Map.insert
                                                    requestId
                                                    (request, Just response)
                                                    requests
                                                )
                                            pure
                                                ( Right
                                                    ( HumanRequestResolvedDurably
                                                        request
                                                    )
                                                )
                                    _ ->
                                        pure
                                            (Right HumanRequestNotFoundDurably)
                    , turnPersistenceLoadHumanResponse = \_ requestId ->
                        atomically do
                            requests <- readTVar sharedRequests
                            pure . Right $
                                Map.lookup requestId requests
                                    >>= snd
                    , turnPersistenceDeleteHumanRequest =
                        \_ requestId disposition -> do
                            atomically $
                                modifyTVar'
                                    sharedRequests
                                    ( case disposition of
                                        HumanRequestAbandoned ->
                                            Map.update
                                                ( \entry@(_, response) ->
                                                    entry <$ response
                                                )
                                                requestId
                                        HumanResponseConsumed ->
                                            Map.delete requestId
                                    )
                            pure (Right ())
                    }
            ownerRunner control _ = do
                answer <-
                    control.turnControlRequestInput
                        HumanRequestSpec
                            { humanRequestSpecKind = ToolApprovalRequest
                            , humanRequestSpecPrompt = "Run the tool?"
                            , humanRequestSpecOptions = ["approve", "cancel"]
                            }
                putMVar resolved answer
                pure (Right testOutput)
            newWith runner =
                newSupervisorWithBoundaryGuardAndPersistence
                    defaultConfig
                    (\_ action -> Right <$> action)
                    persistence
                    runner
        bracket
            (newWith ownerRunner)
            closeSupervisor
            \owner ->
                bracket
                    (newWith (\_ _ -> pure (Right testOutput)))
                    closeSupervisor
                    \other -> do
                        void (submitTurn owner (turnSpec "session-a"))
                        request <- awaitRequest other
                        resolveHumanRequest
                            other
                            localAccessBoundary
                            request.humanRequestId
                            HumanResponse
                                { humanResponseDecision = "approve"
                                , humanResponseValue = Nothing
                                }
                            `shouldReturn` Right request
                        takeWithin resolved
                            `shouldReturn` Right
                                HumanResponse
                                    { humanResponseDecision = "approve"
                                    , humanResponseValue = Nothing
                                    }
                        atomically (readTVar sharedRequests)
                            `shouldReturn` Map.empty

    it "isolates replay buffers by exact access boundary and signals gaps" do
        withSupervisor (\_ _ -> pure (Right testOutput)) \supervisor -> do
            let tenantA = testBoundary tenantAId (Just "gateway")
                tenantB = testBoundary tenantBId (Just "gateway")
            publishEvent supervisor tenantA "a.one" Nothing Nothing (object [])
            publishEvent supervisor tenantB "b.one" Nothing Nothing (object [])
            publishEvent supervisor tenantA "a.two" Nothing Nothing (object [])
            publishEvent supervisor tenantA "a.three" Nothing Nothing (object [])
            subscription <-
                subscribeEvents supervisor tenantA (Just 0) >>= expectRight
            subscription.subscriptionResetRequired `shouldBe` True
            map (.serverEventType) subscription.subscriptionReplay
                `shouldBe` ["a.two", "a.three"]
            publishEvent supervisor tenantB "b.two" Nothing Nothing (object [])
            publishEvent supervisor tenantA "a.four" Nothing Nothing (object [])
            first <- atomically (readTBQueue subscription.subscriptionChannel)
            first.serverEventBoundary `shouldBe` tenantA
            first.serverEventType `shouldBe` "a.four"
            subscription.subscriptionClose

    it "bounds event subscribers globally and by tenant" do
        let config =
                defaultConfig
                    { supervisorMaxEventSubscribers = 2
                    , supervisorMaxEventSubscribersPerTenant = 1
                    }
            tenantA = testBoundary tenantAId (Just "gateway-a")
            tenantAOtherGateway =
                testBoundary tenantAId (Just "gateway-b")
            tenantB = testBoundary tenantBId Nothing
        withSupervisorConfig config (\_ _ -> pure (Right testOutput)) \supervisor -> do
            first <- subscribeEvents supervisor tenantA Nothing >>= expectRight
            subscribeEvents supervisor tenantAOtherGateway Nothing
                >>= expectLeft EventSubscriberTenantLimitReached
            second <- subscribeEvents supervisor tenantB Nothing >>= expectRight
            subscribeEvents supervisor localAccessBoundary Nothing
                >>= expectLeft EventSubscriberLimitReached
            first.subscriptionClose
            second.subscriptionClose

    it "does not run stale queued work across a boundary change" do
        releaseFirst <- newEmptyMVar
        staleRan <- newEmptyMVar
        let tenantA = testBoundary tenantAId (Just "gateway-a")
            tenantB = testBoundary tenantAId (Just "gateway-b")
        current <- newEmptyMVar
        putMVar current tenantA
        let guard expected action = do
                actual <- takeMVar current
                putMVar current actual
                if actual == expected
                    then Right <$> action
                    else pure (Left "boundary changed")
            runner _ spec
                | spec.turnSpecSessionId == "first" =
                    takeMVar releaseFirst >> pure (Right testOutput)
                | otherwise =
                    putMVar staleRan () >> pure (Right testOutput)
        bracket
            ( newSupervisorWithBoundaryGuard
                defaultConfig
                    { supervisorMaxConcurrentTurns = 1
                    }
                guard
                runner
            )
            closeSupervisor
            \supervisor -> do
                void
                    ( submitTurn
                        supervisor
                        (turnSpecFor tenantA "first")
                    )
                void
                    ( submitTurn
                        supervisor
                        (turnSpecFor tenantA "stale")
                    )
                _ <- takeMVar current
                putMVar current tenantB
                putMVar releaseFirst ()
                threadDelay 50000
                timeout 50000 (takeMVar staleRan)
                    `shouldReturn` Nothing

withSupervisor ::
    TurnRunner ->
    (Supervisor -> IO value) ->
    IO value
withSupervisor = withSupervisorConfig defaultConfig

withSupervisorConfig ::
    SupervisorConfig ->
    TurnRunner ->
    (Supervisor -> IO value) ->
    IO value
withSupervisorConfig config runner =
    bracket (newSupervisor config runner) closeSupervisor

defaultConfig :: SupervisorConfig
defaultConfig =
    SupervisorConfig
        { supervisorMaxConcurrentTurns = 1
        , supervisorMaxConcurrentTurnsPerTenant = 1
        , supervisorMaxQueuedTurns = 10
        , supervisorMaxQueuedTurnsPerTenant = 10
        , supervisorMaxEventSubscribers = 10
        , supervisorMaxEventSubscribersPerTenant = 5
        , supervisorEventReplayLimit = 2
        }

turnSpec :: Text -> TurnSpec
turnSpec = turnSpecFor localAccessBoundary

turnSpecFor :: AccessBoundary -> Text -> TurnSpec
turnSpecFor boundary sessionId =
    TurnSpec
        { turnSpecSessionId = sessionId
        , turnSpecClientRequestId =
            ClientRequestId "01999999-1111-7111-8111-111111111111"
        , turnSpecPrompt = "hello"
        , turnSpecImages = []
        , turnSpecFiles = []
        , turnSpecBoundary = boundary
        }

testOutput :: TurnExecutionOutput
testOutput =
    TurnExecutionOutput
        { turnExecutionResponseId = "response-test"
        , turnExecutionAssistantText = Just "done"
        , turnExecutionAssistantTextTruncated = False
        , turnExecutionCompletion = TurnCompletionComplete
        }

testTerminalRecord ::
    UTCTime ->
    TurnTerminalOutcome ->
    TurnRecord ->
    TurnRecord
testTerminalRecord finishedAt outcome record =
    record
        { turnRecordStatus = case outcome of
            TurnSucceeded _ -> TurnCompleted
            TurnErrored _ -> TurnFailed
            TurnWasCancelled -> TurnCancelled
        , turnRecordFinishedAt = Just finishedAt
        , turnRecordError = case outcome of
            TurnErrored err -> Just err
            _ -> Nothing
        }

awaitCompleted :: Supervisor -> TurnId -> IO TurnRecord
awaitCompleted supervisor turnId =
    awaitTurnStatus supervisor turnId TurnCompleted

awaitTurnStatus :: Supervisor -> TurnId -> TurnStatus -> IO TurnRecord
awaitTurnStatus supervisor turnId expected =
    lookupTurn supervisor localAccessBoundary turnId >>= \case
        Just record
            | record.turnRecordStatus == expected -> pure record
        _ ->
            threadDelay 10000
                >> awaitTurnStatus supervisor turnId expected

awaitRequest :: Supervisor -> IO HumanRequest
awaitRequest supervisor = awaitRequestMatching supervisor Nothing

awaitRequestForTurn :: Supervisor -> TurnId -> IO HumanRequest
awaitRequestForTurn supervisor turnId =
    awaitRequestMatching supervisor (Just turnId)

awaitRequestMatching :: Supervisor -> Maybe TurnId -> IO HumanRequest
awaitRequestMatching supervisor turnId = go (100 :: Int)
  where
    go attempts
        | attempts <= 0 =
            expectationFailure "timed out waiting for human request"
                >> fail "unreachable"
        | otherwise =
            listHumanRequests
                supervisor
                localAccessBoundary
                turnId
                >>= \case
                    Right (request : _) -> pure request
                    Right [] -> threadDelay 10000 >> go (attempts - 1)
                    Left err ->
                        expectationFailure
                            ("could not list human requests: " <> show err)
                            >> fail "unreachable"

takeWithin :: MVar value -> IO value
takeWithin value =
    timeout 1000000 (takeMVar value) >>= \case
        Nothing -> expectationFailure "timed out" >> fail "unreachable"
        Just result -> pure result

isRight :: Either left right -> Bool
isRight = \case
    Right _ -> True
    Left _ -> False

expectRight :: (Show left) => Either left right -> IO right
expectRight = \case
    Right value -> pure value
    Left err ->
        expectationFailure ("expected Right, got Left " <> show err)
            >> fail "unreachable"

expectLeft :: (Eq left, Show left) => left -> Either left right -> Expectation
expectLeft expected = \case
    Left actual -> actual `shouldBe` expected
    Right _ -> expectationFailure "expected Left, got Right"

localAccessBoundary :: AccessBoundary
localAccessBoundary =
    accessBoundary localPrincipal (GatewayBoundary Nothing)

testBoundary :: Text -> Maybe Text -> AccessBoundary
testBoundary rawTenant gatewayIdentity =
    AccessBoundary
        { accessTenantId =
            either (error . show) id (parseTenantId rawTenant)
        , accessGatewayBoundary = GatewayBoundary gatewayIdentity
        }

tenantAId :: Text
tenantAId = "018f6a14-7d52-7a52-9c00-66d5e7d70334"

tenantBId :: Text
tenantBId = "018f6a14-7d52-7a52-9c00-66d5e7d70335"
