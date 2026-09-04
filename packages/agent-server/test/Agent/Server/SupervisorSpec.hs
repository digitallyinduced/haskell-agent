module Agent.Server.SupervisorSpec (spec) where

import Agent.Server.Supervisor
import Agent.Server.Types
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
    ( MVar
    , newEmptyMVar
    , putMVar
    , takeMVar
    , tryPutMVar
    )
import Control.Concurrent.STM
    ( atomically
    , readTBQueue
    )
import Control.Concurrent.Async
    ( async
    , wait
    )
import Control.Exception.Safe (bracket, finally)
import Control.Monad (void)
import Data.Aeson (object)
import Data.Text (Text)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "turn supervisor" do
    it "enforces one active turn per session" do
        release <- newEmptyMVar
        let runner _ _ = takeMVar release >> pure (Right ())
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
                pure (Right ())
        withSupervisorConfig
            defaultConfig
                { supervisorMaxConcurrentTurns = 2
                , supervisorMaxConcurrentTurnsPerTenant = 2
                }
            runner \supervisor -> do
                first <-
                    submitTurn supervisor
                        (turnSpecFor tenantA "same-session")
                first `shouldSatisfy` isRight
                second <-
                    submitTurn supervisor
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
        withSupervisor (\_ _ -> pure (Right ())) \supervisor -> do
            mutation <- async $
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

    it "holds the session reservation across checked turn validation" do
        entered <- newEmptyMVar
        release <- newEmptyMVar
        withSupervisor (\_ _ -> pure (Right ())) \supervisor -> do
            admission <- async $
                submitTurnChecked
                    supervisor
                    (turnSpec "session-a")
                    (putMVar entered ()
                        >> takeMVar release
                        >> pure (Right () :: Either Text ()))
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
                pure (Right ())
        withSupervisor runner \supervisor -> do
            submitted <- submitTurn supervisor (turnSpec "session-a")
            turn <- case submitted of
                Left err ->
                    expectationFailure ("turn rejected: " <> show err)
                        >> fail "unreachable"
                Right accepted -> pure accepted
            takeWithin started
            cancelled <- timeout
                (2 * 1000 * 1000)
                (cancelTurn
                    supervisor
                    localAccessBoundary
                    turn.turnRecordId)
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
                    `finally`
                        (putMVar cancelled () >> takeMVar release)
        withSupervisorConfig
            defaultConfig
                { supervisorMaxConcurrentTurns = 2
                , supervisorMaxConcurrentTurnsPerTenant = 2
                }
            runner \supervisor -> do
                void (submitTurn supervisor (turnSpec "session-a"))
                void (submitTurn supervisor (turnSpec "session-b"))
                takeWithin firstStarted
                takeWithin secondStarted

                shutdown <- async (closeSupervisor supervisor)
                cancelledTogether <-
                    timeout 500_000 do
                        takeMVar firstCancelled
                        takeMVar secondCancelled
                putMVar releaseFirst ()
                putMVar releaseSecond ()
                wait shutdown

                cancelledTogether `shouldBe` Just ()

    it "keeps queued turns from blocking work in another session" do
        firstStarted <- newEmptyMVar
        releaseFirst <- newEmptyMVar
        secondStarted <- newEmptyMVar
        let runner _ spec
                | spec.turnSpecSessionId == "session-a" = do
                    void (tryPutMVar firstStarted ())
                    takeMVar releaseFirst
                    pure (Right ())
                | otherwise = do
                    void (tryPutMVar secondStarted ())
                    pure (Right ())
        withSupervisorConfig
            defaultConfig
                { supervisorMaxConcurrentTurns = 2
                , supervisorMaxConcurrentTurnsPerTenant = 2
                }
            runner \supervisor -> do
                void (submitTurn supervisor (turnSpec "session-a"))
                takeWithin firstStarted
                void (submitTurn supervisor (turnSpec "session-b"))
                takeWithin secondStarted
                putMVar releaseFirst ()

    it "waits for and resolves structured human input" do
        resolved <- newEmptyMVar
        let runner control _ = do
                answer <- control.turnControlRequestInput HumanRequestSpec
                    { humanRequestSpecKind = PlanExitRequest
                    , humanRequestSpecPrompt = "Approve?"
                    , humanRequestSpecOptions =
                        ["approve", "request_changes", "cancel"]
                    }
                putMVar resolved answer
                pure (Right ())
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
                `shouldReturn`
                    Right HumanResponse
                        { humanResponseDecision = "request_changes"
                        , humanResponseValue = Just "add tests"
                        }

    it "isolates replay buffers by exact access boundary and signals gaps" do
        withSupervisor (\_ _ -> pure (Right ())) \supervisor -> do
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
        let config = defaultConfig
                { supervisorMaxEventSubscribers = 2
                , supervisorMaxEventSubscribersPerTenant = 1
                }
            tenantA = testBoundary tenantAId (Just "gateway-a")
            tenantAOtherGateway =
                testBoundary tenantAId (Just "gateway-b")
            tenantB = testBoundary tenantBId Nothing
        withSupervisorConfig config (\_ _ -> pure (Right ())) \supervisor -> do
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
                    takeMVar releaseFirst >> pure (Right ())
                | otherwise =
                    putMVar staleRan () >> pure (Right ())
        bracket
            (newSupervisorWithBoundaryGuard
                defaultConfig
                    { supervisorMaxConcurrentTurns = 1 }
                guard
                runner)
            closeSupervisor \supervisor -> do
                void (submitTurn supervisor
                    (turnSpecFor tenantA "first"))
                void (submitTurn supervisor
                    (turnSpecFor tenantA "stale"))
                _ <- takeMVar current
                putMVar current tenantB
                putMVar releaseFirst ()
                threadDelay 50000
                timeout 50000 (takeMVar staleRan)
                    `shouldReturn` Nothing

withSupervisor
    :: TurnRunner
    -> (Supervisor -> IO value)
    -> IO value
withSupervisor = withSupervisorConfig defaultConfig

withSupervisorConfig
    :: SupervisorConfig
    -> TurnRunner
    -> (Supervisor -> IO value)
    -> IO value
withSupervisorConfig config runner =
    bracket (newSupervisor config runner) closeSupervisor

defaultConfig :: SupervisorConfig
defaultConfig = SupervisorConfig
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
turnSpecFor boundary sessionId = TurnSpec
    { turnSpecSessionId = sessionId
    , turnSpecPrompt = "hello"
    , turnSpecBoundary = boundary
    }

awaitRequest :: Supervisor -> IO HumanRequest
awaitRequest supervisor = go (100 :: Int)
  where
    go attempts
        | attempts <= 0 =
            expectationFailure "timed out waiting for human request"
                >> fail "unreachable"
        | otherwise =
            listHumanRequests
                supervisor
                localAccessBoundary >>= \case
                    request : _ -> pure request
                    [] -> threadDelay 10000 >> go (attempts - 1)

takeWithin :: MVar value -> IO value
takeWithin value =
    timeout 1000000 (takeMVar value) >>= \case
        Nothing -> expectationFailure "timed out" >> fail "unreachable"
        Just result -> pure result

isRight :: Either left right -> Bool
isRight = \case
    Right _ -> True
    Left _ -> False

expectRight :: Show left => Either left right -> IO right
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
