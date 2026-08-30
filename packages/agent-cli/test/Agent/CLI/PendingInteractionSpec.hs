module Agent.CLI.PendingInteractionSpec (spec) where

import Agent.CLI.PendingInteraction
import Agent.Store.Postgres.Interaction
import Agent.Store.Types (StoreError(..))
import Agent.Tools.PlanMode (PlanDecision(..), PlanModeHooks(..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
    ( newEmptyMVar
    , putMVar
    , readMVar
    , tryReadMVar
    )
import Control.Exception.Safe (finally)
import Data.IORef
import Data.Text (Text)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..))
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.PendingInteraction" do
    it "publishes before opening local UI and resolves the local answer" do
        events <- newIORef ([] :: [Text])
        published <- newIORef Nothing
        let record event =
                atomicModifyIORef' events \current ->
                    (current <> [event], ())
            store = PendingInteractionStore
                { pendingInteractionPublish = \request -> do
                    record "publish"
                    writeIORef published (Just request)
                    pure
                        (Right
                            InteractionPublishObserved
                                { interactionPublishInserted = True
                                , interactionPublishValue =
                                    interactionFromRequest request Nothing
                                })
                , pendingInteractionLoad = \_ _ ->
                    threadDelay 1000000
                        >> error "poll should not beat immediate local answer"
                , pendingInteractionResolve = \request -> do
                    record "resolve"
                    pure
                        (Right
                            InteractionResolveObserved
                                { interactionResolveWon = True
                                , interactionResolveValue =
                                    resolutionFromRequest request
                                })
                }
            coordinator = testCoordinator store
            context = testContext "tool-call-1"
            local = do
                record "ui"
                pure (PendingInteractionRespond True)

        result <-
            coordinatePlanConfirmEnter
                coordinator
                context
                "Need to inspect architecture"
                local

        readIORef events `shouldReturn` ["publish", "ui", "resolve"]
        request <- readIORef published
        fmap (.interactionRequestKey) request
            `shouldBe` Just "tool-call-1:plan-enter"
        fmap (.interactionRequestKind) request
            `shouldBe` Just "plan_mode.confirm_enter"
        fmap (.interactionRequestPayload) request
            `shouldBe`
                Just
                    "{\"reason\":\"Need to inspect architecture\",\"type\":\"plan_mode.confirm_enter\"}"
        result `shouldSatisfy` \case
            Right PendingInteractionResolved
                { pendingInteractionAnswer = True
                , pendingInteractionWonLocally = True
                } -> True
            _ -> False

    it "cancels local UI when an external resolution wins the race" do
        localStarted <- newEmptyMVar
        localStopped <- newEmptyMVar
        let external = testResolution
                "{\"confirmed\":false,\"type\":\"plan_mode.confirm_enter\"}"
                "remote"
            store = PendingInteractionStore
                { pendingInteractionPublish = \request ->
                    pure
                        (Right
                            InteractionPublishObserved
                                { interactionPublishInserted = True
                                , interactionPublishValue =
                                    interactionFromRequest request Nothing
                                })
                , pendingInteractionLoad = \_ _ ->
                    readMVar localStarted
                        >> pure
                        (Right
                            (Just
                                (testInteraction
                                    (Just external))))
                , pendingInteractionResolve = \_ ->
                    error "external answer should avoid local resolve"
                }
            local =
                (putMVar localStarted ()
                    >> threadDelay 1000000
                    >> pure (PendingInteractionRespond True))
                    `finally` putMVar localStopped ()

        result <-
            coordinatePlanConfirmEnter
                (testCoordinator store)
                (testContext "race")
                "reason"
                local

        tryReadMVar localStarted `shouldReturn` Just ()
        tryReadMVar localStopped `shouldReturn` Just ()
        result `shouldSatisfy` \case
            Right PendingInteractionResolved
                { pendingInteractionAnswer = False
                , pendingInteractionResolution = resolution
                , pendingInteractionWonLocally = False
                } -> resolution.interactionResolutionResponder == "remote"
            _ -> False

    it "returns the observed external winner when local resolution loses" do
        let external = testResolution
                "{\"decision\":\"request_changes\",\"feedback\":\"Add tests\",\"type\":\"plan_mode.decide_exit\"}"
                "remote"
            store = PendingInteractionStore
                { pendingInteractionPublish = \request ->
                    pure
                        (Right
                            InteractionPublishObserved
                                { interactionPublishInserted = True
                                , interactionPublishValue =
                                    interactionFromRequest request Nothing
                                })
                , pendingInteractionLoad = \_ _ ->
                    threadDelay 1000000
                        >> error "local prompt should finish first"
                , pendingInteractionResolve = \_ ->
                    pure
                        (Right
                            InteractionResolveObserved
                                { interactionResolveWon = False
                                , interactionResolveValue = external
                                })
                }

        result <-
            coordinatePlanDecision
                (testCoordinator store)
                (testContext "decision")
                "# Plan"
                (pure (PendingInteractionRespond PlanApprove))

        result `shouldSatisfy` \case
            Right PendingInteractionResolved
                { pendingInteractionAnswer =
                    PlanRequestChanges "Add tests"
                , pendingInteractionWonLocally = False
                } -> True
            _ -> False

    it "resumes an already-resolved request without showing local UI" do
        uiShown <- newIORef False
        let external = testResolution
                "{\"answer\":\"PostgreSQL\",\"type\":\"plan_mode.ask_question\"}"
                "remote"
            store = PendingInteractionStore
                { pendingInteractionPublish = \request ->
                    pure
                        (Right
                            InteractionPublishObserved
                                { interactionPublishInserted = False
                                , interactionPublishValue =
                                    interactionFromRequest
                                        request
                                        (Just external)
                                })
                , pendingInteractionLoad = \_ _ ->
                    error "resolved publish should not poll"
                , pendingInteractionResolve = \_ ->
                    error "resolved publish should not resolve"
                }
            local = do
                writeIORef uiShown True
                pure (PendingInteractionRespond (Just "SQLite"))

        result <-
            coordinatePlanQuestion
                (testCoordinator store)
                (testContext "resume")
                "Which database?"
                ["PostgreSQL", "SQLite"]
                local

        readIORef uiShown `shouldReturn` False
        result `shouldSatisfy` \case
            Right PendingInteractionResolved
                { pendingInteractionAnswer = Just "PostgreSQL"
                , pendingInteractionWonLocally = False
                } -> True
            _ -> False

    it "leaves a published question open when local UI defers" do
        resolved <- newIORef False
        let store = PendingInteractionStore
                { pendingInteractionPublish = \request ->
                    pure
                        (Right
                            InteractionPublishObserved
                                { interactionPublishInserted = True
                                , interactionPublishValue =
                                    interactionFromRequest request Nothing
                                })
                , pendingInteractionLoad = \_ _ ->
                    threadDelay 1000000
                        >> error "immediate deferral should cancel polling"
                , pendingInteractionResolve = \_ ->
                    writeIORef resolved True
                        >> error "deferral must not resolve"
                }

        result <-
            coordinatePlanQuestion
                (testCoordinator store)
                (testContext "defer")
                "Need input"
                []
                (pure PendingInteractionDefer)

        readIORef resolved `shouldReturn` False
        result `shouldSatisfy` \case
            Right PendingInteractionDeferred
                { pendingInteractionOpenRequest = interaction } ->
                    interaction.sessionInteractionRequestKey
                        == "defer:plan-enter"
            _ -> False

    it "fails closed before local UI when publication fails" do
        uiShown <- newIORef False
        failures <- newIORef ([] :: [PendingInteractionError])
        requests <- newIORef []
        let store = PendingInteractionStore
                { pendingInteractionPublish = \_ ->
                    pure
                        (Left
                            (StoreConnectionError "database unavailable"))
                , pendingInteractionLoad = \_ _ ->
                    error "failed publication should not poll"
                , pendingInteractionResolve = \_ ->
                    error "failed publication should not resolve"
                }
            localHooks = PlanModeHooks
                { planConfirmEnter = \_ ->
                    writeIORef uiShown True >> pure True
                , planDecideExit = \_ -> pure PlanApprove
                , planAskQuestion = \_ _ -> pure (Just "answer")
                }
            wrapped = wrapDurablePlanModeHooks
                (testCoordinator store)
                (\request -> do
                    modifyIORef' requests (<> [request])
                    pure (testContext "failed"))
                (\err -> modifyIORef' failures (<> [err]))
                localHooks

        wrapped.planConfirmEnter "reason" `shouldReturn` False
        readIORef uiShown `shouldReturn` False
        readIORef requests `shouldReturn`
            [PlanModeConfirmEnterRequest "reason"]
        observedFailures <- readIORef failures
        observedFailures `shouldSatisfy` \case
            [PendingInteractionStoreError
                (StoreConnectionError "database unavailable")] -> True
            _ -> False

    it "fails closed on malformed externally supplied payloads" do
        failures <- newIORef ([] :: [PendingInteractionError])
        localShown <- newIORef False
        let malformed = testResolution
                "{\"confirmed\":\"yes\",\"type\":\"plan_mode.confirm_enter\"}"
                "remote"
            store = PendingInteractionStore
                { pendingInteractionPublish = \request ->
                    pure
                        (Right
                            InteractionPublishObserved
                                { interactionPublishInserted = False
                                , interactionPublishValue =
                                    interactionFromRequest
                                        request
                                        (Just malformed)
                                })
                , pendingInteractionLoad = \_ _ ->
                    error "already resolved"
                , pendingInteractionResolve = \_ ->
                    error "already resolved"
                }
            hooks = wrapDurablePlanModeHooks
                (testCoordinator store)
                (const (pure (testContext "malformed")))
                (\err -> modifyIORef' failures (<> [err]))
                PlanModeHooks
                    { planConfirmEnter = \_ ->
                        writeIORef localShown True >> pure True
                    , planDecideExit = \_ -> pure PlanApprove
                    , planAskQuestion = \_ _ -> pure Nothing
                    }

        hooks.planConfirmEnter "reason" `shouldReturn` False
        readIORef localShown `shouldReturn` False
        observedFailures <- readIORef failures
        observedFailures `shouldSatisfy` \case
            [PendingInteractionInvalidResolution _] -> True
            _ -> False

    describe "canonicalizeExternalInteractionResponse" do
        it "normalizes human review responses before first-wins storage" do
            let interaction =
                    (testInteraction Nothing)
                        { sessionInteractionKind = "plan_mode.decide_exit"
                        , sessionInteractionPayload =
                            "{\"type\":\"plan_mode.decide_exit\",\"plan_markdown\":\"# Plan\"}"
                        }
            canonicalizeExternalInteractionResponse
                interaction
                "revise: add a restart test"
                `shouldBe`
                    Right
                        (ExternalInteractionResolve
                            "{\"decision\":\"request_changes\",\"feedback\":\"add a restart test\",\"type\":\"plan_mode.decide_exit\"}")

        it "keeps defer open and rejects malformed answers" do
            canonicalizeExternalInteractionResponse
                (testInteraction Nothing)
                "defer"
                `shouldBe` Right ExternalInteractionDefer
            canonicalizeExternalInteractionResponse
                (testInteraction Nothing)
                "perhaps"
                `shouldSatisfy` \case
                    Left _ -> True
                    Right _ -> False

testCoordinator :: PendingInteractionStore -> PendingInteractionCoordinator
testCoordinator store =
    mkPendingInteractionCoordinator
        store
        testSessionKey
        "local"
        1000
        (pure fixedTime)

testContext :: Text -> PendingInteractionContext
testContext prefix = PendingInteractionContext
    { pendingInteractionRequestKey = prefix <> ":plan-enter"
    , pendingInteractionOrigin =
        Just
            InteractionOrigin
                { interactionOriginToolName = "enter_plan_mode"
                , interactionOriginCallId = prefix
                }
    }

interactionFromRequest
    :: InteractionRequest
    -> Maybe InteractionResolution
    -> SessionInteraction
interactionFromRequest request resolution = SessionInteraction
    { sessionInteractionId = testInteractionId
    , sessionInteractionSessionKey =
        request.interactionRequestSessionKey
    , sessionInteractionRequestKey =
        request.interactionRequestKey
    , sessionInteractionKind =
        request.interactionRequestKind
    , sessionInteractionPayloadVersion =
        request.interactionRequestPayloadVersion
    , sessionInteractionPayload =
        request.interactionRequestPayload
    , sessionInteractionOrigin =
        request.interactionRequestOrigin
    , sessionInteractionCreatedAt =
        request.interactionRequestCreatedAt
    , sessionInteractionResolution = resolution
    , sessionInteractionDelivery = Nothing
    }

testInteraction
    :: Maybe InteractionResolution
    -> SessionInteraction
testInteraction resolution = SessionInteraction
    { sessionInteractionId = testInteractionId
    , sessionInteractionSessionKey = testSessionKey
    , sessionInteractionRequestKey = "race:plan-enter"
    , sessionInteractionKind = "plan_mode.confirm_enter"
    , sessionInteractionPayloadVersion = 1
    , sessionInteractionPayload =
        "{\"reason\":\"reason\",\"type\":\"plan_mode.confirm_enter\"}"
    , sessionInteractionOrigin =
        (testContext "race").pendingInteractionOrigin
    , sessionInteractionCreatedAt = fixedTime
    , sessionInteractionResolution = resolution
    , sessionInteractionDelivery = Nothing
    }

resolutionFromRequest
    :: InteractionResolutionRequest
    -> InteractionResolution
resolutionFromRequest request = InteractionResolution
    { interactionResolutionInteractionId =
        request.interactionResolutionRequestInteractionId
    , interactionResolutionPayloadVersion =
        request.interactionResolutionRequestPayloadVersion
    , interactionResolutionPayload =
        request.interactionResolutionRequestPayload
    , interactionResolutionResponder =
        request.interactionResolutionRequestResponder
    , interactionResolutionResolvedAt =
        request.interactionResolutionRequestResolvedAt
    }

testResolution :: Text -> Text -> InteractionResolution
testResolution payload responder = InteractionResolution
    { interactionResolutionInteractionId = testInteractionId
    , interactionResolutionPayloadVersion = 1
    , interactionResolutionPayload = payload
    , interactionResolutionResponder = responder
    , interactionResolutionResolvedAt = fixedTime
    }

testSessionKey :: Text
testSessionKey = "018f44e2-b728-7a44-9c85-7e77ea9bc957"

testInteractionId :: Text
testInteractionId = "018f44e2-b728-7a44-9c85-7e77ea9bc958"

fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2026 8 29) 0
