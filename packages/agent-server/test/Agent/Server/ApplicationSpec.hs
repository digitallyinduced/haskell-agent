module Agent.Server.ApplicationSpec (spec) where

import Agent.Server.Application (
    ApplicationConfig (..),
    newApplication,
 )
import Agent.Server.Auth
import Agent.Server.Backend (Backend (..), SessionMutationLease (..))
import Agent.Server.Supervisor
import Agent.Server.Types
import Control.Concurrent.Async (cancel, wait, waitCatch, withAsync)
import Control.Concurrent.MVar (
    MVar,
    modifyMVar,
    modifyMVar_,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    takeMVar,
    tryPutMVar,
 )
import Control.Exception.Safe (bracket, finally)
import Control.Monad (void)
import Data.Aeson (Value (..), eitherDecode, object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (find)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Network.HTTP.Types (
    Header,
    Method,
    methodGet,
    methodPatch,
    methodPost,
    status200,
    status202,
    status400,
    status403,
    status404,
    status409,
    status413,
    status415,
    status422,
    status503,
 )
import Network.Wai (
    Application,
    defaultRequest,
    pathInfo,
    queryString,
    requestHeaders,
    requestMethod,
 )
import Network.Wai.Test (
    SRequest (..),
    SResponse (..),
    runSession,
    srequest,
 )
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "agent-server WAI application" do
    it "rejects requests without the strict loopback Host" do
        withApplication immediateRunner \application -> do
            response <-
                perform
                    application
                    methodGet
                    ["healthz"]
                    []
                    ""
            response.simpleStatus `shouldBe` status403
            LBS8.unpack response.simpleBody
                `shouldContain` "\"requestId\":\""

    it "keeps allowed CORS headers on authentication failures" do
        let auth =
                AuthConfig
                    { authMode =
                        LoopbackHostAuth
                            (Set.fromList ["127.0.0.1:4096"])
                    , authCorsOrigins =
                        Set.fromList ["https://client.example"]
                    }
        withApplicationAuth auth immediateRunner \application -> do
            response <-
                perform
                    application
                    methodGet
                    ["healthz"]
                    [ ("Host", "attacker.example")
                    , ("Origin", "https://client.example")
                    ]
                    ""
            response.simpleStatus `shouldBe` status403
            lookup
                "Access-Control-Allow-Origin"
                response.simpleHeaders
                `shouldBe` Just "https://client.example"

    it "serves health and attaches a request id" do
        withApplication immediateRunner \application -> do
            response <-
                perform
                    application
                    methodGet
                    ["healthz"]
                    validHeaders
                    ""
            response.simpleStatus `shouldBe` status200
            lookup "X-Request-ID" response.simpleHeaders
                `shouldSatisfy` (/= Nothing)

    it "rejects an oversized body before JSON decoding" do
        withApplication immediateRunner \application -> do
            response <-
                perform
                    application
                    methodPost
                    ["v1", "sessions"]
                    validHeaders
                    (LBS8.replicate 257 'x')
            response.simpleStatus `shouldBe` status413

    it "rejects media types that only prefix-match application/json" do
        withApplication immediateRunner \application -> do
            response <-
                perform
                    application
                    methodPost
                    ["v1", "sessions"]
                    [ ("Host", "127.0.0.1:4096")
                    , ("Content-Type", "application/json-patch")
                    ]
                    "{}"
            response.simpleStatus `shouldBe` status415

    it "rejects request fields outside the published schema" do
        withApplication immediateRunner \application -> do
            response <-
                perform
                    application
                    methodPost
                    ["v1", "sessions"]
                    validHeaders
                    "{\"unexpected\":true}"
            response.simpleStatus `shouldBe` status400

    it "filters pending human requests by turn id" do
        observed <- newEmptyMVar
        let expectedTurnId =
                TurnId "01999999-1111-7111-8111-111111111120"
            backend =
                fakeBackend
                    { backendTurnPersistence =
                        inMemoryTurnPersistence
                            { turnPersistenceListHumanRequests =
                                \_ turnId -> do
                                    putMVar observed turnId
                                    pure (Right [])
                            }
                    }
        withBackendApplication backend immediateRunner \application -> do
            response <-
                runSession
                    ( srequest
                        ( SRequest
                            defaultRequest
                                { requestMethod = methodGet
                                , pathInfo = ["v1", "requests"]
                                , queryString =
                                    [
                                        ( "turnId"
                                        , Just
                                            "01999999-1111-7111-8111-111111111120"
                                        )
                                    ]
                                , requestHeaders = validHeaders
                                }
                            ""
                        )
                    )
                    application
            response.simpleStatus `shouldBe` status200
            takeMVar observed `shouldReturn` Just expectedTurnId

    it "returns 503 when durable human request resolution loses the store" do
        let requestId = "01999999-1111-7111-8111-111111111121"
            backend =
                fakeBackend
                    { backendTurnPersistence =
                        inMemoryTurnPersistence
                            { turnPersistenceResolveHumanRequest =
                                \_ _ _ ->
                                    pure (Left "PostgreSQL is unavailable")
                            }
                    }
        withBackendApplication backend immediateRunner \application -> do
            response <-
                perform
                    application
                    methodPost
                    ["v1", "requests", requestId, "resolve"]
                    validHeaders
                    "{\"decision\":\"approve\"}"
            response.simpleStatus `shouldBe` status503
            LBS8.unpack response.simpleBody
                `shouldContain` "\"code\":\"store_unavailable\""

    it "preserves durable human request conflicts as 409" do
        let requestId = "01999999-1111-7111-8111-111111111124"
            cases =
                [ ( HumanRequestAlreadyResolvedDurably
                  , "request has already been resolved"
                  )
                , ( HumanRequestInvalidDecisionDurably
                  , "decision is not one of the allowed options"
                  )
                ]
            assertConflict (resolution, expectedMessage) = do
                let backend =
                        fakeBackend
                            { backendTurnPersistence =
                                inMemoryTurnPersistence
                                    { turnPersistenceResolveHumanRequest =
                                        \_ _ _ -> pure (Right resolution)
                                    }
                            }
                withBackendApplication backend immediateRunner \application -> do
                    response <-
                        perform
                            application
                            methodPost
                            ["v1", "requests", requestId, "resolve"]
                            validHeaders
                            "{\"decision\":\"approve\"}"
                    response.simpleStatus `shouldBe` status409
                    LBS8.unpack response.simpleBody
                        `shouldContain` "\"code\":\"request_not_resolved\""
                    LBS8.unpack response.simpleBody
                        `shouldContain` expectedMessage
        mapM_ assertConflict cases

    it "bounds merged turn pages after adding local history" do
        createdAt <- addUTCTime (-60) <$> getCurrentTime
        let durableTurn :: AccessBoundary -> Int -> TurnRecord
            durableTurn boundary index =
                TurnRecord
                    { turnRecordId =
                        TurnId
                            ( "01999999-1111-7111-8111-"
                                <> Text.justifyRight
                                    12
                                    '0'
                                    (Text.pack (show index))
                            )
                    , turnRecordSessionId = "session-a"
                    , turnRecordClientRequestId =
                        ClientRequestId
                            "01999999-1111-7111-8111-222222222222"
                    , turnRecordBoundary = boundary
                    , turnRecordStatus = TurnCompleted
                    , turnRecordCreatedAt = createdAt
                    , turnRecordStartedAt = Just createdAt
                    , turnRecordFinishedAt = Just createdAt
                    , turnRecordError = Nothing
                    }
            backend =
                fakeBackend
                    { backendListTurns = \boundary _ ->
                        pure . Right $
                            map (durableTurn boundary) [1 .. 200]
                    }
        withBackendApplication backend immediateRunner \application -> do
            created <-
                perform
                    application
                    methodPost
                    ["v1", "sessions", "session-a", "turns"]
                    validHeaders
                    "{\"clientRequestId\":\"01999999-1111-7111-8111-333333333333\",\"input\":\"hello\"}"
            created.simpleStatus `shouldBe` status202
            localTurnId <- responseTurnId created

            listed <-
                perform
                    application
                    methodGet
                    ["v1", "turns"]
                    validHeaders
                    ""
            listed.simpleStatus `shouldBe` status200
            turnIds <- responseTurnIds listed
            length turnIds `shouldBe` 200
            turnIds `shouldSatisfy` elem localTurnId

    it "distinguishes a remote turn from an unavailable agent snapshot" do
        createdAt <- getCurrentTime
        let turnId = TurnId "01999999-1111-7111-8111-111111111122"
            remoteTurn boundary =
                TurnRecord
                    { turnRecordId = turnId
                    , turnRecordSessionId = "session-a"
                    , turnRecordClientRequestId =
                        ClientRequestId
                            "01999999-1111-7111-8111-111111111123"
                    , turnRecordBoundary = boundary
                    , turnRecordStatus = TurnRunning
                    , turnRecordCreatedAt = createdAt
                    , turnRecordStartedAt = Just createdAt
                    , turnRecordFinishedAt = Nothing
                    , turnRecordError = Nothing
                    }
            backend =
                fakeBackend
                    { backendLookupTurn = \boundary requestedTurnId ->
                        pure . Right $
                            if requestedTurnId == turnId
                                then Just (remoteTurn boundary)
                                else Nothing
                    }
        withBackendApplication backend immediateRunner \application -> do
            response <-
                perform
                    application
                    methodGet
                    ["v1", "turns", turnId.unTurnId, "agents"]
                    validHeaders
                    ""
            response.simpleStatus `shouldBe` status409
            LBS8.unpack response.simpleBody
                `shouldContain` "\"code\":\"turn_agents_unavailable\""

    it "re-admits an owned queued reservation missing from memory" do
        createdAt <- getCurrentTime
        ran <- newEmptyMVar
        let clientRequestId =
                ClientRequestId
                    "01999999-1111-7111-8111-111111111115"
            reserved boundary =
                TurnRecord
                    { turnRecordId =
                        TurnId "01999999-1111-7111-8111-111111111116"
                    , turnRecordSessionId = "session-a"
                    , turnRecordClientRequestId = clientRequestId
                    , turnRecordBoundary = boundary
                    , turnRecordStatus = TurnQueued
                    , turnRecordCreatedAt = createdAt
                    , turnRecordStartedAt = Nothing
                    , turnRecordFinishedAt = Nothing
                    , turnRecordError = Nothing
                    }
            backend =
                fakeBackend
                    { backendReserveTurn =
                        \boundary _ _ _ _ _ ->
                            pure
                                ( Right
                                    ( TurnReservationExistingOwned
                                        (reserved boundary)
                                    )
                                )
                    }
            runner _ _ =
                putMVar ran () >> pure (Right successfulOutput)
        withBackendApplication backend runner \application -> do
            response <-
                perform
                    application
                    methodPost
                    ["v1", "sessions", "session-a", "turns"]
                    validHeaders
                    "{\"clientRequestId\":\"01999999-1111-7111-8111-111111111115\",\"input\":\"hello\"}"
            response.simpleStatus `shouldBe` status202
            takeMVar ran

    it "does not fail an owned reservation when an identical retry loses admission" do
        ledger <- newMVar []
        terminal <- newEmptyMVar
        checkedValidationStarted <- newEmptyMVar
        releaseCheckedValidation <- newEmptyMVar
        validationCalls <- newIORef (0 :: Int)
        let backend =
                (durableBackend ledger terminal immediateRunner)
                    { backendGetSession = \_ _ -> do
                        call <-
                            atomicModifyIORef' validationCalls \count ->
                                let next = count + 1
                                 in (next, next)
                        if call == 2
                            then do
                                putMVar checkedValidationStarted ()
                                takeMVar releaseCheckedValidation
                            else pure ()
                        pure
                            ( Right
                                (object ["id" .= ("session-a" :: String)])
                            )
                    }
            body =
                "{\"clientRequestId\":\"01999999-1111-7111-8111-111111111117\",\"input\":\"hello\"}"
            create application =
                perform
                    application
                    methodPost
                    ["v1", "sessions", "session-a", "turns"]
                    validHeaders
                    body
        withBackendApplication backend immediateRunner \application ->
            withAsync (create application) \original -> do
                takeMVar checkedValidationStarted

                duplicate <- create application
                duplicate.simpleStatus `shouldBe` status202
                LBS8.unpack duplicate.simpleBody
                    `shouldContain` "\"status\":\"queued\""

                putMVar releaseCheckedValidation ()
                originalResponse <- wait original
                originalResponse.simpleStatus `shouldBe` status202

    it "terminalizes a created reservation when admission is interrupted" do
        ledger <- newMVar []
        terminal <- newEmptyMVar
        checkedValidationStarted <- newEmptyMVar
        never <- newEmptyMVar
        validationCalls <- newIORef (0 :: Int)
        let backend =
                (durableBackend ledger terminal immediateRunner)
                    { backendGetSession = \_ _ -> do
                        call <-
                            atomicModifyIORef' validationCalls \count ->
                                let next = count + 1
                                 in (next, next)
                        if call == 2
                            then
                                putMVar checkedValidationStarted ()
                                    >> takeMVar never
                            else pure ()
                        pure
                            ( Right
                                (object ["id" .= ("session-a" :: String)])
                            )
                    }
            body =
                "{\"clientRequestId\":\"01999999-1111-7111-8111-111111111118\",\"input\":\"hello\"}"
            create application =
                perform
                    application
                    methodPost
                    ["v1", "sessions", "session-a", "turns"]
                    validHeaders
                    body
        withBackendApplication backend immediateRunner \application ->
            withAsync (create application) \request -> do
                takeMVar checkedValidationStarted
                cancel request
                void (waitCatch request)
                timeout (2 * 1000 * 1000) (takeMVar terminal)
                    `shouldReturn` Just ()

                stored <- readMVar ledger
                case stored of
                    [entry] ->
                        entry.testStoredRecord.turnRecordStatus
                            `shouldBe` TurnCancelled
                    entries ->
                        expectationFailure
                            ( "expected one durable turn, got "
                                <> show (length entries)
                            )

                retry <- create application
                retry.simpleStatus `shouldBe` status202
                LBS8.unpack retry.simpleBody
                    `shouldContain` "\"status\":\"cancelled\""

    it "fences interrupted reservation cleanup against an identical retry" do
        ledger <- newMVar []
        terminal <- newEmptyMVar
        checkedValidationStarted <- newEmptyMVar
        neverValidation <- newEmptyMVar
        cleanupStarted <- newEmptyMVar
        releaseCleanup <- newEmptyMVar
        ran <- newEmptyMVar
        neverRunner <- newEmptyMVar
        validationCalls <- newIORef (0 :: Int)
        let runner _ _ =
                putMVar ran ()
                    >> takeMVar neverRunner
                    >> pure (Right successfulOutput)
            baseBackend = durableBackend ledger terminal runner
            basePersistence = baseBackend.backendTurnPersistence
            backend =
                baseBackend
                    { backendGetSession = \_ _ -> do
                        call <-
                            atomicModifyIORef' validationCalls \count ->
                                let next = count + 1
                                 in (next, next)
                        if call == 2
                            then
                                putMVar checkedValidationStarted ()
                                    >> takeMVar neverValidation
                            else pure ()
                        pure
                            ( Right
                                (object ["id" .= ("session-a" :: String)])
                            )
                    , backendTurnPersistence =
                        basePersistence
                            { turnPersistenceTerminal =
                                \record finishedAt outcome ->
                                    case outcome of
                                        TurnWasCancelled -> do
                                            void (tryPutMVar cleanupStarted ())
                                            takeMVar releaseCleanup
                                            basePersistence.turnPersistenceTerminal
                                                record
                                                finishedAt
                                                outcome
                                        _ ->
                                            basePersistence.turnPersistenceTerminal
                                                record
                                                finishedAt
                                                outcome
                            }
                    }
            body =
                "{\"clientRequestId\":\"01999999-1111-7111-8111-111111111121\",\"input\":\"hello\"}"
            create application =
                perform
                    application
                    methodPost
                    ["v1", "sessions", "session-a", "turns"]
                    validHeaders
                    body
        withBackendApplication backend runner \application ->
            withAsync (create application) \original -> do
                takeMVar checkedValidationStarted
                withAsync (cancel original) \canceller -> do
                    takeMVar cleanupStarted

                    duplicate <- create application
                    duplicate.simpleStatus `shouldBe` status202
                    LBS8.unpack duplicate.simpleBody
                        `shouldContain` "\"status\":\"queued\""
                    timeout (250 * 1000) (takeMVar ran)
                        `shouldReturn` Nothing

                    putMVar releaseCleanup ()
                    wait canceller
                void (waitCatch original)

                stored <- readMVar ledger
                case stored of
                    [entry] ->
                        entry.testStoredRecord.turnRecordStatus
                            `shouldBe` TurnCancelled
                    entries ->
                        expectationFailure
                            ( "expected one durable turn, got "
                                <> show (length entries)
                            )

    it "rejects malformed turn identifiers before storage lookup" do
        withApplication immediateRunner \application -> do
            response <-
                perform
                    application
                    methodGet
                    ["v1", "turns", "not-a-uuid"]
                    validHeaders
                    ""
            response.simpleStatus `shouldBe` status404

    it "keeps legacy turn creation working without an idempotency key" do
        withApplication immediateRunner \application -> do
            response <-
                perform
                    application
                    methodPost
                    ["v1", "sessions", "session-a", "turns"]
                    validHeaders
                    "{\"input\":\"hello\"}"
            response.simpleStatus `shouldBe` status202
            LBS8.unpack response.simpleBody
                `shouldContain` "\"clientRequestId\":"

    it "returns 404 when a session disappears before turn reservation" do
        ran <- newEmptyMVar
        let backend =
                fakeBackend
                    { backendReserveTurn =
                        \_ _ _ _ _ _ ->
                            pure . Left $
                                ApiError
                                    { apiErrorStatus = 404
                                    , apiErrorCode = "not_found"
                                    , apiErrorMessage = "resource not found"
                                    , apiErrorDetails = Nothing
                                    }
                    }
            runner _ _ =
                putMVar ran () >> pure (Right successfulOutput)
        withBackendApplication backend runner \application -> do
            response <-
                perform
                    application
                    methodPost
                    ["v1", "sessions", "session-a", "turns"]
                    validHeaders
                    "{\"input\":\"hello\"}"
            response.simpleStatus `shouldBe` status404
            LBS8.unpack response.simpleBody
                `shouldContain` "\"code\":\"not_found\""
            timeout (100 * 1000) (takeMVar ran)
                `shouldReturn` Nothing

    it "rejects a non-atomic multi-field session patch" do
        withApplication immediateRunner \application -> do
            response <-
                perform
                    application
                    methodPatch
                    ["v1", "sessions", "session-a"]
                    validHeaders
                    "{\"title\":\"new\",\"archived\":true}"
            response.simpleStatus `shouldBe` status422

    it "returns 409 for mutation while a session turn is active" do
        release <- newEmptyMVar
        started <- newEmptyMVar
        let runner _ _ =
                putMVar started ()
                    >> takeMVar release
                    >> pure (Right successfulOutput)
        withApplication runner \application -> do
            created <-
                perform
                    application
                    methodPost
                    ["v1", "sessions", "session-a", "turns"]
                    validHeaders
                    "{\"clientRequestId\":\"01999999-1111-7111-8111-111111111111\",\"input\":\"hello\"}"
            created.simpleStatus `shouldBe` status202
            takeMVar started
            patched <-
                perform
                    application
                    methodPatch
                    ["v1", "sessions", "session-a"]
                    validHeaders
                    "{\"title\":\"new title\"}"
            patched.simpleStatus `shouldBe` status409
            putMVar release ()

    it "honors a durable mutation reservation held by another instance" do
        patchCalls <- newIORef (0 :: Int)
        let backend =
                fakeBackend
                    { backendReserveSessionMutation =
                        \_ _ _ -> pure (Right Nothing)
                    , backendPatchSession = \_ _ _ -> do
                        atomicModifyIORef' patchCalls \count ->
                            (count + 1, ())
                        pure (Right (object []))
                    }
        withBackendApplication backend immediateRunner \application -> do
            patched <-
                perform
                    application
                    methodPatch
                    ["v1", "sessions", "session-a"]
                    validHeaders
                    "{\"title\":\"new title\"}"
            patched.simpleStatus `shouldBe` status409
            readIORef patchCalls `shouldReturn` 0

    it "does not run a mutation after its owner fence is lost" do
        patchCalls <- newIORef (0 :: Int)
        releaseCalls <- newIORef (0 :: Int)
        let ownerLost =
                ApiError
                    { apiErrorStatus = 503
                    , apiErrorCode = "turn_owner_unavailable"
                    , apiErrorMessage =
                        "the durable server owner fence is unavailable"
                    , apiErrorDetails = Nothing
                    }
            backend =
                fakeBackend
                    { backendReserveSessionMutation =
                        \_ _ _ ->
                            pure . Right . Just $
                                SessionMutationLease
                                    { runSessionMutationLease =
                                        \_ -> pure (Left ownerLost)
                                    , releaseSessionMutationLease =
                                        atomicModifyIORef'
                                            releaseCalls
                                            (\count -> (count + 1, ()))
                                    }
                    , backendPatchSession = \_ _ _ -> do
                        atomicModifyIORef' patchCalls \count ->
                            (count + 1, ())
                        pure (Right (object []))
                    }
        withBackendApplication backend immediateRunner \application -> do
            patched <-
                perform
                    application
                    methodPatch
                    ["v1", "sessions", "session-a"]
                    validHeaders
                    "{\"title\":\"new title\"}"
            patched.simpleStatus `shouldBe` status503
            readIORef patchCalls `shouldReturn` 0
            readIORef releaseCalls `shouldReturn` 1

    it "deduplicates turn retries and exposes their terminal output" do
        release <- newEmptyMVar
        started <- newEmptyMVar
        runCount <- newIORef (0 :: Int)
        let runner _ _ = do
                atomicModifyIORef' runCount \count -> (count + 1, ())
                putMVar started ()
                takeMVar release
                pure (Right successfulOutput)
            body =
                "{\"clientRequestId\":\"01999999-1111-7111-8111-111111111112\",\"input\":\"hello\"}"
        withDurableApplication runner \application terminal -> do
            created <-
                perform
                    application
                    methodPost
                    ["v1", "sessions", "session-a", "turns"]
                    validHeaders
                    body
            created.simpleStatus `shouldBe` status202
            takeMVar started

            duplicate <-
                perform
                    application
                    methodPost
                    ["v1", "sessions", "session-a", "turns"]
                    validHeaders
                    body
            duplicate.simpleStatus `shouldBe` status202
            duplicateTurnId <- responseTurnId duplicate
            originalTurnId <- responseTurnId created
            duplicateTurnId `shouldBe` originalTurnId

            conflict <-
                perform
                    application
                    methodPost
                    ["v1", "sessions", "session-a", "turns"]
                    validHeaders
                    "{\"clientRequestId\":\"01999999-1111-7111-8111-111111111112\",\"input\":\"different\"}"
            conflict.simpleStatus `shouldBe` status409

            activeResult <-
                perform
                    application
                    methodGet
                    ["v1", "turns", originalTurnId, "result"]
                    validHeaders
                    ""
            activeResult.simpleStatus `shouldBe` status409

            putMVar release ()
            takeMVar terminal
            result <-
                perform
                    application
                    methodGet
                    ["v1", "turns", originalTurnId, "result"]
                    validHeaders
                    ""
            result.simpleStatus `shouldBe` status200
            LBS8.unpack result.simpleBody
                `shouldContain` "\"assistantText\":\"done\""
            LBS8.unpack result.simpleBody
                `shouldContain` "\"assistantTextTruncated\":false"
            readIORef runCount `shouldReturn` 1

    it "durably cancels a reserved turn before local admission" do
        createdAt <- getCurrentTime
        let boundary =
                accessBoundary localPrincipal (GatewayBoundary Nothing)
            turnId = TurnId "01999999-1111-7111-8111-111111111113"
            record =
                TurnRecord
                    { turnRecordId = turnId
                    , turnRecordSessionId = "session-a"
                    , turnRecordClientRequestId =
                        ClientRequestId
                            "01999999-1111-7111-8111-111111111114"
                    , turnRecordBoundary = boundary
                    , turnRecordStatus = TurnQueued
                    , turnRecordCreatedAt = createdAt
                    , turnRecordStartedAt = Nothing
                    , turnRecordFinishedAt = Nothing
                    , turnRecordError = Nothing
                    }
        stored <- newMVar record
        let persistence =
                inMemoryTurnPersistence
                    { turnPersistenceStarted = \_ _ -> pure (Right ())
                    , turnPersistenceTerminal =
                        \_ finishedAt outcome ->
                            case outcome of
                                TurnWasCancelled -> do
                                    canonical <-
                                        modifyMVar stored \current ->
                                            let updated =
                                                    current
                                                        { turnRecordStatus =
                                                            TurnCancelled
                                                        , turnRecordFinishedAt =
                                                            Just finishedAt
                                                        }
                                             in pure (updated, updated)
                                    pure (Right canonical)
                                _ -> pure (Left "unexpected terminal outcome")
                    , turnPersistenceShouldCancel = \_ ->
                        pure (Right False)
                    }
            backend =
                fakeBackend
                    { backendLookupTurn = \requestedBoundary requestedId -> do
                        current <- readMVar stored
                        pure . Right $
                            if current.turnRecordBoundary == requestedBoundary
                                && current.turnRecordId == requestedId
                                then Just current
                                else Nothing
                    , backendTurnPersistence = persistence
                    , backendRequestTurnCancellation =
                        \requestedBoundary requestedId _ -> do
                            current <- readMVar stored
                            pure . Right $
                                if current.turnRecordBoundary
                                    == requestedBoundary
                                    && current.turnRecordId
                                        == requestedId
                                    then Just (True, current)
                                    else Nothing
                    }
        withBackendApplication backend immediateRunner \application -> do
            cancelled <-
                perform
                    application
                    methodPost
                    ["v1", "turns", turnId.unTurnId, "cancel"]
                    validHeaders
                    ""
            cancelled.simpleStatus `shouldBe` status200
            LBS8.unpack cancelled.simpleBody
                `shouldContain` "\"status\":\"cancelled\""

    it "canonicalizes an uppercase turn id before local cancellation" do
        createdAt <- getCurrentTime
        started <- newEmptyMVar
        stopped <- newEmptyMVar
        never <- newEmptyMVar
        fallbackCalls <- newIORef (0 :: Int)
        let turnId =
                TurnId "0199abcd-abcd-7abc-8abc-abcdefabcdef"
            reserved boundary sessionId clientRequestId =
                TurnRecord
                    { turnRecordId = turnId
                    , turnRecordSessionId = sessionId
                    , turnRecordClientRequestId = clientRequestId
                    , turnRecordBoundary = boundary
                    , turnRecordStatus = TurnQueued
                    , turnRecordCreatedAt = createdAt
                    , turnRecordStartedAt = Nothing
                    , turnRecordFinishedAt = Nothing
                    , turnRecordError = Nothing
                    }
            runner _ _ =
                (putMVar started () >> takeMVar never)
                    `finally` putMVar stopped ()
                    >> pure (Right successfulOutput)
            storedRecord boundary =
                reserved
                    boundary
                    "session-a"
                    ( ClientRequestId
                        "01999999-1111-7111-8111-111111111122"
                    )
            lookupRecord boundary requestedId =
                storedRecord boundary <$ guardTurnId requestedId
            backend =
                fakeBackend
                    { backendReserveTurn =
                        \boundary sessionId clientRequestId _ _ _ ->
                            pure . Right . TurnReservationCreated $
                                reserved boundary sessionId clientRequestId
                    , backendLookupTurn =
                        \boundary requestedId ->
                            pure (Right (lookupRecord boundary requestedId))
                    , backendRequestTurnCancellation =
                        \boundary requestedId _ -> do
                            atomicModifyIORef' fallbackCalls \count ->
                                (count + 1, ())
                            pure . Right $
                                fmap
                                    (\record -> (True, record))
                                    (lookupRecord boundary requestedId)
                    }
            body =
                "{\"clientRequestId\":\"01999999-1111-7111-8111-111111111122\",\"input\":\"hello\"}"
            guardTurnId requestedId
                | Text.toLower requestedId.unTurnId == turnId.unTurnId =
                    Just ()
                | otherwise = Nothing
        withBackendApplication backend runner \application -> do
            created <-
                perform
                    application
                    methodPost
                    ["v1", "sessions", "session-a", "turns"]
                    validHeaders
                    body
            created.simpleStatus `shouldBe` status202
            takeMVar started

            cancelled <-
                perform
                    application
                    methodPost
                    [ "v1"
                    , "turns"
                    , Text.toUpper turnId.unTurnId
                    , "cancel"
                    ]
                    validHeaders
                    ""
            cancelled.simpleStatus `shouldBe` status200
            timeout (2 * 1000 * 1000) (takeMVar stopped)
                `shouldReturn` Just ()
            readIORef fallbackCalls `shouldReturn` 0

    it "durably requests cancellation from another owning instance" do
        createdAt <- getCurrentTime
        requested <- newEmptyMVar
        let boundary =
                accessBoundary localPrincipal (GatewayBoundary Nothing)
            turnId = TurnId "01999999-1111-7111-8111-111111111119"
            record =
                TurnRecord
                    { turnRecordId = turnId
                    , turnRecordSessionId = "session-a"
                    , turnRecordClientRequestId =
                        ClientRequestId
                            "01999999-1111-7111-8111-111111111120"
                    , turnRecordBoundary = boundary
                    , turnRecordStatus = TurnRunning
                    , turnRecordCreatedAt = createdAt
                    , turnRecordStartedAt = Just createdAt
                    , turnRecordFinishedAt = Nothing
                    , turnRecordError = Nothing
                    }
            backend =
                fakeBackend
                    { backendLookupTurn =
                        \requestedBoundary requestedId ->
                            pure . Right $
                                if requestedBoundary == boundary
                                    && requestedId == turnId
                                    then Just record
                                    else Nothing
                    , backendRequestTurnCancellation =
                        \requestedBoundary requestedId _ -> do
                            putMVar
                                requested
                                (requestedBoundary, requestedId)
                            pure (Right (Just (False, record)))
                    }
        withBackendApplication backend immediateRunner \application -> do
            response <-
                perform
                    application
                    methodPost
                    ["v1", "turns", turnId.unTurnId, "cancel"]
                    validHeaders
                    ""
            response.simpleStatus `shouldBe` status202
            takeMVar requested `shouldReturn` (boundary, turnId)
            LBS8.unpack response.simpleBody
                `shouldContain` "\"status\":\"running\""

withApplication ::
    TurnRunner ->
    (Application -> IO value) ->
    IO value
withApplication = withApplicationAuth auth
  where
    auth =
        AuthConfig
            { authMode =
                LoopbackHostAuth
                    (Set.fromList ["127.0.0.1:4096"])
            , authCorsOrigins = Set.empty
            }

withDurableApplication ::
    TurnRunner ->
    (Application -> MVar () -> IO value) ->
    IO value
withDurableApplication runner action = do
    ledger <- newMVar []
    terminal <- newEmptyMVar
    let backend = durableBackend ledger terminal runner
    bracket
        ( newSupervisorWithBoundaryGuardAndPersistence
            supervisorConfig
            (\_ guarded -> Right <$> guarded)
            backend.backendTurnPersistence
            runner
        )
        closeSupervisor
        \supervisor -> do
            application <-
                newApplication
                    ApplicationConfig
                        { applicationMaximumRequestBytes = 256
                        , applicationOpenApiDocument = "{}"
                        }
                    auth
                    backend
                    supervisor
            action application terminal
  where
    auth =
        AuthConfig
            { authMode =
                LoopbackHostAuth
                    (Set.fromList ["127.0.0.1:4096"])
            , authCorsOrigins = Set.empty
            }
    supervisorConfig =
        SupervisorConfig
            { supervisorMaxConcurrentTurns = 1
            , supervisorMaxConcurrentTurnsPerTenant = 1
            , supervisorMaxQueuedTurns = 10
            , supervisorMaxQueuedTurnsPerTenant = 10
            , supervisorMaxEventSubscribers = 10
            , supervisorMaxEventSubscribersPerTenant = 5
            , supervisorEventReplayLimit = 10
            }

withBackendApplication ::
    Backend ->
    TurnRunner ->
    (Application -> IO value) ->
    IO value
withBackendApplication backend runner action =
    bracket
        ( newSupervisorWithBoundaryGuardAndPersistence
            supervisorConfig
            (\_ guarded -> Right <$> guarded)
            backend.backendTurnPersistence
            runner
        )
        closeSupervisor
        \supervisor -> do
            application <-
                newApplication
                    ApplicationConfig
                        { applicationMaximumRequestBytes = 256
                        , applicationOpenApiDocument = "{}"
                        }
                    auth
                    backend
                    supervisor
            action application
  where
    auth =
        AuthConfig
            { authMode =
                LoopbackHostAuth
                    (Set.fromList ["127.0.0.1:4096"])
            , authCorsOrigins = Set.empty
            }
    supervisorConfig =
        SupervisorConfig
            { supervisorMaxConcurrentTurns = 1
            , supervisorMaxConcurrentTurnsPerTenant = 1
            , supervisorMaxQueuedTurns = 10
            , supervisorMaxQueuedTurnsPerTenant = 10
            , supervisorMaxEventSubscribers = 10
            , supervisorMaxEventSubscribersPerTenant = 5
            , supervisorEventReplayLimit = 10
            }

withApplicationAuth ::
    AuthConfig ->
    TurnRunner ->
    (Application -> IO value) ->
    IO value
withApplicationAuth auth runner action =
    bracket
        (newSupervisor supervisorConfig runner)
        closeSupervisor
        \supervisor -> do
            application <-
                newApplication
                    ApplicationConfig
                        { applicationMaximumRequestBytes = 256
                        , applicationOpenApiDocument = "{}"
                        }
                    auth
                    fakeBackend
                    supervisor
            action application
  where
    supervisorConfig =
        SupervisorConfig
            { supervisorMaxConcurrentTurns = 1
            , supervisorMaxConcurrentTurnsPerTenant = 1
            , supervisorMaxQueuedTurns = 10
            , supervisorMaxQueuedTurnsPerTenant = 10
            , supervisorMaxEventSubscribers = 10
            , supervisorMaxEventSubscribersPerTenant = 5
            , supervisorEventReplayLimit = 10
            }

fakeBackend :: Backend
fakeBackend =
    Backend
        { backendAdmitBoundary = \principal action ->
            Right
                <$> action
                    (accessBoundary principal (GatewayBoundary Nothing))
        , backendContinueBoundary = \_ action ->
            Right <$> action
        , backendTurnBoundaryGuard = \_ action ->
            Right <$> action
        , backendCheckReady = pure (Right ())
        , backendListModels =
            \_ -> pure (Right (object ["models" .= ([] :: [Int])]))
        , backendListSessions =
            \_ _ _ _ ->
                pure (Right (object ["sessions" .= ([] :: [Int])]))
        , backendCreateSession =
            \_ _ -> pure (Right sessionValue)
        , backendGetSession =
            \_ _ -> pure (Right sessionValue)
        , backendPatchSession =
            \_ _ _ -> pure (Right sessionValue)
        , backendDeleteSession =
            \_ _ -> pure (Right ())
        , backendSessionHistory =
            \_ _ _ _ ->
                pure (Right (object ["turns" .= ([] :: [Int])]))
        , backendForkSession =
            \_ _ _ -> pure (Right sessionValue)
        , backendReserveSessionMutation =
            \_ _ _ ->
                pure
                    ( Right
                        ( Just
                            SessionMutationLease
                                { runSessionMutationLease =
                                    \action -> Right <$> action
                                , releaseSessionMutationLease = pure ()
                                }
                        )
                    )
        , backendReserveTurn =
            \boundary sessionId clientRequestId _ turnId createdAt ->
                pure . Right . TurnReservationCreated $
                    TurnRecord
                        { turnRecordId = turnId
                        , turnRecordSessionId = sessionId
                        , turnRecordClientRequestId = clientRequestId
                        , turnRecordBoundary = boundary
                        , turnRecordStatus = TurnQueued
                        , turnRecordCreatedAt = createdAt
                        , turnRecordStartedAt = Nothing
                        , turnRecordFinishedAt = Nothing
                        , turnRecordError = Nothing
                        }
        , backendLookupTurn = \_ _ -> pure (Right Nothing)
        , backendListTurns = \_ _ -> pure (Right [])
        , backendLookupTurnResult = \_ _ -> pure (Right Nothing)
        , backendRequestTurnCancellation =
            \_ _ _ -> pure (Right Nothing)
        , backendTurnPersistence = inMemoryTurnPersistence
        , backendRunTurn = immediateRunner
        }
  where
    sessionValue =
        object
            [ "id" .= ("session-a" :: String)
            ]

data TestStoredTurn = TestStoredTurn
    { testStoredInput :: !Text
    , testStoredRecord :: !TurnRecord
    , testStoredOutput :: !(Maybe TurnExecutionOutput)
    }

durableBackend ::
    MVar [TestStoredTurn] ->
    MVar () ->
    TurnRunner ->
    Backend
durableBackend ledger terminal runner =
    fakeBackend
        { backendReserveTurn =
            \boundary sessionId clientRequestId input turnId createdAt ->
                modifyMVar ledger \stored ->
                    case find
                        ( matchesRequest
                            boundary
                            sessionId
                            clientRequestId
                        )
                        stored of
                        Just existing
                            | existing.testStoredInput == input ->
                                pure
                                    ( stored
                                    , Right
                                        ( TurnReservationExistingOwned
                                            existing.testStoredRecord
                                        )
                                    )
                            | otherwise ->
                                pure
                                    ( stored
                                    , Left
                                        ApiError
                                            { apiErrorStatus = 409
                                            , apiErrorCode =
                                                "idempotency_conflict"
                                            , apiErrorMessage =
                                                "clientRequestId was already used with different input"
                                            , apiErrorDetails = Nothing
                                            }
                                    )
                        Nothing ->
                            let record =
                                    TurnRecord
                                        { turnRecordId = turnId
                                        , turnRecordSessionId = sessionId
                                        , turnRecordClientRequestId =
                                            clientRequestId
                                        , turnRecordBoundary = boundary
                                        , turnRecordStatus = TurnQueued
                                        , turnRecordCreatedAt = createdAt
                                        , turnRecordStartedAt = Nothing
                                        , turnRecordFinishedAt = Nothing
                                        , turnRecordError = Nothing
                                        }
                                entry =
                                    TestStoredTurn
                                        { testStoredInput = input
                                        , testStoredRecord = record
                                        , testStoredOutput = Nothing
                                        }
                             in pure
                                    ( entry : stored
                                    , Right (TurnReservationCreated record)
                                    )
        , backendLookupTurn = \boundary turnId -> do
            stored <- readMVar ledger
            pure . Right $
                (.testStoredRecord)
                    <$> find (matchesTurn boundary turnId) stored
        , backendListTurns = \boundary sessionId -> do
            stored <- readMVar ledger
            pure . Right $
                [ entry.testStoredRecord
                | entry <- stored
                , entry.testStoredRecord.turnRecordBoundary == boundary
                , maybe
                    True
                    (== entry.testStoredRecord.turnRecordSessionId)
                    sessionId
                ]
        , backendLookupTurnResult = \boundary turnId -> do
            stored <- readMVar ledger
            pure . Right $
                ( \entry ->
                    TurnResult
                        { turnResultTurn = entry.testStoredRecord
                        , turnResultOutput = entry.testStoredOutput
                        }
                )
                    <$> find (matchesTurn boundary turnId) stored
        , backendTurnPersistence =
            inMemoryTurnPersistence
                { turnPersistenceStarted = \record startedAt -> do
                    updateStoredTurn ledger record.turnRecordId \entry ->
                        entry
                            { testStoredRecord =
                                entry.testStoredRecord
                                    { turnRecordStatus = TurnRunning
                                    , turnRecordStartedAt = Just startedAt
                                    }
                            }
                    pure (Right ())
                , turnPersistenceTerminal = \record finishedAt outcome -> do
                    let (status, err, output) =
                            case outcome of
                                TurnSucceeded result ->
                                    (TurnCompleted, Nothing, Just result)
                                TurnErrored message ->
                                    (TurnFailed, Just message, Nothing)
                                TurnWasCancelled ->
                                    (TurnCancelled, Nothing, Nothing)
                        canonical =
                            record
                                { turnRecordStatus = status
                                , turnRecordFinishedAt = Just finishedAt
                                , turnRecordError = err
                                }
                    updateStoredTurn ledger record.turnRecordId \entry ->
                        entry
                            { testStoredRecord = canonical
                            , testStoredOutput = output
                            }
                    void (tryPutMVar terminal ())
                    pure (Right canonical)
                , turnPersistenceShouldCancel = \_ ->
                    pure (Right False)
                }
        , backendRunTurn = runner
        }

matchesRequest ::
    AccessBoundary ->
    Text ->
    ClientRequestId ->
    TestStoredTurn ->
    Bool
matchesRequest boundary sessionId clientRequestId entry =
    let record = entry.testStoredRecord
     in record.turnRecordBoundary == boundary
            && record.turnRecordSessionId == sessionId
            && record.turnRecordClientRequestId == clientRequestId

matchesTurn :: AccessBoundary -> TurnId -> TestStoredTurn -> Bool
matchesTurn boundary turnId entry =
    entry.testStoredRecord.turnRecordBoundary == boundary
        && entry.testStoredRecord.turnRecordId == turnId

updateStoredTurn ::
    MVar [TestStoredTurn] ->
    TurnId ->
    (TestStoredTurn -> TestStoredTurn) ->
    IO ()
updateStoredTurn ledger turnId update =
    modifyMVar_ ledger $
        pure
            . map
                ( \entry ->
                    if entry.testStoredRecord.turnRecordId == turnId
                        then update entry
                        else entry
                )

immediateRunner :: TurnRunner
immediateRunner _ _ = pure (Right successfulOutput)

successfulOutput :: TurnExecutionOutput
successfulOutput =
    TurnExecutionOutput
        { turnExecutionResponseId = "response-test"
        , turnExecutionAssistantText = Just "done"
        , turnExecutionAssistantTextTruncated = False
        , turnExecutionCompletion = TurnCompletionComplete
        }

perform ::
    Application ->
    Method ->
    [Text] ->
    [Header] ->
    LBS8.ByteString ->
    IO SResponse
perform application method path headers body =
    runSession
        ( srequest
            ( SRequest
                defaultRequest
                    { requestMethod = method
                    , pathInfo = path
                    , requestHeaders = headers
                    }
                body
            )
        )
        application

responseTurnId :: SResponse -> IO Text
responseTurnId response =
    case eitherDecode response.simpleBody of
        Right (Object value) ->
            case KeyMap.lookup "id" value of
                Just (String turnId) -> pure turnId
                other ->
                    expectationFailure
                        ("turn response has no text id: " <> show other)
                        >> fail "unreachable"
        decoded ->
            expectationFailure
                ("could not decode turn response: " <> show decoded)
                >> fail "unreachable"

responseTurnIds :: SResponse -> IO [Text]
responseTurnIds response =
    case eitherDecode response.simpleBody of
        Right (Object value) ->
            case KeyMap.lookup "data" value of
                Just (Array turns) ->
                    mapM extractTurnId (foldr (:) [] turns)
                other ->
                    expectationFailure
                        ("turn list response has no data array: " <> show other)
                        >> fail "unreachable"
        decoded ->
            expectationFailure
                ("could not decode turn list response: " <> show decoded)
                >> fail "unreachable"
  where
    extractTurnId = \case
        Object turn ->
            case KeyMap.lookup "id" turn of
                Just (String turnId) -> pure turnId
                other ->
                    expectationFailure
                        ("turn list entry has no text id: " <> show other)
                        >> fail "unreachable"
        other ->
            expectationFailure
                ("turn list entry is not an object: " <> show other)
                >> fail "unreachable"

validHeaders :: [Header]
validHeaders =
    [ ("Host", "127.0.0.1:4096")
    , ("Content-Type", "application/json")
    ]
