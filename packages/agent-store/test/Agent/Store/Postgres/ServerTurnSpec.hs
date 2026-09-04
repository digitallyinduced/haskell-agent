{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Agent.Store.Postgres.ServerTurnSpec (spec) where

import Agent.Store.Postgres
    ( StoreError (..)
    , closeStore
    , defaultManagedPostgresConfig
    , openStore
    , trustedPool
    )
import Agent.Store.Postgres.Connection
    ( StorePool
    , closeStoreConnection
    , openStoreConnection
    , withConnectionSession
    , withSession
    )
import Agent.Store.Postgres.Managed (stopManagedPostgres)
import qualified Agent.Store.Postgres.ServerHumanRequest as HumanRequest
import Agent.Store.Postgres.ServerTurn
import Agent.Store.Postgres.Session (SessionMetadata (..), createSession)
import Control.Exception.Safe (bracket, finally)
import Control.Monad (void)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, addUTCTime)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)
import qualified Hasql.Statement as Statement
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "PostgreSQL server turn store" do
    it "keeps admission idempotent and terminal output restart-safe" $
        withSystemTempDirectory "ha-server-turn" \stateDirectory -> do
            let config = defaultManagedPostgresConfig stateDirectory ""
                cleanup = do
                    _ <- stopManagedPostgres config
                    pure ()
            ( openStore config >>= \case
                    Left err ->
                        expectationFailure ("could not open store: " <> show err)
                    Right store ->
                        finally
                            (exerciseTurnStore (trustedPool store))
                            (closeStore store)
                )
                `finally` cleanup

exerciseTurnStore :: StorePool -> IO ()
exerciseTurnStore pool =
    withServerTurnOwner pool instanceOne \ownerOne ->
        withServerTurnOwner pool instanceTwo \ownerTwo ->
            exerciseTurnStoreWithOwners pool ownerOne ownerTwo

exerciseTurnStoreWithOwners ::
    StorePool ->
    ServerTurnOwnerLease ->
    ServerTurnOwnerLease ->
    IO ()
exerciseTurnStoreWithOwners pool ownerOne ownerTwo = do
    let createdAt = read "2026-09-03 08:00:00 UTC"
        startedAt = addUTCTime 1 createdAt
        finishedAt = addUTCTime 2 createdAt
        boundary =
            ServerTurnBoundary
                { serverTurnTenantId = "tenant-a"
                , serverTurnGatewayIdentity =
                    Just "gateway-sha256:generation-a"
                }
        otherBoundary =
            boundary
                { serverTurnGatewayIdentity =
                    Just "gateway-sha256:generation-b"
                }
        request =
            ReserveServerTurn
                { reserveServerTurnId =
                    "01999999-0000-7000-8000-000000000001"
                , reserveServerTurnBoundary = boundary
                , reserveServerTurnSessionId = "session-a"
                , reserveServerTurnClientRequestId =
                    "01999999-0000-7000-8000-000000000002"
                , reserveServerTurnInputDigest =
                    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                , reserveServerTurnOwnerInstanceId = instanceOne
                , reserveServerTurnCreatedAt = createdAt
                }
        mutation =
            ServerSessionMutation
                { serverSessionMutationBoundary = boundary
                , serverSessionMutationSessionId = "session-a"
                , serverSessionMutationOwnerInstanceId =
                    request.reserveServerTurnOwnerInstanceId
                , serverSessionMutationCreatedAt = createdAt
                }

    createSession pool (testMetadata createdAt)
        `shouldReturn` Right True
    heartbeatServerTurnOwner
        ownerOne
        `shouldReturn` Right ()

    wrongBoundaryReservation <-
        reserveServerTurn
            pool
            request
                { reserveServerTurnId =
                    "01999999-0000-7000-8000-000000000009"
                , reserveServerTurnBoundary = otherBoundary
                , reserveServerTurnClientRequestId =
                    "01999999-0000-7000-8000-000000000010"
                }
    wrongBoundaryReservation `shouldSatisfy` \case
        Left (StoreDataError _) -> True
        _ -> False

    shouldCancelServerTurn
        pool
        boundary
        request.reserveServerTurnOwnerInstanceId
        request.reserveServerTurnId
        `shouldReturn` Right True

    reserveServerSessionMutation pool mutation
        `shouldReturn` Right ServerSessionMutationReserved
    reserveServerTurn pool request
        `shouldReturn` Right ServerTurnSessionMutating
    releaseServerSessionMutation
        pool
        mutation
            { serverSessionMutationCreatedAt =
                addUTCTime 1 createdAt
            }
        `shouldReturn` Right ()
    reserveServerSessionMutation pool mutation
        `shouldReturn` Right ServerSessionMutationBusy
    releaseServerSessionMutation pool mutation
        `shouldReturn` Right ()

    first <- reserveServerTurn pool request
    first `shouldSatisfy` \case
        Right (ServerTurnReserved stored) ->
            stored.storedServerTurnId == request.reserveServerTurnId
                && stored.storedServerTurnStatus == ServerTurnQueued
        _ -> False

    reserveServerSessionMutation pool mutation
        `shouldReturn` Right ServerSessionMutationBusy

    reserveServerTurn
        pool
        request
            { reserveServerTurnId =
                "01999999-0000-7000-8000-000000000005"
            }
        `shouldReturn` fmap
            ( \case
                ServerTurnReserved stored ->
                    ServerTurnAlreadyReserved stored
                other -> other
            )
            first

    conflict <-
        reserveServerTurn
            pool
            request
                { reserveServerTurnId =
                    "01999999-0000-7000-8000-000000000006"
                , reserveServerTurnInputDigest =
                    "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                }
    conflict `shouldSatisfy` \case
        Right (ServerTurnIdempotencyConflict stored) ->
            stored.storedServerTurnId == request.reserveServerTurnId
        _ -> False

    sessionBusy <-
        reserveServerTurn
            pool
            request
                { reserveServerTurnId =
                    "01999999-0000-7000-8000-000000000011"
                , reserveServerTurnClientRequestId =
                    "01999999-0000-7000-8000-000000000012"
                , reserveServerTurnOwnerInstanceId =
                    request.reserveServerTurnOwnerInstanceId
                }
    sessionBusy `shouldSatisfy` \case
        Right (ServerTurnSessionBusy stored) ->
            stored.storedServerTurnId == request.reserveServerTurnId
        _ -> False

    loadServerTurn
        pool
        otherBoundary
        request.reserveServerTurnId
        `shouldReturn` Right Nothing

    markServerTurnRunning
        pool
        boundary
        request.reserveServerTurnOwnerInstanceId
        request.reserveServerTurnId
        startedAt
        `shouldReturn` Right True

    shouldCancelServerTurn
        pool
        boundary
        request.reserveServerTurnOwnerInstanceId
        request.reserveServerTurnId
        `shouldReturn` Right False

    let humanRequest =
            HumanRequest.CreateServerHumanRequest
                { HumanRequest.createServerHumanRequestId =
                    "01999999-0000-7000-8000-000000000013"
                , HumanRequest.createServerHumanRequestTurnId =
                    request.reserveServerTurnId
                , HumanRequest.createServerHumanRequestBoundary = boundary
                , HumanRequest.createServerHumanRequestOwnerInstanceId =
                    request.reserveServerTurnOwnerInstanceId
                , HumanRequest.createServerHumanRequestKind = "tool_approval"
                , HumanRequest.createServerHumanRequestPrompt = "Approve?"
                , HumanRequest.createServerHumanRequestOptionsJson =
                    "[\"approve\",\"cancel\"]"
                , HumanRequest.createServerHumanRequestCreatedAt = startedAt
                }
        approval =
            HumanRequest.ServerHumanResponse
                { HumanRequest.serverHumanResponseDecision = "approve"
                , HumanRequest.serverHumanResponseValue = Just "once"
                }
    HumanRequest.createServerHumanRequest pool humanRequest
        `shouldReturn` Right True
    HumanRequest.listServerHumanRequests pool otherBoundary
        `shouldReturn` Right []
    pendingRequests <-
        HumanRequest.listServerHumanRequests pool boundary
    pendingRequests `shouldSatisfy` \case
        Right [pending] ->
            pending.storedServerHumanRequestId
                == humanRequest.createServerHumanRequestId
                && pending.storedServerHumanRequestOptionsJson
                    == "[\"approve\", \"cancel\"]"
        _ -> False
    HumanRequest.listServerHumanRequestsForTurn
        pool
        boundary
        request.reserveServerTurnId
        `shouldReturn` pendingRequests
    HumanRequest.listServerHumanRequestsForTurn
        pool
        boundary
        "01999999-0000-7000-8000-000000000099"
        `shouldReturn` Right []
    HumanRequest.resolveServerHumanRequest
        pool
        boundary
        humanRequest.createServerHumanRequestId
        approval
            { HumanRequest.serverHumanResponseDecision = "deny"
            }
        finishedAt
        `shouldReturn` Right HumanRequest.ServerHumanRequestInvalidDecision
    resolvedRequest <-
        HumanRequest.resolveServerHumanRequest
            pool
            boundary
            humanRequest.createServerHumanRequestId
            approval
            finishedAt
    resolvedRequest `shouldSatisfy` \case
        Right (HumanRequest.ServerHumanRequestResolved resolved) ->
            resolved.storedServerHumanRequestResponseDecision
                == Just "approve"
                && resolved.storedServerHumanRequestResponseValue
                    == Just "once"
        _ -> False
    HumanRequest.loadServerHumanResponse
        pool
        boundary
        instanceTwo
        request.reserveServerTurnId
        humanRequest.createServerHumanRequestId
        `shouldReturn` Right Nothing
    HumanRequest.loadServerHumanResponse
        pool
        boundary
        request.reserveServerTurnOwnerInstanceId
        request.reserveServerTurnId
        humanRequest.createServerHumanRequestId
        `shouldReturn` Right (Just approval)
    HumanRequest.deleteServerHumanRequest
        pool
        boundary
        request.reserveServerTurnOwnerInstanceId
        request.reserveServerTurnId
        humanRequest.createServerHumanRequestId
        `shouldReturn` Right ()
    HumanRequest.loadServerHumanResponse
        pool
        boundary
        request.reserveServerTurnOwnerInstanceId
        request.reserveServerTurnId
        humanRequest.createServerHumanRequestId
        `shouldReturn` Right (Just approval)
    HumanRequest.deleteConsumedServerHumanRequest
        pool
        boundary
        request.reserveServerTurnOwnerInstanceId
        request.reserveServerTurnId
        humanRequest.createServerHumanRequestId
        `shouldReturn` Right ()
    HumanRequest.loadServerHumanResponse
        pool
        boundary
        request.reserveServerTurnOwnerInstanceId
        request.reserveServerTurnId
        humanRequest.createServerHumanRequestId
        `shouldReturn` Right Nothing
    HumanRequest.listServerHumanRequests pool boundary
        `shouldReturn` Right []

    requestedCancellation <-
        requestServerTurnCancellation
            pool
            boundary
            request.reserveServerTurnId
            startedAt
    requestedCancellation `shouldSatisfy` \case
        Right (Just stored) ->
            stored.storedServerTurnStatus == ServerTurnRunning
        _ -> False
    shouldCancelServerTurn
        pool
        boundary
        request.reserveServerTurnOwnerInstanceId
        request.reserveServerTurnId
        `shouldReturn` Right True
    shouldCancelServerTurn
        pool
        boundary
        instanceTwo
        request.reserveServerTurnId
        `shouldReturn` Right True

    completed <-
        finishServerTurn
            pool
            boundary
            request.reserveServerTurnOwnerInstanceId
            request.reserveServerTurnId
            ServerTurnTerminal
                { terminalServerTurnStatus = ServerTurnCompleted
                , terminalServerTurnFinishedAt = finishedAt
                , terminalServerTurnAssistantText = Just "hello"
                , terminalServerTurnAssistantTextTruncated = False
                , terminalServerTurnResponseId = Just "response-a"
                , terminalServerTurnIncompleteReason = Nothing
                , terminalServerTurnIncompleteReasoningTokens = Nothing
                , terminalServerTurnError = Nothing
                }
    completed `shouldSatisfy` \case
        Right (Just stored) ->
            stored.storedServerTurnStatus == ServerTurnCompleted
                && stored.storedServerTurnAssistantText == Just "hello"
                && not stored.storedServerTurnAssistantTextTruncated
                && stored.storedServerTurnResponseId == Just "response-a"
        _ -> False

    -- Wall-clock staleness cannot revoke a process which still owns its
    -- connection-lifetime advisory lock. In particular, a paused mutation
    -- remains exclusive and its handler may safely resume.
    reserveServerSessionMutation pool mutation
        `shouldReturn` Right ServerSessionMutationReserved
    ageServerTurnOwner pool instanceOne
        `shouldReturn` Right ()
    heartbeatServerTurnOwner ownerTwo
        `shouldReturn` Right ()
    let instanceTwoMutation =
            mutation
                { serverSessionMutationOwnerInstanceId = instanceTwo
                , serverSessionMutationCreatedAt = addUTCTime 3 finishedAt
                }
    reserveServerSessionMutation pool instanceTwoMutation
        `shouldReturn` Right ServerSessionMutationBusy
    reserveServerTurn
        pool
        request
            { reserveServerTurnId =
                "01999999-0000-7000-8000-000000000020"
            , reserveServerTurnClientRequestId =
                "01999999-0000-7000-8000-000000000021"
            }
        `shouldReturn` Right ServerTurnSessionMutating
    releaseServerSessionMutation pool mutation
        `shouldReturn` Right ()

    -- Reaping must not wait behind the owner-row share lock held by an
    -- in-flight admission. Waiting would retain an old READ COMMITTED snapshot
    -- and can also make the healthy reaper miss its heartbeat deadline.
    withServerTurnOwnerRowLocked pool instanceOne do
        timeout
            (2 * 1000 * 1000)
            (heartbeatServerTurnOwner ownerTwo)
            `shouldReturn` Just (Right ())

    let interruptedRequest =
            request
                { reserveServerTurnId =
                    "01999999-0000-7000-8000-000000000007"
                , reserveServerTurnClientRequestId =
                    "01999999-0000-7000-8000-000000000008"
                }
    interruptedReservation <- reserveServerTurn pool interruptedRequest
    interruptedReservation `shouldSatisfy` \case
        Right (ServerTurnReserved _) -> True
        _ -> False

    -- Another live instance may inspect and heartbeat without changing a
    -- paused owner's turn, even though its timestamp is arbitrarily old.
    observedByOther <-
        loadServerTurn
            pool
            boundary
            interruptedRequest.reserveServerTurnId
    observedByOther `shouldSatisfy` \case
        Right (Just stored) ->
            stored.storedServerTurnStatus == ServerTurnQueued
        _ -> False
    heartbeatServerTurnOwner ownerTwo
        `shouldReturn` Right ()
    stillQueued <-
        loadServerTurn
            pool
            boundary
            interruptedRequest.reserveServerTurnId
    stillQueued `shouldSatisfy` \case
        Right (Just stored) ->
            stored.storedServerTurnStatus == ServerTurnQueued
        _ -> False

    -- A real crash closes the liveness connection. The next owner can then
    -- recover the turn and session. A shared owner-action fence delays that
    -- recovery while a turn or mutation can still produce effects.
    createSession
        pool
        (testMetadata createdAt)
            { sessionMetadataKey = "session-b"
            }
        `shouldReturn` Right True
    let protectedMutation =
            mutation
                { serverSessionMutationSessionId = "session-b"
                , serverSessionMutationCreatedAt =
                    addUTCTime 4 finishedAt
                }
    withServerTurnOwnerActionFence pool instanceOne \_ -> do
        reserveServerSessionMutation pool protectedMutation
            `shouldReturn` Right ServerSessionMutationReserved
        abandonServerTurnOwnerLease ownerOne
        reserveServerTurn pool interruptedRequest
            `shouldReturn` Right ServerTurnOwnerUnavailable
        reserveServerSessionMutation pool mutation
            `shouldReturn` Right ServerSessionMutationOwnerUnavailable
        heartbeatServerTurnOwner ownerTwo
            `shouldReturn` Right ()
        stillProtected <-
            loadServerTurn
                pool
                boundary
                interruptedRequest.reserveServerTurnId
        stillProtected `shouldSatisfy` \case
            Right (Just stored) ->
                stored.storedServerTurnStatus == ServerTurnQueued
            _ -> False
        reserveServerSessionMutation
            pool
            protectedMutation
                { serverSessionMutationOwnerInstanceId = instanceTwo
                }
            `shouldReturn` Right ServerSessionMutationBusy
        releaseServerSessionMutation pool protectedMutation
            `shouldReturn` Right ()

    heartbeatServerTurnOwner ownerTwo
        `shouldReturn` Right ()
    interrupted <-
        loadServerTurn
            pool
            boundary
            interruptedRequest.reserveServerTurnId
    interrupted `shouldSatisfy` \case
        Right (Just stored) ->
            stored.storedServerTurnStatus == ServerTurnFailed
                && stored.storedServerTurnFinishedAt /= Nothing
                && stored.storedServerTurnError
                    == Just
                        "agent server owner disconnected before the turn completed"
        _ -> False
    replay <-
        reserveServerTurn
            pool
            interruptedRequest
                { reserveServerTurnOwnerInstanceId = instanceTwo
                , reserveServerTurnCreatedAt = addUTCTime 1 finishedAt
                }
    replay `shouldSatisfy` \case
        Right (ServerTurnAlreadyReserved stored) ->
            stored.storedServerTurnStatus == ServerTurnFailed
        _ -> False

    reserveServerSessionMutation pool instanceTwoMutation
        `shouldReturn` Right ServerSessionMutationReserved
    releaseServerSessionMutation pool instanceTwoMutation
        `shouldReturn` Right ()

    let recoveredRequest =
            request
                { reserveServerTurnId =
                    "01999999-0000-7000-8000-000000000016"
                , reserveServerTurnClientRequestId =
                    "01999999-0000-7000-8000-000000000017"
                , reserveServerTurnOwnerInstanceId = instanceTwo
                , reserveServerTurnCreatedAt = addUTCTime 4 finishedAt
                }
    recovered <- reserveServerTurn pool recoveredRequest
    recovered `shouldSatisfy` \case
        Right (ServerTurnReserved _) -> True
        _ -> False

    -- The abandoned identity cannot heartbeat, reserve, or be reopened after
    -- its durable tombstone has fenced it.
    abandonedHeartbeat <- heartbeatServerTurnOwner ownerOne
    abandonedHeartbeat `shouldSatisfy` \case
        Left (StoreDataError _) -> True
        _ -> False
    reopen <- openServerTurnOwnerLease pool instanceOne
    case reopen of
        Left (StoreDataError _) -> pure ()
        Left err ->
            expectationFailure
                ("unexpected owner reopen error: " <> show err)
        Right lease -> do
            void (releaseServerTurnOwner lease)
            expectationFailure "revoked owner identity was reopened"
    reserveServerTurn
        pool
        recoveredRequest
            { reserveServerTurnId =
                "01999999-0000-7000-8000-000000000018"
            , reserveServerTurnClientRequestId =
                "01999999-0000-7000-8000-000000000019"
            , reserveServerTurnOwnerInstanceId = instanceOne
            }
        `shouldReturn` Right ServerTurnOwnerUnavailable
    reserveServerSessionMutation
        pool
        instanceTwoMutation
            { serverSessionMutationOwnerInstanceId = instanceOne
            }
        `shouldReturn` Right ServerSessionMutationOwnerUnavailable

    listed <-
        listServerTurns
            pool
            boundary
            (Just "session-a")
    listed `shouldSatisfy` \case
        Right turns -> length turns == 3
        Left _ -> False

instanceOne :: Text
instanceOne = "01999999-0000-7000-8000-000000000003"

instanceTwo :: Text
instanceTwo = "01999999-0000-7000-8000-000000000004"

withServerTurnOwner ::
    StorePool ->
    Text ->
    (ServerTurnOwnerLease -> IO value) ->
    IO value
withServerTurnOwner pool instanceId =
    bracket
        ( openServerTurnOwnerLease pool instanceId >>= \case
            Left err ->
                fail ("could not open server turn owner: " <> show err)
            Right lease -> pure lease
        )
        (void . releaseServerTurnOwner)

withServerTurnOwnerActionFence ::
    StorePool ->
    Text ->
    (ServerTurnOwnerActionFence -> IO value) ->
    IO value
withServerTurnOwnerActionFence pool instanceId =
    bracket
        ( openServerTurnOwnerActionFence pool instanceId >>= \case
            Left err ->
                fail ("could not open owner action fence: " <> show err)
            Right Nothing ->
                fail "server turn owner was unavailable"
            Right (Just fence) -> pure fence
        )
        closeServerTurnOwnerActionFence

withServerTurnOwnerRowLocked ::
    StorePool ->
    Text ->
    IO value ->
    IO value
withServerTurnOwnerRowLocked pool instanceId action =
    bracket
        ( openStoreConnection pool >>= \case
            Left err ->
                fail ("could not open owner row lock connection: " <> show err)
            Right connection -> pure connection
        )
        closeStoreConnection
        \connection ->
            bracket
                ( do
                    withConnectionSession connection (Session.script "BEGIN")
                        >>= either
                            (fail . ("could not begin owner row lock: " <>) . show)
                            pure
                    withConnectionSession
                        connection
                        ( Session.statement
                            instanceId
                            lockServerTurnOwnerRowStatement
                        )
                        >>= \case
                            Left err ->
                                fail
                                    ( "could not lock server turn owner row: "
                                        <> show err
                                    )
                            Right True -> pure ()
                            Right False ->
                                fail "server turn owner row was missing"
                )
                ( \() ->
                    void $
                        withConnectionSession
                            connection
                            (Session.script "ROLLBACK")
                )
                (const action)

lockServerTurnOwnerRowStatement :: Statement Text Bool
lockServerTurnOwnerRowStatement =
    Statement.preparable
        "SELECT TRUE\
        \ FROM harness.server_turn_owners\
        \ WHERE instance_id = $1::uuid\
        \ FOR SHARE"
        (Encoders.param (Encoders.nonNullable Encoders.text))
        (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))

ageServerTurnOwner :: StorePool -> Text -> IO (Either StoreError ())
ageServerTurnOwner pool instanceId =
    withSession pool $
        Session.statement instanceId ageServerTurnOwnerStatement

ageServerTurnOwnerStatement :: Statement Text ()
ageServerTurnOwnerStatement =
    Statement.preparable
        "UPDATE harness.server_turn_owners\
        \ SET last_heartbeat_at = clock_timestamp() - interval '1 day'\
        \ WHERE instance_id = $1::uuid"
        (Encoders.param (Encoders.nonNullable Encoders.text))
        Decoders.noResult

testMetadata :: UTCTime -> SessionMetadata
testMetadata now =
    SessionMetadata
        { sessionMetadataKey = "session-a"
        , sessionMetadataVersion = 1
        , sessionMetadataCreatedAt = now
        , sessionMetadataUpdatedAt = now
        , sessionMetadataProvider = "openai"
        , sessionMetadataConnection = "organization-gateway"
        , sessionMetadataGatewayIdentity =
            Just "gateway-sha256:generation-a"
        , sessionMetadataModel = "gpt-test"
        , sessionMetadataTransportModel = Just "gpt-test"
        , sessionMetadataDialect = "openai"
        , sessionMetadataLegacyTarget = Nothing
        , sessionMetadataCwd = "/tmp/project"
        , sessionMetadataEffort = "medium"
        , sessionMetadataTitle = "test"
        , sessionMetadataTitleIsManual = False
        , sessionMetadataTitleRefreshIndex = 0
        , sessionMetadataTitleUserTurns = 0
        , sessionMetadataLastResponseId = Nothing
        , sessionMetadataInputTokens = 0
        , sessionMetadataOutputTokens = 0
        , sessionMetadataCachedTokens = 0
        , sessionMetadataLastRecap = Nothing
        , sessionMetadataLastTurnSummary = Nothing
        , sessionMetadataLastRecapMainTurns = 0
        }
