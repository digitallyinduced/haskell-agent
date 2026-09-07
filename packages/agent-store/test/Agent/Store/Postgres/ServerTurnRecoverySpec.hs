{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Agent.Store.Postgres.ServerTurnRecoverySpec (spec) where

import Agent.Store.Postgres (
    ManagedPostgresConfig,
    Store,
    closeStore,
    defaultManagedPostgresConfig,
    openStore,
    trustedPool,
 )
import Agent.Store.Postgres.Connection (StorePool)
import Agent.Store.Postgres.Managed (stopManagedPostgres)
import Agent.Store.Postgres.ServerTurn
import Agent.Store.Postgres.Session (SessionMetadata (..), createSession)
import Control.Exception.Safe (bracket, finally)
import Control.Monad (void)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec =
    describe "PostgreSQL server turn store" do
        it "keeps disconnected actions fenced across a PostgreSQL restart" $
            withSystemTempDirectory "ha-reap" \stateDirectory -> do
                let config = defaultManagedPostgresConfig stateDirectory ""
                    run =
                        withStore config \firstStore ->
                            withAbandonedOwner
                                (trustedPool firstStore)
                                instanceOne
                                \_ ->
                                    withActionFence
                                        (trustedPool firstStore)
                                        instanceOne
                                        \fence ->
                                            exerciseRestartFence
                                                config
                                                (trustedPool firstStore)
                                                fence
                run `finally` void (stopManagedPostgres config)

exerciseRestartFence ::
    ManagedPostgresConfig ->
    StorePool ->
    ServerTurnOwnerActionFence ->
    IO ()
exerciseRestartFence config firstPool fence = do
    let createdAt = read "2026-09-03 08:00:00 UTC"
        boundary =
            ServerTurnBoundary
                { serverTurnTenantId = "tenant-a"
                , serverTurnGatewayIdentity =
                    Just "gateway-sha256:generation-a"
                }
        request =
            ReserveServerTurn
                { reserveServerTurnId =
                    "01999999-0000-7000-8000-000000000031"
                , reserveServerTurnBoundary = boundary
                , reserveServerTurnSessionId = "session-restart-fence"
                , reserveServerTurnClientRequestId =
                    "01999999-0000-7000-8000-000000000032"
                , reserveServerTurnInputDigest =
                    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                , reserveServerTurnOwnerInstanceId = instanceOne
                , reserveServerTurnCreatedAt = createdAt
                }

    createSession firstPool (testMetadata createdAt)
        `shouldReturn` Right True
    reserveServerTurn firstPool request >>= \result ->
        result `shouldSatisfy` \case
            Right (ServerTurnReserved _) -> True
            _ -> False
    stopManagedPostgres config `shouldReturn` Right ()

    withStore config \secondStore ->
        withOwner (trustedPool secondStore) instanceTwo \ownerTwo -> do
            let secondPool = trustedPool secondStore
            loadServerTurn
                secondPool
                boundary
                request.reserveServerTurnId
                >>= \result ->
                    result `shouldSatisfy` \case
                        Right (Just stored) ->
                            stored.storedServerTurnStatus == ServerTurnQueued
                        _ -> False

            closeServerTurnOwnerActionFence fence
            heartbeatServerTurnOwner ownerTwo `shouldReturn` Right ()
            loadServerTurn
                secondPool
                boundary
                request.reserveServerTurnId
                >>= \result ->
                    result `shouldSatisfy` \case
                        Right (Just stored) ->
                            stored.storedServerTurnStatus == ServerTurnFailed
                                && stored.storedServerTurnFinishedAt /= Nothing
                                && stored.storedServerTurnError
                                    == Just
                                        "agent server owner disconnected before the turn completed"
                        _ -> False

withStore ::
    ManagedPostgresConfig ->
    (Store -> IO value) ->
    IO value
withStore config =
    bracket
        ( openStore config >>= \case
            Left err -> fail ("could not open store: " <> show err)
            Right store -> pure store
        )
        closeStore

withAbandonedOwner ::
    StorePool ->
    Text ->
    (ServerTurnOwnerLease -> IO value) ->
    IO value
withAbandonedOwner pool instanceId =
    bracket
        (requireOwner pool instanceId)
        abandonServerTurnOwnerLease

withOwner ::
    StorePool ->
    Text ->
    (ServerTurnOwnerLease -> IO value) ->
    IO value
withOwner pool instanceId =
    bracket
        (requireOwner pool instanceId)
        (void . releaseServerTurnOwner)

requireOwner :: StorePool -> Text -> IO ServerTurnOwnerLease
requireOwner pool instanceId =
    openServerTurnOwnerLease pool instanceId >>= \case
        Left err -> fail ("could not open server turn owner: " <> show err)
        Right owner -> pure owner

withActionFence ::
    StorePool ->
    Text ->
    (ServerTurnOwnerActionFence -> IO value) ->
    IO value
withActionFence pool instanceId =
    bracket
        ( openServerTurnOwnerActionFence pool instanceId >>= \case
            Left err -> fail ("could not open owner action fence: " <> show err)
            Right Nothing -> fail "server turn owner was unavailable"
            Right (Just fence) -> pure fence
        )
        closeServerTurnOwnerActionFence

instanceOne :: Text
instanceOne = "01999999-0000-7000-8000-000000000033"

instanceTwo :: Text
instanceTwo = "01999999-0000-7000-8000-000000000034"

testMetadata :: UTCTime -> SessionMetadata
testMetadata now =
    SessionMetadata
        { sessionMetadataKey = "session-restart-fence"
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
