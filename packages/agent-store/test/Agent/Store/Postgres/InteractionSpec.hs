{-# LANGUAGE OverloadedStrings #-}

module Agent.Store.Postgres.InteractionSpec (spec) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception.Safe (finally)
import qualified Data.ByteString as ByteString
import Data.Either (isLeft)
import Data.Foldable (toList)
import Data.List (nub)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime)
import qualified Hasql.Session as HasqlSession
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Agent.Store.Postgres
    ( ManagedPostgresConfig
    , closeStore
    , defaultManagedPostgresConfig
    , openStore
    , trustedPool
    )
import Agent.Store.Postgres.Connection (StorePool, withSession)
import Agent.Store.Postgres.Interaction
import Agent.Store.Postgres.Managed (stopManagedPostgres)
import Agent.Store.Postgres.Session
    ( SessionMetadata(..)
    , SessionTurn(..)
    , StoredSession(..)
    , StoredTurn(..)
    , TranscriptEffect(..)
    , appendSessionTurnIndexedAndDeliver
    , appendSessionTurnIndexedAndDeliverMany
    , createSession
    , loadSession
    )
import Agent.Store.Types (StoreError(..))

spec :: Spec
spec = describe "PostgreSQL pending interaction storage" do
    it "uses immutable request, resolution, and delivery facts" do
        let ddl = ByteString.intercalate "\n" interactionSchemaStatements
            grants =
                ByteString.intercalate "\n" interactionRuntimeGrantStatements
        ddl `shouldContainBytes`
            "CREATE TABLE IF NOT EXISTS harness.session_interactions"
        ddl `shouldContainBytes`
            "harness.session_interaction_resolutions"
        ddl `shouldContainBytes`
            "CREATE TABLE IF NOT EXISTS harness.session_interaction_deliveries"
        ddl `shouldContainBytes`
            "UNIQUE (session_id, request_key)"
        ddl `shouldContainBytes`
            "REFERENCES harness.session_turns(session_id, turn_index)"
        ddl `shouldContainBytes`
            "reject_session_interaction_fact_mutation"
        ddl `shouldContainBytes` "session_interactions_immutable"
        ddl `shouldContainBytes` "session_interaction_resolutions_immutable"
        ddl `shouldContainBytes` "session_interaction_deliveries_immutable"
        ddl `shouldNotContainBytes` "jsonb"
        grants `shouldContainBytes`
            "GRANT SELECT ON harness.session_interactions"
        grants `shouldContainBytes` "turn_fingerprint"
        grants `shouldNotContainBytes` "UPDATE"
        grants `shouldNotContainBytes` "DELETE"

    it "publishes idempotently, resolves first-answer-wins, and tracks delivery" $
        withSystemTempDirectory "ha" \stateDirectory -> do
            let
                config = defaultManagedPostgresConfig stateDirectory ""
                cleanup = do
                    _ <- stopManagedPostgres config
                    pure ()
            withOpenStore config exerciseInteractions `finally` cleanup

exerciseInteractions :: StorePool -> IO ()
exerciseInteractions pool = do
    let
        now = read "2026-08-30 00:00:00 UTC"
        later = read "2026-08-30 00:01:00 UTC"
        metadata = testMetadata now
        request = InteractionRequest
            { interactionRequestSessionKey = "interaction-session"
            , interactionRequestKey = "plan:revision:7"
            , interactionRequestKind = "plan_approval"
            , interactionRequestPayloadVersion = 1
            , interactionRequestPayload =
                "{\"digest\":\"sha256:abc\",\"markdown\":\"# Plan\"}"
            , interactionRequestOrigin =
                Just InteractionOrigin
                    { interactionOriginToolName = "exit_plan_mode"
                    , interactionOriginCallId = "call-7"
                    }
            , interactionRequestCreatedAt = now
            }
    publishSessionInteraction pool request
        { interactionRequestSessionKey = "missing-session"
        }
        `shouldReturn` Right InteractionPublishSessionNotFound
    createSession pool metadata `shouldReturn` Right True

    first <- publishSessionInteraction pool request
    interaction <- expectPublished True first
    interaction.sessionInteractionRequestKey
        `shouldBe` request.interactionRequestKey
    interaction.sessionInteractionResolution `shouldBe` Nothing
    immutable <- withSession pool
        (HasqlSession.script
            "UPDATE harness.session_interactions\
            \ SET request_payload_text = 'changed'")
    immutable `shouldSatisfy` isLeft

    second <- publishSessionInteraction pool request
    duplicate <- expectPublished False second
    duplicate `shouldBe` interaction

    changed <- publishSessionInteraction pool request
        { interactionRequestPayload = "{\"digest\":\"different\"}"
        }
    changed `shouldSatisfy` isImmutableConflict

    loadSessionInteraction
        pool
        "interaction-session"
        interaction.sessionInteractionId
        `shouldReturn` Right (Just interaction)
    loadSessionInteractionByRequestKey
        pool
        "interaction-session"
        "plan:revision:7"
        `shouldReturn` Right (Just interaction)
    listOpenSessionInteractions pool "interaction-session"
        `shouldReturn` Right [interaction]
    listUndeliveredSessionInteractions pool "interaction-session"
        `shouldReturn` Right []

    markSessionInteractionDelivered pool InteractionDeliveryRequest
        { interactionDeliveryRequestSessionKey = "interaction-session"
        , interactionDeliveryRequestInteractionId =
            interaction.sessionInteractionId
        , interactionDeliveryRequestKind = "tool_output"
        , interactionDeliveryRequestTurnIndex = 0
        , interactionDeliveryRequestTurnFingerprint = Nothing
        , interactionDeliveryRequestDeliveredAt = later
        }
        `shouldReturn` Right InteractionDeliveryUnresolved
    let intent = InteractionDeliveryIntent
            { interactionDeliveryIntentInteractionId =
                interaction.sessionInteractionId
            , interactionDeliveryIntentKind = "tool_output"
            , interactionDeliveryIntentTurnFingerprint = Nothing
            , interactionDeliveryIntentDeliveredAt = later
            }
    appendSessionTurnIndexedAndDeliver
        pool
        (testTurn later)
        metadata
        intent
        `shouldReturn`
            Right (Nothing, InteractionDeliveryUnresolved)
    loadSession pool "interaction-session" >>= \case
        Right (Just stored) ->
            toList stored.storedTurns `shouldBe` []
        other ->
            expectationFailure
                ("unexpected session after blocked atomic append: "
                    <> show other)

    let candidate index =
            InteractionResolutionRequest
                { interactionResolutionRequestSessionKey =
                    "interaction-session"
                , interactionResolutionRequestInteractionId =
                    interaction.sessionInteractionId
                , interactionResolutionRequestPayloadVersion = 1
                , interactionResolutionRequestPayload =
                    "{\"outcome\":\"approve\",\"client\":"
                        <> Text.pack (show index) <> "}"
                , interactionResolutionRequestResponder =
                    "client-" <> Text.pack (show index)
                , interactionResolutionRequestResolvedAt = later
                }
    results <-
        mapConcurrently
            (resolveSessionInteraction pool . candidate)
            ([1 .. 8] :: [Int])
    observations <- mapM expectResolution results
    length
        [ ()
        | InteractionResolveObserved
            { interactionResolveWon = True
            } <- observations
        ]
        `shouldBe` 1
    let winners =
            [ value
            | InteractionResolveObserved
                { interactionResolveValue = value
                } <- observations
            ]
    length (nub winners) `shouldBe` 1
    winner <- case winners of
        value : _ -> pure value
        [] -> expectationFailure "expected a winning resolution" >> fail "no winner"

    listOpenSessionInteractions pool "interaction-session"
        `shouldReturn` Right []
    listUndeliveredSessionInteractions pool "interaction-session" >>= \case
        Right [pending] ->
            pending.sessionInteractionResolution `shouldBe` Just winner
        other ->
            expectationFailure
                ("unexpected undelivered interactions: " <> show other)

    let deliveryRequest = InteractionDeliveryRequest
            { interactionDeliveryRequestSessionKey = "interaction-session"
            , interactionDeliveryRequestInteractionId =
                interaction.sessionInteractionId
            , interactionDeliveryRequestKind = "tool_output"
            , interactionDeliveryRequestTurnIndex = 0
            , interactionDeliveryRequestTurnFingerprint = Nothing
            , interactionDeliveryRequestDeliveredAt = later
            }
    markSessionInteractionDelivered pool deliveryRequest
        `shouldReturn` Right InteractionDeliveryTurnNotFound

    atomic <- appendSessionTurnIndexedAndDeliver
        pool
        (testTurn later)
        metadata
        intent
    delivered <- case atomic of
        Right (Just 0, result) -> pure (Right result)
        other -> do
            expectationFailure
                ("unexpected atomic append result: " <> show other)
            fail "expected atomic append and delivery"
    delivery <- expectDelivered True delivered
    delivery.interactionDeliveryTurnIndex `shouldBe` 0

    -- Simulate a caller that crashed after PostgreSQL committed but before it
    -- observed the return value.  Retrying sees the immutable delivery and
    -- must not append another turn.
    replayed <- appendSessionTurnIndexedAndDeliver
        pool
        (testTurn later)
        metadata
        intent
    repeatedDelivery <- case replayed of
        Right (Nothing, result) -> expectDelivered False (Right result)
        other -> do
            expectationFailure
                ("unexpected atomic replay result: " <> show other)
            fail "expected delivery-only replay"
    repeatedDelivery `shouldBe` delivery

    -- A replay batch is atomic only when every observed delivery matches the
    -- same candidate turn. One exact replay plus one stale delivery must not
    -- be reported as a shorter successful batch.
    secondPublished <- publishSessionInteraction pool
        request
            { interactionRequestKey = "plan:revision:8"
            , interactionRequestOrigin =
                Just InteractionOrigin
                    { interactionOriginToolName = "exit_plan_mode"
                    , interactionOriginCallId = "call-8"
                    }
            }
    second <- expectPublished True secondPublished
    resolveSessionInteraction pool
        InteractionResolutionRequest
            { interactionResolutionRequestSessionKey =
                "interaction-session"
            , interactionResolutionRequestInteractionId =
                second.sessionInteractionId
            , interactionResolutionRequestPayloadVersion = 1
            , interactionResolutionRequestPayload =
                "{\"outcome\":\"approve\"}"
            , interactionResolutionRequestResponder = "client-8"
            , interactionResolutionRequestResolvedAt = later
            }
        >>= expectResolution
        >> pure ()
    markSessionInteractionDelivered pool
        InteractionDeliveryRequest
            { interactionDeliveryRequestSessionKey =
                "interaction-session"
            , interactionDeliveryRequestInteractionId =
                second.sessionInteractionId
            , interactionDeliveryRequestKind = "tool_output"
            , interactionDeliveryRequestTurnIndex = 0
            , interactionDeliveryRequestTurnFingerprint =
                Just "different-candidate"
            , interactionDeliveryRequestDeliveredAt = later
            }
        >>= expectDelivered True
        >> pure ()
    let secondIntent =
            intent
                { interactionDeliveryIntentInteractionId =
                    second.sessionInteractionId
                }
    appendSessionTurnIndexedAndDeliverMany
        pool
        (testTurn later)
        metadata
        [intent, secondIntent]
        `shouldReturn`
            Left
                (StoreDataError
                    "interaction delivery batch contains an unresolved or partially replayed response")

    -- A stale in-memory intent must not suppress a later unrelated turn.
    let nextTurn =
            (testTurn later)
                { sessionTurnUserText = "different turn"
                }
    appendSessionTurnIndexedAndDeliver
        pool
        nextTurn
        metadata
        intent >>= \case
            Right (Just 1, result) ->
                expectDelivered False (Right result)
                    `shouldReturn` delivery
            other ->
                expectationFailure
                    ("unexpected stale-intent append result: "
                        <> show other)
    loadSession pool "interaction-session" >>= \case
        Right (Just stored) ->
            map
                (\storedTurn ->
                    storedTurn.storedTurn.sessionTurnUserText)
                (toList stored.storedTurns)
                `shouldBe` ["approved plan", "different turn"]
        other ->
            expectationFailure
                ("unexpected session after atomic replay: " <> show other)
    listUndeliveredSessionInteractions pool "interaction-session"
        `shouldReturn` Right []
    loadSessionInteraction
        pool
        "interaction-session"
        interaction.sessionInteractionId >>= \case
            Right (Just stored) -> do
                stored.sessionInteractionResolution `shouldBe` Just winner
                stored.sessionInteractionDelivery `shouldBe` Just delivery
            other ->
                expectationFailure
                    ("unexpected delivered interaction: " <> show other)

    loadSessionInteraction pool "interaction-session" "not-a-uuid"
        `shouldReturn`
            Left (StoreDataError "interaction id must be a UUID")

expectPublished
    :: Bool
    -> Either StoreError InteractionPublishResult
    -> IO SessionInteraction
expectPublished expectedInserted = \case
    Right InteractionPublishObserved
        { interactionPublishInserted
        , interactionPublishValue
        } -> do
            interactionPublishInserted `shouldBe` expectedInserted
            pure interactionPublishValue
    other -> do
        expectationFailure
            ("expected published interaction, got: " <> show other)
        fail "expected published interaction"

expectResolution
    :: Either StoreError InteractionResolveResult
    -> IO InteractionResolveResult
expectResolution = \case
    Right observed@InteractionResolveObserved{} -> pure observed
    other -> do
        expectationFailure
            ("expected observed interaction resolution, got: " <> show other)
        fail "expected observed interaction resolution"

expectDelivered
    :: Bool
    -> Either StoreError InteractionDeliveryResult
    -> IO InteractionDelivery
expectDelivered expectedInserted = \case
    Right InteractionDeliveryObserved
        { interactionDeliveryInserted
        , interactionDeliveryValue
        } -> do
            interactionDeliveryInserted `shouldBe` expectedInserted
            pure interactionDeliveryValue
    other -> do
        expectationFailure
            ("expected observed interaction delivery, got: " <> show other)
        fail "expected observed interaction delivery"

isImmutableConflict :: Either StoreError InteractionPublishResult -> Bool
isImmutableConflict = \case
    Left (StoreDataError message) ->
        "different immutable request data"
            `Text.isInfixOf` message
    _ -> False

withOpenStore
    :: ManagedPostgresConfig
    -> (StorePool -> IO a)
    -> IO a
withOpenStore config action =
    openStore config >>= \case
        Left err -> do
            expectationFailure ("could not open store: " <> show err)
            fail "could not open store"
        Right store ->
            action (trustedPool store) `finally` closeStore store

testMetadata :: UTCTime -> SessionMetadata
testMetadata now = SessionMetadata
    { sessionMetadataKey = "interaction-session"
    , sessionMetadataVersion = 1
    , sessionMetadataCreatedAt = now
    , sessionMetadataUpdatedAt = now
    , sessionMetadataProvider = "openai"
    , sessionMetadataConnection = "openai"
    , sessionMetadataModel = "gpt-test"
    , sessionMetadataTransportModel = Just "gpt-test"
    , sessionMetadataDialect = "openai"
    , sessionMetadataLegacyTarget = Nothing
    , sessionMetadataCwd = "/tmp/project"
    , sessionMetadataEffort = "medium"
    , sessionMetadataTitle = "interaction test"
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

testTurn :: UTCTime -> SessionTurn
testTurn now = SessionTurn
    { sessionTurnOccurredAt = now
    , sessionTurnUserText = "approved plan"
    , sessionTurnAssistantText = Just "continuing with implementation"
    , sessionTurnError = Nothing
    , sessionTurnResponseId = Nothing
    , sessionTurnEffect = TranscriptAppend
    , sessionTurnItems = []
    , sessionTurnUsage = Nothing
    }

shouldContainBytes :: ByteString.ByteString -> ByteString.ByteString -> Expectation
shouldContainBytes actual expected =
    actual `shouldSatisfy` ByteString.isInfixOf expected

shouldNotContainBytes
    :: ByteString.ByteString
    -> ByteString.ByteString
    -> Expectation
shouldNotContainBytes actual expected =
    actual `shouldSatisfy` (not . ByteString.isInfixOf expected)
