-- | Durable coordination for plan-mode prompts.
--
-- A request is persisted before the local prompt is shown.  The local prompt
-- then races a polling observer, and any local answer is submitted through the
-- store's first-answer-wins resolution operation.  Reusing a caller-supplied
-- request key resumes the same interaction after a restart.
module Agent.CLI.PendingInteraction
    ( PendingInteractionStore(..)
    , PendingInteractionCoordinator
    , PendingInteractionContext(..)
    , PendingInteractionLocal(..)
    , PendingInteractionOutcome(..)
    , PendingInteractionError(..)
    , PlanModeInteractionRequest(..)
    , PlanModeInteractionContextProvider
    , ExternalInteractionResponse(..)
    , canonicalizeExternalInteractionResponse
    , mkPendingInteractionCoordinator
    , postgresPendingInteractionCoordinator
    , coordinatePlanConfirmEnter
    , coordinatePlanDecision
    , coordinatePlanQuestion
    , wrapDurablePlanModeHooks
    , renderPendingInteractionError
    ) where

import Agent.Store.Postgres.Connection (StorePool)
import Agent.Store.Postgres.Interaction
    ( InteractionOrigin
    , InteractionPublishResult(..)
    , InteractionRequest(..)
    , InteractionResolution(..)
    , InteractionResolutionRequest(..)
    , InteractionResolveResult(..)
    , SessionInteraction(..)
    , loadSessionInteraction
    , publishSessionInteraction
    , resolveSessionInteraction
    )
import Agent.Store.Types (StoreError, renderStoreError)
import Agent.Tools.PlanMode (PlanDecision(..), PlanModeHooks(..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock (UTCTime, getCurrentTime)

-- | Minimal store surface used by the coordinator.  The PostgreSQL-backed
-- constructor is used in production; exposing the record also keeps the race
-- policy independently testable.
data PendingInteractionStore = PendingInteractionStore
    { pendingInteractionPublish
        :: !(InteractionRequest
            -> IO (Either StoreError InteractionPublishResult))
    , pendingInteractionLoad
        :: !(Text
            -> Text
            -> IO (Either StoreError (Maybe SessionInteraction)))
    , pendingInteractionResolve
        :: !(InteractionResolutionRequest
            -> IO (Either StoreError InteractionResolveResult))
    }

data PendingInteractionCoordinator = PendingInteractionCoordinator
    { coordinatorStore :: !PendingInteractionStore
    , coordinatorSessionKey :: !Text
    , coordinatorResponder :: !Text
    , coordinatorPollIntervalMicros :: !Int
    , coordinatorNow :: !(IO UTCTime)
    }

-- | Immutable identity supplied at the hook boundary.
--
-- The request key must be deterministic for a logical prompt (normally based
-- on its tool call id).  The origin must also remain stable when the key is
-- replayed because both are part of the immutable request.
data PendingInteractionContext = PendingInteractionContext
    { pendingInteractionRequestKey :: !Text
    , pendingInteractionOrigin :: !(Maybe InteractionOrigin)
    }
    deriving (Eq, Show)

-- | A local UI may answer or return control while intentionally leaving the
-- durable interaction open for another client.
data PendingInteractionLocal answer
    = PendingInteractionRespond !answer
    | PendingInteractionDefer
    deriving (Eq, Show)

data PendingInteractionOutcome answer
    = PendingInteractionResolved
        { pendingInteractionAnswer :: !answer
        , pendingInteractionResolution :: !InteractionResolution
        , pendingInteractionWonLocally :: !Bool
        }
    | PendingInteractionDeferred
        { pendingInteractionOpenRequest :: !SessionInteraction
        }
    deriving (Eq, Show)

data PendingInteractionError
    = PendingInteractionStoreError !StoreError
    | PendingInteractionSessionNotFound !Text
    | PendingInteractionRequestDisappeared !Text
    | PendingInteractionUnexpectedStoreValue !Text
    | PendingInteractionInvalidLocalAnswer !Text
    | PendingInteractionInvalidResolution !Text
    deriving (Eq, Show)

-- | Description given to the caller's key/context provider before publishing.
-- It deliberately contains the complete immutable prompt so callers can
-- correlate it with their current tool-call context.
data PlanModeInteractionRequest
    = PlanModeConfirmEnterRequest
        { planModeEnterReason :: !Text
        }
    | PlanModeDecisionRequest
        { planModePlanMarkdown :: !Text
        }
    | PlanModeQuestionRequest
        { planModeQuestion :: !Text
        , planModeQuestionOptions :: ![Text]
        }
    deriving (Eq, Show)

type PlanModeInteractionContextProvider =
    PlanModeInteractionRequest -> IO PendingInteractionContext

-- | A validated response supplied by a non-local client.  Deferral is
-- deliberately not stored as a resolution: the immutable request remains
-- open and can still be answered by any client.
data ExternalInteractionResponse
    = ExternalInteractionDefer
    | ExternalInteractionResolve !Text
    deriving (Eq, Show)

-- | Validate a human-friendly or canonical JSON response against the
-- immutable request before it is allowed into the first-answer-wins store.
-- This keeps a malformed early responder from permanently poisoning an
-- otherwise valid interaction.
canonicalizeExternalInteractionResponse
    :: SessionInteraction
    -> Text
    -> Either Text ExternalInteractionResponse
canonicalizeExternalInteractionResponse interaction raw
    | interaction.sessionInteractionPayloadVersion /= planPayloadVersion =
        Left "unsupported interaction request payload version"
    | Text.null stripped =
        Left "response must not be empty"
    | Text.toCaseFold stripped == "defer" =
        Right ExternalInteractionDefer
    | otherwise =
        case interaction.sessionInteractionKind of
            "plan_mode.confirm_enter" -> do
                ensureRequestType "plan_mode.confirm_enter"
                answer <-
                    decodeCanonical decodeConfirmAnswer
                        <|> parseConfirmText stripped
                pure
                    (ExternalInteractionResolve
                        (encodeConfirmAnswer answer))
            "plan_mode.decide_exit" -> do
                ensureRequestType "plan_mode.decide_exit"
                answer <-
                    decodeCanonical decodeDecisionAnswer
                        <|> parseDecisionText stripped
                pure
                    (ExternalInteractionResolve
                        (encodeDecisionAnswer answer))
            "plan_mode.ask_question" -> do
                ensureRequestType "plan_mode.ask_question"
                options <- requestOptions
                answer <-
                    decodeCanonical (decodeQuestionAnswer options)
                        <|> validateQuestionAnswer options (Just stripped)
                case Text.strip <$> answer of
                    Nothing ->
                        Left "question answer must not be null"
                    Just value | Text.null value ->
                        Left "question answer must not be empty"
                    _ -> do
                        payload <- encodeQuestionAnswer options answer
                        pure (ExternalInteractionResolve payload)
            other ->
                Left ("unsupported interaction kind: " <> other)
  where
    stripped = Text.strip raw
    decodeCanonical decoder =
        case Aeson.eitherDecodeStrict'
                (Text.encodeUtf8 stripped) :: Either String Aeson.Value of
            Left _ -> Left "response is not canonical JSON"
            Right _ -> decoder stripped
    ensureRequestType expected = do
        value <-
            case Aeson.eitherDecodeStrict'
                    (Text.encodeUtf8 interaction.sessionInteractionPayload) of
                Left err ->
                    Left
                        ("stored interaction request is invalid JSON: "
                            <> Text.pack err)
                Right decoded -> Right decoded
        case AesonTypes.parseEither
                (Aeson.withObject "interaction request"
                    (requirePayloadType expected))
                value of
            Left err ->
                Left
                    ("stored interaction request is invalid: "
                        <> Text.pack err)
            Right () -> Right ()

    requestOptions =
        decodeJson "plan-mode question request"
            (Aeson.withObject "plan-mode question request" \object -> do
                requirePayloadType "plan_mode.ask_question" object
                object .: "options")
            interaction.sessionInteractionPayload

    parseConfirmText value =
        case Text.toCaseFold value of
            "yes" -> Right True
            "y" -> Right True
            "enter" -> Right True
            "approve" -> Right True
            "no" -> Right False
            "n" -> Right False
            "decline" -> Right False
            "stay" -> Right False
            _ -> Left
                "enter response must be yes/enter, no/stay, defer, or canonical JSON"

    parseDecisionText value
        | folded `elem` ["approve", "approved"] =
            Right PlanApprove
        | folded `elem` ["abandon", "cancel"] =
            Right PlanCancel
        | Just feedback <- revisionFeedback value =
            Right (PlanRequestChanges feedback)
        | otherwise =
            Left
                "review response must be approve, revise <feedback>, abandon, defer, or canonical JSON"
      where
        folded = Text.toCaseFold value

    revisionFeedback value =
        firstNonBlank
            [ stripCommand "revise:" value
            , stripCommand "revise " value
            , stripCommand "request_changes:" value
            , stripCommand "request_changes " value
            ]

    stripCommand command value
        | command `Text.isPrefixOf` Text.toCaseFold value =
            Just (Text.drop (Text.length command) value)
        | otherwise = Nothing

    firstNonBlank =
        foldr
            (\candidate rest ->
                case Text.strip <$> candidate of
                    Just value | not (Text.null value) -> Just value
                    _ -> rest)
            Nothing

    Left _ <|> right = right
    left <|> _ = left

-- | Construct a coordinator with injectable store, clock, and poll interval.
-- Poll intervals below one millisecond are clamped to avoid a busy loop.
mkPendingInteractionCoordinator
    :: PendingInteractionStore
    -> Text
    -- ^ Session UUID.
    -> Text
    -- ^ Local responder identity.
    -> Int
    -- ^ Poll interval in microseconds.
    -> IO UTCTime
    -> PendingInteractionCoordinator
mkPendingInteractionCoordinator store sessionKey responder pollMicros now =
    PendingInteractionCoordinator
        { coordinatorStore = store
        , coordinatorSessionKey = sessionKey
        , coordinatorResponder = responder
        , coordinatorPollIntervalMicros = max 1000 pollMicros
        , coordinatorNow = now
        }

-- | Production coordinator using a 250ms polling interval.
postgresPendingInteractionCoordinator
    :: StorePool
    -> Text
    -- ^ Session UUID.
    -> Text
    -- ^ Local responder identity.
    -> PendingInteractionCoordinator
postgresPendingInteractionCoordinator pool sessionKey responder =
    mkPendingInteractionCoordinator
        PendingInteractionStore
            { pendingInteractionPublish = publishSessionInteraction pool
            , pendingInteractionLoad = loadSessionInteraction pool
            , pendingInteractionResolve = resolveSessionInteraction pool
            }
        sessionKey
        responder
        250000
        getCurrentTime

coordinatePlanConfirmEnter
    :: PendingInteractionCoordinator
    -> PendingInteractionContext
    -> Text
    -> IO (PendingInteractionLocal Bool)
    -> IO (Either PendingInteractionError (PendingInteractionOutcome Bool))
coordinatePlanConfirmEnter coordinator context reason =
    coordinatePlanInteraction
        coordinator
        context
        (PlanModeConfirmEnterRequest reason)
        (Right . encodeConfirmAnswer)
        decodeConfirmAnswer

coordinatePlanDecision
    :: PendingInteractionCoordinator
    -> PendingInteractionContext
    -> Text
    -> IO (PendingInteractionLocal PlanDecision)
    -> IO
        (Either
            PendingInteractionError
            (PendingInteractionOutcome PlanDecision))
coordinatePlanDecision coordinator context planMarkdown =
    coordinatePlanInteraction
        coordinator
        context
        (PlanModeDecisionRequest planMarkdown)
        (Right . encodeDecisionAnswer)
        decodeDecisionAnswer

coordinatePlanQuestion
    :: PendingInteractionCoordinator
    -> PendingInteractionContext
    -> Text
    -> [Text]
    -> IO (PendingInteractionLocal (Maybe Text))
    -> IO
        (Either
            PendingInteractionError
            (PendingInteractionOutcome (Maybe Text)))
coordinatePlanQuestion coordinator context question options =
    coordinatePlanInteraction
        coordinator
        context
        (PlanModeQuestionRequest question options)
        (encodeQuestionAnswer options)
        (decodeQuestionAnswer options)

-- | Adapt existing plan-mode hooks without changing their core shape.
--
-- The context provider is called once per hook invocation and should return a
-- deterministic key, preferably derived from the current tool call id.  Store
-- and payload failures invoke @onFailure@ and return the conservative value:
-- decline enter, cancel exit, or no question answer.  A local @Nothing@ from
-- the question hook is treated as deferral, so the request remains open.
wrapDurablePlanModeHooks
    :: PendingInteractionCoordinator
    -> PlanModeInteractionContextProvider
    -> (PendingInteractionError -> IO ())
    -> PlanModeHooks
    -> PlanModeHooks
wrapDurablePlanModeHooks coordinator provideContext onFailure localHooks =
    PlanModeHooks
        { planConfirmEnter = \reason -> do
            let request = PlanModeConfirmEnterRequest reason
            context <- provideContext request
            closeOnFailure False onFailure $
                coordinatePlanConfirmEnter coordinator context reason
                    (PendingInteractionRespond
                        <$> localHooks.planConfirmEnter reason)
        , planDecideExit = \planMarkdown -> do
            let request = PlanModeDecisionRequest planMarkdown
            context <- provideContext request
            closeOnFailure PlanCancel onFailure $
                coordinatePlanDecision coordinator context planMarkdown
                    (PendingInteractionRespond
                        <$> localHooks.planDecideExit planMarkdown)
        , planAskQuestion = \question options -> do
            let request = PlanModeQuestionRequest question options
            context <- provideContext request
            closeOnFailure Nothing onFailure $
                coordinatePlanQuestion coordinator context question options do
                    localHooks.planAskQuestion question options >>= \case
                        Nothing -> pure PendingInteractionDefer
                        Just answer ->
                            pure
                                (PendingInteractionRespond (Just answer))
        }

renderPendingInteractionError :: PendingInteractionError -> Text
renderPendingInteractionError = \case
    PendingInteractionStoreError err ->
        "pending interaction store failed: " <> renderStoreError err
    PendingInteractionSessionNotFound sessionKey ->
        "pending interaction session is unavailable: " <> sessionKey
    PendingInteractionRequestDisappeared interactionId ->
        "pending interaction disappeared: " <> interactionId
    PendingInteractionUnexpectedStoreValue message ->
        "pending interaction store returned inconsistent data: " <> message
    PendingInteractionInvalidLocalAnswer message ->
        "invalid local pending interaction answer: " <> message
    PendingInteractionInvalidResolution message ->
        "invalid pending interaction resolution: " <> message

data PlanResponseCodec answer = PlanResponseCodec
    { responseEncode :: !(answer -> Either Text Text)
    , responseDecode :: !(Text -> Either Text answer)
    }

coordinatePlanInteraction
    :: PendingInteractionCoordinator
    -> PendingInteractionContext
    -> PlanModeInteractionRequest
    -> (answer -> Either Text Text)
    -> (Text -> Either Text answer)
    -> IO (PendingInteractionLocal answer)
    -> IO
        (Either
            PendingInteractionError
            (PendingInteractionOutcome answer))
coordinatePlanInteraction
    coordinator
    context
    request
    encodeAnswer
    decodeAnswer
    local = do
        createdAt <- coordinator.coordinatorNow
        let codec = PlanResponseCodec encodeAnswer decodeAnswer
            interactionRequest = InteractionRequest
                { interactionRequestSessionKey =
                    coordinator.coordinatorSessionKey
                , interactionRequestKey =
                    context.pendingInteractionRequestKey
                , interactionRequestKind = planRequestKind request
                , interactionRequestPayloadVersion = planPayloadVersion
                , interactionRequestPayload = encodePlanRequest request
                , interactionRequestOrigin =
                    context.pendingInteractionOrigin
                , interactionRequestCreatedAt = createdAt
                }
        coordinator.coordinatorStore.pendingInteractionPublish
            interactionRequest
            >>= \case
                Left err ->
                    pure (Left (PendingInteractionStoreError err))
                Right InteractionPublishSessionNotFound ->
                    pure
                        (Left
                            (PendingInteractionSessionNotFound
                                coordinator.coordinatorSessionKey))
                Right InteractionPublishObserved
                    { interactionPublishValue = interaction } ->
                        case validatePublishedInteraction
                            interactionRequest
                            interaction
                        of
                            Left err -> pure (Left err)
                            Right () ->
                                resumePublishedInteraction
                                    coordinator
                                    codec
                                    interaction
                                    local

resumePublishedInteraction
    :: PendingInteractionCoordinator
    -> PlanResponseCodec answer
    -> SessionInteraction
    -> IO (PendingInteractionLocal answer)
    -> IO
        (Either
            PendingInteractionError
            (PendingInteractionOutcome answer))
resumePublishedInteraction coordinator codec interaction local =
    case interaction.sessionInteractionResolution of
        Just resolution ->
            pure
                (resolvedOutcome
                    codec
                    interaction.sessionInteractionId
                    False
                    resolution)
        Nothing -> do
            race
                local
                (pollForResolution
                    coordinator
                    interaction)
                >>= \case
                    Left PendingInteractionDefer ->
                        pure
                            (Right
                                PendingInteractionDeferred
                                    { pendingInteractionOpenRequest =
                                        interaction
                                    })
                    Left (PendingInteractionRespond answer) ->
                        resolveLocalAnswer
                            coordinator
                            codec
                            interaction
                            answer
                    Right (Left err) -> pure (Left err)
                    Right (Right resolution) ->
                        pure
                            (resolvedOutcome
                                codec
                                interaction.sessionInteractionId
                                False
                                resolution)

resolveLocalAnswer
    :: PendingInteractionCoordinator
    -> PlanResponseCodec answer
    -> SessionInteraction
    -> answer
    -> IO
        (Either
            PendingInteractionError
            (PendingInteractionOutcome answer))
resolveLocalAnswer coordinator codec interaction answer = do
    case codec.responseEncode answer of
        Left err ->
            pure (Left (PendingInteractionInvalidLocalAnswer err))
        Right payload -> do
            resolvedAt <- coordinator.coordinatorNow
            let resolutionRequest = InteractionResolutionRequest
                    { interactionResolutionRequestSessionKey =
                        coordinator.coordinatorSessionKey
                    , interactionResolutionRequestInteractionId =
                        interaction.sessionInteractionId
                    , interactionResolutionRequestPayloadVersion =
                        planPayloadVersion
                    , interactionResolutionRequestPayload = payload
                    , interactionResolutionRequestResponder =
                        coordinator.coordinatorResponder
                    , interactionResolutionRequestResolvedAt = resolvedAt
                    }
            coordinator.coordinatorStore.pendingInteractionResolve
                resolutionRequest
                >>= \case
                    Left err ->
                        pure (Left (PendingInteractionStoreError err))
                    Right InteractionResolveNotFound ->
                        pure
                            (Left
                                (PendingInteractionRequestDisappeared
                                    interaction.sessionInteractionId))
                    Right InteractionResolveObserved
                        { interactionResolveWon = won
                        , interactionResolveValue = resolution
                        } ->
                            pure
                                (resolvedOutcome
                                    codec
                                    interaction.sessionInteractionId
                                    won
                                    resolution)

pollForResolution
    :: PendingInteractionCoordinator
    -> SessionInteraction
    -> IO (Either PendingInteractionError InteractionResolution)
pollForResolution coordinator expected = do
    threadDelay coordinator.coordinatorPollIntervalMicros
    coordinator.coordinatorStore.pendingInteractionLoad
        coordinator.coordinatorSessionKey
        expected.sessionInteractionId
        >>= \case
            Left err ->
                pure (Left (PendingInteractionStoreError err))
            Right Nothing ->
                pure
                    (Left
                        (PendingInteractionRequestDisappeared
                            expected.sessionInteractionId))
            Right (Just interaction) ->
                case validateReloadedInteraction expected interaction of
                    Left err -> pure (Left err)
                    Right () ->
                        case interaction.sessionInteractionResolution of
                            Nothing ->
                                pollForResolution coordinator expected
                            Just resolution -> pure (Right resolution)

validatePublishedInteraction
    :: InteractionRequest
    -> SessionInteraction
    -> Either PendingInteractionError ()
validatePublishedInteraction request interaction
    | ( request.interactionRequestSessionKey
      , request.interactionRequestKey
      , request.interactionRequestKind
      , request.interactionRequestPayloadVersion
      , request.interactionRequestPayload
      , request.interactionRequestOrigin
      )
        == ( interaction.sessionInteractionSessionKey
           , interaction.sessionInteractionRequestKey
           , interaction.sessionInteractionKind
           , interaction.sessionInteractionPayloadVersion
           , interaction.sessionInteractionPayload
           , interaction.sessionInteractionOrigin
           ) =
        Right ()
    | otherwise =
        Left
            (PendingInteractionUnexpectedStoreValue
                "published request does not match the requested immutable data")

validateReloadedInteraction
    :: SessionInteraction
    -> SessionInteraction
    -> Either PendingInteractionError ()
validateReloadedInteraction expected actual
    | ( expected.sessionInteractionId
      , expected.sessionInteractionSessionKey
      , expected.sessionInteractionRequestKey
      , expected.sessionInteractionKind
      , expected.sessionInteractionPayloadVersion
      , expected.sessionInteractionPayload
      , expected.sessionInteractionOrigin
      , expected.sessionInteractionCreatedAt
      )
        == ( actual.sessionInteractionId
           , actual.sessionInteractionSessionKey
           , actual.sessionInteractionRequestKey
           , actual.sessionInteractionKind
           , actual.sessionInteractionPayloadVersion
           , actual.sessionInteractionPayload
           , actual.sessionInteractionOrigin
           , actual.sessionInteractionCreatedAt
           ) =
        Right ()
    | otherwise =
        Left
            (PendingInteractionUnexpectedStoreValue
                "reloaded request changed immutable interaction data")

resolvedOutcome
    :: PlanResponseCodec answer
    -> Text
    -> Bool
    -> InteractionResolution
    -> Either
        PendingInteractionError
        (PendingInteractionOutcome answer)
resolvedOutcome codec interactionId wonLocally resolution
    | resolution.interactionResolutionInteractionId /= interactionId =
        Left
            (PendingInteractionInvalidResolution
                "resolution interaction id does not match its request")
    | resolution.interactionResolutionPayloadVersion /= planPayloadVersion =
        Left
            (PendingInteractionInvalidResolution
                ("unsupported payload version "
                    <> Text.pack
                        (show
                            resolution.interactionResolutionPayloadVersion)))
    | otherwise =
        case codec.responseDecode resolution.interactionResolutionPayload of
            Left err -> Left (PendingInteractionInvalidResolution err)
            Right answer ->
                Right
                    PendingInteractionResolved
                        { pendingInteractionAnswer = answer
                        , pendingInteractionResolution = resolution
                        , pendingInteractionWonLocally = wonLocally
                        }

closeOnFailure
    :: answer
    -> (PendingInteractionError -> IO ())
    -> IO
        (Either
            PendingInteractionError
            (PendingInteractionOutcome answer))
    -> IO answer
closeOnFailure fallback onFailure action =
    action >>= \case
        Left err -> onFailure err >> pure fallback
        Right PendingInteractionDeferred{} -> pure fallback
        Right PendingInteractionResolved
            { pendingInteractionAnswer = answer } ->
                pure answer

planPayloadVersion :: Int32
planPayloadVersion = 1

planRequestKind :: PlanModeInteractionRequest -> Text
planRequestKind = \case
    PlanModeConfirmEnterRequest{} -> "plan_mode.confirm_enter"
    PlanModeDecisionRequest{} -> "plan_mode.decide_exit"
    PlanModeQuestionRequest{} -> "plan_mode.ask_question"

encodePlanRequest :: PlanModeInteractionRequest -> Text
encodePlanRequest = \case
    PlanModeConfirmEnterRequest reason ->
        encodeJson
            (Aeson.object
                [ "type" .= ("plan_mode.confirm_enter" :: Text)
                , "reason" .= reason
                ])
    PlanModeDecisionRequest planMarkdown ->
        encodeJson
            (Aeson.object
                [ "type" .= ("plan_mode.decide_exit" :: Text)
                , "plan_markdown" .= planMarkdown
                ])
    PlanModeQuestionRequest question options ->
        encodeJson
            (Aeson.object
                [ "type" .= ("plan_mode.ask_question" :: Text)
                , "question" .= question
                , "options" .= options
                ])

encodeConfirmAnswer :: Bool -> Text
encodeConfirmAnswer confirmed =
    encodeJson
        (Aeson.object
            [ "type" .= ("plan_mode.confirm_enter" :: Text)
            , "confirmed" .= confirmed
            ])

decodeConfirmAnswer :: Text -> Either Text Bool
decodeConfirmAnswer =
    decodeJson "plan-mode enter response" $
        Aeson.withObject "plan-mode enter response" \object -> do
            requirePayloadType "plan_mode.confirm_enter" object
            object .: "confirmed"

encodeDecisionAnswer :: PlanDecision -> Text
encodeDecisionAnswer decision =
    encodeJson
        (Aeson.object
            ([ "type" .= ("plan_mode.decide_exit" :: Text)
             , "decision" .= decisionName decision
             ]
                <> case decision of
                    PlanRequestChanges feedback ->
                        ["feedback" .= feedback]
                    PlanApprove -> []
                    PlanCancel -> []))

decodeDecisionAnswer :: Text -> Either Text PlanDecision
decodeDecisionAnswer =
    decodeJson "plan-mode decision response" $
        Aeson.withObject "plan-mode decision response" \object -> do
            requirePayloadType "plan_mode.decide_exit" object
            decision <- object .: "decision"
            case (decision :: Text) of
                "approve" -> pure PlanApprove
                "request_changes" ->
                    PlanRequestChanges
                        <$> object .:? "feedback" Aeson..!= ""
                "cancel" -> pure PlanCancel
                other ->
                    fail
                        ("unknown plan-mode decision: "
                            <> Text.unpack other)

decisionName :: PlanDecision -> Text
decisionName = \case
    PlanApprove -> "approve"
    PlanRequestChanges _ -> "request_changes"
    PlanCancel -> "cancel"

encodeQuestionAnswer :: [Text] -> Maybe Text -> Either Text Text
encodeQuestionAnswer options answer = do
    validated <- validateQuestionAnswer options answer
    pure
        (encodeJson
            (Aeson.object
                [ "type" .= ("plan_mode.ask_question" :: Text)
                , "answer" .= validated
                ]))

decodeQuestionAnswer :: [Text] -> Text -> Either Text (Maybe Text)
decodeQuestionAnswer options payload = do
    answer <- decodeJson "plan-mode question response" (
        Aeson.withObject "plan-mode question response" \object -> do
            requirePayloadType "plan_mode.ask_question" object
            object .: "answer") payload
    validateQuestionAnswer options answer

validateQuestionAnswer :: [Text] -> Maybe Text -> Either Text (Maybe Text)
validateQuestionAnswer _ Nothing =
    Left "question answers must be non-null; defer instead"
validateQuestionAnswer options answer@(Just text)
    | Text.null (Text.strip text) =
        Left "question answers must not be blank"
    | not (null options) && text `notElem` options =
        Left "question answer is not one of the offered options"
    | otherwise = Right answer

requirePayloadType :: Text -> Aeson.Object -> AesonTypes.Parser ()
requirePayloadType expected object = do
    actual <- object .: "type"
    if actual == expected
        then pure ()
        else
            fail
                ("expected payload type "
                    <> Text.unpack expected
                    <> ", received "
                    <> Text.unpack actual)

encodeJson :: Aeson.Value -> Text
encodeJson =
    Text.decodeUtf8
        . LazyByteString.toStrict
        . Aeson.encode

decodeJson
    :: Text
    -> (Aeson.Value -> AesonTypes.Parser answer)
    -> Text
    -> Either Text answer
decodeJson label parser payload = do
    value <-
        case Aeson.eitherDecodeStrict' (Text.encodeUtf8 payload) of
            Left err ->
                Left
                    (label <> " is not valid JSON: " <> Text.pack err)
            Right decoded -> Right decoded
    case AesonTypes.parseEither parser value of
        Left err -> Left (label <> " is invalid: " <> Text.pack err)
        Right answer -> Right answer
