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
    , DurableInteractionDelivery(..)
    , PlanModeInteractionRequest(..)
    , PlanModeInteractionContextProvider
    , deterministicPlanModeInteractionContext
    , ExternalInteractionResponse(..)
    , canonicalizeExternalInteractionResponse
    , mkPendingInteractionCoordinator
    , postgresPendingInteractionCoordinator
    , postgresPendingInteractionCoordinatorDynamic
    , withPendingInteractionResolution
    , recordDurableInteractionDelivery
    , recoverUndeliveredInteractions
    , renderRestoredInteractions
    , resolvedPlanEnterDecision
    , coordinatePlanConfirmEnter
    , coordinatePlanDecision
    , coordinatePlanReview
    , coordinatePlanQuestion
    , coordinatePlanQuestionnaire
    , wrapDurablePlanModeHooks
    , renderPendingInteractionError
    ) where

import Agent.Store.Postgres.Connection (StorePool)
import Agent.Store.Postgres.Interaction
    ( InteractionDeliveryIntent(..)
    , InteractionOrigin(..)
    , InteractionPublishResult(..)
    , InteractionRequest(..)
    , InteractionResolution(..)
    , InteractionResolutionRequest(..)
    , InteractionResolveResult(..)
    , SessionInteraction(..)
    , loadSessionInteraction
    , listUndeliveredSessionInteractions
    , publishSessionInteraction
    , resolveSessionInteraction
    )
import Agent.Store.Types (StoreError, renderStoreError)
import Agent.OsPath (toText)
import Agent.Tools.PlanMode
    ( AskUserQuestion(..)
    , AskUserQuestionOption(..)
    , PlanEnterRequest(..)
    , PlanDecision(..)
    , PlanModeHooks(..)
    , PlanQuestionnaireAnswer(..)
    , PlanQuestionnaireDecision(..)
    , PlanQuestionnaireRequest(..)
    , PlanReviewDecision(..)
    , PlanReviewRequest(..)
    , legacyPlanQuestionnaireHook
    , validatePlanQuestionnaireDecision
    )
import Agent.Tools.PlanMode.Document
    ( PlanValidationWarning(..) )
import Agent.Tools.PlanMode.File (PlanDigest(..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Monad (filterM, void)
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Int (Int32)
import Data.IORef (IORef, atomicModifyIORef')
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock (UTCTime, getCurrentTime)
import Crypto.Hash (Digest, SHA256, hash)

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
    , coordinatorSessionKey :: !(IO Text)
    , coordinatorResponder :: !Text
    , coordinatorPollIntervalMicros :: !Int
    , coordinatorNow :: !(IO UTCTime)
    , coordinatorOnResolved
        :: !(SessionInteraction -> InteractionResolution -> IO ())
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

data DurableInteractionDelivery = DurableInteractionDelivery
    { durableDeliverySessionKey :: !Text
    , durableDeliveryInteractionKind :: !Text
    , durableDeliveryIntent :: !InteractionDeliveryIntent
    } deriving (Eq, Show)

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
        { planModeEnterRequest :: !PlanEnterRequest
        }
    | PlanModeDecisionRequest
        { planModePlanMarkdown :: !Text
        }
    | PlanModeReviewRequest
        { planModeReview :: !PlanReviewRequest
        }
    | PlanModeQuestionRequest
        { planModeQuestion :: !Text
        , planModeQuestionOptions :: ![Text]
        }
    | PlanModeQuestionnaireRequest
        { planModeQuestionnaire :: !PlanQuestionnaireRequest
        }
    deriving (Eq, Show)

type PlanModeInteractionContextProvider =
    PlanModeInteractionRequest -> IO PendingInteractionContext

-- | Stable fallback correlation when the host cannot expose a provider tool
-- call id. Reviews retain the core generation+digest key; other requests hash
-- their complete immutable payload. Hosts with a real call id should prefer
-- supplying it directly.
deterministicPlanModeInteractionContext
    :: PlanModeInteractionRequest
    -> PendingInteractionContext
deterministicPlanModeInteractionContext request =
    PendingInteractionContext
        { pendingInteractionRequestKey = requestKey
        , pendingInteractionOrigin =
            Just InteractionOrigin
                { interactionOriginToolName = planRequestToolName request
                , interactionOriginCallId = originCallId
                }
        }
  where
    (requestKey, originCallId) = case request of
        PlanModeReviewRequest review ->
            (review.planReviewRequestKey, review.planReviewRequestKey)
        PlanModeQuestionnaireRequest questionnaire ->
            ( "ask_user_question:"
                <> questionnaire.planQuestionnaireRequestKey
            , questionnaire.planQuestionnaireRequestKey
            )
        PlanModeConfirmEnterRequest enterRequest ->
            ( "enter_plan_mode:"
                <> enterRequest.planEnterRequestKey
            , enterRequest.planEnterRequestKey
            )
        _ ->
            let key =
                    planRequestKind request
                        <> ":"
                        <> Text.pack
                            (show
                                (hash
                                    (Text.encodeUtf8
                                        (encodePlanRequest request))
                                    :: Digest SHA256))
            in (key, key)

planRequestToolName :: PlanModeInteractionRequest -> Text
planRequestToolName = \case
    PlanModeConfirmEnterRequest{} -> "enter_plan_mode"
    PlanModeDecisionRequest{} -> "exit_plan_mode"
    PlanModeReviewRequest{} -> "exit_plan_mode"
    PlanModeQuestionRequest{} -> "ask_user_question"
    PlanModeQuestionnaireRequest{} -> "ask_user_question"

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
            "plan_mode.review" -> do
                ensureRequestType "plan_mode.review"
                hasWarnings <- requestReviewHasWarnings
                answer <-
                    decodeCanonical decodeReviewAnswer
                        <|> parseReviewText stripped
                case answer of
                    PlanReviewApprove
                        | hasWarnings ->
                            Left
                                "this plan has advisory warnings; respond with approve_anyway to acknowledge them"
                    PlanReviewDefer ->
                        Right ExternalInteractionDefer
                    _ ->
                        ExternalInteractionResolve
                            <$> encodeReviewAnswer answer
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
            "plan_mode.questionnaire" -> do
                ensureRequestType "plan_mode.questionnaire"
                questions <- requestQuestionnaire
                answer <-
                    decodeCanonical
                        (decodeQuestionnaireAnswer questions)
                        <|> parseQuestionnaireText stripped
                case answer of
                    PlanQuestionnaireDeferred ->
                        Right ExternalInteractionDefer
                    _ ->
                        ExternalInteractionResolve
                            <$> encodeQuestionnaireAnswer questions answer
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

    requestQuestionnaire =
        decodeJson "plan-mode questionnaire request"
            (Aeson.withObject "plan-mode questionnaire request" \object -> do
                requirePayloadType "plan_mode.questionnaire" object
                requestId <- object .: "request_id"
                values <- object .: "questions"
                questions <- traverse parseStoredQuestion values
                pure PlanQuestionnaireRequest
                    { planQuestionnaireRequestKey = requestId
                    , planQuestionnaireQuestions = questions
                    })
            interaction.sessionInteractionPayload

    requestReviewHasWarnings =
        decodeJson "plan-mode review request"
            (Aeson.withObject "plan-mode review request" \object -> do
                requirePayloadType "plan_mode.review" object
                warnings <- object .: "warnings" :: AesonTypes.Parser [Aeson.Value]
                pure (not (null warnings)))
            interaction.sessionInteractionPayload

    parseStoredQuestion =
        Aeson.withObject "questionnaire question" \object ->
            AskUserQuestion
                <$> object .: "question"
                <*> (object .: "options" >>= traverse parseStoredOption)
                <*> object .:? "multi_select"
    parseStoredOption =
        Aeson.withObject "questionnaire option" \object ->
            AskUserQuestionOption
                <$> object .: "label"
                <*> object .: "description"
                <*> object .:? "preview"

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

    parseQuestionnaireText value
        | Text.toCaseFold value `elem` ["cancel", "abandon"] =
            Right PlanQuestionnaireCancelled
        | Text.toCaseFold value `elem` ["finish", "done"] =
            Right PlanQuestionnaireFinished
        | Just clarification <- firstNonBlank
            [ stripCommand "clarify:" value
            , stripCommand "clarify " value
            ] =
                Right (PlanQuestionnaireClarification clarification)
        | otherwise =
            Left
                "questionnaire response must be canonical JSON, clarify <text>, finish, cancel, or defer"

    parseReviewText value
        | folded `elem` ["approve", "approved"] =
            Right PlanReviewApprove
        | folded `elem` ["approve_anyway", "approve anyway"] =
            Right PlanReviewApproveAnyway
        | folded `elem` ["abandon", "cancel"] =
            Right PlanReviewAbandon
        | folded == "defer" =
            Right PlanReviewDefer
        | Just feedback <- revisionFeedback value =
            Right (PlanReviewRequestChanges feedback)
        | otherwise =
            Left
                "review response must be approve, approve_anyway, revise <feedback>, abandon, or defer"
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
        , coordinatorSessionKey = pure sessionKey
        , coordinatorResponder = responder
        , coordinatorPollIntervalMicros = max 1000 pollMicros
        , coordinatorNow = now
        , coordinatorOnResolved = \_ _ -> pure ()
        }

withPendingInteractionResolution
    :: (SessionInteraction -> InteractionResolution -> IO ())
    -> PendingInteractionCoordinator
    -> PendingInteractionCoordinator
withPendingInteractionResolution callback coordinator =
    coordinator { coordinatorOnResolved = callback }

-- | Remember a newly observed durable response until the model turn carrying
-- that response commits it atomically. Already-delivered rows are deliberately
-- ignored, and duplicate observations remain idempotent.
recordDurableInteractionDelivery
    :: IORef [DurableInteractionDelivery]
    -> SessionInteraction
    -> InteractionResolution
    -> IO ()
recordDurableInteractionDelivery ref interaction resolution =
    void
        (recordDurableInteractionDeliveryIfNew
            ref
            interaction
            resolution)

recordDurableInteractionDeliveryIfNew
    :: IORef [DurableInteractionDelivery]
    -> SessionInteraction
    -> InteractionResolution
    -> IO Bool
recordDurableInteractionDeliveryIfNew ref interaction resolution
    | isNothing interaction.sessionInteractionDelivery =
        atomicModifyIORef' ref \pending ->
            let
                interactionId = interaction.sessionInteractionId
                intent = InteractionDeliveryIntent
                    { interactionDeliveryIntentInteractionId =
                        interactionId
                    , interactionDeliveryIntentKind = "tool_output"
                    , interactionDeliveryIntentTurnFingerprint = Nothing
                    , interactionDeliveryIntentDeliveredAt =
                        resolution.interactionResolutionResolvedAt
                    }
                delivery = DurableInteractionDelivery
                    { durableDeliverySessionKey =
                        interaction.sessionInteractionSessionKey
                    , durableDeliveryInteractionKind =
                        interaction.sessionInteractionKind
                    , durableDeliveryIntent = intent
                    }
                alreadyRecorded candidate =
                    candidate.durableDeliveryIntent.interactionDeliveryIntentInteractionId
                        == interactionId
            in if any alreadyRecorded pending
                then (pending, False)
                else (pending <> [delivery], True)
    | otherwise = pure False

-- | Load validated, resolved interactions that have not yet been attached to
-- a durable model turn. Reviews have their own tracker-correlated replay path;
-- all other responses are queued exactly once for the next turn that includes
-- the returned canonical context.
recoverUndeliveredInteractions
    :: StorePool
    -> Text
    -> IORef [DurableInteractionDelivery]
    -> IO (Either PendingInteractionError [SessionInteraction])
recoverUndeliveredInteractions pool sessionKey ref =
    listUndeliveredSessionInteractions pool sessionKey >>= \case
        Left err -> pure (Left (PendingInteractionStoreError err))
        Right interactions -> do
            let candidates =
                    filter
                        ((/= "plan_mode.review")
                            . (.sessionInteractionKind))
                        interactions
            case traverse validateCanonicalResolution candidates of
                Left err ->
                    pure (Left (PendingInteractionInvalidResolution err))
                Right _ ->
                    Right <$> filterM queueResolution candidates
  where
    queueResolution interaction =
        case interaction.sessionInteractionResolution of
            Nothing -> pure False
            Just resolution ->
                recordDurableInteractionDeliveryIfNew
                    ref
                    interaction
                    resolution

    validateCanonicalResolution interaction =
        case interaction.sessionInteractionResolution of
            Nothing -> Left "undelivered interaction has no resolution"
            Just resolution ->
                case
                    canonicalizeExternalInteractionResponse
                        interaction
                        resolution.interactionResolutionPayload
                of
                    Right (ExternalInteractionResolve canonical)
                        | canonical
                            == resolution.interactionResolutionPayload ->
                                Right ()
                    Right ExternalInteractionDefer ->
                        Left "stored interaction resolution decoded as a deferral"
                    _ -> Left "stored interaction resolution is not canonical"

renderRestoredInteractions :: [SessionInteraction] -> Text
renderRestoredInteractions interactions =
    Text.unlines
        ( "Durable user interaction responses arrived while the previous turn was interrupted. Resume from these canonical responses; do not ask the same questions again."
        : map render interactions
        )
  where
    render interaction =
        "- "
            <> interaction.sessionInteractionKind
            <> " ("
            <> interaction.sessionInteractionRequestKey
            <> ")\n  request: "
            <> interaction.sessionInteractionPayload
            <> "\n  response: "
            <> maybe
                "(missing resolution)"
                (.interactionResolutionPayload)
                interaction.sessionInteractionResolution

resolvedPlanEnterDecision
    :: SessionInteraction
    -> Either Text (Maybe Bool)
resolvedPlanEnterDecision interaction
    | interaction.sessionInteractionKind
        /= "plan_mode.confirm_enter" =
            Left "interaction is not an enter-plan confirmation"
    | otherwise =
        traverse
            (decodeConfirmAnswer
                . (.interactionResolutionPayload))
            interaction.sessionInteractionResolution

-- | Production coordinator using a 250ms polling interval.
postgresPendingInteractionCoordinator
    :: StorePool
    -> Text
    -- ^ Session UUID.
    -> Text
    -- ^ Local responder identity.
    -> PendingInteractionCoordinator
postgresPendingInteractionCoordinator pool sessionKey responder =
    postgresPendingInteractionCoordinatorDynamic
        pool
        (pure sessionKey)
        responder

postgresPendingInteractionCoordinatorDynamic
    :: StorePool
    -> IO Text
    -- ^ Resolve the current session UUID for each new interaction.
    -> Text
    -- ^ Local responder identity.
    -> PendingInteractionCoordinator
postgresPendingInteractionCoordinatorDynamic pool sessionKey responder =
    (mkPendingInteractionCoordinator
        PendingInteractionStore
            { pendingInteractionPublish = publishSessionInteraction pool
            , pendingInteractionLoad = loadSessionInteraction pool
            , pendingInteractionResolve = resolveSessionInteraction pool
            }
        ""
        responder
        250000
        getCurrentTime)
            { coordinatorSessionKey = sessionKey }

coordinatePlanConfirmEnter
    :: PendingInteractionCoordinator
    -> PendingInteractionContext
    -> PlanEnterRequest
    -> IO (PendingInteractionLocal Bool)
    -> IO (Either PendingInteractionError (PendingInteractionOutcome Bool))
coordinatePlanConfirmEnter coordinator context request =
    coordinatePlanInteraction
        coordinator
        context
        (PlanModeConfirmEnterRequest request)
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

coordinatePlanReview
    :: PendingInteractionCoordinator
    -> PendingInteractionContext
    -> PlanReviewRequest
    -> IO (PendingInteractionLocal PlanReviewDecision)
    -> IO
        (Either
            PendingInteractionError
            (PendingInteractionOutcome PlanReviewDecision))
coordinatePlanReview coordinator context request =
    coordinatePlanInteraction
        coordinator
        context
        (PlanModeReviewRequest request)
        (encodeReviewAnswerFor request)
        decodeReviewAnswer

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

coordinatePlanQuestionnaire
    :: PendingInteractionCoordinator
    -> PendingInteractionContext
    -> PlanQuestionnaireRequest
    -> IO (PendingInteractionLocal PlanQuestionnaireDecision)
    -> IO
        (Either
            PendingInteractionError
            (PendingInteractionOutcome PlanQuestionnaireDecision))
coordinatePlanQuestionnaire coordinator context request =
    coordinatePlanInteraction
        coordinator
        context
        (PlanModeQuestionnaireRequest request)
        (encodeQuestionnaireAnswer request)
        (decodeQuestionnaireAnswer request)

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
    PlanModeLifecycleHooks
        { planConfirmEnter = legacyEnter
        , planConfirmEnterRequest = durableEnter
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
        , planAskQuestionnaire = \questionnaire -> do
            let request = PlanModeQuestionnaireRequest questionnaire
            supplied <- provideContext request
            let context = supplied
                    { pendingInteractionRequestKey =
                        "ask_user_question:"
                            <> questionnaire.planQuestionnaireRequestKey
                    , pendingInteractionOrigin =
                        Just InteractionOrigin
                            { interactionOriginToolName =
                                "ask_user_question"
                            , interactionOriginCallId =
                                questionnaire.planQuestionnaireRequestKey
                            }
                    }
            closeOnFailure PlanQuestionnaireDeferred onFailure $
                coordinatePlanQuestionnaire
                    coordinator
                    context
                    questionnaire
                    do
                        localPlanQuestionnaire
                            localHooks
                            questionnaire
                            >>= \case
                                PlanQuestionnaireDeferred ->
                                    pure PendingInteractionDefer
                                decision ->
                                    pure
                                        (PendingInteractionRespond decision)
        , planReviewPlan = \request -> do
            let interactionRequest = PlanModeReviewRequest request
            supplied <- provideContext interactionRequest
            let context = supplied
                    { pendingInteractionRequestKey =
                        request.planReviewRequestKey
                    }
            closeOnFailure PlanReviewDefer onFailure $
                coordinatePlanReview coordinator context request do
                    localPlanReview localHooks request >>= \case
                        PlanReviewDefer -> pure PendingInteractionDefer
                        decision ->
                            pure (PendingInteractionRespond decision)
        , planQuiesceBeforeActivation =
            case localHooks of
                PlanModeHooks{} -> pure (Right ())
                PlanModeLifecycleHooks{planQuiesceBeforeActivation} ->
                    planQuiesceBeforeActivation
        , planResumeAfterExit =
            case localHooks of
                PlanModeHooks{} -> pure ()
                PlanModeLifecycleHooks{planResumeAfterExit} ->
                    planResumeAfterExit
        }
  where
    legacyEnter reason =
        durableEnter PlanEnterRequest
            { planEnterRequestKey =
                "legacy:"
                    <> Text.pack
                        (show
                            (hash (Text.encodeUtf8 reason)
                                :: Digest SHA256))
            , planEnterReason = reason
            }

    durableEnter enterRequest = do
        let request = PlanModeConfirmEnterRequest enterRequest
        supplied <- provideContext request
        let context = supplied
                { pendingInteractionRequestKey =
                    "enter_plan_mode:" <> enterRequest.planEnterRequestKey
                , pendingInteractionOrigin =
                    Just InteractionOrigin
                        { interactionOriginToolName = "enter_plan_mode"
                        , interactionOriginCallId =
                            enterRequest.planEnterRequestKey
                        }
                }
        closeOnFailure False onFailure $
            coordinatePlanConfirmEnter coordinator context enterRequest
                (PendingInteractionRespond
                    <$> localPlanConfirmEnter localHooks enterRequest)

localPlanConfirmEnter :: PlanModeHooks -> PlanEnterRequest -> IO Bool
localPlanConfirmEnter hooks request =
    case hooks of
        PlanModeHooks{planConfirmEnter} ->
            planConfirmEnter request.planEnterReason
        PlanModeLifecycleHooks{planConfirmEnterRequest} ->
            planConfirmEnterRequest request

localPlanReview
    :: PlanModeHooks
    -> PlanReviewRequest
    -> IO PlanReviewDecision
localPlanReview hooks request =
    case hooks of
        PlanModeHooks{planDecideExit} ->
            case planDecideExit of
                decide -> decide request.planReviewMarkdown >>= \case
                    PlanApprove -> pure PlanReviewApprove
                    PlanRequestChanges notes ->
                        pure (PlanReviewRequestChanges notes)
                    PlanCancel -> pure PlanReviewAbandon
        PlanModeLifecycleHooks{planReviewPlan} ->
            planReviewPlan request

localPlanQuestionnaire
    :: PlanModeHooks
    -> PlanQuestionnaireRequest
    -> IO PlanQuestionnaireDecision
localPlanQuestionnaire hooks request =
    case hooks of
        PlanModeHooks{planAskQuestion} ->
            legacyPlanQuestionnaireHook planAskQuestion request
        PlanModeLifecycleHooks{planAskQuestionnaire} ->
            planAskQuestionnaire request

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
        sessionKey <- coordinator.coordinatorSessionKey
        createdAt <- coordinator.coordinatorNow
        let codec = PlanResponseCodec encodeAnswer decodeAnswer
            interactionRequest = InteractionRequest
                { interactionRequestSessionKey =
                    sessionKey
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
                                sessionKey))
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
            observeResolution
                coordinator codec interaction False resolution
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
                        observeResolution
                            coordinator codec interaction False resolution

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
                        interaction.sessionInteractionSessionKey
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
                            observeResolution
                                coordinator codec interaction won resolution

pollForResolution
    :: PendingInteractionCoordinator
    -> SessionInteraction
    -> IO (Either PendingInteractionError InteractionResolution)
pollForResolution coordinator expected = do
    threadDelay coordinator.coordinatorPollIntervalMicros
    coordinator.coordinatorStore.pendingInteractionLoad
        expected.sessionInteractionSessionKey
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

observeResolution
    :: PendingInteractionCoordinator
    -> PlanResponseCodec answer
    -> SessionInteraction
    -> Bool
    -> InteractionResolution
    -> IO
        (Either
            PendingInteractionError
            (PendingInteractionOutcome answer))
observeResolution coordinator codec interaction wonLocally resolution =
    case
        resolvedOutcome
            codec
            interaction.sessionInteractionId
            wonLocally
            resolution
    of
        Left err -> pure (Left err)
        Right outcome -> do
            coordinator.coordinatorOnResolved interaction resolution
            pure (Right outcome)

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
    PlanModeReviewRequest{} -> "plan_mode.review"
    PlanModeQuestionRequest{} -> "plan_mode.ask_question"
    PlanModeQuestionnaireRequest{} -> "plan_mode.questionnaire"

encodePlanRequest :: PlanModeInteractionRequest -> Text
encodePlanRequest = \case
    PlanModeConfirmEnterRequest request ->
        encodeJson
            (Aeson.object
                [ "type" .= ("plan_mode.confirm_enter" :: Text)
                , "request_id" .= request.planEnterRequestKey
                , "reason" .= request.planEnterReason
                ])
    PlanModeDecisionRequest planMarkdown ->
        encodeJson
            (Aeson.object
                [ "type" .= ("plan_mode.decide_exit" :: Text)
                , "plan_markdown" .= planMarkdown
                ])
    PlanModeReviewRequest request ->
        encodeJson
            (Aeson.object
                [ "type" .= ("plan_mode.review" :: Text)
                , "request_key" .= request.planReviewRequestKey
                , "path" .= toText request.planReviewPath
                , "digest" .=
                    request.planReviewSnapshotDigest.unPlanDigest
                , "plan_markdown" .= request.planReviewMarkdown
                , "warnings" .=
                    [ Aeson.object
                        [ "code" .=
                            Text.pack (show warning.planWarningCode)
                        , "message" .= warning.planWarningMessage
                        , "line" .= warning.planWarningLine
                        ]
                    | warning <- request.planReviewWarnings
                    ]
                , "verification" .= request.planReviewVerification
                ])
    PlanModeQuestionRequest question options ->
        encodeJson
            (Aeson.object
                [ "type" .= ("plan_mode.ask_question" :: Text)
                , "question" .= question
                , "options" .= options
                ])
    PlanModeQuestionnaireRequest request ->
        encodeJson
            (Aeson.object
                [ "type" .= ("plan_mode.questionnaire" :: Text)
                , "request_id" .=
                    request.planQuestionnaireRequestKey
                , "questions" .=
                    map
                        encodeQuestionnaireQuestion
                        request.planQuestionnaireQuestions
                ])

encodeQuestionnaireQuestion :: AskUserQuestion -> Aeson.Value
encodeQuestionnaireQuestion question =
    Aeson.object
        [ "question" .= question.question
        , "options" .= map encodeQuestionnaireOption question.options
        , "multi_select" .= (question.multiSelect == Just True)
        ]

encodeQuestionnaireOption :: AskUserQuestionOption -> Aeson.Value
encodeQuestionnaireOption option =
    Aeson.object
        [ "label" .= option.label
        , "description" .= option.description
        , "preview" .= option.preview
        ]

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

encodeReviewAnswer :: PlanReviewDecision -> Either Text Text
encodeReviewAnswer = \case
    PlanReviewDefer ->
        Left "defer must leave the durable interaction unresolved"
    decision ->
        Right $
            encodeJson
                (Aeson.object
                    ([ "type" .= ("plan_mode.review" :: Text)
                     , "decision" .= reviewDecisionName decision
                     ]
                        <> case decision of
                            PlanReviewRequestChanges feedback ->
                                ["feedback" .= feedback]
                            _ -> []))

encodeReviewAnswerFor
    :: PlanReviewRequest
    -> PlanReviewDecision
    -> Either Text Text
encodeReviewAnswerFor request decision
    | decision == PlanReviewApprove
    , not (null request.planReviewWarnings) =
        Left
            "plans with advisory warnings require an explicit approve-anyway decision"
    | otherwise = encodeReviewAnswer decision

decodeReviewAnswer :: Text -> Either Text PlanReviewDecision
decodeReviewAnswer =
    decodeJson "plan-mode review response" $
        Aeson.withObject "plan-mode review response" \object -> do
            requirePayloadType "plan_mode.review" object
            decision <- object .: "decision"
            case (decision :: Text) of
                "approve" -> pure PlanReviewApprove
                "approve_anyway" -> pure PlanReviewApproveAnyway
                "revise" ->
                    PlanReviewRequestChanges
                        <$> object .:? "feedback" Aeson..!= ""
                "abandon" -> pure PlanReviewAbandon
                "defer" -> pure PlanReviewDefer
                other ->
                    fail
                        ("unknown plan-mode review decision: "
                            <> Text.unpack other)

reviewDecisionName :: PlanReviewDecision -> Text
reviewDecisionName = \case
    PlanReviewApprove -> "approve"
    PlanReviewApproveAnyway -> "approve_anyway"
    PlanReviewRequestChanges _ -> "revise"
    PlanReviewAbandon -> "abandon"
    PlanReviewDefer -> "defer"

encodeQuestionnaireAnswer
    :: PlanQuestionnaireRequest
    -> PlanQuestionnaireDecision
    -> Either Text Text
encodeQuestionnaireAnswer request decision = do
    validated <- validatePlanQuestionnaireDecision request decision
    case validated of
        PlanQuestionnaireDeferred ->
            Left "defer must leave the durable interaction unresolved"
        _ ->
            pure $
                encodeJson
                    (Aeson.object
                        ([ "type" .= ("plan_mode.questionnaire" :: Text)
                         , "decision" .= questionnaireDecisionName validated
                         ]
                            <> questionnaireDecisionFields validated))

questionnaireDecisionFields
    :: PlanQuestionnaireDecision
    -> [AesonTypes.Pair]
questionnaireDecisionFields = \case
    PlanQuestionnaireSubmitted answers ->
        ["answers" .= map encodeAnswer answers]
    PlanQuestionnaireClarification clarification ->
        ["clarification" .= clarification]
    _ -> []
  where
    encodeAnswer answer =
        Aeson.object
            [ "question_index" .= answer.planAnswerQuestionIndex
            , "question" .= answer.planAnswerQuestion
            , "labels" .= answer.planAnswerLabels
            , "other" .= answer.planAnswerOther
            ]

decodeQuestionnaireAnswer
    :: PlanQuestionnaireRequest
    -> Text
    -> Either Text PlanQuestionnaireDecision
decodeQuestionnaireAnswer request payload = do
    decision <-
        decodeJson "plan-mode questionnaire response"
            (Aeson.withObject "plan-mode questionnaire response" \object -> do
                requirePayloadType "plan_mode.questionnaire" object
                choice <- object .: "decision"
                case (choice :: Text) of
                    "submitted" ->
                        PlanQuestionnaireSubmitted
                            <$> (object .: "answers"
                                >>= traverse parseAnswer)
                    "clarification" ->
                        PlanQuestionnaireClarification
                            <$> object .: "clarification"
                    "finished" -> pure PlanQuestionnaireFinished
                    "cancelled" -> pure PlanQuestionnaireCancelled
                    "defer" -> pure PlanQuestionnaireDeferred
                    other ->
                        fail
                            ("unknown questionnaire decision: "
                                <> Text.unpack other))
            payload
    validatePlanQuestionnaireDecision request decision
  where
    parseAnswer =
        Aeson.withObject "questionnaire answer" \object ->
            PlanQuestionnaireAnswer
                <$> object .: "question_index"
                <*> object .: "question"
                <*> object .: "labels"
                <*> object .:? "other"

questionnaireDecisionName :: PlanQuestionnaireDecision -> Text
questionnaireDecisionName = \case
    PlanQuestionnaireSubmitted _ -> "submitted"
    PlanQuestionnaireClarification _ -> "clarification"
    PlanQuestionnaireFinished -> "finished"
    PlanQuestionnaireCancelled -> "cancelled"
    PlanQuestionnaireDeferred -> "defer"

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
