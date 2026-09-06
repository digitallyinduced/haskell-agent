-- | Versioned WAI REST and SSE application.
module Agent.Server.Application
    ( ApplicationConfig(..)
    , newApplication
    ) where

import Agent.Server.Auth
    ( AuthConfig
    , AuthFailure(..)
    , AuthenticatedRequest(..)
    , authorizePreflight
    , authorizeRequest
    , corsResponseHeaders
    , isCorsPreflight
    )
import Agent.Loop (ImageAttachment(..))
import Agent.Server.Backend (Backend(..), SessionMutationLease(..))
import Agent.Server.Identifier (isUUIDText, newUUIDv7Text)
import Agent.Server.Supervisor
    ( CheckedSubmitError(..)
    , EventSubscriptionError(..)
    , HumanRequestResolutionError(..)
    , SubmitError(..)
    , SessionMutationError(..)
    , Supervisor
    , TurnPersistence(..)
    , cancelTurn
    , listHumanRequests
    , listTurns
    , lookupTurn
    , lookupTurnAgents
    , resolveHumanRequest
    , submitReservedTurnChecked
    , subscribeEvents
    , trySubmitReservedTurnChecked
    , withSessionCleanup
    , withSessionMutation
    )
import Agent.Server.Types
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM
    ( TBQueue
    , atomically
    , readTBQueue
    )
import Control.Exception.Safe
    ( finally
    , mask
    , onException
    , tryAny
    )
import Control.Monad (foldM, void)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson
    ( FromJSON
    , Value
    , eitherDecodeStrict'
    , encode
    , object
    , (.=)
    )
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder
    ( Builder
    , byteString
    , integerDec
    , lazyByteString
    )
import Data.ByteString.Char8 qualified as ByteString8
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Time.Clock (UTCTime, getCurrentTime)
import Network.HTTP.Types
    ( Header
    , Status
    , hCacheControl
    , hContentType
    , mkStatus
    , status200
    , status201
    , status202
    , status204
    )
import Network.Wai
    ( Application
    , getRequestBodyChunk
    , Request
    , RequestBodyLength(..)
    , Response
    , pathInfo
    , queryString
    , requestBodyLength
    , requestHeaders
    , requestMethod
    , responseLBS
    , responseStream
    )
import System.Timeout (timeout)
import Text.Read (readMaybe)

data ApplicationConfig = ApplicationConfig
    { applicationMaximumRequestBytes :: !Int
    , applicationOpenApiDocument :: !LazyByteString.ByteString
    }

newApplication
    :: ApplicationConfig
    -> AuthConfig
    -> Backend
    -> Supervisor
    -> IO Application
newApplication config auth backend supervisor =
    pure \request respond -> do
        requestId <- newUUIDv7Text
        let corsHeaders = corsResponseHeaders auth request
        if isCorsPreflight request
            then
                case authorizePreflight auth request of
                    Left failure ->
                        respond
                            (authFailureResponse
                                requestId corsHeaders failure)
                    Right preflightHeaders ->
                        respond $
                            responseLBS
                                status204
                                (responseHeaders
                                    requestId preflightHeaders [])
                                ""
            else
                case authorizeRequest auth request of
                    Left failure ->
                        respond
                            (authFailureResponse
                                requestId corsHeaders failure)
                    Right authenticated -> do
                        outcome <-
                            tryAny
                                (routeRequest
                                    config
                                    backend
                                    supervisor
                                    requestId
                                    authenticated.authenticatedCorsHeaders
                                    authenticated.authenticatedPrincipal
                                    request)
                        respond case outcome of
                            Left _ ->
                                apiErrorResponse
                                    requestId
                                    authenticated.authenticatedCorsHeaders
                                    ApiError
                                        { apiErrorStatus = 500
                                        , apiErrorCode = "internal_error"
                                        , apiErrorMessage =
                                            "the request could not be completed"
                                        , apiErrorDetails = Nothing
                                        }
                            Right response -> response

routeRequest
    :: ApplicationConfig
    -> Backend
    -> Supervisor
    -> Text
    -> [Header]
    -> Principal
    -> Request
    -> IO Response
routeRequest
        config backend supervisor requestId corsHeaders principal request =
    case (request.requestMethod, request.pathInfo) of
        ("GET", ["healthz"]) ->
            pure (jsonResponse status200 headers (object ["ok" .= True]))
        ("GET", ["readyz"]) ->
            backend.backendCheckReady >>= \case
                Left err -> pure (apiErrorResponse requestId corsHeaders err)
                Right () ->
                    pure
                        (jsonResponse
                            status200
                            headers
                            (object ["ready" .= True]))
        ("GET", ["openapi.json"]) ->
            pure $
                responseLBS
                    status200
                    (responseHeaders
                        requestId
                        corsHeaders
                        [(hContentType, "application/json")])
                    config.applicationOpenApiDocument
        _ ->
            admitBoundary
                requestId corsHeaders backend principal \boundary ->
                dispatchBoundary
                    config
                    backend
                    supervisor
                    requestId
                    corsHeaders
                    request
                    boundary >>= \case
                        Left err ->
                            pure (apiErrorResponse requestId corsHeaders err)
                        Right response -> pure response
  where
    headers = responseHeaders requestId corsHeaders []

admitBoundary
    :: Text
    -> [Header]
    -> Backend
    -> Principal
    -> (AccessBoundary -> IO Response)
    -> IO Response
admitBoundary requestId corsHeaders backend principal action = do
    let Backend { backendAdmitBoundary = admit } = backend
    admit principal action >>= \case
        Left err -> pure (apiErrorResponse requestId corsHeaders err)
        Right response -> pure response

dispatchBoundary
    :: ApplicationConfig
    -> Backend
    -> Supervisor
    -> Text
    -> [Header]
    -> Request
    -> AccessBoundary
    -> IO (Either ApiError Response)
dispatchBoundary
        config backend supervisor requestId corsHeaders request boundary =
    case (request.requestMethod, request.pathInfo) of
        ("GET", ["v1", "models"]) ->
            fmap
                (fmap (jsonResponse status200 headers))
                (backend.backendListModels boundary)
        ("GET", ["v1", "sessions"]) ->
            listSessionsResponse backend boundary headers request
        ("POST", ["v1", "sessions"]) ->
            createSessionResponse config backend boundary headers request
        ("GET", ["v1", "sessions", sessionId]) ->
            fmap
                (fmap (jsonResponse status200 headers))
                (backend.backendGetSession boundary sessionId)
        ("PATCH", ["v1", "sessions", sessionId]) ->
            patchSessionResponse
                config backend supervisor boundary sessionId headers request
        ("DELETE", ["v1", "sessions", sessionId]) ->
            deleteSessionResponse
                backend supervisor boundary sessionId headers
        ("GET", ["v1", "sessions", sessionId, "history"]) ->
            sessionHistoryResponse backend boundary sessionId headers request
        ("POST", ["v1", "sessions", sessionId, "fork"]) ->
            forkSessionResponse
                config backend supervisor boundary sessionId headers request
        ("POST", ["v1", "sessions", sessionId, "turns"]) ->
            createTurnResponse
                config backend supervisor boundary sessionId headers request
        ("GET", ["v1", "turns"]) ->
            listTurnsResponse backend supervisor boundary headers request
        ("GET", ["v1", "turns", rawTurnId]) ->
            findTurn backend supervisor boundary rawTurnId >>= pure
                . fmap (jsonResponse status200 headers . toJSONValue)
        ("GET", ["v1", "turns", rawTurnId, "result"]) ->
            findTurnResult backend boundary rawTurnId >>= pure
                . fmap (jsonResponse status200 headers . toJSONValue)
        ("POST", ["v1", "turns", rawTurnId, "cancel"]) ->
            cancelTurnResponse backend supervisor boundary rawTurnId headers
        ("GET", ["v1", "turns", rawTurnId, "agents"]) ->
            turnAgentsResponse backend supervisor boundary rawTurnId headers
        ("GET", ["v1", "requests"]) ->
            humanRequestsResponse supervisor boundary headers request
        ("POST", ["v1", "requests", rawRequestId, "resolve"]) ->
            resolveHumanRequestResponse
                config supervisor boundary rawRequestId headers request
        ("GET", ["v1", "events"]) ->
            createEventResponse
                backend
                supervisor
                boundary
                requestId
                corsHeaders
                request
        _ -> pure (Left routeNotFound)
  where
    headers = responseHeaders requestId corsHeaders []

listSessionsResponse
    :: Backend
    -> AccessBoundary
    -> [Header]
    -> Request
    -> IO (Either ApiError Response)
listSessionsResponse backend boundary headers request = do
    let archive = queryArchiveFilter request
        cursor = queryOptionalText "cursor" request
        limit = queryLimit "limit" 50 100 request
    case (archive, cursor, limit) of
        (Right archiveFilter, Right pageCursor, Right pageLimit) ->
            fmap
                (fmap (jsonResponse status200 headers))
                (backend.backendListSessions
                    boundary
                    archiveFilter
                    pageCursor
                    pageLimit)
        _ -> pure $
            Left $
                firstQueryError
                    [ () <$ archive
                    , () <$ cursor
                    , () <$ limit
                    ]

createSessionResponse
    :: ApplicationConfig
    -> Backend
    -> AccessBoundary
    -> [Header]
    -> Request
    -> IO (Either ApiError Response)
createSessionResponse config backend boundary headers request =
    withJsonBody config request \body ->
        fmap
            (fmap (jsonResponse status201 headers))
            (backend.backendCreateSession boundary body)

patchSessionResponse
    :: ApplicationConfig
    -> Backend
    -> Supervisor
    -> AccessBoundary
    -> Text
    -> [Header]
    -> Request
    -> IO (Either ApiError Response)
patchSessionResponse
        config backend supervisor boundary sessionId headers request =
    withJsonBody config request \(body :: PatchSessionRequest) ->
        if body.patchSessionTitle == Nothing
            && body.patchSessionArchived == Nothing
            then
                pure
                    (Left ApiError
                        { apiErrorStatus = 400
                        , apiErrorCode =
                            "empty_patch"
                        , apiErrorMessage =
                            "at least one patch field is required"
                        , apiErrorDetails = Nothing
                        })
            else if body.patchSessionTitle /= Nothing
                && body.patchSessionArchived /= Nothing
                then
                    pure
                        (Left ApiError
                            { apiErrorStatus = 422
                            , apiErrorCode =
                                "non_atomic_patch"
                            , apiErrorMessage =
                                "title and archived must be patched in separate requests"
                            , apiErrorDetails = Nothing
                            })
                else
                    runSessionMutation
                        backend
                        supervisor
                        boundary
                        sessionId
                        (fmap
                            (fmap
                                (jsonResponse
                                    status200
                                    headers))
                            (backend.backendPatchSession
                                boundary
                                sessionId
                                body))

deleteSessionResponse
    :: Backend
    -> Supervisor
    -> AccessBoundary
    -> Text
    -> [Header]
    -> IO (Either ApiError Response)
deleteSessionResponse backend supervisor boundary sessionId headers =
    runSessionMutation
        backend
        supervisor
        boundary
        sessionId
        (do
            backend.backendDeleteSession
                boundary sessionId >>= \case
                    Left err -> pure (Left err)
                    Right () ->
                        pure $
                            Right $
                                responseLBS
                                    status204
                                    headers
                                    "")

sessionHistoryResponse
    :: Backend
    -> AccessBoundary
    -> Text
    -> [Header]
    -> Request
    -> IO (Either ApiError Response)
sessionHistoryResponse backend boundary sessionId headers request = do
    let before =
            queryOptionalInteger "cursor" request >>= \case
                Nothing ->
                    queryOptionalInteger "before" request
                value -> Right value
        limit = queryLimit "limit" 50 100 request
    case (before, limit) of
        (Right cursor, Right pageLimit) ->
            fmap
                (fmap (jsonResponse status200 headers))
                (backend.backendSessionHistory
                    boundary
                    sessionId
                    cursor
                    pageLimit)
        _ -> pure $
            Left $
                firstQueryError
                    [() <$ before, () <$ limit]

forkSessionResponse
    :: ApplicationConfig
    -> Backend
    -> Supervisor
    -> AccessBoundary
    -> Text
    -> [Header]
    -> Request
    -> IO (Either ApiError Response)
forkSessionResponse
        config backend supervisor boundary sessionId headers request =
    withJsonBody config request \body ->
        runSessionMutation
            backend
            supervisor
            boundary
            sessionId
            (do
                fmap
                    (fmap (jsonResponse status201 headers))
                    (backend.backendForkSession
                        boundary
                        sessionId
                        body))

createTurnResponse
    :: ApplicationConfig
    -> Backend
    -> Supervisor
    -> AccessBoundary
    -> Text
    -> [Header]
    -> Request
    -> IO (Either ApiError Response)
createTurnResponse
        config backend supervisor boundary sessionId headers request =
    withJsonBody config request \body ->
        createTurn backend supervisor boundary sessionId body
            >>= pure
                . fmap (jsonResponse status202 headers . toJSONValue)

listTurnsResponse
    :: Backend
    -> Supervisor
    -> AccessBoundary
    -> [Header]
    -> Request
    -> IO (Either ApiError Response)
listTurnsResponse backend supervisor boundary headers request =
    case queryOptionalText "sessionId" request of
        Left err -> pure (Left err)
        Right sessionId ->
            listKnownTurns
                backend
                supervisor
                boundary
                sessionId >>= pure
                    . fmap
                        (\turns ->
                            jsonResponse
                                status200
                                headers
                                (object ["data" .= turns]))

cancelTurnResponse
    :: Backend
    -> Supervisor
    -> AccessBoundary
    -> Text
    -> [Header]
    -> IO (Either ApiError Response)
cancelTurnResponse backend supervisor boundary rawTurnId headers =
    cancelKnownTurn
        backend
        supervisor
        boundary
        rawTurnId >>= pure
            . fmap
                (\(responseStatus, turn) ->
                    jsonResponse
                        responseStatus
                        headers
                        (toJSONValue turn))

turnAgentsResponse
    :: Backend
    -> Supervisor
    -> AccessBoundary
    -> Text
    -> [Header]
    -> IO (Either ApiError Response)
turnAgentsResponse backend supervisor boundary rawTurnId headers =
    case canonicalTurnId rawTurnId of
        Nothing -> pure (Left turnNotFound)
        Just turnId ->
            lookupTurnAgents supervisor boundary turnId >>= \case
                Nothing ->
                    backend.backendLookupTurn boundary turnId >>= \case
                        Left err -> pure (Left err)
                        Right Nothing -> pure (Left turnNotFound)
                        Right (Just _) ->
                            pure (Left turnAgentsUnavailable)
                Just agents ->
                    pure $
                        Right $
                            jsonResponse
                                status200
                                headers
                                (object ["data" .= agents])

humanRequestsResponse
    :: Supervisor
    -> AccessBoundary
    -> [Header]
    -> Request
    -> IO (Either ApiError Response)
humanRequestsResponse supervisor boundary headers request =
    case queryOptionalTurnId request of
        Left err -> pure (Left err)
        Right turnId ->
            listHumanRequests supervisor boundary turnId >>= \case
                Left message ->
                    pure $
                        Left
                            ApiError
                                { apiErrorStatus = 503
                                , apiErrorCode = "store_unavailable"
                                , apiErrorMessage = message
                                , apiErrorDetails = Nothing
                                }
                Right requests ->
                    pure $
                        Right $
                            jsonResponse
                                status200
                                headers
                                (object ["data" .= requests])

resolveHumanRequestResponse
    :: ApplicationConfig
    -> Supervisor
    -> AccessBoundary
    -> Text
    -> [Header]
    -> Request
    -> IO (Either ApiError Response)
resolveHumanRequestResponse
        config supervisor boundary rawRequestId headers request =
    if not (isUUIDText rawRequestId)
        then pure (Left humanRequestNotFound)
        else
            withJsonBody config request \(body :: ResolveRequest) -> do
                let response = HumanResponse
                        { humanResponseDecision =
                            body.resolveRequestDecision
                        , humanResponseValue =
                            body.resolveRequestValue
                        }
                resolveHumanRequest
                    supervisor
                    boundary
                    (RequestId (Text.toLower rawRequestId))
                    response >>= \case
                        Left HumanRequestResolutionNotFound ->
                            pure (Left humanRequestNotFound)
                        Left
                            (HumanRequestResolutionStoreUnavailable message) ->
                            pure $
                                Left ApiError
                                    { apiErrorStatus = 503
                                    , apiErrorCode = "store_unavailable"
                                    , apiErrorMessage = message
                                    , apiErrorDetails = Nothing
                                    }
                        Left (HumanRequestResolutionConflict message) ->
                            pure $
                                Left ApiError
                                    { apiErrorStatus = 409
                                    , apiErrorCode = "request_not_resolved"
                                    , apiErrorMessage = message
                                    , apiErrorDetails = Nothing
                                    }
                        Right resolved ->
                            pure $
                                Right $
                                    jsonResponse
                                        status200
                                        headers
                                        (toJSONValue resolved)

turnReservationInput :: CreateTurnRequest -> Text
turnReservationInput request
    | null request.createTurnImages && null request.createTurnFiles =
        request.createTurnInput
    | null request.createTurnFiles =
        "image-turn-v1:"
            <> Text.pack (show (hash imagePayload :: Digest SHA256))
    | otherwise =
        "attachment-turn-v1:"
            <> Text.pack (show (hash payload :: Digest SHA256))
  where
    promptBytes = TextEncoding.encodeUtf8 request.createTurnInput
    imagePayload =
        lengthPrefix promptBytes
            <> promptBytes
            <> foldMap encodeImage request.createTurnImages
    payload =
        imagePayload
            <> foldMap encodeFile request.createTurnFiles
    encodeImage image =
        let mimeBytes = TextEncoding.encodeUtf8 image.imageMime
         in lengthPrefix mimeBytes
                <> mimeBytes
                <> lengthPrefix image.imageBytes
                <> image.imageBytes
    encodeFile file =
        let nameBytes = TextEncoding.encodeUtf8 file.fileName
            mimeBytes = TextEncoding.encodeUtf8 file.fileMime
         in lengthPrefix nameBytes
                <> nameBytes
                <> lengthPrefix mimeBytes
                <> mimeBytes
                <> lengthPrefix file.fileBytes
                <> file.fileBytes
    lengthPrefix bytes =
        ByteString8.pack (show (ByteString.length bytes) <> ":")

createTurn
    :: Backend
    -> Supervisor
    -> AccessBoundary
    -> Text
    -> CreateTurnRequest
    -> IO (Either ApiError TurnRecord)
createTurn backend supervisor boundary sessionId request
    | Text.null (Text.strip request.createTurnInput)
        && null request.createTurnImages
        && null request.createTurnFiles =
        pure $
            Left ApiError
                { apiErrorStatus = 422
                , apiErrorCode = "empty_input"
                , apiErrorMessage = "turn input or an attachment is required"
                , apiErrorDetails = Nothing
                }
    | otherwise = do
        now <- getCurrentTime
        turnId <- TurnId <$> newUUIDv7Text
        let clientRequestId =
                fromMaybe
                    (ClientRequestId turnId.unTurnId)
                    request.createTurnClientRequestId
            spec = TurnSpec
                { turnSpecSessionId = sessionId
                , turnSpecClientRequestId = clientRequestId
                , turnSpecPrompt = request.createTurnInput
                , turnSpecImages = request.createTurnImages
                , turnSpecFiles = request.createTurnFiles
                , turnSpecBoundary = boundary
                }
            validateSession =
                fmap (fmap (const ())) $
                    backend.backendGetSession boundary sessionId
        validateSession >>= \case
            Left err -> pure (Left err)
            Right () ->
                mask \restore -> do
                    reservation <-
                        restore $
                            backend.backendReserveTurn
                                boundary
                                sessionId
                                clientRequestId
                                (turnReservationInput request)
                                turnId
                                now
                    case reservation of
                        Left err -> pure (Left err)
                        Right (TurnReservationExisting existing) ->
                            pure (Right existing)
                        Right (TurnReservationExistingOwned existing)
                            | existing.turnRecordStatus == TurnQueued ->
                                restore $
                                    lookupTurn
                                        supervisor
                                        boundary
                                        existing.turnRecordId
                                        >>= \case
                                            Just admitted ->
                                                pure (Right admitted)
                                            Nothing ->
                                                admitOwnedReservation
                                                    existing
                                                    spec
                                                    validateSession
                            | otherwise ->
                                pure (Right existing)
                        Right (TurnReservationCreated reserved) ->
                            restore
                                ( admitCreatedReservation
                                    reserved
                                    spec
                                    validateSession
                                )
                                `onException`
                                    abandonCreatedReservation reserved
  where
    admitOwnedReservation reserved spec validateSession =
        trySubmitReservedTurnChecked
            supervisor
            spec
            reserved
            validateSession
            >>= \case
                Left (SubmitValidationRejected SubmitSessionBusy) ->
                    lookupTurn
                        supervisor
                        boundary
                        reserved.turnRecordId
                        >>= pure . Right . fromMaybe reserved
                result -> completeAdmission reserved result

    admitCreatedReservation reserved spec validateSession =
        submitReservedTurnChecked
            supervisor
            spec
            reserved
            validateSession
            >>= completeAdmission reserved

    abandonCreatedReservation reserved =
        void $
            withSessionCleanup
                supervisor
                reserved.turnRecordBoundary
                reserved.turnRecordSessionId
                ( lookupTurn
                    supervisor
                    boundary
                    reserved.turnRecordId
                    >>= \case
                        Just _ -> pure ()
                        Nothing -> do
                            cancelledAt <- getCurrentTime
                            void $
                                persistTurnTerminalEventually
                                    backend
                                    reserved
                                    cancelledAt
                                    TurnWasCancelled
                )

    completeAdmission reserved result =
        case firstCheckedSubmitError result of
            Right admitted ->
                pure (Right admitted)
            Left err -> do
                rejectedAt <- getCurrentTime
                Right
                    <$> persistTurnTerminalEventually
                        backend
                        reserved
                        rejectedAt
                        (TurnErrored err.apiErrorMessage)

firstCheckedSubmitError
    :: Either (CheckedSubmitError ApiError) TurnRecord
    -> Either ApiError TurnRecord
firstCheckedSubmitError = \case
    Left (SubmitValidationFailed err) -> Left err
    Left (SubmitValidationRejected err) ->
        firstSubmitError (Left err)
    Right turn -> Right turn

firstSubmitError
    :: Either SubmitError TurnRecord
    -> Either ApiError TurnRecord
firstSubmitError = \case
    Left SubmitQueueFull ->
        Left ApiError
            { apiErrorStatus = 429
            , apiErrorCode = "turn_queue_full"
            , apiErrorMessage = "the turn queue is full"
            , apiErrorDetails = Nothing
            }
    Left SubmitTenantQueueFull ->
        Left ApiError
            { apiErrorStatus = 429
            , apiErrorCode = "tenant_turn_queue_full"
            , apiErrorMessage =
                "the authenticated tenant turn queue is full"
            , apiErrorDetails = Nothing
            }
    Left SubmitSessionBusy ->
        Left ApiError
            { apiErrorStatus = 409
            , apiErrorCode = "session_busy"
            , apiErrorMessage =
                "the session already has an active turn"
            , apiErrorDetails = Nothing
            }
    Left SubmitSupervisorClosed ->
        Left ApiError
            { apiErrorStatus = 503
            , apiErrorCode = "server_stopping"
            , apiErrorMessage = "the turn supervisor is stopping"
            , apiErrorDetails = Nothing
            }
    Right turn -> Right turn

persistTurnTerminalEventually
    :: Backend
    -> TurnRecord
    -> UTCTime
    -> TurnTerminalOutcome
    -> IO TurnRecord
persistTurnTerminalEventually backend record finishedAt outcome =
    go 100_000
  where
    go retryDelay =
        backend.backendTurnPersistence.turnPersistenceTerminal
            record
            finishedAt
            outcome
            >>= \case
                Right canonical -> pure canonical
                Left _ -> do
                    threadDelay retryDelay
                    go (min 2_000_000 (retryDelay * 2))

findTurn
    :: Backend
    -> Supervisor
    -> AccessBoundary
    -> Text
    -> IO (Either ApiError TurnRecord)
findTurn backend supervisor boundary rawTurnId = do
    case canonicalTurnId rawTurnId of
        Nothing -> pure (Left turnNotFound)
        Just turnId -> do
            backend.backendLookupTurn boundary turnId >>= \case
                Left err -> pure (Left err)
                Right Nothing -> pure (Left turnNotFound)
                Right (Just durable)
                    | isActiveTurn durable ->
                        lookupTurn supervisor boundary turnId
                            >>= pure . Right . maybe durable id
                    | otherwise -> pure (Right durable)

listKnownTurns
    :: Backend
    -> Supervisor
    -> AccessBoundary
    -> Maybe Text
    -> IO (Either ApiError [TurnRecord])
listKnownTurns backend supervisor boundary sessionId =
    backend.backendListTurns boundary sessionId >>= \case
        Left err -> pure (Left err)
        Right durable -> do
            active <- listTurns supervisor boundary sessionId
            let activeById =
                    Map.fromList
                        [ (turn.turnRecordId, turn)
                        | turn <- active
                        ]
                durableIds =
                    Map.fromList
                        [ (turn.turnRecordId, ())
                        | turn <- durable
                        ]
                merged =
                    [ if isActiveTurn turn
                        then
                            Map.findWithDefault
                                turn
                                turn.turnRecordId
                                activeById
                        else turn
                    | turn <- durable
                    ]
                        <> [ turn
                           | turn <- active
                           , Map.notMember turn.turnRecordId durableIds
                           ]
            pure . Right . take maximumTurnPageSize $
                sortOn
                    ( \turn ->
                        Down
                            ( turn.turnRecordCreatedAt
                            , turn.turnRecordId
                            )
                    )
                    merged

maximumTurnPageSize :: Int
maximumTurnPageSize = 200

cancelKnownTurn
    :: Backend
    -> Supervisor
    -> AccessBoundary
    -> Text
    -> IO (Either ApiError (Status, TurnRecord))
cancelKnownTurn backend supervisor boundary rawTurnId
    | Just turnId <- canonicalTurnId rawTurnId =
        cancelTurn supervisor boundary turnId >>= \case
            Right turn -> pure (Right (status200, turn))
            Left "turn not found" ->
                backend.backendLookupTurn boundary turnId >>= \case
                    Left err -> pure (Left err)
                    Right Nothing -> pure (Left turnNotFound)
                    Right (Just turn)
                        | not (isActiveTurn turn) ->
                            pure (Right (status200, turn))
                        | otherwise -> do
                            requestedAt <- getCurrentTime
                            backend.backendRequestTurnCancellation
                                boundary
                                turnId
                                requestedAt
                                >>= \case
                                    Left err -> pure (Left err)
                                    Right (Just (True, owned)) -> do
                                        cancelledAt <- getCurrentTime
                                        canonical <-
                                            persistTurnTerminalEventually
                                                backend
                                                owned
                                                cancelledAt
                                                TurnWasCancelled
                                        pure
                                            (Right (status200, canonical))
                                    Right (Just (False, requested)) ->
                                        pure
                                            (Right (status202, requested))
                                    Right Nothing ->
                                        backend.backendLookupTurn
                                            boundary
                                            turnId
                                            >>= \case
                                                Left err -> pure (Left err)
                                                Right Nothing ->
                                                    pure (Left turnNotFound)
                                                Right (Just canonical) ->
                                                    pure
                                                        ( Right
                                                            ( status200
                                                            , canonical
                                                            )
                                                        )
            Left message ->
                pure (Left (cancellationError message))
    | otherwise =
        pure (Left turnNotFound)
  where
    cancellationError message =
        ApiError
            { apiErrorStatus = 503
            , apiErrorCode = "turn_cancellation_failed"
            , apiErrorMessage = message
            , apiErrorDetails = Nothing
            }

findTurnResult
    :: Backend
    -> AccessBoundary
    -> Text
    -> IO (Either ApiError TurnResult)
findTurnResult backend boundary rawTurnId =
    case canonicalTurnId rawTurnId of
        Nothing -> pure (Left turnNotFound)
        Just turnId ->
            backend.backendLookupTurnResult
                boundary
                turnId
                >>= \case
                    Left err -> pure (Left err)
                    Right Nothing -> pure (Left turnNotFound)
                    Right (Just result)
                        | isActiveTurn result.turnResultTurn ->
                            pure . Left $
                                ApiError
                                    { apiErrorStatus = 409
                                    , apiErrorCode = "turn_not_terminal"
                                    , apiErrorMessage =
                                        "the turn has not reached a terminal state"
                                    , apiErrorDetails =
                                        Just
                                            ( toJSONValue
                                                result.turnResultTurn
                                            )
                                    }
                        | otherwise -> pure (Right result)

isActiveTurn :: TurnRecord -> Bool
isActiveTurn turn =
    turn.turnRecordStatus
        `elem` [TurnQueued, TurnRunning, TurnWaitingForInput]

runSessionMutation
    :: Backend
    -> Supervisor
    -> AccessBoundary
    -> Text
    -> IO (Either ApiError value)
    -> IO (Either ApiError value)
runSessionMutation backend supervisor boundary sessionId action =
    withSessionMutation
        supervisor
        boundary
        sessionId
        durableMutation
        >>= \case
            Left SessionMutationBusy ->
                pure $
                    Left ApiError
                        { apiErrorStatus = 409
                        , apiErrorCode = "session_busy"
                        , apiErrorMessage =
                            "the session has an active turn or mutation"
                        , apiErrorDetails = Nothing
                        }
            Left SessionMutationSupervisorClosed ->
                pure $
                    Left ApiError
                        { apiErrorStatus = 503
                        , apiErrorCode = "server_stopping"
                        , apiErrorMessage =
                            "the turn supervisor is stopping"
                        , apiErrorDetails = Nothing
                        }
            Right result -> pure result
  where
    durableMutation =
        mask \restore -> do
            now <- getCurrentTime
            reservation <-
                backend.backendReserveSessionMutation
                    boundary
                    sessionId
                    now
            case reservation of
                Left err -> pure (Left err)
                Right Nothing -> pure (Left sessionBusyError)
                Right
                    ( Just
                            SessionMutationLease
                                { runSessionMutationLease = runMutation
                                , releaseSessionMutationLease = releaseMutation
                                }
                        ) -> do
                        guarded <-
                            restore (runMutation action)
                                `finally` releaseMutation
                        pure (guarded >>= id)

    sessionBusyError =
        ApiError
            { apiErrorStatus = 409
            , apiErrorCode = "session_busy"
            , apiErrorMessage =
                "the session has an active turn or mutation"
            , apiErrorDetails = Nothing
            }

createEventResponse
    :: Backend
    -> Supervisor
    -> AccessBoundary
    -> Text
    -> [Header]
    -> Request
    -> IO (Either ApiError Response)
createEventResponse
        backend supervisor boundary requestId corsHeaders request =
    case parseLastEventId request of
        Left err -> pure (Left err)
        Right lastEventId -> do
            subscribeEvents supervisor boundary lastEventId >>= \case
                Left EventSubscriberLimitReached ->
                    pure (Left eventSubscriberLimitError)
                Left EventSubscriberTenantLimitReached ->
                    pure (Left eventSubscriberTenantLimitError)
                Right subscription -> do
                    let stream write flush =
                            finally
                                (do
                                    continue <-
                                        emitInitialEvents
                                            backend
                                            boundary
                                            subscription
                                            write
                                            flush
                                    if continue
                                        then
                                            eventLoop
                                                backend
                                                boundary
                                                subscription.subscriptionChannel
                                                (lastDeliveredId
                                                    subscription.subscriptionLatestEventId
                                                    subscription.subscriptionReplay)
                                                write
                                                flush
                                        else pure ())
                                subscription.subscriptionClose
                    pure $
                        Right $
                            responseStream
                                status200
                                (responseHeaders
                                    requestId
                                    corsHeaders
                                    [ (hContentType, "text/event-stream")
                                    , (hCacheControl, "no-cache")
                                    , ("X-Accel-Buffering", "no")
                                    ])
                                stream
  where
    eventSubscriberLimitError = ApiError
        { apiErrorStatus = 429
        , apiErrorCode = "event_subscriber_limit"
        , apiErrorMessage =
            "the server event subscriber limit has been reached"
        , apiErrorDetails = Nothing
        }
    eventSubscriberTenantLimitError = ApiError
        { apiErrorStatus = 429
        , apiErrorCode = "tenant_event_subscriber_limit"
        , apiErrorMessage =
            "the tenant event subscriber limit has been reached"
        , apiErrorDetails = Nothing
        }

emitInitialEvents
    :: Backend
    -> AccessBoundary
    -> EventSubscription (TBQueue ServerEvent)
    -> (Builder -> IO ())
    -> IO ()
    -> IO Bool
emitInitialEvents backend boundary subscription write flush = do
    resetOk <-
        if subscription.subscriptionResetRequired
            then emitUnderBoundary
                backend
                boundary
                resetEventBuilder
                write
                flush
            else pure True
    if not resetOk
        then pure False
        else
            foldM
                (\continue event ->
                    if not continue
                        then pure False
                        else if event.serverEventBoundary /= boundary
                            then pure True
                            else do
                                emitUnderBoundary
                                    backend
                                    boundary
                                    (serverEventBuilder event)
                                    write
                                    flush)
                True
                subscription.subscriptionReplay

eventLoop
    :: Backend
    -> AccessBoundary
    -> TBQueue ServerEvent
    -> Maybe Integer
    -> (Builder -> IO ())
    -> IO ()
    -> IO ()
eventLoop backend boundary channel previousId write flush =
    race
        (threadDelay (15 * 1000 * 1000))
        (atomically (readTBQueue channel)) >>= \case
            Left () -> do
                continue <-
                    emitUnderBoundary
                        backend
                        boundary
                        (byteString ": keep-alive\n\n")
                        write
                        flush
                if continue
                    then
                        eventLoop
                            backend boundary channel previousId write flush
                    else pure ()
            Right event
                | event.serverEventBoundary /= boundary ->
                    eventLoop backend boundary channel previousId write flush
                | otherwise -> do
                    gapOk <-
                        if hasEventGap previousId event
                            then emitUnderBoundary
                                backend
                                boundary
                                resetEventBuilder
                                write
                                flush
                            else pure True
                    eventOk <-
                        if gapOk
                            then emitUnderBoundary
                                backend
                                boundary
                                (serverEventBuilder event)
                                write
                                flush
                            else pure False
                    if eventOk
                        then
                            eventLoop
                                backend
                                boundary
                                channel
                                (Just event.serverEventId)
                                write
                                flush
                        else pure ()

emitUnderBoundary
    :: Backend
    -> AccessBoundary
    -> Builder
    -> (Builder -> IO ())
    -> IO ()
    -> IO Bool
emitUnderBoundary backend boundary builder write flush = do
    let Backend { backendContinueBoundary = continueBoundary } = backend
    result <-
        timeout
            (5 * 1000 * 1000)
            (continueBoundary boundary (write builder >> flush))
    pure case result of
        Just (Right ()) -> True
        _ -> False

serverEventBuilder :: ServerEvent -> Builder
serverEventBuilder event =
    byteString "id: "
        <> integerDec event.serverEventId
        <> byteString "\nevent: "
        <> byteString
            (TextEncoding.encodeUtf8
                (safeSseEventType event.serverEventType))
        <> byteString "\ndata: "
        <> lazyByteString (encode event)
        <> byteString "\n\n"

safeSseEventType :: Text -> Text
safeSseEventType raw =
    let sanitized =
            Text.take 128 $
                Text.filter
                    (\character ->
                        character == '.'
                            || character == '-'
                            || character == '_'
                            || character >= 'a' && character <= 'z'
                            || character >= 'A' && character <= 'Z'
                            || character >= '0' && character <= '9')
                    raw
    in if Text.null sanitized then "message" else sanitized

resetEventBuilder :: Builder
resetEventBuilder =
    byteString "event: replay.reset\ndata: "
        <> lazyByteString
            (encode
                (object
                    [ "reason" .= ("event_gap" :: Text)
                    , "refetch" .= True
                    ]))
        <> byteString "\n\n"

hasEventGap :: Maybe Integer -> ServerEvent -> Bool
hasEventGap Nothing _ = False
hasEventGap (Just previous) event =
    event.serverEventId > previous + 1

lastDeliveredId
    :: Maybe Integer
    -> [ServerEvent]
    -> Maybe Integer
lastDeliveredId initial replay =
    case reverse replay of
        event : _ -> Just event.serverEventId
        [] -> initial

parseLastEventId :: Request -> Either ApiError (Maybe Integer)
parseLastEventId request =
    case lookup "Last-Event-ID" request.requestHeaders of
        Nothing -> Right Nothing
        Just raw ->
            case readMaybe (ByteString8.unpack raw) of
                Just value
                    | value >= 0 -> Right (Just value)
                _ ->
                    Left ApiError
                        { apiErrorStatus = 400
                        , apiErrorCode = "invalid_last_event_id"
                        , apiErrorMessage =
                            "Last-Event-ID must be a non-negative integer"
                        , apiErrorDetails = Nothing
                        }

withJsonBody
    :: FromJSON body
    => ApplicationConfig
    -> Request
    -> (body -> IO (Either ApiError Response))
    -> IO (Either ApiError Response)
withJsonBody config request action =
    readJsonBody config request >>= \case
        Left err -> pure (Left err)
        Right body -> action body

readJsonBody
    :: FromJSON body
    => ApplicationConfig
    -> Request
    -> IO (Either ApiError body)
readJsonBody config request
    | not (jsonContentType request) =
        pure $
            Left ApiError
                { apiErrorStatus = 415
                , apiErrorCode = "unsupported_media_type"
                , apiErrorMessage =
                    "request bodies must use application/json"
                , apiErrorDetails = Nothing
                }
    | KnownLength lengthValue <- request.requestBodyLength
    , lengthValue
        > fromIntegral config.applicationMaximumRequestBytes =
        pure (Left requestTooLarge)
    | otherwise =
        readBoundedBody
            config.applicationMaximumRequestBytes
            request >>= \case
                Left err -> pure (Left err)
                Right bytes ->
                    pure case eitherDecodeStrict' bytes of
                        Left _ ->
                            Left ApiError
                                { apiErrorStatus = 400
                                , apiErrorCode = "invalid_json"
                                , apiErrorMessage =
                                    "the request body is not valid for this endpoint"
                                , apiErrorDetails = Nothing
                                }
                        Right value -> Right value

readBoundedBody
    :: Int
    -> Request
    -> IO (Either ApiError ByteString)
readBoundedBody maximumBytes request = go 0 []
  where
    go size chunks = do
        chunk <- getRequestBodyChunk request
        if ByteString.null chunk
            then pure (Right (ByteString.concat (reverse chunks)))
            else
                let nextSize = size + ByteString.length chunk
                in if nextSize > maximumBytes
                    then pure (Left requestTooLarge)
                    else go nextSize (chunk : chunks)

requestTooLarge :: ApiError
requestTooLarge = ApiError
    { apiErrorStatus = 413
    , apiErrorCode = "request_too_large"
    , apiErrorMessage = "the request body exceeds the configured limit"
    , apiErrorDetails = Nothing
    }

jsonContentType :: Request -> Bool
jsonContentType request =
    case lookup hContentType request.requestHeaders of
        Nothing -> False
        Just value ->
            stripAsciiSpace
                (ByteString8.map
                    asciiLower
                    (ByteString8.takeWhile (/= ';') value))
                == "application/json"

queryArchiveFilter
    :: Request
    -> Either ApiError SessionArchiveFilter
queryArchiveFilter request =
    queryOptionalText "archive" request >>= \case
        Nothing -> Right ActiveSessions
        Just "active" -> Right ActiveSessions
        Just "archived" -> Right ArchivedSessions
        Just "all" -> Right AllSessions
        Just _ ->
            Left ApiError
                { apiErrorStatus = 400
                , apiErrorCode = "invalid_archive_filter"
                , apiErrorMessage =
                    "archive must be active, archived, or all"
                , apiErrorDetails = Nothing
                }

queryLimit
    :: ByteString
    -> Int
    -> Int
    -> Request
    -> Either ApiError Int
queryLimit name defaultValue maximum request =
    queryOptionalText name request >>= \case
        Nothing -> Right defaultValue
        Just raw ->
            case readMaybe (Text.unpack raw) of
                Just value
                    | value >= 1
                    , value <= maximum ->
                        Right value
                _ ->
                    Left ApiError
                        { apiErrorStatus = 400
                        , apiErrorCode = "invalid_limit"
                        , apiErrorMessage =
                            "limit is outside the allowed range"
                        , apiErrorDetails = Nothing
                        }

queryOptionalTurnId :: Request -> Either ApiError (Maybe TurnId)
queryOptionalTurnId request =
    queryOptionalText "turnId" request >>= \case
        Nothing -> Right Nothing
        Just raw
            | Just turnId <- canonicalTurnId raw -> Right (Just turnId)
            | otherwise ->
                Left ApiError
                    { apiErrorStatus = 400
                    , apiErrorCode = "invalid_query"
                    , apiErrorMessage = "turnId must be a UUID"
                    , apiErrorDetails = Nothing
                    }

canonicalTurnId :: Text -> Maybe TurnId
canonicalTurnId raw
    | isUUIDText raw = Just (TurnId (Text.toLower raw))
    | otherwise = Nothing

queryOptionalInteger
    :: ByteString
    -> Request
    -> Either ApiError (Maybe Integer)
queryOptionalInteger name request =
    queryOptionalText name request >>= traverse parse
  where
    parse raw =
        case readMaybe (Text.unpack raw) of
            Just value
                | value >= 0 -> Right value
            _ ->
                Left ApiError
                    { apiErrorStatus = 400
                    , apiErrorCode = "invalid_cursor"
                    , apiErrorMessage =
                        "the numeric cursor is invalid"
                    , apiErrorDetails = Nothing
                    }

queryOptionalText
    :: ByteString
    -> Request
    -> Either ApiError (Maybe Text)
queryOptionalText name request =
    case lookup name request.queryString of
        Nothing -> Right Nothing
        Just Nothing -> Right (Just "")
        Just (Just raw) ->
            case TextEncoding.decodeUtf8' raw of
                Left _ ->
                    Left ApiError
                        { apiErrorStatus = 400
                        , apiErrorCode = "invalid_query"
                        , apiErrorMessage =
                            "a query parameter is not valid UTF-8"
                        , apiErrorDetails = Nothing
                        }
                Right value -> Right (Just value)

firstQueryError :: [Either ApiError value] -> ApiError
firstQueryError values =
    case [err | Left err <- values] of
        err : _ -> err
        [] ->
            ApiError
                { apiErrorStatus = 400
                , apiErrorCode = "invalid_query"
                , apiErrorMessage = "the query is invalid"
                , apiErrorDetails = Nothing
                }

jsonResponse :: Status -> [Header] -> Value -> Response
jsonResponse status headers value =
    responseLBS
        status
        ( (hContentType, "application/json")
        : (hCacheControl, "no-store")
        : ("X-Content-Type-Options", "nosniff")
        : headers
        )
        (encode value)

apiErrorResponse
    :: Text
    -> [Header]
    -> ApiError
    -> Response
apiErrorResponse requestId corsHeaders err =
    jsonResponse
        (mkStatus err.apiErrorStatus "")
        (responseHeaders requestId corsHeaders [])
        (object
            [ "error" .= object
                ( [ "code" .= err.apiErrorCode
                  , "message" .= err.apiErrorMessage
                  , "requestId" .= requestId
                  ]
                    <> maybe
                        []
                        (\details -> ["details" .= details])
                        err.apiErrorDetails
                )
            ])

authFailureResponse
    :: Text
    -> [Header]
    -> AuthFailure
    -> Response
authFailureResponse requestId corsHeaders failure =
    apiErrorResponse
        requestId
        corsHeaders
        ApiError
            { apiErrorStatus = failure.authFailureStatus
            , apiErrorCode = failure.authFailureCode
            , apiErrorMessage = failure.authFailureMessage
            , apiErrorDetails = Nothing
            }

responseHeaders :: Text -> [Header] -> [Header] -> [Header]
responseHeaders requestId corsHeaders additional =
    ("X-Request-ID", TextEncoding.encodeUtf8 requestId)
        : corsHeaders
        <> additional

toJSONValue :: Aeson.ToJSON value => value -> Value
toJSONValue = Aeson.toJSON

turnNotFound :: ApiError
turnNotFound = ApiError
    { apiErrorStatus = 404
    , apiErrorCode = "turn_not_found"
    , apiErrorMessage = "turn not found"
    , apiErrorDetails = Nothing
    }

turnAgentsUnavailable :: ApiError
turnAgentsUnavailable = ApiError
    { apiErrorStatus = 409
    , apiErrorCode = "turn_agents_unavailable"
    , apiErrorMessage =
        "turn agent snapshot is unavailable on this server instance"
    , apiErrorDetails = Nothing
    }

humanRequestNotFound :: ApiError
humanRequestNotFound = ApiError
    { apiErrorStatus = 404
    , apiErrorCode = "request_not_found"
    , apiErrorMessage = "request not found"
    , apiErrorDetails = Nothing
    }

routeNotFound :: ApiError
routeNotFound = ApiError
    { apiErrorStatus = 404
    , apiErrorCode = "route_not_found"
    , apiErrorMessage = "route not found"
    , apiErrorDetails = Nothing
    }

asciiLower :: Char -> Char
asciiLower character
    | character >= 'A' && character <= 'Z' =
        toEnum (fromEnum character + 32)
    | otherwise = character

stripAsciiSpace :: ByteString -> ByteString
stripAsciiSpace =
    ByteString8.dropWhileEnd isAsciiSpace
        . ByteString8.dropWhile isAsciiSpace
  where
    isAsciiSpace character =
        character == ' '
            || character == '\t'
