-- | Versioned WAI REST and SSE application.
module Agent.Server.Application
    ( ApplicationConfig(..)
    , newApplication
    ) where

import Agent.Server.Auth
    ( AuthConfig
    , AuthFailure(..)
    , authorizePreflight
    , authorizeRequest
    , corsResponseHeaders
    , isCorsPreflight
    )
import Agent.Server.Backend (Backend(..))
import Agent.Server.Supervisor
    ( CheckedSubmitError(..)
    , SubmitError(..)
    , SessionMutationError(..)
    , Supervisor
    , cancelTurn
    , listHumanRequests
    , listTurns
    , lookupTurn
    , lookupTurnAgents
    , resolveHumanRequest
    , submitTurnChecked
    , subscribeEvents
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
    , tryAny
    )
import Control.Monad (foldM)
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
import Data.IORef
    ( atomicModifyIORef'
    , newIORef
    )
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
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
newApplication config auth backend supervisor = do
    requestCounter <- newIORef (1 :: Integer)
    pure \request respond -> do
        requestId <- atomicModifyIORef' requestCounter \next ->
            (next + 1, "request-" <> Text.pack (show next))
        let corsHeaders = corsResponseHeaders auth request
            authorization
                | isCorsPreflight request =
                    authorizePreflight auth request
                | otherwise = authorizeRequest auth request
        case authorization of
            Left failure ->
                respond $
                    apiErrorResponse
                        requestId
                        corsHeaders
                        ApiError
                            { apiErrorStatus = failure.authFailureStatus
                            , apiErrorCode = failure.authFailureCode
                            , apiErrorMessage = failure.authFailureMessage
                            , apiErrorDetails = Nothing
                            }
            Right corsHeaders
                | isCorsPreflight request ->
                    respond $
                        responseLBS
                            status204
                            (responseHeaders requestId corsHeaders [])
                            ""
                | otherwise -> do
                    outcome <-
                        tryAny
                            (routeRequest
                                config
                                backend
                                supervisor
                                requestId
                                corsHeaders
                                request)
                    respond case outcome of
                        Left _ ->
                            apiErrorResponse
                                requestId
                                corsHeaders
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
    -> Request
    -> IO Response
routeRequest config backend supervisor requestId corsHeaders request =
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
            admitBoundary requestId corsHeaders backend \boundary ->
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
    -> (GatewayBoundary -> IO Response)
    -> IO Response
admitBoundary requestId corsHeaders backend action = do
    let Backend { backendAdmitBoundary = admit } = backend
    admit action >>= \case
        Left err -> pure (apiErrorResponse requestId corsHeaders err)
        Right response -> pure response

dispatchBoundary
    :: ApplicationConfig
    -> Backend
    -> Supervisor
    -> Text
    -> [Header]
    -> Request
    -> GatewayBoundary
    -> IO (Either ApiError Response)
dispatchBoundary
        config backend supervisor requestId corsHeaders request boundary =
    case (request.requestMethod, request.pathInfo) of
        ("GET", ["v1", "models"]) ->
            fmap
                (fmap (jsonResponse status200 headers))
                (backend.backendListModels boundary)
        ("GET", ["v1", "sessions"]) -> do
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
        ("POST", ["v1", "sessions"]) ->
            withJsonBody config request \body ->
                fmap
                    (fmap (jsonResponse status201 headers))
                    (backend.backendCreateSession boundary body)
        ("GET", ["v1", "sessions", sessionId]) ->
            fmap
                (fmap (jsonResponse status200 headers))
                (backend.backendGetSession boundary sessionId)
        ("PATCH", ["v1", "sessions", sessionId]) ->
            withJsonBody config request
                \(body :: PatchSessionRequest) ->
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
        ("DELETE", ["v1", "sessions", sessionId]) ->
            runSessionMutation
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
        ("GET", ["v1", "sessions", sessionId, "history"]) -> do
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
        ("POST", ["v1", "sessions", sessionId, "fork"]) ->
            withJsonBody config request \body ->
                runSessionMutation
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
        ("POST", ["v1", "sessions", sessionId, "turns"]) ->
            withJsonBody config request \body ->
                createTurn backend supervisor boundary sessionId body
                    >>= pure
                        . fmap (jsonResponse status202 headers . toJSONValue)
        ("GET", ["v1", "turns"]) ->
            case queryOptionalText "sessionId" request of
                Left err -> pure (Left err)
                Right sessionId -> do
                    turns <- listTurns supervisor boundary sessionId
                    pure $
                        Right $
                            jsonResponse
                                status200
                                headers
                                (object ["data" .= turns])
        ("GET", ["v1", "turns", rawTurnId]) ->
            findTurn supervisor boundary rawTurnId >>= pure
                . fmap (jsonResponse status200 headers . toJSONValue)
        ("POST", ["v1", "turns", rawTurnId, "cancel"]) ->
            cancelTurn
                supervisor
                boundary
                (TurnId rawTurnId) >>= \case
                    Left _ -> pure (Left turnNotFound)
                    Right turn ->
                        pure $
                            Right $
                                jsonResponse
                                    status200
                                    headers
                                    (toJSONValue turn)
        ("GET", ["v1", "turns", rawTurnId, "agents"]) ->
            lookupTurnAgents
                supervisor
                boundary
                (TurnId rawTurnId) >>= \case
                    Nothing -> pure (Left turnNotFound)
                    Just agents ->
                        pure $
                            Right $
                                jsonResponse
                                    status200
                                    headers
                                    (object ["data" .= agents])
        ("GET", ["v1", "requests"]) -> do
            requests <- listHumanRequests supervisor boundary
            pure $
                Right $
                    jsonResponse
                        status200
                        headers
                        (object ["data" .= requests])
        ("POST", ["v1", "requests", rawRequestId, "resolve"]) ->
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
                    (RequestId rawRequestId)
                    response >>= \case
                        Left message ->
                            pure $
                                Left ApiError
                                    { apiErrorStatus =
                                        if "not found"
                                            `Text.isInfixOf`
                                                Text.toLower message
                                            then 404
                                            else 409
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

createTurn
    :: Backend
    -> Supervisor
    -> GatewayBoundary
    -> Text
    -> CreateTurnRequest
    -> IO (Either ApiError TurnRecord)
createTurn backend supervisor boundary sessionId request
    | Text.null (Text.strip request.createTurnInput) =
        pure $
            Left ApiError
                { apiErrorStatus = 422
                , apiErrorCode = "empty_input"
                , apiErrorMessage = "turn input must not be empty"
                , apiErrorDetails = Nothing
                }
    | otherwise =
        let spec = TurnSpec
                { turnSpecSessionId = sessionId
                , turnSpecPrompt = request.createTurnInput
                , turnSpecBoundary = boundary
                }
            validateSession =
                fmap (fmap (const ())) $
                    backend.backendGetSession boundary sessionId
        in
            submitTurnChecked
                supervisor
                spec
                validateSession >>= pure . firstCheckedSubmitError

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

findTurn
    :: Supervisor
    -> GatewayBoundary
    -> Text
    -> IO (Either ApiError TurnRecord)
findTurn supervisor boundary rawTurnId =
    lookupTurn supervisor boundary (TurnId rawTurnId) >>= \case
        Nothing -> pure (Left turnNotFound)
        Just turn -> pure (Right turn)

runSessionMutation
    :: Supervisor
    -> GatewayBoundary
    -> Text
    -> IO (Either ApiError value)
    -> IO (Either ApiError value)
runSessionMutation supervisor boundary sessionId action =
    withSessionMutation
        supervisor boundary sessionId action >>= \case
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

createEventResponse
    :: Backend
    -> Supervisor
    -> GatewayBoundary
    -> Text
    -> [Header]
    -> Request
    -> IO (Either ApiError Response)
createEventResponse
        backend supervisor boundary requestId corsHeaders request =
    case parseLastEventId request of
        Left err -> pure (Left err)
        Right lastEventId -> do
            subscription <-
                subscribeEvents supervisor boundary lastEventId
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

emitInitialEvents
    :: Backend
    -> GatewayBoundary
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
    -> GatewayBoundary
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
    -> GatewayBoundary
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
