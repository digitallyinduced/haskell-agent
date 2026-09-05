{- | Bounded, redirect-free HTTP and SSE client for the versioned agent-server
API. Deployment-specific routing and authorization policy belong to callers.
-}
module Agent.Server.Client (
    module Agent.Server.Client.GatewayIdentity,
    module Agent.Server.Client.Protocol,
    AgentServerClient,
    AgentServerClientConfig (..),
    AgentServerClientError (..),
    AgentServerStreamResult (..),
    newAgentServerClient,
    createAgentServerSession,
    createAgentServerTurn,
    getAgentServerTurn,
    getAgentServerTurnResult,
    listAgentServerTurns,
    cancelAgentServerTurn,
    listAgentServerRequests,
    listAgentServerRequestsForTurn,
    resolveAgentServerRequest,
    getAgentServerHistory,
    streamAgentServerTurn,
)
where

import Agent.Server.Client.GatewayIdentity
import Agent.Server.Client.Protocol
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Exception (IOException)
import Control.Exception.Safe qualified as Exception
import Control.Monad (when)
import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.Bifunctor qualified as Bifunctor
import Data.Bits qualified as Bits
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString8
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Char (toLower)
import Data.Int (Int64)
import Data.List (minimumBy)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (
    hAccept,
    hAuthorization,
    hContentType,
 )
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.URI (urlEncode)
import System.IO.Error (isEOFError)
import System.Posix.Files (
    fileMode,
    fileOwner,
    getFdStatus,
    isRegularFile,
 )
import System.Posix.IO (
    OpenFileFlags (..),
    OpenMode (ReadOnly),
    closeFd,
    defaultFileFlags,
    openFd,
 )
import System.Posix.IO.ByteString qualified as Posix
import System.Posix.Types (Fd)
import System.Posix.User (getEffectiveUserID)

data AgentServerClientConfig = AgentServerClientConfig
    { agentServerBaseUrl :: !Text
    , agentServerCredentialFile :: !FilePath
    }
    deriving (Eq, Show)

data AgentServerClient = AgentServerClient
    { clientManager :: !Manager
    , clientBaseUrl :: !Text
    , clientBearerToken :: !ByteString.ByteString
    }

data AgentServerClientError
    = AgentServerCredentialError !Text
    | AgentServerTransportError !Text
    | AgentServerHttpError !Int !(Maybe Text) !Text
    | AgentServerDecodeError !Text
    | AgentServerProtocolError !Text
    deriving (Eq, Show)

data AgentServerStreamResult
    = AgentServerStreamCompleted
    | AgentServerStreamFailed !(Maybe Text)
    | AgentServerStreamCancelled
    | AgentServerStreamNeedsRefetch
    deriving (Eq, Show)

newAgentServerClient ::
    AgentServerClientConfig ->
    IO (Either AgentServerClientError AgentServerClient)
newAgentServerClient config =
    validateAgentServerBaseUrl config.agentServerBaseUrl >>= \case
        Left err -> pure (Left err)
        Right baseUrl ->
            readPrivateBearerToken config.agentServerCredentialFile >>= \case
                Left err -> pure (Left err)
                Right token -> do
                    manager <- newManager tlsManagerSettings
                    pure . Right $
                        AgentServerClient
                            { clientManager = manager
                            , clientBaseUrl = baseUrl
                            , clientBearerToken = token
                            }

createAgentServerSession ::
    AgentServerClient ->
    AgentServerCreateSessionRequest ->
    IO (Either AgentServerClientError AgentServerSession)
createAgentServerSession client =
    performJsonRequest client "POST" "/v1/sessions" [201]
        . Just
        . Aeson.encode

createAgentServerTurn ::
    AgentServerClient ->
    Text ->
    AgentServerCreateTurnRequest ->
    IO (Either AgentServerClientError AgentServerTurn)
createAgentServerTurn client sessionId =
    performJsonRequest
        client
        "POST"
        ("/v1/sessions/" <> encodePathSegment sessionId <> "/turns")
        [202]
        . Just
        . Aeson.encode

getAgentServerTurn ::
    AgentServerClient ->
    Text ->
    IO (Either AgentServerClientError AgentServerTurn)
getAgentServerTurn client turnId =
    performJsonRequest
        client
        "GET"
        ("/v1/turns/" <> encodePathSegment turnId)
        [200]
        Nothing

getAgentServerTurnResult ::
    AgentServerClient ->
    Text ->
    IO (Either AgentServerClientError AgentServerTurnResult)
getAgentServerTurnResult client turnId =
    performJsonRequest
        client
        "GET"
        ("/v1/turns/" <> encodePathSegment turnId <> "/result")
        [200]
        Nothing

listAgentServerTurns ::
    AgentServerClient ->
    Text ->
    IO (Either AgentServerClientError AgentServerTurnList)
listAgentServerTurns client sessionId =
    performJsonRequestWithLimit
        maximumTurnListResponseBytes
        client
        "GET"
        ("/v1/turns?sessionId=" <> encodeQueryValue sessionId)
        [200]
        Nothing

cancelAgentServerTurn ::
    AgentServerClient ->
    Text ->
    IO (Either AgentServerClientError AgentServerTurn)
cancelAgentServerTurn client turnId =
    performJsonRequest
        client
        "POST"
        ("/v1/turns/" <> encodePathSegment turnId <> "/cancel")
        [200, 202]
        Nothing

listAgentServerRequests ::
    AgentServerClient ->
    IO (Either AgentServerClientError AgentServerRequestList)
listAgentServerRequests client =
    performJsonRequestWithLimit
        maximumRequestListResponseBytes
        client
        "GET"
        "/v1/requests"
        [200]
        Nothing

listAgentServerRequestsForTurn ::
    AgentServerClient ->
    Text ->
    IO (Either AgentServerClientError AgentServerRequestList)
listAgentServerRequestsForTurn client turnId =
    performJsonRequestWithLimit
        maximumRequestListResponseBytes
        client
        "GET"
        ("/v1/requests?turnId=" <> encodeQueryValue turnId)
        [200]
        Nothing

resolveAgentServerRequest ::
    AgentServerClient ->
    Text ->
    AgentServerResolveRequest ->
    IO (Either AgentServerClientError AgentServerHumanRequest)
resolveAgentServerRequest client requestId =
    performJsonRequest
        client
        "POST"
        ("/v1/requests/" <> encodePathSegment requestId <> "/resolve")
        [200]
        . Just
        . Aeson.encode

getAgentServerHistory ::
    AgentServerClient ->
    Text ->
    IO (Either AgentServerClientError AgentServerHistory)
-- The current turn is always in the recent page. Twenty maximally projected
-- turns can exceed the default JSON response limit, so history has its own
-- bound sized for the server's per-turn projection budget.
getAgentServerHistory client sessionId =
    performJsonRequestWithLimit
        maximumHistoryResponseBytes
        client
        "GET"
        ( "/v1/sessions/"
            <> encodePathSegment sessionId
            <> "/history?limit=20"
        )
        [200]
        Nothing

streamAgentServerTurn ::
    AgentServerClient ->
    Text ->
    Maybe Int64 ->
    (AgentServerEvent -> IO (Either AgentServerClientError ())) ->
    IO (Either AgentServerClientError AgentServerStreamResult)
streamAgentServerTurn client expectedTurnId lastEventId onEvent =
    race consumeStream monitorDurableTurn >>= pure . either id id
  where
    consumeStream = do
        outcome <- Exception.try @IO @HttpException do
            request <- authenticatedRequest client "GET" "/v1/events"
            let cursor = maybe 0 id lastEventId
                eventRequest =
                    request
                        { requestHeaders =
                            (hAccept, "text/event-stream")
                                : ( "Last-Event-ID"
                                  , ByteString8.pack (show cursor)
                                  )
                                : filter
                                    ( \header ->
                                        fst header /= hAccept
                                            && fst header /= "Last-Event-ID"
                                    )
                                    request.requestHeaders
                        , responseTimeout = responseTimeoutNone
                        }
            withResponse eventRequest client.clientManager \response ->
                if statusCode response.responseStatus /= 200
                    then decodeErrorResponse response
                    else
                        consumeEventStream
                            ByteString.empty
                            response.responseBody
        pure case outcome of
            Left _ ->
                Left
                    ( AgentServerTransportError
                        "agent-server event stream request failed"
                    )
            Right result -> result

    monitorDurableTurn = do
        threadDelay durableTurnPollIntervalMicros
        getAgentServerTurn client expectedTurnId >>= \case
            Right turn
                | isTerminalTurnStatus turn.agentServerTurnStatus ->
                    pure (Right AgentServerStreamNeedsRefetch)
            Left (AgentServerHttpError 404 _ _) ->
                pure (Right AgentServerStreamNeedsRefetch)
            _ ->
                listAgentServerRequestsForTurn client expectedTurnId >>= \case
                    Right requests
                        | any
                            ( (== expectedTurnId)
                                . (.agentServerRequestTurnId)
                            )
                            requests.agentServerRequests ->
                            pure (Right AgentServerStreamNeedsRefetch)
                    _ -> monitorDurableTurn

    consumeEventStream buffered bodyReader = do
        chunk <- brRead bodyReader
        if ByteString.null chunk
            then
                pure
                    ( Left
                        ( AgentServerProtocolError
                            "agent-server closed the event stream before the turn finished"
                        )
                    )
            else do
                let combined = buffered <> chunk
                if ByteString.length combined > maximumSseBufferBytes
                    then
                        pure
                            ( Left
                                ( AgentServerProtocolError
                                    "agent-server SSE frame exceeded the size limit"
                                )
                            )
                    else
                        processFrames combined >>= \case
                            Left err -> pure (Left err)
                            Right (remaining, Nothing) ->
                                consumeEventStream remaining bodyReader
                            Right (_, Just terminal) ->
                                pure (Right terminal)

    processFrames buffered =
        case takeSseFrame buffered of
            Nothing -> pure (Right (buffered, Nothing))
            Just (frame, rest) ->
                case parseAgentServerSseFrame frame of
                    Left message ->
                        pure (Left (AgentServerProtocolError message))
                    Right Nothing -> processFrames rest
                    Right (Just (AgentServerSseReplayReset _)) ->
                        pure
                            ( Right
                                (rest, Just AgentServerStreamNeedsRefetch)
                            )
                    Right (Just (AgentServerSseEvent event))
                        | event.agentServerEventTurnId
                            /= Just expectedTurnId ->
                            processFrames rest
                        | otherwise ->
                            onEvent event >>= \case
                                Left err -> pure (Left err)
                                Right () ->
                                    case event.agentServerEventPayload of
                                        AgentServerTurnCompletedEvent ->
                                            pure
                                                ( Right
                                                    ( rest
                                                    , Just
                                                        AgentServerStreamCompleted
                                                    )
                                                )
                                        AgentServerTurnFailedEvent message ->
                                            pure
                                                ( Right
                                                    ( rest
                                                    , Just
                                                        ( AgentServerStreamFailed
                                                            message
                                                        )
                                                    )
                                                )
                                        AgentServerTurnCancelledEvent ->
                                            pure
                                                ( Right
                                                    ( rest
                                                    , Just
                                                        AgentServerStreamCancelled
                                                    )
                                                )
                                        _ -> processFrames rest

isTerminalTurnStatus :: AgentServerTurnStatus -> Bool
isTerminalTurnStatus = \case
    AgentServerTurnQueued -> False
    AgentServerTurnRunning -> False
    AgentServerTurnWaitingForInput -> False
    AgentServerTurnCompleted -> True
    AgentServerTurnFailed -> True
    AgentServerTurnCancelled -> True

performJsonRequest ::
    (FromJSON responseBody) =>
    AgentServerClient ->
    ByteString.ByteString ->
    Text ->
    [Int] ->
    Maybe LazyByteString.ByteString ->
    IO (Either AgentServerClientError responseBody)
performJsonRequest =
    performJsonRequestWithLimit maximumJsonResponseBytes

performJsonRequestWithLimit ::
    (FromJSON responseBody) =>
    Int ->
    AgentServerClient ->
    ByteString.ByteString ->
    Text ->
    [Int] ->
    Maybe LazyByteString.ByteString ->
    IO (Either AgentServerClientError responseBody)
performJsonRequestWithLimit
    maximumResponseBytes
    client
    method
    path
    expectedStatuses
    requestBody = do
        outcome <- Exception.try @IO @HttpException do
            baseRequest <- authenticatedRequest client method path
            let request = case requestBody of
                    Nothing -> baseRequest
                    Just body ->
                        baseRequest
                            { requestHeaders =
                                (hContentType, "application/json")
                                    : baseRequest.requestHeaders
                            , requestBody = RequestBodyLBS body
                            }
            withResponse request client.clientManager \response ->
                if statusCode response.responseStatus `elem` expectedStatuses
                    then
                        readBoundedBody
                            maximumResponseBytes
                            response.responseBody
                            >>= \case
                                Left err -> pure (Left err)
                                Right bytes ->
                                    pure $
                                        Bifunctor.first
                                            (AgentServerDecodeError . Text.pack)
                                            (Aeson.eitherDecodeStrict' bytes)
                    else decodeErrorResponse response
        pure case outcome of
            Left _ ->
                Left
                    ( AgentServerTransportError
                        "agent-server HTTP request failed"
                    )
            Right result -> result

authenticatedRequest ::
    AgentServerClient ->
    ByteString.ByteString ->
    Text ->
    IO Request
authenticatedRequest client method path = do
    request <- parseRequest (Text.unpack (client.clientBaseUrl <> path))
    pure
        request
            { method = method
            , requestHeaders =
                [
                    ( hAuthorization
                    , "Bearer " <> client.clientBearerToken
                    )
                , (hAccept, "application/json")
                ]
            , redirectCount = 0
            , responseTimeout = responseTimeoutMicro 30_000_000
            , checkResponse = \_ _ -> pure ()
            }

validateAgentServerBaseUrl ::
    Text ->
    IO (Either AgentServerClientError Text)
validateAgentServerBaseUrl raw = do
    parsed <-
        Exception.try @IO @HttpException $
            parseRequest (Text.unpack raw)
    pure case parsed of
        Left _ ->
            Left
                ( AgentServerProtocolError
                    "agent-server base URL is invalid"
                )
        Right request
            | request.path /= "/"
                || not (ByteString.null request.queryString) ->
                Left
                    ( AgentServerProtocolError
                        "agent-server base URL must not contain a path or query"
                    )
            | lookup hAuthorization request.requestHeaders /= Nothing ->
                Left
                    ( AgentServerProtocolError
                        "agent-server base URL must not contain user info"
                    )
            | not request.secure
                && not (isLiteralLoopback request.host) ->
                Left
                    ( AgentServerProtocolError
                        "plaintext agent-server URLs must use a literal loopback host"
                    )
            | otherwise ->
                Right (Text.dropWhileEnd (== '/') (Text.strip raw))

isLiteralLoopback :: ByteString.ByteString -> Bool
isLiteralLoopback rawHost =
    ByteString8.map toLower rawHost
        `elem` ["127.0.0.1", "::1"]

decodeErrorResponse ::
    Response BodyReader ->
    IO (Either AgentServerClientError value)
decodeErrorResponse response = do
    body <- readBoundedBody maximumJsonResponseBytes response.responseBody
    pure case body of
        Left err -> Left err
        Right bytes ->
            case Aeson.eitherDecodeStrict' bytes of
                Right envelope ->
                    let serverError =
                            ( envelope ::
                                AgentServerErrorEnvelope
                            ).agentServerError
                     in Left
                            ( AgentServerHttpError
                                (statusCode response.responseStatus)
                                ( Just
                                    ( Text.take
                                        128
                                        serverError.agentServerErrorCode
                                    )
                                )
                                ( Text.take
                                    1000
                                    serverError.agentServerErrorMessage
                                )
                            )
                Left _ ->
                    Left
                        ( AgentServerHttpError
                            (statusCode response.responseStatus)
                            Nothing
                            "agent-server returned a non-success response"
                        )

readBoundedBody ::
    Int ->
    BodyReader ->
    IO (Either AgentServerClientError ByteString.ByteString)
readBoundedBody maximumBytes = go 0 []
  where
    go total chunks bodyReader = do
        chunk <- brRead bodyReader
        if ByteString.null chunk
            then pure (Right (ByteString.concat (reverse chunks)))
            else do
                let nextTotal = total + ByteString.length chunk
                if nextTotal > maximumBytes
                    then
                        pure
                            ( Left
                                ( AgentServerProtocolError
                                    "agent-server response exceeded the size limit"
                                )
                            )
                    else go nextTotal (chunk : chunks) bodyReader

readPrivateBearerToken ::
    FilePath ->
    IO (Either AgentServerClientError ByteString.ByteString)
readPrivateBearerToken path = do
    inspected <- Exception.try @IO @IOException do
        Exception.bracket
            ( openFd
                path
                ReadOnly
                defaultFileFlags
                    { nofollow = True
                    , cloexec = True
                    }
            )
            closeFd
            \descriptor -> do
                status <- getFdStatus descriptor
                effectiveUser <- getEffectiveUserID
                if not (isRegularFile status)
                    then pure (Left "credential path is not a regular file")
                    else
                        if fileOwner status /= effectiveUser
                            then
                                pure
                                    ( Left
                                        "credential file is not owned by this process"
                                    )
                            else
                                if fileMode status Bits..&. 0o077 /= 0
                                    then
                                        pure
                                            ( Left
                                                "credential file permissions are too broad"
                                            )
                                    else
                                        validateBearerToken
                                            <$> readFdBytes
                                                (maximumCredentialBytes + 1)
                                                descriptor
    pure case inspected of
        Left _ ->
            Left
                ( AgentServerCredentialError
                    "could not securely read the agent-server credential"
                )
        Right result ->
            Bifunctor.first AgentServerCredentialError result

validateBearerToken ::
    ByteString.ByteString ->
    Either Text ByteString.ByteString
validateBearerToken raw = do
    when (ByteString.length raw > maximumCredentialBytes) $
        Left "agent-server credential is too large"
    let token = stripAsciiWhitespace raw
    when (ByteString.length token < 32) $
        Left "agent-server credential is too short"
    when (ByteString.any (\byte -> byte < 33 || byte > 126) token) $
        Left "agent-server credential contains invalid header bytes"
    pure token

readFdBytes :: Int -> Fd -> IO ByteString.ByteString
readFdBytes maximumBytes = go maximumBytes []
  where
    go remaining chunks descriptor
        | remaining <= 0 =
            pure (ByteString.concat (reverse chunks))
        | otherwise =
            Exception.tryIO
                (Posix.fdRead descriptor (fromIntegral remaining))
                >>= \case
                    Left err
                        | isEOFError err ->
                            pure (ByteString.concat (reverse chunks))
                        | otherwise -> Exception.throwIO err
                    Right chunk
                        | ByteString.null chunk ->
                            pure (ByteString.concat (reverse chunks))
                        | otherwise ->
                            go
                                (remaining - ByteString.length chunk)
                                (chunk : chunks)
                                descriptor

encodePathSegment :: Text -> Text
encodePathSegment =
    TextEncoding.decodeUtf8
        . urlEncode False
        . TextEncoding.encodeUtf8

encodeQueryValue :: Text -> Text
encodeQueryValue =
    TextEncoding.decodeUtf8
        . urlEncode True
        . TextEncoding.encodeUtf8

stripAsciiWhitespace ::
    ByteString.ByteString ->
    ByteString.ByteString
stripAsciiWhitespace =
    ByteString8.dropWhileEnd isSpaceByte
        . ByteString8.dropWhile isSpaceByte
  where
    isSpaceByte character =
        character == ' '
            || character == '\t'
            || character == '\r'
            || character == '\n'

takeSseFrame ::
    ByteString.ByteString ->
    Maybe (ByteString.ByteString, ByteString.ByteString)
takeSseFrame bytes =
    case catMaybes [candidate "\n\n", candidate "\r\n\r\n"] of
        [] -> Nothing
        candidates ->
            let (offset, separatorLength) =
                    minimumBy
                        (\left right -> compare (fst left) (fst right))
                        candidates
             in Just
                    ( ByteString.take offset bytes
                    , ByteString.drop (offset + separatorLength) bytes
                    )
  where
    candidate separator =
        let (prefix, suffix) = ByteString.breakSubstring separator bytes
         in if ByteString.null suffix
                then Nothing
                else Just (ByteString.length prefix, ByteString.length separator)

maximumJsonResponseBytes :: Int
maximumJsonResponseBytes = 1024 * 1024

maximumHistoryResponseBytes :: Int
maximumHistoryResponseBytes = 16 * 1024 * 1024

maximumTurnListResponseBytes :: Int
maximumTurnListResponseBytes = 32 * 1024 * 1024

maximumRequestListResponseBytes :: Int
maximumRequestListResponseBytes = 16 * 1024 * 1024

maximumSseBufferBytes :: Int
maximumSseBufferBytes = 1024 * 1024

durableTurnPollIntervalMicros :: Int
durableTurnPollIntervalMicros = 1000 * 1000

maximumCredentialBytes :: Int
maximumCredentialBytes = 4096
