-- | Device and browser authorization workflows and token exchange.
module Agent.CLI.Gateway.OAuth
    ( connectGatewayBrowser
    , connectGatewayBrowserWithCancel
    , startGatewayAuthorization
    , startNativeGatewayAuthorization
    , pollGatewayAuthorization
    , pollNativeGatewayAuthorizationAndSave
    , pollNativeGatewayAuthorizationAndSaveWith
    , exchangeNativeGatewayAuthorizationCode
    , exchangeNativeGatewayAuthorizationCodeWith
    , openGatewayAuthorizationPage
    , pollUntilAuthorized
    ) where

import Agent.CLI.Gateway.Credentials
    ( saveGatewayCredential
    , saveGatewayCredentialWith
    )
import Agent.CLI.Gateway.OAuth.Protocol
import Agent.CLI.Gateway.Origin (validateBaseUrl)
import Agent.Json.Decode qualified as Hermes
import Agent.Server.Client.GatewayIdentity (GatewayCredential(..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.MVar
    ( newEmptyMVar
    , putMVar
    , readMVar
    , takeMVar
    , tryPutMVar
    )
import Control.Concurrent.STM (atomically, retry)
import Control.Exception.Safe (bracket, bracketOnError, finally, tryAny)
import Control.Monad (void)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as AesonTypes
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as Base64Url
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types
    ( hCacheControl
    , hConnection
    , hContentLength
    , hContentType
    , status200
    , statusCode
    , statusIsSuccessful
    )
import Network.HTTP.Types.URI (queryToQueryText, renderQueryText)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Socket
    ( Family(AF_INET)
    , SockAddr(SockAddrInet)
    , Socket
    , SocketOption(ReuseAddr)
    , SocketType(Stream)
    , bind
    , close
    , defaultProtocol
    , getSocketName
    , listen
    , setSocketOption
    , socket
    , tupleToHostAddress
    )
import System.Entropy (getEntropy)
import System.Exit (ExitCode(..))
import System.Process (rawSystem)
import System.Timeout (timeout)

startGatewayAuthorization
    :: Text
    -> IO (Either Text GatewayAuthorization)
startGatewayAuthorization rawBaseUrl =
    startGatewayAuthorizationWithClient rawBaseUrl "haskell-agent"

-- | Start the device flow used by native clients without opening a browser or
-- beginning a poll loop.
startNativeGatewayAuthorization
    :: Text
    -> Text
    -> IO (Either Text GatewayDeviceAuthorization)
startNativeGatewayAuthorization rawBaseUrl rawClientName =
    fmap (fmap (.authorizationDevice)) $
        startGatewayAuthorizationWithClient rawBaseUrl rawClientName

startGatewayAuthorizationWithClient
    :: Text
    -> Text
    -> IO (Either Text GatewayAuthorization)
startGatewayAuthorizationWithClient rawBaseUrl rawClientName =
    case validateBaseUrl rawBaseUrl of
        Left err -> pure (Left err)
        Right baseUrl
            | Text.null clientName || Text.length clientName > 160 ->
                pure
                    (Left
                        "Gateway client name must contain between 1 and 160 characters.")
            | otherwise -> do
                tryAny newTlsManager >>= \case
                    Left exception ->
                        pure (Left (Text.pack (show exception)))
                    Right manager -> do
                        result <-
                            postJson manager
                                (baseUrl
                                    <> "/api/v1/agent-connections/device")
                                (Aeson.object
                                    [ "client_name"
                                        .= clientName
                                    ])
                                gatewayDeviceDecoder
                        pure do
                            device <- result
                            GatewayAuthorization baseUrl
                                <$> validateGatewayDeviceAuthorization
                                    baseUrl
                                    device
  where
    clientName = Text.strip rawClientName

pollGatewayAuthorization
    :: GatewayAuthorization
    -> IO (Either Text GatewayPollResult)
pollGatewayAuthorization authorization = do
    tryAny newTlsManager >>= \case
        Left exception ->
            pure (Left (Text.pack (show exception)))
        Right manager ->
            pollGatewayAuthorizationWith manager authorization

openGatewayAuthorizationPage :: GatewayAuthorization -> IO Bool
openGatewayAuthorizationPage =
    openBrowser
        . (.verificationUriComplete)
        . (.authorizationDevice)

-- | Complete the hosted gateway's browser OAuth flow through a loopback
-- callback. The injected presenter normally opens the URL in the user's
-- browser; keeping it injectable makes the network-independent contract
-- testable and lets interactive callers choose their own presentation.
connectGatewayBrowser
    :: Text
    -> Text
    -> (Text -> IO Bool)
    -> IO (Either Text ())
connectGatewayBrowser rawBaseUrl rawClientName present =
    connectGatewayBrowserWithCancel
        rawBaseUrl
        rawClientName
        present
        (atomically retry)

-- | Browser authorization with an explicit cancellation signal. Callers that
-- own an interactive prompt can complete the supplied action to stop waiting
-- for the loopback callback without waiting for the five-minute timeout.
connectGatewayBrowserWithCancel
    :: Text
    -> Text
    -> (Text -> IO Bool)
    -> IO ()
    -> IO (Either Text ())
connectGatewayBrowserWithCancel
    rawBaseUrl rawClientName present waitForCancellation =
    case validateBaseUrl rawBaseUrl of
        Left err -> pure (Left err)
        Right baseUrl -> do
            attempted <-
                tryAny $
                    bracket openGatewayCallbackSocket close $
                        runBrowserFlow baseUrl
            pure case attempted of
                Left _ ->
                    Left
                        "Gateway browser authorization failed unexpectedly."
                Right result -> result
  where
    runBrowserFlow baseUrl listener = do
        redirectUri <- gatewayLoopbackRedirectUri listener
        verifier <- randomUrlText 32
        state <- randomUrlText 32
        case
            gatewayAuthorizationUrl
                baseUrl
                redirectUri
                state
                (gatewayPkceChallenge verifier)
                rawClientName of
            Left err -> pure (Left err)
            Right authorizationUrl -> do
                presented <- present authorizationUrl
                if not presented
                    then
                        pure
                            (Left
                                "Could not open the gateway authorization page.")
                    else do
                        callback <-
                            timeout
                                gatewayBrowserTimeoutMicroseconds
                                (race
                                    waitForCancellation
                                    (receiveGatewayAuthorizationCallback
                                        listener state))
                        case callback of
                            Nothing ->
                                pure
                                    (Left
                                        "Gateway browser authorization timed out.")
                            Just (Left ()) ->
                                pure
                                    (Left
                                        "Gateway browser authorization was cancelled.")
                            Just (Right (Left err)) -> pure (Left err)
                            Just (Right (Right authorizationCode)) ->
                                exchangeGatewayAuthorizationCode
                                    baseUrl
                                    redirectUri
                                    verifier
                                    authorizationCode
                                    >>= \case
                                        Left err -> pure (Left err)
                                        Right response ->
                                            case
                                                validateGatewayAuthorizationCodeResponse
                                                    baseUrl response of
                                                Left err -> pure (Left err)
                                                Right credential ->
                                                    saveGatewayCredential
                                                        credential

gatewayBrowserTimeoutMicroseconds :: Int
gatewayBrowserTimeoutMicroseconds = 5 * 60 * 1_000_000

pollUntilAuthorized
    :: HTTP.Manager
    -> GatewayAuthorization
    -> IO GatewayCredential
pollUntilAuthorized manager authorization =
    go device.expiresInSeconds (max 1 device.pollIntervalSeconds)
  where
    baseUrl = authorization.authorizationBaseUrl
    device = authorization.authorizationDevice
    go remaining interval
        | remaining <= 0 = failText "Gateway authorization expired."
        | otherwise = do
            threadDelay (interval * 1_000_000)
            result <-
                pollGatewayAuthorizationWith manager authorization
                    >>= either failText pure
            case result of
                GatewayAuthorized accessToken websocketUrl ->
                    pure
                        GatewayCredential
                            { gatewayBaseUrl = baseUrl
                            , gatewayWebSocketUrl = websocketUrl
                            , gatewayAccessToken = accessToken
                            }
                GatewayAuthorizationPending serverInterval ->
                    let next = maybe interval (max 1) serverInterval
                     in go (remaining - next) next
                GatewaySlowDown serverInterval ->
                    let next =
                            maybe
                                (interval + 5)
                                (max (interval + 5))
                                serverInterval
                     in go (remaining - next) next
                GatewayAccessDenied -> failText "Gateway authorization was denied."
                GatewayExpired -> failText "Gateway authorization expired."
                GatewayPollFailed code ->
                    failText ("Gateway authorization failed: " <> code)

pollGatewayAuthorizationWith
    :: HTTP.Manager
    -> GatewayAuthorization
    -> IO (Either Text GatewayPollResult)
pollGatewayAuthorizationWith manager authorization =
    pollGatewayDeviceCodeWith
        manager
        authorization.authorizationBaseUrl
        authorization.authorizationDevice.deviceCode

pollGatewayDeviceCodeWith
    :: HTTP.Manager
    -> Text
    -> Text
    -> IO (Either Text GatewayPollResult)
pollGatewayDeviceCodeWith manager baseUrl deviceCode =
    postJson
        manager
        (baseUrl <> "/api/v1/agent-connections/token")
        (Aeson.object
            [ "device_code" .= deviceCode ])
        gatewayPollDecoder

-- | Perform one native device-flow poll and persist an authorized credential.
-- Secret token fields remain inside the trusted Haskell runtime.
pollNativeGatewayAuthorizationAndSave
    :: Text
    -> Text
    -> IO (Either Text GatewayPollResult)
pollNativeGatewayAuthorizationAndSave rawBaseUrl rawDeviceCode =
    pollNativeGatewayAuthorizationAndSaveWith
        rawBaseUrl rawDeviceCode (const (pure ()))

pollNativeGatewayAuthorizationAndSaveWith
    :: Text
    -> Text
    -> (GatewayPollResult -> IO ())
    -> IO (Either Text GatewayPollResult)
pollNativeGatewayAuthorizationAndSaveWith
        rawBaseUrl rawDeviceCode afterAuthorized =
    case validateBaseUrl rawBaseUrl of
        Left err -> pure (Left err)
        Right baseUrl
            | Text.null deviceCode || Text.length deviceCode > 4096 ->
                pure (Left "Gateway device code is invalid.")
            | otherwise ->
                tryAny newTlsManager >>= \case
                    Left exception ->
                        pure (Left (Text.pack (show exception)))
                    Right manager ->
                        pollGatewayDeviceCodeWith manager baseUrl deviceCode
                            >>= \case
                                Left err -> pure (Left err)
                                Right
                                    authorized@(GatewayAuthorized
                                        accessToken
                                        websocketUrl) -> do
                                        saveGatewayCredentialWith
                                            GatewayCredential
                                                { gatewayBaseUrl = baseUrl
                                                , gatewayWebSocketUrl =
                                                    websocketUrl
                                                , gatewayAccessToken =
                                                    accessToken
                                                }
                                            (afterAuthorized authorized)
                                            >>= \case
                                                Left err -> pure (Left err)
                                                Right () ->
                                                    pure (Right authorized)
                                Right result -> pure (Right result)
  where
    deviceCode = Text.strip rawDeviceCode

postJson
    :: HTTP.Manager
    -> Text
    -> Aeson.Value
    -> Hermes.Decoder value
    -> IO (Either Text value)
postJson manager url payload decoder = do
    parsed <- tryAny (HTTP.parseRequest (Text.unpack url))
    case parsed of
        Left exception -> pure (Left (Text.pack (show exception)))
        Right initial -> do
            response <-
                tryAny $
                    HTTP.httpLbs
                        initial
                            { HTTP.method = "POST"
                            , HTTP.requestHeaders = [(hContentType, "application/json")]
                            , HTTP.requestBody = HTTP.RequestBodyLBS (Aeson.encode payload)
                            , HTTP.checkResponse = \_ _ -> pure ()
                            }
                        manager
            pure case response of
                Left _ ->
                    Left "Could not reach the gateway authorization endpoint."
                Right value ->
                    case Hermes.decodeEither decoder (LBS.toStrict (HTTP.responseBody value)) of
                        Left err -> Left (Hermes.jsonErrorMessage err)
                        Right decoded -> Right decoded

exchangeGatewayAuthorizationCode
    :: Text
    -> Text
    -> Text
    -> Text
    -> IO (Either Text GatewayAuthorizationCodeResponse)
exchangeGatewayAuthorizationCode
    baseUrl redirectUri verifier authorizationCode = do
        manager <- newTlsManager
        postGatewayOAuthForm
            manager
            (baseUrl <> "/api/v1/agent-connections/oauth/token")
            [ ("grant_type", "authorization_code")
            , ("client_id", gatewayBrowserClientId)
            , ("code", authorizationCode)
            , ("code_verifier", verifier)
            , ("redirect_uri", redirectUri)
            ]

-- | Exchange the registered macOS custom-scheme callback and persist the
-- resulting credential without exposing its secret fields through the ABI.
exchangeNativeGatewayAuthorizationCode
    :: Text
    -> Text
    -> Text
    -> Text
    -> Text
    -> IO (Either Text ())
exchangeNativeGatewayAuthorizationCode
    rawBaseUrl clientId authorizationCode verifier redirectUri =
        exchangeNativeGatewayAuthorizationCodeWith
            rawBaseUrl
            clientId
            authorizationCode
            verifier
            redirectUri
            (pure ())

exchangeNativeGatewayAuthorizationCodeWith
    :: Text
    -> Text
    -> Text
    -> Text
    -> Text
    -> IO ()
    -> IO (Either Text ())
exchangeNativeGatewayAuthorizationCodeWith
    rawBaseUrl clientId authorizationCode verifier redirectUri afterSave =
        case validateNativeGatewayAuthorizationExchange
            rawBaseUrl
            clientId
            authorizationCode
            verifier
            redirectUri of
            Left err -> pure (Left err)
            Right baseUrl ->
                tryAny newTlsManager >>= \case
                    Left _ ->
                        pure
                            (Left
                                "Could not prepare the gateway token request.")
                    Right manager ->
                        postGatewayOAuthForm
                            manager
                            (baseUrl
                                <> "/api/v1/agent-connections/oauth/token")
                            [ ("grant_type", "authorization_code")
                            , ("client_id", clientId)
                            , ("code", authorizationCode)
                            , ("code_verifier", verifier)
                            , ("redirect_uri", redirectUri)
                            ]
                            >>= \case
                                Left err -> pure (Left err)
                                Right response ->
                                    case
                                        validateGatewayAuthorizationCodeResponse
                                            baseUrl
                                            response of
                                        Left err -> pure (Left err)
                                        Right credential ->
                                            saveGatewayCredentialWith
                                                credential
                                                afterSave

postGatewayOAuthForm
    :: HTTP.Manager
    -> Text
    -> [(Text, Text)]
    -> IO (Either Text GatewayAuthorizationCodeResponse)
postGatewayOAuthForm manager url fields = do
    parsed <- tryAny (HTTP.parseRequest (Text.unpack url))
    case parsed of
        Left _ ->
            pure (Left "Could not prepare the gateway token request.")
        Right initial -> do
            response <-
                tryAny $
                    HTTP.httpLbs
                        initial
                            { HTTP.method = "POST"
                            , HTTP.requestHeaders =
                                [ ( hContentType
                                  , "application/x-www-form-urlencoded"
                                  )
                                ]
                            , HTTP.requestBody =
                                HTTP.RequestBodyLBS $
                                    Builder.toLazyByteString $
                                        renderQueryText
                                            False
                                            (fmap
                                                (\(name, value) ->
                                                    (name, Just value))
                                                fields)
                            , HTTP.checkResponse = \_ _ -> pure ()
                            , HTTP.redirectCount = 0
                            , HTTP.responseTimeout =
                                HTTP.responseTimeoutMicro (30 * 1_000_000)
                            }
                        manager
            pure case response of
                Left _ ->
                    Left "Could not reach the gateway OAuth token endpoint."
                Right value
                    | statusIsSuccessful (HTTP.responseStatus value) ->
                        case Hermes.decodeEither
                            gatewayAuthorizationCodeDecoder
                            (LBS.toStrict (HTTP.responseBody value)) of
                            Left _ ->
                                Left
                                    "The gateway returned an invalid authorization response."
                            Right decoded -> Right decoded
                    | otherwise ->
                        Left $
                            gatewayOAuthResponseError
                                (statusCode (HTTP.responseStatus value))
                                (HTTP.responseBody value)

gatewayOAuthResponseError :: Int -> LBS.ByteString -> Text
gatewayOAuthResponseError responseStatus body =
    "Gateway authorization failed"
        <> maybe
            (" (HTTP " <> Text.pack (show responseStatus) <> ").")
            (<> ".")
            (safeOAuthErrorCode body)

safeOAuthErrorCode :: LBS.ByteString -> Maybe Text
safeOAuthErrorCode body = do
    value <- Aeson.decode body
    rawCode <- case value of
        Aeson.Object object ->
            AesonTypes.parseMaybe
                (\fields -> fields Aeson..: "error")
                object
        _ -> Nothing
    let code = Text.take 64 (Text.strip rawCode)
    if Text.null code
        || not
            (Text.all
                (\character ->
                    isAsciiAlphaNumeric character
                        || character `elem` ("_-" :: String))
                code)
        then Nothing
        else Just (": " <> code)

openGatewayCallbackSocket :: IO Socket
openGatewayCallbackSocket =
    bracketOnError
        (socket AF_INET Stream defaultProtocol)
        close
        \listener -> do
            setSocketOption listener ReuseAddr 1
            bind listener
                (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
            listen listener 1
            pure listener

gatewayLoopbackRedirectUri :: Socket -> IO Text
gatewayLoopbackRedirectUri listener =
    getSocketName listener >>= \case
        SockAddrInet port _ ->
            pure
                ("http://127.0.0.1:"
                    <> Text.pack (show port)
                    <> gatewayBrowserRedirectPath)
        _ -> failText "Gateway OAuth callback did not bind IPv4 loopback."

receiveGatewayAuthorizationCallback
    :: Socket
    -> Text
    -> IO (Either Text Text)
receiveGatewayAuthorizationCallback listener expectedState = do
    resultVar <- newEmptyMVar
    shutdownVar <- newEmptyMVar
    let settings =
            Warp.setHost "127.0.0.1"
                $ Warp.setMaxTotalHeaderLength 16_384
                $ Warp.setInstallShutdownHandler
                    (\shutdown -> putMVar shutdownVar shutdown)
                $ Warp.defaultSettings
        application request respond = do
            let result =
                    validateGatewayAuthorizationParameters
                        expectedState
                        (Wai.requestMethod request)
                        (Wai.rawPathInfo request)
                        (not (BS.null (Wai.rawQueryString request)))
                        (queryToQueryText (Wai.queryString request))
                page = gatewayCallbackPage result
                finish = do
                    void (tryPutMVar resultVar result)
                    readMVar shutdownVar >>= id
            respond
                (Wai.responseLBS
                    status200
                    [ (hContentType, "text/html; charset=utf-8")
                    , (hCacheControl, "no-store")
                    , (hConnection, "close")
                    , ( "Content-Security-Policy"
                      , "default-src 'none'; style-src 'unsafe-inline'"
                      )
                    , ("X-Content-Type-Options", "nosniff")
                    , (hContentLength, BS8.pack (show (BS.length page)))
                    ]
                    (LBS.fromStrict page))
                `finally` finish
    Warp.runSettingsSocket settings listener application
    takeMVar resultVar

gatewayCallbackPage :: Either Text Text -> BS.ByteString
gatewayCallbackPage result =
    TextEncoding.encodeUtf8 $
        "<!doctype html><html><head><meta charset=\"utf-8\">\
        \<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">\
        \<title>Haskell Agent</title></head>\
        \<body style=\"font-family:system-ui;margin:3rem;max-width:40rem\">\
        \<h1>"
            <> title
            <> "</h1><p>"
            <> message
            <> "</p></body></html>"
  where
    (title, message) = case result of
        Right _ ->
            ( "Authorization received"
            , "Return to Haskell Agent while it finishes connecting."
            )
        Left _ ->
            ( "Haskell Agent could not connect"
            , "Return to Haskell Agent to see the error and try again."
            )

randomUrlText :: Int -> IO Text
randomUrlText byteCount =
    TextEncoding.decodeUtf8
        . Base64Url.encodeUnpadded
        <$> getEntropy byteCount

openBrowser :: Text -> IO Bool
openBrowser url = do
    result <- tryAny (rawSystem "open" [Text.unpack url])
    case result of
        Right ExitSuccess -> pure True
        _ -> do
            fallback <- tryAny (rawSystem "xdg-open" [Text.unpack url])
            pure case fallback of
                Right ExitSuccess -> True
                _ -> False

failText :: Text -> IO a
failText = ioError . userError . Text.unpack
