-- | HTTPS device authorization and restricted gateway credential storage.
module Agent.CLI.GatewayClient
    ( GatewayCredential(..)
    , GatewayAuthorization(..)
    , GatewayAuthorizationCodeResponse(..)
    , GatewayDeviceAuthorization(..)
    , GatewayPollResult(..)
    , connectGatewayBrowser
    , connectGatewayBrowserWithCancel
    , defaultGatewayBaseUrl
    , gatewayAuthorizationCodeDecoder
    , gatewayAuthorizationUrl
    , gatewayBrowserClientId
    , gatewayBrowserRedirectPath
    , gatewayPkceChallenge
    , validateGatewayAuthorizationCallback
    , validateGatewayAuthorizationCodeResponse
    , startGatewayAuthorization
    , pollGatewayAuthorization
    , saveGatewayCredential
    , removeGatewayCredential
    , openGatewayAuthorizationPage
    , connectGateway
    , disconnectGateway
    , gatewayCredentialPath
    , gatewayDeviceDecoder
    , gatewayPollDecoder
    , loadGatewayCredential
    , loadGatewayCredentialAt
    , runGatewayCommand
    , saveGatewayCredentialAt
    , showGatewayStatus
    , validateBaseUrl
    ) where

import Agent.FileRetry (retryOnFileBusy, writeLazyFileAtomically)
import Agent.CLI.Options (GatewayCommand (..))
import Agent.Json.Decode qualified as Hermes
import Agent.OpenAI.WebSocketClient (validateGatewayWebSocketUrl)
import Agent.OsPath (unsafeToFilePath)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM (atomically, retry)
import Control.Exception.Safe (bracket, bracketOnError, tryAny)
import Control.Monad (when)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as AesonTypes
import Data.ByteArray qualified as ByteArray
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as Base64Url
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types
    ( hContentType
    , statusCode
    , statusIsSuccessful
    )
import Network.HTTP.Types.URI (parseQueryText, renderQueryText)
import Network.URI qualified as URI
import Network.Socket
    ( Family(AF_INET)
    , SockAddr(SockAddrInet)
    , Socket
    , SocketOption(ReuseAddr)
    , SocketType(Stream)
    , accept
    , bind
    , close
    , defaultProtocol
    , getSocketName
    , listen
    , setSocketOption
    , socket
    , tupleToHostAddress
    )
import Network.Socket.ByteString qualified as Socket
import System.Directory.OsPath qualified as Directory
import System.Entropy (getEntropy)
import System.Exit (ExitCode (..))
import System.OsPath (OsPath, takeDirectory, unsafeEncodeUtf, (</>))
import System.Posix.Files (setFileMode)
import System.Process (rawSystem)
import System.Timeout (timeout)
import Text.Read (readMaybe)

data GatewayCredential = GatewayCredential
    { gatewayBaseUrl :: !Text
    , gatewayWebSocketUrl :: !Text
    , gatewayAccessToken :: !Text
    }
    deriving (Eq)

-- | The hosted gateway selected by the interactive @/login@ flow.
defaultGatewayBaseUrl :: Text
defaultGatewayBaseUrl = "https://platform.digitallyinduced.com"

-- | Public OAuth client registered for the terminal application's loopback
-- Authorization Code + PKCE flow.
gatewayBrowserClientId :: Text
gatewayBrowserClientId = "haskell-agent-cli"

gatewayBrowserRedirectPath :: Text
gatewayBrowserRedirectPath = "/oauth2callback"

-- | A validated gateway base URL paired with its short-lived device flow.
--
-- Keeping the normalized base URL in the value prevents UI callers from
-- accidentally saving a different origin from the one that issued the code.
data GatewayAuthorization = GatewayAuthorization
    { authorizationBaseUrl :: !Text
    , authorizationDevice :: !GatewayDeviceAuthorization
    }
    deriving (Eq)

instance Show GatewayAuthorization where
    show authorization =
        "GatewayAuthorization { authorizationBaseUrl = "
            <> show authorization.authorizationBaseUrl
            <> ", authorizationDevice = <redacted> }"

instance Show GatewayCredential where
    show credential =
        "GatewayCredential { gatewayBaseUrl = "
            <> show credential.gatewayBaseUrl
            <> ", gatewayWebSocketUrl = "
            <> show credential.gatewayWebSocketUrl
            <> ", gatewayAccessToken = <redacted> }"

instance Aeson.ToJSON GatewayCredential where
    toJSON credential =
        Aeson.object
            [ "version" .= (1 :: Int)
            , "base_url" .= credential.gatewayBaseUrl
            , "websocket_url" .= credential.gatewayWebSocketUrl
            , "access_token" .= credential.gatewayAccessToken
            ]

data GatewayAuthorizationCodeResponse = GatewayAuthorizationCodeResponse
    { authorizationAccessToken :: !Text
    , authorizationTokenType :: !Text
    , authorizationResponseBaseUrl :: !Text
    , authorizationWebSocketUrl :: !Text
    }
    deriving (Eq)

instance Show GatewayAuthorizationCodeResponse where
    show response =
        "GatewayAuthorizationCodeResponse"
            <> " { authorizationAccessToken = <redacted>"
            <> ", authorizationTokenType = "
            <> show response.authorizationTokenType
            <> ", authorizationResponseBaseUrl = "
            <> show response.authorizationResponseBaseUrl
            <> ", authorizationWebSocketUrl = <redacted> }"

gatewayAuthorizationCodeDecoder
    :: Hermes.Decoder GatewayAuthorizationCodeResponse
gatewayAuthorizationCodeDecoder =
    Hermes.object $
        GatewayAuthorizationCodeResponse
            <$> Hermes.atKey "access_token" Hermes.text
            <*> Hermes.atKey "token_type" Hermes.text
            <*> Hermes.atKey "base_url" Hermes.text
            <*> Hermes.atKey "websocket_url" Hermes.text

gatewayCredentialDecoder :: Hermes.Decoder GatewayCredential
gatewayCredentialDecoder =
    Hermes.object $
        GatewayCredential
            <$> Hermes.atKey "base_url" Hermes.text
            <*> Hermes.atKey "websocket_url" Hermes.text
            <*> Hermes.atKey "access_token" Hermes.text

data GatewayDeviceAuthorization = GatewayDeviceAuthorization
    { deviceCode :: !Text
    , userCode :: !Text
    , verificationUri :: !Text
    , verificationUriComplete :: !Text
    , expiresInSeconds :: !Int
    , pollIntervalSeconds :: !Int
    }
    deriving (Eq)

instance Show GatewayDeviceAuthorization where
    show device =
        "GatewayDeviceAuthorization { deviceCode = <redacted>, userCode = "
            <> show device.userCode
            <> ", verificationUri = "
            <> show device.verificationUri
            <> ", verificationUriComplete = "
            <> show device.verificationUriComplete
            <> ", expiresInSeconds = "
            <> show device.expiresInSeconds
            <> ", pollIntervalSeconds = "
            <> show device.pollIntervalSeconds
            <> " }"

gatewayDeviceDecoder :: Hermes.Decoder GatewayDeviceAuthorization
gatewayDeviceDecoder =
    Hermes.object $
        GatewayDeviceAuthorization
            <$> Hermes.atKey "device_code" Hermes.text
            <*> Hermes.atKey "user_code" Hermes.text
            <*> Hermes.atKey "verification_uri" Hermes.text
            <*> Hermes.atKey "verification_uri_complete" Hermes.text
            <*> Hermes.atKey "expires_in" Hermes.int
            <*> Hermes.atKey "interval" Hermes.int

data GatewayPollResult
    = GatewayAuthorized !Text !Text
    | GatewayAuthorizationPending !(Maybe Int)
    | GatewaySlowDown !(Maybe Int)
    | GatewayAccessDenied
    | GatewayExpired
    | GatewayPollFailed !Text
    deriving (Eq)

instance Show GatewayPollResult where
    show result = case result of
        GatewayAuthorized _ websocketUrl ->
            "GatewayAuthorized <redacted> " <> show websocketUrl
        GatewayAuthorizationPending interval ->
            "GatewayAuthorizationPending " <> show interval
        GatewaySlowDown interval ->
            "GatewaySlowDown " <> show interval
        GatewayAccessDenied -> "GatewayAccessDenied"
        GatewayExpired -> "GatewayExpired"
        GatewayPollFailed code ->
            "GatewayPollFailed " <> show code

gatewayPollDecoder :: Hermes.Decoder GatewayPollResult
gatewayPollDecoder =
    Hermes.withOwnedRawJson \raw ->
        case Hermes.decodeEither successDecoder raw of
            Right (token, websocketUrl) ->
                pure (GatewayAuthorized token websocketUrl)
            Left _ -> case Hermes.decodeEither errorDecoder raw of
                Right (code, interval) -> pure case code of
                    "authorization_pending" -> GatewayAuthorizationPending interval
                    "slow_down" -> GatewaySlowDown interval
                    "access_denied" -> GatewayAccessDenied
                    "expired_token" -> GatewayExpired
                    other -> GatewayPollFailed other
                Left err -> fail (Text.unpack (Hermes.jsonErrorMessage err))
  where
    successDecoder =
        Hermes.object $
            (,)
                <$> Hermes.atKey "access_token" Hermes.text
                <*> Hermes.atKey "websocket_url" Hermes.text
    errorDecoder =
        Hermes.object $
            (,)
                <$> Hermes.atKey "error" Hermes.text
                <*> Hermes.optionalKey "interval" Hermes.int

gatewayCredentialPath :: OsPath -> OsPath
gatewayCredentialPath home =
    home
        </> unsafeEncodeUtf ".haskell-agent"
        </> unsafeEncodeUtf "credentials"
        </> unsafeEncodeUtf "gateway.json"

loadGatewayCredential :: IO (Either Text (Maybe GatewayCredential))
loadGatewayCredential = do
    home <- Directory.getHomeDirectory
    loadGatewayCredentialAt home

loadGatewayCredentialAt
    :: OsPath
    -> IO (Either Text (Maybe GatewayCredential))
loadGatewayCredentialAt home = do
    let path = gatewayCredentialPath home
    exists <- Directory.doesFileExist path
    if not exists
        then pure (Right Nothing)
        else do
            result <-
                retryOnFileBusy $
                    tryAny (LBS.readFile (unsafeToFilePath path))
            pure case result of
                Left exception -> Left (Text.pack (show exception))
                Right bytes ->
                    case Hermes.decodeEither gatewayCredentialDecoder (LBS.toStrict bytes) of
                        Left err -> Left (Hermes.jsonErrorMessage err)
                        Right credential ->
                            case validateGatewayCredential credential of
                                Left err -> Left err
                                Right () -> Right (Just credential)

saveGatewayCredentialAt :: OsPath -> GatewayCredential -> IO (Either Text ())
saveGatewayCredentialAt home credential =
    case validateGatewayCredential credential of
        Left err -> pure (Left err)
        Right () -> do
            let path = gatewayCredentialPath home
                directory = takeDirectory path
            result <- tryAny do
                Directory.createDirectoryIfMissing True directory
                setFileMode (unsafeToFilePath directory) 0o700
                writeLazyFileAtomically path 0o600 (Aeson.encode credential)
            pure case result of
                Left exception -> Left (Text.pack (show exception))
                Right () -> Right ()

validateGatewayCredential :: GatewayCredential -> Either Text ()
validateGatewayCredential credential = do
    _ <- validateBaseUrl credential.gatewayBaseUrl
    validateGatewayWebSocketUrl credential.gatewayWebSocketUrl
    whenEither
        (Text.null (Text.strip credential.gatewayAccessToken))
        "Gateway access token cannot be empty."
  where
    whenEither condition message
        | condition = Left message
        | otherwise = Right ()

saveGatewayCredential :: GatewayCredential -> IO (Either Text ())
saveGatewayCredential credential =
    tryAny Directory.getHomeDirectory >>= \case
        Left exception -> pure (Left (Text.pack (show exception)))
        Right home -> saveGatewayCredentialAt home credential

startGatewayAuthorization
    :: Text
    -> IO (Either Text GatewayAuthorization)
startGatewayAuthorization rawBaseUrl =
    case validateBaseUrl rawBaseUrl of
        Left err -> pure (Left err)
        Right baseUrl -> do
            tryAny newTlsManager >>= \case
                Left exception ->
                    pure (Left (Text.pack (show exception)))
                Right manager ->
                    fmap (GatewayAuthorization baseUrl) <$>
                        postJson manager
                            (baseUrl
                                <> "/api/v1/agent-connections/device")
                            (Aeson.object
                                [ "client_name"
                                    .= ("haskell-agent" :: Text)
                                ])
                            gatewayDeviceDecoder

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

-- | Construct the registered authorization request. The redirect is accepted
-- only when it is an IPv4 loopback URI with the exact callback path.
gatewayAuthorizationUrl
    :: Text
    -> Text
    -> Text
    -> Text
    -> Text
    -> Either Text Text
gatewayAuthorizationUrl
    rawBaseUrl redirectUri state challenge rawClientName = do
        baseUrl <- validateBaseUrl rawBaseUrl
        validateGatewayLoopbackRedirectUri redirectUri
        whenEither
            ( Text.length state < 32
                || Text.length state > 200
                || not (Text.all isPkceCharacter state)
            )
            "Gateway OAuth state is invalid."
        whenEither
            ( Text.length challenge /= 43
                || not (Text.all isBase64UrlCharacter challenge)
            )
            "Gateway PKCE challenge is invalid."
        whenEither
            (Text.null clientName || Text.length clientName > 160)
            "Gateway client name must contain between 1 and 160 characters."
        pure $
            baseUrl
                <> "/connect/agent/authorize"
                <> query
  where
    clientName = Text.strip rawClientName
    query =
        TextEncoding.decodeUtf8 $
            LBS.toStrict $
                Builder.toLazyByteString $
                    renderQueryText
                        True
                        [ ("response_type", Just "code")
                        , ("client_id", Just gatewayBrowserClientId)
                        , ("redirect_uri", Just redirectUri)
                        , ("state", Just state)
                        , ("code_challenge", Just challenge)
                        , ("code_challenge_method", Just "S256")
                        , ("client_name", Just clientName)
                        ]

gatewayPkceChallenge :: Text -> Text
gatewayPkceChallenge =
    TextEncoding.decodeUtf8
        . Base64Url.encodeUnpadded
        . ByteArray.convert
        . (hash :: BS.ByteString -> Digest SHA256)
        . TextEncoding.encodeUtf8

-- | Validate a complete HTTP callback request and return only the one-time
-- authorization code. Method, path, singleton state, and state value are
-- checked before an OAuth error or code is accepted.
validateGatewayAuthorizationCallback
    :: Text
    -> BS.ByteString
    -> Either Text Text
validateGatewayAuthorizationCallback expectedState request = do
    target <- case BS8.words requestLine of
        method : rawTarget : _
            | method == "GET" -> Right rawTarget
            | otherwise -> Left "Gateway OAuth callback must use GET."
        _ -> Left "Gateway OAuth callback request is malformed."
    let (path, rawQuery) = BS.break (== 63) target
    whenEither
        (path /= TextEncoding.encodeUtf8 gatewayBrowserRedirectPath)
        "Gateway OAuth callback path is invalid."
    whenEither
        (BS.null rawQuery)
        "Gateway OAuth callback query is missing."
    state <- singletonParameter "state" parameters
        >>= maybe
            (Left "Gateway OAuth callback state is missing.")
            Right
    whenEither
        (state /= expectedState)
        "Gateway OAuth callback state mismatch."
    case singletonParameter "error" parameters of
        Left err -> Left err
        Right (Just oauthError) ->
            Left
                ("Gateway authorization was not granted"
                    <> safeOAuthErrorSuffix oauthError)
        Right Nothing -> do
            code <- singletonParameter "code" parameters
                >>= maybe
                    (Left
                        "Gateway OAuth callback authorization code is missing.")
                    Right
            whenEither
                (Text.null code || Text.length code > 4096)
                "Gateway OAuth callback authorization code is invalid."
            pure code
  where
    requestLine = BS8.takeWhile (/= '\r') request
    parameters = parseQueryText (BS.drop 1 rawQueryBytes)
    (_, rawQueryBytes) =
        case BS8.words requestLine of
            _ : target : _ -> BS.break (== 63) target
            _ -> ("", "")

validateGatewayAuthorizationCodeResponse
    :: Text
    -> GatewayAuthorizationCodeResponse
    -> Either Text GatewayCredential
validateGatewayAuthorizationCodeResponse rawRequestedBaseUrl response = do
    requestedBaseUrl <- validateBaseUrl rawRequestedBaseUrl
    responseBaseUrl <-
        validateBaseUrl response.authorizationResponseBaseUrl
    requestedOrigin <-
        parseGatewayOrigin
            "Gateway URL is invalid."
            requestedBaseUrl
    responseOrigin <-
        parseGatewayOrigin
            "The gateway returned an invalid base URL."
            responseBaseUrl
    whenEither
        (response.authorizationTokenType /= "Bearer")
        "The gateway returned an unsupported token type."
    whenEither
        (requestedOrigin /= responseOrigin)
        "The gateway returned a credential for a different origin."
    websocketOrigin <-
        parseGatewayOrigin
            "The gateway returned an invalid WebSocket URL."
            response.authorizationWebSocketUrl
    let expectedWebSocketOrigin =
            case responseOrigin of
                ("https:", host, port) -> ("wss:", host, port)
                ("http:", host, port) -> ("ws:", host, port)
                origin -> origin
    whenEither
        (websocketOrigin /= expectedWebSocketOrigin)
        "The gateway returned a WebSocket URL for a different origin."
    let credential =
            GatewayCredential
                { gatewayBaseUrl = responseBaseUrl
                , gatewayWebSocketUrl =
                    response.authorizationWebSocketUrl
                , gatewayAccessToken =
                    response.authorizationAccessToken
                }
    validateGatewayCredential credential
    pure credential

connectGateway :: Text -> IO ()
connectGateway rawBaseUrl = do
    authorization <-
        startGatewayAuthorization rawBaseUrl >>= either failText pure
    manager <- newTlsManager
    let device = authorization.authorizationDevice
    putStrLn ("Enter code " <> Text.unpack device.userCode <> " at:")
    putStrLn (Text.unpack device.verificationUri)
    opened <- openGatewayAuthorizationPage authorization
    when (not opened) $
        putStrLn "Could not open a browser automatically."
    credential <- pollUntilAuthorized manager authorization
    saveGatewayCredential credential >>= either failText pure
    putStrLn "Gateway connection saved."

showGatewayStatus :: IO ()
showGatewayStatus =
    loadGatewayCredential >>= \case
        Left err -> failText err
        Right Nothing -> putStrLn "Not connected to a gateway."
        Right (Just credential) -> do
            putStrLn ("Connected to " <> Text.unpack credential.gatewayBaseUrl)
            putStrLn ("Responses WebSocket: " <> Text.unpack credential.gatewayWebSocketUrl)

disconnectGateway :: IO ()
disconnectGateway = do
    removeGatewayCredential >>= either failText pure
    putStrLn "Gateway connection removed."

removeGatewayCredential :: IO (Either Text ())
removeGatewayCredential = do
    result <- tryAny do
        home <- Directory.getHomeDirectory
        let path = gatewayCredentialPath home
        exists <- Directory.doesFileExist path
        when exists (Directory.removeFile path)
    pure case result of
        Left exception -> Left (Text.pack (show exception))
        Right () -> Right ()

runGatewayCommand :: GatewayCommand -> IO ()
runGatewayCommand = \case
    GatewayConnect url -> connectGateway url
    GatewayStatus -> showGatewayStatus
    GatewayDisconnect -> disconnectGateway

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
    postJson
        manager
        (authorization.authorizationBaseUrl
            <> "/api/v1/agent-connections/token")
        (Aeson.object
            [ "device_code"
                .= authorization.authorizationDevice.deviceCode
            ])
        gatewayPollDecoder

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
receiveGatewayAuthorizationCallback listener expectedState =
    bracket (fst <$> accept listener) close \connection -> do
        request <- receiveGatewayCallbackHeaders connection
        let result =
                request >>= validateGatewayAuthorizationCallback expectedState
            page = gatewayCallbackPage result
            response =
                "HTTP/1.1 200 OK\r\n\
                \Content-Type: text/html; charset=utf-8\r\n\
                \Cache-Control: no-store\r\n\
                \Content-Security-Policy: default-src 'none'; style-src 'unsafe-inline'\r\n\
                \X-Content-Type-Options: nosniff\r\n\
                \Connection: close\r\n\
                \Content-Length: "
                    <> BS8.pack (show (BS.length page))
                    <> "\r\n\r\n"
                    <> page
        Socket.sendAll connection response
        pure result

receiveGatewayCallbackHeaders
    :: Socket
    -> IO (Either Text BS.ByteString)
receiveGatewayCallbackHeaders connection = go BS.empty
  where
    maximumHeaderBytes = 16_384
    delimiter = "\r\n\r\n"

    go accumulated
        | delimiter `BS.isInfixOf` accumulated =
            pure (Right accumulated)
        | BS.length accumulated >= maximumHeaderBytes =
            pure (Left "Gateway OAuth callback headers were too large.")
        | otherwise = do
            chunk <-
                Socket.recv
                    connection
                    (maximumHeaderBytes - BS.length accumulated)
            if BS.null chunk
                then
                    pure
                        (Left
                            "Gateway OAuth callback closed before sending headers.")
                else go (accumulated <> chunk)

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

validateGatewayLoopbackRedirectUri :: Text -> Either Text ()
validateGatewayLoopbackRedirectUri raw =
    case URI.parseURI (Text.unpack raw) of
        Just uri
            | URI.uriScheme uri == "http:"
            , Just authority <- URI.uriAuthority uri
            , null (URI.uriUserInfo authority)
            , URI.uriRegName authority == "127.0.0.1"
            , validPort (URI.uriPort authority)
            , Text.pack (URI.uriPath uri) == gatewayBrowserRedirectPath
            , null (URI.uriQuery uri)
            , null (URI.uriFragment uri) ->
                Right ()
        _ -> Left "Gateway OAuth redirect URI is invalid."
  where
    validPort (':' : digits) =
        not (null digits)
            && all isDigit digits
            && case readMaybe digits of
                Just port -> port > (0 :: Int) && port <= 65535
                Nothing -> False
    validPort _ = False

singletonParameter
    :: Text
    -> [(Text, Maybe Text)]
    -> Either Text (Maybe Text)
singletonParameter name parameters =
    case [value | (key, value) <- parameters, key == name] of
        [] -> Right Nothing
        [Just value] -> Right (Just value)
        _ ->
            Left
                ("Gateway OAuth callback contains duplicate or invalid "
                    <> name <> " parameters.")

safeOAuthErrorSuffix :: Text -> Text
safeOAuthErrorSuffix raw =
    let value = Text.take 64 (Text.strip raw)
    in if Text.null value
        || not
            (Text.all
                (\character ->
                    isAsciiAlphaNumeric character
                        || character `elem` ("_-" :: String))
                value)
        then "."
        else ": " <> value <> "."

randomUrlText :: Int -> IO Text
randomUrlText byteCount =
    TextEncoding.decodeUtf8
        . Base64Url.encodeUnpadded
        <$> getEntropy byteCount

isAsciiAlphaNumeric :: Char -> Bool
isAsciiAlphaNumeric character =
    character >= 'a' && character <= 'z'
        || character >= 'A' && character <= 'Z'
        || character >= '0' && character <= '9'

isBase64UrlCharacter :: Char -> Bool
isBase64UrlCharacter character =
    isAsciiAlphaNumeric character || character `elem` ("-_" :: String)

isPkceCharacter :: Char -> Bool
isPkceCharacter character =
    isBase64UrlCharacter character
        || character `elem` (".~" :: String)

parseGatewayOrigin
    :: Text
    -> Text
    -> Either Text (Text, Text, Int)
parseGatewayOrigin errorMessage raw = do
    uri <-
        maybe (Left errorMessage) Right $
            URI.parseURI (Text.unpack (Text.strip raw))
    authority <-
        maybe (Left errorMessage) Right (URI.uriAuthority uri)
    let scheme = Text.toLower (Text.pack (URI.uriScheme uri))
        host = Text.toLower (Text.pack (URI.uriRegName authority))
    whenEither
        ( not (null (URI.uriUserInfo authority))
            || Text.null host
            || not (null (URI.uriQuery uri))
            || not (null (URI.uriFragment uri))
        )
        errorMessage
    port <- gatewayOriginPort errorMessage scheme (URI.uriPort authority)
    pure (scheme, host, port)

gatewayOriginPort :: Text -> Text -> String -> Either Text Int
gatewayOriginPort _ "https:" "" = Right 443
gatewayOriginPort _ "http:" "" = Right 80
gatewayOriginPort _ "wss:" "" = Right 443
gatewayOriginPort _ "ws:" "" = Right 80
gatewayOriginPort errorMessage scheme (':' : digits)
    | scheme `elem` ["https:", "http:", "wss:", "ws:"]
    , not (null digits)
    , all isDigit digits
    , Just port <- readMaybe digits
    , port > 0
    , port <= (65535 :: Int) =
        Right port
    | otherwise = Left errorMessage
gatewayOriginPort errorMessage _ _ = Left errorMessage

whenEither :: Bool -> Text -> Either Text ()
whenEither condition message
    | condition = Left message
    | otherwise = Right ()

validateBaseUrl :: Text -> Either Text Text
validateBaseUrl raw
    | Text.null base = Left "Gateway URL cannot be empty."
    | otherwise = do
        uri <- maybe
            (Left "Gateway URL is invalid.")
            Right
            (URI.parseURI (Text.unpack base))
        authority <- maybe
            (Left "Gateway URL must include a host.")
            Right
            (URI.uriAuthority uri)
        whenEither
            (null (URI.uriRegName authority))
            "Gateway URL must include a host."
        whenEither
            (not (null (URI.uriUserInfo authority))
                || not (null (URI.uriQuery uri))
                || not (null (URI.uriFragment uri)))
            "Gateway URL cannot contain user info, a query, or a fragment."
        case Text.toLower (Text.pack (URI.uriScheme uri)) of
            "https:" -> Right base
            "http:"
                | localHost (URI.uriRegName authority) -> Right base
            _ ->
                Left
                    "Gateway URL must use HTTPS (HTTP is allowed only for localhost development)."
  where
    base = Text.dropWhileEnd (== '/') (Text.strip raw)
    localHost rawHost =
        Text.toLower (Text.pack rawHost)
            `elem` ["localhost", "127.0.0.1", "::1", "[::1]"]
    whenEither condition message
        | condition = Left message
        | otherwise = Right ()

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
