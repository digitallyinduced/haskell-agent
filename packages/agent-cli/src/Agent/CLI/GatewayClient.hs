-- | HTTPS device authorization and restricted gateway credential storage.
module Agent.CLI.GatewayClient
    ( GatewayCredential(..)
    , GatewayDeviceAuthorization(..)
    , GatewayPollResult(..)
    , connectGateway
    , disconnectGateway
    , gatewayCredentialPath
    , loadGatewayCredential
    , loadGatewayCredentialAt
    , pollGatewayAuthorization
    , pollGatewayAuthorizationAndSave
    , removeGatewayCredential
    , runGatewayCommand
    , saveGatewayCredentialAt
    , showGatewayStatus
    , startGatewayAuthorization
    , validateBaseUrl
    , validateGatewayCredential
    , validateGatewayDeviceAuthorization
    ) where

import Agent.FileRetry (retryOnFileBusy, writeLazyFileAtomically)
import Agent.CLI.Options (GatewayCommand (..))
import Agent.OpenAI.WebSocketClient (validateGatewayWebSocketUrl)
import Agent.OsPath (unsafeToFilePath)
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (tryAny)
import Control.Monad (when)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as Text
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (hContentType)
import Network.URI qualified as URI
import System.Directory.OsPath qualified as Directory
import System.Exit (ExitCode (..))
import System.OsPath (OsPath, takeDirectory, unsafeEncodeUtf, (</>))
import System.Posix.Files (setFileMode)
import System.Process (rawSystem)
import Text.Read (readMaybe)

data GatewayCredential = GatewayCredential
    { gatewayBaseUrl :: !Text
    , gatewayWebSocketUrl :: !Text
    , gatewayAccessToken :: !Text
    }
    deriving (Eq)

instance Show GatewayCredential where
    show credential =
        "GatewayCredential { gatewayBaseUrl = "
            <> show credential.gatewayBaseUrl
            <> ", gatewayWebSocketUrl = <redacted>"
            <> ", gatewayAccessToken = <redacted> }"

instance Aeson.ToJSON GatewayCredential where
    toJSON credential =
        Aeson.object
            [ "version" .= (1 :: Int)
            , "base_url" .= credential.gatewayBaseUrl
            , "websocket_url" .= credential.gatewayWebSocketUrl
            , "access_token" .= credential.gatewayAccessToken
            ]

instance Aeson.FromJSON GatewayCredential where
    parseJSON = Aeson.withObject "GatewayCredential" \object ->
        GatewayCredential
            <$> object Aeson..: "base_url"
            <*> object Aeson..: "websocket_url"
            <*> object Aeson..: "access_token"

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
    show authorization =
        "GatewayDeviceAuthorization"
            <> " { deviceCode = <redacted>"
            <> ", userCode = <redacted>"
            <> ", verificationUri = "
            <> show authorization.verificationUri
            <> ", verificationUriComplete = <redacted>"
            <> ", expiresInSeconds = "
            <> show authorization.expiresInSeconds
            <> ", pollIntervalSeconds = "
            <> show authorization.pollIntervalSeconds
            <> " }"

instance Aeson.FromJSON GatewayDeviceAuthorization where
    parseJSON = Aeson.withObject "GatewayDeviceAuthorization" \object ->
        GatewayDeviceAuthorization
            <$> object Aeson..: "device_code"
            <*> object Aeson..: "user_code"
            <*> object Aeson..: "verification_uri"
            <*> object Aeson..: "verification_uri_complete"
            <*> object Aeson..: "expires_in"
            <*> object Aeson..: "interval"

data GatewayPollResult
    = GatewayAuthorized !Text !Text
    | GatewayAuthorizationPending !(Maybe Int)
    | GatewaySlowDown !(Maybe Int)
    | GatewayAccessDenied
    | GatewayExpired
    | GatewayPollFailed !Text
    deriving (Eq)

instance Show GatewayPollResult where
    show = \case
        GatewayAuthorized _ _ ->
            "GatewayAuthorized <redacted> <redacted>"
        GatewayAuthorizationPending interval ->
            "GatewayAuthorizationPending " <> show interval
        GatewaySlowDown interval ->
            "GatewaySlowDown " <> show interval
        GatewayAccessDenied -> "GatewayAccessDenied"
        GatewayExpired -> "GatewayExpired"
        GatewayPollFailed code -> "GatewayPollFailed " <> show code

instance Aeson.FromJSON GatewayPollResult where
    parseJSON = Aeson.withObject "GatewayPollResult" \object -> do
        maybeAccessToken <- object Aeson..:? "access_token"
        case maybeAccessToken of
            Just accessToken ->
                GatewayAuthorized accessToken
                    <$> object Aeson..: "websocket_url"
            Nothing -> do
                code <- object Aeson..: "error"
                interval <- object Aeson..:? "interval"
                pure case code of
                    "authorization_pending" -> GatewayAuthorizationPending interval
                    "slow_down" -> GatewaySlowDown interval
                    "access_denied" -> GatewayAccessDenied
                    "expired_token" -> GatewayExpired
                    other -> GatewayPollFailed other

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
                    case Aeson.eitherDecode bytes of
                        Left err -> Left (Text.pack err)
                        Right credential ->
                            Just <$> validateGatewayCredential credential

saveGatewayCredentialAt :: OsPath -> GatewayCredential -> IO (Either Text ())
saveGatewayCredentialAt home credential = do
    let path = gatewayCredentialPath home
        directory = takeDirectory path
    result <- tryAny do
        Directory.createDirectoryIfMissing True directory
        setFileMode (unsafeToFilePath directory) 0o700
        writeLazyFileAtomically path 0o600 (Aeson.encode credential)
    pure case result of
        Left exception -> Left (Text.pack (show exception))
        Right () -> Right ()

connectGateway :: Text -> IO ()
connectGateway rawBaseUrl = do
    device <-
        startGatewayAuthorization rawBaseUrl "haskell-agent"
            >>= either failText pure
    baseUrl <- either failText pure (validateBaseUrl rawBaseUrl)
    putStrLn ("Enter code " <> Text.unpack device.userCode <> " at:")
    putStrLn (Text.unpack device.verificationUri)
    opened <- openBrowser device.verificationUriComplete
    when (not opened) $
        putStrLn "Could not open a browser automatically."
    pollUntilAuthorized baseUrl device
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

runGatewayCommand :: GatewayCommand -> IO ()
runGatewayCommand = \case
    GatewayConnect url -> connectGateway url
    GatewayStatus -> showGatewayStatus
    GatewayDisconnect -> disconnectGateway

pollUntilAuthorized
    :: Text
    -> GatewayDeviceAuthorization
    -> IO ()
pollUntilAuthorized baseUrl device =
    go device.expiresInSeconds (max 1 device.pollIntervalSeconds)
  where
    go remaining interval
        | remaining <= 0 = failText "Gateway authorization expired."
        | otherwise = do
            threadDelay (interval * 1_000_000)
            result <-
                pollGatewayAuthorizationAndSave baseUrl device.deviceCode
                    >>= either failText pure
            case result of
                GatewayAuthorized _ _ -> pure ()
                GatewayAuthorizationPending serverInterval ->
                    let next = maybe interval (max 1) serverInterval
                     in go (remaining - next) next
                GatewaySlowDown serverInterval ->
                    let next = maybe (interval + 5) (max (interval + 1)) serverInterval
                     in go (remaining - next) next
                GatewayAccessDenied -> failText "Gateway authorization was denied."
                GatewayExpired -> failText "Gateway authorization expired."
                GatewayPollFailed code ->
                    failText ("Gateway authorization failed: " <> code)

-- | Start device authorization without opening a browser or beginning a poll
-- loop. This is the primitive used by native GUI clients.
startGatewayAuthorization
    :: Text
    -> Text
    -> IO (Either Text GatewayDeviceAuthorization)
startGatewayAuthorization rawBaseUrl rawClientName =
    case validateBaseUrl rawBaseUrl of
        Left err -> pure (Left err)
        Right baseUrl
            | Text.null clientName ->
                pure (Left "Gateway client name cannot be empty.")
            | otherwise -> do
                manager <- newTlsManager
                postJson manager
                    (baseUrl <> "/api/v1/agent-connections/device")
                    (Aeson.object ["client_name" .= clientName])
                    >>= \case
                        Left err -> pure (Left err)
                        Right authorization ->
                            pure $
                                validateGatewayDeviceAuthorization
                                    baseUrl
                                    authorization
  where
    clientName = Text.strip rawClientName

-- | Perform one device-token poll. Callers must not expose the authorized
-- token outside the trusted runtime; native clients should use
-- 'pollGatewayAuthorizationAndSave' instead.
pollGatewayAuthorization
    :: Text
    -> Text
    -> IO (Either Text GatewayPollResult)
pollGatewayAuthorization rawBaseUrl rawDeviceCode =
    case validateBaseUrl rawBaseUrl of
        Left err -> pure (Left err)
        Right baseUrl
            | Text.null deviceCode ->
                pure (Left "Gateway device code cannot be empty.")
            | otherwise -> do
                manager <- newTlsManager
                postJson manager
                    (baseUrl <> "/api/v1/agent-connections/token")
                    (Aeson.object ["device_code" .= deviceCode])
  where
    deviceCode = Text.strip rawDeviceCode

-- | Poll once and atomically persist an authorized credential. The result is
-- still useful to trusted Haskell callers, but the native ABI intentionally
-- maps authorization to a status code and never returns either secret field.
pollGatewayAuthorizationAndSave
    :: Text
    -> Text
    -> IO (Either Text GatewayPollResult)
pollGatewayAuthorizationAndSave rawBaseUrl deviceCode = do
    let validatedBaseUrl = validateBaseUrl rawBaseUrl
    case validatedBaseUrl of
        Left err -> pure (Left err)
        Right baseUrl ->
            pollGatewayAuthorization baseUrl deviceCode >>= \case
                Left err -> pure (Left err)
                Right authorized@(GatewayAuthorized accessToken websocketUrl) ->
                    case validateGatewayCredential GatewayCredential
                        { gatewayBaseUrl = baseUrl
                        , gatewayWebSocketUrl = websocketUrl
                        , gatewayAccessToken = accessToken
                        } of
                        Left err -> pure (Left err)
                        Right credential -> do
                            home <- Directory.getHomeDirectory
                            saveGatewayCredentialAt home credential
                                >>= \case
                                    Left err -> pure (Left err)
                                    Right () -> pure (Right authorized)
                Right result -> pure (Right result)

removeGatewayCredential :: IO (Either Text ())
removeGatewayCredential = do
    home <- Directory.getHomeDirectory
    let path = gatewayCredentialPath home
    result <- tryAny do
        exists <- Directory.doesFileExist path
        when exists (Directory.removeFile path)
    pure case result of
        Left exception -> Left (Text.pack (show exception))
        Right () -> Right ()

postJson
    :: Aeson.FromJSON value
    => HTTP.Manager
    -> Text
    -> Aeson.Value
    -> IO (Either Text value)
postJson manager url payload = do
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
                Left exception -> Left (Text.pack (show exception))
                Right value ->
                    case Aeson.eitherDecode (HTTP.responseBody value) of
                        Left err -> Left (Text.pack err)
                        Right decoded -> Right decoded

validateBaseUrl :: Text -> Either Text Text
validateBaseUrl raw
    | Text.null base = Left "Gateway URL cannot be empty."
    | otherwise =
        case URI.parseURI (Text.unpack base) of
            Just uri
                | validUri uri
                , scheme uri == "https:" ->
                    Right base
                | validUri uri
                , scheme uri == "http:"
                , Just authority <- URI.uriAuthority uri
                , host authority `elem` localHosts ->
                    Right base
            _ -> invalid
  where
    base = Text.dropWhileEnd (== '/') (Text.strip raw)
    invalid =
        Left "Gateway URL must use HTTPS (HTTP is allowed only for localhost development)."
    scheme = Text.toLower . Text.pack . URI.uriScheme
    host = Text.toLower . Text.pack . URI.uriRegName
    localHosts = ["localhost", "127.0.0.1", "[::1]"]
    validUri uri =
        null (URI.uriQuery uri)
            && null (URI.uriFragment uri)
            && case URI.uriAuthority uri of
                Nothing -> False
                Just authority ->
                    null (URI.uriUserInfo authority)
                        && not (null (URI.uriRegName authority))
                        && validPort (URI.uriPort authority)
    validPort "" = True
    validPort (':' : digits) =
        not (null digits)
            && all isDigit digits
            && case readMaybe digits of
                Just port -> port > (0 :: Int) && port <= 65535
                Nothing -> False
    validPort _ = False

-- | Reject corrupted or tampered credential files before they affect model
-- discovery or send a bearer token to an unrelated WebSocket origin.
validateGatewayCredential
    :: GatewayCredential
    -> Either Text GatewayCredential
validateGatewayCredential credential = do
    baseUrl <- validateBaseUrl credential.gatewayBaseUrl
    when
        (Text.null (Text.strip credential.gatewayAccessToken))
        (Left "The gateway credential contains an empty access token.")
    validateGatewayWebSocketUrl credential.gatewayWebSocketUrl
    baseOrigin <-
        parseOrigin
            "The gateway credential contains an invalid base URL."
            baseUrl
    websocketOrigin <-
        parseOrigin
            "The gateway credential contains an invalid WebSocket URL."
            credential.gatewayWebSocketUrl
    let expectedWebSocketScheme = case baseOrigin of
            ("https:", _, _) -> "wss:"
            ("http:", _, _) -> "ws:"
            _ -> ""
        (_, baseHost, basePort) = baseOrigin
        (websocketScheme, websocketHost, websocketPort) = websocketOrigin
    if websocketScheme == expectedWebSocketScheme
        && websocketHost == baseHost
        && websocketPort == basePort
        then Right credential { gatewayBaseUrl = baseUrl }
        else
            Left
                "The gateway credential WebSocket URL uses a different origin."

-- | Ensure an authorization response cannot make a native or CLI client open
-- an unrelated website, local file, or custom URL scheme. The verification
-- URLs must be absolute and share the validated gateway's origin.
validateGatewayDeviceAuthorization
    :: Text
    -> GatewayDeviceAuthorization
    -> Either Text GatewayDeviceAuthorization
validateGatewayDeviceAuthorization baseUrl authorization
    | Text.null (Text.strip authorization.deviceCode) =
        Left "The gateway returned an empty device code."
    | Text.null (Text.strip authorization.userCode) =
        Left "The gateway returned an empty user code."
    | authorization.expiresInSeconds <= 0 =
        Left "The gateway returned an invalid authorization expiry."
    | authorization.pollIntervalSeconds <= 0 =
        Left "The gateway returned an invalid polling interval."
    | otherwise = do
        gatewayOrigin <-
            parseOrigin
                "The gateway URL is invalid."
                baseUrl
        verificationOrigin <-
            parseOrigin
                "The gateway returned an invalid verification URL."
                authorization.verificationUri
        completeOrigin <-
            parseOrigin
                "The gateway returned an invalid complete verification URL."
                authorization.verificationUriComplete
        if verificationOrigin == gatewayOrigin
            && completeOrigin == gatewayOrigin
            then Right authorization
            else
                Left
                    "The gateway returned a verification URL for a different origin."

parseOrigin :: Text -> Text -> Either Text (Text, Text, Int)
parseOrigin errorMessage raw = do
    uri <- maybe (Left errorMessage) Right $
        URI.parseURI (Text.unpack (Text.strip raw))
    authority <- maybe (Left errorMessage) Right (URI.uriAuthority uri)
    let scheme = Text.toLower (Text.pack (URI.uriScheme uri))
        host = Text.toLower (Text.pack (URI.uriRegName authority))
    if null (URI.uriUserInfo authority)
        && not (Text.null host)
        && null (URI.uriFragment uri)
        then do
            port <- originPort errorMessage scheme (URI.uriPort authority)
            pure (scheme, host, port)
        else Left errorMessage

originPort :: Text -> Text -> String -> Either Text Int
originPort _ "https:" "" = Right 443
originPort _ "http:" "" = Right 80
originPort _ "wss:" "" = Right 443
originPort _ "ws:" "" = Right 80
originPort _ scheme (':' : digits)
    | scheme `elem` ["https:", "http:", "wss:", "ws:"]
    , not (null digits)
    , all isDigit digits
    , Just port <- readMaybe digits
    , port > 0
    , port <= (65535 :: Int) =
        Right port
originPort errorMessage _ _ = Left errorMessage

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
