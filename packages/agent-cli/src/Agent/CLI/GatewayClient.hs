-- | HTTPS device authorization and restricted gateway credential storage.
module Agent.CLI.GatewayClient
    ( GatewayCredential(..)
    , GatewayDeviceAuthorization(..)
    , GatewayPollResult(..)
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
import Agent.OsPath (unsafeToFilePath)
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (tryAny)
import Control.Monad (when)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as Text
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (hContentType)
import System.Directory.OsPath qualified as Directory
import System.Exit (ExitCode (..))
import System.OsPath (OsPath, takeDirectory, unsafeEncodeUtf, (</>))
import System.Posix.Files (setFileMode)
import System.Process (rawSystem)

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
    deriving (Eq, Show)

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
    deriving (Eq, Show)

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
                        Right credential -> Right (Just credential)

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
    baseUrl <- either failText pure (validateBaseUrl rawBaseUrl)
    manager <- newTlsManager
    device <-
        postJson manager (baseUrl <> "/api/v1/agent-connections/device")
            (Aeson.object ["client_name" .= ("haskell-agent" :: Text)])
            gatewayDeviceDecoder
            >>= either failText pure
    putStrLn ("Enter code " <> Text.unpack device.userCode <> " at:")
    putStrLn (Text.unpack device.verificationUri)
    opened <- openBrowser device.verificationUriComplete
    when (not opened) $
        putStrLn "Could not open a browser automatically."
    credential <- pollUntilAuthorized manager baseUrl device
    home <- Directory.getHomeDirectory
    saveGatewayCredentialAt home credential >>= either failText pure
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
    home <- Directory.getHomeDirectory
    let path = gatewayCredentialPath home
    exists <- Directory.doesFileExist path
    when exists (Directory.removeFile path)
    putStrLn "Gateway connection removed."

runGatewayCommand :: GatewayCommand -> IO ()
runGatewayCommand = \case
    GatewayConnect url -> connectGateway url
    GatewayStatus -> showGatewayStatus
    GatewayDisconnect -> disconnectGateway

pollUntilAuthorized
    :: HTTP.Manager
    -> Text
    -> GatewayDeviceAuthorization
    -> IO GatewayCredential
pollUntilAuthorized manager baseUrl device =
    go device.expiresInSeconds (max 1 device.pollIntervalSeconds)
  where
    go remaining interval
        | remaining <= 0 = failText "Gateway authorization expired."
        | otherwise = do
            threadDelay (interval * 1_000_000)
            result <-
                postJson manager (baseUrl <> "/api/v1/agent-connections/token")
                    (Aeson.object ["device_code" .= device.deviceCode])
                    gatewayPollDecoder
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
                    let next = maybe (interval + 5) (max (interval + 1)) serverInterval
                     in go (remaining - next) next
                GatewayAccessDenied -> failText "Gateway authorization was denied."
                GatewayExpired -> failText "Gateway authorization expired."
                GatewayPollFailed code ->
                    failText ("Gateway authorization failed: " <> code)

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
                Left exception -> Left (Text.pack (show exception))
                Right value ->
                    case Hermes.decodeEither decoder (LBS.toStrict (HTTP.responseBody value)) of
                        Left err -> Left (Hermes.jsonErrorMessage err)
                        Right decoded -> Right decoded

validateBaseUrl :: Text -> Either Text Text
validateBaseUrl raw
    | Text.null base = Left "Gateway URL cannot be empty."
    | "https://" `Text.isPrefixOf` lower = Right base
    | any localOrigin
        [ "http://127.0.0.1"
        , "http://localhost"
        , "http://[::1]"
        ] = Right base
    | otherwise = Left "Gateway URL must use HTTPS (HTTP is allowed only for localhost development)."
  where
    base = Text.dropWhileEnd (== '/') (Text.strip raw)
    lower = Text.toLower base
    localOrigin origin =
        lower == origin
            || (origin <> ":") `Text.isPrefixOf` lower
            || (origin <> "/") `Text.isPrefixOf` lower

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
