-- | OAuth wire values, PKCE construction, and callback/credential validation.
module Agent.CLI.Gateway.OAuth.Protocol
    ( GatewayAuthorization(..)
    , GatewayAuthorizationCodeResponse(..)
    , GatewayDeviceAuthorization(..)
    , GatewayPollResult(..)
    , defaultGatewayBaseUrl
    , gatewayBrowserClientId
    , gatewayBrowserRedirectPath
    , gatewayAuthorizationCodeDecoder
    , gatewayDeviceDecoder
    , gatewayPollDecoder
    , validateGatewayDeviceAuthorization
    , gatewayAuthorizationUrl
    , gatewayPkceChallenge
    , validateGatewayAuthorizationCallback
    , validateGatewayAuthorizationParameters
    , validateGatewayAuthorizationCodeResponse
    , validateNativeGatewayAuthorizationExchange
    , isAsciiAlphaNumeric
    ) where

import Agent.CLI.Gateway.Credentials (validateGatewayCredential)
import Agent.CLI.Gateway.Origin
    ( parseGatewayOrigin
    , parseGatewayResourceOrigin
    , validateBaseUrl
    , whenEither
    )
import Agent.Json.Decode qualified as Hermes
import Agent.Server.Client.GatewayIdentity (GatewayCredential(..))
import Crypto.Hash (Digest, SHA256, hash)
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
import Network.HTTP.Types (methodGet)
import Network.HTTP.Types.URI (parseQueryText, renderQueryText)
import Network.URI qualified as URI
import Text.Read (readMaybe)

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

-- | Ensure a device response cannot make a native or terminal client open an
-- unrelated origin.
validateGatewayDeviceAuthorization
    :: Text
    -> GatewayDeviceAuthorization
    -> Either Text GatewayDeviceAuthorization
validateGatewayDeviceAuthorization rawBaseUrl authorization = do
    baseUrl <- validateBaseUrl rawBaseUrl
    whenEither
        (Text.null (Text.strip authorization.deviceCode))
        "The gateway returned an empty device code."
    whenEither
        (Text.null (Text.strip authorization.userCode))
        "The gateway returned an empty user code."
    whenEither
        (authorization.expiresInSeconds <= 0)
        "The gateway returned an invalid authorization expiry."
    whenEither
        (authorization.pollIntervalSeconds <= 0)
        "The gateway returned an invalid polling interval."
    gatewayOrigin <-
        parseGatewayOrigin
            "The gateway URL is invalid."
            baseUrl
    verificationOrigin <-
        parseGatewayResourceOrigin
            "The gateway returned an invalid verification URL."
            authorization.verificationUri
    completeOrigin <-
        parseGatewayResourceOrigin
            "The gateway returned an invalid complete verification URL."
            authorization.verificationUriComplete
    whenEither
        (verificationOrigin /= gatewayOrigin
            || completeOrigin /= gatewayOrigin)
        "The gateway returned a verification URL for a different origin."
    pure authorization

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
    (method, target) <- case BS8.words requestLine of
        requestMethod : rawTarget : _ -> Right (requestMethod, rawTarget)
        _ -> Left "Gateway OAuth callback request is malformed."
    let (path, rawQuery) = BS.break (== 63) target
    validateGatewayAuthorizationParameters
        expectedState
        method
        path
        (not (BS.null rawQuery))
        (parseQueryText (BS.drop 1 rawQuery))
  where
    requestLine = BS8.takeWhile (/= '\r') request

validateGatewayAuthorizationParameters
    :: Text
    -> BS.ByteString
    -> BS.ByteString
    -> Bool
    -> [(Text, Maybe Text)]
    -> Either Text Text
validateGatewayAuthorizationParameters
    expectedState
    method
    path
    hasQuery
    parameters = do
    whenEither
        (method /= methodGet)
        "Gateway OAuth callback must use GET."
    whenEither
        (path /= TextEncoding.encodeUtf8 gatewayBrowserRedirectPath)
        "Gateway OAuth callback path is invalid."
    whenEither
        (not hasQuery)
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

validateNativeGatewayAuthorizationExchange
    :: Text
    -> Text
    -> Text
    -> Text
    -> Text
    -> Either Text Text
validateNativeGatewayAuthorizationExchange
    rawBaseUrl clientId authorizationCode verifier redirectUri = do
        baseUrl <- validateBaseUrl rawBaseUrl
        whenEither
            (clientId /= "haskell-agent-macos")
            "Gateway OAuth client ID is invalid."
        whenEither
            (Text.null authorizationCode
                || Text.length authorizationCode > 4096)
            "Gateway authorization code is invalid."
        whenEither
            ( Text.length verifier < 43
                || Text.length verifier > 128
                || not (Text.all isPkceCharacter verifier)
            )
            "Gateway PKCE code verifier is invalid."
        whenEither
            (redirectUri /= "haskell-agent-auth://gateway/callback")
            "Gateway OAuth redirect URI is invalid."
        pure baseUrl

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
