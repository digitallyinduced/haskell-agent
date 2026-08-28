-- | Headless ChatGPT OAuth login compatible with the Codex CLI device-code flow.
module Agent.OpenAI.Login
    ( DeviceCode(..)
    , LoginOptions(..)
    , defaultLoginOptions
    , requestDeviceCode
    , pollDeviceCode
    , completeDeviceCodeLogin
    , writeAuthFile
    ) where

import Agent.FileRetry (writeLazyFileAtomically)
import Agent.Http.Url (trimTrailingSlash)
import qualified Agent.Json.Decode as Json
import Agent.OpenAI.Auth (deriveAccountId)
import System.OsPath (OsPath)
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (tryAny)
import Control.Monad (unless)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock (getCurrentTime)
import Network.HTTP.Simple
import System.Directory.OsPath (createDirectoryIfMissing)
import System.OsPath (takeDirectory)

data LoginOptions = LoginOptions
    { issuer :: !String
    , clientId :: !Text
    } deriving (Eq, Show)

-- | Build login options for the standard OpenAI issuer. The OAuth public
-- client id is runtime configuration and is never embedded in this package.
defaultLoginOptions :: Text -> LoginOptions
defaultLoginOptions oauthClientId = LoginOptions
    { issuer = "https://auth.openai.com"
    , clientId = oauthClientId
    }

data DeviceCode = DeviceCode
    { verificationUrl :: !String
    , userCode :: !Text
    , deviceAuthId :: !Text
    , pollIntervalSeconds :: !Int
    } deriving (Eq)

-- Device authorization values grant access while the login flow is pending.
-- In particular, verification URLs may embed the user code.
instance Show DeviceCode where
    show code =
        "DeviceCode { verificationUrl = <redacted>, userCode = <redacted>"
            <> ", deviceAuthId = <redacted>"
            <> ", pollIntervalSeconds = " <> show code.pollIntervalSeconds
            <> " }"

data Tokens = Tokens
    { idToken :: !Text
    , accessToken :: !Text
    , refreshToken :: !Text
    }

requestDeviceCode :: LoginOptions -> IO (Either Text DeviceCode)
requestDeviceCode options = safely do
    request <- parseRequest (trimTrailingSlash options.issuer <> "/api/accounts/deviceauth/usercode")
    response <- httpLBS
        $ setRequestMethod "POST"
        $ setRequestHeader "Content-Type" ["application/json"]
        $ setRequestBodyJSON (Aeson.object ["client_id" .= options.clientId]) request
    requireSuccess "device code request" response
    decodeResponse "device code response"
        (deviceCodeDecoder
            (trimTrailingSlash options.issuer <> "/codex/device"))
        response

completeDeviceCodeLogin :: LoginOptions -> DeviceCode -> IO (Either Text Aeson.Value)
completeDeviceCodeLogin options deviceCode = safely do
    tokens <- poll 0
    authValue tokens
  where
    poll elapsed
        | elapsed >= 15 * 60 = fail "device authorization timed out after 15 minutes"
        | otherwise = pollOnce options deviceCode >>= \case
            Nothing -> do
                let seconds = deviceCode.pollIntervalSeconds
                threadDelay (seconds * 1_000_000)
                poll (elapsed + seconds)
            Just tokens -> pure tokens

-- | Poll once without blocking. 'Nothing' means the user has not authorized
-- the device yet. This is useful for web applications that poll from the
-- browser and must not keep a request open for up to fifteen minutes.
pollDeviceCode :: LoginOptions -> DeviceCode -> IO (Either Text (Maybe Aeson.Value))
pollDeviceCode options deviceCode = safely do
    pollOnce options deviceCode >>= traverse authValue

authValue :: Tokens -> IO Aeson.Value
authValue tokens = do
    now <- getCurrentTime
    account <- maybe (fail "id_token does not contain a ChatGPT account id") pure
        (deriveAccountId tokens.idToken)
    pure $ Aeson.object
        [ "OPENAI_API_KEY" .= Aeson.Null
        , "auth_mode" .= ("chatgpt" :: Text)
        , "last_refresh" .= now
        , "tokens" .= Aeson.object
            [ "id_token" .= tokens.idToken
            , "access_token" .= tokens.accessToken
            , "refresh_token" .= tokens.refreshToken
            , "account_id" .= account
            ]
        ]
pollOnce :: LoginOptions -> DeviceCode -> IO (Maybe Tokens)
pollOnce options deviceCode = do
    request <- parseRequest (trimTrailingSlash options.issuer <> "/api/accounts/deviceauth/token")
    response <- httpLBS
        $ setRequestMethod "POST"
        $ setRequestHeader "Content-Type" ["application/json"]
        $ setRequestBodyJSON (Aeson.object
            [ "device_auth_id" .= deviceCode.deviceAuthId
            , "user_code" .= deviceCode.userCode
            ]) request
    case getResponseStatusCode response of
        status | status >= 200 && status < 300 -> do
            (authorizationCode, codeVerifier) <-
                decodeResponse "device authorization response"
                    authorizationDecoder response
            Just <$> exchange authorizationCode codeVerifier
        403 -> pure Nothing
        404 -> pure Nothing
        status -> fail ("device authorization failed with HTTP " <> show status)
  where
    exchange authorizationCode codeVerifier = do
        request <- parseRequest (trimTrailingSlash options.issuer <> "/oauth/token")
        response <- httpLBS
            $ setRequestMethod "POST"
            $ setRequestHeader "Content-Type" ["application/x-www-form-urlencoded"]
            $ setRequestBodyURLEncoded
                [ ("grant_type", "authorization_code")
                , ("code", encode authorizationCode)
                , ("redirect_uri", encode (Text.pack (trimTrailingSlash options.issuer <> "/deviceauth/callback")))
                , ("client_id", encode options.clientId)
                , ("code_verifier", encode codeVerifier)
                ] request
        requireSuccess "OAuth token exchange" response
        decodeResponse "OAuth token response" tokensDecoder response

writeAuthFile :: OsPath -> Aeson.Value -> IO ()
writeAuthFile path value = do
    createDirectoryIfMissing True (takeDirectory path)
    writeLazyFileAtomically path 0o600 (Aeson.encode value)

safely :: IO a -> IO (Either Text a)
safely action = tryAny action >>= \case
    Left exception -> pure (Left (Text.pack (show exception)))
    Right result -> pure (Right result)

requireSuccess label response = do
    let status = getResponseStatusCode response
    unless (status >= 200 && status < 300) $ fail
        (Text.unpack label <> " failed with HTTP " <> show status)

decodeResponse label decoder response =
    case Json.decodeEither decoder
            (LBS.toStrict (getResponseBody response)) of
        Left err -> fail
            (Text.unpack label <> " is invalid JSON: "
                <> Text.unpack (Json.jsonErrorMessage err))
        Right value -> pure value

deviceCodeDecoder :: String -> Json.Decoder DeviceCode
deviceCodeDecoder verificationUrl = Json.object do
    userCode <- Json.atKey "user_code" Json.text
    deviceAuthId <- Json.atKey "device_auth_id" Json.text
    interval <- Json.atKey "interval" intervalDecoder
    pure DeviceCode
        { verificationUrl
        , userCode
        , deviceAuthId
        , pollIntervalSeconds = max 1 interval
        }

authorizationDecoder :: Json.Decoder (Text, Text)
authorizationDecoder = Json.object $
    (,)
        <$> Json.atKey "authorization_code" Json.text
        <*> Json.atKey "code_verifier" Json.text

tokensDecoder :: Json.Decoder Tokens
tokensDecoder = Json.object $
    Tokens
        <$> Json.atKey "id_token" Json.text
        <*> Json.atKey "access_token" Json.text
        <*> Json.atKey "refresh_token" Json.text

intervalDecoder :: Json.Decoder Int
intervalDecoder = Json.withType \case
    Json.VString -> Json.withText \value ->
        maybe (fail "interval is invalid") pure (readMaybeInt value)
    Json.VNumber -> Json.int
    _ -> fail "interval is invalid"

encode = Text.encodeUtf8


readMaybeInt value = case reads (Text.unpack value) of
    [(number, "")] -> Just number
    _ -> Nothing
