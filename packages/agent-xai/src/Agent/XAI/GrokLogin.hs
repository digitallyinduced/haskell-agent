-- | Headless xAI OAuth login and refresh for Grok subscription credentials.
--
-- xAI's auth server (@auth.x.ai@) is a standard OAuth 2.1 provider. Two
-- grants are relevant for server-side use:
--
-- * RFC 8628 device authorization (@POST /oauth2/device/code@ +
--   @POST /oauth2/token@) — the only interactive grant that works without a
--   loopback listener, so it is what a broker's connect flow uses. Whether
--   the grant is enabled is server-controlled; a 404 from the device-code
--   endpoint means it is disabled for this client and credentials must be
--   imported from an existing @grok@ CLI login instead.
-- * @refresh_token@ — used for rotation. xAI may or may not rotate the
--   refresh token on use; when the response carries none, the old one stays
--   valid. @invalid_grant@/@invalid_client@ are terminal (re-login required),
--   anything else is worth retrying.
--
-- The OAuth public client id is supplied by the application at runtime and is
-- never embedded in this package.
module Agent.XAI.GrokLogin
    ( GrokLoginOptions(..)
    , defaultGrokLoginOptions
    , GrokDeviceCode(..)
    , GrokTokens(..)
    , requestGrokDeviceCode
    , pollGrokDeviceCode
    , completeGrokDeviceCodeLogin
    , refreshGrokAccessToken
    , deriveGrokAccountId
    ) where

import Agent.Error (ApiError(..), ErrorType(..))
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (tryAny)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified "base64-bytestring" Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.HTTP.Simple

data GrokLoginOptions = GrokLoginOptions
    { issuer :: !String
    , clientId :: !Text
    , scopes :: ![Text]
      -- | Team logins carry the team principal through token requests.
      -- Personal subscriptions leave both fields empty.
    , principalType :: !(Maybe Text)
    , principalId :: !(Maybe Text)
    } deriving (Eq, Show)

-- | The grok CLI's OAuth issuer and scope contract. The caller supplies its
-- OAuth public client id at runtime. @grok-cli:access@ authorizes the token for
-- subscription-proxy requests; @offline_access@ yields a refresh token.
defaultGrokLoginOptions :: Text -> GrokLoginOptions
defaultGrokLoginOptions oauthClientId = GrokLoginOptions
    { issuer = "https://auth.x.ai"
    , clientId = oauthClientId
    , scopes =
        [ "openid"
        , "profile"
        , "email"
        , "offline_access"
        , "grok-cli:access"
        , "api:access"
        , "conversations:read"
        , "conversations:write"
        , "workspaces:read"
        , "workspaces:write"
        ]
    , principalType = Nothing
    , principalId = Nothing
    }

data GrokDeviceCode = GrokDeviceCode
    { deviceCode :: !Text
    , userCode :: !Text
    , verificationUrl :: !Text
    , pollIntervalSeconds :: !Int
    , expiresInSeconds :: !(Maybe Int)
    } deriving (Eq, Show)

data GrokTokens = GrokTokens
    { accessToken :: !Text
    , refreshToken :: !(Maybe Text)
    , idToken :: !(Maybe Text)
    , expiresInSeconds :: !(Maybe Int)
    } deriving (Eq)

-- Keep bearer/refresh tokens out of logs.
instance Show GrokTokens where
    show tokens = "GrokTokens { accessToken = <redacted>, refreshToken = "
        <> (case tokens.refreshToken of
                Nothing -> "Nothing"
                Just _ -> "Just <redacted>")
        <> ", expiresInSeconds = " <> show tokens.expiresInSeconds <> " }"

-- | Start a device authorization. A 404 means the device grant is disabled
-- server-side for this client — surfaced as a distinguishable error text so
-- callers can fall back to credential import.
requestGrokDeviceCode :: GrokLoginOptions -> IO (Either Text GrokDeviceCode)
requestGrokDeviceCode options = safely do
    request <- parseRequest ("POST " <> trimIssuer options.issuer <> "/oauth2/device/code")
    response <- httpLBS
        $ setRequestHeader "Content-Type" ["application/x-www-form-urlencoded"]
        $ setRequestBodyURLEncoded
            ([ ("client_id", Text.encodeUtf8 options.clientId)
             , ("scope", Text.encodeUtf8 (Text.unwords options.scopes))
             , ("referrer", "grok-build")
             ] <> principalFields options) request
    case getResponseStatusCode response of
        404 -> fail deviceFlowDisabledMessage
        status | status < 200 || status >= 300 ->
            fail ("device code request failed with HTTP " <> show status
                <> ": " <> lbsPreview (getResponseBody response))
        _ -> pure ()
    object <- decodeObject "device code response" response
    deviceCode <- field "device_code" object
    userCode <- field "user_code" object
    verificationUri <- field "verification_uri" object
    let verificationUrl = Maybe.fromMaybe verificationUri
            (textFieldMaybe "verification_uri_complete" object)
    pure GrokDeviceCode
        { deviceCode
        , userCode
        , verificationUrl
        , pollIntervalSeconds = max 1 (Maybe.fromMaybe 5 (intFieldMaybe "interval" object))
        , expiresInSeconds = intFieldMaybe "expires_in" object
        }

deviceFlowDisabledMessage :: String
deviceFlowDisabledMessage =
    "device authorization is disabled for this client (HTTP 404); import an existing grok CLI credential instead"

-- | Poll once without blocking; 'Nothing' means the user has not approved
-- the device yet ('authorization_pending' / 'slow_down').
pollGrokDeviceCode :: GrokLoginOptions -> GrokDeviceCode -> IO (Either Text (Maybe GrokTokens))
pollGrokDeviceCode options deviceCode = safely do
    -- The principal has to be declared when the token is minted, not only
    -- when it is later refreshed: a team login that omits it here gets a
    -- personally scoped token that the refresh then contradicts.
    response <- postTokenForm options $
        [ ("grant_type", "urn:ietf:params:oauth:grant-type:device_code")
        , ("device_code", Text.encodeUtf8 deviceCode.deviceCode)
        , ("client_id", Text.encodeUtf8 options.clientId)
        ] <> principalFields options
    let status = getResponseStatusCode response
    if status >= 200 && status < 300
        then Just <$> parseTokens response
        else case oauthErrorCode response of
            Just "authorization_pending" -> pure Nothing
            Just "slow_down" -> pure Nothing
            Just "access_denied" -> fail "the user denied the device authorization"
            Just "expired_token" -> fail "the device code expired before it was approved"
            Just other -> fail ("device token request failed: " <> Text.unpack other)
            Nothing -> fail ("device token request failed with HTTP " <> show status
                <> ": " <> lbsPreview (getResponseBody response))

-- | Blocking convenience wrapper around 'pollGrokDeviceCode' for CLI use.
completeGrokDeviceCodeLogin :: GrokLoginOptions -> GrokDeviceCode -> IO (Either Text GrokTokens)
completeGrokDeviceCodeLogin options deviceCode = go 0
  where
    timeoutSeconds = Maybe.fromMaybe (15 * 60) deviceCode.expiresInSeconds

    go elapsed
        | elapsed >= timeoutSeconds =
            pure (Left "device authorization timed out")
        | otherwise = pollGrokDeviceCode options deviceCode >>= \case
            Left err -> pure (Left err)
            Right (Just tokens) -> pure (Right tokens)
            Right Nothing -> do
                let seconds = deviceCode.pollIntervalSeconds
                threadDelay (seconds * 1_000_000)
                go (elapsed + seconds)

-- | Rotate an access token. Terminal rejections ('invalid_grant',
-- 'invalid_client') come back as 'AuthenticationError' so brokers mark the
-- account as needing reconnection; transport and 5xx failures stay retryable.
refreshGrokAccessToken :: GrokLoginOptions -> Text -> IO (Either ApiError GrokTokens)
refreshGrokAccessToken options refreshToken = do
    result <- tryAny $ postTokenForm options $
        [ ("grant_type", "refresh_token")
        , ("refresh_token", Text.encodeUtf8 refreshToken)
        , ("client_id", Text.encodeUtf8 options.clientId)
        ] <> principalFields options
    case result of
        Left exception -> pure $ Left $ ConnectionError
            ("Grok token refresh failed: " <> Text.pack (show exception))
        Right response -> do
            let status = getResponseStatusCode response
            if status >= 200 && status < 300
                then tryAny (parseTokens response) >>= \case
                    Left exception -> pure $ Left $ ProviderError AuthenticationError
                        ("Failed to parse Grok token refresh response: " <> Text.pack (show exception))
                        Nothing
                    Right tokens -> pure (Right tokens)
                else pure $ Left case oauthErrorCode response of
                    Just code | code `elem` ["invalid_grant", "invalid_client"] ->
                        ProviderError AuthenticationError
                            ("Grok token refresh rejected: " <> code)
                            Nothing
                    _ -> HttpError status
                        (Text.take 500 (Text.pack (lbsPreview (getResponseBody response))))

principalFields :: GrokLoginOptions -> [(BS.ByteString, BS.ByteString)]
principalFields options = Maybe.catMaybes
    [ (\value -> ("principal_type", Text.encodeUtf8 value)) <$> options.principalType
    , (\value -> ("principal_id", Text.encodeUtf8 value)) <$> options.principalId
    ]

postTokenForm :: GrokLoginOptions -> [(BS.ByteString, BS.ByteString)] -> IO (Response LBS.ByteString)
postTokenForm options formFields = do
    request <- parseRequest ("POST " <> trimIssuer options.issuer <> "/oauth2/token")
    httpLBS
        $ setRequestHeader "Content-Type" ["application/x-www-form-urlencoded"]
        $ setRequestBodyURLEncoded formFields request

parseTokens :: Response LBS.ByteString -> IO GrokTokens
parseTokens response = do
    object <- decodeObject "token response" response
    accessToken <- field "access_token" object
    pure GrokTokens
        { accessToken
        , refreshToken = textFieldMaybe "refresh_token" object
        , idToken = textFieldMaybe "id_token" object
        , expiresInSeconds = intFieldMaybe "expires_in" object
        }

-- | Stable per-account identity for pooling, from the access-token JWT
-- claims. Personal tokens carry the xAI user id in @sub@; team tokens
-- additionally carry the principal. No signature verification — the id only
-- namespaces accounts, it grants nothing.
deriveGrokAccountId :: Text -> Maybe Text
deriveGrokAccountId accessToken = do
    payload <- decodeJwtPayload accessToken
    textClaim "sub" payload
        `orElse` textClaim "principal_id" payload
        `orElse` textClaim "principalId" payload
  where
    textClaim name object = case KeyMap.lookup (Key.fromText name) object of
        Just (Aeson.String value) | not (Text.null value) -> Just value
        _ -> Nothing

    orElse (Just a) _ = Just a
    orElse Nothing b = b

decodeJwtPayload :: Text -> Maybe Aeson.Object
decodeJwtPayload token = do
    payloadB64 <- case Text.splitOn "." token of
        (_ : payload : _) -> Just payload
        _ -> Nothing
    let padded = base64UrlToBase64 payloadB64
    payloadBytes <- either (const Nothing) Just (Base64.decode (Text.encodeUtf8 padded))
    case Aeson.decode (LBS.fromStrict payloadBytes) of
        Just (Aeson.Object object) -> Just object
        _ -> Nothing

base64UrlToBase64 :: Text -> Text
base64UrlToBase64 input = padded
  where
    replaced = Text.map replace input
    replace '-' = '+'
    replace '_' = '/'
    replace c = c
    padding = (4 - Text.length replaced `mod` 4) `mod` 4
    padded = replaced <> Text.replicate padding "="

--------------------------------------------------------------------------------
-- Small helpers
--------------------------------------------------------------------------------

safely :: IO a -> IO (Either Text a)
safely action = tryAny action >>= \case
    Left exception -> pure (Left (Text.pack (show exception)))
    Right result -> pure (Right result)

oauthErrorCode :: Response LBS.ByteString -> Maybe Text
oauthErrorCode response = case Aeson.eitherDecode (getResponseBody response) of
    Right (Aeson.Object object) -> case KeyMap.lookup "error" object of
        Just (Aeson.String code) -> Just code
        _ -> Nothing
    _ -> Nothing

decodeObject :: String -> Response LBS.ByteString -> IO Aeson.Object
decodeObject label response = case Aeson.eitherDecode (getResponseBody response) of
    Left err -> fail (label <> " is invalid JSON: " <> err)
    Right (Aeson.Object object) -> pure object
    Right _ -> fail (label <> " is not a JSON object")

field :: Text -> Aeson.Object -> IO Text
field name object = case KeyMap.lookup (Key.fromText name) object of
    Just (Aeson.String value) -> pure value
    Just _ -> fail (Text.unpack name <> " is not a string")
    Nothing -> fail (Text.unpack name <> " is missing")

textFieldMaybe :: Text -> Aeson.Object -> Maybe Text
textFieldMaybe name object = case KeyMap.lookup (Key.fromText name) object of
    Just (Aeson.String value) -> Just value
    _ -> Nothing

intFieldMaybe :: Text -> Aeson.Object -> Maybe Int
intFieldMaybe name object = case KeyMap.lookup (Key.fromText name) object of
    Just (Aeson.Number value) -> Just (round value)
    Just (Aeson.String value) -> case reads (Text.unpack value) of
        [(number, "")] -> Just number
        _ -> Nothing
    _ -> Nothing

lbsPreview :: LBS.ByteString -> String
lbsPreview = Text.unpack . Text.take 300 . Text.decodeUtf8With (\_ _ -> Just '?') . LBS.toStrict

trimIssuer :: String -> String
trimIssuer = reverse . dropWhile (== '/') . reverse
