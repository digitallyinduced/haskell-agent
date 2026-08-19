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

import Agent.OpenAI.Auth (deriveAccountId)
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (tryAny)
import Control.Monad (unless)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock (getCurrentTime)
import Network.HTTP.Simple
import System.Directory (createDirectoryIfMissing, renameFile)
import System.FilePath (takeDirectory)
import System.Posix.Files (setFileMode)

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
    } deriving (Eq, Show)

data Tokens = Tokens
    { idToken :: !Text
    , accessToken :: !Text
    , refreshToken :: !Text
    }

requestDeviceCode :: LoginOptions -> IO (Either Text DeviceCode)
requestDeviceCode options = safely do
    request <- parseRequest (trimIssuer options.issuer <> "/api/accounts/deviceauth/usercode")
    response <- httpLBS
        $ setRequestMethod "POST"
        $ setRequestHeader "Content-Type" ["application/json"]
        $ setRequestBodyJSON (Aeson.object ["client_id" .= options.clientId]) request
    requireSuccess "device code request" response
    object <- decodeObject "device code response" response
    code <- field "user_code" object
    authId <- field "device_auth_id" object
    interval <- intervalField object
    pure DeviceCode
        { verificationUrl = trimIssuer options.issuer <> "/codex/device"
        , userCode = code
        , deviceAuthId = authId
        , pollIntervalSeconds = max 1 interval
        }

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
    request <- parseRequest (trimIssuer options.issuer <> "/api/accounts/deviceauth/token")
    response <- httpLBS
        $ setRequestMethod "POST"
        $ setRequestHeader "Content-Type" ["application/json"]
        $ setRequestBodyJSON (Aeson.object
            [ "device_auth_id" .= deviceCode.deviceAuthId
            , "user_code" .= deviceCode.userCode
            ]) request
    case getResponseStatusCode response of
        status | status >= 200 && status < 300 -> do
            codeResponse <- decodeObject "device authorization response" response
            authorizationCode <- field "authorization_code" codeResponse
            codeVerifier <- field "code_verifier" codeResponse
            Just <$> exchange authorizationCode codeVerifier
        403 -> pure Nothing
        404 -> pure Nothing
        status -> fail ("device authorization failed with HTTP " <> show status)
  where
    exchange authorizationCode codeVerifier = do
        request <- parseRequest (trimIssuer options.issuer <> "/oauth/token")
        response <- httpLBS
            $ setRequestMethod "POST"
            $ setRequestHeader "Content-Type" ["application/x-www-form-urlencoded"]
            $ setRequestBodyURLEncoded
                [ ("grant_type", "authorization_code")
                , ("code", encode authorizationCode)
                , ("redirect_uri", encode (Text.pack (trimIssuer options.issuer <> "/deviceauth/callback")))
                , ("client_id", encode options.clientId)
                , ("code_verifier", encode codeVerifier)
                ] request
        requireSuccess "OAuth token exchange" response
        object <- decodeObject "OAuth token response" response
        Tokens <$> field "id_token" object <*> field "access_token" object <*> field "refresh_token" object

writeAuthFile :: FilePath -> Aeson.Value -> IO ()
writeAuthFile path value = do
    createDirectoryIfMissing True (takeDirectory path)
    let temporary = path <> ".tmp"
    LBS.writeFile temporary (Aeson.encode value)
    setFileMode temporary 0o600
    renameFile temporary path

safely :: IO a -> IO (Either Text a)
safely action = tryAny action >>= \case
    Left exception -> pure (Left (Text.pack (show exception)))
    Right result -> pure (Right result)

requireSuccess label response = do
    let status = getResponseStatusCode response
    unless (status >= 200 && status < 300) $ fail
        (Text.unpack label <> " failed with HTTP " <> show status)

decodeObject label response = case Aeson.eitherDecode (getResponseBody response) of
    Left err -> fail (Text.unpack label <> " is invalid JSON: " <> err)
    Right (Aeson.Object object) -> pure object
    Right _ -> fail (Text.unpack label <> " is not a JSON object")

field name object = case KeyMap.lookup (Key.fromText name) object of
    Just value -> case Aeson.fromJSON value of
        Aeson.Success result -> pure result
        Aeson.Error err -> fail (Text.unpack name <> " is invalid: " <> err)
    Nothing -> fail (Text.unpack name <> " is missing")

intervalField object = case KeyMap.lookup "interval" object of
    Just (Aeson.String value) -> maybe invalid pure (readMaybeInt value)
    Just (Aeson.Number value) -> pure (round value)
    _ -> invalid
  where
    invalid = fail "interval is missing or invalid"

encode = Text.encodeUtf8

trimIssuer = reverse . dropWhile (== '/') . reverse

readMaybeInt value = case reads (Text.unpack value) of
    [(number, "")] -> Just number
    _ -> Nothing
