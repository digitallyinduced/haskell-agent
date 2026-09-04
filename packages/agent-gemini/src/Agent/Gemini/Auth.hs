-- | Google OAuth and Gemini Code Assist account setup.
--
-- The embedded client credentials are the public installed-application
-- credentials used by the official Gemini CLI. Installed-app client secrets
-- are application identifiers rather than confidential secrets. Deployments
-- can override the endpoints and client credentials through the environment.
module Agent.Gemini.Auth
    ( OAuthOptions(..)
    , defaultOAuthOptions
    , oauthOptionsFromEnv
    , OAuthTokens(..)
    , GeminiAuthState(..)
    , CodeAssistOptions(..)
    , defaultCodeAssistOptions
    , codeAssistOptionsFromEnv
    , CodeAssistUser(..)
    , runLoopbackOAuth
    , exchangeAuthorizationCode
    , refreshAccessToken
    , fetchUserEmail
    , setupCodeAssist
    , setupCodeAssistWithValidation
    , authenticateGoogleAccount
    , authorizationUrl
    , validateOAuthCallback
    , pkceChallenge
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
    ( newEmptyMVar
    , putMVar
    , readMVar
    , takeMVar
    , tryPutMVar
    )
import Control.Exception.Safe
    ( IOException
    , SomeException
    , bracket
    , finally
    , fromException
    , tryAny
    )
import Control.Monad (void)
import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.:?), (.!=))
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as Base64Url
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Bifunctor (first)
import Data.Char (isDigit, toLower)
import Data.List (find)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Network.HTTP.Simple
import Network.HTTP.Types
    ( hCacheControl
    , hContentLength
    , hLocation
    , methodGet
    , status302
    )
import Network.HTTP.Types.URI
    ( parseQueryText
    , queryToQueryText
    , renderQueryText
    )
import qualified Network.HTTP.Client as HttpClient
import qualified Network.Socket as Net
import qualified Network.URI as URI
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import System.Environment (lookupEnv)
import System.Entropy (getEntropy)
import System.IO.Error (ioeGetErrorString)
import System.Timeout (timeout)
import Text.Read (readMaybe)

data OAuthOptions = OAuthOptions
    { authorizationEndpoint :: !String
    , tokenEndpoint :: !String
    , userInfoEndpoint :: !String
    , clientId :: !Text
    , clientSecret :: !Text
    , scopes :: ![Text]
    , timeoutSeconds :: !Int
    } deriving (Eq)

instance Show OAuthOptions where
    show options =
        "OAuthOptions { authorizationEndpoint = "
            <> show options.authorizationEndpoint
            <> ", tokenEndpoint = " <> show options.tokenEndpoint
            <> ", userInfoEndpoint = " <> show options.userInfoEndpoint
            <> ", clientId = " <> show options.clientId
            <> ", clientSecret = <redacted>, scopes = " <> show options.scopes
            <> ", timeoutSeconds = " <> show options.timeoutSeconds <> " }"

-- | Public installed-app OAuth client from the official Gemini CLI.
defaultOAuthOptions :: OAuthOptions
defaultOAuthOptions = OAuthOptions
    { authorizationEndpoint = "https://accounts.google.com/o/oauth2/v2/auth"
    , tokenEndpoint = "https://oauth2.googleapis.com/token"
    , userInfoEndpoint = "https://www.googleapis.com/oauth2/v2/userinfo"
    , clientId =
        "681255809395-oo8ft2oprdrnp9e3aqf6av3hmdib135j.apps.googleusercontent.com"
    , clientSecret = "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"
    , scopes =
        [ "https://www.googleapis.com/auth/cloud-platform"
        , "https://www.googleapis.com/auth/userinfo.email"
        , "https://www.googleapis.com/auth/userinfo.profile"
        ]
    , timeoutSeconds = 5 * 60
    }

oauthOptionsFromEnv :: IO OAuthOptions
oauthOptionsFromEnv = do
    authorizationEndpoint <- envString
        "GEMINI_OAUTH_AUTHORIZATION_ENDPOINT"
        defaultOAuthOptions.authorizationEndpoint
    tokenEndpoint <- envString
        "GEMINI_OAUTH_TOKEN_ENDPOINT"
        defaultOAuthOptions.tokenEndpoint
    userInfoEndpoint <- envString
        "GEMINI_OAUTH_USERINFO_ENDPOINT"
        defaultOAuthOptions.userInfoEndpoint
    clientId <- envText "GEMINI_OAUTH_CLIENT_ID" defaultOAuthOptions.clientId
    clientSecret <- envText
        "GEMINI_OAUTH_CLIENT_SECRET"
        defaultOAuthOptions.clientSecret
    pure defaultOAuthOptions
        { authorizationEndpoint
        , tokenEndpoint
        , userInfoEndpoint
        , clientId
        , clientSecret
        }

data OAuthTokens = OAuthTokens
    { accessToken :: !Text
    , refreshToken :: !(Maybe Text)
    , expiresInSeconds :: !(Maybe Int)
    , tokenType :: !(Maybe Text)
    , scope :: !(Maybe Text)
    } deriving (Eq)

instance Show OAuthTokens where
    show tokens =
        "OAuthTokens { accessToken = <redacted>, refreshToken = "
            <> maybe "Nothing" (const "Just <redacted>") tokens.refreshToken
            <> ", expiresInSeconds = " <> show tokens.expiresInSeconds
            <> ", tokenType = " <> show tokens.tokenType
            <> ", scope = " <> show tokens.scope <> " }"

instance Aeson.FromJSON OAuthTokens where
    parseJSON = Aeson.withObject "OAuthTokens" \object -> OAuthTokens
        <$> object .: "access_token"
        <*> object .:? "refresh_token"
        <*> object .:? "expires_in"
        <*> object .:? "token_type"
        <*> object .:? "scope"

-- | Persisted Google credentials and Code Assist routing metadata.
data GeminiAuthState = GeminiAuthState
    { accessToken :: !Text
    , refreshToken :: !(Maybe Text)
    , expiresAt :: !(Maybe UTCTime)
    , email :: !Text
    , projectId :: !Text
    , userTier :: !Text
    } deriving (Eq)

instance Show GeminiAuthState where
    show state =
        "GeminiAuthState { accessToken = <redacted>, refreshToken = "
            <> maybe "Nothing" (const "Just <redacted>") state.refreshToken
            <> ", expiresAt = " <> show state.expiresAt
            <> ", email = " <> show state.email
            <> ", projectId = " <> show state.projectId
            <> ", userTier = " <> show state.userTier <> " }"

instance Aeson.ToJSON GeminiAuthState where
    toJSON state = Aeson.object
        [ "access_token" Aeson..= state.accessToken
        , "refresh_token" Aeson..= state.refreshToken
        , "expires_at" Aeson..= state.expiresAt
        , "email" Aeson..= state.email
        , "project_id" Aeson..= state.projectId
        , "user_tier" Aeson..= state.userTier
        ]

instance Aeson.FromJSON GeminiAuthState where
    parseJSON = Aeson.withObject "GeminiAuthState" \object -> GeminiAuthState
        <$> object .: "access_token"
        <*> object .:? "refresh_token"
        <*> object .:? "expires_at"
        <*> object .: "email"
        <*> object .: "project_id"
        <*> object .: "user_tier"

data CodeAssistOptions = CodeAssistOptions
    { baseUrl :: !String
    , configuredProject :: !(Maybe Text)
    , pollIntervalMicros :: !Int
    , maxPollAttempts :: !Int
    , requestTimeoutSeconds :: !Int
    } deriving (Eq, Show)

defaultCodeAssistOptions :: CodeAssistOptions
defaultCodeAssistOptions = CodeAssistOptions
    { baseUrl = "https://cloudcode-pa.googleapis.com/v1internal"
    , configuredProject = Nothing
    , pollIntervalMicros = 5 * 1_000_000
    , maxPollAttempts = 60
    , requestTimeoutSeconds = 60
    }

codeAssistOptionsFromEnv :: IO CodeAssistOptions
codeAssistOptionsFromEnv = do
    baseUrl <- envString
        "GEMINI_CODE_ASSIST_BASE_URL"
        defaultCodeAssistOptions.baseUrl
    googleCloudProject <- nonEmptyEnv "GOOGLE_CLOUD_PROJECT"
    googleCloudProjectId <- nonEmptyEnv "GOOGLE_CLOUD_PROJECT_ID"
    let configuredProject = googleCloudProject `orElse` googleCloudProjectId
    pure defaultCodeAssistOptions { baseUrl, configuredProject }

data CodeAssistUser = CodeAssistUser
    { projectId :: !Text
    , userTier :: !Text
    , userTierName :: !(Maybe Text)
    } deriving (Eq, Show)

-- | Start a PKCE-S256 loopback flow and exchange the callback code.
runLoopbackOAuth
    :: OAuthOptions
    -> (Text -> IO ())
    -> IO (Either Text OAuthTokens)
runLoopbackOAuth options presentAuthorizationUrl = safely $
    bracket openLoopbackSocket Net.close \listener -> do
        redirectUri <- loopbackRedirectUri listener
        state <- randomUrlText 32
        verifier <- randomUrlText 32
        presentAuthorizationUrl
            (authorizationUrl options redirectUri state (pkceChallenge verifier))
        callback <- timeout
            (max 1 options.timeoutSeconds * 1_000_000)
            (receiveOAuthCallback listener state)
        code <- case callback of
            Nothing -> fail
                ("Google OAuth authorization timed out after "
                    <> show options.timeoutSeconds <> " seconds")
            Just (Left err) -> fail (Text.unpack err)
            Just (Right value) -> pure value
        exchangeAuthorizationCode options redirectUri verifier code >>= \case
            Left err -> fail (Text.unpack err)
            Right tokens -> pure tokens

authorizationUrl :: OAuthOptions -> Text -> Text -> Text -> Text
authorizationUrl options redirectUri state challenge =
    Text.pack options.authorizationEndpoint
        <> TextEncoding.decodeUtf8
            (LBS.toStrict
                (Builder.toLazyByteString
                    (renderQueryText True
                        [ ("client_id", Just options.clientId)
                        , ("redirect_uri", Just redirectUri)
                        , ("response_type", Just "code")
                        , ("scope", Just (Text.unwords options.scopes))
                        , ("access_type", Just "offline")
                        , ("prompt", Just "consent")
                        , ("state", Just state)
                        , ("code_challenge", Just challenge)
                        , ("code_challenge_method", Just "S256")
                        ])))

validateOAuthCallback :: Text -> BS.ByteString -> Either Text Text
validateOAuthCallback expectedState requestTarget = do
    let (_, queryWithQuestion) = BS8.break (== '?') requestTarget
    validateOAuthParameters
        expectedState
        (parseQueryText (BS.drop 1 queryWithQuestion))

validateOAuthParameters
    :: Text
    -> [(Text, Maybe Text)]
    -> Either Text Text
validateOAuthParameters expectedState parameters = do
    let parameter key = lookup key parameters >>= id
    actualState <- maybe
        (Left "Google OAuth callback did not include state")
        Right
        (parameter "state")
    if actualState /= expectedState
        then Left "Google OAuth state mismatch; authorization was rejected"
        else do
            case parameter "error" of
                Just oauthError ->
                    Left ("Google OAuth rejected authorization: " <> oauthError
                        <> maybe "" (": " <>)
                            (parameter "error_description"))
                Nothing -> pure ()
            maybe
                (Left
                    "Google OAuth callback did not include an authorization code")
                Right
                (parameter "code")

pkceChallenge :: Text -> Text
pkceChallenge verifier =
    TextEncoding.decodeUtf8
        (Base64Url.encodeUnpadded
            (BA.convert
                (hash (TextEncoding.encodeUtf8 verifier) :: Digest SHA256)))

exchangeAuthorizationCode
    :: OAuthOptions
    -> Text
    -> Text
    -> Text
    -> IO (Either Text OAuthTokens)
exchangeAuthorizationCode options redirectUri verifier code =
    postTokenForm options
        [ ("grant_type", "authorization_code")
        , ("code", TextEncoding.encodeUtf8 code)
        , ("redirect_uri", TextEncoding.encodeUtf8 redirectUri)
        , ("code_verifier", TextEncoding.encodeUtf8 verifier)
        , ("client_id", TextEncoding.encodeUtf8 options.clientId)
        , ("client_secret", TextEncoding.encodeUtf8 options.clientSecret)
        ]

-- | Refresh, retaining the old refresh token if Google does not rotate it.
refreshAccessToken :: OAuthOptions -> Text -> IO (Either Text OAuthTokens)
refreshAccessToken options oldRefreshToken = do
    result <- postTokenForm options
        [ ("grant_type", "refresh_token")
        , ("refresh_token", TextEncoding.encodeUtf8 oldRefreshToken)
        , ("client_id", TextEncoding.encodeUtf8 options.clientId)
        , ("client_secret", TextEncoding.encodeUtf8 options.clientSecret)
        ]
    pure $ fmap
        (\(tokens :: OAuthTokens) -> OAuthTokens
            { accessToken = tokens.accessToken
            , refreshToken =
                tokens.refreshToken `orElse` Just oldRefreshToken
            , expiresInSeconds = tokens.expiresInSeconds
            , tokenType = tokens.tokenType
            , scope = tokens.scope
            })
        result

fetchUserEmail :: OAuthOptions -> Text -> IO (Either Text Text)
fetchUserEmail options bearerToken = safely do
    request <- parseRequest options.userInfoEndpoint
    response <- httpLBS
        $ setRequestHeader "Authorization"
            ["Bearer " <> TextEncoding.encodeUtf8 bearerToken]
        $ setRequestHeader "Accept" ["application/json"]
        $ withResponseTimeout options.timeoutSeconds
        $ withoutRedirects request
    ensureSuccess bearerToken "Google userinfo request" response
    userInfo <-
        (decodeBody "Google userinfo response" response :: IO UserInfo)
    let email = Text.strip userInfo.email
    if Text.null email
        then fail "Google userinfo response contained an empty email"
        else pure email

setupCodeAssist
    :: CodeAssistOptions
    -> Text
    -> IO (Either Text CodeAssistUser)
setupCodeAssist options bearerToken =
    setupCodeAssistWithValidation options bearerToken Nothing

-- | Set up Code Assist, optionally presenting Google's one-time account
-- validation page and polling until validation completes.
setupCodeAssistWithValidation
    :: CodeAssistOptions
    -> Text
    -> Maybe (Text -> Text -> IO ())
    -> IO (Either Text CodeAssistUser)
setupCodeAssistWithValidation options bearerToken validationPresenter =
    first (redactSecret bearerToken) <$> safely do
    validateConfiguredProject options.configuredProject
    loadResponse <- loadCodeAssistWithValidation
        options bearerToken validationPresenter
    case loadResponse.currentTier of
        Just tier -> do
            project <- requireProject loadResponse options.configuredProject
            let paidTierId = loadResponse.paidTier >>= (.tierId)
                paidTierName = loadResponse.paidTier >>= (.name)
            pure CodeAssistUser
                { projectId = project
                , userTier =
                    fromMaybe "standard-tier"
                        (paidTierId `orElse` tier.tierId)
                , userTierName = paidTierName `orElse` tier.name
                }
        Nothing -> do
            let tier = fromMaybe legacyTier
                    (find (.isDefault) loadResponse.allowedTiers)
                onboardProject
                    | tier.tierId == Just "free-tier" = Nothing
                    | otherwise = options.configuredProject
            operation <- postCodeAssist
                options bearerToken ":onboardUser"
                (OnboardRequest tier onboardProject)
            completed <- awaitOperation options bearerToken operation
            project <- case completed.response >>= (.project) >>= (.projectId) of
                Just value | not (Text.null (Text.strip value)) -> pure value
                _ -> do
                    rejectIneligible loadResponse
                    requireProject loadResponse options.configuredProject
            pure CodeAssistUser
                { projectId = project
                , userTier = fromMaybe "standard-tier" tier.tierId
                , userTierName = tier.name
                }

authenticateGoogleAccount
    :: OAuthOptions
    -> CodeAssistOptions
    -> (Text -> IO ())
    -> IO (Either Text GeminiAuthState)
authenticateGoogleAccount oauthOptions codeAssistOptions presentUrl =
    timeout
        (max 1 oauthOptions.timeoutSeconds * 1_000_000)
        authenticate >>= \case
            Nothing ->
                pure $ Left
                    ("Google sign-in timed out after "
                        <> Text.pack (show oauthOptions.timeoutSeconds)
                        <> " seconds")
            Just result -> pure result
  where
    authenticate = runLoopbackOAuth oauthOptions presentUrl >>= \case
        Left err -> pure (Left err)
        Right tokens -> fetchUserEmail oauthOptions tokens.accessToken >>= \case
            Left err -> pure (Left err)
            Right email -> setupCodeAssistWithValidation
                codeAssistOptions
                tokens.accessToken
                (Just (\url _description -> presentUrl url)) >>= \case
                    Left err -> pure (Left err)
                    Right user -> do
                        now <- getCurrentTime
                        pure $ Right GeminiAuthState
                            { accessToken = tokens.accessToken
                            , refreshToken = tokens.refreshToken
                            , expiresAt =
                                (`addUTCTime` now)
                                    . fromIntegral
                                    <$> tokens.expiresInSeconds
                            , email
                            , projectId = user.projectId
                            , userTier = user.userTier
                            }

--------------------------------------------------------------------------------
-- Loopback listener
--------------------------------------------------------------------------------

openLoopbackSocket :: IO Net.Socket
openLoopbackSocket = do
    socket <- Net.socket Net.AF_INET Net.Stream Net.defaultProtocol
    Net.setSocketOption socket Net.ReuseAddr 1
    Net.bind socket
        (Net.SockAddrInet 0 (Net.tupleToHostAddress (127, 0, 0, 1)))
    Net.listen socket 1
    pure socket

loopbackRedirectUri :: Net.Socket -> IO Text
loopbackRedirectUri socket = Net.getSocketName socket >>= \case
    Net.SockAddrInet port _ ->
        pure ("http://127.0.0.1:" <> Text.pack (show port) <> "/oauth2callback")
    _ -> fail "Google OAuth loopback listener did not bind an IPv4 address"

receiveOAuthCallback :: Net.Socket -> Text -> IO (Either Text Text)
receiveOAuthCallback listener expectedState = do
    resultVar <- newEmptyMVar
    shutdownVar <- newEmptyMVar
    let settings =
            Warp.setHost "127.0.0.1"
                $ Warp.setMaxTotalHeaderLength 16_384
                $ Warp.setInstallShutdownHandler (putMVar shutdownVar)
                $ Warp.defaultSettings
        application request respond = do
            let result
                    | Wai.requestMethod request /= methodGet =
                        Left "Google OAuth callback must use GET"
                    | Wai.rawPathInfo request /= "/oauth2callback" =
                        Left "Google OAuth callback used an unexpected path"
                    | otherwise =
                        validateOAuthParameters
                            expectedState
                            (queryToQueryText (Wai.queryString request))
                location = case result of
                    Right _ ->
                        "https://developers.google.com/gemini-code-assist/auth_success_gemini"
                    Left _ ->
                        "https://developers.google.com/gemini-code-assist/auth_failure_gemini"
                finish = do
                    void (tryPutMVar resultVar result)
                    readMVar shutdownVar >>= id
            respond
                (Wai.responseLBS
                    status302
                    [ (hLocation, location)
                    , (hContentLength, "0")
                    , (hCacheControl, "no-store")
                    ]
                    "")
                `finally` finish
    Warp.runSettingsSocket settings listener application
    takeMVar resultVar

--------------------------------------------------------------------------------
-- HTTP and Code Assist JSON
--------------------------------------------------------------------------------

postTokenForm
    :: OAuthOptions
    -> [(BS.ByteString, BS.ByteString)]
    -> IO (Either Text OAuthTokens)
postTokenForm options form = safely do
    request <- parseRequest ("POST " <> options.tokenEndpoint)
    response <- httpLBS
        $ setRequestHeader "Content-Type" ["application/x-www-form-urlencoded"]
        $ setRequestBodyURLEncoded form
        $ withResponseTimeout options.timeoutSeconds
        $ withoutRedirects request
    ensureSuccessWithoutBody "Google OAuth token request" response
    decodeBody "Google OAuth token response" response

postCodeAssist
    :: (Aeson.ToJSON request, Aeson.FromJSON response)
    => CodeAssistOptions
    -> Text
    -> String
    -> request
    -> IO response
postCodeAssist options token suffix body = do
    request <- parseRequest ("POST " <> trimSlash options.baseUrl <> suffix)
    response <- httpLBS
        $ setRequestHeader "Authorization"
            ["Bearer " <> TextEncoding.encodeUtf8 token]
        $ setRequestHeader "Content-Type" ["application/json"]
        $ setRequestBodyJSON body
        $ withResponseTimeout options.requestTimeoutSeconds
        $ withoutRedirects request
    ensureSuccess token ("Gemini Code Assist " <> suffix) response
    decodeBody ("Gemini Code Assist " <> suffix <> " response") response

getCodeAssist
    :: Aeson.FromJSON response
    => CodeAssistOptions
    -> Text
    -> Text
    -> IO response
getCodeAssist options token operationName = do
    request <- parseRequest
        (trimSlash options.baseUrl <> "/" <> Text.unpack operationName)
    response <- httpLBS
        $ setRequestHeader "Authorization"
            ["Bearer " <> TextEncoding.encodeUtf8 token]
        $ withResponseTimeout options.requestTimeoutSeconds
        $ withoutRedirects request
    ensureSuccess token "Gemini Code Assist operation poll" response
    decodeBody "Gemini Code Assist operation response" response

awaitOperation
    :: CodeAssistOptions
    -> Text
    -> Operation
    -> IO Operation
awaitOperation options token = go 0
  where
    go :: Int -> Operation -> IO Operation
    go attempt operation
        | Just operationError <- operation.operationError =
            fail (Text.unpack (renderOperationError operationError))
        | operation.done = pure operation
        | attempt >= max 0 options.maxPollAttempts =
            fail "Gemini Code Assist onboarding timed out"
        | otherwise = case operation.name of
            Nothing -> fail
                "Gemini Code Assist onboarding returned an unfinished operation without a name"
            Just name -> do
                threadDelay (max 0 options.pollIntervalMicros)
                next <- getCodeAssist options token name
                go (attempt + 1) next

data UserInfo = UserInfo { email :: !Text }
instance Aeson.FromJSON UserInfo where
    parseJSON = Aeson.withObject "UserInfo" \object ->
        UserInfo <$> object .: "email"

data ClientMetadata = ClientMetadata
instance Aeson.ToJSON ClientMetadata where
    toJSON _ = Aeson.object
        [ "ideType" Aeson..= ("IDE_UNSPECIFIED" :: Text)
        , "platform" Aeson..= ("PLATFORM_UNSPECIFIED" :: Text)
        , "pluginType" Aeson..= ("GEMINI" :: Text)
        ]

newtype LoadRequest = LoadRequest (Maybe Text)
instance Aeson.ToJSON LoadRequest where
    toJSON (LoadRequest project) = Aeson.object $
        maybe [] (\value -> ["cloudaicompanionProject" Aeson..= value]) project
        <> [ "metadata" Aeson..= Aeson.object
                ( [ "ideType" Aeson..= ("IDE_UNSPECIFIED" :: Text)
                  , "platform" Aeson..= ("PLATFORM_UNSPECIFIED" :: Text)
                  , "pluginType" Aeson..= ("GEMINI" :: Text)
                  ]
                  <> maybe [] (\value -> ["duetProject" Aeson..= value]) project
                )
           ]

data Tier = Tier
    { tierId :: !(Maybe Text)
    , name :: !(Maybe Text)
    , isDefault :: !Bool
    }
instance Aeson.FromJSON Tier where
    parseJSON = Aeson.withObject "Tier" \object -> Tier
        <$> object .:? "id"
        <*> object .:? "name"
        <*> object .:? "isDefault" .!= False

legacyTier :: Tier
legacyTier = Tier (Just "legacy-tier") Nothing True

data IneligibleTier = IneligibleTier
    { reasonMessage :: !(Maybe Text)
    , reasonCode :: !(Maybe Text)
    , validationUrl :: !(Maybe Text)
    }
instance Aeson.FromJSON IneligibleTier where
    parseJSON = Aeson.withObject "IneligibleTier" \object -> IneligibleTier
        <$> object .:? "reasonMessage"
        <*> object .:? "reasonCode"
        <*> object .:? "validationUrl"

data LoadResponse = LoadResponse
    { currentTier :: !(Maybe Tier)
    , paidTier :: !(Maybe Tier)
    , allowedTiers :: ![Tier]
    , ineligibleTiers :: ![IneligibleTier]
    , cloudaicompanionProject :: !(Maybe Text)
    }
instance Aeson.FromJSON LoadResponse where
    parseJSON = Aeson.withObject "LoadResponse" \object -> LoadResponse
        <$> object .:? "currentTier"
        <*> object .:? "paidTier"
        <*> object .:? "allowedTiers" .!= []
        <*> object .:? "ineligibleTiers" .!= []
        <*> object .:? "cloudaicompanionProject"

data OnboardRequest = OnboardRequest !Tier !(Maybe Text)
instance Aeson.ToJSON OnboardRequest where
    toJSON (OnboardRequest tier project) = Aeson.object $
        maybe [] (\value -> ["tierId" Aeson..= value]) tier.tierId
        <> maybe [] (\value -> ["cloudaicompanionProject" Aeson..= value]) project
        <> [ "metadata" Aeson..= case project of
                Nothing -> Aeson.toJSON ClientMetadata
                Just value -> Aeson.object
                    [ "ideType" Aeson..= ("IDE_UNSPECIFIED" :: Text)
                    , "platform" Aeson..= ("PLATFORM_UNSPECIFIED" :: Text)
                    , "pluginType" Aeson..= ("GEMINI" :: Text)
                    , "duetProject" Aeson..= value
                    ]
           ]

data Project = Project { projectId :: !(Maybe Text) }
instance Aeson.FromJSON Project where
    parseJSON = Aeson.withObject "Project" \object ->
        Project <$> object .:? "id"

newtype OnboardResponse = OnboardResponse { project :: Maybe Project }
instance Aeson.FromJSON OnboardResponse where
    parseJSON = Aeson.withObject "OnboardResponse" \object ->
        OnboardResponse <$> object .:? "cloudaicompanionProject"

data OperationError = OperationError
    { code :: !(Maybe Int)
    , message :: !(Maybe Text)
    }
instance Aeson.FromJSON OperationError where
    parseJSON = Aeson.withObject "OperationError" \object -> OperationError
        <$> object .:? "code"
        <*> object .:? "message"

data Operation = Operation
    { name :: !(Maybe Text)
    , done :: !Bool
    , response :: !(Maybe OnboardResponse)
    , operationError :: !(Maybe OperationError)
    }
instance Aeson.FromJSON Operation where
    parseJSON = Aeson.withObject "Operation" \object -> Operation
        <$> object .:? "name"
        <*> object .:? "done" .!= False
        <*> object .:? "response"
        <*> object .:? "error"

renderOperationError :: OperationError -> Text
renderOperationError operationError =
    "Gemini Code Assist onboarding failed"
        <> maybe "" (\code -> " (code " <> Text.pack (show code) <> ")")
            operationError.code
        <> maybe "" (": " <>) operationError.message

requireProject :: LoadResponse -> Maybe Text -> IO Text
requireProject response configured = case
    response.cloudaicompanionProject `orElse` configured of
        Just project | not (Text.null (Text.strip project)) -> pure project
        _ -> fail
            "This Google account requires GOOGLE_CLOUD_PROJECT or GOOGLE_CLOUD_PROJECT_ID"

rejectIneligible :: LoadResponse -> IO ()
rejectIneligible response = case listToMaybe response.ineligibleTiers of
    Nothing -> pure ()
    Just tier -> fail . Text.unpack $
        fromMaybe "This Google account is not eligible for Gemini Code Assist"
            tier.reasonMessage
        <> maybe "" ("\nValidate the account at: " <>) tier.validationUrl

rejectValidationRequired :: LoadResponse -> IO ()
rejectValidationRequired response =
    case find
        (\tier ->
            tier.reasonCode == Just "VALIDATION_REQUIRED"
                && maybe False (not . Text.null . Text.strip)
                    tier.validationUrl)
        response.ineligibleTiers of
        Nothing -> pure ()
        Just tier -> fail . Text.unpack $
            fromMaybe "Google account validation is required"
                tier.reasonMessage
            <> maybe "" ("\nValidate the account at: " <>) tier.validationUrl

loadCodeAssistWithValidation
    :: CodeAssistOptions
    -> Text
    -> Maybe (Text -> Text -> IO ())
    -> IO LoadResponse
loadCodeAssistWithValidation options token presenter = go 0 Nothing
  where
    go attempts lastPresentedUrl = do
        response <- postCodeAssist
            options token ":loadCodeAssist"
            (LoadRequest options.configuredProject)
        case validationRequirement response of
            Nothing -> pure response
            Just (url, description) -> case presenter of
                Nothing -> rejectValidationRequired response >> pure response
                Just present -> do
                    validationUrl <- requireHttpsValidationUrl url
                    if Just validationUrl == lastPresentedUrl
                        then pure ()
                        else present validationUrl description
                    if attempts >= max 0 options.maxPollAttempts
                        then fail
                            "Google account validation did not complete before the retry limit"
                        else do
                            threadDelay (max 0 options.pollIntervalMicros)
                            go (attempts + 1) (Just validationUrl)

validationRequirement :: LoadResponse -> Maybe (Text, Text)
validationRequirement response = do
    Nothing <- pure response.currentTier
    tier <- find
        (\candidate ->
            candidate.reasonCode == Just "VALIDATION_REQUIRED"
                && maybe False (not . Text.null . Text.strip)
                    candidate.validationUrl)
        response.ineligibleTiers
    url <- Text.strip <$> tier.validationUrl
    pure
        ( url
        , fromMaybe "Google account validation is required"
            tier.reasonMessage
        )

requireHttpsValidationUrl :: Text -> IO Text
requireHttpsValidationUrl url =
    case URI.parseURI (Text.unpack url) of
        Just uri
            | map toLower (URI.uriScheme uri) == "https:"
            , Just authority <- URI.uriAuthority uri
            , null (URI.uriUserInfo authority)
            , not (null (URI.uriRegName authority))
            , validUriPort (URI.uriPort authority) ->
                pure url
        _ ->
            fail "Google account validation returned an invalid non-HTTPS URL"

validUriPort :: String -> Bool
validUriPort "" = True
validUriPort (':' : digits)
    | not (null digits)
    , all isDigit digits
    , Just port <- readMaybe digits =
        port > (0 :: Int) && port < 65_536
validUriPort _ = False

validateConfiguredProject :: Maybe Text -> IO ()
validateConfiguredProject = \case
    Just project
        | not (Text.null project) && Text.all (`elem` ['0'..'9']) project ->
            fail ("GOOGLE_CLOUD_PROJECT must be a project ID, not numeric project number: "
                <> Text.unpack project)
    _ -> pure ()

ensureSuccess :: Text -> String -> Response LBS.ByteString -> IO ()
ensureSuccess secret label response = do
    let status = getResponseStatusCode response
    if status >= 200 && status < 300
        then pure ()
        else fail (label <> " failed with HTTP " <> show status <> ": "
            <> Text.unpack bodyText)
  where
    bodyText =
        Text.take 500
            (redactSecret secret
                (TextEncoding.decodeUtf8With
                    (\_ _ -> Just '\xfffd')
                    (LBS.toStrict (getResponseBody response))))

redactSecret :: Text -> Text -> Text
redactSecret secret text
    | Text.null secret = text
    | otherwise = Text.replace secret "<redacted>" text

decodeBody
    :: Aeson.FromJSON value
    => String
    -> Response LBS.ByteString
    -> IO value
decodeBody label response = case Aeson.eitherDecode (getResponseBody response) of
    Left err -> fail (label <> " was invalid JSON: " <> err)
    Right value -> pure value

safely :: IO value -> IO (Either Text value)
safely action = tryAny action >>= \case
    Left exception -> pure (Left (safeExceptionText exception))
    Right value -> pure (Right value)

-- Never render an HTTP request here. Token-exchange request bodies contain
-- authorization codes, refresh tokens, and the installed-app client secret.
safeExceptionText :: SomeException -> Text
safeExceptionText exception =
    case fromException exception :: Maybe HttpClient.HttpException of
        Just (HttpClient.InvalidUrlException url reason) ->
            "invalid Google endpoint URL "
                <> Text.pack url <> ": " <> Text.pack reason
        Just HttpClient.HttpExceptionRequest{} ->
            "Google HTTP request failed"
        Nothing -> case fromException exception :: Maybe IOException of
            Just ioError -> Text.pack (ioeGetErrorString ioError)
            Nothing -> Text.pack (show exception)

ensureSuccessWithoutBody
    :: String
    -> Response LBS.ByteString
    -> IO ()
ensureSuccessWithoutBody label response = do
    let status = getResponseStatusCode response
    if status >= 200 && status < 300
        then pure ()
        else fail (label <> " failed with HTTP " <> show status)

-- Credential-bearing API requests must never follow redirects. http-client's
-- default redirect policy can preserve custom Authorization/API-key headers,
-- and 307/308 redirects can also replay OAuth token form bodies.
withoutRedirects :: Request -> Request
withoutRedirects request = request { HttpClient.redirectCount = 0 }

withResponseTimeout :: Int -> Request -> Request
withResponseTimeout timeoutSeconds request =
    request
        { HttpClient.responseTimeout =
            HttpClient.responseTimeoutMicro
                (max 1 timeoutSeconds * 1_000_000)
        }

randomUrlText :: Int -> IO Text
randomUrlText bytes =
    TextEncoding.decodeUtf8 . Base64Url.encodeUnpadded <$> getEntropy bytes

trimSlash :: String -> String
trimSlash = reverse . dropWhile (== '/') . reverse

orElse :: Maybe value -> Maybe value -> Maybe value
orElse (Just value) _ = Just value
orElse Nothing right = right

nonEmptyEnv :: String -> IO (Maybe Text)
nonEmptyEnv key = lookupEnv key >>= \case
    Just value | not (Text.null (Text.strip (Text.pack value))) ->
        pure (Just (Text.strip (Text.pack value)))
    _ -> pure Nothing

envString :: String -> String -> IO String
envString key fallback = maybe fallback Text.unpack <$> nonEmptyEnv key

envText :: String -> Text -> IO Text
envText key fallback = fromMaybe fallback <$> nonEmptyEnv key
