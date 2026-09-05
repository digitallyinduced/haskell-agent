-- | Provider account connection requests, independent of the native ABI.
module Agent.CLI.MacOS.AccountConnection
    ( AccountProviderRequest(..)
    , AccountOAuthPollRequest(..)
    , AccountAPIKeyRequest(..)
    , startAccountOAuth
    , pollAccountOAuth
    , connectAccountAPIKey
    ) where

import Agent.CLI.Auth
    ( GrokAuthState(..)
    , grokAuthStateToJson
    , openAIOAuthClientId
    , openaiAuthStateFromJson
    , xaiOAuthClientId
    )
import Agent.CLI.CredentialStore (ManagedAuthKind(..))
import Agent.CLI.Environment (lookupNonEmpty)
import Agent.CLI.Login (storeConnectedCredential)
import qualified Agent.OpenAI.Auth as OpenAIAuth
import qualified Agent.OpenAI.Auth.Types as OpenAIAuthTypes
import qualified Agent.OpenAI.Login as OpenAILogin
import qualified Agent.OpenRouter.Usage as OpenRouter
import Agent.Provider (Provider(..), parseProvider, BillingMode(..))
import qualified Agent.XAI.Auth as XAIAuth
import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Clock (addUTCTime, getCurrentTime)

data AccountProviderRequest = AccountProviderRequest
    { accountProvider :: !Text
    }

data AccountOAuthPollRequest = AccountOAuthPollRequest
    { oauthPollProvider :: !Text
    , oauthPollVerificationUrl :: !(Maybe Text)
    , oauthPollUserCode :: !(Maybe Text)
    , oauthPollDeviceAuthId :: !(Maybe Text)
    , oauthPollDeviceCode :: !(Maybe Text)
    , oauthPollIntervalSeconds :: !(Maybe Int)
    , oauthPollExpiresInSeconds :: !(Maybe Int)
    }

data AccountAPIKeyRequest = AccountAPIKeyRequest
    { accountAPIKeyProvider :: !Text
    , accountAPIKey :: !Text
    }

startAccountOAuth
    :: AccountProviderRequest
    -> IO (Either Text Aeson.Value)
startAccountOAuth request =
    case parseProvider request.accountProvider of
        Just OpenAIProvider -> do
            clientId <- openAIOAuthClientId <$> lookupNonEmpty
                "OPENAI_OAUTH_CLIENT_ID"
            OpenAILogin.requestDeviceCode
                (OpenAILogin.defaultLoginOptions clientId) >>= \case
                    Left err -> pure (Left err)
                    Right code -> pure $ Right (Aeson.object
                        [ "provider" Aeson..= ("openai" :: Text)
                        , "verificationUrl" Aeson..= code.verificationUrl
                        , "userCode" Aeson..= code.userCode
                        , "deviceAuthId" Aeson..= code.deviceAuthId
                        , "pollIntervalSeconds" Aeson..=
                            code.pollIntervalSeconds
                        ])
        Just XAIProvider -> do
            clientId <- xaiOAuthClientId <$> lookupNonEmpty
                "XAI_OAUTH_CLIENT_ID"
            XAIAuth.requestDeviceAuthorization
                (XAIAuth.defaultOAuthOptions clientId) >>= \case
                    Left err -> pure (Left err)
                    Right code -> pure $ Right (Aeson.object
                        [ "provider" Aeson..= ("xai" :: Text)
                        , "verificationUrl" Aeson..= code.verificationUrl
                        , "userCode" Aeson..= code.userCode
                        , "deviceCode" Aeson..= code.deviceCode
                        , "pollIntervalSeconds" Aeson..=
                            code.pollIntervalSeconds
                        , "expiresInSeconds" Aeson..=
                            code.expiresInSeconds
                        ])
        _ -> pure (Left "OAuth account connection is not supported for this provider")

pollAccountOAuth
    :: AccountOAuthPollRequest
    -> IO (Either Text Aeson.Value)
pollAccountOAuth request =
    case parseProvider request.oauthPollProvider of
        Just OpenAIProvider -> case
            (request.oauthPollVerificationUrl, request.oauthPollUserCode,
                request.oauthPollDeviceAuthId) of
            (Just url, Just userCode, Just authId) ->
                do
                    clientId <- openAIOAuthClientId <$> lookupNonEmpty
                        "OPENAI_OAUTH_CLIENT_ID"
                    OpenAILogin.pollDeviceCode
                        (OpenAILogin.defaultLoginOptions clientId)
                        OpenAILogin.DeviceCode
                            { OpenAILogin.verificationUrl = Text.unpack url
                            , OpenAILogin.userCode = userCode
                            , OpenAILogin.deviceAuthId = authId
                            , OpenAILogin.pollIntervalSeconds =
                                fromMaybe 5 request.oauthPollIntervalSeconds
                            } >>= \case
                            Left err -> pure (Left err)
                            Right Nothing -> pure $ Right
                                (Aeson.object ["status" Aeson..= ("pending" :: Text)])
                            Right (Just authJson) -> do
                                now <- getCurrentTime
                                case openaiAuthStateFromJson now
                                    (Aeson.encode authJson) of
                                    Nothing -> pure (Left
                                        "OpenAI returned invalid account data")
                                    Just auth -> do
                                        let accountId =
                                                case auth of
                                                    OpenAIAuthTypes.AuthState
                                                        _ _ value _ _ -> value
                                        stored <- storeConnectedCredential
                                            False OpenAIProvider
                                                accountId
                                            "ChatGPT" SubscriptionBilled
                                            ManagedOpenAIAuthJson
                                            (TextEncoding.decodeUtf8
                                                (LBS.toStrict
                                                    (Aeson.encode authJson)))
                                        pure $ if stored
                                            then Right (Aeson.object
                                                [ "status" Aeson..=
                                                    ("connected" :: Text)
                                                , "accountID" Aeson..=
                                                    auth.accountId
                                                ])
                                            else Left "could not store account"
            _ -> pure (Left "OAuth challenge is missing required fields")
        Just XAIProvider -> case
            (request.oauthPollUserCode, request.oauthPollDeviceCode) of
            (Just _, Just deviceCode) -> do
                clientId <- xaiOAuthClientId <$> lookupNonEmpty
                    "XAI_OAUTH_CLIENT_ID"
                let challenge = XAIAuth.DeviceAuthorization
                        { XAIAuth.deviceCode = deviceCode
                        , XAIAuth.userCode = fromMaybe "" request.oauthPollUserCode
                        , XAIAuth.verificationUrl =
                            fromMaybe "" request.oauthPollVerificationUrl
                        , XAIAuth.pollIntervalSeconds =
                            fromMaybe 5 request.oauthPollIntervalSeconds
                        , XAIAuth.expiresInSeconds =
                            request.oauthPollExpiresInSeconds
                        }
                XAIAuth.pollDeviceAuthorization
                    (XAIAuth.defaultOAuthOptions clientId) challenge >>= \case
                        Left err -> pure (Left err)
                        Right Nothing -> pure $ Right
                            (Aeson.object ["status" Aeson..= ("pending" :: Text)])
                        Right (Just tokens) -> do
                            now <- getCurrentTime
                            let accountId = fromMaybe "grok"
                                    (XAIAuth.accountIdFromAccessToken
                                        tokens.accessToken)
                                label = fromMaybe "Grok" $
                                    (tokens.idToken >>= XAIAuth.emailFromToken)
                                    <|> XAIAuth.emailFromToken tokens.accessToken
                                authJson = grokAuthStateToJson GrokAuthState
                                    { grokAccessToken = tokens.accessToken
                                    , grokRefreshToken = tokens.refreshToken
                                    , grokIdToken = tokens.idToken
                                    , grokExpiresAt =
                                        ((`addUTCTime` now) . fromIntegral
                                            <$> tokens.expiresInSeconds)
                                    }
                            case tokens.refreshToken of
                                Nothing -> pure (Left
                                    "Grok login did not return a refresh token")
                                Just _ -> do
                                    stored <- storeConnectedCredential
                                        False XAIProvider accountId label
                                        SubscriptionBilled ManagedGrokAuthJson
                                        (TextEncoding.decodeUtf8
                                            (LBS.toStrict
                                                (Aeson.encode authJson)))
                                    pure $ if stored
                                        then Right (Aeson.object
                                            [ "status" Aeson..=
                                                ("connected" :: Text)
                                            , "accountID" Aeson..= accountId
                                            ])
                                        else Left "could not store account"
            _ -> pure (Left "OAuth challenge is missing required fields")
        _ -> pure (Left "OAuth account connection is not supported for this provider")

connectAccountAPIKey
    :: AccountAPIKeyRequest
    -> IO (Either Text Aeson.Value)
connectAccountAPIKey request =
    case parseProvider request.accountAPIKeyProvider of
        Just OpenRouterProvider
            | not (Text.null (Text.strip request.accountAPIKey)) ->
                OpenRouter.fetchOpenRouterUsage request.accountAPIKey >>= \case
                    Left err -> pure (Left ("OpenRouter rejected the key: " <> err))
                    Right usage -> do
                        let accountId = fromMaybe "openrouter" usage.keyLabel
                            label = fromMaybe "OpenRouter" usage.keyLabel
                        stored <- storeConnectedCredential
                            False OpenRouterProvider accountId label ApiBilled
                            ManagedBearerToken request.accountAPIKey
                        pure $ if stored
                            then Right (Aeson.object
                                [ "status" Aeson..= ("connected" :: Text)
                                , "accountID" Aeson..= accountId
                                ])
                            else Left "could not store account"
        _ -> pure (Left "API-key connections are supported for OpenRouter")
