{-# LANGUAGE OverloadedStrings #-}
module Agent.MCP.OAuth
    ( OAuthTokens(..), OAuthTokenResponse(..), ProtectedResourceMetadata(..)
    , AuthorizationServerMetadata(..), ClientRegistration(..)
    , discoverProtectedResource, discoverAuthorizationServer, registerClient
    , refreshAccessToken, oauthCallbackSuccessPage
    ) where

import Control.Exception.Safe (tryAny)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Network.HTTP.Client (Manager, RequestBody(..), httpLbs, parseRequest, responseBody, responseStatus, urlEncodedBody)
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types (statusCode)

data OAuthTokens = OAuthTokens { accessToken :: !Text, refreshToken :: !(Maybe Text), expiresIn :: !(Maybe Int) } deriving (Eq)
instance Show OAuthTokens where
    show tokens = "OAuthTokens { accessToken = <redacted>, refreshToken = " <> show (maybe False (const True) tokens.refreshToken) <> " }"
data OAuthTokenResponse = OAuthTokenSuccess !OAuthTokens | OAuthTokenFailure !Text deriving (Eq, Show)

data ProtectedResourceMetadata = ProtectedResourceMetadata { resource :: !(Maybe Text), authorizationServers :: ![Text], scopesSupported :: ![Text] } deriving (Eq, Show)
instance Aeson.FromJSON ProtectedResourceMetadata where
    parseJSON = Aeson.withObject "ProtectedResourceMetadata" $ \o -> ProtectedResourceMetadata <$> o Aeson..:? "resource" <*> o Aeson..:? "authorization_servers" Aeson..!= [] <*> o Aeson..:? "scopes_supported" Aeson..!= []

data AuthorizationServerMetadata = AuthorizationServerMetadata { issuer :: !(Maybe Text), authorizationEndpoint :: !Text, tokenEndpoint :: !Text, registrationEndpoint :: !(Maybe Text), codeChallengeMethodsSupported :: ![Text], scopesSupportedByServer :: ![Text] } deriving (Eq, Show)
instance Aeson.FromJSON AuthorizationServerMetadata where
    parseJSON = Aeson.withObject "AuthorizationServerMetadata" $ \o -> AuthorizationServerMetadata <$> o Aeson..:? "issuer" <*> o Aeson..: "authorization_endpoint" <*> o Aeson..: "token_endpoint" <*> o Aeson..:? "registration_endpoint" <*> o Aeson..:? "code_challenge_methods_supported" Aeson..!= [] <*> o Aeson..:? "scopes_supported" Aeson..!= []

data ClientRegistration = ClientRegistration { clientId :: !Text, clientSecret :: !(Maybe Text) } deriving (Eq)
instance Show ClientRegistration where
    show registration = "ClientRegistration { clientId = <redacted>, clientSecret = " <> show (maybe False (const True) registration.clientSecret) <> " }"
instance Aeson.FromJSON ClientRegistration where
    parseJSON = Aeson.withObject "ClientRegistration" $ \o -> ClientRegistration <$> o Aeson..: "client_id" <*> o Aeson..:? "client_secret"

discoverProtectedResource :: Manager -> Text -> IO (Either Text ProtectedResourceMetadata)
discoverProtectedResource manager resourceUrl = getJson manager (metadataRoot resourceUrl <> "/.well-known/oauth-protected-resource")

discoverAuthorizationServer :: Manager -> Text -> IO (Either Text AuthorizationServerMetadata)
discoverAuthorizationServer manager issuerUrl = getJson manager (trimTrailingSlash issuerUrl <> "/.well-known/oauth-authorization-server")

registerClient :: Manager -> Text -> [Text] -> [Text] -> IO (Either Text ClientRegistration)
registerClient manager registrationUrl redirectUris scopes = do
    result <- tryAny $ do
        request <- parseRequest (Text.unpack registrationUrl)
        let payload = Aeson.encode $ Aeson.object ["redirect_uris" Aeson..= redirectUris, "token_endpoint_auth_method" Aeson..= ("none" :: Text), "grant_types" Aeson..= ["authorization_code", "refresh_token" :: Text], "response_types" Aeson..= ["code" :: Text], "scope" Aeson..= Text.unwords scopes]
            request' = request { HC.method = "POST", HC.requestBody = RequestBodyLBS payload, HC.requestHeaders = [("Content-Type", "application/json"), ("Accept", "application/json")] }
        httpLbs request' manager
    case result of
        Left exception -> pure (Left (Text.pack (show exception)))
        Right response | statusCode (responseStatus response) < 200 || statusCode (responseStatus response) >= 300 -> pure (Left "OAuth client registration failed")
                       | otherwise -> decodeBody response

refreshAccessToken :: Manager -> Text -> Text -> Text -> IO OAuthTokenResponse
refreshAccessToken manager endpoint clientId oldRefresh = do
    result <- tryAny $ do
        request <- parseRequest (Text.unpack endpoint)
        let request' = urlEncodedBody [("grant_type", "refresh_token"), ("refresh_token", Encoding.encodeUtf8 oldRefresh), ("client_id", Encoding.encodeUtf8 clientId)] request { HC.method = "POST" }
        httpLbs request' manager
    case result of
        Left e -> pure (OAuthTokenFailure (Text.pack (show e)))
        Right response | statusCode (responseStatus response) < 200 || statusCode (responseStatus response) >= 300 -> pure (OAuthTokenFailure "OAuth token request failed")
                       | otherwise -> pure $ either (OAuthTokenFailure . Text.pack) OAuthTokenSuccess (Aeson.eitherDecode (responseBody response))

getJson :: Aeson.FromJSON value => Manager -> Text -> IO (Either Text value)
getJson manager url = do
    result <- tryAny $ do
        request <- parseRequest (Text.unpack url)
        httpLbs request { HC.requestHeaders = [("Accept", "application/json")] } manager
    case result of
        Left exception -> pure (Left (Text.pack (show exception)))
        Right response | statusCode (responseStatus response) < 200 || statusCode (responseStatus response) >= 300 -> pure (Left "OAuth metadata request failed")
                       | otherwise -> decodeBody response

decodeBody :: Aeson.FromJSON value => HC.Response LBS.ByteString -> IO (Either Text value)
decodeBody response = pure $ either (Left . Text.pack) Right (Aeson.eitherDecode (responseBody response))
metadataRoot :: Text -> Text
metadataRoot url = case Text.breakOn "://" url of
    (scheme, rest) | Text.null rest -> trimTrailingSlash url
                   | otherwise -> scheme <> "://" <> Text.takeWhile (/= '/') (Text.drop 3 rest)
trimTrailingSlash :: Text -> Text
trimTrailingSlash t = Text.dropWhileEnd (== '/') t

-- | UTF-8 success page served by the interactive OAuth callback listener.
oauthCallbackSuccessPage :: LBS.ByteString
oauthCallbackSuccessPage = "<!doctype html><html><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width\"><title>Connected</title><style>body{margin:0;min-height:100vh;display:grid;place-items:center;background:#0b1020;color:#e8ecf5;font:16px -apple-system,BlinkMacSystemFont,Segoe UI,sans-serif}.card{max-width:430px;margin:24px;padding:36px;border:1px solid #28324a;border-radius:20px;background:#131a2d;box-shadow:0 24px 70px #0008;text-align:center}.check{display:grid;place-items:center;width:56px;height:56px;margin:0 auto 20px;border-radius:50%;background:#173d31;color:#6ee7b7;font-size:30px}h1{margin:0 0 10px;font-size:24px}p{margin:0;color:#aab4ca;line-height:1.55}</style></head><body><main class=\"card\"><div class=\"check\">&#10003;</div><h1>MCP connected</h1><p>Authorization completed successfully. You can close this tab and return to Haskell Agent.</p></main></body></html>"

instance Aeson.FromJSON OAuthTokens where
    parseJSON = Aeson.withObject "OAuthTokens" $ \object ->
        OAuthTokens
            <$> object Aeson..: "access_token"
            <*> object Aeson..:? "refresh_token"
            <*> object Aeson..:? "expires_in"
