{-# LANGUAGE OverloadedStrings #-}
module Agent.MCP.OAuth
    ( OAuthTokens(..), OAuthTokenResponse(..), ProtectedResourceMetadata(..)
    , AuthorizationServerMetadata(..), ClientRegistration(..)
    , discoverProtectedResource, discoverAuthorizationServer, registerClient
    , refreshAccessToken
    ) where

import Control.Exception.Safe (tryAny)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Network.HTTP.Client (Manager, RequestBody(..), httpLbs, parseRequest, responseBody, responseStatus)
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types (statusCode)
import Network.HTTP.Types.URI (urlEncodeAsForm)

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
        let body = urlEncodeAsForm [("grant_type", "refresh_token"), ("refresh_token", Encoding.encodeUtf8 oldRefresh), ("client_id", Encoding.encodeUtf8 clientId)]
            request' = request { HC.method = "POST", HC.requestBody = RequestBodyLBS body, HC.requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")] }
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
