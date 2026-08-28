{-# LANGUAGE OverloadedStrings #-}
module Agent.MCP.OAuth
    ( OAuthTokens(..), OAuthTokenResponse(..), refreshAccessToken ) where

import Control.Exception.Safe (tryAny)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Network.HTTP.Client (Manager, RequestBody(..), httpLbs, parseRequest, responseBody, responseStatus)
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types (statusCode)
import Network.HTTP.Types.URI (urlEncodeAsForm)

data OAuthTokens = OAuthTokens
    { accessToken :: !Text
    , refreshToken :: !(Maybe Text)
    , expiresIn :: !(Maybe Int)
    } deriving (Eq)

instance Show OAuthTokens where
    show tokens = "OAuthTokens { accessToken = <redacted>, refreshToken = " <> show (maybe False (const True) tokens.refreshToken) <> " }"

data OAuthTokenResponse = OAuthTokenSuccess !OAuthTokens | OAuthTokenFailure !Text
    deriving (Eq, Show)

refreshAccessToken :: Manager -> Text -> Text -> Text -> IO OAuthTokenResponse
refreshAccessToken manager endpoint clientId oldRefresh = do
    result <- tryAny $ do
        request <- parseRequest (Text.unpack endpoint)
        let body = urlEncodeAsForm
                [("grant_type", "refresh_token"), ("refresh_token", Encoding.encodeUtf8 oldRefresh), ("client_id", Encoding.encodeUtf8 clientId)]
            request' = request { HC.method = "POST", HC.requestBody = RequestBodyLBS body, HC.requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")] }
        httpLbs request' manager
    case result of
        Left e -> pure (OAuthTokenFailure (Text.pack (show e)))
        Right response
            | statusCode (responseStatus response) < 200 || statusCode (responseStatus response) >= 300 -> pure (OAuthTokenFailure "OAuth token request failed")
            | otherwise -> pure $ either (OAuthTokenFailure . Text.pack) OAuthTokenSuccess (Aeson.eitherDecode (responseBody response))

instance Aeson.FromJSON OAuthTokens where
    parseJSON = Aeson.withObject "OAuthTokens" $ \o -> OAuthTokens <$> o Aeson..: "access_token" <*> o Aeson..:? "refresh_token" <*> o Aeson..:? "expires_in"
