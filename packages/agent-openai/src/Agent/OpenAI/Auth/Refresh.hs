module Agent.OpenAI.Auth.Refresh
    ( RefreshResponse(..)
    , decodeRefreshResponse
    , refreshAccessTokenHTTP
    ) where

import Agent.Error (ApiError(..), ErrorType(..))
import qualified Agent.Json.Decode as Json
import Agent.OpenAI.Auth.JWT (deriveAccountId)
import Agent.OpenAI.Auth.Types (AuthState(..))
import Control.Applicative ((<|>))
import Control.Exception.Safe (tryAny)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text (lenientDecode)
import Data.Time.Clock (getCurrentTime)
import Network.HTTP.Simple
    ( getResponseBody
    , getResponseStatusCode
    , httpLBS
    , parseRequest_
    , setRequestBodyLBS
    , setRequestHeader
    )

tokenEndpoint :: String
tokenEndpoint = "https://auth.openai.com/oauth/token"

-- | Pure HTTP call to OpenAI's @/oauth/token@ endpoint to rotate the access
-- and refresh tokens for a single account. Persistence and cross-process
-- locking remain the caller's responsibility.
refreshAccessTokenHTTP :: Text -> AuthState -> IO (Either ApiError AuthState)
refreshAccessTokenHTTP oauthClientId state = do
    let body = Aeson.encode $ Aeson.object
            [ "grant_type"    Aeson..= ("refresh_token" :: Text)
            , "refresh_token" Aeson..= state.refreshToken
            , "client_id"     Aeson..= oauthClientId
            , "scope"         Aeson..= ("openid profile email offline_access" :: Text)
            ]
        request = setRequestBodyLBS body
            $ setRequestHeader "Content-Type" ["application/json"]
            $ parseRequest_ ("POST " <> tokenEndpoint)
    tryAny (httpLBS request) >>= \case
        Left exception ->
            pure $ Left (ConnectionError (Text.pack (show exception)))
        Right response -> do
            let status = getResponseStatusCode response
            if status < 200 || status >= 300
                then pure $ Left $ ProviderError AuthenticationError
                    ("Codex token refresh failed with HTTP "
                        <> Text.pack (show status) <> ": "
                        <> Text.decodeUtf8With Text.lenientDecode
                            (LBS.toStrict (LBS.take 500 (getResponseBody response))))
                    Nothing
                else case decodeRefreshResponse (getResponseBody response) of
                    Left err -> pure (Left err)
                    Right RefreshResponse{..} -> do
                        let newRefreshToken =
                                fromMaybe state.refreshToken refreshToken
                            newAccountId =
                                fromMaybe state.accountId
                                    (idToken >>= deriveAccountId)
                        now <- getCurrentTime
                        pure $ Right state
                            { accessToken
                            , refreshToken = newRefreshToken
                            , accountId = newAccountId
                            , idToken = idToken <|> state.idToken
                            , lastRefresh = now
                            }

data RefreshResponse = RefreshResponse
    { accessToken :: !Text
    , refreshToken :: !(Maybe Text)
    , idToken :: !(Maybe Text)
    } deriving (Eq, Show)

-- | Parse a token endpoint body. The optional tokens may be absent or an
-- explicit @null@; either keeps the previously stored value.
decodeRefreshResponse :: LBS.ByteString -> Either ApiError RefreshResponse
decodeRefreshResponse body =
    case Json.decodeEither refreshResponseDecoder (LBS.toStrict body) of
        Left err ->
            Left $ ProviderError AuthenticationError
                ("Failed to parse token refresh response: "
                    <> Json.jsonErrorMessage err)
                Nothing
        Right response -> Right response

refreshResponseDecoder :: Json.Decoder RefreshResponse
refreshResponseDecoder = Json.object $
    RefreshResponse
        <$> Json.atKey "access_token" Json.text
        <*> Json.optionalKey "refresh_token" Json.text
        <*> Json.optionalKey "id_token" Json.text
