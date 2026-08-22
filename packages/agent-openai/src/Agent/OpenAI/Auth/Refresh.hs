module Agent.OpenAI.Auth.Refresh (refreshAccessTokenHTTP) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.OpenAI.Auth.JWT (deriveAccountId)
import Agent.OpenAI.Auth.Types (AuthState(..))
import Control.Applicative ((<|>))
import Control.Exception.Safe (tryAny)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
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
                else case Aeson.eitherDecode (getResponseBody response) of
                    Left err ->
                        pure $ Left $ ProviderError AuthenticationError
                            ("Failed to parse token refresh response: " <> Text.pack err)
                            Nothing
                    Right (object :: Aeson.Value) ->
                        case jsonTextMaybe object "access_token" of
                            Nothing ->
                                pure $ Left $ ProviderError AuthenticationError
                                    "Token refresh response missing access_token"
                                    Nothing
                            Just newAccessToken -> do
                                let newRefreshToken =
                                        fromMaybe state.refreshToken
                                            (jsonTextMaybe object "refresh_token")
                                    newIdToken = jsonTextMaybe object "id_token"
                                    newAccountId =
                                        fromMaybe state.accountId
                                            (newIdToken >>= deriveAccountId)
                                now <- getCurrentTime
                                pure $ Right state
                                    { accessToken = newAccessToken
                                    , refreshToken = newRefreshToken
                                    , accountId = newAccountId
                                    , idToken = newIdToken <|> state.idToken
                                    , lastRefresh = now
                                    }

jsonTextMaybe :: Aeson.Value -> Text -> Maybe Text
jsonTextMaybe (Aeson.Object object) key =
    case KeyMap.lookup (Key.fromText key) object of
        Just (Aeson.String text) -> Just text
        _ -> Nothing
jsonTextMaybe _ _ = Nothing
