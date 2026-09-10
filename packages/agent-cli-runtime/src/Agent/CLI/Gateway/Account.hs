module Agent.CLI.Gateway.Account (GatewayAccount(..), fetchGatewayAccount) where

import Agent.CLI.Gateway.Credentials (loadGatewayCredential, validateGatewayCredential, withGatewayCredentialLease)
import Agent.CLI.Gateway.Http (readBoundedBody)
import Agent.ClientIdentity (gatewayUserAgent)
import Agent.Server.Client.GatewayIdentity (GatewayCredential(..))
import Control.Exception.Safe (tryAny)
import Data.Aeson (FromJSON(..), eitherDecodeStrict', withObject, (.:), (.:?))
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (statusCode)

data GatewayAccount = GatewayAccount
    { organizationId :: !Text
    , organizationName :: !Text
    , userName :: !Text
    , iconPng :: !BS.ByteString
    } deriving (Eq, Show)

instance FromJSON GatewayAccount where
    parseJSON = withObject "GatewayAccount" \value -> do
        organization <- value .: "organization"
        user <- value .: "user"
        organizationId <- organization .: "id"
        organizationName <- organization .: "name"
        userName <- user .: "name"
        icon <- organization .:? "iconDataUrl"
        let iconPng = case icon >>= Text.stripPrefix "data:image/png;base64," of
                Just encoded | Text.length encoded <= 350000 ->
                    either (const BS.empty) id (Base64.decode (Text.encodeUtf8 encoded))
                _ -> BS.empty
        pure GatewayAccount{..}

-- Keep the profile tied to the credential lease, never expose its token.
fetchGatewayAccount :: IO (Either Text (Maybe GatewayAccount))
fetchGatewayAccount = withGatewayCredentialLease do
    loadGatewayCredential >>= \case
        Left message -> pure (Left message)
        Right Nothing -> pure (Right Nothing)
        Right (Just credential) -> case validateGatewayCredential credential of
            Left _ -> pure (Left "Gateway credential is invalid.")
            Right () -> do
                outcome <- tryAny do
                    manager <- newTlsManager
                    userAgent <- gatewayUserAgent
                    initial <- HTTP.parseRequest (Text.unpack (Text.dropWhileEnd (== '/') credential.gatewayBaseUrl <> "/api/v1/account"))
                    let request = initial
                            { HTTP.requestHeaders = [("Authorization", "Bearer " <> Text.encodeUtf8 credential.gatewayAccessToken), ("Accept", "application/json"), ("User-Agent", userAgent)]
                            , HTTP.redirectCount = 0
                            , HTTP.checkResponse = \_ _ -> pure ()
                            , HTTP.responseTimeout = HTTP.responseTimeoutMicro 5000000
                            }
                    HTTP.withResponse request manager \response -> do
                        body <- readBoundedBody 400000 (HTTP.responseBody response)
                        pure (statusCode (HTTP.responseStatus response), body)
                pure case outcome of
                    Right (200, Just body) -> case eitherDecodeStrict' body of
                        Right account -> Right (Just account)
                        Left _ -> Left "The gateway account response could not be read."
                    Right (404, _) -> Right Nothing
                    _ -> Left "The gateway account could not be loaded."
