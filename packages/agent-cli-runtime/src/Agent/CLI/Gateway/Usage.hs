-- | Credential-leased organization usage transport.
module Agent.CLI.Gateway.Usage (fetchGatewayUsageWithCredential) where

import Agent.CLI.Gateway.Credentials
    ( loadGatewayCredential
    , validateGatewayCredential
    , withGatewayCredentialLease
    )
import Agent.CLI.Gateway.Http (gatewayMaxResponseBytes, readBoundedBody)
import Agent.OpenAI.Usage (UsageSnapshot, decodeUsageResponse)
import Agent.Server.Client.GatewayIdentity (GatewayCredential(..))
import Control.Exception.Safe (tryAny)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types
    ( hAccept
    , hAuthorization
    , statusCode
    , statusIsSuccessful
    )
import Network.HTTP.Types.URI (renderQueryText)

fetchGatewayUsageWithCredential
    :: GatewayCredential
    -> Text
    -> IO (Either Text UsageSnapshot)
fetchGatewayUsageWithCredential admitted model =
    withGatewayCredentialLease do
        loadGatewayCredential >>= \case
            Right (Just current)
                | current == admitted ->
                    requestGatewayUsage current model
            _ ->
                pure $
                    Left
                        "The organization gateway changed before usage was refreshed."

requestGatewayUsage
    :: GatewayCredential
    -> Text
    -> IO (Either Text UsageSnapshot)
requestGatewayUsage credential model =
    case validateGatewayCredential credential of
        Left _ -> pure (Left "Gateway credential is invalid.")
        Right () -> do
            let query =
                    TextEncoding.decodeUtf8
                        . LBS.toStrict
                        . Builder.toLazyByteString
                        $ renderQueryText True [("model", Just model)]
                endpoint =
                    Text.dropWhileEnd (== '/')
                        (Text.strip credential.gatewayBaseUrl)
                        <> "/backend-api/wham/usage"
                        <> query
            outcome <- tryAny do
                manager <- newTlsManager
                initial <- HTTP.parseRequest (Text.unpack endpoint)
                let request =
                        initial
                            { HTTP.method = "GET"
                            , HTTP.requestHeaders =
                                [ ( hAuthorization
                                  , "Bearer "
                                        <> TextEncoding.encodeUtf8
                                            credential.gatewayAccessToken
                                  )
                                , (hAccept, "application/json")
                                ]
                            , HTTP.checkResponse = \_ _ -> pure ()
                            -- Never forward the gateway bearer to a redirect.
                            , HTTP.redirectCount = 0
                            , HTTP.responseTimeout =
                                HTTP.responseTimeoutMicro (5 * 1_000_000)
                            }
                HTTP.withResponse request manager \response -> do
                    body <-
                        readBoundedBody
                            gatewayMaxResponseBytes
                            (HTTP.responseBody response)
                    pure (HTTP.responseStatus response, body)
            pure case outcome of
                Left _ ->
                    Left "Could not reach the gateway usage endpoint."
                Right (_status, Nothing) ->
                    Left "Gateway usage returned an oversized response."
                Right (status, Just body)
                    | statusIsSuccessful status ->
                        case decodeUsageResponse (LBS.fromStrict body) of
                            Left _ ->
                                Left
                                    "Gateway returned an unreadable usage response."
                            Right snapshot -> Right snapshot
                    | statusCode status == 404 ->
                        Left
                            "Usage is not supported by this organization gateway."
                    | otherwise ->
                        Left $
                            "Gateway usage returned HTTP "
                                <> Text.pack (show (statusCode status))
