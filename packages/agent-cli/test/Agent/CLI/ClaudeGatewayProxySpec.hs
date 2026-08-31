module Agent.CLI.ClaudeGatewayProxySpec (spec) where

import Agent.CLI.ClaudeGatewayProxy (withClaudeGatewayProxy)
import Agent.CLI.GatewayClient (GatewayCredential(..))
import Agent.Claude (ClaudeCodeTransport(..))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp qualified as Warp
import Test.Hspec

spec :: Spec
spec = describe "Claude gateway loopback proxy" do
    it "keeps the organization bearer in the parent and forwards only Messages" do
        observed <- newIORef Nothing
        Warp.testWithApplication (pure (upstream observed)) \port -> do
            manager <- HTTP.newManager HTTP.defaultManagerSettings
            let origin = "http://127.0.0.1:" <> Text.pack (show port)
                credential =
                    GatewayCredential
                        { gatewayBaseUrl = origin
                        , gatewayWebSocketUrl =
                            "ws://127.0.0.1:" <> Text.pack (show port)
                        , gatewayAccessToken = "organization-secret"
                        }
            result <-
                withClaudeGatewayProxy credential \case
                    ClaudeCodeLocalSubscription ->
                        expectationFailure "expected gateway transport"
                    ClaudeCodeGateway{gatewayBaseUrl, gatewayToken} -> do
                        gatewayToken `shouldNotBe` "organization-secret"
                        request <-
                            HTTP.parseRequest
                                (Text.unpack gatewayBaseUrl <> "/v1/messages")
                        response <-
                            HTTP.httpLbs
                                request
                                    { HTTP.method = "POST"
                                    , HTTP.requestHeaders =
                                        [ (hAuthorization
                                          , "Bearer "
                                                <> Text.encodeUtf8 gatewayToken)
                                        , (hContentType, "application/json")
                                        ]
                                    , HTTP.requestBody =
                                        HTTP.RequestBodyLBS
                                            "{\"model\":\"sonnet\"}"
                                    }
                                manager
                        response.responseStatus `shouldBe` status200
            result `shouldBe` Right ()
            readIORef observed
                `shouldReturn`
                    Just
                        ( ["anthropic", "v1", "messages"]
                        , Just "Bearer organization-secret"
                        , "{\"model\":\"sonnet\"}"
                        )

upstream
    :: IORef (Maybe ([Text], Maybe ByteString, LBS.ByteString))
    -> Application
upstream observed request respond = do
    body <- strictRequestBody request
    writeIORef observed $
        Just
            (request.pathInfo, lookup hAuthorization request.requestHeaders, body)
    respond $
        responseLBS
            status200
            [(hContentType, "application/json")]
            "{\"ok\":true}"
