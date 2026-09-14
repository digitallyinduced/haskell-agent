module Agent.CLI.ClaudeGatewayProxySpec (spec) where

import Agent.CLI.ClaudeGatewayProxy
    ( gatewayUsageRetryAfterSeconds
    , withClaudeGatewayProxy
    , withClaudeGatewayProxyWith
    )
import Agent.Runtime.GatewayClient (GatewayCredential(..))
import Agent.Claude (ClaudeCodeTransport(..))
import Agent.ClientIdentity (gatewayUserAgent)
import Agent.OpenAI.Usage (UsageLimit(..), UsageSnapshot(..), UsageWindow(..))
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
    it "keeps the organization bearer in the parent process" do
        userAgent <- gatewayUserAgent
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
                                (Text.unpack gatewayBaseUrl
                                    <> "/anthropic/v1/messages")
                        response <-
                            HTTP.httpLbs
                                request
                                    { HTTP.method = "POST"
                                    , HTTP.requestHeaders =
                                        [ ( hAuthorization
                                          , "Bearer "
                                                <> Text.encodeUtf8 gatewayToken
                                          )
                                        , (hContentType, "application/json")
                                        , (hUserAgent, "claude-code/downstream")
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
                        , [userAgent]
                        )

    describe "gateway HTTP 429 responses" do
        it "adds a Retry-After once gateway usage reports the limit reached" do
            usageRequests <- newIORef []
            response <-
                rejectedRequest
                    []
                    (\model -> do
                        modifyIORef' usageRequests (model :)
                        pure (Right (usageSnapshot False True 100 419738)))
            response.responseStatus `shouldBe` status429
            lookup hRetryAfter response.responseHeaders
                `shouldBe` Just "419738"
            response.responseBody `shouldBe` rejectedBody
            readIORef usageRequests `shouldReturn` ["claude-opus-5"]

        it "keeps a gateway 429 retryable while usage still allows requests" do
            response <-
                rejectedRequest
                    []
                    (\_ -> pure (Right (usageSnapshot True False 5 12938)))
            response.responseStatus `shouldBe` status429
            lookup hRetryAfter response.responseHeaders `shouldBe` Nothing

        it "keeps a gateway 429 retryable when usage cannot be read" do
            response <-
                rejectedRequest
                    []
                    (\_ -> pure (Left "Could not reach the gateway usage endpoint."))
            response.responseStatus `shouldBe` status429
            lookup hRetryAfter response.responseHeaders `shouldBe` Nothing

        it "preserves an upstream Retry-After without consulting usage" do
            usageRequests <- newIORef (0 :: Int)
            response <-
                rejectedRequest
                    [(hRetryAfter, "30")]
                    (\_ -> do
                        modifyIORef' usageRequests (+ 1)
                        pure (Right (usageSnapshot False True 100 419738)))
            lookup hRetryAfter response.responseHeaders `shouldBe` Just "30"
            readIORef usageRequests `shouldReturn` 0

    describe "gatewayUsageRetryAfterSeconds" do
        it "reports the exhausted window's reset" do
            gatewayUsageRetryAfterSeconds (usageSnapshot False True 100 419738)
                `shouldBe` Just 419738

        it "stays above Claude Code's one-minute retry threshold" do
            gatewayUsageRetryAfterSeconds (usageSnapshot False True 100 10)
                `shouldBe` Just 61

        it "falls back to the gateway's five-minute cooldown" do
            gatewayUsageRetryAfterSeconds (usageSnapshot False True 80 12938)
                `shouldBe` Just 300

        it "does nothing while requests are still admitted" do
            gatewayUsageRetryAfterSeconds (usageSnapshot True False 99 12938)
                `shouldBe` Nothing
            gatewayUsageRetryAfterSeconds
                UsageSnapshot
                    { planType = "anthropic"
                    , rateLimit = Nothing
                    , additionalRateLimits = []
                    }
                `shouldBe` Nothing

-- | Send one Claude request through the proxy to an upstream that answers
-- HTTP 429 with the given extra headers and the gateway's error body.
rejectedRequest
    :: ResponseHeaders
    -> (Text -> IO (Either Text UsageSnapshot))
    -> IO (HTTP.Response LBS.ByteString)
rejectedRequest extraHeaders fetchUsage =
    Warp.testWithApplication (pure rejectingUpstream) \port -> do
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
            withClaudeGatewayProxyWith fetchUsage credential \case
                ClaudeCodeLocalSubscription ->
                    fail "expected gateway transport"
                ClaudeCodeGateway{gatewayBaseUrl, gatewayToken} -> do
                    request <-
                        HTTP.parseRequest
                            (Text.unpack gatewayBaseUrl
                                <> "/anthropic/v1/messages")
                    HTTP.httpLbs
                        request
                            { HTTP.method = "POST"
                            , HTTP.requestHeaders =
                                [ ( hAuthorization
                                  , "Bearer " <> Text.encodeUtf8 gatewayToken
                                  )
                                , (hContentType, "application/json")
                                ]
                            , HTTP.requestBody =
                                HTTP.RequestBodyLBS
                                    "{\"model\":\"claude-opus-5\",\"messages\":[]}"
                            }
                        manager
        either (fail . Text.unpack) pure result
  where
    rejectingUpstream _ respond =
        respond $
            responseLBS
                status429
                ((hContentType, "application/json") : extraHeaders)
                rejectedBody

hRetryAfter :: HeaderName
hRetryAfter = "Retry-After"

rejectedBody :: LBS.ByteString
rejectedBody =
    "{\"type\":\"error\",\"error\":{\"type\":\"rate_limit_error\",\
    \\"message\":\"No granted Anthropic subscription account is currently available.\"}}"

usageSnapshot :: Bool -> Bool -> Int -> Int -> UsageSnapshot
usageSnapshot allowed limitReached secondaryUsed secondaryReset =
    UsageSnapshot
        { planType = "anthropic"
        , rateLimit =
            Just
                UsageLimit
                    { allowed
                    , limitReached
                    , primaryWindow =
                        Just
                            UsageWindow
                                { usedPercent = 5
                                , limitWindowSeconds = 18000
                                , resetAfterSeconds = 12938
                                , resetAt = 1789383600
                                }
                    , secondaryWindow =
                        Just
                            UsageWindow
                                { usedPercent = secondaryUsed
                                , limitWindowSeconds = 604800
                                , resetAfterSeconds = secondaryReset
                                , resetAt = 1789790400
                                }
                    }
        , additionalRateLimits = []
        }

upstream
    :: IORef (Maybe ([Text], Maybe ByteString, LBS.ByteString, [ByteString]))
    -> Application
upstream observed request respond = do
    body <- strictRequestBody request
    writeIORef observed $
        Just
            ( request.pathInfo
            , lookup hAuthorization request.requestHeaders
            , body
            , [value | (name, value) <- request.requestHeaders, name == hUserAgent]
            )
    respond $
        responseLBS
            status200
            [(hContentType, "application/json")]
            "{\"ok\":true}"
