module Agent.CLI.ClaudeGatewayProxy
    ( withClaudeGatewayProxy
    , withClaudeGatewayProxyWith
    , gatewayUsageRetryAfterSeconds
    ) where

import Agent.Runtime.GatewayClient
    ( GatewayCredential(..)
    , fetchGatewayUsageWithCredential
    , validateGatewayCredential
    )
import Agent.Claude (ClaudeCodeTransport(..))
import Agent.ClientIdentity (gatewayUserAgent)
import Agent.OpenAI.Usage (UsageLimit(..), UsageSnapshot(..), UsageWindow(..))
import Control.Concurrent.Async (withAsync)
import Control.Exception.Safe (bracket, tryAny)
import Control.Monad (unless)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, byteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Char (intToDigit)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types
import Network.Socket qualified as Socket
import Network.Wai
import Network.Wai.Handler.Warp qualified as Warp
import System.IO (IOMode(ReadMode), hSetBinaryMode, withFile)

-- | Keep the organization bearer in the parent process. Claude receives only
-- a random, session-scoped capability accepted by this loopback-only proxy.
withClaudeGatewayProxy
    :: GatewayCredential
    -> (ClaudeCodeTransport -> IO value)
    -> IO (Either Text value)
withClaudeGatewayProxy credential =
    withClaudeGatewayProxyWith
        (fetchGatewayUsageWithCredential credential)
        credential

-- | Injectable gateway usage lookup. The proxy consults it only after the
-- gateway rejected a request with HTTP 429, so a slow or failing lookup never
-- delays a successful turn.
withClaudeGatewayProxyWith
    :: (Text -> IO (Either Text UsageSnapshot))
    -> GatewayCredential
    -> (ClaudeCodeTransport -> IO value)
    -> IO (Either Text value)
withClaudeGatewayProxyWith fetchUsage rawCredential callback =
    case validateGatewayCredential rawCredential of
        Left _ -> pure (Left proxyError)
        Right () -> do
            manager <- newTlsManager
            capability <- randomCapability
            bracket Warp.openFreePort (Socket.close . snd) \(port, socket) ->
                withAsync
                    (Warp.runSettingsSocket
                        (Warp.setHost "127.0.0.1" Warp.defaultSettings)
                        socket
                        (proxyApplication
                            manager
                            rawCredential
                            capability
                            fetchUsage))
                    \_ ->
                        Right <$> callback ClaudeCodeGateway
                            { gatewayBaseUrl =
                                "http://127.0.0.1:" <> Text.pack (show port)
                            , gatewayToken = capability
                            }

proxyError :: Text
proxyError = "The Claude gateway transport is unavailable."

randomCapability :: IO Text
randomCapability =
    withFile "/dev/urandom" ReadMode \handle -> do
        hSetBinaryMode handle True
        bytes <- BS.hGet handle 32
        if BS.length bytes /= 32
            then fail "unable to create Claude gateway capability"
            else pure (Text.pack (concatMap hexByte (BS.unpack bytes)))
  where
    hexByte byte =
        [ intToDigit (fromIntegral byte `div` 16)
        , intToDigit (fromIntegral byte `mod` 16)
        ]

proxyApplication
    :: HTTP.Manager
    -> GatewayCredential
    -> Text
    -> (Text -> IO (Either Text UsageSnapshot))
    -> Application
proxyApplication manager credential capability fetchUsage request respond
    | requestMethod request /= "POST"
        || pathInfo request /= ["anthropic", "v1", "messages"] =
        respondJson status404
    | lookup hAuthorization request.requestHeaders
        /= Just ("Bearer " <> Text.encodeUtf8 capability) =
        respondJson status401
    | otherwise =
        readBoundedRequestBody (32 * 1024 * 1024) request >>= \case
            Nothing -> respondJson status413
            Just body ->
                tryAny
                    (forward manager credential fetchUsage request body respond)
                    >>= \case
                        Left _ -> respondJson status502
                        Right received -> pure received
  where
    respondJson status =
        respond
            (responseLBS status [(hContentType, "application/json")] "{}")

forward
    :: HTTP.Manager
    -> GatewayCredential
    -> (Text -> IO (Either Text UsageSnapshot))
    -> Request
    -> ByteString
    -> (Response -> IO ResponseReceived)
    -> IO ResponseReceived
forward manager credential fetchUsage downstream body respond = do
    userAgent <- gatewayUserAgent
    initial <-
        HTTP.parseRequest $
            Text.unpack credential.gatewayBaseUrl <> "/anthropic/v1/messages"
    HTTP.withResponse
        initial
            { HTTP.method = "POST"
            , HTTP.requestHeaders =
                [ (hUserAgent, userAgent)
                , ( hAuthorization
                  , "Bearer " <> Text.encodeUtf8 credential.gatewayAccessToken
                  )
                ]
                    <> filter
                        (\(name, _) ->
                            name `elem`
                                [ hAccept
                                , hContentType
                                , "anthropic-version"
                                , "anthropic-beta"
                                ])
                        downstream.requestHeaders
            , HTTP.requestBody = HTTP.RequestBodyBS body
            , HTTP.redirectCount = 0
            , HTTP.checkResponse = \_ _ -> pure ()
            , HTTP.responseTimeout =
                HTTP.responseTimeoutMicro (10 * 60 * 1_000_000)
            }
        manager
        \upstream -> do
            retryAfter <-
                if statusCode upstream.responseStatus == 429
                    && isNothing (lookup hRetryAfter upstream.responseHeaders)
                    then exhaustionRetryAfter fetchUsage body
                    else pure Nothing
            respond $
                responseStream
                    upstream.responseStatus
                    (filter
                        (\(name, _) ->
                            name
                                `notElem`
                                    [ hConnection
                                    , hContentLength
                                    , "Transfer-Encoding"
                                    ])
                        upstream.responseHeaders
                        <> [ (hRetryAfter, BS8.pack (show seconds))
                           | Just seconds <- [retryAfter]
                           ])
                    \send flush ->
                        streamBody upstream.responseBody send flush

-- | Claude Code retries every HTTP 429 with exponential backoff: ten attempts
-- and more than three minutes of silence unless the response carries a
-- Retry-After above one minute. The gateway answers 429 without that header
-- when no granted account can serve the request. When its own usage snapshot
-- confirms the limit is reached, no retry can succeed before the reported
-- reset, so pass that horizon on and let the turn fail at once with the
-- gateway's message. A transient 429 keeps Claude Code's short retries.
exhaustionRetryAfter
    :: (Text -> IO (Either Text UsageSnapshot))
    -> ByteString
    -> IO (Maybe Int)
exhaustionRetryAfter fetchUsage body =
    case requestedModel body of
        Nothing -> pure Nothing
        Just model ->
            tryAny (fetchUsage model) >>= \case
                Right (Right snapshot) ->
                    pure (gatewayUsageRetryAfterSeconds snapshot)
                _ -> pure Nothing

hRetryAfter :: HeaderName
hRetryAfter = "Retry-After"

requestedModel :: ByteString -> Maybe Text
requestedModel body =
    case Aeson.decodeStrict' body of
        Just (Aeson.Object object)
            | Just (Aeson.String model) <- KeyMap.lookup "model" object
            , not (Text.null (Text.strip model)) ->
                Just (Text.strip model)
        _ -> Nothing

-- | Seconds until the gateway's exhausted usage window resets, or 'Nothing'
-- while the gateway still admits requests. Always above the one-minute
-- threshold at which Claude Code stops retrying.
gatewayUsageRetryAfterSeconds :: UsageSnapshot -> Maybe Int
gatewayUsageRetryAfterSeconds snapshot = do
    limits <- snapshot.rateLimit
    if limits.allowed && not limits.limitReached
        then Nothing
        else
            Just $
                max minimumRetryAfter case exhaustedResets limits of
                    [] -> fallbackCooldown
                    resets -> maximum resets
  where
    exhaustedResets limits =
        [ window.resetAfterSeconds
        | Just window <- [limits.primaryWindow, limits.secondaryWindow]
        , window.usedPercent >= 100
        ]
    -- The gateway pauses a limited account for five minutes when no window
    -- reports its reset.
    fallbackCooldown = 300
    minimumRetryAfter = 61

streamBody
    :: HTTP.BodyReader
    -> (Builder -> IO ())
    -> IO ()
    -> IO ()
streamBody reader send flush = go
  where
    go =
        HTTP.brRead reader >>= \chunk ->
            unless (BS.null chunk) (send (byteString chunk) >> flush >> go)

readBoundedRequestBody :: Int -> Request -> IO (Maybe ByteString)
readBoundedRequestBody limit request = go 0 []
  where
    go size chunks =
        getRequestBodyChunk request >>= \chunk ->
            if BS.null chunk
                then pure (Just (BS.concat (reverse chunks)))
                else
                    let next = size + BS.length chunk
                    in if next > limit
                        then drain >> pure Nothing
                        else go next (chunk : chunks)
    drain =
        getRequestBodyChunk request >>= \chunk ->
            unless (BS.null chunk) drain
