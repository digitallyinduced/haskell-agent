module Agent.CLI.ClaudeGatewayProxy (withClaudeGatewayProxy) where

import Agent.CLI.GatewayClient (GatewayCredential(..), validateGatewayCredential)
import Agent.Claude (ClaudeCodeTransport(..))
import Control.Concurrent.Async (withAsync)
import Control.Exception.Safe (bracket, tryAny)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, byteString)
import Data.Char (intToDigit)
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

withClaudeGatewayProxy
    :: GatewayCredential
    -> (ClaudeCodeTransport -> IO value)
    -> IO (Either Text value)
withClaudeGatewayProxy rawCredential callback =
    case validateGatewayCredential rawCredential of
        Left _ -> pure (Left proxyError)
        Right credential -> do
            manager <- newTlsManager
            capability <- randomCapability
            bracket Warp.openFreePort (Socket.close . snd) \(port, socket) ->
                withAsync
                    (Warp.runSettingsSocket
                        (Warp.setHost "127.0.0.1" Warp.defaultSettings)
                        socket
                        (proxyApplication manager credential capability))
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
    -> Application
proxyApplication manager credential capability request respond
    | requestMethod request /= "POST"
        || pathInfo request /= ["v1", "messages"] =
        respondJson status404
    | lookup hAuthorization request.requestHeaders
        /= Just ("Bearer " <> Text.encodeUtf8 capability) =
        respondJson status401
    | otherwise =
        readBoundedRequestBody (32 * 1024 * 1024) request >>= \case
            Nothing -> respondJson status413
            Just body ->
                tryAny (forward manager credential request body respond) >>= \case
                    Left _ -> respondJson status502
                    Right received -> pure received
  where
    respondJson status =
        respond (responseLBS status [(hContentType, "application/json")] "{}")

forward
    :: HTTP.Manager
    -> GatewayCredential
    -> Request
    -> ByteString
    -> (Response -> IO ResponseReceived)
    -> IO ResponseReceived
forward manager credential downstream body respond = do
    initial <-
        HTTP.parseRequest $
            Text.unpack credential.gatewayBaseUrl <> "/anthropic/v1/messages"
    HTTP.withResponse
        initial
            { HTTP.method = "POST"
            , HTTP.requestHeaders =
                (hAuthorization, "Bearer " <> Text.encodeUtf8 credential.gatewayAccessToken)
                    : filter
                        (\(name, _) ->
                            name `elem`
                                [ hAccept, hContentType, "anthropic-version"
                                , "anthropic-beta", hUserAgent
                                ])
                        downstream.requestHeaders
            , HTTP.requestBody = HTTP.RequestBodyBS body
            , HTTP.redirectCount = 0
            , HTTP.checkResponse = \_ _ -> pure ()
            , HTTP.responseTimeout =
                HTTP.responseTimeoutMicro (10 * 60 * 1_000_000)
            }
        manager
        \upstream ->
            respond $
                responseStream
                    upstream.responseStatus
                    (filter
                        (\(name, _) ->
                            name `notElem`
                                [hConnection, hContentLength, hTransferEncoding])
                        upstream.responseHeaders)
                    \send flush -> streamBody upstream.responseBody send flush

streamBody :: HTTP.BodyReader -> (Builder -> IO ()) -> IO () -> IO ()
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
