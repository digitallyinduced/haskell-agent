-- | HTTP client for the xAI Grok subscription proxy Responses endpoint.
module Agent.XAI.Client
    ( StreamEventCallback
    , createResponse
    , createResponseWith
    , createResponseWithEvents
    ) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.OpenAI.Responses.Types
import Agent.Provider (Credential(..), Provider(..))
import Agent.XAI.Error (classifyFailure)
import Agent.XAI.Options
import Agent.XAI.Request (buildRequest)
import Agent.XAI.Stream (buildResponse, parseSseEvents)
import Control.Exception.Safe (tryAny)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text (lenientDecode)
import qualified Network.HTTP.Client as HttpClient
import Network.HTTP.Simple hiding (Response)

type StreamEventCallback = ResponseStreamEvent -> IO ()

-- | Send one request using environment-derived client options.
createResponse :: Credential -> ResponseCreateParams -> IO (Either ApiError Response)
createResponse credential request = do
    options <- clientOptionsFromEnv
    createResponseWith options credential request

-- | Send one request using explicit client options.
createResponseWith
    :: ClientOptions
    -> Credential
    -> ResponseCreateParams
    -> IO (Either ApiError Response)
createResponseWith options credential request =
    createResponseWithEvents options credential request (const (pure ()))

-- | Send one request and deliver every decoded typed Responses event before
-- returning the assembled terminal response. The current HTTP backend buffers
-- the response body, so callbacks run in wire order after the body completes.
createResponseWithEvents
    :: ClientOptions
    -> Credential
    -> ResponseCreateParams
    -> StreamEventCallback
    -> IO (Either ApiError Response)
createResponseWithEvents options credential request onEvent
    | credential.provider /= XAIProvider = pure $ Left $ ProviderError ApiErrorType
        "agent-xai requires an xAI credential"
        Nothing
    | otherwise = tryAny performRequest >>= \case
        Left exception -> pure $ Left $ ConnectionError
            ("xAI request failed: " <> Text.pack (show exception))
        Right response -> handleResponse response
  where
    performRequest = do
        httpRequest <- parseRequest ("POST " <> trimSlash options.baseUrl <> "/responses")
        httpLBS
            $ setRequestBodyLBS (Aeson.encode (buildRequest options request))
            $ setRequestHeader "Authorization"
                ["Bearer " <> Text.encodeUtf8 credential.accessToken]
            $ setRequestHeader "X-XAI-Token-Auth" ["xai-grok-cli"]
            $ setRequestHeader "x-grok-client-version" [Text.encodeUtf8 options.clientVersion]
            $ setRequestHeader "x-grok-client-identifier" ["grok-shell"]
            $ setRequestHeader "x-grok-client-mode" ["interactive"]
            $ setRequestHeader "Content-Type" ["application/json"]
            $ setRequestHeader "Accept" ["text/event-stream"]
            $ setRequestHeader "User-Agent" ["codex-hs"]
            $ withTimeout httpRequest

    withTimeout httpRequest = httpRequest
        { HttpClient.responseTimeout =
            HttpClient.responseTimeoutMicro (options.requestTimeoutSeconds * 1_000_000)
        }

    handleResponse response = do
        let status = getResponseStatusCode response
            bodyText = Text.decodeUtf8With Text.lenientDecode
                (LBS.toStrict (getResponseBody response))
        if status >= 200 && status < 300
            then case parseSseEvents bodyText of
                Left err -> pure (Left err)
                Right events -> do
                    mapM_ onEvent events
                    pure (buildResponse events)
            else pure $ Left $ classifyFailure status (retryAfterSeconds response) bodyText

    retryAfterSeconds response = case getResponseHeader "Retry-After" response of
        (value : _) -> case reads (BS8.unpack value) of
            [(seconds, "")] -> Just (max 1 seconds)
            _ -> Nothing
        [] -> Nothing

trimSlash :: String -> String
trimSlash = reverse . dropWhile (== '/') . reverse
