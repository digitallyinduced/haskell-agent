module Agent.OpenAI.Client
    ( CodexRequestOptions(..)
    , defaultCodexRequestOptions
    , remoteCompactionV2RequestOptions
    , createCodexMessage
    , createCodexMessageWithProvider
    , createCodexMessageWithProviderWithTurnState
    , createCodexMessageWithProviderWithOptions
    , createCodexMessageWithProviderWithOptionsAndTurnState
    , createCodexMessageWithProviderAt
    , createCodexMessageWithProviderAtWithTurnState
    , createCodexMessageWithProviderAtWithOptions
    , createCodexMessageWithProviderAtWithOptionsAndTurnState
    , readCodexSseChunks
    , defaultCodexBaseUrl
    , retryTransientCodexResultWithPolicy
    ) where

import Agent.OpenAI.Auth (Pool)
import Agent.OpenAI.Credential (poolTokenProvider)
import Agent.Error
import Agent.Http.Header (parseRetryAfterSeconds)
import Agent.OpenAI.Error (classifyHttpFailure)
import Agent.OpenAI.Features
    ( betaFeaturesHeaderValue
    , remoteCompactionV2Feature
    )
import Agent.OpenAI.Http
    ( decodeCodexHttpBodyBytesWithModel
    , finishCodexStreamResponse
    , postCodexJson
    , stepCodexStreamResponse
    )
import Agent.OpenAI.ModelMetadata (isCodexResponsesLiteModel)
import Agent.OpenAI.Request (sanitizeCodexRequest)
import Agent.OpenAI.WebSocketClient
    ( CodexTurnState
    , finishCodexTurnStateResponse
    , readCodexTurnState
    , recordCodexTurnState
    )
import Agent.Provider
    ( Credential(..)
    , Provider(..)
    , TokenProvider
    , runWithTokenProvider
    )
import Agent.Retry (handleSyncExceptions)
import Agent.Responses.SSE
    ( feedSseDecoder
    , finishSseDecoder
    , newSseDecoder
    )
import Agent.Responses.StreamAssembly
    ( StreamAssemblyState
    , emptyStreamAssemblyState
    )
import qualified Agent.Responses.Types as OpenAI
import Control.Monad (forM_, when)
import Control.Retry
    ( RetryPolicyM
    , exponentialBackoff
    , limitRetries
    , retrying
    )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text (lenientDecode)
import qualified System.IO.Streams as Streams
import Network.Http.Client (Response, getStatusCode)
import qualified Network.Http.Client
import qualified System.Timeout

-- | ChatGPT Codex REST prefix. 'createCodexMessage' and
-- 'createCodexMessageWithProvider' POST to @{defaultCodexBaseUrl}/responses@.
-- Point 'createCodexMessageWithProviderAt' at another OpenAI-compatible
-- Responses host (for example an llm-router @{origin}/v1@) to reuse the same
-- request/SSE client without the ChatGPT broker.
defaultCodexBaseUrl :: Text
defaultCodexBaseUrl = "https://chatgpt.com/backend-api/codex"

-- | Per-request Codex transport controls that are not part of the public
-- Responses JSON schema.
data CodexRequestOptions = CodexRequestOptions
    { betaFeatures :: ![Text]
    , responseIdleTimeoutMicros :: !Int
    , preserveTurnStateAfterResponse :: !Bool
    } deriving (Eq, Show)

defaultCodexRequestOptions :: CodexRequestOptions
defaultCodexRequestOptions = CodexRequestOptions
    { betaFeatures = [remoteCompactionV2Feature]
    , responseIdleTimeoutMicros = 300 * 1_000_000
    , preserveTurnStateAfterResponse = False
    }

-- | Normal Responses transport options for the @compaction_trigger@ protocol.
remoteCompactionV2RequestOptions :: CodexRequestOptions
remoteCompactionV2RequestOptions = defaultCodexRequestOptions
    { preserveTurnStateAfterResponse = True
    }

-- | Send a request to the Codex Responses API and parse the response.
-- Serialises 'OpenAI.ResponseCreateParams' via its 'ToJSON' instance, POSTs to
-- @/responses@, extracts the final @response.completed@ SSE event, and
-- decodes as 'OpenAI.Response'. Retries up to 3 times on transient errors.
--
-- On a 401/403, force-refreshes the rejected account immediately and retries
-- once with its rotated token before failing over. This covers tokens that
-- ChatGPT invalidates before their JWT expiry. On rate-limit errors (HTTP 429, @rate_limit_error@, or
-- @usage_limit_reached@), marks the offending account as cooling down and
-- retries with a different account from the pool. For
-- @usage_limit_reached@ the cooldown honours the server's
-- @resets_in_seconds@ exactly, so an exhausted 5h window is skipped for its
-- full duration rather than the short default.
createCodexMessage :: Pool -> OpenAI.ResponseCreateParams -> IO (Either ApiError OpenAI.Response)
createCodexMessage pool request = do
    provider <- poolTokenProvider pool
    createCodexMessageWithProvider provider request

-- | Provider-based REST client. Account selection, cooldowns, credential
-- refresh, and broker feedback live behind 'TokenProvider'; the transport only
-- reports structured account failures and retries the request with the
-- returned credential. XAI credentials belong to the @agent-xai@ transport
-- package and are rejected at this OpenAI-specific boundary.
createCodexMessageWithProvider
    :: TokenProvider
    -> OpenAI.ResponseCreateParams
    -> IO (Either ApiError OpenAI.Response)
createCodexMessageWithProvider =
    createCodexMessageWithProviderWithOptions defaultCodexRequestOptions

-- | Send a normal model request through HTTP while sharing the logical turn's
-- sticky-routing token with WebSocket transports and tool continuations.
createCodexMessageWithProviderWithTurnState
    :: CodexTurnState
    -> TokenProvider
    -> OpenAI.ResponseCreateParams
    -> IO (Either ApiError OpenAI.Response)
createCodexMessageWithProviderWithTurnState =
    createCodexMessageWithProviderWithOptionsAndTurnState
        defaultCodexRequestOptions

-- | Provider-based REST client with Codex-specific transport options.
createCodexMessageWithProviderWithOptions
    :: CodexRequestOptions
    -> TokenProvider
    -> OpenAI.ResponseCreateParams
    -> IO (Either ApiError OpenAI.Response)
createCodexMessageWithProviderWithOptions options =
    createCodexMessageWithProviderAtWithOptions
        options
        defaultCodexBaseUrl

-- | Like 'createCodexMessageWithProviderWithOptions', but capture and replay
-- the shared per-turn sticky-routing token. Remote compaction options preserve
-- the token for the following inference request.
createCodexMessageWithProviderWithOptionsAndTurnState
    :: CodexRequestOptions
    -> CodexTurnState
    -> TokenProvider
    -> OpenAI.ResponseCreateParams
    -> IO (Either ApiError OpenAI.Response)
createCodexMessageWithProviderWithOptionsAndTurnState options turnState =
    createCodexMessageWithProviderAtWithOptionsAndTurnState
        options
        defaultCodexBaseUrl
        turnState

-- | Like 'createCodexMessageWithProvider', but POST to
-- @{baseUrl}/responses@ instead of the ChatGPT Codex backend.
--
-- @baseUrl@ is the Responses API prefix with no trailing slash, e.g.
-- @https://llm-router.example/v1@. Empty @accountId@ credentials omit the
-- ChatGPT-only @chatgpt-account-id@ header so a static bearer can talk to a
-- compatible proxy without a broker-issued account.
createCodexMessageWithProviderAt
    :: Text
    -> TokenProvider
    -> OpenAI.ResponseCreateParams
    -> IO (Either ApiError OpenAI.Response)
createCodexMessageWithProviderAt =
    createCodexMessageWithProviderAtWithOptions defaultCodexRequestOptions

-- | Custom-base-URL variant of
-- 'createCodexMessageWithProviderWithTurnState'.
createCodexMessageWithProviderAtWithTurnState
    :: Text
    -> CodexTurnState
    -> TokenProvider
    -> OpenAI.ResponseCreateParams
    -> IO (Either ApiError OpenAI.Response)
createCodexMessageWithProviderAtWithTurnState baseUrl turnState =
    createCodexMessageWithProviderAtWithOptionsInternal
        defaultCodexRequestOptions
        (Just turnState)
        baseUrl

-- | Like 'createCodexMessageWithProviderAt', with additional Codex transport
-- headers such as @x-codex-beta-features@.
createCodexMessageWithProviderAtWithOptions
    :: CodexRequestOptions
    -> Text
    -> TokenProvider
    -> OpenAI.ResponseCreateParams
    -> IO (Either ApiError OpenAI.Response)
createCodexMessageWithProviderAtWithOptions options =
    createCodexMessageWithProviderAtWithOptionsInternal options Nothing

-- | Custom-base-URL variant with both transport options and a shared logical
-- turn state. This is primarily useful for compatible proxies and tests.
createCodexMessageWithProviderAtWithOptionsAndTurnState
    :: CodexRequestOptions
    -> Text
    -> CodexTurnState
    -> TokenProvider
    -> OpenAI.ResponseCreateParams
    -> IO (Either ApiError OpenAI.Response)
createCodexMessageWithProviderAtWithOptionsAndTurnState options baseUrl turnState =
    createCodexMessageWithProviderAtWithOptionsInternal
        options
        (Just turnState)
        baseUrl

createCodexMessageWithProviderAtWithOptionsInternal
    :: CodexRequestOptions
    -> Maybe CodexTurnState
    -> Text
    -> TokenProvider
    -> OpenAI.ResponseCreateParams
    -> IO (Either ApiError OpenAI.Response)
createCodexMessageWithProviderAtWithOptionsInternal options turnState
        baseUrl provider request = do
    result <- runWithTokenProvider provider \credential ->
        case credential.provider of
            XAIProvider -> pure $ Left $ ProviderError ApiErrorType
                "XAI credentials must be used through agent-xai"
                Nothing
            OpenRouterProvider -> pure $ Left $ ProviderError ApiErrorType
                "OpenRouter credentials must be used through agent-openrouter"
                Nothing
            ClaudeCodeProvider -> pure $ Left $ ProviderError ApiErrorType
                "Claude Code subscription sessions must use agent-claude"
                Nothing
            OpenAIProvider ->
                retryTransientCodexResultWithPolicy transientResultPolicy $
                    handleSyncExceptions
                        (ConnectionError
                            . ("Codex request failed: " <>)
                            . Text.pack
                            . show) $
                        makeCodexRequest
                            options
                            baseUrl
                            credential.accessToken
                            credential.accountId
                            turnState
                            request
    case (turnState, result) of
        (Just state, Right response)
            | not options.preserveTurnStateAfterResponse ->
                finishCodexTurnStateResponse state response
        _ -> pure ()
    pure result
  where
    transientResultPolicy = exponentialBackoff 5_000_000 <> limitRetries 3

-- | Retry short-lived provider and transport failures. Quota and rate-limit
-- errors remain excluded by 'isInlineRetryableProviderError'.
retryTransientCodexResultWithPolicy
    :: RetryPolicyM IO
    -> IO (Either ApiError value)
    -> IO (Either ApiError value)
retryTransientCodexResultWithPolicy policy request =
    retrying policy shouldRetry (const request)
  where
    shouldRetry _retryStatus = \case
        Left apiError | isInlineRetryableProviderError apiError -> pure True
        _ -> pure False

makeCodexRequest
    :: CodexRequestOptions
    -> Text
    -> Text
    -> Text
    -> Maybe CodexTurnState
    -> OpenAI.ResponseCreateParams
    -> IO (Either ApiError OpenAI.Response)
makeCodexRequest options baseUrl accessToken accountId turnState request = do
    -- The ChatGPT Codex HTTP endpoint only serves Responses requests as SSE
    -- and rejects an omitted/false stream flag. WebSocket callers do not need
    -- this field, so enforce it at the HTTP transport boundary.
    turnStateValue <- maybe (pure Nothing) readCodexTurnState turnState
    let requestBody = Aeson.toJSON
            (sanitizeCodexRequest request) { OpenAI.stream = Just True }
        addBetaFeaturesHeader req = do
            req
            forM_ (betaFeaturesHeaderValue options.betaFeatures) \features ->
                Network.Http.Client.setHeader
                    "x-codex-beta-features"
                    (Text.encodeUtf8 features)
            when (maybe False isCodexResponsesLiteModel request.model) $
                Network.Http.Client.setHeader
                    "x-openai-internal-codex-responses-lite"
                    "true"
            forM_ turnStateValue \value ->
                Network.Http.Client.setHeader
                    "x-codex-turn-state"
                    (Text.encodeUtf8 value)
    postCodexJson
        baseUrl
        "/responses"
        accessToken
        accountId
        addBetaFeaturesHeader
        requestBody
        (responseHandler
            options.responseIdleTimeoutMicros
            request.model
            turnState)

responseHandler
    :: Int
    -> Maybe Text
    -> Maybe CodexTurnState
    -> Response
    -> Streams.InputStream BS.ByteString
    -> IO (Either ApiError OpenAI.Response)
responseHandler idleTimeoutMicros modelHint turnState response stream = do
    let status = getStatusCode response
    captureResponseTurnState turnState response
    if status >= 200 && status < 300
        then
            if responseIsEventStream response
                then readCodexSseResponse
                    idleTimeoutMicros modelHint stream []
                else sniffSuccessfulResponse
                    idleTimeoutMicros modelHint stream
        else do
            bodyResult <- readBoundedBodyWithIdleTimeout
                idleTimeoutMicros maxErrorBodyBytes stream [] 0
            let bodyBytes = LBS.toStrict (bodyReadBytes bodyResult)
                bodyText = Text.decodeUtf8With Text.lenientDecode bodyBytes
                classified = case bodyResult of
                    BodyReadComplete _ ->
                        classifyHttpFailure status bodyText
                    BodyReadTimedOut _ ->
                        classifyTimedOutHttpFailure status bodyText
                    BodyReadTooLarge _ ->
                        appendBodyTruncatedMessage
                            maxErrorBodyBytes
                            (classifyHttpFailure status bodyText)
            pure $ Left (withRetryAfterHeader response classified)

captureResponseTurnState :: Maybe CodexTurnState -> Response -> IO ()
captureResponseTurnState Nothing _ = pure ()
captureResponseTurnState (Just turnState) response =
    case Network.Http.Client.getHeader response "x-codex-turn-state"
            >>= either (const Nothing) Just . Text.decodeUtf8' of
        Nothing -> pure ()
        Just value -> recordCodexTurnState turnState value

classifyTimedOutHttpFailure :: Int -> Text -> ApiError
classifyTimedOutHttpFailure status bodyText =
    case classifyHttpFailure status bodyText of
        HttpError 429 message ->
            ProviderError RateLimitError
                (if Text.null (Text.strip message)
                    then "Codex HTTP 429 response body idle timeout"
                    else message)
                Nothing
        HttpError statusCode message ->
            HttpError statusCode
                (if Text.null (Text.strip message)
                    then "Codex HTTP error response body idle timeout"
                    else message)
        apiError -> apiError

withRetryAfterHeader :: Response -> ApiError -> ApiError
withRetryAfterHeader response apiError =
    case parseRetryAfterHeader
            (Network.Http.Client.getHeader response "Retry-After") of
        Nothing -> apiError
        retryValue -> case apiError of
            ProviderError{ retryAfter = Nothing } ->
                apiError { retryAfter = retryValue }
            HttpError 429 message ->
                ProviderError RateLimitError message retryValue
            _ -> apiError

parseRetryAfterHeader :: Maybe BS.ByteString -> Maybe Int
parseRetryAfterHeader Nothing = Nothing
parseRetryAfterHeader (Just value) = parseRetryAfterSeconds [value]

data BodyReadResult
    = BodyReadComplete !LBS.ByteString
    | BodyReadTimedOut !LBS.ByteString
    | BodyReadTooLarge !LBS.ByteString

bodyReadBytes :: BodyReadResult -> LBS.ByteString
bodyReadBytes = \case
    BodyReadComplete bytes -> bytes
    BodyReadTimedOut bytes -> bytes
    BodyReadTooLarge bytes -> bytes

readBoundedBodyWithIdleTimeout
    :: Int
    -> Int
    -> Streams.InputStream BS.ByteString
    -> [BS.ByteString]
    -> Int
    -> IO BodyReadResult
readBoundedBodyWithIdleTimeout idleMicros byteLimit stream =
    go
  where
    go reversedChunks bytesRead = do
        next <- System.Timeout.timeout idleMicros (Streams.read stream)
        case next of
            Nothing ->
                pure (BodyReadTimedOut
                    (LBS.fromChunks (reverse reversedChunks)))
            Just Nothing ->
                pure (BodyReadComplete
                    (LBS.fromChunks (reverse reversedChunks)))
            Just (Just chunk) -> do
                let remaining = byteLimit - bytesRead
                if BS.length chunk > remaining
                    then pure $ BodyReadTooLarge $ LBS.fromChunks $
                        reverse (BS.take remaining chunk : reversedChunks)
                    else go (chunk : reversedChunks)
                        (bytesRead + BS.length chunk)

sniffSuccessfulResponse
    :: Int
    -> Maybe Text
    -> Streams.InputStream BS.ByteString
    -> IO (Either ApiError OpenAI.Response)
sniffSuccessfulResponse idleMicros modelHint stream =
    sniff [] 0
  where
    sniff reversedChunks bytesRead = do
        next <- System.Timeout.timeout idleMicros (Streams.read stream)
        case next of
            Nothing -> pure $ Left $ ConnectionError
                "Codex HTTP response idle timeout"
            Just Nothing ->
                pure $ decodeCodexHttpBodyBytesWithModel modelHint $
                    LBS.toStrict (LBS.fromChunks (reverse reversedChunks))
            Just (Just chunk) -> do
                let chunks' = chunk : reversedChunks
                    bytesRead' = bytesRead + BS.length chunk
                    sniffBytes = LBS.toStrict $ LBS.take
                        (fromIntegral maxResponseSniffBytes)
                        (LBS.fromChunks (reverse chunks'))
                if looksLikeSsePrefix sniffBytes
                    then readCodexSseResponse
                        idleMicros modelHint stream (reverse chunks')
                    else if looksLikeJsonPrefix sniffBytes
                        || bytesRead' >= maxResponseSniffBytes
                        then readSuccessfulJsonResponse
                            idleMicros modelHint stream chunks' bytesRead'
                        else sniff chunks' bytesRead'

readSuccessfulJsonResponse
    :: Int
    -> Maybe Text
    -> Streams.InputStream BS.ByteString
    -> [BS.ByteString]
    -> Int
    -> IO (Either ApiError OpenAI.Response)
readSuccessfulJsonResponse idleMicros modelHint stream reversedChunks bytesRead
    | bytesRead > maxSuccessfulJsonBytes =
        pure $ Left $ ProviderError PayloadTooLargeError
            successfulJsonTooLargeMessage
            Nothing
    | otherwise = do
        bodyResult <- readBoundedBodyWithIdleTimeout
            idleMicros
            maxSuccessfulJsonBytes
            stream
            reversedChunks
            bytesRead
        pure $ case bodyResult of
            BodyReadComplete bytes ->
                decodeCodexHttpBodyBytesWithModel modelHint (LBS.toStrict bytes)
            BodyReadTimedOut _ ->
                Left (ConnectionError "Codex HTTP response idle timeout")
            BodyReadTooLarge _ ->
                Left $ ProviderError PayloadTooLargeError
                    successfulJsonTooLargeMessage
                    Nothing

readCodexSseResponse
    :: Int
    -> Maybe Text
    -> Streams.InputStream BS.ByteString
    -> [BS.ByteString]
    -> IO (Either ApiError OpenAI.Response)
readCodexSseResponse idleMicros modelHint stream initialChunks =
    readCodexSseChunks
        idleMicros modelHint (Streams.read stream) initialChunks

-- | Incrementally decode a Codex SSE body from an arbitrary chunk source.
-- The idle timeout applies independently to every source read, and decoding
-- stops immediately at a terminal event without retaining earlier wire data.
readCodexSseChunks
    :: Int
    -> Maybe Text
    -> IO (Maybe BS.ByteString)
    -> [BS.ByteString]
    -> IO (Either ApiError OpenAI.Response)
readCodexSseChunks idleMicros modelHint readChunk initialChunks =
    case feedInitial newSseDecoder emptyStreamAssemblyState initialChunks of
        Left err -> pure (Left err)
        Right (_, _, Just response) -> pure (Right response)
        Right (decoder, state, Nothing) -> go decoder state
  where
    feedInitial decoder state [] = Right (decoder, state, Nothing)
    feedInitial decoder state (chunk : rest) = do
        (nextDecoder, events) <- feedSseDecoder decoder chunk
        (nextState, completed) <- applyEvents modelHint state events
        case completed of
            Just response -> Right (nextDecoder, nextState, Just response)
            Nothing -> feedInitial nextDecoder nextState rest

    go decoder state = do
        next <- System.Timeout.timeout idleMicros readChunk
        case next of
            Nothing -> pure $ Left $ ConnectionError
                "Codex HTTP response idle timeout"
            Just Nothing -> pure $ do
                trailing <- finishSseDecoder decoder
                (nextState, completed) <- applyEvents modelHint state trailing
                maybe (finishCodexStreamResponse nextState) Right completed
            Just (Just chunk) ->
                case feedSseDecoder decoder chunk of
                    Left err -> pure (Left err)
                    Right (nextDecoder, events) ->
                        case applyEvents modelHint state events of
                            Left err -> pure (Left err)
                            Right (_, Just response) -> pure (Right response)
                            Right (nextState, Nothing) ->
                                go nextDecoder nextState

applyEvents
    :: Maybe Text
    -> StreamAssemblyState
    -> [OpenAI.ResponseStreamEvent]
    -> Either ApiError (StreamAssemblyState, Maybe OpenAI.Response)
applyEvents _ state [] = Right (state, Nothing)
applyEvents modelHint state (event : rest) = do
    (next, completed) <- stepCodexStreamResponse modelHint state event
    case completed of
        Just response -> Right (next, Just response)
        Nothing -> applyEvents modelHint next rest

responseIsEventStream :: Response -> Bool
responseIsEventStream response =
    maybe False isEventStreamContentType
        (Network.Http.Client.getHeader response "Content-Type")

isEventStreamContentType :: BS.ByteString -> Bool
isEventStreamContentType =
    (== "text/event-stream")
        . BS8.dropWhileEnd (`elem` [' ', '\t'])
        . BS8.map toLower
        . BS8.takeWhile (/= ';')
        . BS8.dropWhile (`elem` [' ', '\t'])

looksLikeSsePrefix :: BS.ByteString -> Bool
looksLikeSsePrefix bytes =
    any isSseLine (BS8.lines (dropAsciiSpace bytes))
  where
    isSseLine line =
        "event:" `BS.isPrefixOf` line
            || "data:" `BS.isPrefixOf` line
            || ":" `BS.isPrefixOf` line

looksLikeJsonPrefix :: BS.ByteString -> Bool
looksLikeJsonPrefix bytes =
    case BS.uncons (dropAsciiSpace bytes) of
        Just (byte, _) -> byte == 0x7b || byte == 0x5b
        Nothing -> False

dropAsciiSpace :: BS.ByteString -> BS.ByteString
dropAsciiSpace = BS.dropWhile (`elem` [0x20, 0x09, 0x0a, 0x0d])

appendBodyTruncatedMessage :: Int -> ApiError -> ApiError
appendBodyTruncatedMessage limit apiError =
    let suffix = "\n[response body truncated after "
            <> Text.pack (show limit) <> " bytes]"
    in case apiError of
        HttpError status message -> HttpError status (message <> suffix)
        ProviderError errorType message retryAfter ->
            ProviderError errorType (message <> suffix) retryAfter
        JsonDecodeError message body ->
            JsonDecodeError (message <> suffix) body
        ConnectionError message -> ConnectionError (message <> suffix)
        other -> other

successfulJsonTooLargeMessage :: Text
successfulJsonTooLargeMessage =
    "Codex successful JSON response exceeds "
        <> Text.pack (show maxSuccessfulJsonBytes)
        <> " bytes"

maxResponseSniffBytes :: Int
maxResponseSniffBytes = 8 * 1024

maxSuccessfulJsonBytes :: Int
maxSuccessfulJsonBytes = 64 * 1024 * 1024

maxErrorBodyBytes :: Int
maxErrorBodyBytes = 1024 * 1024
