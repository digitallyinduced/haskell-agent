-- | Generic stateless HTTP client for streaming OpenAI Responses-compatible
-- endpoints.
module Agent.Responses.GenericClient
    ( GenericClientOptions(..)
    , buildRequest
    , createResponseWith
    , createResponseWithEvents
    , createResponseWithEventsPolicy
    , retryTransientResultWithPolicy
    , classifyFailure
    , classifyStreamError
    , buildResponse
    ) where

import Agent.Error
    ( ApiError(..)
    , ErrorType(..)
    , errorTypeFromText
    , isInlineRetryableProviderError
    )
import Agent.Responses.Client
    ( ResponsesClientConfig(..)
    , performResponsesRequest
    , retryStreamingResultWithPolicy
    )
import Agent.Responses.Error
    ( classifyHttpFailure
    , mkOpenAIError
    )
import qualified Agent.Responses.HttpSSE as HttpSSE
import Agent.Responses.Request
    ( forceStatelessStreaming
    , setResponseModel
    )
import Agent.Responses.StreamAssembly
    ( StreamAssemblyConfig(..)
    , buildStreamResponse
    , failedStreamResponseMessage
    )
import Agent.Responses.Types
import Control.Retry
    ( RetryPolicyM
    , exponentialBackoff
    , limitRetries
    )
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.HTTP.Simple hiding (Response)

data GenericClientOptions = GenericClientOptions
    { baseUrl :: !String
      -- ^ API prefix including any version segment, but not @/responses@.
    , model :: !Text
      -- ^ Exact model identifier sent to the endpoint.
    , bearerToken :: !(Maybe Text)
      -- ^ Optional bearer token. 'Nothing' omits the Authorization header.
    , requestTimeoutSeconds :: !Int
      -- ^ Full streaming-response timeout.
    } deriving (Eq, Show)

-- | Project canonical request parameters onto a stateless Responses endpoint.
--
-- Stateless endpoints receive the complete local transcript on every request,
-- so remote storage and continuation identifiers must stay disabled.
buildRequest :: GenericClientOptions -> ResponseCreateParams -> ResponseCreateParams
buildRequest options request =
    setResponseModel options.model (forceStatelessStreaming request)

createResponseWith
    :: GenericClientOptions
    -> ResponseCreateParams
    -> IO (Either ApiError Response)
createResponseWith options request =
    createResponseWithEvents options request (const (pure ()))

createResponseWithEvents
    :: GenericClientOptions
    -> ResponseCreateParams
    -> HttpSSE.StreamEventCallback
    -> IO (Either ApiError Response)
createResponseWithEvents =
    createResponseWithEventsPolicy transientResultPolicy

-- | Send a request using an injectable retry policy. Transient failures retry
-- only before the first callback, preventing duplicate streamed output.
createResponseWithEventsPolicy
    :: RetryPolicyM IO
    -> GenericClientOptions
    -> ResponseCreateParams
    -> HttpSSE.StreamEventCallback
    -> IO (Either ApiError Response)
createResponseWithEventsPolicy policy options request onEvent =
    retryTransientResultWithPolicy policy performOnce onEvent
  where
    performOnce =
        performResponsesRequest
            ResponsesClientConfig
                { clientExceptionPrefix = "Responses request failed"
                , clientBaseUrl = options.baseUrl
                , clientTimeoutSeconds = options.requestTimeoutSeconds
                , clientClassifyFailure = classifyFailure
                , clientBuildResponse = buildResponse
                }
            (buildRequest options request)
            configureRequest

    configureRequest =
        maybe
            id
            (\token ->
                setRequestHeader
                    "Authorization"
                    ["Bearer " <> Text.encodeUtf8 token])
            (nonEmptyText options.bearerToken)
            . setRequestHeader "User-Agent" ["haskell-agent"]

retryTransientResultWithPolicy
    :: RetryPolicyM IO
    -> ((event -> IO ()) -> IO (Either ApiError value))
    -> (event -> IO ())
    -> IO (Either ApiError value)
retryTransientResultWithPolicy policy request onEvent =
    retryStreamingResultWithPolicy
        policy
        isInlineRetryableProviderError
        request
        (Just onEvent)

-- | Decode standard OpenAI-shaped error bodies while preserving Retry-After
-- from the HTTP response when the body omitted it.
classifyFailure :: Int -> Maybe Int -> Text -> ApiError
classifyFailure status retryAfterHeader body =
    addRetryAfter retryAfterHeader (classifyHttpFailure status body)

classifyStreamError :: ResponseStreamError -> ApiError
classifyStreamError streamError =
    mkOpenAIError
        (maybe ApiErrorType errorTypeFromText streamError.errorType)
        streamError.message
        streamError.code
        streamError.retryAfter

buildResponse :: [ResponseStreamEvent] -> Either ApiError Response
buildResponse = buildStreamResponse StreamAssemblyConfig
    { missingCompletionMessage =
        "No terminal response event found in Responses SSE stream"
    , classifyStreamError
    , classifyFailedResponse =
        ConnectionError . failedStreamResponseMessage
    }

transientResultPolicy :: RetryPolicyM IO
transientResultPolicy = exponentialBackoff 1_000_000 <> limitRetries 3

nonEmptyText :: Maybe Text -> Maybe Text
nonEmptyText (Just value)
    | not (Text.null (Text.strip value)) = Just value
nonEmptyText _ = Nothing

addRetryAfter :: Maybe Int -> ApiError -> ApiError
addRetryAfter fallback = \case
    ProviderError errorType message retryAfter ->
        ProviderError errorType message (retryAfter `orElse` fallback)
    errorValue -> errorValue

Just value `orElse` _ = Just value
Nothing `orElse` fallback = fallback
