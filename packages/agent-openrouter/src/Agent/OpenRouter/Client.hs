-- | HTTP client for the OpenRouter Responses endpoint.
module Agent.OpenRouter.Client
    ( StreamEventCallback
    , createResponse
    , createResponseWith
    , createResponseWithEvents
    , createResponseWithEventsPolicy
    , retryTransientOpenRouterResultWithPolicy
    ) where

import Agent.Error
    ( ApiError(..)
    , ErrorType(..)
    , isInlineRetryableProviderError
    )
import Agent.Responses.Client
    ( ResponsesClientConfig(..)
    , performResponsesRequest
    , retryStreamingResultWithPolicy
    )
import qualified Agent.Responses.HttpSSE as HttpSSE
import Agent.Responses.Types
import Agent.Provider (Credential(..), Provider(..))
import Agent.OpenRouter.Error (classifyFailure)
import Agent.OpenRouter.Options
import Agent.OpenRouter.Request (buildRequest)
import Agent.OpenRouter.Stream (streamAssemblyConfig)
import Control.Retry
    ( RetryPolicyM
    , exponentialBackoff
    , limitRetries
    )
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.HTTP.Simple hiding (Response)
import Network.HTTP.Types (HeaderName)

type StreamEventCallback = HttpSSE.StreamEventCallback

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

-- | Send one request and deliver decoded typed Responses events incrementally
-- in wire order before returning the assembled terminal response.
createResponseWithEvents
    :: ClientOptions
    -> Credential
    -> ResponseCreateParams
    -> StreamEventCallback
    -> IO (Either ApiError Response)
createResponseWithEvents =
    createResponseWithEventsPolicy transientResultPolicy

-- | Like 'createResponseWithEvents', with an injectable retry policy for
-- deterministic tests. Transient failures retry only before the first stream
-- callback, so callers never observe replayed output.
createResponseWithEventsPolicy
    :: RetryPolicyM IO
    -> ClientOptions
    -> Credential
    -> ResponseCreateParams
    -> StreamEventCallback
    -> IO (Either ApiError Response)
createResponseWithEventsPolicy policy options credential request onEvent
    | credential.provider /= OpenRouterProvider = pure $ Left $ ProviderError ApiErrorType
        "agent-openrouter requires an OpenRouter credential"
        Nothing
    | otherwise =
        retryTransientOpenRouterResultWithPolicy policy performOnce onEvent
  where
    performOnce =
        performResponsesRequest
            ResponsesClientConfig
                { clientExceptionPrefix = "OpenRouter request failed"
                , clientBaseUrl = options.baseUrl
                , clientTimeoutSeconds = options.requestTimeoutSeconds
                , clientClassifyFailure = classifyFailure
                , clientAssemblyConfig = streamAssemblyConfig
                }
            (buildRequest options request)
            configureRequest

    configureRequest =
        setRequestHeader "Authorization"
            ["Bearer " <> Text.encodeUtf8 credential.accessToken]
            . setRequestHeader "User-Agent" ["haskell-agent"]
            . optionalHeader "HTTP-Referer" options.httpReferer
            . optionalHeader "X-Title" options.appTitle

-- | Retry transient failures while replay is safe. The callback marker is
-- written before user code runs, so callback exceptions are never retried.
retryTransientOpenRouterResultWithPolicy
    :: RetryPolicyM IO
    -> ((event -> IO ()) -> IO (Either ApiError value))
    -> (event -> IO ())
    -> IO (Either ApiError value)
retryTransientOpenRouterResultWithPolicy policy request onEvent =
    retryStreamingResultWithPolicy
        policy
        isInlineRetryableProviderError
        request
        (Just onEvent)

transientResultPolicy :: RetryPolicyM IO
transientResultPolicy = exponentialBackoff 1_000_000 <> limitRetries 3

optionalHeader :: HeaderName -> Maybe Text -> Request -> Request
optionalHeader name value request = case nonEmptyText value of
    Just text -> setRequestHeader name [Text.encodeUtf8 text] request
    Nothing -> request

nonEmptyText :: Maybe Text -> Maybe Text
nonEmptyText (Just value) | not (Text.null (Text.strip value)) = Just value
nonEmptyText _ = Nothing

