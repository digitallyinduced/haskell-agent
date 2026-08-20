-- | xAI-specific normalization of HTTP and streaming failures.
module Agent.XAI.Error
    ( classifyFailure
    , classifyStreamError
    , isFreeLimitBody
    , isCapacityBody
    , capacityRetryAfterSeconds
    ) where

import Agent.Error (ApiError(..), ErrorType(..), errorTypeFromText)
import Agent.OpenAI.Error (classifyHttpFailure, mkOpenAIError)
import Agent.OpenAI.Responses.Types (ResponseStreamError(..))
import Data.Text (Text)
import qualified Data.Text as Text

-- | Default wait when xAI reports model capacity pressure without a
-- structured @resets_in_seconds@ / @Retry-After@ value.
capacityRetryAfterSeconds :: Int
capacityRetryAfterSeconds = 30

-- | Classify a non-success response from the Grok subscription proxy.
classifyFailure :: Int -> Maybe Int -> Text -> ApiError
classifyFailure status retryAfterHeader body
    | status == 426 =
        ProviderError InvalidRequestError
            ("xAI proxy rejected the client version: " <> Text.take 500 body)
            Nothing
    | isFreeLimitBody body =
        ProviderError UsageLimitReached (Text.take 500 body) retryAfterHeader
    | isCapacityBody body =
        ProviderError OverloadedError (Text.take 500 body)
            (retryAfterHeader `orElse` Just capacityRetryAfterSeconds)
    | otherwise = case classifyHttpFailure status body of
        ProviderError errType message retryAfter ->
            ProviderError errType message (retryAfter `orElse` retryAfterHeader)
        HttpError 429 message ->
            ProviderError RateLimitError (Text.take 500 message) retryAfterHeader
        other -> other
  where
    Just value `orElse` _ = Just value
    Nothing `orElse` fallback = fallback

-- | Convert a typed Responses streaming error into the shared error channel.
classifyStreamError :: ResponseStreamError -> ApiError
classifyStreamError streamError
    | isFreeLimitBody streamError.message =
        ProviderError UsageLimitReached streamError.message streamError.retryAfter
    | isCapacityBody streamError.message =
        ProviderError OverloadedError streamError.message
            (streamError.retryAfter `orElse` Just capacityRetryAfterSeconds)
    | Just errorType <- streamError.errorType =
        mkOpenAIError
            (errorTypeFromText errorType)
            streamError.message
            streamError.code
            streamError.retryAfter
    | otherwise = ConnectionError
        ("xAI stream error: " <> streamError.message)
  where
    Just value `orElse` _ = Just value
    Nothing `orElse` fallback = fallback

-- | The subscription proxy sometimes reports quota exhaustion as upsell text
-- instead of a structured error.
isFreeLimitBody :: Text -> Bool
isFreeLimitBody body =
    "grok.com/supergrok" `Text.isInfixOf` lowered
        || "upgrade to a grok subscription" `Text.isInfixOf` lowered
  where
    lowered = Text.toLower body

-- | Detect capacity / priority-processing pressure from unstructured xAI
-- stream and HTTP bodies. These are short-lived overloads, not quota caps.
isCapacityBody :: Text -> Bool
isCapacityBody body =
    "currently at capacity" `Text.isInfixOf` lowered
        || "at capacity due to high demand" `Text.isInfixOf` lowered
        || "use a higher service tier" `Text.isInfixOf` lowered
        || "priority processing" `Text.isInfixOf` lowered
  where
    lowered = Text.toLower body
