-- | xAI-specific normalization of HTTP and streaming failures.
module Agent.XAI.Error
    ( classifyFailure
    , classifyStreamError
    , isFreeLimitBody
    ) where

import Agent.Error (ApiError(..), ErrorType(..), errorTypeFromText)
import Agent.OpenAI.Error (classifyHttpFailure, mkOpenAIError)
import Agent.OpenAI.Responses.Types (ResponseStreamError(..))
import Data.Text (Text)
import qualified Data.Text as Text

-- | Classify a non-success response from the Grok subscription proxy.
classifyFailure :: Int -> Maybe Int -> Text -> ApiError
classifyFailure status retryAfterHeader body
    | status == 426 =
        ProviderError InvalidRequestError
            ("xAI proxy rejected the client version: " <> Text.take 500 body)
            Nothing
    | isFreeLimitBody body =
        ProviderError UsageLimitReached (Text.take 500 body) retryAfterHeader
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
    | Just errorType <- streamError.errorType =
        mkOpenAIError
            (errorTypeFromText errorType)
            streamError.message
            streamError.code
            streamError.retryAfter
    | otherwise = ConnectionError
        ("xAI stream error: " <> streamError.message)

-- | The subscription proxy sometimes reports quota exhaustion as upsell text
-- instead of a structured error.
isFreeLimitBody :: Text -> Bool
isFreeLimitBody body =
    "grok.com/supergrok" `Text.isInfixOf` lowered
        || "upgrade to a grok subscription" `Text.isInfixOf` lowered
  where
    lowered = Text.toLower body
