module Agent.OpenAI.Compaction.Transport
    ( sendNativeCompaction
    , nativeCompactionRequestBody
    ) where

import qualified Agent.Error as Agent
import Agent.Http.Header (parseRetryAfterSeconds)
import qualified Agent.OpenAI.Client as Client
import Agent.OpenAI.Compaction.Envelope (nativeCompactionRequestBody)
import Agent.OpenAI.Error (classifyHttpFailure)
import Agent.OpenAI.Features (remoteCompactionV2Feature)
import Agent.OpenAI.Http (postCodexJson)
import Agent.OpenAI.ModelMetadata (isCodexResponsesLiteModel)
import Control.Applicative ((<|>))
import qualified Control.Exception.Safe as Exception
import Control.Monad (when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Network.Http.Client as Http
import qualified System.IO.Streams as Streams
import System.Timeout (timeout)

-- | Credentials and accounting instrumentation remain caller-owned. All wire
-- normalization, deadlines, response assembly and error classification are
-- shared with the provider library. Async cancellation is never swallowed.
sendNativeCompaction
    :: (Aeson.ToJSON request, Aeson.FromJSON response)
    => Text -> Text -> Text -> request -> IO (Either Agent.ApiError response)
sendNativeCompaction accessToken accountId model request =
    Exception.catchAny performRequest \_ ->
        pure (Left (Agent.ConnectionError "Compaction transport failed"))
  where
    options = Client.remoteCompactionV2RequestOptions
    performRequest = do
        result <- timeout (2 * options.responseIdleTimeoutMicros) $
            postCodexJson Client.defaultCodexBaseUrl "/responses"
                accessToken accountId configureRequest
                (nativeCompactionRequestBody model request) handleResponse
        pure $ case result of
            Nothing -> Left (Agent.ConnectionError "Compaction request timed out")
            Just (Left err) -> Left err
            Just (Right response) -> case Aeson.fromJSON (Aeson.toJSON response) of
                Aeson.Success native -> Right native
                Aeson.Error message -> Left (Agent.JsonDecodeError (Text.pack message) "")
    configureRequest headers = do
        headers
        Http.setHeader "x-codex-beta-features" (Text.encodeUtf8 remoteCompactionV2Feature)
        when (isCodexResponsesLiteModel model) $
            Http.setHeader "x-openai-internal-codex-responses-lite" "true"
    handleResponse response stream
        | status >= 200 && status < 300 =
            Client.readCodexSseChunks options.responseIdleTimeoutMicros
                (Just model) (Streams.read stream) []
        | otherwise = do
            body <- readErrorBody options.responseIdleTimeoutMicros stream
            let err = classifyHttpFailure status body
                retryAfter = parseRetryAfterSeconds $
                    maybeToList (Http.getHeader response "Retry-After")
            pure $ Left case err of
                Agent.ProviderError kind message delay ->
                    Agent.ProviderError kind message (delay <|> retryAfter)
                Agent.HttpError 429 message ->
                    Agent.ProviderError Agent.RateLimitError message retryAfter
                other -> other
      where
        status = Http.getStatusCode response

readErrorBody :: Int -> Streams.InputStream BS.ByteString -> IO Text
readErrorBody idleMicros stream = go 65536 []
  where
    go remaining chunks
        | remaining <= 0 = pure (decode chunks <> "\n[error body truncated]")
        | otherwise = timeout idleMicros (Streams.read stream) >>= \case
            Nothing -> pure (decode chunks <> "\n[error body idle timeout]")
            Just Nothing -> pure (decode chunks)
            Just (Just chunk) ->
                go (remaining - BS.length chunk) (BS.take remaining chunk : chunks)
    decode = Text.decodeUtf8With Text.lenientDecode . BS.concat . reverse
