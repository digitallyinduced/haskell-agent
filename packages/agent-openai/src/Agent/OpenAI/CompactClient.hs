-- | Codex remote conversation compaction via POST /responses/compact.
module Agent.OpenAI.CompactClient
    ( CompactRequest(..)
    , compactConversation
    , compactConversationAt
    ) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.OpenAI.Client (defaultCodexBaseUrl)
import Agent.OpenAI.Error (classifyHttpFailure)
import Agent.OpenAI.Http (postCodexJson)
import Agent.Responses.Types hiding (Response)
import qualified Agent.Responses.Types.Request as ResponseRequest
import qualified Agent.Json.Decoder as JsonDecoder
import qualified Agent.Json.Encoder as JsonEncoder
import Agent.Provider
    ( Credential(..)
    , Provider(..)
    , TokenProvider
    , runWithTokenProvider
    )
import Control.Exception.Safe (tryAny)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text (lenientDecode)
import Network.Http.Client (Response, getStatusCode)
import qualified System.IO.Streams as Streams

-- | Body for Codex @/responses/compact@ (subset of CompactionInput).
data CompactRequest = CompactRequest
    { compactModel :: !Text
    , compactInput :: ![ResponseItem]
    , compactInstructions :: !(Maybe Text)
    , compactTools :: !(Maybe [ResponseTool])
    , compactParallelToolCalls :: !Bool
    , compactReasoning :: !(Maybe ReasoningConfig)
    } deriving (Eq, Show)

compactRequestEncoder :: JsonEncoder.Encoder CompactRequest
compactRequestEncoder = JsonEncoder.object
    [ JsonEncoder.field "model" JsonEncoder.text (.compactModel)
    , JsonEncoder.field "input"
        (JsonEncoder.list responseItemEncoder)
        (.compactInput)
    , JsonEncoder.optionalField "instructions"
        JsonEncoder.text
        (.compactInstructions)
    , JsonEncoder.optionalField "tools"
        (JsonEncoder.list responseToolEncoder)
        (.compactTools)
    , JsonEncoder.field "parallel_tool_calls"
        JsonEncoder.bool
        (.compactParallelToolCalls)
    , JsonEncoder.optionalField "reasoning"
        ResponseRequest.reasoningConfigEncoder
        (.compactReasoning)
    ]

compactConversation
    :: TokenProvider
    -> CompactRequest
    -> IO (Either ApiError [ResponseItem])
compactConversation =
    compactConversationAt defaultCodexBaseUrl

compactConversationAt
    :: Text
    -> TokenProvider
    -> CompactRequest
    -> IO (Either ApiError [ResponseItem])
compactConversationAt baseUrl provider request =
    runWithTokenProvider provider \credential ->
        postCompact baseUrl credential request

postCompact
    :: Text
    -> Credential
    -> CompactRequest
    -> IO (Either ApiError [ResponseItem])
postCompact _ credential _
    | credential.provider /= OpenAIProvider =
        pure $ Left $ ProviderError ApiErrorType
            "Codex compaction requires an OpenAI credential"
            Nothing
postCompact baseUrl credential request =
    tryAny perform >>= \case
        Left exception -> pure $ Left $ ConnectionError
            ("Codex compaction request failed: " <> Text.pack (show exception))
        Right result -> pure result
  where
    perform = do
        postCodexJson
            baseUrl
            "/responses/compact"
            credential.accessToken
            credential.accountId
            id
            (JsonEncoder.encode compactRequestEncoder request)
            compactResponseHandler

compactResponseHandler
    :: Response
    -> Streams.InputStream BS.ByteString
    -> IO (Either ApiError [ResponseItem])
compactResponseHandler response stream = do
    let status = getStatusCode response
    bytes <- Streams.fold mappend mempty stream
    let bodyText = Text.decodeUtf8With Text.lenientDecode bytes
    if status >= 200 && status < 300
        then pure (decodeCompactBody bodyText)
        else pure $ Left (classifyHttpFailure status bodyText)

decodeCompactBody :: Text -> Either ApiError [ResponseItem]
decodeCompactBody bodyText =
    case JsonDecoder.decode compactResponseDecoder
            (Text.encodeUtf8 bodyText) of
        Left err -> Left (JsonDecodeError
            (JsonDecoder.renderDecodeError err)
            bodyText)
        Right items -> Right items

compactResponseDecoder :: JsonDecoder.Decoder [ResponseItem]
compactResponseDecoder =
    JsonDecoder.objectFields
        (JsonDecoder.requiredField "output"
            (JsonDecoder.list responseItemDecoder))
