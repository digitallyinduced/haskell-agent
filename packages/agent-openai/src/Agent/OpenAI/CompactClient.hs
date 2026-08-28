-- | Codex remote conversation compaction via POST /responses/compact.
module Agent.OpenAI.CompactClient
    ( CompactRequest(..)
    , compactConversation
    , compactConversationAt
    , decodeCompactBodyBytes
    ) where

import Agent.Error (ApiError(..), ErrorType(..))
import qualified Agent.Json.Decode as Json
import Agent.OpenAI.Client (defaultCodexBaseUrl)
import Agent.OpenAI.Error (classifyHttpFailure)
import Agent.OpenAI.Http (postCodexJson)
import Agent.Responses.Types hiding (Response)
import Agent.Responses.Types.Items (responseItemDecoder)
import Agent.Provider
    ( Credential(..)
    , Provider(..)
    , TokenProvider
    , runWithTokenProvider
    )
import Control.Exception.Safe (tryAny)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Maybe (catMaybes)
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

instance Aeson.ToJSON CompactRequest where
    toJSON req =
        Aeson.object $ catMaybes
            [ Just ("model" .= req.compactModel)
            , Just ("input" .= req.compactInput)
            , ("instructions" .=) <$> req.compactInstructions
            , ("tools" .=) <$> req.compactTools
            , Just ("parallel_tool_calls" .= req.compactParallelToolCalls)
            , ("reasoning" .=) <$> req.compactReasoning
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
            (Aeson.toJSON request)
            compactResponseHandler

compactResponseHandler
    :: Response
    -> Streams.InputStream BS.ByteString
    -> IO (Either ApiError [ResponseItem])
compactResponseHandler response stream = do
    let status = getStatusCode response
    bytes <- Streams.fold mappend mempty stream
    if status >= 200 && status < 300
        then pure (decodeCompactBodyBytes bytes)
        else pure $ Left (classifyHttpFailure status (bodyText bytes))

decodeCompactBodyBytes :: BS.ByteString -> Either ApiError [ResponseItem]
decodeCompactBodyBytes bytes =
    case Json.decodeEither compactOutputDecoder bytes of
        Left err -> Left
            (JsonDecodeError
                (Json.jsonErrorMessage err)
                (bodyText bytes))
        Right output -> Right output

compactOutputDecoder :: Json.Decoder [ResponseItem]
compactOutputDecoder =
    Json.object $
        Json.atKey "output" (Json.list responseItemDecoder)

bodyText :: BS.ByteString -> Text
bodyText = Text.decodeUtf8With Text.lenientDecode
