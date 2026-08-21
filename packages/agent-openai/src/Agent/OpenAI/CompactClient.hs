-- | Codex remote conversation compaction via POST /responses/compact.
module Agent.OpenAI.CompactClient
    ( CompactRequest(..)
    , compactConversation
    , compactConversationAt
    ) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.OpenAI.Client (defaultCodexBaseUrl)
import Agent.OpenAI.Error (classifyHttpFailure)
import Agent.OpenAI.Responses.Types hiding (Response)
import Agent.Provider
    ( Credential(..)
    , Provider(..)
    , TokenProvider
    , runWithTokenProvider
    )
import Control.Exception.Safe (tryAny)
import Control.Monad (when)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text (lenientDecode)
import Network.Http.Client
    ( Method(POST)
    , Response
    , buildRequest1
    , establishConnection
    , getStatusCode
    , http
    , jsonBody
    , receiveResponse
    , sendRequest
    , setContentType
    , setHeader
    , withConnection
    )
import OpenSSL (withOpenSSL)
import qualified System.IO.Streams as Streams
import qualified Network.URI as URI

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
        let url = Text.dropWhileEnd (== '/') baseUrl <> "/responses/compact"
            body = Aeson.toJSON request
        case URI.parseURI (Text.unpack url) of
            Nothing -> pure $ Left (JsonDecodeError ("Invalid URL: " <> url) "")
            Just uri ->
                withOpenSSL $
                    withConnection (establishConnection (Text.encodeUtf8 url)) $ \conn -> do
                        let path = Text.encodeUtf8 (Text.pack uri.uriPath)
                            req = buildRequest1 $ do
                                http POST path
                                setContentType "application/json"
                                setHeader "Authorization"
                                    ("Bearer " <> Text.encodeUtf8 credential.accessToken)
                                when (not (Text.null (Text.strip credential.accountId))) $
                                    setHeader "chatgpt-account-id"
                                        (Text.encodeUtf8 credential.accountId)
                        sendRequest conn req (jsonBody body)
                        receiveResponse conn compactResponseHandler

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
    case Aeson.eitherDecodeStrict (Text.encodeUtf8 bodyText) of
        Left err -> Left (JsonDecodeError (Text.pack err) bodyText)
        Right (Aeson.Object object) ->
            case KeyMap.lookup "output" object of
                Just value ->
                    case Aeson.fromJSON value of
                        Aeson.Success items -> Right items
                        Aeson.Error err ->
                            Left (JsonDecodeError (Text.pack err) bodyText)
                Nothing ->
                    Left (JsonDecodeError "compact response missing output" bodyText)
        Right _ ->
            Left (JsonDecodeError "compact response was not an object" bodyText)
