-- | The private, line-delimited JSON protocol spoken by the code-mode worker.
--
-- Keeping this protocol separate from the process host makes its trust
-- boundary explicit: every message received from the worker is decoded and
-- validated before it can cause a tool effect.
module Agent.Tools.CodeMode.Protocol
    ( CodeModeToolMetadata(..)
    , ProtocolMessage(..)
    , ToolInvocation(..)
    , decodeProtocolMessage
    , encodeExecRequest
    , encodeExecRequestWithState
    , encodeExecRequestWithStateAndImageDetail
    , encodeToolFailure
    , encodeToolSuccess
    ) where

import Data.Aeson
    ( FromJSON(..)
    , Object
    , ToJSON(..)
    , Value(..)
    , eitherDecodeStrict'
    , encode
    , object
    , withObject
    , (.:)
    , (.:?)
    , (.!=)
    , (.=)
    )
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

data CodeModeToolMetadata = CodeModeToolMetadata
    { toolMetadataName :: !Text
    , toolMetadataDescription :: !Text
    } deriving (Eq, Show)

instance ToJSON CodeModeToolMetadata where
    toJSON metadata = object
        [ "name" .= metadata.toolMetadataName
        , "description" .= metadata.toolMetadataDescription
        ]

data ToolInvocation = ToolInvocation
    { invocationId :: !Text
    , invocationName :: !Text
    , invocationArguments :: !Value
    } deriving (Eq, Show)

data ProtocolMessage
    = WorkerReady
    | WorkerToolInvocation !ToolInvocation
    | WorkerYielded
        { responseValue :: !Value
        }
    | WorkerNotification
        { notificationText :: !Text
        }
    | WorkerContent
        { contentValue :: !Value
        }
    | WorkerExecSucceeded
        { responseId :: !Text
        , responseValue :: !Value
        , responseStoredValueWrites :: !(Map Text Value)
        }
    | WorkerExecFailed
        { responseId :: !Text
        , responseError :: !Text
        , responseValue :: !Value
        , responseStoredValueWrites :: !(Map Text Value)
        }
    deriving (Eq, Show)

instance FromJSON ProtocolMessage where
    parseJSON = withObject "code-mode protocol message" \message -> do
        ensureOnly ["jsonrpc", "method", "id", "params", "result", "error", "partial_result", "stored_value_writes"] message
        version <- message .: "jsonrpc"
        if (version :: Text) /= "2.0"
            then fail "unsupported jsonrpc version"
            else parseBody message
      where
        parseBody message = do
            method <- message .:? "method"
            case (method :: Maybe Text) of
                Just "ready" -> do
                    ensureOnly ["jsonrpc", "method"] message
                    pure WorkerReady
                Just "tool/call" -> do
                    ensureOnly ["jsonrpc", "method", "id", "params"] message
                    requestId <- message .: "id"
                    params <- message .: "params"
                    invocation <- withObject "tool/call params" (\paramsObject -> do
                        ensureOnly ["name", "arguments"] paramsObject
                        parseJSON (Object paramsObject)) params
                    pure $ WorkerToolInvocation invocation
                        { invocationId = requestId }
                Just "yield" -> do
                    ensureOnly ["jsonrpc", "method", "params"] message
                    params <- message .: "params"
                    withObject "yield params" (\paramsObject -> do
                        ensureOnly ["value"] paramsObject
                        WorkerYielded <$> paramsObject .: "value") params
                Just "notify" -> do
                    ensureOnly ["jsonrpc", "method", "params"] message
                    params <- message .: "params"
                    withObject "notify params" (\paramsObject -> do
                        ensureOnly ["text"] paramsObject
                        WorkerNotification <$> paramsObject .: "text") params
                Just "content" -> do
                    ensureOnly ["jsonrpc", "method", "params"] message
                    params <- message .: "params"
                    withObject "content params" (\paramsObject -> do
                        ensureOnly ["value"] paramsObject
                        WorkerContent <$> paramsObject .: "value") params
                Just unknown ->
                    fail $ "unsupported worker method: " <> show unknown
                Nothing -> do
                    ensureOnly
                        ["jsonrpc", "id", "result", "error", "partial_result", "stored_value_writes"]
                        message
                    requestId <- message .: "id"
                    result <- message .:? "result"
                    err <- message .:? "error"
                    case (result, err) of
                        (Just value, Nothing) ->
                            WorkerExecSucceeded
                                <$> pure requestId
                                <*> pure value
                                <*> message .:? "stored_value_writes"
                                    .!= Map.empty
                        (Nothing, Just errorObject) ->
                            WorkerExecFailed requestId
                                <$> parseError errorObject
                                <*> message .:? "partial_result"
                                    .!= object
                                        [ "content" .= ([] :: [Value]) ]
                                <*> message .:? "stored_value_writes"
                                    .!= Map.empty
                        _ -> fail "response must contain exactly one of result or error"

        parseError = withObject "JSON-RPC error" \errorObject -> do
            ensureOnly ["code", "message"] errorObject
            (_ :: Int) <- errorObject .: "code"
            errorObject .: "message"

instance FromJSON ToolInvocation where
    parseJSON = withObject "tool invocation" \params ->
        ToolInvocation
            <$> pure ""
            <*> params .: "name"
            <*> params .: "arguments"

ensureOnly :: [Text] -> Object -> Parser ()
ensureOnly allowed object =
    case
        [ Key.toText key
        | key <- KeyMap.keys object
        , Key.toText key `notElem` allowed
        ]
    of
        [] -> pure ()
        unknown : _ ->
            fail $ "unexpected field `" <> Text.unpack unknown <> "`"

decodeProtocolMessage :: BS.ByteString -> Either String ProtocolMessage
decodeProtocolMessage = eitherDecodeStrict'

encodeExecRequest :: Text -> Text -> [Text] -> BS.ByteString
encodeExecRequest requestId source toolNames =
    encodeExecRequestWithState
        requestId
        source
        [ CodeModeToolMetadata
            { toolMetadataName = name
            , toolMetadataDescription = ""
            }
        | name <- toolNames
        ]
        Map.empty

encodeExecRequestWithState
    :: Text
    -> Text
    -> [CodeModeToolMetadata]
    -> Map Text Value
    -> BS.ByteString
encodeExecRequestWithState requestId source tools storedValues =
    encodeExecRequestWithStateAndImageDetail
        requestId source tools storedValues True

encodeExecRequestWithStateAndImageDetail
    :: Text
    -> Text
    -> [CodeModeToolMetadata]
    -> Map Text Value
    -> Bool
    -> BS.ByteString
encodeExecRequestWithStateAndImageDetail
        requestId source tools storedValues imageDetailVisible =
    strictEncode $ object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= requestId
        , "method" .= ("exec" :: Text)
        , "params" .= object
            [ "source" .= source
            , "tools" .= tools
            , "stored_values" .= storedValues
            , "image_detail_visible" .= imageDetailVisible
            ]
        ]

encodeToolSuccess :: Text -> Value -> BS.ByteString
encodeToolSuccess requestId value =
    strictEncode $ object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= requestId
        , "result" .= value
        ]

encodeToolFailure :: Text -> Text -> BS.ByteString
encodeToolFailure requestId message =
    strictEncode $ object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= requestId
        , "error" .= object
            [ "code" .= (-32000 :: Int)
            , "message" .= message
            ]
        ]

strictEncode :: Value -> BS.ByteString
strictEncode = LBS.toStrict . encode
