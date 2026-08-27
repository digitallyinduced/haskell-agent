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

import qualified Agent.Json.Decode as Json
import Data.Aeson
    ( ToJSON(..)
    , Value(..)
    , encode
    , object
    , (.=)
    )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector

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

protocolMessageDecoder :: Json.Decoder ProtocolMessage
protocolMessageDecoder = Json.object do
        version <- Json.atKey "jsonrpc" Json.text
        if (version :: Text) /= "2.0"
            then fail "unsupported jsonrpc version"
            else parseBody
      where
        parseBody = do
            method <- Json.atKeyOptional "method" Json.text
            case (method :: Maybe Text) of
                Just "ready" -> pure WorkerReady
                Just "tool/call" -> do
                    requestId <- Json.atKey "id" Json.text
                    invocation <- Json.atKey "params" toolInvocationDecoder
                    pure $ WorkerToolInvocation invocation
                        { invocationId = requestId }
                Just "yield" ->
                    Json.atKey "params" $
                        Json.object (WorkerYielded <$> Json.atKey "value" aesonValueDecoder)
                Just "notify" ->
                    Json.atKey "params" $
                        Json.object (WorkerNotification <$> Json.atKey "text" Json.text)
                Just "content" ->
                    Json.atKey "params" $
                        Json.object (WorkerContent <$> Json.atKey "value" aesonValueDecoder)
                Just unknown ->
                    fail $ "unsupported worker method: " <> show unknown
                Nothing -> do
                    requestId <- Json.atKey "id" Json.text
                    result <- Json.atKeyOptional "result" aesonValueDecoder
                    err <- Json.atKeyOptional "error" errorDecoder
                    writes <- maybe Map.empty id
                        <$> Json.atKeyOptional "stored_value_writes"
                            (Json.objectAsMap pure aesonValueDecoder)
                    case (result, err) of
                        (Just value, Nothing) ->
                            pure (WorkerExecSucceeded requestId value writes)
                        (Nothing, Just message) -> do
                            partial <- maybe (object ["content" .= ([] :: [Value])]) id
                                <$> Json.atKeyOptional "partial_result" aesonValueDecoder
                            pure (WorkerExecFailed requestId message partial writes)
                        _ -> fail "response must contain exactly one of result or error"

        errorDecoder = Json.object do
            (_ :: Int) <- Json.atKey "code" Json.int
            Json.atKey "message" Json.text

toolInvocationDecoder :: Json.Decoder ToolInvocation
toolInvocationDecoder = Json.object $
        ToolInvocation
            <$> pure ""
            <*> Json.atKey "name" Json.text
            <*> Json.atKey "arguments" aesonValueDecoder

decodeProtocolMessage :: BS.ByteString -> Either String ProtocolMessage
decodeProtocolMessage =
    either (Left . Text.unpack . (.jsonErrorMessage)) Right
        . Json.decodeEither protocolMessageDecoder

-- Code mode deliberately transports arbitrary JavaScript values. Decode that
-- dynamic protocol leaf with Hermes, while keeping Aeson only as the in-memory
-- representation used by the existing evaluator and encoder.
aesonValueDecoder :: Json.Decoder Value
aesonValueDecoder = Json.withType \case
    Json.VArray -> Array . Vector.fromList <$> Json.list aesonValueDecoder
    Json.VObject ->
        Object . KeyMap.fromList
            <$> Json.objectAsKeyValues
                (pure . Key.fromText)
                aesonValueDecoder
    Json.VNumber -> Number <$> Json.scientific
    Json.VString -> String <$> Json.text
    Json.VBoolean -> Bool <$> Json.bool
    Json.VNull -> pure Null

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
