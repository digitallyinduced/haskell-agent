{-# LANGUAGE ExistentialQuantification #-}

module Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , ToolCallResult(..)
    , ToolDispatchConfig(..)
    , ToolHandler
    , typedTool
    , noArgsTool
    , functionToolCall
    , customToolCall
    , dispatchToolCall
    , toolArgumentsValue
    , decodeToolArguments
    ) where

import Agent.ToolArgs (stripAesonPrefix)
import Control.Exception (SomeException)
import qualified Control.Exception as Exception
import Data.Aeson (FromJSON, Value(..))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseEither)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

-- | How the originating model turn encoded this call. Adapters need this to
-- emit @function_call_output@ versus @custom_tool_call_output@.
data ToolCallKind
    = FunctionCallKind
    | CustomCallKind
    deriving (Eq, Show)

-- | Provider-neutral function or custom tool call emitted by a model transport.
data ToolCall = ToolCall
    { callId :: !Text
    , name :: !Text
    , arguments :: !Text
    , callKind :: !ToolCallKind
    } deriving (Eq, Show)

-- | Provider-neutral result ready for a transport adapter to encode.
data ToolCallResult = ToolCallResult
    { callId :: !Text
    , output :: !Text
    , callKind :: !ToolCallKind
    } deriving (Eq, Show)

functionToolCall :: Text -> Text -> Text -> ToolCall
functionToolCall callId name arguments = ToolCall
    { callId
    , name
    , arguments
    , callKind = FunctionCallKind
    }

customToolCall :: Text -> Text -> Text -> ToolCall
customToolCall callId name arguments = ToolCall
    { callId
    , name
    , arguments
    , callKind = CustomCallKind
    }

data ToolDispatchConfig = ToolDispatchConfig
    { toolDispatchUnknownTool :: Text -> Text
    , toolDispatchFormatResult :: Either Text Text -> Text
    , toolDispatchFormatException :: Text -> SomeException -> Text
    , toolDispatchOnException :: Text -> SomeException -> IO ()
    }

data ToolHandler
    = forall args. FromJSON args => TypedTool Text (args -> IO (Either Text Text))
    | NoArgsTool Text (IO (Either Text Text))

typedTool :: FromJSON args => Text -> (args -> IO (Either Text Text)) -> ToolHandler
typedTool = TypedTool

noArgsTool :: Text -> IO (Either Text Text) -> ToolHandler
noArgsTool = NoArgsTool

dispatchToolCall :: ToolDispatchConfig -> [ToolHandler] -> ToolCall -> IO ToolCallResult
dispatchToolCall config handlers call = do
    let callName = call.name
        input = toolArgumentsValue call.arguments
        runTool = case findHandler callName handlers of
            Just handler -> runHandler input handler
            Nothing -> pure (Left (config.toolDispatchUnknownTool callName))
    result <- Exception.try @SomeException runTool
    resultOutput <- case result of
        Right toolResult ->
            pure (config.toolDispatchFormatResult toolResult)
        Left exception -> do
            config.toolDispatchOnException callName exception
            pure (config.toolDispatchFormatException callName exception)
    pure ToolCallResult
        { callId = call.callId
        , output = resultOutput
        , callKind = call.callKind
        }

toolArgumentsValue :: Text -> Value
toolArgumentsValue arguments =
    case Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 arguments) of
        Right value -> value
        Left _ -> String arguments

decodeToolArguments :: FromJSON args => Value -> Either Text args
decodeToolArguments value =
    case parseEither Aeson.parseJSON value of
        Right args -> Right args
        Left err -> Left (stripAesonPrefix (Text.pack err))

findHandler :: Text -> [ToolHandler] -> Maybe ToolHandler
findHandler name = find ((== name) . handlerName)

handlerName :: ToolHandler -> Text
handlerName = \case
    TypedTool name _ -> name
    NoArgsTool name _ -> name

runHandler :: Value -> ToolHandler -> IO (Either Text Text)
runHandler value = \case
    TypedTool _ run ->
        case decodeToolArguments value of
            Right args -> run args
            Left err -> pure (Left err)
    NoArgsTool _ run ->
        run
