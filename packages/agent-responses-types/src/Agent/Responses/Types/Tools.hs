-- | Tool definitions accepted and returned by the Responses API.
module Agent.Responses.Types.Tools
    ( ResponseTool(..)
    , ResponseToolType(..)
    , responseToolTypeText
    , knownResponseTool
    , FunctionTool(..)
    , computerFunctionNamespace
    , computerFunctionName
    ) where

import Agent.Responses.Types.Common
import Data.Aeson hiding (TaggedObject)
import qualified Data.Aeson as Aeson
import Data.Text (Text)

-- | Reserved names used when a Responses Lite transport cannot register the
-- provider-native computer tool and must expose the same harness as a
-- screenshot-returning function instead.
computerFunctionNamespace :: Text
computerFunctionNamespace = "computer_use"

computerFunctionName :: Text
computerFunctionName = "computer"

data ResponseToolType
    = ToolFunction
    | ToolFileSearch
    | ToolComputer
    | ToolComputerUsePreview
    | ToolWebSearch
    | ToolXSearch
    | ToolMcp
    | ToolCodeInterpreter
    | ToolProgrammaticToolCalling
    | ToolImageGeneration
    | ToolLocalShell
    | ToolShell
    | ToolCustom
    | ToolNamespace
    | ToolSearch
    | ToolWebSearchPreview
    | ToolApplyPatch
    | ToolUnknownType !Text
    deriving stock (Eq, Show)

responseToolTypeText :: ResponseToolType -> Text
responseToolTypeText = \case
    ToolFunction -> "function"
    ToolFileSearch -> "file_search"
    ToolComputer -> "computer"
    ToolComputerUsePreview -> "computer_use_preview"
    ToolWebSearch -> "web_search"
    ToolXSearch -> "x_search"
    ToolMcp -> "mcp"
    ToolCodeInterpreter -> "code_interpreter"
    ToolProgrammaticToolCalling -> "programmatic_tool_calling"
    ToolImageGeneration -> "image_generation"
    ToolLocalShell -> "local_shell"
    ToolShell -> "shell"
    ToolCustom -> "custom"
    ToolNamespace -> "namespace"
    ToolSearch -> "tool_search"
    ToolWebSearchPreview -> "web_search_preview"
    ToolApplyPatch -> "apply_patch"
    ToolUnknownType value -> value

parseResponseToolType :: Text -> ResponseToolType
parseResponseToolType value = case value of
    "function" -> ToolFunction
    "file_search" -> ToolFileSearch
    "computer" -> ToolComputer
    "computer_use_preview" -> ToolComputerUsePreview
    "computer_use" -> ToolComputer
    "web_search" -> ToolWebSearch
    "x_search" -> ToolXSearch
    "mcp" -> ToolMcp
    "code_interpreter" -> ToolCodeInterpreter
    "programmatic_tool_calling" -> ToolProgrammaticToolCalling
    "image_generation" -> ToolImageGeneration
    "local_shell" -> ToolLocalShell
    "shell" -> ToolShell
    "custom" -> ToolCustom
    "namespace" -> ToolNamespace
    "tool_search" -> ToolSearch
    "web_search_preview" -> ToolWebSearchPreview
    "apply_patch" -> ToolApplyPatch
    other -> ToolUnknownType other

data FunctionTool = FunctionTool
    { name        :: !Text
    , description :: !(Maybe Text)
    , parameters  :: !(Maybe Aeson.Value)
    , strict      :: !(Maybe Bool)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON FunctionTool where
    toJSON FunctionTool
        { name, description, parameters, strict, extraFields } =
            objectWith extraFields
                [ Just (field "type" ("function" :: Text))
                , Just (field "name" name)
                , optionalField "description" description
                , optionalField "parameters" parameters
                , optionalField "strict" strict
                ]

instance FromJSON FunctionTool where
    parseJSON = withObject "FunctionTool" $ \o -> FunctionTool
        <$> o .: "name"
        <*> o .:? "description"
        <*> o .:? "parameters"
        <*> o .:? "strict"
        <*> pure
            (without
                ["type", "name", "description", "parameters", "strict"] o)

data ResponseTool
    = FunctionToolValue !FunctionTool
    | KnownResponseTool !ResponseToolType !TaggedObject
    | UnknownResponseTool !TaggedObject
    deriving stock (Eq, Show)

-- | Hosted or built-in Responses tool whose wire @type@ comes from
-- 'ResponseToolType', not a caller-supplied tag string.
knownResponseTool :: ResponseToolType -> Aeson.Object -> ResponseTool
knownResponseTool toolType fields =
    KnownResponseTool toolType TaggedObject
        { tag = responseToolTypeText toolType
        , fields
        }

instance ToJSON ResponseTool where
    toJSON (FunctionToolValue value) = toJSON value
    toJSON (KnownResponseTool toolType TaggedObject { fields }) =
        objectWith fields [Just (field "type" (responseToolTypeText toolType))]
    toJSON (UnknownResponseTool value) = toJSON value

instance FromJSON ResponseTool where
    parseJSON value = withObject "ResponseTool" (\o -> do
        tag <- o .: "type"
        case parseResponseToolType tag of
            ToolFunction -> FunctionToolValue <$> parseJSON value
            ToolUnknownType{} -> UnknownResponseTool <$> parseJSON value
            toolType -> KnownResponseTool toolType <$> parseJSON value) value
