-- | Tool definitions accepted and returned by the Responses API.
module Agent.Responses.Types.Tools
    ( ResponseTool(..)
    , ResponseToolType(..)
    , responseToolTypeText
    , knownResponseTool
    , FunctionTool(..)
    , taggedObjectEncoder
    , taggedObjectDecoder
    , responseToolTypeEncoder
    , responseToolTypeDecoder
    , functionToolEncoder
    , functionToolDecoder
    , responseToolEncoder
    , responseToolDecoder
    ) where

import Agent.Json
    ( Extensions
    , RawJson
    )
import qualified Agent.Json.Decoder as Decoder
import qualified Agent.Json.Encoder as Encoder
import Agent.Responses.Types.Common (TaggedObject(..))
import Data.Text (Text)

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
    , parameters  :: !(Maybe RawJson)
    , strict      :: !(Maybe Bool)
    , extraFields :: !Extensions
    } deriving stock (Eq, Show)

data ResponseTool
    = FunctionToolValue !FunctionTool
    | KnownResponseTool !ResponseToolType !TaggedObject
    | UnknownResponseTool !TaggedObject
    deriving stock (Eq, Show)

-- | Hosted or built-in Responses tool whose wire @type@ comes from
-- 'ResponseToolType', not a caller-supplied tag string.
knownResponseTool :: ResponseToolType -> Extensions -> ResponseTool
knownResponseTool toolType fields =
    KnownResponseTool toolType TaggedObject
        { tag = responseToolTypeText toolType
        , fields
        }

taggedObjectEncoder :: Encoder.Encoder TaggedObject
taggedObjectEncoder = Encoder.objectWithExtensions (.fields)
    [ Encoder.field "type" Encoder.text (.tag) ]

taggedObjectDecoder :: Decoder.Decoder TaggedObject
taggedObjectDecoder = Decoder.objectFields $
    TaggedObject
        <$> Decoder.requiredField "type" Decoder.text
        <*> Decoder.extensionFields

responseToolTypeEncoder :: Encoder.Encoder ResponseToolType
responseToolTypeEncoder = Encoder.contramap responseToolTypeText Encoder.text

responseToolTypeDecoder :: Decoder.Decoder ResponseToolType
responseToolTypeDecoder = Decoder.mapDecoder parseResponseToolType Decoder.text

functionToolEncoder :: Encoder.Encoder FunctionTool
functionToolEncoder = Encoder.objectWithExtensions (.extraFields)
    [ Encoder.field "type" Encoder.text (const "function")
    , Encoder.field "name" Encoder.text (.name)
    , Encoder.optionalField "description" Encoder.text (.description)
    , Encoder.optionalField "parameters" Encoder.rawJson (.parameters)
    , Encoder.optionalField "strict" Encoder.bool (.strict)
    ]

functionToolDecoder :: Decoder.Decoder FunctionTool
functionToolDecoder = Decoder.objectFields $
    FunctionTool
        <$> Decoder.requiredField "name" Decoder.text
        <*> Decoder.optionalField "description" Decoder.text
        <*> Decoder.optionalField "parameters" Decoder.rawJson
        <*> Decoder.optionalField "strict" Decoder.bool
        <*> Decoder.extensionFields
        <* Decoder.defaultField
            ()
            "type"
            (() <$ Decoder.text)

responseToolEncoder :: Encoder.Encoder ResponseTool
responseToolEncoder = Encoder.choose \case
    FunctionToolValue _ ->
        Encoder.contramap functionValue functionToolEncoder
    KnownResponseTool toolType _ ->
        Encoder.contramap
            knownTagged
            (Encoder.objectWithExtensions (.fields)
                [ Encoder.field
                    "type"
                    Encoder.text
                    (const (responseToolTypeText toolType))
                ])
    UnknownResponseTool _ ->
        Encoder.contramap unknownTagged taggedObjectEncoder
  where
    functionValue = \case
        FunctionToolValue value -> value
        _ -> impossible
    knownTagged = \case
        KnownResponseTool _ value -> value
        _ -> impossible
    unknownTagged = \case
        UnknownResponseTool value -> value
        _ -> impossible
    impossible = error "responseToolEncoder: impossible variant"

responseToolDecoder :: Decoder.Decoder ResponseTool
responseToolDecoder =
    Decoder.discriminatedObject "type" \tag ->
        case parseResponseToolType tag of
            ToolFunction ->
                FunctionToolValue <$> functionToolDecoder
            ToolUnknownType{} ->
                UnknownResponseTool <$> taggedObjectDecoder
            toolType ->
                KnownResponseTool toolType <$> taggedObjectDecoder
