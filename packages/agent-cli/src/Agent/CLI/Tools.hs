-- | Convert registered AppTools into Responses tool schemas.
module Agent.CLI.Tools
    ( requireToolRegistry
    , lookupAppTool
    , schemasFromAppTools
    , schemasFromAppToolsCodeMode
    , hostedSearchToolNames
    , hostedSearchToolCollisions
    , webSearchTool
    , xSearchTool
    ) where

import Agent.Responses.Types.Tools
    ( ResponseTool(..)
    , ResponseToolType(..)
    , FunctionTool(..)
    , CustomTool(..)
    , NamespaceTool(..)
    , knownResponseTool
    , responseToolTypeText
    )
import Agent.Json (rawJsonFromEncoding)
import Agent.Dialect
    ( Dialect
    , DialectId(..)
    , FunctionSchemaStyle(..)
    , ToolLayout(..)
    , dialectId
    , dialectFunctionSchemaStyle
    , dialectToolLayout
    , grokBuildPublicToolName
    )
import Agent.OpenAI.ImageGeneration
    ( imageGenerationNamespace
    , imageGenerationNamespaceDescription
    , imageGenerationToolName
    )
import Agent.OpenAI.ToolDSL (buildGrokTool, buildTool)
import Agent.ToolDSL (PropertySchema(..), parametersObjectLoose)
import Agent.ToolDispatch (canonicalToolName)
import Agent.Codex.Dialect.ApplyPatch (applyPatchGrammar)
import Agent.Tools.MultiAgents (multiAgentNamespace, multiAgentToolNames)
import Agent.Tools.Types
    ( AppTool(..)
    , ToolSchema(..)
    , ToolRegistry
    , mkToolRegistry
    )
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List (partition)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

requireToolRegistry :: [AppTool] -> IO ToolRegistry
requireToolRegistry tools =
    either (ioError . userError . Text.unpack) pure (mkToolRegistry tools)

lookupAppTool :: Text -> [AppTool] -> Maybe AppTool
lookupAppTool name tools =
    Map.lookup (canonicalToolName name) byName
  where
    byName =
        Map.fromList
            [ (canonicalToolName tool.appToolName, tool)
            | tool <- tools
            ]

-- | Built-in Responses @web_search@ tool, enabled for every provider by default.
-- The host runs the search server-side; the agent loop never dispatches it.
webSearchTool :: ResponseTool
webSearchTool = knownResponseTool ToolWebSearch

-- | Grok Build hosted @x_search@ tool. Server-side; the loop never dispatches it.
xSearchTool :: ResponseTool
xSearchTool = knownResponseTool ToolXSearch

hostedSearchToolTypes :: Dialect -> [ResponseToolType]
hostedSearchToolTypes dialect =
    ToolWebSearch
        : [ToolXSearch | dialectId dialect == GrokBuildDialect]

hostedSearchTools :: Dialect -> [ResponseTool]
hostedSearchTools dialect =
    map knownResponseTool (hostedSearchToolTypes dialect)

-- | Model-facing names for hosted search tools advertised in this dialect.
hostedSearchToolNames :: Dialect -> [Text]
hostedSearchToolNames =
    map responseToolTypeText . hostedSearchToolTypes

-- | Names reserved so MCP servers cannot shadow hosted search tools.
hostedSearchToolCollisions :: [(Text, Text)]
hostedSearchToolCollisions =
    [ (responseToolTypeText ToolWebSearch, "built-in web search")
    , (responseToolTypeText ToolXSearch, "built-in X search")
    ]

schemasFromAppTools :: Dialect -> [AppTool] -> [ResponseTool]
schemasFromAppTools dialect tools = case dialectToolLayout dialect of
    CollaborationNamespaceLayout ->
        let (multi, nonMulti) = partition isMultiAgentTool tools
            (imageGeneration, rest) =
                partition isImageGenerationTool nonMulti
            base = hostedSearchTools dialect
                ++ mapMaybe (schemaFromAppTool dialect) rest
            imageNamespaces =
                [ imageGenerationNamespaceTool imageGeneration
                | not (null imageGeneration)
                ]
            collaborationNamespaces =
                [ multiAgentNamespaceTool multi
                | not (null multi)
                ]
        in base ++ imageNamespaces ++ collaborationNamespaces
    FlatToolLayout ->
        hostedSearchTools dialect ++ mapMaybe (schemaFromAppTool dialect) tools
    NoHostToolLayout ->
        []

isMultiAgentTool :: AppTool -> Bool
isMultiAgentTool tool = tool.appToolName `elem` multiAgentToolNames

isImageGenerationTool :: AppTool -> Bool
isImageGenerationTool tool =
    tool.appToolName == imageGenerationToolName

schemaFromAppTool :: Dialect -> AppTool -> Maybe ResponseTool
schemaFromAppTool dialect tool =
    case tool.appToolSchema of
        HostedComputerSchema ->
            Just (knownResponseTool ToolComputer)
        JsonFunctionSchema parameters ->
            case dialectFunctionSchemaStyle dialect of
                NoFunctionSchemas ->
                    Nothing
                StrictFunctionSchemas ->
                    Just (buildSchema buildTool parameters)
                LooseFunctionSchemas ->
                    Just (buildSchema buildGrokTool parameters)
        RawJsonFunctionSchema parameters ->
            Just (FunctionToolValue FunctionTool
                { name = tool.appToolName
                , description = Just tool.appToolDescription
                , parameters =
                    Just (rawJsonFromEncoding (Aeson.toEncoding parameters))
                , strict = case dialectFunctionSchemaStyle dialect of
                    StrictFunctionSchemas -> Just False
                    LooseFunctionSchemas -> Nothing
                    NoFunctionSchemas -> Nothing
                })
        FreeformApplyPatchSchema ->
            case dialectFunctionSchemaStyle dialect of
                NoFunctionSchemas -> Nothing
                _ -> Just (applyPatchCustomTool tool.appToolName tool.appToolDescription)
        FreeformGrammarSchema syntax definition ->
            case dialectFunctionSchemaStyle dialect of
                NoFunctionSchemas -> Nothing
                _ -> Just (grammarCustomTool
                    tool.appToolName
                    tool.appToolDescription
                    syntax
                    definition)
  where
    buildSchema build parameters =
        let (name, description, projectedParameters) =
                projectFunctionTool dialect tool parameters
        in build name description projectedParameters

projectFunctionTool
    :: Dialect
    -> AppTool
    -> [PropertySchema]
    -> (Text, Text, [PropertySchema])
projectFunctionTool dialect tool parameters
    | dialectId dialect == GrokBuildDialect =
        ( grokBuildPublicToolName tool.appToolName
        , grokPublicText tool.appToolDescription
        , map (projectProperty tool.appToolName) parameters
        )
    | otherwise =
        (tool.appToolName, tool.appToolDescription, parameters)

projectProperty :: Text -> PropertySchema -> PropertySchema
projectProperty toolName property =
    property
        { propertyName =
            if toolName == "task"
                && property.propertyName == "run_in_background"
                then "background"
                else property.propertyName
        , description = grokPublicText <$> property.description
        }

grokPublicText :: Text -> Text
grokPublicText =
    replace "run_in_background" "background"
        . replace "run_terminal_cmd" "run_terminal_command"
        . replace "get_task_output" "get_command_or_subagent_output"
        . replace "wait_tasks" "wait_commands_or_subagents"
        . replace "kill_task" "kill_command_or_subagent"
        . replaceTaskName
  where
    replace = Text.replace
    -- Avoid replacing ordinary prose uses of "task"; only the common
    -- backtick-delimited tool reference is unambiguous.
    replaceTaskName = Text.replace "`task`" "`spawn_subagent`"

-- | Codex collaboration namespace: nested non-strict function tools.
multiAgentNamespaceTool :: [AppTool] -> ResponseTool
multiAgentNamespaceTool tools =
    namespaceTool
        multiAgentNamespace
        "Tools for spawning and managing sub-agents."
        tools

imageGenerationNamespaceTool :: [AppTool] -> ResponseTool
imageGenerationNamespaceTool =
    namespaceTool
        imageGenerationNamespace
        imageGenerationNamespaceDescription

namespaceTool :: Text -> Text -> [AppTool] -> ResponseTool
namespaceTool namespaceName namespaceDescription tools =
    NamespaceToolValue NamespaceTool
        { name = namespaceName
        , description = Just namespaceDescription
        , tools = map nestedFunction tools
        }
  where
    nestedFunction tool =
        FunctionToolValue FunctionTool
            { name = tool.appToolName
            , description = Just tool.appToolDescription
            , strict = Just False
            , parameters = Just . rawJsonFromEncoding . Aeson.toEncoding $
                namespaceParameters tool
            }

    namespaceParameters tool = case parametersValue tool of
        Aeson.Object schema
            | Just (Aeson.Array required) <- KeyMap.lookup "required" schema
            , null required -> Aeson.Object (KeyMap.delete "required" schema)
        schema -> schema

    parametersValue tool = case tool.appToolSchema of
        JsonFunctionSchema parameters -> parametersObjectLoose parameters
        RawJsonFunctionSchema parameters -> parameters
        FreeformApplyPatchSchema -> Aeson.object []
        FreeformGrammarSchema _ _ -> Aeson.object []
        HostedComputerSchema -> Aeson.object []

-- | Codex registers apply_patch as a Responses custom tool with a Lark grammar.
applyPatchCustomTool :: Text -> Text -> ResponseTool
applyPatchCustomTool name description =
    grammarCustomTool name description "lark" applyPatchGrammar

-- | Freeform custom tool with an explicit provider grammar, matching the
-- Codex wire shape for tools such as code-mode @exec@.
grammarCustomTool :: Text -> Text -> Text -> Text -> ResponseTool
grammarCustomTool name description syntax definition =
    CustomToolValue CustomTool
        { name
        , description = Just description
        , format = Just . rawJsonFromEncoding . Aeson.toEncoding $
            Aeson.object
                [ "type" .= ("grammar" :: Text)
                , "syntax" .= syntax
                , "definition" .= definition
                ]
        }

-- | Code-mode tool surface: the @exec@/@wait@ entry points first, hosted
-- search tools last, matching the upstream Codex wire order.
schemasFromAppToolsCodeMode :: Dialect -> [AppTool] -> [ResponseTool]
schemasFromAppToolsCodeMode dialect tools =
    mapMaybe (schemaFromAppTool dialect) tools
        ++ hostedSearchTools dialect
