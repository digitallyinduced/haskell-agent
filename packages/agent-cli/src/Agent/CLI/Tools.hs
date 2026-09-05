-- | Convert registered AppTools into Responses tool schemas.
module Agent.CLI.Tools
    ( requireToolRegistry
    , lookupAppTool
    , schemasFromAppTools
    , schemasFromAppToolsWithAsyncCapability
    , schemasFromAppToolsWithHostedSearch
    , schemasFromAppToolsWithHostedSearchAndAsyncCapability
    , schemasFromAppToolsCodeMode
    , schemasFromAppToolsCodeModeWithAsyncCapability
    , schemasFromAppToolsCodeModeWithHostedSearch
    , schemasFromAppToolsCodeModeWithHostedSearchAndAsyncCapability
    , hostedSearchToolNames
    , hostedSearchToolNamesWhen
    , hostedSearchToolCollisions
    , webSearchTool
    , xSearchTool
    ) where

import Agent.CLI.ComputerUse (computerFunctionParameters)
import Agent.Responses.Types.Tools
    ( ResponseTool(..)
    , ResponseToolType(..)
    , FunctionTool(..)
    , CustomTool(..)
    , NamespaceTool(..)
    , computerFunctionName
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
    , appToolSupportsAsync
    )
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List (partition)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Info (os)

requireToolRegistry :: [AppTool] -> IO ToolRegistry
requireToolRegistry tools
    | any reservesComputerFunction tools =
        ioError . userError . Text.unpack $
            "tool name " <> computerFunctionName
                <> " is reserved for local computer control"
    | otherwise =
        either (ioError . userError . Text.unpack) pure (mkToolRegistry tools)
  where
    reservesComputerFunction tool =
        canonicalToolName tool.appToolName == computerFunctionName
            && tool.appToolSchema /= HostedComputerSchema

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

hostedSearchToolsWhen :: Bool -> Dialect -> [ResponseTool]
hostedSearchToolsWhen includeHostedSearch dialect
    | includeHostedSearch = hostedSearchTools dialect
    | otherwise = []

-- | Model-facing names for hosted search tools advertised in this dialect.
hostedSearchToolNames :: Dialect -> [Text]
hostedSearchToolNames =
    map responseToolTypeText . hostedSearchToolTypes

-- | Model-facing hosted search names, optionally omitted at an execution
-- boundary which requires every model-controlled network action to be routed
-- through an application tool.
hostedSearchToolNamesWhen :: Bool -> Dialect -> [Text]
hostedSearchToolNamesWhen includeHostedSearch dialect
    | includeHostedSearch = hostedSearchToolNames dialect
    | otherwise = []

-- | Names reserved so MCP servers cannot shadow hosted search tools.
hostedSearchToolCollisions :: [(Text, Text)]
hostedSearchToolCollisions =
    [ (responseToolTypeText ToolWebSearch, "built-in web search")
    , (responseToolTypeText ToolXSearch, "built-in X search")
    ]

schemasFromAppTools :: Dialect -> [AppTool] -> [ResponseTool]
schemasFromAppTools = schemasFromAppToolsWithHostedSearch True

schemasFromAppToolsWithAsyncCapability
    :: Bool
    -> Dialect
    -> [AppTool]
    -> [ResponseTool]
schemasFromAppToolsWithAsyncCapability =
    schemasFromAppToolsWithHostedSearchAndAsyncCapability True

-- | Project application tools while explicitly controlling provider-hosted
-- search. Hosted search bypasses application-tool dispatch, so embeddings
-- without that capability must pass 'False'.
schemasFromAppToolsWithHostedSearch
    :: Bool
    -> Dialect
    -> [AppTool]
    -> [ResponseTool]
schemasFromAppToolsWithHostedSearch includeHostedSearch =
    schemasFromAppToolsWithHostedSearchAndAsyncCapability
        includeHostedSearch
        False

schemasFromAppToolsWithHostedSearchAndAsyncCapability
    :: Bool
    -> Bool
    -> Dialect
    -> [AppTool]
    -> [ResponseTool]
schemasFromAppToolsWithHostedSearchAndAsyncCapability
        includeHostedSearch modelSupportsAsync dialect tools =
    case dialectToolLayout dialect of
        CollaborationNamespaceLayout ->
            let (multi, nonMulti) = partition isMultiAgentTool tools
                (imageGeneration, rest) =
                    partition isImageGenerationTool nonMulti
                base = hostedSearchToolsWhen includeHostedSearch dialect
                    ++ mapMaybe
                        (schemaFromAppTool modelSupportsAsync dialect)
                        rest
                imageNamespaces =
                    [ imageGenerationNamespaceTool
                        modelSupportsAsync
                        imageGeneration
                    | not (null imageGeneration)
                    ]
                collaborationNamespaces =
                    [ multiAgentNamespaceTool modelSupportsAsync multi
                    | not (null multi)
                    ]
            in base ++ imageNamespaces ++ collaborationNamespaces
        FlatToolLayout ->
            hostedSearchToolsWhen includeHostedSearch dialect
                ++ mapMaybe
                    (schemaFromAppTool modelSupportsAsync dialect)
                    tools
        NoHostToolLayout ->
            []

isMultiAgentTool :: AppTool -> Bool
isMultiAgentTool tool = tool.appToolName `elem` multiAgentToolNames

isImageGenerationTool :: AppTool -> Bool
isImageGenerationTool tool =
    tool.appToolName == imageGenerationToolName

schemaFromAppTool :: Bool -> Dialect -> AppTool -> Maybe ResponseTool
schemaFromAppTool _ _ tool
    | canonicalToolName tool.appToolName == computerFunctionName
    , tool.appToolSchema /= HostedComputerSchema =
        Nothing
schemaFromAppTool modelSupportsAsync dialect tool =
    fmap (setAsyncCapability supportsAsync) $
        case tool.appToolSchema of
            HostedComputerSchema ->
                if os == "darwin" && dialectId dialect == CodexDialect
                    then Just (FunctionToolValue FunctionTool
                        { name = computerFunctionName
                        , description = Just tool.appToolDescription
                        , parameters = Just
                            (rawJsonFromEncoding
                                (Aeson.toEncoding computerFunctionParameters))
                        , strict = Just True
                        , async = Nothing
                        })
                    else Nothing
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
                    , async = Nothing
                    })
            FreeformApplyPatchSchema ->
                case dialectFunctionSchemaStyle dialect of
                    NoFunctionSchemas -> Nothing
                    _ -> Just
                        (applyPatchCustomTool
                            tool.appToolName
                            tool.appToolDescription)
            FreeformGrammarSchema syntax definition ->
                case dialectFunctionSchemaStyle dialect of
                    NoFunctionSchemas -> Nothing
                    _ -> Just (grammarCustomTool
                        tool.appToolName
                        tool.appToolDescription
                        syntax
                        definition)
  where
    supportsAsync = modelSupportsAsync && appToolSupportsAsync tool

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

setAsyncCapability :: Bool -> ResponseTool -> ResponseTool
setAsyncCapability enabled = \case
    FunctionToolValue tool ->
        FunctionToolValue tool
            { async = if enabled then Just True else Nothing }
    CustomToolValue tool ->
        CustomToolValue tool
            { async = if enabled then Just True else Nothing }
    tool -> tool

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
multiAgentNamespaceTool :: Bool -> [AppTool] -> ResponseTool
multiAgentNamespaceTool modelSupportsAsync tools =
    namespaceTool
        modelSupportsAsync
        multiAgentNamespace
        "Tools for spawning and managing sub-agents."
        tools

imageGenerationNamespaceTool :: Bool -> [AppTool] -> ResponseTool
imageGenerationNamespaceTool modelSupportsAsync =
    namespaceTool
        modelSupportsAsync
        imageGenerationNamespace
        imageGenerationNamespaceDescription

namespaceTool :: Bool -> Text -> Text -> [AppTool] -> ResponseTool
namespaceTool modelSupportsAsync namespaceName namespaceDescription tools =
    NamespaceToolValue NamespaceTool
        { name = namespaceName
        , description = Just namespaceDescription
        , tools = map nestedFunction tools
        }
  where
    nestedFunction tool =
        setAsyncCapability
            (modelSupportsAsync && appToolSupportsAsync tool) $
        FunctionToolValue FunctionTool
            { name = tool.appToolName
            , description = Just tool.appToolDescription
            , strict = Just False
            , async = Nothing
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
        , async = Nothing
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
schemasFromAppToolsCodeMode =
    schemasFromAppToolsCodeModeWithHostedSearch True

schemasFromAppToolsCodeModeWithAsyncCapability
    :: Bool
    -> Dialect
    -> [AppTool]
    -> [ResponseTool]
schemasFromAppToolsCodeModeWithAsyncCapability =
    schemasFromAppToolsCodeModeWithHostedSearchAndAsyncCapability True

schemasFromAppToolsCodeModeWithHostedSearch
    :: Bool
    -> Dialect
    -> [AppTool]
    -> [ResponseTool]
schemasFromAppToolsCodeModeWithHostedSearch
        includeHostedSearch =
    schemasFromAppToolsCodeModeWithHostedSearchAndAsyncCapability
        includeHostedSearch
        False

schemasFromAppToolsCodeModeWithHostedSearchAndAsyncCapability
    :: Bool
    -> Bool
    -> Dialect
    -> [AppTool]
    -> [ResponseTool]
schemasFromAppToolsCodeModeWithHostedSearchAndAsyncCapability
        includeHostedSearch modelSupportsAsync dialect tools =
    mapMaybe (schemaFromAppTool modelSupportsAsync dialect) tools
        ++ hostedSearchToolsWhen includeHostedSearch dialect
