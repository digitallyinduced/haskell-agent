-- | Convert registered AppTools into Responses tool schemas.
module Agent.CLI.Tools
    ( requireToolRegistry
    , lookupAppTool
    , schemasFromAppTools
    , schemasFromAppToolsWithWeb
    , webSearchTool
    ) where

import Agent.Responses.Types
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
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List (find, partition)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

requireToolRegistry :: [AppTool] -> IO ToolRegistry
requireToolRegistry tools =
    either (ioError . userError . Text.unpack) pure (mkToolRegistry tools)

lookupAppTool :: Text -> [AppTool] -> Maybe AppTool
lookupAppTool name =
    find (\tool -> tool.appToolName == canonicalToolName name)

-- | Built-in Responses @web_search@ tool, enabled for every provider by default.
-- The host runs the search server-side; the agent loop never dispatches it.
webSearchTool :: ResponseTool
webSearchTool = KnownResponseTool ToolWebSearch TaggedObject
    { tag = "web_search"
    , fields = KeyMap.empty
    }

schemasFromAppTools :: Dialect -> [AppTool] -> [ResponseTool]
schemasFromAppTools = schemasFromAppToolsWithWeb True

schemasFromAppToolsWithWeb :: Bool -> Dialect -> [AppTool] -> [ResponseTool]
schemasFromAppToolsWithWeb includeWeb dialect tools = case dialectToolLayout dialect of
    CollaborationNamespaceLayout ->
        let (multi, rest) = partition isMultiAgentTool tools
            base = webTools <> mapMaybe (schemaFromAppTool dialect) rest
        in if null multi
            then base
            else base ++ [multiAgentNamespaceTool multi]
    FlatToolLayout ->
        webTools <> mapMaybe (schemaFromAppTool dialect) tools
    NoHostToolLayout ->
        []
  where
    webTools = [webSearchTool | includeWeb]

isMultiAgentTool :: AppTool -> Bool
isMultiAgentTool tool = tool.appToolName `elem` multiAgentToolNames

schemaFromAppTool :: Dialect -> AppTool -> Maybe ResponseTool
schemaFromAppTool dialect tool =
    case tool.appToolSchema of
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
                , parameters = Just parameters
                , strict = case dialectFunctionSchemaStyle dialect of
                    StrictFunctionSchemas -> Just False
                    LooseFunctionSchemas -> Nothing
                    NoFunctionSchemas -> Nothing
                , extraFields = KeyMap.empty
                })
        FreeformApplyPatchSchema ->
            case dialectFunctionSchemaStyle dialect of
                NoFunctionSchemas -> Nothing
                _ -> Just (applyPatchCustomTool tool.appToolName tool.appToolDescription)
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
multiAgentNamespaceTool tools = KnownResponseTool ToolNamespace TaggedObject
    { tag = "namespace"
    , fields = KeyMap.fromList
        [ (Key.fromText "name", Aeson.String multiAgentNamespace)
        , (Key.fromText "description", Aeson.String
            "Tools for spawning and managing sub-agents.")
        , (Key.fromText "tools", Aeson.toJSON (map nestedFunction tools))
        ]
    }
  where
    nestedFunction tool = Aeson.object
        [ "type" .= ("function" :: Text)
        , "name" .= tool.appToolName
        , "description" .= tool.appToolDescription
        , "strict" .= False
        , "parameters" .= namespaceParameters (appToolJsonParameters tool)
        ]

    namespaceParameters parameters = case parametersObjectLoose parameters of
        Aeson.Object schema
            | Just (Aeson.Array required) <- KeyMap.lookup "required" schema
            , null required -> Aeson.Object (KeyMap.delete "required" schema)
        schema -> schema

appToolJsonParameters :: AppTool -> [PropertySchema]
appToolJsonParameters tool = case tool.appToolSchema of
    JsonFunctionSchema parameters -> parameters
    RawJsonFunctionSchema _ -> []
    FreeformApplyPatchSchema -> []

-- | Codex registers apply_patch as a Responses custom tool with a Lark grammar.
applyPatchCustomTool :: Text -> Text -> ResponseTool
applyPatchCustomTool name description = KnownResponseTool ToolCustom TaggedObject
    { tag = "custom"
    , fields = KeyMap.fromList
        [ (Key.fromText "name", Aeson.String name)
        , (Key.fromText "description", Aeson.String description)
        , (Key.fromText "format", Aeson.object
            [ "type" .= ("grammar" :: Text)
            , "syntax" .= ("lark" :: Text)
            , "definition" .= applyPatchGrammar
            ])
        ]
    }
