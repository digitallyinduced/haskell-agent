-- | Convert registered AppTools into Responses tool schemas.
module Agent.CLI.Tools
    ( requireToolRegistry
    , lookupAppTool
    , withoutDirectShellTools
    , schemasFromAppTools
    , webSearchTool
    ) where

import Agent.Responses.Types
import Agent.OpenAI.ToolDSL (buildGrokTool, buildTool)
import Agent.Provider (Provider(..))
import Agent.ToolDSL (PropertySchema, parametersObjectLoose)
import Agent.ToolDispatch (canonicalToolName)
import Agent.Tools.ApplyPatch (applyPatchGrammar)
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
import Data.Text (Text)
import qualified Data.Text as Text

requireToolRegistry :: [AppTool] -> IO ToolRegistry
requireToolRegistry tools =
    either (ioError . userError . Text.unpack) pure (mkToolRegistry tools)

lookupAppTool :: Text -> [AppTool] -> Maybe AppTool
lookupAppTool name =
    find (\tool -> tool.appToolName == canonicalToolName name)

-- | Hide parent-facing shell entry points while retaining the full list for
-- nested programmatic dispatch. Background process controls are hidden with
-- their launcher because they are otherwise unusable and provide an escape
-- hatch back to direct terminal interaction.
withoutDirectShellTools :: [AppTool] -> [AppTool]
withoutDirectShellTools =
    filter
        (\tool ->
            canonicalToolName tool.appToolName
                `notElem`
                    [ "shell_command"
                    , "write_stdin"
                    , "run_terminal_cmd"
                    , "get_task_output"
                    , "kill_task"
                    ])

-- | Built-in Responses @web_search@ tool, enabled for every provider by default.
-- The host runs the search server-side; the agent loop never dispatches it.
webSearchTool :: ResponseTool
webSearchTool = KnownResponseTool ToolWebSearch TaggedObject
    { tag = "web_search"
    , fields = KeyMap.empty
    }

schemasFromAppTools :: Provider -> [AppTool] -> [ResponseTool]
schemasFromAppTools provider tools = case provider of
    OpenAIProvider ->
        let (multi, rest) = partition isMultiAgentTool tools
            base = webSearchTool : map (schemaFromAppTool provider) rest
        in if null multi
            then base
            else base ++ [multiAgentNamespaceTool multi]
    _ ->
        webSearchTool : map (schemaFromAppTool provider) tools

isMultiAgentTool :: AppTool -> Bool
isMultiAgentTool tool = tool.appToolName `elem` multiAgentToolNames

schemaFromAppTool :: Provider -> AppTool -> ResponseTool
schemaFromAppTool provider tool = case tool.appToolSchema of
    JsonFunctionSchema parameters ->
        let build = case provider of
                XAIProvider -> buildGrokTool
                OpenRouterProvider -> buildGrokTool
                OpenAIProvider -> buildTool
        in build tool.appToolName tool.appToolDescription parameters
    FreeformApplyPatchSchema ->
        applyPatchCustomTool tool.appToolName tool.appToolDescription

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
