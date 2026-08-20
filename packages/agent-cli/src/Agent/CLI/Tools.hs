-- | Convert registered AppTools into Responses tool schemas.
module Agent.CLI.Tools
    ( lookupAppTool
    , schemasFromAppTools
    , webSearchTool
    ) where

import Agent.OpenAI.Responses.Types
import Agent.OpenAI.ToolDSL (buildGrokTool, buildTool)
import Agent.Provider (Provider(..))
import Agent.Tools.ApplyPatch (applyPatchGrammar)
import Agent.Tools.Types (AppTool(..), AppToolKind(..))
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List (find)
import Data.Text (Text)

lookupAppTool :: Text -> [AppTool] -> Maybe AppTool
lookupAppTool name = find (\tool -> tool.appToolName == name)

-- | Built-in Responses @web_search@ tool, enabled for every provider by default.
-- The host runs the search server-side; the agent loop never dispatches it.
webSearchTool :: ResponseTool
webSearchTool = KnownResponseTool ToolWebSearch TaggedObject
    { tag = "web_search"
    , fields = KeyMap.empty
    }

schemasFromAppTools :: Provider -> [AppTool] -> [ResponseTool]
schemasFromAppTools provider tools =
    webSearchTool : map (schemaFromAppTool provider) tools

schemaFromAppTool :: Provider -> AppTool -> ResponseTool
schemaFromAppTool provider tool = case tool.appToolKind of
    JsonFunction ->
        let build = case provider of
                XAIProvider -> buildGrokTool
                OpenRouterProvider -> buildGrokTool
                OpenAIProvider -> buildTool
        in build tool.appToolName tool.appToolDescription tool.appToolParameters
    FreeformApplyPatch ->
        applyPatchCustomTool tool.appToolName tool.appToolDescription

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
