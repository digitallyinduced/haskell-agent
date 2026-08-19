-- | Convert registered AppTools into Responses tool schemas.
module Agent.CLI.Tools
    ( lookupAppTool
    , schemasFromAppTools
    ) where

import Agent.OpenAI.Responses.Types
import Agent.OpenAI.ToolDSL (buildTool)
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

schemasFromAppTools :: [AppTool] -> [ResponseTool]
schemasFromAppTools = map schemaFromAppTool

schemaFromAppTool :: AppTool -> ResponseTool
schemaFromAppTool tool = case tool.appToolKind of
    JsonFunction ->
        buildTool tool.appToolName tool.appToolDescription tool.appToolParameters
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
