-- | Skills that bring their own tools and load on demand through
-- client-executed tool search.
--
-- A tool skill is an instruction text plus the tools it needs. Its tools are
-- not part of the request's tool list. The request advertises one
-- @tool_search@ tool (@execution: client@) whose description lists the skill
-- catalog. When the model calls it, the host answers with a
-- @tool_search_output@ item that carries each requested skill as a tool
-- namespace: the namespace description holds the skill's instructions and
-- its tools are marked @defer_loading@. The model then calls those tools like
-- any other function, with the skill name as the call's namespace.
--
-- This mirrors Codex's deferred tools. The request's instructions and tool
-- list stay byte-identical while skills load, so the cached prompt prefix
-- survives; adding the same tools to the tool list instead would invalidate
-- it. Loaded definitions live in the transcript, so they survive replays of
-- that transcript. A compaction that drops the search items unloads them, and
-- the model searches again.
module Agent.Skills.ToolSearch
    ( ToolSkill(..)
    , validateToolSkills
    , toolSearchToolName
    , defaultToolSkillSearchPreamble
    , toolSkillSearchDefinition
    , toolSkillSearchDescription
    , toolSkillSearchParameters
    , toolSkillSearchTool
    , resolveToolSkillSearch
    , toolSkillNamespace
    , toolSkillToolNames
    , toolSearchOutputTools
    ) where

import Agent.ToolArgs (objectArgs, reqTextList)
import Agent.ToolDispatch
    ( ToolHandlerResult(..)
    , decodeToolArguments
    , passthroughTool
    )
import Agent.Tools.Types
    ( AppTool
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , rawJsonAppToolWithExecution
    )
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (nub, (\\))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

-- | One skill of the catalog.
data ToolSkill = ToolSkill
    { toolSkillName :: !Text
    -- ^ Name the model loads the skill by. It is also the namespace of the
    -- skill's tool calls, so it must be a valid tool name.
    , toolSkillDescription :: !Text
    -- ^ One catalog line: what the skill covers and when to load it.
    , toolSkillInstructions :: !Text
    -- ^ Full instructions, delivered as the namespace description once the
    -- skill is loaded.
    , toolSkillTools :: ![Value]
    -- ^ Provider function definitions (@{"type":"function",...}@). The
    -- Responses API rejects a namespace without tools, so a skill needs at
    -- least one.
    } deriving (Eq, Show)

-- | Reject catalogs the provider would reject when a skill loads, or that
-- the model could not address: invalid or duplicate skill names, skills
-- without tools, tools without a name.
validateToolSkills :: [ToolSkill] -> Either Text [ToolSkill]
validateToolSkills skills
    | Just skill <- findSkill (not . validName . (.toolSkillName)) =
        Left ("tool skill has an invalid name: " <> skill.toolSkillName)
    | duplicates /= [] =
        Left ("duplicate tool skill names: " <> Text.intercalate ", " duplicates)
    | Just skill <- findSkill (null . (.toolSkillTools)) =
        Left ("tool skill has no tools: " <> skill.toolSkillName)
    | Just skill <- findSkill (\skill -> length (toolSkillToolNames skill) /= length skill.toolSkillTools) =
        Left ("tool skill has a tool without a name: " <> skill.toolSkillName)
    | otherwise = Right skills
  where
    names = map (.toolSkillName) skills
    duplicates = nub (names \\ nub names)
    findSkill predicate = case filter predicate skills of
        skill : _ -> Just skill
        [] -> Nothing
    validName name =
        not (Text.null name)
            && Text.length name <= 64
            && Text.all (\c -> isAsciiLower c || isAsciiUpper c || isDigit c || c == '_' || c == '-') name

-- | The Responses tool name of client-executed tool search.
toolSearchToolName :: Text
toolSearchToolName = "tool_search"

-- | Description preamble for hosts without their own wording.
defaultToolSkillSearchPreamble :: Text
defaultToolSkillSearchPreamble =
    "# Skills\n\n\
    \Loads skills: their instructions and tools become available for the next model call \
    \and stay available in this conversation. Some tools are not provided upfront; load \
    \the skill that covers a task before working on it."

-- | The request's @tool_search@ tool. Hosts advertise this definition
-- instead of rendering 'toolSkillSearchTool' as a function.
toolSkillSearchDefinition :: Text -> [ToolSkill] -> Value
toolSkillSearchDefinition preamble skills = object
    [ "type" .= toolSearchToolName
    , "execution" .= ("client" :: Text)
    , "description" .= toolSkillSearchDescription preamble skills
    , "parameters" .= toolSkillSearchParameters skills
    ]

-- | The preamble followed by one catalog line per skill. The line names the
-- skill's tools so that instructions mentioning a tool lead to its skill.
toolSkillSearchDescription :: Text -> [ToolSkill] -> Text
toolSkillSearchDescription preamble skills =
    Text.intercalate "\n" (preamble : "" : "Available skills:" : map catalogLine skills)
  where
    catalogLine skill =
        "- " <> skill.toolSkillName <> ": " <> singleLine skill.toolSkillDescription
            <> " Tools: " <> Text.intercalate ", " (toolSkillToolNames skill)
    singleLine = Text.unwords . Text.words

toolSkillSearchParameters :: [ToolSkill] -> Value
toolSkillSearchParameters skills = object
    [ "type" .= ("object" :: Text)
    , "properties" .= object
        [ "skills" .= object
            [ "type" .= ("array" :: Text)
            , "description" .= ("Names of the skills to load." :: Text)
            , "items" .= object
                [ "type" .= ("string" :: Text)
                , "enum" .= map (.toolSkillName) skills
                ]
            ]
        ]
    , "required" .= (["skills"] :: [Text])
    , "additionalProperties" .= False
    ]

-- | Dispatch target for a tool search call. Its output is the JSON array for
-- the @tool_search_output@ item; see 'toolSearchOutputTools'. Names outside
-- the catalog are skipped, so an invalid request loads nothing instead of
-- failing the turn.
toolSkillSearchTool :: [ToolSkill] -> AppTool
toolSkillSearchTool skills =
    rawJsonAppToolWithExecution
        toolSearchToolName
        (toolSkillSearchDescription defaultToolSkillSearchPreamble skills)
        (toolSkillSearchParameters skills)
        AlwaysAllowed
        ParallelSafe
        (passthroughTool toolSearchToolName \_emit call -> pure do
            loaded <- resolveToolSkillSearch skills call.arguments
            let output = Aeson.toJSON (map toolSkillNamespace loaded)
            Right (ToolHandlerResult (Text.decodeUtf8 (LBS.toStrict (Aeson.encode output))) []))

-- | The catalog skills a tool search call requests, in request order and
-- without repetitions.
resolveToolSkillSearch :: [ToolSkill] -> Text -> Either Text [ToolSkill]
resolveToolSkillSearch skills arguments = do
    requested <- decodeToolArguments (objectArgs \args -> reqTextList args "skills") arguments
    pure (mapMaybe lookupSkill (nub requested))
  where
    lookupSkill name = case filter ((== name) . (.toolSkillName)) skills of
        skill : _ -> Just skill
        [] -> Nothing

-- | A loaded skill as a Responses tool namespace.
toolSkillNamespace :: ToolSkill -> Value
toolSkillNamespace skill = object
    [ "type" .= ("namespace" :: Text)
    , "name" .= skill.toolSkillName
    , "description" .= skill.toolSkillInstructions
    , "tools" .= map deferLoading skill.toolSkillTools
    ]
  where
    deferLoading (Object definition) = Object (KeyMap.insert "defer_loading" (Bool True) definition)
    deferLoading definition = definition

toolSkillToolNames :: ToolSkill -> [Text]
toolSkillToolNames skill = mapMaybe toolName skill.toolSkillTools
  where
    toolName (Object definition) = case KeyMap.lookup "name" definition of
        Just (String name) | not (Text.null name) -> Just name
        _ -> Nothing
    toolName _ = Nothing

-- | The tool definitions of a tool search result, for the
-- @tool_search_output@ item. Output that is not a JSON array, such as an
-- error message, loads nothing.
toolSearchOutputTools :: Text -> [Value]
toolSearchOutputTools output =
    case Aeson.decodeStrict (Text.encodeUtf8 output) of
        Just (Array tools) -> foldr (:) [] tools
        _ -> []
