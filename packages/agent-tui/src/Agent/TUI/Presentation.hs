-- | Presentation-neutral formatting shared by retained UI state.
module Agent.TUI.Presentation
    ( SearchReplaceAction(..)
    , SearchReplaceDiff(..)
    , SearchReplaceLine(..)
    , formatSearchReplaceDiff
    , formatToolOutput
    , parseSearchReplaceDiff
    , permissionToolCallPrompt
    , summarizeToolCall
    , toolCallInput
    , toolCallTitle
    , toolDetail
    ) where

import Agent.JsonText (jsonTextField, jsonTextFieldDefault)
import Agent.TUI.TextWidth (displayTerminalText)
import Agent.ToolDispatch
    ( ToolCall(..)
    , canonicalToolName
    )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

summarizeToolCall :: ToolCall -> Text
summarizeToolCall call =
    let verb = toolVerb call.name
        detail = toolDetail call
    in if Text.null detail then verb else verb <> " " <> detail

-- | Complete question shown before a mutating tool call is approved.
-- Command-like tools include their full input: a first-line activity summary
-- is not enough for multiline @do@ blocks or shell scripts.
permissionToolCallPrompt :: ToolCall -> Text
permissionToolCallPrompt call =
    displayTerminalText case canonicalToolName call.name of
        "run_ghci" ->
            detailedPrompt
                "Evaluate this Haskell code in GHCi?"
                (jsonTextFieldDefault "expression" call.arguments)
        "run_terminal_cmd" ->
            detailedPrompt
                "Run this shell command?"
                (jsonTextFieldDefault "command" call.arguments)
        "shell_command" ->
            detailedPrompt
                "Run this shell command?"
                (jsonTextFieldDefault "command" call.arguments)
        "skill_create" ->
            "Create learned skill " <> skillIdentity call.arguments <> "?"
        "skill_update" ->
            "Update learned skill " <> skillIdentity call.arguments <> "?"
        "skill_archive" ->
            "Archive learned skill " <> skillIdentity call.arguments <> "?"
        "skill_rollback" ->
            "Restore learned skill " <> skillIdentity call.arguments <> "?"
        _ -> "Allow " <> summarizeToolCall call <> "?"
  where
    detailedPrompt question input
        | Text.null (Text.strip input) = question
        | otherwise = question <> "\n\n" <> input

-- | Compact heading for a retained tool block. GHCi expressions are rendered
-- separately as code, so keeping them out of the heading avoids an unbounded
-- single terminal row and leaves a useful activity label.
toolCallTitle :: ToolCall -> Text
toolCallTitle call
    | canonicalToolName call.name == "run_ghci" = "$ ghci"
    | otherwise = summarizeToolCall call

-- | Full invocation text that benefits from dedicated code rendering.
toolCallInput :: ToolCall -> Text
toolCallInput call = case canonicalToolName call.name of
    "run_ghci" -> jsonTextFieldDefault "expression" call.arguments
    _ -> ""

data SearchReplaceAction
    = SearchReplaceCreate
    | SearchReplaceDelete
    deriving (Eq, Show)

data SearchReplaceLine
    = SearchReplaceRemoved !Text
    | SearchReplaceAdded !Text
    deriving (Eq, Show)

data SearchReplaceDiff = SearchReplaceDiff
    { diffPath :: !Text
    , diffAction :: !(Maybe SearchReplaceAction)
    , diffLines :: ![SearchReplaceLine]
    , diffHiddenLines :: !Int
    }
    deriving (Eq, Show)

parseSearchReplaceDiff :: Text -> SearchReplaceDiff
parseSearchReplaceDiff arguments =
    let path = jsonTextFieldDefault "file_path" arguments
        oldText = jsonTextFieldDefault "old_string" arguments
        newText = jsonTextFieldDefault "new_string" arguments
        action = case (Text.null oldText, Text.null newText) of
            (True, False) -> Just SearchReplaceCreate
            (False, True) -> Just SearchReplaceDelete
            _ -> Nothing
        raw =
            map SearchReplaceRemoved (Text.lines oldText)
                <> map SearchReplaceAdded (Text.lines newText)
        shown = take 20 raw
    in SearchReplaceDiff
        { diffPath = path
        , diffAction = action
        , diffLines = shown
        , diffHiddenLines = length raw - length shown
        }

formatSearchReplaceDiff :: Text -> Text
formatSearchReplaceDiff arguments =
    let SearchReplaceDiff { diffPath, diffAction, diffLines, diffHiddenLines } =
            parseSearchReplaceDiff arguments
        header = case diffAction of
            Just SearchReplaceCreate -> "  create " <> diffPath
            Just SearchReplaceDelete -> "  delete " <> diffPath
            _ -> ""
        shown = map formatLine diffLines
        more
            | diffHiddenLines == 0 = []
            | otherwise =
                ["  … " <> Text.pack (show diffHiddenLines) <> " more"]
    in Text.intercalate "\n" (filter (not . Text.null) (header : shown <> more))
  where
    formatLine = \case
        SearchReplaceRemoved line -> "  -" <> line
        SearchReplaceAdded line -> "  +" <> line

formatToolOutput :: ToolCall -> Text -> Text
formatToolOutput call output = case canonicalToolName call.name of
    "spawn_agent" ->
        maybe output ("Agent: " <>) (nonEmptyJsonText "task_name" output)
    "wait_agent" ->
        fromMaybe output (nonEmptyJsonText "message" output)
    "list_agents" ->
        fromMaybe output (formatAgentList output)
    "interrupt_agent" ->
        maybe output ("Previous status: " <>)
            (nonEmptyJsonText "previous_status" output)
    "skill_create" -> fromMaybe output (formatSkillMutation output)
    "skill_update" -> fromMaybe output (formatSkillMutation output)
    "skill_archive" -> fromMaybe output (formatSkillMutation output)
    "skill_rollback" -> fromMaybe output (formatSkillMutation output)
    _ -> output

toolVerb :: Text -> Text
toolVerb name = case canonicalToolName name of
    "read_file" -> "Read"
    "list_dir" -> "Listed"
    "grep" -> "Searched"
    "search_replace" -> "Edited"
    "apply_patch" -> "Edited"
    "run_terminal_cmd" -> "$"
    "shell_command" -> "$"
    "write_stdin" -> "Continued"
    "run_ghci" -> "$"
    "get_task_output" -> "Read"
    "wait_tasks" -> "Waited"
    "kill_task" -> "Killed"
    "task" -> "Ran"
    "spawn_agent" -> "Spawned agent"
    "wait_agent" -> "Waited for agent updates"
    "send_message" -> "Sent message to"
    "followup_task" -> "Followed up with"
    "list_agents" -> "Listed agents"
    "interrupt_agent" -> "Interrupted"
    "create_agent_session" -> "Created agent session"
    "read_agent_session" -> "Read agent session"
    "send_agent_session_message" -> "Messaged agent session"
    "update_plan" -> "Updated"
    "enter_plan_mode" -> "Entered"
    "exit_plan_mode" -> "Exited"
    "ask_user_question" -> "Asked"
    "ask_secret" -> "Requested secret"
    "skill_search" -> "Searched skills"
    "skill_read" -> "Read skill"
    "skill_create" -> "Learned"
    "skill_update" -> "Updated skill"
    "skill_archive" -> "Archived skill"
    "skill_rollback" -> "Restored skill"
    "conversation_search" -> "Searched conversations"
    other -> other

toolDetail :: ToolCall -> Text
toolDetail call = case canonicalToolName call.name of
    "read_file" -> jsonTextFieldDefault "target_file" call.arguments
    "list_dir" -> jsonTextFieldDefault "target_directory" call.arguments
    "search_replace" -> jsonTextFieldDefault "file_path" call.arguments
    "grep" -> jsonTextFieldDefault "pattern" call.arguments
    "run_terminal_cmd" -> firstLine (jsonTextFieldDefault "command" call.arguments)
    "run_ghci" -> firstLine (jsonTextFieldDefault "expression" call.arguments)
    "shell_command" -> firstLine (jsonTextFieldDefault "command" call.arguments)
    "write_stdin" ->
        maybe "" ("session " <>) (jsonIntField "session_id" call.arguments)
    "apply_patch" -> fromMaybe "patch" (firstPatchPath call.arguments)
    "update_plan" -> "plan"
    "enter_plan_mode" -> "enter"
    "exit_plan_mode" -> "exit"
    "ask_user_question" -> askUserQuestionDetail call.arguments
    "ask_secret" ->
        firstLine $
            let purpose = jsonTextFieldDefault "purpose" call.arguments
            in if Text.null (Text.strip purpose)
                then jsonTextFieldDefault "prompt" call.arguments
                else purpose
    "spawn_agent" -> jsonTextFieldDefault "task_name" call.arguments
    "send_message" -> jsonTextFieldDefault "target" call.arguments
    "followup_task" -> jsonTextFieldDefault "target" call.arguments
    "interrupt_agent" -> jsonTextFieldDefault "target" call.arguments
    "create_agent_session" ->
        firstLine (jsonTextFieldDefault "title" call.arguments)
    "read_agent_session" ->
        jsonTextFieldDefault "session_id" call.arguments
    "send_agent_session_message" ->
        jsonTextFieldDefault "session_id" call.arguments
    "list_agents" ->
        maybe "" ("under " <>) (nonEmptyJsonText "path_prefix" call.arguments)
    "skill_search" -> firstLine (jsonTextFieldDefault "query" call.arguments)
    "skill_read" -> skillIdentity call.arguments
    "skill_create" -> skillIdentity call.arguments
    "skill_update" -> skillIdentity call.arguments
    "skill_archive" -> skillIdentity call.arguments
    "skill_rollback" -> skillIdentity call.arguments
    "conversation_search" ->
        firstLine (jsonTextFieldDefault "query" call.arguments)
    _ -> ""

skillIdentity :: Text -> Text
skillIdentity arguments =
    case
        ( nonEmptyJsonText "scope" arguments
        , nonEmptyJsonText "slug" arguments
        )
    of
        (Just scope, Just slug) -> scope <> "/" <> slug
        (Nothing, Just slug) -> slug
        _ -> "the selected skill"

formatSkillMutation :: Text -> Maybe Text
formatSkillMutation output = do
    Aeson.Object envelope <- Aeson.decodeStrict (TextEncoding.encodeUtf8 output)
    Aeson.Object skill <- KeyMap.lookup "skill" envelope
    scope <- jsonObjectText "scope" skill
    slug <- jsonObjectText "slug" skill
    revision <- jsonObjectInteger "revision" skill
    let activation = maybe "" (" · " <>) (jsonObjectText "activation" skill)
    pure $
        scope
            <> "/"
            <> slug
            <> " · revision "
            <> revision
            <> activation

nonEmptyJsonText :: Text -> Text -> Maybe Text
nonEmptyJsonText key input = jsonTextField key input >>= \value ->
    let stripped = Text.strip value
    in if Text.null stripped then Nothing else Just stripped

jsonIntField :: Text -> Text -> Maybe Text
jsonIntField key input = do
    Aeson.Object object <- Aeson.decodeStrict (TextEncoding.encodeUtf8 input)
    field <- KeyMap.lookup (Key.fromText key) object
    case Aeson.fromJSON field :: Aeson.Result Int of
        Aeson.Success value -> pure (Text.pack (show value))
        Aeson.Error _ -> Nothing

formatAgentList :: Text -> Maybe Text
formatAgentList output = do
    Aeson.Object object <- Aeson.decodeStrict (TextEncoding.encodeUtf8 output)
    Aeson.Array agents <- KeyMap.lookup (Key.fromText "agents") object
    let rows = mapMaybe formatAgentRow (toList agents)
    pure $ case rows of
        [] -> "(no live agents)"
        _ -> Text.intercalate "\n" rows

formatAgentRow :: Aeson.Value -> Maybe Text
formatAgentRow (Aeson.Object agent) = do
    name <- jsonObjectText "agent_name" agent
    pure $ maybe name (\status -> name <> " · " <> status)
        (jsonObjectText "agent_status" agent)
formatAgentRow _ = Nothing

jsonObjectText :: Text -> Aeson.Object -> Maybe Text
jsonObjectText key object =
    case KeyMap.lookup (Key.fromText key) object of
        Just (Aeson.String value)
            | not (Text.null (Text.strip value)) -> Just (Text.strip value)
        _ -> Nothing

jsonObjectInteger :: Text -> Aeson.Object -> Maybe Text
jsonObjectInteger key object = do
    value <- KeyMap.lookup (Key.fromText key) object
    case Aeson.fromJSON value :: Aeson.Result Integer of
        Aeson.Success integer -> pure (Text.pack (show integer))
        Aeson.Error _ -> Nothing

firstPatchPath :: Text -> Maybe Text
firstPatchPath patch =
    case
        [ Text.drop (Text.length prefix) line
        | line <- Text.lines patch
        , prefix <- ["*** Add File: ", "*** Update File: ", "*** Delete File: "]
        , prefix `Text.isPrefixOf` line
        ] of
        path : _ | not (Text.null path) -> Just path
        _ -> Nothing

firstLine :: Text -> Text
firstLine = Text.takeWhile (/= '\n')

askUserQuestionDetail :: Text -> Text
askUserQuestionDetail arguments =
    case firstLine (jsonTextFieldDefault "question" arguments) of
        legacy | not (Text.null legacy) -> legacy
        _ -> fromMaybe "" do
            Aeson.Object object <-
                Aeson.decodeStrict (TextEncoding.encodeUtf8 arguments)
            Aeson.Array questions <- KeyMap.lookup "questions" object
            Aeson.Object questionObject <- case toList questions of
                question : _ -> Just question
                [] -> Nothing
            Aeson.String question <- KeyMap.lookup "question" questionObject
            pure (firstLine question)
