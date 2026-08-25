-- | Presentation-neutral formatting shared by retained UI state.
module Agent.TUI.Presentation
    ( SearchReplaceAction(..)
    , SearchReplaceDiff(..)
    , SearchReplaceLine(..)
    , TodoDisplayLine(..)
    , TodoDisplayStatus(..)
    , formatSearchReplaceDiff
    , formatTodoList
    , formatToolOutput
    , liveTodoPanelLines
    , parseSearchReplaceDiff
    , parseTodoList
    , permissionToolCallPrompt
    , todoListFromToolOutput
    , summarizeToolCall
    , todoCallPreview
    , todoListHasInProgress
    , todoListHasOpenWork
    , todoStatusGlyph
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
import Control.Applicative ((<|>))
import Data.Char (isSpace)
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
    "todo_write" -> formatTodoList output
    "update_plan" -> formatTodoList output
    _ -> output

data TodoDisplayStatus
    = TodoDisplayPending
    | TodoDisplayInProgress
    | TodoDisplayCompleted
    | TodoDisplayCancelled
    deriving (Eq, Show)

data TodoDisplayLine = TodoDisplayLine
    { todoLineStatus :: !TodoDisplayStatus
    , todoLineText :: !Text
    }
    deriving (Eq, Show)

-- | Compact preview of the first todo content from a @todo_write@ /
-- @update_plan@ argument payload, used while the tool is still running.
todoCallPreview :: ToolCall -> Text
todoCallPreview call = case canonicalToolName call.name of
    "todo_write" -> firstTodoContentFromArguments call.arguments
    "update_plan" -> firstPlanStepFromArguments call.arguments
    _ -> ""

formatTodoList :: Text -> Text
formatTodoList output =
    Text.intercalate "\n" (map formatTodoDisplayLine (parseTodoList output))

liveTodoPanelLines :: Int -> [TodoDisplayLine] -> [Text]
liveTodoPanelLines maxRows todos
    | maxRows <= 0 || null todos = []
    | length todos <= maxRows =
        map formatTodoDisplayLine todos
    | otherwise =
        let shownCount = max 0 (maxRows - 1)
            shown = take shownCount todos
            hidden = length todos - length shown
        in map formatTodoDisplayLine shown
            <> ["… +" <> Text.pack (show hidden) <> " more"]

todoListHasOpenWork :: [TodoDisplayLine] -> Bool
todoListHasOpenWork =
    any \line ->
        line.todoLineStatus `elem` [TodoDisplayPending, TodoDisplayInProgress]

todoListHasInProgress :: [TodoDisplayLine] -> Bool
todoListHasInProgress =
    any \line -> line.todoLineStatus == TodoDisplayInProgress

-- | Replace the live list only when tool output is an actual checklist.
-- Unrecognized output is ignored so ordinary tools cannot clobber it.
todoListFromToolOutput :: Text -> Maybe [TodoDisplayLine]
todoListFromToolOutput output =
    let stripped = Text.strip output
        rows =
            [ Text.strip line
            | line <- Text.lines stripped
            , not (Text.null (Text.strip line))
            ]
        parsed = mapMaybe parseTodoDisplayLine rows
    in if stripped == "No tasks currently tracked."
        then Just []
        else if null parsed then Nothing else Just parsed

parseTodoList :: Text -> [TodoDisplayLine]
parseTodoList output =
    let rows =
            [ Text.strip line
            | line <- Text.lines (Text.stripEnd output)
            , not (Text.null (Text.strip line))
            ]
        parsed = mapMaybe parseTodoDisplayLine rows
    in if null parsed
        then map (TodoDisplayLine TodoDisplayPending) rows
        else parsed

parseTodoDisplayLine :: Text -> Maybe TodoDisplayLine
parseTodoDisplayLine raw =
    parseGlyphTodoLine line <|> parseMarkedTodoLine line
  where
    line = Text.strip raw

parseGlyphTodoLine :: Text -> Maybe TodoDisplayLine
parseGlyphTodoLine line =
    foldr (\(status, glyph) acc -> acc <|> prefixed status glyph) Nothing
        [ (TodoDisplayCompleted, "✓")
        , (TodoDisplayInProgress, "▶")
        , (TodoDisplayCancelled, "✗")
        , (TodoDisplayPending, "□")
        ]
  where
    prefixed status glyph = do
        rest <-
            Text.stripPrefix (glyph <> " ") line
                <|> Text.stripPrefix glyph line
        let content = Text.strip rest
        if Text.null content
            then Nothing
            else Just (TodoDisplayLine status content)

parseMarkedTodoLine :: Text -> Maybe TodoDisplayLine
parseMarkedTodoLine line =
    let body = Text.strip (Text.dropWhile (== '-') line)
        (marker, rest) = Text.break (== ']') body
    in do
        status <- parseTodoStatusMarker (Text.strip (Text.dropWhile (== '[') marker))
        rest' <- Text.stripPrefix "]" rest
        let content = stripTodoIdentifier (Text.strip rest')
        if Text.null content
            then Nothing
            else Just (TodoDisplayLine status content)

parseTodoStatusMarker :: Text -> Maybe TodoDisplayStatus
parseTodoStatusMarker = \case
    "pending" -> Just TodoDisplayPending
    "in_progress" -> Just TodoDisplayInProgress
    "completed" -> Just TodoDisplayCompleted
    "cancelled" -> Just TodoDisplayCancelled
    _ -> Nothing

stripTodoIdentifier :: Text -> Text
stripTodoIdentifier text =
    case Text.break (== ':') text of
        (before, after)
            | not (Text.null after)
            , looksLikeTodoId (Text.strip before) ->
                Text.strip (Text.drop 1 after)
        _ -> text

looksLikeTodoId :: Text -> Bool
looksLikeTodoId text =
    not (Text.null text)
        && Text.length text <= 24
        && not (Text.any isSpace text)
        && Text.all isTodoIdChar text

isTodoIdChar :: Char -> Bool
isTodoIdChar c =
    c == '-' || c == '_' || c == '.' || isAsciiAlphaNum c

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c =
    (c >= '0' && c <= '9')
        || (c >= 'A' && c <= 'Z')
        || (c >= 'a' && c <= 'z')

formatTodoDisplayLine :: TodoDisplayLine -> Text
formatTodoDisplayLine line =
    todoStatusGlyph line.todoLineStatus <> " " <> line.todoLineText

todoStatusGlyph :: TodoDisplayStatus -> Text
todoStatusGlyph = \case
    TodoDisplayPending -> "□"
    TodoDisplayInProgress -> "▶"
    TodoDisplayCompleted -> "✓"
    TodoDisplayCancelled -> "✗"

firstTodoContentFromArguments :: Text -> Text
firstTodoContentFromArguments arguments = fromMaybe "" do
    Aeson.Object object <- Aeson.decodeStrict (TextEncoding.encodeUtf8 arguments)
    Aeson.Array todos <- KeyMap.lookup "todos" object
    Aeson.Object todo <- case toList todos of
        first : _ -> Just first
        [] -> Nothing
    content <- jsonObjectText "content" todo
    pure (firstLine content)

firstPlanStepFromArguments :: Text -> Text
firstPlanStepFromArguments arguments = fromMaybe "" do
    Aeson.Object object <- Aeson.decodeStrict (TextEncoding.encodeUtf8 arguments)
    Aeson.Array plan <- KeyMap.lookup "plan" object
    Aeson.Object item <- case toList plan of
        first : _ -> Just first
        [] -> Nothing
    step <- jsonObjectText "step" item
    pure (firstLine step)

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
    "todo_write" -> "todo_write"
    "update_plan" -> "update_plan"
    "enter_plan_mode" -> "Entered"
    "exit_plan_mode" -> "Exited"
    "ask_user_question" -> "Asked"
    "ask_secret" -> "Requested secret"
    "skill_search" -> "Searched skills"
    "view_skill" -> "Viewed skill"
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
    "view_skill" -> viewSkillIdentity call.arguments
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

viewSkillIdentity :: Text -> Text
viewSkillIdentity arguments =
    case
        ( nonEmptyJsonText "scope" arguments
        , nonEmptyJsonText "name" arguments
        )
    of
        (Just scope, Just name) -> scope <> "/" <> name
        (Nothing, Just name) -> name
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
