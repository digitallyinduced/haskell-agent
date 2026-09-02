-- | Presentation-neutral formatting shared by retained UI state.
module Agent.TUI.Presentation
    ( SearchReplaceAction(..)
    , SearchReplaceDiff(..)
    , SearchReplaceLine(..)
    , DiffDisplayLine(..)
    , DiffLineKind(..)
    , TodoDisplayLine(..)
    , TodoDisplayStatus(..)
    , formatSearchReplaceDiff
    , formatSearchReplaceDiffRelative
    , formatToolDiffRelative
    , formatToolDiffRelativeWithOutput
    , formatTodoList
    , formatToolOutput
    , formatToolOutputRelative
    , diffHeaderParts
    , isInspectionTool
    , liveTodoPanelLines
    , parseApplyPatchDiffs
    , parseDiffDisplayLine
    , parseSearchReplaceDiff
    , parseWriteFileDiff
    , parseTodoList
    , permissionToolCallPrompt
    , permissionToolCallPromptRelative
    , todoListFromToolArguments
    , todoListFromToolOutput
    , summarizeToolCall
    , summarizeToolCallRelative
    , todoCallPreview
    , todoListHasInProgress
    , todoListHasOpenWork
    , todoStatusGlyph
    , toolCallInput
    , toolCallHeaderRelative
    , toolCallTitle
    , toolCallTitleRelative
    , toolCallDiff
    , toolCallDiffs
    , toolDetail
    , toolOutputCodeLanguage
    , toolPathArgument
    , toolVerb
    , workspaceRelativeDisplayPath
    ) where

import Agent.JsonText (jsonTextField, jsonTextFieldDefault)
import qualified Agent.Json.Decode as Hermes
import Agent.OsPath (fromText, relativeDisplayPath)
import Agent.TUI.TextWidth (displayTerminalText)
import Agent.ToolDispatch
    ( ToolCall(..)
    , canonicalToolName
    )
import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Char (isDigit, isSpace)
import qualified Data.Foldable as Foldable
import Data.List (sortOn)
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

summarizeToolCall :: ToolCall -> Text
summarizeToolCall = summarizeToolCallRelative ""

-- | Like 'summarizeToolCall', but filesystem paths inside the workspace are
-- shown relative to that workspace.
summarizeToolCallRelative :: Text -> ToolCall -> Text
summarizeToolCallRelative workspace call =
    let verb = case canonicalToolName call.name of
            "mcp_call" -> mcpCallDisplayName call.arguments
            "use_tool" -> mcpCallDisplayName call.arguments
            _ -> toolVerb call.name
        detail = case toolPathArgument call of
            Just path -> workspaceRelativeDisplayPath workspace path
            Nothing -> toolDetail call
    in if Text.null detail then verb else verb <> " " <> detail

-- | Complete question shown before a mutating tool call is approved.
-- Command-like tools include their full input: a first-line activity summary
-- is not enough for multiline @do@ blocks or shell scripts.
permissionToolCallPrompt :: ToolCall -> Text
permissionToolCallPrompt = permissionToolCallPromptRelative ""

permissionToolCallPromptRelative :: Text -> ToolCall -> Text
permissionToolCallPromptRelative workspace call =
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
        _ -> "Allow " <> summarizeToolCallRelative workspace call <> "?"
  where
    detailedPrompt question input
        | Text.null (Text.strip input) = question
        | otherwise = question <> "\n\n" <> input

-- | Compact heading for a retained tool block. GHCi expressions are rendered
-- separately as code, so keeping them out of the heading avoids an unbounded
-- single terminal row and leaves a useful activity label.
toolCallTitle :: ToolCall -> Text
toolCallTitle = toolCallTitleRelative ""

toolCallTitleRelative :: Text -> ToolCall -> Text
toolCallTitleRelative workspace call
    | canonicalToolName call.name == "run_ghci" = "$ ghci"
    | canonicalToolName call.name == "exec" = "$ exec"
    | otherwise = summarizeToolCallRelative workspace call

-- | Separate a filesystem action from its path so renderers can style the
-- path without guessing from the human-readable title. Non-filesystem calls
-- retain their complete title and have no separate header detail.
toolCallHeaderRelative :: Text -> ToolCall -> (Text, Maybe Text)
toolCallHeaderRelative workspace call =
    case toolPathArgument call of
        Just path ->
            ( toolVerb call.name
            , Just (workspaceRelativeDisplayPath workspace path)
            )
        Nothing -> (toolCallTitleRelative workspace call, Nothing)

-- | Full invocation text that benefits from dedicated code rendering.
toolCallInput :: ToolCall -> Text
toolCallInput call = case canonicalToolName call.name of
    "run_ghci" -> jsonTextFieldDefault "expression" call.arguments
    "exec" -> call.arguments
    _ -> ""

-- Computer-call arguments can contain secrets in @type@ and @keypress@
-- actions. Keep approval/activity chrome structural: report action kinds and
-- text lengths, never the text or complete JSON payload.
computerActionDetail :: Text -> Text
computerActionDetail _ = "computer action"

data SearchReplaceAction
    = SearchReplaceCreate
    | SearchReplaceDelete
    -- | Whole-file write where the previous contents are unknown.
    | SearchReplaceWrite
    -- | One file in a potentially multi-file patch was updated.
    | SearchReplaceUpdate
    -- | A patch moved a file to the given destination.
    | SearchReplaceMove !Text
    deriving (Eq, Show)

data SearchReplaceLine
    = SearchReplaceRemoved !Text
    | SearchReplaceAdded !Text
    | SearchReplaceContext !Text
    deriving (Eq, Show)

data DiffLineKind
    = DiffLineRemoved
    | DiffLineAdded
    | DiffLineContext
    deriving (Eq, Show)

data DiffDisplayLine = DiffDisplayLine
    { diffDisplayGutter :: !Text
    , diffDisplayMarker :: !Text
    , diffDisplayCode :: !Text
    , diffDisplayKind :: !DiffLineKind
    }
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

-- | Preview for Claude Code's @Write@ tool: the new file contents as added
-- lines. Whether the path already exists is unknown here, so the header
-- says @write@ rather than @create@.
parseWriteFileDiff :: Text -> SearchReplaceDiff
parseWriteFileDiff arguments =
    let path = jsonTextFieldDefault "file_path" arguments
        content = jsonTextFieldDefault "content" arguments
        raw = map SearchReplaceAdded (Text.lines content)
        shown = take 20 raw
    in SearchReplaceDiff
        { diffPath = path
        , diffAction = Just SearchReplaceWrite
        , diffLines = shown
        , diffHiddenLines = length raw - length shown
        }

-- | Compact previews parsed from Codex's freeform @apply_patch@ language.
-- The execution parser lives in the Codex dialect package, which depends on
-- this presentation package, so this deliberately permissive parser only
-- extracts file boundaries and changed lines for display.
parseApplyPatchDiffs :: Text -> [SearchReplaceDiff]
parseApplyPatchDiffs patch =
    case Text.lines (Text.strip patch) of
        first : rest
            | Text.strip first == "*** Begin Patch" ->
                suppressFirstUpdateHeader (parseHunks (dropEnvironment rest))
        _ -> []
  where
    dropEnvironment (line : rest)
        | "*** Environment ID:" `Text.isPrefixOf` Text.strip line = rest
    dropEnvironment lines_ = lines_

    parseHunks [] = []
    parseHunks (line : rest)
        | Just path <- patchPath "*** Add File: " line =
            let (body, remaining) = span (not . patchBoundary) rest
            in makeDiff path (Just SearchReplaceCreate)
                    (mapMaybe addedLine body)
                : parseHunks remaining
        | Just path <- patchPath "*** Delete File: " line =
            makeDiff path (Just SearchReplaceDelete) []
                : parseHunks rest
        | Just path <- patchPath "*** Update File: " line =
            let (body, remaining) = span (not . patchBoundary) rest
                (action, changes) = case body of
                    moveLine : more
                        | Just destination <-
                            patchPath "*** Move to: " moveLine ->
                                (Just (SearchReplaceMove destination), more)
                    _ -> (Just SearchReplaceUpdate, body)
            in makeDiff path action (mapMaybe changedLine changes)
                : parseHunks remaining
        | otherwise = parseHunks rest

    suppressFirstUpdateHeader = \case
        diff : rest
            | diff.diffAction == Just SearchReplaceUpdate ->
                diff { diffAction = Nothing } : rest
        diffs -> diffs

    makeDiff path action raw =
        let shown = cappedPatchPreview 20 raw
        in SearchReplaceDiff
            { diffPath = path
            , diffAction = action
            , diffLines = shown
            , diffHiddenLines = length raw - length shown
            }

    -- Prefer actual changes when long context runs exceed the preview budget.
    -- Remaining slots show the context closest to those changes, in source
    -- order, so an approval preview cannot consist entirely of unchanged rows.
    cappedPatchPreview limit raw
        | length raw <= limit = raw
        | null changed = take limit raw
        | otherwise =
            [ line
            | (index, line) <- indexed
            , index `elem` selectedIndices
            ]
      where
        indexed = zip [0 :: Int ..] raw
        changed =
            filter (isChanged . snd) indexed
        shownChanges = take limit changed
        changedIndices = map fst shownChanges
        contextBudget = limit - length shownChanges
        contextByProximity =
            sortOn
                (\(index, _) ->
                    ( minimum
                        (map (abs . (index -)) changedIndices)
                    , index
                    ))
                (filter (not . isChanged . snd) indexed)
        selectedIndices =
            changedIndices
                <> map fst (take contextBudget contextByProximity)

    isChanged = \case
        SearchReplaceContext _ -> False
        SearchReplaceRemoved _ -> True
        SearchReplaceAdded _ -> True

    patchBoundary line =
        any (`Text.isPrefixOf` line)
            [ "*** Add File: "
            , "*** Delete File: "
            , "*** Update File: "
            , "*** End Patch"
            ]

    patchPath prefix line =
        nonEmptyText =<< Text.stripPrefix prefix line

    addedLine line = case Text.uncons line of
        Just ('+', text) -> Just (SearchReplaceAdded text)
        _ -> Nothing

    changedLine line = case Text.uncons line of
        Just ('-', text) -> Just (SearchReplaceRemoved text)
        Just ('+', text) -> Just (SearchReplaceAdded text)
        Just (' ', text) -> Just (SearchReplaceContext text)
        _ -> Nothing

    nonEmptyText text =
        let stripped = Text.strip text
        in if Text.null stripped then Nothing else Just stripped

-- | Diff previews carried by a tool call's arguments.
toolCallDiffs :: ToolCall -> [SearchReplaceDiff]
toolCallDiffs call = case canonicalToolName call.name of
    "search_replace" -> [parseSearchReplaceDiff call.arguments]
    "apply_patch" -> parseApplyPatchDiffs call.arguments
    "Write" -> [parseWriteFileDiff call.arguments]
    _ -> []

-- | First diff preview carried by a tool call, retained for callers that only
-- need to identify whether a preview exists.
toolCallDiff :: ToolCall -> Maybe SearchReplaceDiff
toolCallDiff = listToMaybe . toolCallDiffs

formatSearchReplaceDiff :: Text -> Text
formatSearchReplaceDiff = formatSearchReplaceDiffRelative ""

formatSearchReplaceDiffRelative :: Text -> Text -> Text
formatSearchReplaceDiffRelative workspace arguments =
    formatDiffRelative workspace (parseSearchReplaceDiff arguments)

-- | Diff body for a tool block; empty when the tool carries no diff.
formatToolDiffRelative :: Text -> ToolCall -> Text
formatToolDiffRelative workspace call =
    Text.intercalate "\n" $
        map (formatDiffRelative workspace) (toolCallDiffs call)

-- | Completed diff body, enriched with exact source location metadata emitted
-- by edit tools. While a tool is running we deliberately keep the preview
-- unnumbered rather than guessing where a search string will match.
formatToolDiffRelativeWithOutput :: Text -> ToolCall -> Text -> Text
formatToolDiffRelativeWithOutput workspace call output =
    Text.intercalate "\n" $
        map (formatDiffRelativeAt workspace lineStart) (toolCallDiffs call)
  where
    lineStart
        | canonicalToolName call.name == "search_replace" =
            searchReplaceLineStart output
        | otherwise = Nothing

formatDiffRelative :: Text -> SearchReplaceDiff -> Text
formatDiffRelative workspace = formatDiffRelativeAt workspace Nothing

formatDiffRelativeAt :: Text -> Maybe Int -> SearchReplaceDiff -> Text
formatDiffRelativeAt workspace reportedStart diff =
    let SearchReplaceDiff { diffPath, diffAction, diffLines, diffHiddenLines } =
            diff
        displayedPath = workspaceRelativeDisplayPath workspace diffPath
        header = case diffAction of
            Just SearchReplaceCreate -> "  create " <> displayedPath
            Just SearchReplaceDelete -> "  delete " <> displayedPath
            Just SearchReplaceWrite -> "  write " <> displayedPath
            Just SearchReplaceUpdate -> "  update " <> displayedPath
            Just (SearchReplaceMove destination) ->
                "  move "
                    <> displayedPath
                    <> " → "
                    <> workspaceRelativeDisplayPath workspace destination
            Nothing -> ""
        lineStart = reportedStart <|> actionLineStart diffAction
        shown = formatDisplayLines lineStart diffLines
        more
            | diffHiddenLines == 0 = []
            | otherwise =
                ["  … " <> Text.pack (show diffHiddenLines) <> " more"]
    in Text.intercalate "\n" (filter (not . Text.null) (header : shown <> more))
  where
    formatDisplayLines start lines_ =
        let numbered = numberLines start start lines_
            largest =
                maximum
                    (1 :
                        [ number
                        | (oldLine, newLine, _) <- numbered
                        , number <- maybeToList oldLine <> maybeToList newLine
                        ])
            width = max 3 (length (show largest))
        in map (formatNumberedLine width) numbered

    numberLines
        :: Maybe Int
        -> Maybe Int
        -> [SearchReplaceLine]
        -> [(Maybe Int, Maybe Int, SearchReplaceLine)]
    numberLines _ _ [] = []
    numberLines oldLine newLine (line : rest) =
        case line of
            SearchReplaceRemoved _ ->
                (oldLine, Nothing, line)
                    : numberLines (advance oldLine) newLine rest
            SearchReplaceAdded _ ->
                (Nothing, newLine, line)
                    : numberLines oldLine (advance newLine) rest
            SearchReplaceContext _ ->
                (oldLine, newLine, line)
                    : numberLines
                        (advance oldLine)
                        (advance newLine)
                        rest

    advance = fmap (+ 1)

    formatNumberedLine
        :: Int
        -> (Maybe Int, Maybe Int, SearchReplaceLine)
        -> Text
    formatNumberedLine width (oldLine, newLine, line) =
        "  "
            <> numberColumn width oldLine
            <> " "
            <> numberColumn width newLine
            <> " │ "
            <> case line of
                SearchReplaceRemoved text -> "-" <> text
                SearchReplaceAdded text -> "+" <> text
                SearchReplaceContext text -> " " <> text

    numberColumn :: Int -> Maybe Int -> Text
    numberColumn width =
        Text.justifyRight width ' ' . maybe "" (Text.pack . show)

    actionLineStart = \case
        Just SearchReplaceCreate -> Just 1
        Just SearchReplaceWrite -> Just 1
        _ -> Nothing

-- | Split a formatted multi-file header into its semantic action and path.
diffHeaderParts :: Text -> Maybe (Text, Text)
diffHeaderParts line =
    firstMatch
        [ "create"
        , "delete"
        , "write"
        , "update"
        , "move"
        ]
  where
    firstMatch [] = Nothing
    firstMatch (action : rest) =
        case Text.stripPrefix ("  " <> action <> " ") line of
            Just path
                | not (Text.null (Text.strip path)) ->
                    Just (action, path)
            _ -> firstMatch rest

-- | Decode the stable line-number gutter emitted by 'formatDiffRelative'.
parseDiffDisplayLine :: Text -> Maybe DiffDisplayLine
parseDiffDisplayLine line =
    let (gutter, separatorAndPayload) = Text.breakOn " │ " line
    in do
        payload <- Text.stripPrefix " │ " separatorAndPayload
        (marker, code) <- Text.uncons payload
        kind <- case marker of
            '-' -> Just DiffLineRemoved
            '+' -> Just DiffLineAdded
            ' ' -> Just DiffLineContext
            _ -> Nothing
        pure DiffDisplayLine
            { diffDisplayGutter = gutter
            , diffDisplayMarker = Text.singleton marker
            , diffDisplayCode = code
            , diffDisplayKind = kind
            }

searchReplaceLineStart :: Text -> Maybe Int
searchReplaceLineStart output =
    case Text.breakOn marker output of
        (_, rest)
            | Text.null rest -> Nothing
            | otherwise ->
                let digits = Text.takeWhile isDigit (Text.drop (Text.length marker) rest)
                in case reads (Text.unpack digits) of
                    [(lineNumber, "")] | lineNumber > 0 -> Just lineNumber
                    _ -> Nothing
  where
    marker = "Changed lines start at "

formatToolOutput :: ToolCall -> Text -> Text
formatToolOutput call output = case canonicalToolName call.name of
    "computer" -> "Screenshot captured"
    "exec" -> completedExecOutput output
    "mcp_call" -> formatMcpOutput output
    "use_tool" -> formatMcpOutput output
    name | name `elem` ["spawn_agent", "spawn_agent_in_worktree"] ->
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

-- | MCP servers commonly return a compact @CallToolResult@ envelope whose
-- text block is itself JSON. Show the useful text payload rather than escaped
-- transport JSON when every content block is textual, then indent JSON values.
-- This is presentation-only; the canonical tool result remains unchanged.
formatMcpOutput :: Text -> Text
formatMcpOutput output =
    case decodeJsonValue output of
        Just value ->
            case mcpTextBlocks value of
                Just texts | not (null texts) ->
                    Text.intercalate "\n" (map beautifyJson texts)
                _ -> prettyJsonValue value
        Nothing -> output

beautifyJson :: Text -> Text
beautifyJson text =
    case decodeJsonValue text of
        Just value@Aeson.Object{} -> prettyJsonValue value
        Just value@Aeson.Array{} -> prettyJsonValue value
        _ -> text

-- | Select syntax highlighting for structured tool output. Primitive JSON
-- values stay as ordinary text; objects and arrays benefit from JSON token
-- colors without changing the retained transcript body.
toolOutputCodeLanguage :: Text -> Maybe Text
toolOutputCodeLanguage text =
    case decodeJsonValue text of
        Just Aeson.Object{} -> Just "json"
        Just Aeson.Array{} -> Just "json"
        _ -> Nothing

decodeJsonValue :: Text -> Maybe Aeson.Value
decodeJsonValue =
    Aeson.decodeStrict' . TextEncoding.encodeUtf8 . Text.strip

prettyJsonValue :: Aeson.Value -> Text
prettyJsonValue =
    TextEncoding.decodeUtf8
        . LazyByteString.toStrict
        . AesonPretty.encodePretty

mcpTextBlocks :: Aeson.Value -> Maybe [Text]
mcpTextBlocks (Aeson.Object object) = do
    Aeson.Array blocks <- KeyMap.lookup "content" object
    traverse textBlock (Foldable.toList blocks)
  where
    textBlock (Aeson.Object block) = do
        Aeson.String text <- KeyMap.lookup "text" block
        pure text
    textBlock _ = Nothing
mcpTextBlocks _ = Nothing

-- The exec protocol keeps status and timing metadata for the model. In the
-- transcript, the invocation itself already communicates successful
-- completion, so retain only the script's meaningful output.
completedExecOutput :: Text -> Text
completedExecOutput output
    | "Script completed\n" `Text.isPrefixOf` output =
        case Text.breakOn "\nOutput:\n" output of
            (_, rest)
                | Text.null rest -> output
                | otherwise -> Text.drop (Text.length "\nOutput:\n") rest
    | otherwise = output

-- | Rewrite workspace-absolute filesystem paths in tool chrome/output without
-- touching file contents returned by @read_file@.
formatToolOutputRelative :: Text -> ToolCall -> Text -> Text
formatToolOutputRelative workspace call output =
    formatToolOutput call $
        if shouldRelativizeToolOutput call
            then rewriteToolPathInText workspace call output
            else output

shouldRelativizeToolOutput :: ToolCall -> Bool
shouldRelativizeToolOutput call =
    canonicalToolName call.name
        `elem` ["search_replace", "list_dir", "apply_patch"]

rewriteToolPathInText :: Text -> ToolCall -> Text -> Text
rewriteToolPathInText workspace call output =
    case toolPathArgument call of
        Just path ->
            let displayed = workspaceRelativeDisplayPath workspace path
            in if path == displayed
                then output
                else Text.replace path displayed output
        Nothing -> output

-- | Show @path@ relative to @workspace@ when it is inside that tree.
-- Already-relative paths are rewritten through the workspace so @src/../a@
-- becomes @a@. Paths outside the workspace stay absolute after normalization.
workspaceRelativeDisplayPath :: Text -> Text -> Text
workspaceRelativeDisplayPath workspace path =
    relativeDisplayPath (fromText workspace) (fromText path)

-- | Filesystem path argument used in tool chrome, when the tool has one.
toolPathArgument :: ToolCall -> Maybe Text
toolPathArgument call =
    nonEmptyPath $ case canonicalToolName call.name of
        "read_file" -> readFilePath call.arguments
        "list_dir" -> jsonTextFieldDefault "target_directory" call.arguments
        "search_replace" -> jsonTextFieldDefault "file_path" call.arguments
        "apply_patch" -> fromMaybe "" (firstPatchPath call.arguments)
        "Write" -> jsonTextFieldDefault "file_path" call.arguments
        "NotebookEdit" -> jsonTextFieldDefault "notebook_path" call.arguments
        _ -> ""
  where
    nonEmptyPath text =
        let stripped = Text.strip text
        in if Text.null stripped then Nothing else Just stripped

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

-- | Checklist carried by a @todo_write@-shaped argument payload. Claude
-- Code's @TodoWrite@ answers with prose, so its list is only visible here.
todoListFromToolArguments :: Text -> Maybe [TodoDisplayLine]
todoListFromToolArguments arguments =
    case decodeMaybe todosDecoder arguments of
        Just (Just todos) -> Just (mapMaybe id todos)
        _ -> Nothing
  where
    todosDecoder =
        Hermes.object $
            Hermes.atKeyOptional "todos" (Hermes.list todoDecoder)
    todoDecoder =
        Hermes.getType >>= \case
            Hermes.VObject ->
                Hermes.object do
                    content <- Hermes.atKeyOptional "content" Hermes.text
                    status <- Hermes.atKeyOptional "status" Hermes.text
                    pure do
                        text <- Text.strip <$> content
                        if Text.null text
                            then Nothing
                            else Just TodoDisplayLine
                                { todoLineStatus =
                                    fromMaybe
                                        TodoDisplayPending
                                        (status >>= parseTodoStatusMarker)
                                , todoLineText = text
                                }
            _ -> pure Nothing

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
firstTodoContentFromArguments arguments =
    fromMaybe "" (decodeMaybe firstTodoContentDecoder arguments)
  where
    firstTodoContentDecoder =
        Hermes.object do
            todos <- Hermes.atKeyOptional "todos" $
                Hermes.list todoDecoder
            pure $ case todos >>= listToMaybe of
                Just (Just content) -> firstLine content
                _ -> ""
    todoDecoder =
        Hermes.getType >>= \case
            Hermes.VObject ->
                Hermes.object (Hermes.atKeyOptional "content" Hermes.text)
            _ -> pure Nothing

firstPlanStepFromArguments :: Text -> Text
firstPlanStepFromArguments arguments =
    fromMaybe "" (decodeMaybe firstPlanStepDecoder arguments)
  where
    firstPlanStepDecoder =
        Hermes.object do
            plan <- Hermes.atKeyOptional "plan" $
                Hermes.list planDecoder
            pure $ case plan >>= listToMaybe of
                Just (Just step) -> firstLine step
                _ -> ""
    planDecoder =
        Hermes.getType >>= \case
            Hermes.VObject ->
                Hermes.object (Hermes.atKeyOptional "step" Hermes.text)
            _ -> pure Nothing

-- | Read-only discovery and inspection tools use an outline marker in the
-- activity chrome. Keep this list explicit: an unknown or generic MCP call
-- may mutate external state and therefore remains an action.
isInspectionTool :: Text -> Bool
isInspectionTool rawName =
    canonicalToolName rawName
        `elem`
            [ "read_file"
            , "list_dir"
            , "grep"
            , "get_task_output"
            , "read_tool_output"
            , "search_tool_output"
            , "view_image"
            , "mcp_search"
            , "search_tool"
            , "mcp_list_resources"
            , "mcp_read_resource"
            , "database_schema"
            , "database_query"
            , "conversation_search"
            , "skill_search"
            , "view_skill"
            , "read_agent_session"
            , "list_agents"
            , "Glob"
            , "WebFetch"
            , "WebSearch"
            , "ToolSearch"
            , "ListAgents"
            ]

toolVerb :: Text -> Text
toolVerb name = case canonicalToolName name of
    "computer" -> "Control computer"
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
    "spawn_agent_in_worktree" -> "Spawned worktree agent"
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
    -- Claude Code built-ins without a host equivalent keep their wire names
    -- as identity; the equivalents are mapped by 'canonicalToolName'.
    "Write" -> "Wrote"
    "Glob" -> "Globbed"
    "WebFetch" -> "Fetched"
    "WebSearch" -> "Searched web"
    "ToolSearch" -> "Searched tools"
    "Agent" -> "Spawned agent"
    "Task" -> "Spawned agent"
    "NotebookEdit" -> "Edited"
    "Monitor" -> "Monitored"
    "Skill" -> "Ran skill"
    "EnterWorktree" -> "Entered worktree"
    "ExitWorktree" -> "Exited worktree"
    "SendMessage" -> "Sent message to"
    "ListAgents" -> "Listed agents"
    other -> mcpToolDisplayName other

-- | Claude Code names MCP tools @mcp__server__tool@; show @server: tool@.
mcpToolDisplayName :: Text -> Text
mcpToolDisplayName name =
    case Text.stripPrefix "mcp__" name of
        Just rest
            | (server, tool) <- Text.breakOn "__" rest
            , not (Text.null server)
            , Just toolName <- Text.stripPrefix "__" tool
            , not (Text.null toolName) ->
                server <> ": " <> toolName
        _ -> name

-- | The generic MCP wrapper keeps the selected tool in its arguments. Display
-- that identity as @server: tool@ instead of the unhelpful @mcp_call@ name.
mcpCallDisplayName :: Text -> Text
mcpCallDisplayName arguments =
    case nonEmptyJsonText "name" arguments
        <|> nonEmptyJsonText "tool_name" arguments of
        Just qualifiedName -> qualifiedMcpDisplayName qualifiedName
        Nothing -> "MCP call"

qualifiedMcpDisplayName :: Text -> Text
qualifiedMcpDisplayName name =
    case Text.breakOn "__" name of
        (server, rest)
            | not (Text.null server)
            , Just tool <- Text.stripPrefix "__" rest
            , not (Text.null tool) ->
                unescape server <> ": " <> unescape tool
        _ -> name
  where
    unescape =
        Text.replace "%25" "%"
            . Text.replace "%5F%5F" "__"

toolDetail :: ToolCall -> Text
toolDetail call = case canonicalToolName call.name of
    "computer" -> computerActionDetail call.arguments
    "read_file" -> readFilePath call.arguments
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
    "spawn_agent_in_worktree" ->
        jsonTextFieldDefault "task_name" call.arguments
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
    "get_task_output" -> jsonTextFieldDefault "task_id" call.arguments
    "kill_task" -> jsonTextFieldDefault "task_id" call.arguments
    "Write" -> jsonTextFieldDefault "file_path" call.arguments
    "Glob" -> jsonTextFieldDefault "pattern" call.arguments
    "WebFetch" -> jsonTextFieldDefault "url" call.arguments
    "WebSearch" -> firstLine (jsonTextFieldDefault "query" call.arguments)
    "ToolSearch" -> firstLine (jsonTextFieldDefault "query" call.arguments)
    "Agent" -> firstLine (jsonTextFieldDefault "description" call.arguments)
    "Task" -> firstLine (jsonTextFieldDefault "description" call.arguments)
    "NotebookEdit" -> jsonTextFieldDefault "notebook_path" call.arguments
    "Monitor" -> firstLine (jsonTextFieldDefault "command" call.arguments)
    "Skill" -> firstLine (jsonTextFieldDefault "skill" call.arguments)
    "SendMessage" -> jsonTextFieldDefault "to" call.arguments
    _ -> ""

-- | Host @read_file@ takes @target_file@; Claude Code's @Read@ shares the
-- canonical name but passes @file_path@.
readFilePath :: Text -> Text
readFilePath arguments =
    case nonEmptyJsonText "target_file" arguments of
        Just path -> path
        Nothing -> jsonTextFieldDefault "file_path" arguments

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
    (scope, slug, revision, activation) <- decodeMaybe skillMutationDecoder output
    pure $ scope <> "/" <> slug <> " · revision " <> Text.pack (show revision)
        <> maybe "" (" · " <>) activation
  where
    skillMutationDecoder =
        Hermes.object do
            skill <- Hermes.atKeyOptional "skill" $
                Hermes.object do
                    scope <- Hermes.atKeyOptional "scope" Hermes.text
                    slug <- Hermes.atKeyOptional "slug" Hermes.text
                    revision <- Hermes.atKeyOptional "revision" Hermes.int
                    activation <- Hermes.atKeyOptional "activation" Hermes.text
                    pure (scope, slug, revision, activation)
            case skill of
                Just (Just scope, Just slug, Just revision, activation) ->
                    pure (scope, slug, revision, activation)
                _ -> fail "missing skill fields"

nonEmptyJsonText :: Text -> Text -> Maybe Text
nonEmptyJsonText key input = jsonTextField key input >>= \value ->
    let stripped = Text.strip value
    in if Text.null stripped then Nothing else Just stripped

jsonIntField :: Text -> Text -> Maybe Text
jsonIntField key input =
    fmap (Text.pack . show) $
        decodeMaybe (Hermes.object (Hermes.atKeyOptional key Hermes.int)) input
            >>= id

formatAgentList :: Text -> Maybe Text
formatAgentList output = do
    agents <- decodeMaybe
        (Hermes.object $ Hermes.atKeyOptional "agents" $
            Hermes.list agentRowDecoder)
        output
    let rows = mapMaybe id (fromMaybe [] agents)
    pure $ case rows of
        [] -> "(no live agents)"
        _ -> Text.intercalate "\n" rows

  where
    agentRowDecoder =
        Hermes.getType >>= \case
            Hermes.VObject ->
                Hermes.object do
                    name <- Hermes.atKeyOptional "agent_name" Hermes.text
                    status <- Hermes.atKeyOptional "agent_status" Hermes.text
                    pure $ case name of
                        Just value
                            | not (Text.null (Text.strip value)) ->
                                Just $ maybe (Text.strip value)
                                    (\s -> Text.strip value <> " · " <> Text.strip s)
                                    status
                        _ -> Nothing
            _ -> pure Nothing

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
        _ -> fromMaybe "" (decodeMaybe questionDetailDecoder arguments)
  where
    questionDetailDecoder =
        Hermes.object do
            questions <- Hermes.atKeyOptional "questions" $
                Hermes.list questionDecoder
            pure $ case questions >>= listToMaybe of
                Just (Just question) -> firstLine question
                _ -> ""
    questionDecoder =
        Hermes.getType >>= \case
            Hermes.VObject ->
                Hermes.object (Hermes.atKeyOptional "question" Hermes.text)
            _ -> pure Nothing

decodeMaybe :: Hermes.Decoder a -> Text -> Maybe a
decodeMaybe decoder input =
    either (const Nothing) Just (Hermes.decodeText decoder input)

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : _) = Just x
