-- | Interactive REPL slash commands.
module Agent.CLI.Command
    ( ReplAction(..)
    , ShellMode(..)
    , SkillCommand(..)
    , SlashCommand(..)
    , SlashMenu(..)
    , SlashSuggestion(..)
    , currentEffort
    , currentModel
    , formatSlashHelp
    , formatSlashHelpWithSkills
    , lookupSlashCommand
    , parseReplLine
    , parseReplLineWithSkills
    , setModel
    , setReasoningEffort
    , slashCommands
    , slashCompletionCandidates
    , slashCompletionCandidatesWithModels
    , slashCompletionCandidatesWithSkills
    , slashCompletionCandidatesWithSkillsAndModels
    , slashMenuFor
    , slashMenuForWithModels
    , slashMenuForWithSkills
    , slashMenuForWithSkillsAndModels
    ) where

import Agent.CLI.Options (parseEffort, reasoningEfforts)
import Agent.CLI.Style (roleMuted, rolePrompt)
import Agent.Responses.Types

import qualified Data.Aeson.KeyMap as KeyMap
import Data.Char (isSpace)
import Data.List (find, isPrefixOf, sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as Text

data ReplAction
    = ReplQuit
    | ReplReload
    | ReplPrompt Text
    | ReplShowEffort
    | ReplSetEffort Text
    | ReplShowModel
    | ReplSetModel Text
    | ReplToggleAlwaysApprove
    | ReplPlan (Maybe Text)
    -- ^ Enter plan mode. @Just@ starts a turn with that description.
    | ReplBtw Text
    -- ^ Ask an isolated one-shot question over the current context.
    | ReplShowSession
    | ReplWorktree
    | ReplRename Text
    | ReplRenameAuto
    | ReplLogin
    | ReplReloadAuth
    | ReplPaste !Bool !Text
    | ReplClearAttachments
    | ReplShowAttachments
    | ReplCopyLast
    | ReplCopyCode Int
    | ReplCopyDiff
    | ReplCopyPath
    | ReplCopySession
    | ReplShowTerminal
    | ReplAgents
    | ReplMcp
    | ReplSkills !Bool
    | ReplShowShell
    | ReplSetShell !ShellMode
    | ReplInvokeSkill !Text !Text
    | ReplHelp (Maybe Text)
    -- ^ @Nothing@ lists every command; @Just@ is a canonical name without @/@.
    | ReplResume (Maybe Text)
    -- ^ @Nothing@ opens the session picker; @Just@ is a session id.
    | ReplCompact (Maybe Text)
    -- ^ Optional focus note for what to keep while compacting history.
    | ReplClear
      -- ^ Soft-reset live transcript; keep the same session id.
    | ReplNew
      -- ^ Start a fresh persisted session id with empty history.
    | ReplUsage
    | ReplCommandError Text
    deriving (Eq, Show)

data ShellMode
    = ShellGhci
    | ShellBash
    | ShellBoth
    | ShellNone
    deriving (Eq, Show)

data SkillCommand = SkillCommand
    { skillCommandName :: !Text
    , skillCommandSummary :: !Text
    , skillCommandArgumentHint :: !(Maybe Text)
    , skillCommandSource :: !Text
    }
    deriving (Eq, Show)

-- | One REPL slash command. @slashName@ is the canonical name without a
-- leading @/@; aliases are also stored without @/@.
data SlashCommand = SlashCommand
    { slashName :: !Text
    , slashAliases :: ![Text]
    , slashUsage :: !Text
    , slashSummary :: !Text
    , slashTakesArguments :: !Bool
    }
    deriving (Eq, Show)

slashCommands :: [SlashCommand]
slashCommands =
    [ cmd "help" [] "/help [NAME]" "List slash commands, or describe one" True
    , cmd "model" ["m"] "/model [NAME]" "Open the model picker, or set a model" True
    , cmd "effort" [] "/effort [none|low|medium|high|xhigh|max]" "Show or set reasoning effort" True
    , cmd "plan" [] "/plan [description]" "Enter plan mode (or Shift+Tab)" True
    , cmd "btw" [] "/btw <QUESTION>" "Ask a side question without changing the conversation" True
    , cmd "session" [] "/session" "Print the current session id" False
    , cmd "worktree" [] "/worktree" "Start a fresh session in a new git worktree" False
    , cmd "rename" ["title"] "/rename <TITLE>|--auto" "Rename the current session, or restore automatic titles" True
    , cmd "login" ["accounts"] "/login" "Manage provider credentials and usage" False
    , cmd "resume" [] "/resume [ID]" "Pick a session to resume, or resume ID" True
    , cmd "compact" [] "/compact [FOCUS]" "Summarize history to free context" True
    , cmd "clear" [] "/clear" "Reset the live conversation (same session id)" False
    , cmd "new" [] "/new" "Start a fresh persisted session id" False
    , cmd "usage" [] "/usage" "Show usage, pacing, and reset times for connected accounts" False
    , cmd "reload-auth" [] "/reload-auth" "Re-read provider credentials" False
    , cmd "paste" [] "/paste [--send] [TEXT]" "Attach a clipboard image (Cmd+V / Ctrl+V) and preview it in the terminal" True
    , cmd "attachments" [] "/attachments" "List queued clipboard images" False
    , cmd "clear-attachments" [] "/clear-attachments" "Drop queued clipboard images" False
    , cmd "copy" ["copy-last"] "/copy" "Copy the last assistant response" False
    , cmd "copy-code" [] "/copy-code [N]" "Copy fenced code block N from the last response" True
    , cmd "copy-diff" [] "/copy-diff" "Copy the last diff block" False
    , cmd "copy-path" [] "/copy-path" "Copy the active worktree path" False
    , cmd "copy-session" [] "/copy-session" "Copy the current session id" False
    , cmd "terminal" ["ghostty"] "/terminal" "Show detected terminal capabilities" False
    , cmd "agents" ["a"] "/agents" "Browse the agent hierarchy and switch viewport" False
    , cmd "mcp" [] "/mcp" "Manage local MCP servers" False
    , cmd "skills" [] "/skills [reload]" "List discovered skills or reload them from disk" True
    , cmd "shell" [] "/shell [ghci|bash|both|none]" "Show or select the allowed shell tools" True
    , cmd "always-approve" ["yolo"] "/always-approve" "Toggle project auto-approve (or Shift+Tab)" False
    ]
  where
    cmd name aliases usage summary takesArguments =
        SlashCommand
            { slashName = name
            , slashAliases = aliases
            , slashUsage = usage
            , slashSummary = summary
            , slashTakesArguments = takesArguments
            }

lookupSlashCommand :: Text -> Maybe SlashCommand
lookupSlashCommand raw =
    let name = Text.toLower (Text.dropWhile (== '/') (Text.strip raw))
    in find (\cmd -> cmd.slashName == name || name `elem` cmd.slashAliases)
        slashCommands

parseReplLine :: Text -> ReplAction
parseReplLine = parseReplLineWithSkills []

parseReplLineWithSkills :: [SkillCommand] -> Text -> ReplAction
parseReplLineWithSkills skills raw =
    let line = Text.strip raw
    in if line == ":q" || line == ":quit"
        then ReplQuit
        else if line == ":reload"
            then ReplReload
            else case Text.uncons line of
                Just ('/', _) -> parseSlash skills line
                Just (':', _) -> parseColon raw
                _ -> ReplPrompt raw

parseColon :: Text -> ReplAction
parseColon raw
    | isAlwaysApproveAlias (Text.drop 1 (Text.strip raw)) = ReplToggleAlwaysApprove
    | otherwise = ReplPrompt raw

parseSlash :: [SkillCommand] -> Text -> ReplAction
parseSlash skills line = case Text.words line of
    [] -> unknownCommand "/"
    command : args -> case lookupSlashCommand command of
        Nothing -> case lookupSkillCommand skills command of
            Just skill ->
                ReplInvokeSkill
                    skill.skillCommandName
                    (Text.strip (Text.drop (Text.length command) line))
            Nothing -> unknownCommand command
        Just spec -> case spec.slashName of
            "help" -> parseHelpCommand skills args
            "effort" -> parseEffortCommand args
            "model" -> parseModelCommand args
            "plan" ->
                let description =
                        Text.strip (Text.drop (Text.length command) line)
                in ReplPlan
                    (if Text.null description then Nothing else Just description)
            "btw" ->
                let question =
                        Text.strip (Text.drop (Text.length command) line)
                in if Text.null question
                    then ReplCommandError "usage: /btw <QUESTION>"
                    else ReplBtw question
            "session" ->
                if null args
                    then ReplShowSession
                    else ReplCommandError "usage: /session"
            "worktree" ->
                if null args
                    then ReplWorktree
                    else ReplCommandError "usage: /worktree"
            "rename" ->
                let title = Text.strip (Text.drop (Text.length command) line)
                in if title == "--auto"
                    then ReplRenameAuto
                    else if Text.null title
                        then ReplCommandError "usage: /rename <TITLE>|--auto"
                        else if Text.length title > 100
                            then ReplCommandError
                                "session titles must be at most 100 characters"
                            else ReplRename title
            "login" ->
                if null args
                    then ReplLogin
                    else ReplCommandError "usage: /login"
            "resume" -> parseResumeCommand args
            "compact" ->
                let focus =
                        Text.strip (Text.drop (Text.length command) line)
                in ReplCompact
                    (if Text.null focus then Nothing else Just focus)
            "clear" ->
                if null args
                    then ReplClear
                    else ReplCommandError "usage: /clear"
            "new" ->
                if null args
                    then ReplNew
                    else ReplCommandError "usage: /new"
            "usage" ->
                if null args
                    then ReplUsage
                    else ReplCommandError "usage: /usage"
            "reload-auth" ->
                if null args
                    then ReplReloadAuth
                    else ReplCommandError "usage: /reload-auth"
            "paste" ->
                parsePasteCommand (Text.strip (Text.drop (Text.length command) line))
            "attachments" ->
                if null args
                    then ReplShowAttachments
                    else ReplCommandError "usage: /attachments"
            "clear-attachments" ->
                if null args
                    then ReplClearAttachments
                    else ReplCommandError "usage: /clear-attachments"
            "copy" ->
                if null args
                    then ReplCopyLast
                    else ReplCommandError "usage: /copy"
            "copy-code" -> parseCopyCodeCommand args
            "copy-diff" ->
                if null args
                    then ReplCopyDiff
                    else ReplCommandError "usage: /copy-diff"
            "copy-path" ->
                if null args
                    then ReplCopyPath
                    else ReplCommandError "usage: /copy-path"
            "copy-session" ->
                if null args
                    then ReplCopySession
                    else ReplCommandError "usage: /copy-session"
            "terminal" ->
                if null args
                    then ReplShowTerminal
                    else ReplCommandError "usage: /terminal"
            "agents" ->
                if null args
                    then ReplAgents
                    else ReplCommandError "usage: /agents"
            "mcp" ->
                if null args
                    then ReplMcp
                    else ReplCommandError "usage: /mcp"
            "skills" -> case args of
                [] -> ReplSkills False
                ["reload"] -> ReplSkills True
                _ -> ReplCommandError "usage: /skills [reload]"
            "shell" -> parseShellCommand args
            "always-approve" ->
                if null args
                    then ReplToggleAlwaysApprove
                    else ReplCommandError "usage: /always-approve"
            other -> unknownCommand ("/" <> other)

unknownCommand :: Text -> ReplAction
unknownCommand command =
    ReplCommandError ("unknown command: " <> command <> " (try /help)")

lookupSkillCommand :: [SkillCommand] -> Text -> Maybe SkillCommand
lookupSkillCommand skills raw =
    let name = Text.toLower (Text.dropWhile (== '/') (Text.strip raw))
    in find ((== name) . Text.toLower . (.skillCommandName)) skills

parseHelpCommand :: [SkillCommand] -> [Text] -> ReplAction
parseHelpCommand skills = \case
    [] -> ReplHelp Nothing
    [name] -> case lookupSlashCommand name of
        Just spec -> ReplHelp (Just spec.slashName)
        Nothing -> case lookupSkillCommand skills name of
            Just skill -> ReplHelp (Just skill.skillCommandName)
            Nothing -> unknownCommand name
    _ -> ReplCommandError "usage: /help [NAME]"

parseResumeCommand :: [Text] -> ReplAction
parseResumeCommand = \case
    [] -> ReplResume Nothing
    [sessionId]
        | Text.null (Text.strip sessionId) ->
            ReplCommandError "usage: /resume [ID]"
        | otherwise -> ReplResume (Just sessionId)
    _ -> ReplCommandError "usage: /resume [ID]"

parseCopyCodeCommand :: [Text] -> ReplAction
parseCopyCodeCommand = \case
    [] -> ReplCopyCode 1
    [raw] -> case reads (Text.unpack raw) of
        [(n, "")] | n > 0 -> ReplCopyCode n
        _ -> ReplCommandError "usage: /copy-code [N]"
    _ -> ReplCommandError "usage: /copy-code [N]"

isAlwaysApproveAlias :: Text -> Bool
isAlwaysApproveAlias name =
    Text.toLower name `elem` ["always-approve", "yolo"]

-- | @/paste@ queues a clipboard image on the next prompt.
-- @/paste --send [caption]@ sends immediately (old behavior).
parsePasteCommand :: Text -> ReplAction
parsePasteCommand rest =
    let (immediate, caption) = case Text.words rest of
            ("--send":xs) -> (True, Text.unwords xs)
            ("-s":xs) -> (True, Text.unwords xs)
            _ -> (False, rest)
    in ReplPaste immediate (Text.strip caption)

parseEffortCommand :: [Text] -> ReplAction
parseEffortCommand = \case
    [] -> ReplShowEffort
    [level] -> case parseEffort level of
        Right effort -> ReplSetEffort effort
        Left err -> ReplCommandError (Text.pack err)
    _ -> ReplCommandError "usage: /effort [none|low|medium|high|xhigh|max]"

parseShellCommand :: [Text] -> ReplAction
parseShellCommand = \case
    [] -> ReplShowShell
    [raw] -> case Text.toLower raw of
        "ghci" -> ReplSetShell ShellGhci
        "bash" -> ReplSetShell ShellBash
        "both" -> ReplSetShell ShellBoth
        "none" -> ReplSetShell ShellNone
        _ -> ReplCommandError "usage: /shell [ghci|bash|both|none]"
    _ -> ReplCommandError "usage: /shell [ghci|bash|both|none]"

parseModelCommand :: [Text] -> ReplAction
parseModelCommand = \case
    [] -> ReplShowModel
    [name]
        | Text.null (Text.strip name) ->
            ReplCommandError "usage: /model [NAME]"
        | otherwise -> ReplSetModel name
    _ -> ReplCommandError "usage: /model [NAME]"

-- | Rebuild from the constructor: 'input' is also a field on 'CustomToolCall'.
setReasoningEffort :: Text -> ResponseCreateParams -> ResponseCreateParams
setReasoningEffort level ResponseCreateParams{..} =
    ResponseCreateParams
        { reasoning = Just updated
        , ..
        }
  where
    updated = case reasoning of
        Just ReasoningConfig{..} -> ReasoningConfig { effort = Just level, .. }
        Nothing -> ReasoningConfig
            { context = Nothing
            , effort = Just level
            , generateSummary = Nothing
            , reasoningMode = Nothing
            , summary = Nothing
            , extraFields = KeyMap.empty
            }

currentEffort :: ResponseCreateParams -> Text
currentEffort params =
    fromMaybe "low" (params.reasoning >>= (.effort))

-- | Rebuild from the constructor: 'input' is also a field on 'CustomToolCall'.
setModel :: Text -> ResponseCreateParams -> ResponseCreateParams
setModel name ResponseCreateParams{..} =
    ResponseCreateParams
        { model = Just name
        , ..
        }

currentModel :: ResponseCreateParams -> Text
currentModel params =
    fromMaybe "(unset)" params.model

-- | Help text for @/help@ / @/help NAME@.
formatSlashHelp :: Bool -> Maybe Text -> Text
formatSlashHelp color = formatSlashHelpWithSkills color []

formatSlashHelpWithSkills :: Bool -> [SkillCommand] -> Maybe Text -> Text
formatSlashHelpWithSkills color skills = \case
    Nothing ->
        Text.intercalate "\n"
            (map (formatSlashHelpRow color) slashCommands
                <> map (formatSkillHelpRow color) skills)
    Just name ->
        case lookupSlashCommand name of
            Just spec -> formatSlashHelpRow color spec
            Nothing -> case lookupSkillCommand skills name of
                Just skill -> formatSkillHelpRow color skill
                Nothing -> roleMuted color ("unknown command: " <> name <> " (try /help)")

formatSlashHelpRow :: Bool -> SlashCommand -> Text
formatSlashHelpRow color spec =
    let aliases =
            if null spec.slashAliases
                then ""
                else
                    " ("
                        <> Text.intercalate ", "
                            (map ("/" <>) spec.slashAliases)
                        <> ")"
    in rolePrompt color spec.slashUsage
        <> aliases
        <> "\n  "
        <> roleMuted color spec.slashSummary

formatSkillHelpRow :: Bool -> SkillCommand -> Text
formatSkillHelpRow color skill =
    let usage =
            "/"
                <> skill.skillCommandName
                <> maybe "" (" " <>) skill.skillCommandArgumentHint
    in rolePrompt color usage
        <> "\n  "
        <> roleMuted color
            (skill.skillCommandSummary <> " · skill · " <> skill.skillCommandSource)

-- | Haskeline replacements for the word being completed.
-- @reversedPrev@ is the text before that word, reversed (haskeline's
-- 'completeWordWithPrev' convention). Empty when the buffer is not a slash
-- line.
slashCompletionCandidates :: String -> String -> [String]
slashCompletionCandidates = slashCompletionCandidatesWithSkillsAndModels [] []

slashCompletionCandidatesWithModels
    :: [Text]
    -> String
    -> String
    -> [String]
slashCompletionCandidatesWithModels =
    slashCompletionCandidatesWithSkillsAndModels []

slashCompletionCandidatesWithSkills
    :: [SkillCommand]
    -> String
    -> String
    -> [String]
slashCompletionCandidatesWithSkills skills =
    slashCompletionCandidatesWithSkillsAndModels skills []

slashCompletionCandidatesWithSkillsAndModels
    :: [SkillCommand]
    -> [Text]
    -> String
    -> String
    -> [String]
slashCompletionCandidatesWithSkillsAndModels
        skills modelIds reversedPrev word =
    let prev = reverse reversedPrev
    in if not (isSlashLine prev word)
        then []
        else case words prev of
            [] -> completeSlashNames skills word
            cmd : _ -> completeSlashArgs modelIds cmd word

isSlashLine :: String -> String -> Bool
isSlashLine prev word = case dropWhile isSpace prev of
    [] -> "/" `isPrefixOf` word
    rest -> "/" `isPrefixOf` rest

completeSlashNames :: [SkillCommand] -> String -> [String]
completeSlashNames skills word =
    let needle = Text.toLower (Text.dropWhile (== '/') (Text.pack word))
        names =
            concatMap
                (\cmd -> ("/" <> cmd.slashName) : map ("/" <>) cmd.slashAliases)
                slashCommands
        skillNames = map (("/" <>) . (.skillCommandName)) skills
    in filter (\name -> needle `Text.isPrefixOf` Text.drop 1 (Text.toLower (Text.pack name)))
        (map Text.unpack (names <> skillNames))

completeSlashArgs :: [Text] -> String -> String -> [String]
completeSlashArgs modelIds cmd word =
    case lookupSlashCommand (Text.pack cmd) of
        Nothing -> []
        Just spec ->
            let needle = Text.toLower (Text.pack word)
                options = argCompletions modelIds spec
            in map Text.unpack $
                filter (Text.isPrefixOf needle . Text.toLower) options

argCompletions :: [Text] -> SlashCommand -> [Text]
argCompletions modelIds spec = case spec.slashName of
    "effort" -> reasoningEfforts
    "model" -> modelIds
    "shell" -> ["ghci", "bash", "both", "none"]
    "help" -> map (.slashName) slashCommands
    "rename" -> ["--auto"]
    "paste" -> ["--send"]
    _ -> []

-- | One row in the live slash-command dropdown.
data SlashSuggestion = SlashSuggestion
    { slashSuggestionDisplay :: !Text
    , slashSuggestionReplacement :: !Text
    , slashSuggestionSummary :: !Text
    , slashSuggestionTakesArguments :: !Bool
    , slashSuggestionMatchPositions :: ![Int]
    }
    deriving (Eq, Show)

-- | Current live slash menu and the character range replaced on acceptance.
data SlashMenu = SlashMenu
    { slashMenuReplaceStart :: !Int
    , slashMenuReplaceEnd :: !Int
    , slashMenuSuggestions :: ![SlashSuggestion]
    }
    deriving (Eq, Show)

-- | Derive a live menu from a leading slash command at the cursor.
slashMenuFor :: Text -> Int -> Maybe SlashMenu
slashMenuFor = slashMenuForWithSkillsAndModels [] []

slashMenuForWithModels :: [Text] -> Text -> Int -> Maybe SlashMenu
slashMenuForWithModels = slashMenuForWithSkillsAndModels []

slashMenuForWithSkills :: [SkillCommand] -> Text -> Int -> Maybe SlashMenu
slashMenuForWithSkills skills =
    slashMenuForWithSkillsAndModels skills []

slashMenuForWithSkillsAndModels
    :: [SkillCommand]
    -> [Text]
    -> Text
    -> Int
    -> Maybe SlashMenu
slashMenuForWithSkillsAndModels skills modelIds text cursor
    | cursor < 1 || not (Text.isPrefixOf "/" text) = Nothing
    | otherwise =
        let commandToken = Text.takeWhile (not . isSpace) text
            commandEnd = Text.length commandToken
        in if cursor <= commandEnd
            then commandMenu skills (Text.take cursor text) commandEnd
            else argumentMenu modelIds commandToken commandEnd text cursor

commandMenu :: [SkillCommand] -> Text -> Int -> Maybe SlashMenu
commandMenu skills token replaceEnd =
    let query = Text.toLower (Text.drop 1 token)
        commands = slashCommands <> map skillAsSlashCommand skills
        scored = mapMaybe (scoreCommand query) (zip [0 :: Int ..] commands)
        ordered
            | Text.null query = scored
            | otherwise = sortOn (\(score, order, _, _) -> (Down score, order)) scored
        rows =
            [ SlashSuggestion
                { slashSuggestionDisplay = "/" <> command.slashName
                , slashSuggestionReplacement =
                    "/" <> command.slashName
                        <> if command.slashTakesArguments then " " else ""
                , slashSuggestionSummary = command.slashSummary
                , slashSuggestionTakesArguments = command.slashTakesArguments
                , slashSuggestionMatchPositions = map (+ 1) positions
                }
            | (_, _, command, positions) <- ordered
            ]
    in if Text.any (== '/') query || null rows
        then Nothing
        else Just SlashMenu
            { slashMenuReplaceStart = 0
            , slashMenuReplaceEnd = replaceEnd
            , slashMenuSuggestions = rows
            }

skillAsSlashCommand :: SkillCommand -> SlashCommand
skillAsSlashCommand skill =
    SlashCommand
        { slashName = skill.skillCommandName
        , slashAliases = []
        , slashUsage =
            "/"
                <> skill.skillCommandName
                <> maybe "" (" " <>) skill.skillCommandArgumentHint
        , slashSummary =
            skill.skillCommandSummary <> " · skill · " <> skill.skillCommandSource
        , slashTakesArguments = True
        }

scoreCommand
    :: Text
    -> (Int, SlashCommand)
    -> Maybe (Int, Int, SlashCommand, [Int])
scoreCommand query (order, command)
    | Text.null query = Just (0, order, command, [])
    | otherwise =
        case sortOn (Down . fst) $
            mapMaybe (fuzzyMatch query . Text.toLower)
                (command.slashName : command.slashAliases) of
            [] -> Nothing
            (score, positions) : _ ->
                Just (score, order, command, positions)

argumentMenu :: [Text] -> Text -> Int -> Text -> Int -> Maybe SlashMenu
argumentMenu modelIds commandToken commandEnd text cursor = do
    command <- lookupSlashCommand commandToken
    let before = Text.take cursor text
        suffix = Text.takeWhileEnd (not . isSpace) before
        argStart = Text.length before - Text.length suffix
        tokenEnd =
            cursor
                + Text.length
                    (Text.takeWhile (not . isSpace) (Text.drop cursor text))
        precedingArgs =
            Text.words
                (Text.take (argStart - commandEnd) (Text.drop commandEnd text))
        options
            | null precedingArgs = argCompletions modelIds command
            | otherwise = []
        query = Text.toLower suffix
        ordered = sortOn (\(score, option, _) -> (Down score, option))
            [ (score, option, positions)
            | option <- options
            , Just (score, positions) <- [fuzzyMatch query (Text.toLower option)]
            ]
        rows =
            [ SlashSuggestion
                { slashSuggestionDisplay = option
                , slashSuggestionReplacement = option
                , slashSuggestionSummary = ""
                , slashSuggestionTakesArguments = False
                , slashSuggestionMatchPositions = positions
                }
            | (_, option, positions) <- ordered
            ]
    if null rows
        then Nothing
        else Just SlashMenu
            { slashMenuReplaceStart = argStart
            , slashMenuReplaceEnd = tokenEnd
            , slashMenuSuggestions = rows
            }

-- | Small deterministic fuzzy matcher for the short command catalog.
fuzzyMatch :: Text -> Text -> Maybe (Int, [Int])
fuzzyMatch needle haystack
    | Text.null needle = Just (0, [])
    | needle == haystack =
        Just (10000, [0 .. Text.length needle - 1])
    | needle `Text.isPrefixOf` haystack =
        Just (8000 - Text.length haystack, [0 .. Text.length needle - 1])
    | otherwise = do
        positions@(firstPos:_) <- subsequencePositions needle haystack
        lastPos <- safeLast positions
        let
            gaps = lastPos - firstPos + 1 - length positions
            boundaryBonus =
                sum
                    [ if pos == 0 || Text.index haystack (pos - 1) == '-'
                        then 40
                        else 0
                    | pos <- positions
                    ]
        pure
            ( 4000
                + boundaryBonus
                - firstPos * 10
                - gaps * 20
                - Text.length haystack
            , positions
            )
  where
    safeLast = \case
        [] -> Nothing
        first : rest -> Just (foldl (\_ item -> item) first rest)

subsequencePositions :: Text -> Text -> Maybe [Int]
subsequencePositions needle haystack =
    go 0 (Text.unpack needle) (Text.unpack haystack)
  where
    go _ [] _ = Just []
    go _ _ [] = Nothing
    go index wanted@(n:ns) (h:hs)
        | n == h = (index :) <$> go (index + 1) ns hs
        | otherwise = go (index + 1) wanted hs
