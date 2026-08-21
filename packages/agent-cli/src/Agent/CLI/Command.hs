-- | Interactive REPL slash commands.
module Agent.CLI.Command
    ( ReplAction(..)
    , SlashCommand(..)
    , currentEffort
    , currentModel
    , formatSlashHelp
    , lookupSlashCommand
    , parseReplLine
    , setModel
    , setReasoningEffort
    , slashCommands
    , slashCompletionCandidates
    ) where

import Agent.CLI.Models (catalogModelIds)
import Agent.CLI.Options (parseEffort, reasoningEfforts)
import Agent.CLI.Style (roleMuted, rolePrompt)
import Agent.OpenAI.Responses.Types

import qualified Data.Aeson.KeyMap as KeyMap
import Data.Char (isSpace)
import Data.List (find, isPrefixOf)
import Data.Maybe (fromMaybe)
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
    | ReplShowSession
    | ReplLogin
    | ReplReloadAuth
    | ReplPaste
        { pasteImmediate :: !Bool
        , pasteCaption :: !Text
        }
    | ReplClearAttachments
    | ReplShowAttachments
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
    | ReplCommandError Text
    deriving (Eq, Show)

-- | One REPL slash command. @slashName@ is the canonical name without a
-- leading @/@; aliases are also stored without @/@.
data SlashCommand = SlashCommand
    { slashName :: !Text
    , slashAliases :: ![Text]
    , slashUsage :: !Text
    , slashSummary :: !Text
    }
    deriving (Eq, Show)

slashCommands :: [SlashCommand]
slashCommands =
    [ cmd "help" [] "/help [NAME]" "List slash commands, or describe one"
    , cmd "model" ["m"] "/model [NAME]" "Open the model picker, or set a model"
    , cmd "effort" [] "/effort [none|low|medium|high|xhigh|max]" "Show or set reasoning effort"
    , cmd "plan" [] "/plan [description]" "Enter plan mode (or Shift+Tab)"
    , cmd "session" [] "/session" "Print the current session id"
    , cmd "login" ["accounts"] "/login" "Manage provider credentials and usage"
    , cmd "resume" [] "/resume [ID]" "Pick a session to resume, or resume ID"
    , cmd "compact" [] "/compact [FOCUS]" "Summarize history to free context"
    , cmd "clear" [] "/clear" "Reset the live conversation (same session id)"
    , cmd "new" [] "/new" "Start a fresh persisted session id"
    , cmd "reload-auth" [] "/reload-auth" "Re-read xAI/OpenRouter credentials"
    , cmd "paste" [] "/paste [--send] [TEXT]" "Attach a clipboard image (Cmd+V / Ctrl+V) and preview it in the terminal"
    , cmd "attachments" [] "/attachments" "List queued clipboard images"
    , cmd "clear-attachments" [] "/clear-attachments" "Drop queued clipboard images"
    , cmd "always-approve" ["yolo"] "/always-approve" "Toggle project auto-approve (or Shift+Tab)"
    ]
  where
    cmd name aliases usage summary =
        SlashCommand
            { slashName = name
            , slashAliases = aliases
            , slashUsage = usage
            , slashSummary = summary
            }

lookupSlashCommand :: Text -> Maybe SlashCommand
lookupSlashCommand raw =
    let name = Text.toLower (Text.dropWhile (== '/') (Text.strip raw))
    in find (\cmd -> cmd.slashName == name || name `elem` cmd.slashAliases)
        slashCommands

parseReplLine :: Text -> ReplAction
parseReplLine raw =
    let line = Text.strip raw
    in if line == ":q" || line == ":quit"
        then ReplQuit
        else if line == ":reload"
            then ReplReload
            else case Text.uncons line of
                Just ('/', _) -> parseSlash line
                Just (':', _) -> parseColon line
                _ -> ReplPrompt line

parseColon :: Text -> ReplAction
parseColon line
    | isAlwaysApproveAlias (Text.drop 1 line) = ReplToggleAlwaysApprove
    | otherwise = ReplPrompt line

parseSlash :: Text -> ReplAction
parseSlash line = case Text.words line of
    [] -> unknownCommand "/"
    command : args -> case lookupSlashCommand command of
        Nothing -> unknownCommand command
        Just spec -> case spec.slashName of
            "help" -> parseHelpCommand args
            "effort" -> parseEffortCommand args
            "model" -> parseModelCommand args
            "plan" ->
                let description =
                        Text.strip (Text.drop (Text.length command) line)
                in ReplPlan
                    (if Text.null description then Nothing else Just description)
            "session" ->
                if null args
                    then ReplShowSession
                    else ReplCommandError "usage: /session"
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
            "always-approve" ->
                if null args
                    then ReplToggleAlwaysApprove
                    else ReplCommandError "usage: /always-approve"
            other -> unknownCommand ("/" <> other)

unknownCommand :: Text -> ReplAction
unknownCommand command =
    ReplCommandError ("unknown command: " <> command <> " (try /help)")

parseHelpCommand :: [Text] -> ReplAction
parseHelpCommand = \case
    [] -> ReplHelp Nothing
    [name] -> case lookupSlashCommand name of
        Just spec -> ReplHelp (Just spec.slashName)
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
    in ReplPaste
        { pasteImmediate = immediate
        , pasteCaption = Text.strip caption
        }

parseEffortCommand :: [Text] -> ReplAction
parseEffortCommand = \case
    [] -> ReplShowEffort
    [level] -> case parseEffort level of
        Right effort -> ReplSetEffort effort
        Left err -> ReplCommandError (Text.pack err)
    _ -> ReplCommandError "usage: /effort [none|low|medium|high|xhigh|max]"

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
formatSlashHelp color = \case
    Nothing ->
        Text.intercalate "\n" (map (formatSlashHelpRow color) slashCommands)
    Just name ->
        case lookupSlashCommand name of
            Just spec -> formatSlashHelpRow color spec
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

-- | Haskeline replacements for the word being completed.
-- @reversedPrev@ is the text before that word, reversed (haskeline's
-- 'completeWordWithPrev' convention). Empty when the buffer is not a slash
-- line.
slashCompletionCandidates :: String -> String -> [String]
slashCompletionCandidates reversedPrev word =
    let prev = reverse reversedPrev
    in if not (isSlashLine prev word)
        then []
        else case words prev of
            [] -> completeSlashNames word
            cmd : _ -> completeSlashArgs cmd word

isSlashLine :: String -> String -> Bool
isSlashLine prev word = case dropWhile isSpace prev of
    [] -> "/" `isPrefixOf` word
    rest -> "/" `isPrefixOf` rest

completeSlashNames :: String -> [String]
completeSlashNames word =
    let needle = Text.toLower (Text.dropWhile (== '/') (Text.pack word))
        names =
            concatMap
                (\cmd -> ("/" <> cmd.slashName) : map ("/" <>) cmd.slashAliases)
                slashCommands
    in filter (\name -> needle `Text.isPrefixOf` Text.drop 1 (Text.toLower (Text.pack name)))
        (map Text.unpack names)

completeSlashArgs :: String -> String -> [String]
completeSlashArgs cmd word =
    case lookupSlashCommand (Text.pack cmd) of
        Nothing -> []
        Just spec ->
            let needle = Text.toLower (Text.pack word)
                options = argCompletions spec
            in map Text.unpack $
                filter (Text.isPrefixOf needle . Text.toLower) options

argCompletions :: SlashCommand -> [Text]
argCompletions spec = case spec.slashName of
    "effort" -> reasoningEfforts
    "model" -> catalogModelIds
    "help" -> map (.slashName) slashCommands
    "paste" -> ["--send"]
    _ -> []
