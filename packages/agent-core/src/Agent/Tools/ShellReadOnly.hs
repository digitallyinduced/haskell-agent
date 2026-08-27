-- | Strict observational-shell allowlist used by tool resource claims.
--
-- Unknown, mutating, or redirected commands stay exclusive. A command may be
-- a chain of observational segments joined by @&&@, @;@, or @|@. A segment
-- may be @cd DIR@ with a simple path.
module Agent.Tools.ShellReadOnly
    ( shellCommandIsReadOnly
    ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified System.FilePath as FilePath

-- | True when the complete command is a recognized read-only invocation.
shellCommandIsReadOnly :: Text -> Bool
shellCommandIsReadOnly command =
    case observationalSegments (Text.strip command) of
        Just segments@(_ : _) -> all classifySimple segments
        _ -> False

observationalSegments :: Text -> Maybe [Text]
observationalSegments command
    | Text.null command = Nothing
    | Text.any (`elem` ['\n', '\r', '>', '<']) command = Nothing
    | "$(" `Text.isInfixOf` command = Nothing
    | "`" `Text.isInfixOf` command = Nothing
    | "$" `Text.isInfixOf` command = Nothing
    | "||" `Text.isInfixOf` command = Nothing
    | hasBareAmpersand command = Nothing
    | otherwise =
        let segments =
                filter (not . Text.null) $
                    map Text.strip $
                        concatMap (Text.splitOn "|") $
                            concatMap (Text.splitOn "&&") $
                                Text.splitOn ";" command
        in if null segments then Nothing else Just segments

hasBareAmpersand :: Text -> Bool
hasBareAmpersand = go
  where
    go text
        | Text.null text = False
        | "&&" `Text.isPrefixOf` text = go (Text.drop 2 text)
        | Text.head text == '&' = True
        | otherwise = go (Text.drop 1 text)

classifySimple :: Text -> Bool
classifySimple command
    | Text.null command = False
    | Text.any (`elem` forbiddenChars) command = False
    | otherwise =
        case Text.words command of
            [] -> False
            executable : arguments ->
                allowedCommand
                    (Text.pack
                        (FilePath.takeFileName (Text.unpack executable)))
                    arguments
  where
    forbiddenChars = ['\n', '\r', ';', '&', '|', '>', '<']

allowedCommand :: Text -> [Text] -> Bool
allowedCommand executable arguments
    | executable == "cd" =
        case arguments of
            [dir] -> simpleDirectory (unquote dir)
            _ -> False
    | executable `elem`
        [ "cat", "head", "tail", "grep", "rg", "fd", "ls"
        , "pwd", "wc", "stat", "file", "jq", "sort", "cut"
        , "tr", "realpath", "basename", "dirname", "which", "du", "df"
        , "nl", "ps", "test", "diff", "printf", "echo"
        ] =
        not (any mutatingFlag arguments)
            && not (executable == "sort" && any outputFlag arguments)
    | executable == "uniq" =
        not (any mutatingFlag arguments)
            && not (uniqWritesOutput arguments)
    | executable == "find" =
        not (any findAction arguments)
    | executable == "sed" =
        safeSed arguments
    | executable == "git" =
        allowedGit arguments
    | executable == "gh" =
        allowedGh arguments
    | otherwise = False
  where
    mutatingFlag argument =
        argument `elem` ["-i", "--in-place", "--delete", "-delete"]
    outputFlag argument =
        argument == "-o"
            || "--output" `Text.isPrefixOf` argument
    findAction argument =
        argument `elem`
            [ "-delete", "-exec", "-execdir", "-ok", "-okdir"
            , "-fprint", "-fprintf", "-fls"
            ]

allowedGit :: [Text] -> Bool
allowedGit arguments =
    case dropGitGlobals arguments of
        [] -> False
        subcommand : rest ->
            subcommand `elem` gitReadOnlySubcommands
                && not (any gitOutputFlag rest)
                && not (subcommand == "branch" && any branchMutatingFlag rest)
  where
    gitOutputFlag argument =
        argument == "-o"
            || "--output" `Text.isPrefixOf` argument
    branchMutatingFlag argument =
        argument `elem`
            [ "-d", "-D", "-m", "-M", "--delete", "--move", "--copy" ]

gitReadOnlySubcommands :: [Text]
gitReadOnlySubcommands =
    [ "diff", "log", "show", "rev-parse", "ls-files", "blame", "grep"
    , "merge-base", "rev-list", "symbolic-ref", "ls-remote", "status"
    , "branch", "describe", "cat-file", "ls-tree", "version", "help"
    , "for-each-ref", "name-rev", "shortlog"
    ]

dropGitGlobals :: [Text] -> [Text]
dropGitGlobals = \case
    "-C" : _ : rest -> dropGitGlobals rest
    "-c" : _ : rest -> dropGitGlobals rest
    rest -> rest

allowedGh :: [Text] -> Bool
allowedGh = \case
    "pr" : subcommand : _ ->
        subcommand `elem` ["view", "list", "checks", "status", "diff"]
    "issue" : subcommand : _ ->
        subcommand `elem` ["view", "list"]
    "repo" : "view" : _ -> True
    "run" : subcommand : _ -> subcommand `elem` ["list", "view"]
    "auth" : "status" : _ -> True
    _ -> False

simpleDirectory :: Text -> Bool
simpleDirectory dir =
    not (Text.null dir)
        && not (Text.isPrefixOf "-" dir)
        && Text.all allowedDirectoryChar dir

allowedDirectoryChar :: Char -> Bool
allowedDirectoryChar char =
    char `notElem`
        ['$', '`', '\n', '\r', ';', '&', '|', '>', '<', '(', ')', '{', '}', '*', '?', '~', '!']

unquote :: Text -> Text
unquote text =
    case Text.uncons text of
        Just (quote, rest)
            | quote == '\'' || quote == '"'
            , not (Text.null rest)
            , Text.last rest == quote ->
                Text.dropEnd 1 rest
        _ -> text

-- | GNU uniq writes a second positional operand: @uniq [OPTION]... [INPUT [OUTPUT]]@.
uniqWritesOutput :: [Text] -> Bool
uniqWritesOutput arguments =
    length positional >= 2
  where
    positional =
        filter
            (\argument -> argument /= "--" && not ("-" `Text.isPrefixOf` argument))
            arguments

safeSed :: [Text] -> Bool
safeSed = \case
    "-n" : script : paths ->
        safePrintScript script
            && not (null paths)
            && all (not . Text.isPrefixOf "-") paths
    _ -> False
  where
    safePrintScript raw =
        let script = Text.dropAround (`elem` ['\'', '"']) raw
            withoutDigits = Text.filter
                (\char -> char < '0' || char > '9')
                script
        in withoutDigits `elem` ["p", ",p"]
