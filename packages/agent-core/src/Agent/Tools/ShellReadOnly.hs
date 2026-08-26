-- | Strict observational-shell allowlist used by tool resource claims.
--
-- Unknown, mutating, redirected, piped, or compound commands stay exclusive.
-- A single @cd DIR && COMMAND@ prefix is accepted when DIR is a simple path
-- and COMMAND itself is observational.
module Agent.Tools.ShellReadOnly
    ( shellCommandIsReadOnly
    ) where

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified System.FilePath as FilePath

-- | True when the complete command is a recognized read-only invocation.
shellCommandIsReadOnly :: Text -> Bool
shellCommandIsReadOnly command =
    classifySimple $ maybe stripped id (cdAndCommand stripped)
  where
    stripped = Text.strip command

classifySimple :: Text -> Bool
classifySimple command
    | Text.null command = False
    | Text.any (`elem` forbiddenChars) command = False
    | "$(" `Text.isInfixOf` command = False
    | "`" `Text.isInfixOf` command = False
    | "$" `Text.isInfixOf` command = False
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

-- | Strip one @cd DIR && COMMAND@ prefix. Anything more compound stays
-- exclusive via 'classifySimple'.
cdAndCommand :: Text -> Maybe Text
cdAndCommand text = do
    afterCd <- Text.stripPrefix "cd" (Text.stripStart text)
    afterSpace <- case Text.uncons afterCd of
        Just (char, rest) | isSpace char ->
            Just (Text.dropWhile isSpace rest)
        _ -> Nothing
    (dir, afterDir) <- nextToken afterSpace
    afterAnd <- Text.stripPrefix "&&" (Text.dropWhile isSpace afterDir)
    let command = Text.strip afterAnd
    if simpleDirectory dir && not (Text.null command)
        then Just command
        else Nothing

nextToken :: Text -> Maybe (Text, Text)
nextToken text
    | Text.null text = Nothing
    | otherwise = case Text.uncons text of
        Just (quote, rest)
            | quote == '\'' || quote == '"' ->
                case Text.break (== quote) rest of
                    (inside, after)
                        | Text.null after -> Nothing
                        | Text.any (== '\\') inside -> Nothing
                        | otherwise -> Just (inside, Text.drop 1 after)
        _ ->
            let (token, rest) = Text.break isSpace text
            in if Text.null token then Nothing else Just (token, rest)

simpleDirectory :: Text -> Bool
simpleDirectory dir =
    not (Text.null dir)
        && not (Text.isPrefixOf "-" dir)
        && Text.all allowedDirectoryChar dir

allowedDirectoryChar :: Char -> Bool
allowedDirectoryChar char =
    char `notElem`
        ['$', '`', '\n', '\r', ';', '&', '|', '>', '<', '(', ')', '{', '}', '*', '?', '~', '!']

allowedCommand :: Text -> [Text] -> Bool
allowedCommand executable arguments
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
        case arguments of
            subcommand : _ ->
                subcommand `elem`
                    [ "diff", "log", "show", "rev-parse"
                    , "ls-files", "blame", "grep", "merge-base", "rev-list"
                    , "symbolic-ref", "ls-remote"
                    ]
                    && not (any gitMutatingFlag arguments)
            [] -> False
    | executable == "gh" =
        case arguments of
            "pr" : subcommand : _ ->
                subcommand `elem` ["view", "list", "checks", "status", "diff"]
            "repo" : "view" : _ -> True
            "run" : subcommand : _ -> subcommand `elem` ["list", "view"]
            "auth" : "status" : _ -> True
            _ -> False
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
    gitMutatingFlag argument =
        outputFlag argument
            || argument `elem`
                [ "-d", "-D", "-m", "-M", "-c", "-C"
                , "--delete", "--move", "--copy"
                ]

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
