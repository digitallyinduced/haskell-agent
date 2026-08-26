-- | Strict observational-shell allowlist used by tool resource claims.
--
-- Unknown, mutating, redirected, piped, or compound commands stay exclusive.
module Agent.Tools.ShellReadOnly
    ( shellCommandIsReadOnly
    ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified System.FilePath as FilePath

-- | True when the complete command is a recognized read-only invocation.
shellCommandIsReadOnly :: Text -> Bool
shellCommandIsReadOnly command
    | Text.null stripped = False
    | Text.any (`elem` forbiddenChars) stripped = False
    | "$(" `Text.isInfixOf` stripped = False
    | "`" `Text.isInfixOf` stripped = False
    | "$" `Text.isInfixOf` stripped = False
    | otherwise =
        case Text.words stripped of
            [] -> False
            executable : arguments ->
                allowedCommand
                    (Text.pack
                        (FilePath.takeFileName (Text.unpack executable)))
                    arguments
  where
    stripped = Text.strip command
    forbiddenChars = ['\n', '\r', ';', '&', '|', '>', '<']

allowedCommand :: Text -> [Text] -> Bool
allowedCommand executable arguments
    | executable `elem`
        [ "cat", "head", "tail", "grep", "rg", "fd", "ls"
        , "pwd", "wc", "stat", "file", "jq", "sort", "uniq", "cut"
        , "tr", "realpath", "basename", "dirname", "which", "du", "df"
        , "nl", "ps", "test", "diff", "printf", "echo"
        ] =
        not (any mutatingFlag arguments)
            && not (executable == "sort" && any outputFlag arguments)
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
