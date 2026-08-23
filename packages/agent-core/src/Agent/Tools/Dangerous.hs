-- | Hard-deny patterns for shell tools, even under auto-approve / yolo.
--
-- Inspired by Grok Build's "always-approve + deny rules" and Codex's forbidden
-- exec-policy prefixes. First cut blocks recursive force deletes.
module Agent.Tools.Dangerous
    ( shellCommandBlocked
    , commandLooksLikeRmRf
    , forbiddenRmRfReason
    ) where

import Agent.JsonText (jsonTextField)
import Data.Char (isAlphaNum, toLower)
import Data.Text (Text)
import qualified Data.Text as Text

-- | If @toolName@ is a shell tool and @argumentsJson@ contains a forbidden
-- command, return a rejection message for the model. Otherwise 'Nothing'.
shellCommandBlocked :: Text -> Text -> Maybe Text
shellCommandBlocked toolName argumentsJson
    | toolName `elem`
        ["run_terminal_cmd", "run_terminal_command", "shell_command"] =
        case jsonTextField "command" argumentsJson of
            Nothing -> Nothing
            Just command
                | commandLooksLikeRmRf command ->
                    Just (forbiddenRmRfReason command)
                | otherwise -> Nothing
    | otherwise = Nothing

forbiddenRmRfReason :: Text -> Text
forbiddenRmRfReason command =
    "Blocked dangerous shell command (rm -rf / recursive force delete). "
        <> "Remove files more narrowly, or ask the user to run the delete outside the agent. "
        <> "Command: "
        <> Text.take 200 (Text.strip command)

-- | Best-effort detection for recursive force deletes.
--
-- Splits on common shell chain/pipe separators so @ls && rm -rf tmp@ is still
-- caught. Looks for an @rm@ invocation whose flags include both recursive and
-- force (any order, clustered or separate: @-rf@, @-fr@, @-r -f@, long opts).
commandLooksLikeRmRf :: Text -> Bool
commandLooksLikeRmRf command =
    any segmentLooksLikeRmRf (splitShellSegments command)

splitShellSegments :: Text -> [Text]
splitShellSegments =
    filter (not . Text.null . Text.strip)
        . Text.split (\c -> c == ';' || c == '|' || c == '&' || c == '\n')

segmentLooksLikeRmRf :: Text -> Bool
segmentLooksLikeRmRf segment =
    case dropEnvPrefixes (tokenize segment) of
        (cmd : rest)
            | isRmCommand cmd -> flagsHaveRecursiveForce rest
        _ -> False

-- | Strip leading @VAR=val@ assignments and common wrappers (@sudo@, @env@,
-- @command@, @nice@, @nohup@, @time@).
dropEnvPrefixes :: [Text] -> [Text]
dropEnvPrefixes = go
  where
    go [] = []
    go (t : ts)
        | Text.any (== '=') t
            && not (Text.isPrefixOf "-" t)
            && Text.all isEnvAssignChar (Text.takeWhile (/= '=') t) =
            go ts
        | Text.toLower t `elem` wrappers = go ts
        | otherwise = t : ts
    wrappers = ["sudo", "env", "command", "nice", "nohup", "time", "stdbuf"]
    isEnvAssignChar c = isAlphaNum c || c == '_'

isRmCommand :: Text -> Bool
isRmCommand cmd =
    let base = Text.toLower (Text.takeWhileEnd (/= '/') cmd)
    in base == "rm" || base == "rm.exe"

flagsHaveRecursiveForce :: [Text] -> Bool
flagsHaveRecursiveForce args =
    let flags = takeWhile isFlag args
        clustered = Text.concat (map stripDashes flags)
        lower = Text.map toLower clustered
        hasR = Text.any (== 'r') lower || any isLongRecursive flags
        hasF = Text.any (== 'f') lower || any isLongForce flags
    in hasR && hasF
  where
    isFlag t = Text.isPrefixOf "-" t
    stripDashes = Text.dropWhile (== '-')
    isLongRecursive t =
        let x = Text.toLower t
        in x == "--recursive" || Text.isPrefixOf "--recursive=" x
    isLongForce t =
        let x = Text.toLower t
        in x == "--force" || Text.isPrefixOf "--force=" x

tokenize :: Text -> [Text]
tokenize = map Text.pack . go [] . Text.unpack
  where
    go acc [] = reverse (filter (not . null) acc)
    go acc cs =
        let cs' = dropWhile isHorzSpace cs
        in case cs' of
            [] -> reverse (filter (not . null) acc)
            '\'' : rest ->
                let (body, after) = break (== '\'') rest
                    rest' = case after of
                        '\'' : more -> more
                        _ -> after
                in go (body : acc) rest'
            '"' : rest ->
                let (body, after) = break (== '"') rest
                    rest' = case after of
                        '"' : more -> more
                        _ -> after
                in go (body : acc) rest'
            _ ->
                let (tok, rest) = break isHorzSpace cs'
                in go (tok : acc) rest
    isHorzSpace c = c == ' ' || c == '\t' || c == '\n'
