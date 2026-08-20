-- | Hard-deny patterns for shell tools, even under auto-approve / yolo.
--
-- Inspired by Grok Build's "always-approve + deny rules" and Codex's forbidden
-- exec-policy prefixes. First cut blocks recursive force deletes.
module Agent.Tools.Dangerous
    ( shellCommandBlocked
    , commandLooksLikeRmRf
    , forbiddenRmRfReason
    ) where

import Data.Char (isAlphaNum, toLower)
import Data.Text (Text)
import qualified Data.Text as Text

-- | If @toolName@ is a shell tool and @argumentsJson@ contains a forbidden
-- command, return a rejection message for the model. Otherwise 'Nothing'.
shellCommandBlocked :: Text -> Text -> Maybe Text
shellCommandBlocked toolName argumentsJson
    | toolName `elem` ["run_terminal_cmd", "shell_command"] =
        case jsonStringField "command" argumentsJson of
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
tokenize = filter (not . Text.null) . Text.words

-- | Minimal JSON string-field extractor for @{"command":"..."}@ payloads.
-- Avoids pulling aeson into this leaf helper; sufficient for our tool args.
jsonStringField :: Text -> Text -> Maybe Text
jsonStringField key raw =
    let needle = "\"" <> key <> "\""
    in case Text.breakOn needle raw of
        (_, rest)
            | Text.null rest -> Nothing
            | otherwise ->
                let afterKey = Text.drop (Text.length needle) rest
                    afterColon = Text.dropWhile (\c -> c == ' ' || c == '\t' || c == '\n' || c == ':') afterKey
                in parseJsonString afterColon

parseJsonString :: Text -> Maybe Text
parseJsonString t = case Text.uncons t of
    Just ('"', rest) -> go rest ""
    _ -> Nothing
  where
    go txt acc = case Text.uncons txt of
        Nothing -> Nothing
        Just ('"', _) -> Just (Text.reverse acc)
        Just ('\\', rest) -> case Text.uncons rest of
            Just (c, rest') -> go rest' (Text.cons c acc)
            Nothing -> Nothing
        Just (c, rest) -> go rest (Text.cons c acc)
