-- | Hard-deny patterns for shell tools, even under auto-approve / yolo.
--
-- Inspired by Grok Build's "always-approve + deny rules" and Codex's forbidden
-- exec-policy prefixes. Blocks recursive force deletes and shell references to
-- the shared host temp namespace.
module Agent.Tools.Dangerous
    ( shellCommandBlocked
    , blockedShellCommandReason
    , commandLooksLikeRmRf
    , commandUsesHardcodedSystemTmp
    , forbiddenRmRfReason
    , hardcodedSystemTmpReason
    ) where

import Agent.JsonText (jsonTextField)
import Data.Char
    ( chr
    , digitToInt
    , isAlphaNum
    , isHexDigit
    , isSpace
    , toLower
    )
import Data.Text (Text)
import qualified Data.Text as Text

-- | If @toolName@ is a shell tool and @argumentsJson@ contains a forbidden
-- command, return a rejection message for the model. Otherwise 'Nothing'.
shellCommandBlocked :: Text -> Text -> Maybe Text
shellCommandBlocked toolName argumentsJson
    | toolName `elem`
        ["monitor", "run_terminal_cmd", "run_terminal_command", "shell_command"] =
        case jsonTextField "command" argumentsJson of
            Nothing -> Nothing
            Just command -> blockedShellCommandReason command
    | otherwise = Nothing

-- | Apply every first-party shell hard-deny in stable precedence order.
blockedShellCommandReason :: Text -> Maybe Text
blockedShellCommandReason command
    | commandLooksLikeRmRf command =
        Just (forbiddenRmRfReason command)
    | commandUsesHardcodedSystemTmp command =
        Just (hardcodedSystemTmpReason command)
    | otherwise = Nothing

forbiddenRmRfReason :: Text -> Text
forbiddenRmRfReason command =
    "Blocked dangerous shell command (rm -rf / recursive force delete). "
        <> "Remove files more narrowly, or ask the user to run the delete outside the agent. "
        <> "Command: "
        <> Text.take 200 (Text.strip command)

-- | Reject literal references to the host's shared temp namespace.
--
-- Shell source cannot be rewritten safely: a textual substitution could alter
-- quoted data, heredocs, URLs, or nested programs. Instead, require the
-- environment variable that the runtime points at the session-private temp
-- directory. This is intentionally best-effort rather than a shell parser; it
-- recognizes absolute paths at token-like boundaries, lexically normalizes
-- dot/parent components, and avoids ordinary URL path components and names
-- such as @/tmpfile@.
commandUsesHardcodedSystemTmp :: Text -> Bool
commandUsesHardcodedSystemTmp command =
    go Nothing command
        || normalizedAbsolutePathTargetsTemp command
        || localFileUrlTargetsTemp command
  where
    go previous remaining
        | Text.null remaining = False
        | tempPathStartsHere previous remaining = True
        | otherwise =
            let current = Text.head remaining
            in go (Just current) (Text.tail remaining)

    tempPathStartsHere previous remaining =
        pathBoundaryBefore previous
            && case Text.span (== '/') remaining of
                (slashes, afterSlashes)
                    | not (Text.null slashes) ->
                        tempComponentAtStart afterSlashes
                            || privateTempAtStart afterSlashes
                _ -> False

    tempComponentAtStart remaining =
        case Text.stripPrefix "tmp" remaining of
            Just suffix -> pathBoundaryAfter suffix
            Nothing -> False

    privateTempAtStart remaining =
        case Text.stripPrefix "private" remaining of
            Just afterPrivate ->
                case Text.span (== '/') afterPrivate of
                    (slashes, afterSlashes)
                        | not (Text.null slashes) ->
                            tempComponentAtStart afterSlashes
                    _ -> False
            Nothing -> False

    pathBoundaryBefore = \case
        Nothing -> True
        Just char -> not (isPathOrUrlChar char)

    pathBoundaryAfter suffix =
        Text.null suffix
            || Text.head suffix == '/'
            || not (isPathNameChar (Text.head suffix))

    normalizedAbsolutePathTargetsTemp = scan Nothing
      where
        scan previous remaining
            | Text.null remaining = False
            | pathBoundaryBefore previous
            , Text.head remaining == '/' =
                let (candidate, _) =
                        Text.span isAbsolutePathChar remaining
                in normalizedPathTargetsTemp candidate
                    || advance remaining
            | otherwise = advance remaining
          where
            advance text =
                scan (Just (Text.head text)) (Text.tail text)

    localFileUrlTargetsTemp = scan Nothing
      where
        scan previous remaining
            | Text.null remaining = False
            | pathBoundaryBefore previous
            , "file:" == Text.toLower (Text.take 5 remaining)
            , Just path <- fileUrlAbsolutePath (Text.drop 5 remaining) =
                normalizedPathTargetsTemp
                    (percentDecodePath
                        (Text.takeWhile isFileUrlPathChar path))
                    || advance remaining
            | otherwise = advance remaining
          where
            advance text =
                scan (Just (Text.head text)) (Text.tail text)

        fileUrlAbsolutePath afterScheme
            | Just afterAuthority <- Text.stripPrefix "//" afterScheme =
                let (_, path) = Text.breakOn "/" afterAuthority
                in if Text.null path then Nothing else Just path
            | Text.isPrefixOf "/" afterScheme = Just afterScheme
            | Text.isPrefixOf "/" (percentDecodePath afterScheme) =
                Just afterScheme
            | otherwise = Nothing

        isFileUrlPathChar char =
            not (isSpace char)
                && char `notElem` ("'\";|&()?#" :: String)

    isAbsolutePathChar char =
        char == '/' || isPathNameChar char

    normalizedPathTargetsTemp path =
        case reverse (foldl normalizeComponent [] (Text.splitOn "/" path)) of
            "tmp" : _ -> True
            "private" : "tmp" : _ -> True
            _ -> False

    -- The accumulator is reversed, so an absolute parent component pops the
    -- most recent ordinary component and clamps at the root.
    normalizeComponent components component
        | Text.null component || component == "." = components
        | component == ".." = drop 1 components
        | otherwise = component : components

    percentDecodePath = Text.pack . decode . Text.unpack
      where
        decode ('%' : high : low : rest)
            | isHexDigit high
            , isHexDigit low =
                chr (digitToInt high * 16 + digitToInt low) : decode rest
        decode (char : rest) = char : decode rest
        decode [] = []

    -- A preceding URL/path character means this slash is a path component,
    -- rather than the beginning of an absolute temp path.
    isPathOrUrlChar char =
        isPathNameChar char || char `elem` ("/:" :: String)

    isPathNameChar char =
        isAlphaNum char || char `elem` ("._-" :: String)

hardcodedSystemTmpReason :: Text -> Text
hardcodedSystemTmpReason command =
    "Blocked hardcoded system temp path. Use $TMPDIR (or \
    \$HASKELL_AGENT_TMPDIR) so scratch files stay in this session's private \
    \temp directory; do not use literal /tmp or /private/tmp paths. Command: "
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
        shortFlags = filter (not . isLongFlag) flags
        clustered = Text.concat (map stripDashes shortFlags)
        lower = Text.map toLower clustered
        hasR = Text.any (== 'r') lower || any isLongRecursive flags
        hasF = Text.any (== 'f') lower || any isLongForce flags
    in hasR && hasF
  where
    isFlag t = Text.isPrefixOf "-" t
    isLongFlag t = Text.isPrefixOf "--" t
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
