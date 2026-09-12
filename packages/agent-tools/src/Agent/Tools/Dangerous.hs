-- | Hard-deny patterns for shell tools, even under auto-approve / yolo.
--
-- Inspired by Grok Build's "always-approve + deny rules" and Codex's forbidden
-- exec-policy prefixes. Blocks recursive force deletes and shell references to
-- the shared host temp namespace.
module Agent.Tools.Dangerous
    ( shellCommandBlocked
    , blockedShellCommandReason
    , blockedShellCommandReasonAt
    , blockedShellCommandReasonIn
    , blockedTempAccessReasonAt
    , commandLooksLikeRmRf
    , commandUsesHardcodedSystemTmp
    , commandUsesHardcodedSystemTmpAt
    , commandEscapesSessionTempVariable
    , forbiddenRmRfReason
    , hardcodedSystemTmpReason
    , sessionTempEscapeReason
    ) where

import Agent.JsonText (jsonTextField)
import Agent.OsPath (fromText, toText, unsafeToFilePath)
import Agent.Tools.FileSystem (pathTargetsSystemTemp)
import Data.Char
    ( chr
    , digitToInt
    , isAlphaNum
    , isHexDigit
    , isSpace
    , toLower
    )
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (canonicalizePath, doesDirectoryExist)
import System.FilePath
    ( isAbsolute
    , normalise
    , splitDirectories
    , (</>)
    )
import System.OsPath (OsPath, unsafeEncodeUtf)

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
    | commandEscapesSessionTempVariable command =
        Just (sessionTempEscapeReason command)
    | commandUsesHardcodedSystemTmp command =
        Just (hardcodedSystemTmpReason command)
    | otherwise = Nothing

-- | Apply shell hard-denies after resolving the command's initial working
-- directory. This additionally catches relative spellings that reach the
-- shared system temp namespace from that directory.
blockedShellCommandReasonAt :: OsPath -> Text -> IO (Maybe Text)
blockedShellCommandReasonAt cwd command
    | commandLooksLikeRmRf command =
        pure $ Just (forbiddenRmRfReason command)
    | otherwise = blockedTempAccessReasonAt cwd command

-- | Apply the shell hard-denies while also protecting an active session-temp
-- root when the persistent shell is currently inside it.
blockedShellCommandReasonIn
    :: Maybe OsPath
    -> OsPath
    -> Text
    -> IO (Maybe Text)
blockedShellCommandReasonIn sessionTmp cwd command
    | commandLooksLikeRmRf command =
        pure $ Just (forbiddenRmRfReason command)
    | otherwise =
        case sessionTmp of
            Nothing -> blockedTempAccessReasonAt cwd command
            Just temp
                | commandEscapesSessionTempAt temp cwd command ->
                    pure $ Just (sessionTempEscapeReason command)
                | otherwise -> blockedTempAccessReasonAt cwd command

-- | Temp-specific deny shared by shell and GHCi tools.
blockedTempAccessReasonAt :: OsPath -> Text -> IO (Maybe Text)
blockedTempAccessReasonAt cwd command
    | commandEscapesSessionTempVariable command =
        pure $ Just (sessionTempEscapeReason command)
    | otherwise =
        commandUsesHardcodedSystemTmpAt cwd command >>= \usesShared ->
            pure $
                if usesShared
                    then Just (hardcodedSystemTmpReason command)
                    else Nothing

forbiddenRmRfReason :: Text -> Text
forbiddenRmRfReason command =
    "Blocked dangerous shell command (rm -rf / recursive force delete). "
        <> "Remove files more narrowly, or ask the user to run the delete outside the agent. "
        <> "Command: "
        <> Text.take 200 (Text.strip command)

sessionTempEscapeReason :: Text -> Text
sessionTempEscapeReason command =
    "Blocked path traversal outside the session's private temp directory. "
        <> "Keep $TMPDIR and $HASKELL_AGENT_TMPDIR paths within their root. "
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
commandUsesHardcodedSystemTmp =
    commandUsesHardcodedSystemTmpLexically True

-- | Like 'commandUsesHardcodedSystemTmp', but resolve relative path-shaped
-- tokens against the shell's initial working directory. Case variants are
-- aliases only when the host filesystem resolves them to the same directory.
commandUsesHardcodedSystemTmpAt :: OsPath -> Text -> IO Bool
commandUsesHardcodedSystemTmpAt cwd command = do
    let cwdText = toText cwd
        direct =
            commandUsesHardcodedSystemTmpLexically False command
    if direct
        then pure True
        else shellCwdMutationTargetsTemp cwdText command

commandUsesHardcodedSystemTmpLexically :: Bool -> Text -> Bool
commandUsesHardcodedSystemTmpLexically includeNormalized command =
    go Nothing command
        || includeNormalized
            && ( normalizedAbsolutePathTargetsTemp command
                || localFileUrlTargetsTemp command
               )
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
        case stripPrefixAlias "tmp" remaining of
            Just suffix -> pathBoundaryAfter suffix
            Nothing -> False

    stripPrefixAlias prefix text
        | candidate == prefix =
            Just (Text.drop (Text.length prefix) text)
        | otherwise = Nothing
      where
        candidate = Text.take (Text.length prefix) text

    privateTempAtStart remaining =
        case stripPrefixAlias "private" remaining of
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
        case
            reverse (foldl normalizeComponent [] (Text.splitOn "/" path))
        of
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

-- | Reject lexical traversal above either environment variable's root.
-- Ordinary normalization inside that root remains valid.
commandEscapesSessionTempVariable :: Text -> Bool
commandEscapesSessionTempVariable command =
    scan Nothing command
        || sessionTempCwdTraversal command
  where
    scan previous remaining
        | Text.null remaining = False
        | variableBoundaryBefore previous
        , Just suffix <- sessionTempVariableSuffix remaining =
            variableSuffixEscapes suffix || advance remaining
        | otherwise = advance remaining
      where
        advance text =
            scan (Just (Text.head text)) (Text.tail text)

    sessionTempVariableSuffix text =
        firstMatch
            [ "${HASKELL_AGENT_TMPDIR}"
            , "${TMPDIR}"
            , "$HASKELL_AGENT_TMPDIR"
            , "$TMPDIR"
            ]
      where
        firstMatch [] = Nothing
        firstMatch (prefix : prefixes)
            | Text.isPrefixOf prefix text
            , variableBoundaryAfter prefix text =
                Just (Text.drop (Text.length prefix) text)
            | otherwise = firstMatch prefixes

    variableBoundaryAfter prefix text
        | Text.isPrefixOf "${" prefix = True
        | Text.length text == Text.length prefix = True
        | otherwise =
            not (isVariableNameChar (Text.index text (Text.length prefix)))

    variableSuffixEscapes suffix =
        case Text.uncons renderedSuffix of
            Nothing -> False
            Just ('/', path) ->
                componentsEscape
                    (Text.splitOn "/" path)
            -- Concatenating onto the expansion changes the temp root itself:
            -- e.g. @$TMPDIR-other@ names a sibling, not a child.
            Just _ -> True
      where
        renderedSuffix =
            Text.filter (not . isShellSyntax)
                (Text.takeWhile isVariableSuffixChar suffix)

    componentsEscape = goDepth (0 :: Int)
      where
        goDepth _ [] = False
        goDepth depth (component : rest)
            | Text.null component || component == "." =
                goDepth depth rest
            | component == ".." =
                depth == 0 || goDepth (depth - 1) rest
            | otherwise = goDepth (depth + 1) rest

    variableBoundaryBefore = \case
        Nothing -> True
        Just char -> not (isVariableNameChar char)

    isVariableNameChar char = isAlphaNum char || char == '_'
    isShellSyntax char =
        char == '\'' || char == '"' || char == '\\'
    isVariableSuffixChar char =
        not (isSpace char)
            && char `notElem` (";|&()<>" :: String)

sessionTempCwdTraversal :: Text -> Bool
sessionTempCwdTraversal command =
    shellTraversalEscapes Nothing command

shellTraversalEscapes :: Maybe Int -> Text -> Bool
shellTraversalEscapes initialDepth command =
    go initialDepth (shellSegments command)
  where
    go _ [] = False
    go depth (segment : rest)
        | maybe False
            (\current ->
                any (relativeCandidateEscapes current)
                    (relativePathCandidates segment))
            depth =
                True
        | otherwise =
            go (nextDepth depth segment) rest

    nextDepth current segment =
        case shellCdTarget segment of
            Nothing -> current
            Just target ->
                case sessionVariableRelative target of
                    Just relative -> depthAfter 0 relative
                    Nothing
                        | isAbsolute target -> Nothing
                        | otherwise -> current >>= (`depthAfter` target)

    sessionVariableRelative target =
        firstMatch
            [ "${HASKELL_AGENT_TMPDIR}"
            , "${TMPDIR}"
            , "$HASKELL_AGENT_TMPDIR"
            , "$TMPDIR"
            ]
      where
        text = Text.pack target
        firstMatch [] = Nothing
        firstMatch (prefix : prefixes)
            | Just suffix <- Text.stripPrefix prefix text
            , Text.isPrefixOf "${" prefix
                || Text.null suffix
                || not (isVariableNameChar (Text.head suffix)) =
                Just (Text.unpack (Text.dropWhile (== '/') suffix))
            | otherwise = firstMatch prefixes

        isVariableNameChar char =
            isAlphaNum char || char == '_'

    depthAfter initial path =
        foldDepth initial (splitDirectories path)

    foldDepth depth [] = Just depth
    foldDepth depth (component : rest)
        | component == "." || null component = foldDepth depth rest
        | component == ".."
        , depth == 0 = Nothing
        | component == ".." = foldDepth (depth - 1) rest
        | otherwise = foldDepth (depth + 1) rest

    relativeCandidateEscapes initial candidate =
        not (Text.isPrefixOf "/" candidate)
            && case depthAfter initial (Text.unpack candidate) of
                Nothing -> True
                Just _ -> False

-- | A persistent shell may already be inside its private temp directory. Block
-- relative parent traversal that would leave that root on a later call.
commandEscapesSessionTempAt :: OsPath -> OsPath -> Text -> Bool
commandEscapesSessionTempAt temp cwd command =
    case depthWithin (unsafeToFilePath temp) (unsafeToFilePath cwd) of
        Nothing -> False
        Just initialDepth -> shellTraversalEscapes (Just initialDepth) command
  where
    depthWithin root path =
        let rootComponents = splitDirectories (normalise root)
            pathComponents = splitDirectories (normalise path)
        in if rootComponents `isPrefixOf` pathComponents
            then Just (length pathComponents - length rootComponents)
            else Nothing

-- | Track straightforward @cd@ commands using real filesystem resolution.
-- This closes common multi-command aliases without pretending to be a full
-- shell parser.
shellCwdMutationTargetsTemp :: Text -> Text -> IO Bool
shellCwdMutationTargetsTemp initialCwd command =
    go (Text.unpack initialCwd) (shellSegments command)
  where
    go _ [] = pure False
    go cwd (segment : rest) = do
        targetsTemp <- anyM pathTargetsShellTemp
            (shellPathCandidates cwd segment)
        if targetsTemp
            then pure True
            else nextShellCwd cwd segment >>= \next ->
                go next rest

    pathTargetsShellTemp path = do
        actual <- pathTargetsSystemTemp path
        if actual
            then pure True
            else do
                privateTempExists <- doesDirectoryExist "/private/tmp"
                pure $
                    not privateTempExists
                        && lexicallyTargetsPrivateTemp
                            (unsafeToFilePath path)

    lexicallyTargetsPrivateTemp path =
        case splitDirectories (normalise path) of
            root : private : tmp : _ ->
                root == "/"
                    && private == "private"
                    && tmp == "tmp"
            _ -> False

    nextShellCwd cwd segment =
        case shellCdTarget segment of
            Nothing -> pure cwd
            Just target -> do
                let requested
                        | isAbsolute target = target
                        | otherwise = cwd </> target
                exists <- doesDirectoryExist requested
                if exists
                    then canonicalizePath requested
                    else pure cwd

shellSegments :: Text -> [Text]
shellSegments =
    filter (not . Text.null . Text.strip)
        . Text.split (\char ->
            char == ';' || char == '\n' || char == '|' || char == '&')

shellCdTarget :: Text -> Maybe FilePath
shellCdTarget segment =
    case Text.words (Text.strip segment) of
        ["cd", target] -> Just (Text.unpack (stripShellQuotes target))
        ["builtin", "cd", target] ->
            Just (Text.unpack (stripShellQuotes target))
        _ -> Nothing

stripShellQuotes :: Text -> Text
stripShellQuotes =
    Text.filter (\char -> char /= '"' && char /= '\'')

relativePathCandidates :: Text -> [Text]
relativePathCandidates = go Nothing
  where
    go previous remaining
        | Text.null remaining = []
        | candidateBoundaryBefore previous
        , isCandidateChar (Text.head remaining) =
            let (candidate, suffix) = Text.span isCandidateChar remaining
            in candidate : continue candidate suffix
        | otherwise =
            go (Just (Text.head remaining)) (Text.tail remaining)

    continue candidate suffix
        | Text.null suffix = []
        | otherwise =
            go (Just (Text.last candidate)) suffix

    candidateBoundaryBefore = \case
        Nothing -> True
        Just char ->
            not (isAlphaNum char || char `elem` ("._-/:" :: String))

    isCandidateChar char =
        char == '/' || isAlphaNum char || char `elem` ("._-" :: String)

shellPathCandidates :: FilePath -> Text -> [OsPath]
shellPathCandidates cwd command =
    map candidatePath (relativePathCandidates command)
        <> map fromText (localFileUrlPathCandidates command)
  where
    candidatePath candidate
        | Text.isPrefixOf "/" candidate = fromText candidate
        | otherwise =
            unsafeEncodeUtf (cwd </> Text.unpack candidate)

localFileUrlPathCandidates :: Text -> [Text]
localFileUrlPathCandidates = scan Nothing
  where
    scan previous remaining
        | Text.null remaining = []
        | pathBoundaryBefore previous
        , "file:" == Text.toLower (Text.take 5 remaining)
        , Just path <- fileUrlAbsolutePath (Text.drop 5 remaining) =
            percentDecodePath
                (Text.takeWhile isFileUrlPathChar path)
                : advance remaining
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

    pathBoundaryBefore = \case
        Nothing -> True
        Just char ->
            not
                ( isAlphaNum char
                    || char `elem` ("._-/:" :: String)
                )

percentDecodePath :: Text -> Text
percentDecodePath = Text.pack . decode . Text.unpack
  where
    decode ('%' : high : low : rest)
        | isHexDigit high
        , isHexDigit low =
            chr (digitToInt high * 16 + digitToInt low) : decode rest
    decode (char : rest) = char : decode rest
    decode [] = []

anyM :: (a -> IO Bool) -> [a] -> IO Bool
anyM _ [] = pure False
anyM predicate (value : rest) =
    predicate value >>= \case
        True -> pure True
        False -> anyM predicate rest

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
