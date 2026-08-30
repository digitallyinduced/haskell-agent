-- | Workspace-confined filesystem helpers for coding tools.
module Agent.Tools.FileSystem
    ( deleteTextFile
    , displayPathInWorkspace
    , listDirectoryEntries
    , pathTargetsSystemTemp
    , readTextFile
    , renameTextFile
    , resolveForRead
    , resolveUnderCwd
    , writeTextFile
    ) where

import Agent.FileRetry (retryOnFileBusy)
import Agent.Concurrent (mapConcurrentlyBounded)
import Agent.OsPath (relativeDisplayPath, toText, unsafeToFilePath)
import Agent.Tools.Types (ToolEnv(..), addToolAllowedRoot)
import Control.Exception.Safe (SomeException, try, tryAny, tryIO)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import Data.IORef (readIORef)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import System.Directory.OsPath
    ( canonicalizePath
    , createDirectoryIfMissing
    , doesDirectoryExist
    , doesPathExist
    , listDirectory
    , pathIsSymbolicLink
    , removeFile
    , renameFile
    )
import System.OsPath
    ( OsPath
    , dropTrailingPathSeparator
    , equalFilePath
    , isAbsolute
    , joinPath
    , makeRelative
    , splitDirectories
    , takeDirectory
    , takeFileName
    , unsafeEncodeUtf
    , (</>)
    )
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files
    ( deviceID
    , fileID
    , getFileStatus
    )
import System.Posix.Types (DeviceID, FileID)

-- | Resolve a model-supplied path against the tool cwd and reject anything
-- that canonicalizes outside the cwd or an explicitly allowed root, including
-- via symlinks. Relative paths are always cwd-relative. When a private session
-- temp root is configured, absolute paths under the system @/tmp@ and
-- @/private/tmp@ aliases are resolved under that private root instead.
resolveUnderCwd :: ToolEnv -> OsPath -> IO (Either Text OsPath)
resolveUnderCwd env requested =
    resolveWithRoots env requested []

-- | Resolve a read-only tool path, additionally allowing resources belonging
-- to the current skill catalog. Mutating tools deliberately continue to use
-- 'resolveUnderCwd', so discovering a user skill does not make it editable.
resolveForRead :: ToolEnv -> OsPath -> IO (Either Text OsPath)
resolveForRead env requested = do
    skillRoots <- readIORef env.toolSkillRoots
    resolveWithRoots env requested skillRoots

resolveWithRoots
    :: ToolEnv
    -> OsPath
    -> [OsPath]
    -> IO (Either Text OsPath)
resolveWithRoots env requested extraRoots = do
    resolveWithRootsAttempt env requested extraRoots >>= \case
        Right resolved -> pure (Right resolved)
        Left (OutsideAllowedRoots resolved) ->
            readIORef env.toolRootAccessRequest >>= \case
                Nothing -> pure $ Left (outsideRootsMessage requested)
                Just requestAccess -> do
                    root <- nearestExistingDirectory resolved
                    case root of
                        Nothing -> pure $ Left (outsideRootsMessage requested)
                        Just requestedRoot ->
                            tryAny (requestAccess requestedRoot) >>= \case
                                Left exception ->
                                    pure $ Left
                                        ("Filesystem access request failed: "
                                            <> Text.pack (show exception))
                                Right False ->
                                    pure $ Left (outsideRootsMessage requested)
                                Right True -> do
                                    addToolAllowedRoot env requestedRoot
                                    resolveWithRootsAttempt env requested extraRoots >>= \case
                                        Right value -> pure (Right value)
                                        Left _ -> pure $
                                            Left (outsideRootsMessage requested)
        Left (ResolverFailure err) -> pure (Left err)

data ResolveFailure
    = OutsideAllowedRoots !OsPath
    | ResolverFailure !Text

resolveWithRootsAttempt
    :: ToolEnv
    -> OsPath
    -> [OsPath]
    -> IO (Either ResolveFailure OsPath)
resolveWithRootsAttempt env requested extraRoots = do
    configuredRoots <- readIORef env.toolAllowedRoots
    sessionTmp <- readIORef env.toolSessionTmp
    canonicalRoots <-
        mapConcurrentlyBounded rootCanonicalizationConcurrency canonicalizePath
            (env.toolCwd : configuredRoots <> extraRoots)
    canonicalSessionTmp <- traverse canonicalizePath sessionTmp
    let (absCwd, configuredAndExtraRoots) = case canonicalRoots of
            root : roots -> (root, roots)
            [] -> error "resolveWithRoots: cwd canonicalization omitted"
        allowedRoots =
            configuredAndExtraRoots <> maybe [] pure canonicalSessionTmp
    let combined
            | isAbsolute requested = requested
            | otherwise = absCwd </> requested
        roots = absCwd : allowedRoots
    tempAlias <-
        if isAbsolute requested
            then systemTempRelative combined
            else pure Nothing
    case (canonicalSessionTmp, tempAlias) of
        (Just tempRoot, Just (aliasRoot, relative)) -> do
            explicitlyAllowed <-
                systemTempPathIsAllowed roots aliasRoot relative
            if explicitlyAllowed
                then resolveOrdinary roots combined
                else resolvePrivateTemp requested tempRoot relative
        _ -> resolveOrdinary roots combined

resolveOrdinary :: [OsPath] -> OsPath -> IO (Either ResolveFailure OsPath)
resolveOrdinary roots path =
    resolvePath path >>= \case
        Left err -> pure (Left (ResolverFailure err))
        Right resolved
            | any (`isInside` resolved) roots -> pure (Right resolved)
            | otherwise -> pure (Left (OutsideAllowedRoots resolved))

resolvePrivateTemp
    :: OsPath
    -> OsPath
    -> OsPath
    -> IO (Either ResolveFailure OsPath)
resolvePrivateTemp requested tempRoot relative =
    resolvePath (tempRoot </> relative) >>= \case
        Left err -> pure (Left (ResolverFailure err))
        Right resolved
            | tempRoot `isInside` resolved -> pure (Right resolved)
            | otherwise ->
                pure (Left (ResolverFailure (tempEscapeMessage requested)))

resolvePath :: OsPath -> IO (Either Text OsPath)
resolvePath path = do
    exists <- doesPathExist path
    if exists
        then Right <$> canonicalizePath path
        else resolveMissing path

-- | Treat existing spellings of the conventional POSIX temp namespace as
-- aliases for the session-private temp root. Inspect each on-disk prefix so
-- @..@ follows preceding symlinks exactly as the filesystem does. This also
-- makes case variants aliases only on filesystems where they resolve to the
-- same directory.
systemTempRelative :: OsPath -> IO (Maybe (OsPath, OsPath))
systemTempRelative path =
    case directAlias components of
        Just match -> pure (Just match)
        Nothing -> do
            aliases <- fmap concat $
                mapM
                    (\alias ->
                        pathIdentity alias >>= \case
                            Nothing -> pure []
                            Just identity -> pure [(alias, identity)])
                    [systemTmpRoot, privateSystemTmpRoot]
            findAlias aliases [] components
  where
    components =
        normalizePosixRoot $
            splitDirectories (dropTrailingPathSeparator path)
    normalizePosixRoot = \case
        root : rest
            | let display = toText root
            , not (Text.null display)
            , Text.all (== '/') display ->
                posixRoot : rest
        other -> other

    directAlias = \case
        [root, tmp]
            | root == posixRoot
            , tmp == tmpComponent ->
                Just (systemTmpRoot, currentDirectory)
        root : tmp : rest
            | root == posixRoot
            , tmp == tmpComponent ->
                Just (systemTmpRoot, joinPath rest)
        [root, private, tmp]
            | root == posixRoot
            , private == privateComponent
            , tmp == tmpComponent ->
                Just (privateSystemTmpRoot, currentDirectory)
        root : private : tmp : rest
            | root == posixRoot
            , private == privateComponent
            , tmp == tmpComponent ->
                Just (privateSystemTmpRoot, joinPath rest)
        _ -> Nothing
    findAlias _ _ [] = pure Nothing
    findAlias aliases prefix (component : rest) = do
        let nextPrefix = prefix <> [component]
        candidateIdentity <- pathIdentity (joinPath nextPrefix)
        case candidateIdentity >>= matchingAlias aliases of
            Just alias ->
                pure $ Just
                    ( alias
                    , if null rest then currentDirectory else joinPath rest
                    )
            Nothing -> findAlias aliases nextPrefix rest

    matchingAlias aliases identity =
        fst <$> find ((== identity) . snd) aliases

pathIdentity :: OsPath -> IO (Maybe (DeviceID, FileID))
pathIdentity path =
    tryAny (getFileStatus (unsafeToFilePath path)) >>= \case
        Left _ -> pure Nothing
        Right status -> pure $ Just (deviceID status, fileID status)

-- | Whether the host filesystem resolves any prefix of this absolute path to
-- the conventional shared temp directory.
pathTargetsSystemTemp :: OsPath -> IO Bool
pathTargetsSystemTemp path =
    maybe False (const True) <$> systemTempRelative path

-- | A preconfigured host-temp root is an explicit escape hatch from the
-- private alias. Canonicalize only the alias root, not its descendants, so
-- shared-host files and symlinks cannot affect the default remapping decision.
systemTempPathIsAllowed
    :: [OsPath]
    -> OsPath
    -> OsPath
    -> IO Bool
systemTempPathIsAllowed roots aliasRoot relative =
    tryAny (canonicalizePath aliasRoot) >>= \case
        Left _ -> pure False
        Right canonicalAlias ->
            pure $
                any
                    (`isInside` (canonicalAlias </> relative))
                    roots

systemTmpRoot :: OsPath
systemTmpRoot = unsafeEncodeUtf "/tmp"

privateSystemTmpRoot :: OsPath
privateSystemTmpRoot = unsafeEncodeUtf "/private/tmp"

tmpComponent :: OsPath
tmpComponent = unsafeEncodeUtf "tmp"

privateComponent :: OsPath
privateComponent = unsafeEncodeUtf "private"

currentDirectory :: OsPath
currentDirectory = unsafeEncodeUtf "."

posixRoot :: OsPath
posixRoot = unsafeEncodeUtf "/"

tempEscapeMessage :: OsPath -> Text
tempEscapeMessage requested =
    "Path escapes the private session temp directory: " <> toText requested

outsideRootsMessage :: OsPath -> Text
outsideRootsMessage requested =
    "Path escapes the allowed filesystem roots: " <> toText requested

-- | Return the nearest existing directory containing a requested path. This
-- keeps grants useful for new files while avoiding a grant for a nonexistent
-- path that cannot be canonicalized yet.
nearestExistingDirectory :: OsPath -> IO (Maybe OsPath)
nearestExistingDirectory path = do
    doesDirectoryExist path >>= \case
        True -> Just <$> canonicalizePath path
        False -> go (takeDirectory path)
  where
    go directory = do
        doesDirectoryExist directory >>= \case
            True -> Just <$> canonicalizePath directory
            False ->
                let parent = takeDirectory directory
                in if equalFilePath parent directory
                    then pure Nothing
                    else go parent

resolveMissing :: OsPath -> IO (Either Text OsPath)
resolveMissing path = do
    tryIO (pathIsSymbolicLink path) >>= \case
        Right True ->
            try @_ @SomeException (canonicalizePath path) >>= \case
                Left err -> pure $ Left $
                    "Cannot resolve symbolic link: " <> Text.pack (show err)
                Right resolved -> pure (Right resolved)
        Left err
            | not (isDoesNotExistError err) ->
                pure $ Left ("Cannot inspect path: " <> Text.pack (show err))
        _ -> do
            exists <- doesPathExist path
            if exists
                then Right <$> canonicalizePath path
                else do
                    let parent = takeDirectory path
                    if equalFilePath parent path
                        then pure (Right path)
                        else fmap (</> takeFileName path) <$> resolveMissing parent

isInside :: OsPath -> OsPath -> Bool
isInside root path
    | equalFilePath root path = True
    | otherwise =
        let relative = makeRelative root path
        in not (isAbsolute relative)
            && staysWithinRoot (splitDirectories relative)
  where
    parent = unsafeEncodeUtf ".."
    current = unsafeEncodeUtf "."

    -- A missing path is intentionally kept lexical by 'resolveMissing'.
    -- Track directory depth instead of checking only the first component:
    -- e.g. @nested/../../outside@ escapes even though it starts safely.
    staysWithinRoot = go (0 :: Int)
      where
        go _ [] = True
        go depth (component : rest)
            | component == current = go depth rest
            | component == parent =
                depth > 0 && go (depth - 1) rest
            | otherwise = go (depth + 1) rest

-- | Present a resolved filesystem path relative to the tool workspace.
displayPathInWorkspace :: ToolEnv -> OsPath -> IO Text
displayPathInWorkspace env path =
    try @_ @SomeException (canonicalizePath env.toolCwd) >>= \case
        Right cwd -> pure (relativeDisplayPath cwd path)
        Left _ -> pure (relativeDisplayPath env.toolCwd path)

readTextFile :: OsPath -> IO (Either Text Text)
readTextFile path =
    try @_ @SomeException (retryOnFileBusy (BS.readFile (unsafeToFilePath path))) >>= \case
    Left err -> pure $ Left $ "Failed to read file: " <> Text.pack (show err)
    Right bytes
        | BS.elem 0 (BS.take 8192 bytes) ->
            pure $ Left "Cannot read binary file"
        | otherwise ->
            pure $ Right $ decodeUtf8With lenientDecode bytes

writeTextFile :: OsPath -> Text -> IO (Either Text ())
writeTextFile path content = do
    createDirectoryIfMissing True (takeDirectory path)
    try @_ @SomeException
        (retryOnFileBusy (BS.writeFile (unsafeToFilePath path) (encodeUtf8 content))) >>= \case
        Left err -> pure $ Left $ "Failed to write file: " <> Text.pack (show err)
        Right () -> pure (Right ())

deleteTextFile :: OsPath -> IO (Either Text ())
deleteTextFile path =
    try @_ @SomeException (removeFile path) >>= \case
        Left err -> pure $ Left $ "Failed to delete file: " <> Text.pack (show err)
        Right () -> pure (Right ())

renameTextFile :: OsPath -> OsPath -> IO (Either Text ())
renameTextFile from to = do
    createDirectoryIfMissing True (takeDirectory to)
    try @_ @SomeException (retryOnFileBusy (renameFile from to)) >>= \case
        Left err -> pure $ Left $ "Failed to move file: " <> Text.pack (show err)
        Right () -> pure (Right ())

listDirectoryEntries :: OsPath -> IO (Either Text [(OsPath, Bool)])
listDirectoryEntries path = try @_ @SomeException (listDirectory path) >>= \case
    Left err -> pure $ Left $ "Failed to list directory: " <> Text.pack (show err)
    Right names ->
        Right
            <$> mapConcurrentlyBounded directoryClassificationConcurrency
                (classify path)
                names
  where
    classify root name = do
        isDir <- doesDirectoryExist (root </> name)
        pure (name, isDir)

rootCanonicalizationConcurrency :: Int
rootCanonicalizationConcurrency = 8

directoryClassificationConcurrency :: Int
directoryClassificationConcurrency = 16
