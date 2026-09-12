-- | Narrow exceptions for disposable or independently retained ignored paths.
-- Never treat an arbitrary ignored directory as disposable user data.
module Agent.CLI.Worktree.Ignored (checkIgnoredPath, inspectCabalArtifacts, removeCabalArtifacts) where

import Control.Monad (unless, forM, forM_)
import qualified Data.ByteString as BS
import Data.List (isPrefixOf, sort, nub, partition)
import qualified System.Directory as Directory
import System.FilePath ((</>), dropTrailingPathSeparator, takeFileName, takeDirectory, splitDirectories, takeExtension, joinPath)
import System.IO (withBinaryFile, IOMode(ReadMode))
import qualified System.Posix.Files as Posix

-- | Deliberately narrower than whole-checkout collection: only ordinary,
-- singly-linked compiled files with compiler-output signatures beneath
-- Cabal's build directory. Never descend into Swift checkouts or Cabal's
-- downloaded sources, and never remove directories, settings, or history.
inspectCabalArtifacts :: ([String] -> String -> IO String) -> FilePath -> IO [(FilePath, Integer)]
inspectCabalArtifacts git repository = do
    let relative = "dist-newstyle/build"
    exists <- Directory.doesPathExist (repository </> relative)
    if not exists then pure [] else do
        requireDirectories repository (splitDirectories relative)
        tracked <- git ["ls-files", "--cached", "-z", "--", "dist-newstyle"] ""
        unless (null tracked) $ fail "Cabal output contains tracked data"
        binaries <- walk relative
        monitors <- concat <$> mapM monitor (nub [root | (name, _) <- binaries, Just root <- [componentRoot name]])
        let candidates = sort (binaries <> monitors)
        if null candidates then pure [] else do
            evidence <- git ["check-ignore", "-v", "-z", "--stdin"]
                (concatMap (\(name, _) -> name <> "\0") candidates)
            accepted <- parseEvidence (nul evidence)
            unless (all (\(name, _) -> name `elem` accepted) monitors) $
                fail "Cabal build monitor is not repository-ignored; refusing cleanup"
            pure (filter (\(name, _) -> name `elem` accepted) candidates)
  where
    -- Cabal's successful-build monitor must be invalidated or a normal build
    -- may incorrectly report missing executables as up-to-date. Only this
    -- versioned binary monitor is considered, never config/plan/source caches.
    monitor root = do
        let relative = root </> "cache/build"
        exists <- Directory.doesPathExist (repository </> relative)
        if not exists then pure [] else do
            requireDirectories repository (splitDirectories (takeDirectory relative))
            status <- Posix.getSymbolicLinkStatus (repository </> relative)
            unless (Posix.isRegularFile status && Posix.linkCount status == 1) $
                fail "unsafe Cabal build monitor"
            signature <- withBinaryFile (repository </> relative) ReadMode (`BS.hGet` 16)
            unless (signature == BS.pack [0x8e,0x83,0x8b,0x82,0x1c,0x8c,0xaf,0xef,0xa5,0x80,0x29,0xe2,0x5c,0x29,0x52,0xa5]) $
                fail "unrecognized Cabal build monitor format; refusing cleanup"
            pure [(relative, fromIntegral (Posix.fileSize status))]
    parseEvidence [] = pure []
    parseEvidence (source : _line : patternText : name : rest) = do
        remaining <- parseEvidence rest
        pure $ if takeFileName source == ".gitignore"
            && not ("/" `isPrefixOf` source)
            && not (".." `elem` splitDirectories source)
            && not ("!" `isPrefixOf` patternText)
            then name : remaining else remaining
    parseEvidence _ = fail "malformed Git ignore evidence"
    walk relative = do
        names <- sort <$> Directory.listDirectory (repository </> relative)
        unless (not (".git" `elem` names)) $ fail "nested repository in Cabal output"
        concat <$> forM names (\name -> do
            let child = relative </> name
            status <- Posix.getSymbolicLinkStatus (repository </> child)
            if Posix.isDirectory status then walk child
            else if Posix.isRegularFile status && Posix.linkCount status == 1
                && (takeExtension child `elem` [".o", ".dyn_o", ".a"]
                    || (componentRoot child /= Nothing
                        && takeExtension child `elem` ["", ".dylib", ".so"]))
              then do
                signature <- withBinaryFile (repository </> child) ReadMode (`BS.hGet` 8)
                if compilerSignature signature then
                    pure [(child, fromIntegral (Posix.fileSize status))]
                else pure []
              else pure [])

-- dist-newstyle/build/PLATFORM/GHC/PACKAGE[/x|t|b|l/COMPONENT]/build/...
componentRoot :: FilePath -> Maybe FilePath
componentRoot path = case splitDirectories path of
    "dist-newstyle" : "build" : platform : compiler : package : rest
        | "ghc-" `isPrefixOf` compiler ->
            let prefix = ["dist-newstyle", "build", platform, compiler, package]
            in case rest of
                "build" : _ : _ -> Just (joinPath prefix)
                kind : component : "build" : _ : _ | kind `elem` ["x", "t", "b", "l"] -> Just (joinPath (prefix <> [kind, component]))
                _ -> Nothing
    _ -> Nothing

compilerSignature :: BS.ByteString -> Bool
compilerSignature bytes = any (`BS.isPrefixOf` bytes)
    [ BS.pack [0x7f, 0x45, 0x4c, 0x46] -- ELF
    , BS.pack [0xcf, 0xfa, 0xed, 0xfe] -- Mach-O 64-bit little endian
    , BS.pack [0xce, 0xfa, 0xed, 0xfe] -- Mach-O 32-bit little endian
    , BS.pack [0xfe, 0xed, 0xfa, 0xcf]
    , BS.pack [0xfe, 0xed, 0xfa, 0xce]
    , BS.pack [33, 60, 97, 114, 99, 104, 62, 10] -- archive
    ]

-- | Caller holds the exclusive checkout and repository maintenance leases.
-- Recheck the complete inventory before execution and filesystem identity
-- immediately before each unlink. Non-harness build processes must be stopped.
removeCabalArtifacts :: ([String] -> String -> IO String) -> FilePath -> [(FilePath, Integer)] -> IO ()
removeCabalArtifacts git repository expected = do
    inspected <- inspectCabalArtifacts git repository
    unless (inspected == expected) $ fail "Cabal artifact inventory changed; inspect again"
    -- Invalidate monitors first so an interrupted run also rebuilds normally.
    let (monitors, binaries) = partition (\(name, _) -> takeFileName name == "build" && takeFileName (takeDirectory name) == "cache") expected
    identities <- forM (monitors <> binaries) $ \(relative, bytes) -> do
        status <- Posix.getSymbolicLinkStatus (repository </> relative)
        pure (relative, bytes, Posix.deviceID status, Posix.fileID status)
    forM_ identities $ \(relative, bytes, device, inode) -> do
        requireDirectories repository (splitDirectories (takeDirectory relative))
        status <- Posix.getSymbolicLinkStatus (repository </> relative)
        unless (Posix.isRegularFile status && Posix.linkCount status == 1
            && Posix.deviceID status == device && Posix.fileID status == inode
            && fromIntegral (Posix.fileSize status) == bytes) $
                fail "Cabal artifact identity changed; stopping cleanup"
        Posix.removeLink (repository </> relative)

checkIgnoredPath :: ([String] -> String -> IO String) -> FilePath -> FilePath -> IO ()
checkIgnoredPath git repository name = do
    let relative = dropTrailingPathSeparator name
        components = splitDirectories relative
        reject reason = fail (reason <> ": " <> show name)
    unless (not (null components) && all (`notElem` ["", ".", "..", "/"]) components) $
        reject "unsafe ignored path"
    evidence <- git ["check-ignore", "-v", "-z", "--stdin"] (name <> "\0")
    case nul evidence of
        [source, _, _, _] ->
            unless (takeFileName source == ".gitignore" &&
                not ("/" `isPrefixOf` source) &&
                not (".." `elem` splitDirectories source)) $
                reject "ignored path is not excluded by repository ignore rules"
        _ -> reject "cannot verify ignored path rule"
    if not (null name) && last name == '/' &&
        takeFileName relative `elem`
            ["dist-newstyle", "node_modules", ".venv", "venv", "target",
             ".direnv", "__pycache__", ".pytest_cache", ".mypy_cache",
             ".ruff_cache", ".next", ".nuxt", ".stack-work"]
      then requireDirectories repository components
      else case relative of
        ".haskell-agent" -> do
            requireDirectories repository components
            entries <- Directory.listDirectory (repository </> relative)
            unless (entries == ["settings.json"]) $
                reject "ignored agent directory contains additional state"
            checkDuplicate git repository ".haskell-agent/settings.json"
        ".haskell-agent/settings.json" -> checkDuplicate git repository relative
        "packages/agent-openai/data/models.json" -> checkDuplicate git repository relative
        "packages/agent-openai/data/prompt.md" -> checkDuplicate git repository relative
        "result" -> do
            status <- Posix.getSymbolicLinkStatus (repository </> relative)
            unless (Posix.isSymbolicLink status) $
                reject "ignored result is not a Nix store symlink"
            target <- Posix.readSymbolicLink (repository </> relative)
            unless (takeDirectory target == "/nix/store" && validStoreName (takeFileName target)) $
                reject "ignored result does not point directly into the Nix store"
        _ -> reject "ignored files are not a recognized build/cache directory"

-- Exact duplicates are already retained in the repository's primary checkout.
-- Both copies and every intervening directory must be ordinary filesystem nodes.
checkDuplicate :: ([String] -> String -> IO String) -> FilePath -> FilePath -> IO ()
checkDuplicate git repository relative = do
    common <- trim <$> git ["rev-parse", "--path-format=absolute", "--git-common-dir"] ""
    let primary = takeDirectory common
    unless (takeFileName common == ".git") $
        fail ("cannot locate primary checkout for ignored file: " <> show relative)
    primaryCanonical <- Directory.canonicalizePath primary
    repositoryCanonical <- Directory.canonicalizePath repository
    unless (primaryCanonical /= repositoryCanonical) $
        fail ("ignored file has no independent primary checkout copy: " <> show relative)
    first <- readRegular repository relative
    second <- readRegular primary relative
    unless (first == second) $
        fail ("ignored file differs from primary checkout: " <> show relative)

readRegular :: FilePath -> FilePath -> IO BS.ByteString
readRegular root relative = do
    requireDirectories root (splitDirectories (takeDirectory relative))
    status <- Posix.getSymbolicLinkStatus (root </> relative)
    unless (Posix.isRegularFile status) $
        fail ("ignored duplicate is not a regular file: " <> show relative)
    unless (Posix.fileSize status <= 8 * 1024 * 1024) $
        fail ("ignored duplicate exceeds inspection size limit: " <> show relative)
    BS.readFile (root </> relative)

requireDirectories :: FilePath -> [FilePath] -> IO ()
requireDirectories root components = do
    status <- Posix.getSymbolicLinkStatus root
    unless (Posix.isDirectory status) $
        fail "ignored path root is not an ordinary directory"
    go root components
  where
    go _ [] = pure ()
    go parent (component : rest) = do
        let path = parent </> component
        status <- Posix.getSymbolicLinkStatus path
        unless (Posix.isDirectory status) $
            fail ("ignored path has a non-directory or symlinked parent: " <> show component)
        go path rest

validStoreName :: String -> Bool
validStoreName name = case splitAt 32 name of
    (digest, '-' : suffix) ->
        length digest == 32 && not (null suffix) &&
        all (`elem` ("0123456789abcdfghijklmnpqrsvwxyz" :: String)) digest
    _ -> False

trim :: String -> String
trim = reverse . dropWhile (`elem` ['\r', '\n']) . reverse

nul :: String -> [String]
nul [] = []
nul value = case break (== '\0') value of
    (entry, []) -> [entry]
    (entry, _ : rest) -> entry : nul rest
