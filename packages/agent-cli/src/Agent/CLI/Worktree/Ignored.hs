-- | Narrow exceptions for disposable or independently retained ignored paths.
-- Never treat an arbitrary ignored directory as disposable user data.
module Agent.CLI.Worktree.Ignored (checkIgnoredPath) where

import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.List (isPrefixOf)
import qualified System.Directory as Directory
import System.FilePath ((</>), dropTrailingPathSeparator, takeFileName, takeDirectory, splitDirectories)
import qualified System.Posix.Files as Posix

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
