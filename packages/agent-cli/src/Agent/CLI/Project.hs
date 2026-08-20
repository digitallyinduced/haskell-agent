-- | Project-scoped settings under @<project>/.haskell-agent/settings.json@.
module Agent.CLI.Project
    ( ProjectSettings(..)
    , defaultProjectSettings
    , loadProjectSettings
    , projectSettingsPath
    , resolveProjectRoot
    , saveProjectAutoApprove
    ) where

import Control.Exception (try)
import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isSpace)
import Data.List (dropWhileEnd, stripPrefix)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import System.Directory
    ( canonicalizePath
    , createDirectoryIfMissing
    , doesFileExist
    , removeFile
    , renameFile
    )
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Posix.Files (setFileMode)
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode)

settingsSchemaVersion :: Int
settingsSchemaVersion = 1

-- | @dir/.haskell-agent/settings.json@.
projectSettingsPath :: FilePath -> FilePath
projectSettingsPath projectRoot =
    projectRoot </> ".haskell-agent" </> "settings.json"

data ProjectSettings = ProjectSettings
    { settingsVersion :: !Int
    , settingsAutoApprove :: !Bool
    } deriving (Eq, Show)

defaultProjectSettings :: ProjectSettings
defaultProjectSettings = ProjectSettings
    { settingsVersion = settingsSchemaVersion
    , settingsAutoApprove = False
    }

instance ToJSON ProjectSettings where
    toJSON settings = object
        [ "version" .= settings.settingsVersion
        , "autoApprove" .= settings.settingsAutoApprove
        ]

instance FromJSON ProjectSettings where
    parseJSON = withObject "ProjectSettings" \o -> do
        version <- fromMaybe settingsSchemaVersion <$> o .:? "version"
        autoApprove <- fromMaybe False <$> o .:? "autoApprove"
        pure ProjectSettings
            { settingsVersion = version
            , settingsAutoApprove = autoApprove
            }

-- | Shared settings root for the repo that contains @cwd@.
-- Uses the primary git worktree when available so linked worktrees created with
-- @--worktree@ share @.haskell-agent/settings.json@ with the main checkout.
-- Falls back to the cwd toplevel, then @cwd@ itself. Paths are canonicalized
-- so macOS @/var@ vs @/private/var@ does not diverge.
resolveProjectRoot :: FilePath -> IO FilePath
resolveProjectRoot cwd = do
    root <- gitPrimaryWorktree cwd >>= \case
        Just primary -> pure primary
        Nothing ->
            gitToplevel cwd >>= \case
                Just toplevel -> pure toplevel
                Nothing -> pure cwd
    canonicalizePath root

-- | Missing or unreadable settings files yield the defaults.
loadProjectSettings :: FilePath -> IO ProjectSettings
loadProjectSettings projectRoot = do
    let path = projectSettingsPath projectRoot
    exists <- doesFileExist path
    if not exists
        then pure defaultProjectSettings
        else do
            result <- try @IOError (LBS.readFile path)
            pure $ case result of
                Left _ -> defaultProjectSettings
                Right bytes ->
                    case Aeson.eitherDecode' bytes of
                        Left _ -> defaultProjectSettings
                        Right settings -> settings

-- | Persist the project-wide auto-approve flag, creating @.haskell-agent@ as needed.
saveProjectAutoApprove :: FilePath -> Bool -> IO ()
saveProjectAutoApprove projectRoot autoApprove = do
    let dir = projectRoot </> ".haskell-agent"
        path = projectSettingsPath projectRoot
        tmp = path <> ".tmp"
        settings = defaultProjectSettings { settingsAutoApprove = autoApprove }
    createDirectoryIfMissing True dir
    _ <- try @IOError (setFileMode dir 0o700)
    LBS.writeFile tmp (Aeson.encode settings)
    setFileMode tmp 0o600
    renameOrReplace tmp path
    setFileMode path 0o600

gitToplevel :: FilePath -> IO (Maybe FilePath)
gitToplevel dir = do
    result <- try @IOError $
        readCreateProcessWithExitCode
            (proc "git" ["rev-parse", "--show-toplevel"]) { cwd = Just dir }
            ""
    pure $ case result of
        Left _ -> Nothing
        Right (ExitSuccess, out, _) ->
            let trimmed = trim out
            in if null trimmed then Nothing else Just trimmed
        Right _ -> Nothing

-- | First worktree from @git worktree list --porcelain@ is the primary checkout.
gitPrimaryWorktree :: FilePath -> IO (Maybe FilePath)
gitPrimaryWorktree dir = do
    result <- try @IOError $
        readCreateProcessWithExitCode
            (proc "git" ["worktree", "list", "--porcelain"]) { cwd = Just dir }
            ""
    pure $ case result of
        Left _ -> Nothing
        Right (ExitSuccess, out, _) ->
            listToMaybe (mapMaybe parseWorktreeLine (lines out))
        Right _ -> Nothing

parseWorktreeLine :: String -> Maybe FilePath
parseWorktreeLine line = case stripPrefix "worktree " line of
    Just path | not (null (trim path)) -> Just (trim path)
    _ -> Nothing

renameOrReplace :: FilePath -> FilePath -> IO ()
renameOrReplace tmp path = do
    result <- try @IOError (renameFile tmp path)
    case result of
        Right () -> pure ()
        Left _ -> do
            bytes <- LBS.readFile tmp
            LBS.writeFile path bytes
            _ <- try @IOError (removeFile tmp)
            pure ()

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
