-- | Project-scoped settings under @<project>/.haskell-agent/settings.json@.
module Agent.CLI.Project
    ( ProjectSettings(..)
    , defaultProjectSettings
    , loadProjectSettings
    , projectSettingsPath
    , resolveProjectRoot
    , saveProjectAutoApprove
    ) where

import Agent.FileRetry (retryOnFileBusy, writeLazyFileAtomically)
import Control.Exception (try)
import Control.Exception.Safe (impureThrow)
import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe)
import System.Directory.OsPath
    ( canonicalizePath
    , createDirectoryIfMissing
    , doesFileExist
    )
import System.Exit (ExitCode(..))
import System.OsPath (OsPath, decodeUtf, unsafeEncodeUtf, (</>))
import System.Posix.Files (setFileMode)
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode)

settingsSchemaVersion :: Int
settingsSchemaVersion = 1

-- | @dir/.haskell-agent/settings.json@.
projectSettingsPath :: OsPath -> OsPath
projectSettingsPath projectRoot =
    projectRoot
        </> unsafeEncodeUtf ".haskell-agent"
        </> unsafeEncodeUtf "settings.json"

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

-- | Settings root for the checkout that contains @cwd@.
-- Uses @git rev-parse --show-toplevel@ so a linked worktree stays in that
-- worktree instead of jumping to the primary clone. Falls back to @cwd@.
-- Paths are canonicalized so macOS @/var@ vs @/private/var@ does not diverge.
resolveProjectRoot :: OsPath -> IO OsPath
resolveProjectRoot cwd = do
    root <- gitToplevel cwd >>= \case
        Just toplevel -> pure toplevel
        Nothing -> pure cwd
    canonicalizePath root

-- | Missing or unreadable settings files yield the defaults.
loadProjectSettings :: OsPath -> IO ProjectSettings
loadProjectSettings projectRoot = do
    let path = projectSettingsPath projectRoot
    exists <- doesFileExist path
    if not exists
        then pure defaultProjectSettings
        else do
            result <- try @IOError (retryOnFileBusy (LBS.readFile (decodeUtfPath path)))
            pure $ case result of
                Left _ -> defaultProjectSettings
                Right bytes ->
                    case Aeson.eitherDecode' bytes of
                        Left _ -> defaultProjectSettings
                        Right settings -> settings

-- | Persist the project-wide auto-approve flag, creating @.haskell-agent@ as needed.
saveProjectAutoApprove :: OsPath -> Bool -> IO ()
saveProjectAutoApprove projectRoot autoApprove = do
    let dir = projectRoot </> unsafeEncodeUtf ".haskell-agent"
        path = projectSettingsPath projectRoot
        settings = defaultProjectSettings { settingsAutoApprove = autoApprove }
    createDirectoryIfMissing True dir
    _ <- try @IOError (setFileMode (decodeUtfPath dir) 0o700)
    writeLazyFileAtomically path 0o600 (Aeson.encode settings)

gitToplevel :: OsPath -> IO (Maybe OsPath)
gitToplevel dir = do
    result <- try @IOError $
        readCreateProcessWithExitCode
            (proc "git" ["rev-parse", "--show-toplevel"])
                { cwd = Just (decodeUtfPath dir) }
            ""
    pure $ case result of
        Left _ -> Nothing
        Right (ExitSuccess, out, _) ->
            let trimmed = trim out
            in if null trimmed then Nothing else Just (unsafeEncodeUtf trimmed)
        Right _ -> Nothing

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

decodeUtfPath :: OsPath -> FilePath
decodeUtfPath = either impureThrow id . decodeUtf
