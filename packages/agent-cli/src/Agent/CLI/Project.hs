-- | Project-scoped settings under @<project>/.haskell-agent/settings.json@.
module Agent.CLI.Project
    ( ProjectModel(..)
    , ProjectSettings(..)
    , defaultProjectSettings
    , loadProjectSettings
    , projectDialectFor
    , projectModelFor
    , projectModelProvider
    , projectSettingsPath
    , resolveProjectRoot
    , saveProjectAutoApprove
    , saveProjectModel
    ) where

import Agent.FileRetry (retryOnFileBusy, writeLazyFileAtomically)
import Agent.Dialect
    ( DialectId
    , dialectSlug
    , legacyDialectIdForProvider
    , parseDialect
    , providerSupportsDialect
    )
import Agent.OsPath (unsafeToFilePath)
import Agent.Provider (Provider, parseProvider, providerSlug)
import Control.Exception.Safe (tryIO)
import Control.Monad (unless)
import Data.Aeson
    ( FromJSON(..)
    , ToJSON(..)
    , object
    , withObject
    , (.:)
    , (.:?)
    , (.=)
    )
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory.OsPath
    ( canonicalizePath
    , createDirectoryIfMissing
    , doesFileExist
    )
import System.Exit (ExitCode(..))
import System.OsPath (OsPath, unsafeEncodeUtf, (</>))
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

data ProjectModel = ProjectModel
    { projectModelProvider :: !Provider
    , projectModelName :: !Text
    , projectModelTransportName :: !(Maybe Text)
    , projectModelDialect :: !DialectId
    } deriving (Eq, Show)

data ProjectSettings = ProjectSettings
    { settingsVersion :: !Int
    , settingsAutoApprove :: !Bool
    , settingsLastModel :: !(Maybe ProjectModel)
    } deriving (Eq, Show)

defaultProjectSettings :: ProjectSettings
defaultProjectSettings = ProjectSettings
    { settingsVersion = settingsSchemaVersion
    , settingsAutoApprove = False
    , settingsLastModel = Nothing
    }

instance ToJSON ProjectModel where
    toJSON model = object
        [ "provider" .= providerSlug model.projectModelProvider
        , "model" .= model.projectModelName
        , "transportModel" .= model.projectModelTransportName
        , "dialect" .= dialectSlug model.projectModelDialect
        ]

instance FromJSON ProjectModel where
    parseJSON = withObject "ProjectModel" \o -> do
        providerText <- o .: "provider"
        provider <- case parseProvider providerText of
            Just parsed -> pure parsed
            Nothing -> fail ("unknown provider: " <> Text.unpack providerText)
        model <- o .: "model"
        transportModel <- o .:? "transportModel"
        dialectText <- o .:? "dialect"
        dialect <- case dialectText of
            Nothing -> pure (legacyDialectIdForProvider provider)
            Just text -> case parseDialect text of
                Just parsed -> pure parsed
                Nothing -> fail ("unknown dialect: " <> Text.unpack text)
        unless (providerSupportsDialect provider dialect) $
            fail
                ( "dialect "
                    <> Text.unpack (dialectSlug dialect)
                    <> " is incompatible with provider "
                    <> Text.unpack (providerSlug provider)
                )
        if Text.null (Text.strip model)
            then fail "model must not be empty"
            else pure ProjectModel
                { projectModelProvider = provider
                , projectModelName = model
                , projectModelTransportName = transportModel
                , projectModelDialect = dialect
                }

instance ToJSON ProjectSettings where
    toJSON settings = object
        [ "version" .= settings.settingsVersion
        , "autoApprove" .= settings.settingsAutoApprove
        , "lastModel" .= settings.settingsLastModel
        ]

instance FromJSON ProjectSettings where
    parseJSON = withObject "ProjectSettings" \o -> do
        version <- fromMaybe settingsSchemaVersion <$> o .:? "version"
        autoApprove <- fromMaybe False <$> o .:? "autoApprove"
        lastModelValue <- o .:? "lastModel"
        pure ProjectSettings
            { settingsVersion = version
            , settingsAutoApprove = autoApprove
            -- A malformed or obsolete model selection should not discard
            -- unrelated project settings such as auto-approve.
            , settingsLastModel =
                lastModelValue >>= parseMaybe parseJSON
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
            result <- tryIO (retryOnFileBusy (LBS.readFile (unsafeToFilePath path)))
            pure $ case result of
                Left _ -> defaultProjectSettings
                Right bytes ->
                    case Aeson.eitherDecode' bytes of
                        Left _ -> defaultProjectSettings
                        Right settings -> settings

-- | Persist the project-wide auto-approve flag, creating @.haskell-agent@ as needed.
saveProjectAutoApprove :: OsPath -> Bool -> IO ()
saveProjectAutoApprove projectRoot autoApprove =
    updateProjectSettings projectRoot \settings ->
        settings { settingsAutoApprove = autoApprove }

-- | Remember the most recently selected provider/model pair for this project.
saveProjectModel :: OsPath -> Provider -> Text -> Text -> DialectId -> IO ()
saveProjectModel projectRoot provider model transportModel dialect =
    updateProjectSettings projectRoot \settings ->
        settings
            { settingsLastModel = Just ProjectModel
                { projectModelProvider = provider
                , projectModelName = model
                , projectModelTransportName = Just transportModel
                , projectModelDialect = dialect
                }
            }

projectModelProvider :: ProjectSettings -> Maybe Provider
projectModelProvider settings =
    (.projectModelProvider) <$> settings.settingsLastModel

-- | Return the remembered model only when it belongs to the active provider.
projectModelFor :: Provider -> ProjectSettings -> Maybe Text
projectModelFor provider settings = do
    remembered <- settings.settingsLastModel
    if remembered.projectModelProvider == provider
        then Just remembered.projectModelName
        else Nothing

projectDialectFor :: Provider -> ProjectSettings -> Maybe DialectId
projectDialectFor provider settings = do
    remembered <- settings.settingsLastModel
    if remembered.projectModelProvider == provider
        then Just remembered.projectModelDialect
        else Nothing

updateProjectSettings
    :: OsPath
    -> (ProjectSettings -> ProjectSettings)
    -> IO ()
updateProjectSettings projectRoot update = do
    let dir = projectRoot </> unsafeEncodeUtf ".haskell-agent"
        path = projectSettingsPath projectRoot
    settings <- update <$> loadProjectSettings projectRoot
    createDirectoryIfMissing True dir
    _ <- tryIO (setFileMode (unsafeToFilePath dir) 0o700)
    writeLazyFileAtomically path 0o600 (Aeson.encode settings)

gitToplevel :: OsPath -> IO (Maybe OsPath)
gitToplevel dir = do
    result <- tryIO $
        readCreateProcessWithExitCode
            (proc "git" ["rev-parse", "--show-toplevel"])
                { cwd = Just (unsafeToFilePath dir) }
            ""
    pure $ case result of
        Left _ -> Nothing
        Right (ExitSuccess, out, _) ->
            let trimmed = trim out
            in if null trimmed then Nothing else Just (unsafeEncodeUtf trimmed)
        Right _ -> Nothing

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
