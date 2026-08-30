-- | Project-scoped settings under @<project>/.haskell-agent/settings.json@,
-- plus the user-level last model under @~/.haskell-agent/settings.json@.
module Agent.CLI.Project
    ( ModelSwitchScope(..)
    , ProjectAccount(..)
    , ProjectModel(..)
    , ProjectSettings(..)
    , defaultProjectSettings
    , loadProjectSettings
    , loadUserSettings
    , projectDialectFor
    , projectAccountFor
    , projectModelFor
    , projectModelProvider
    , projectSettingsPath
    , resolveProjectRoot
    , saveProjectAutoApprove
    , saveProjectMaxConcurrentAgents
    , saveProjectAccount
    , saveProjectModel
    , persistModelSwitch
    , userSettingsPath
    , withInheritedLastModel
    ) where

import Agent.FileRetry (retryOnFileBusy, writeLazyFileAtomically)
import Agent.CLI.Json (decodeLazy)
import Agent.Json.Decode (defaultKey, optionalKey)
import Agent.Json.Decode qualified as Hermes
import Agent.CLI.Models (ModelTarget(..))
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
    ( ToJSON(..)
    , object
    , (.=)
    )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe, mapMaybe)
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

-- | @~/.haskell-agent/settings.json@ given the user's home directory.
userSettingsPath :: OsPath -> OsPath
userSettingsPath = projectSettingsPath

data ProjectModel = ProjectModel
    { projectModelTarget :: !ModelTarget
    } deriving (Eq, Show)

-- | Stable account identity remembered for one project/provider.  Secrets and
-- usage responses are deliberately excluded.
data ProjectAccount = ProjectAccount
    { projectAccountProvider :: !Provider
    , projectAccountSelectionId :: !Text
    , projectAccountId :: !Text
    } deriving (Eq, Show)

data ProjectSettings = ProjectSettings
    { settingsVersion :: !Int
    , settingsAutoApprove :: !Bool
    , settingsLastModel :: !(Maybe ProjectModel)
    , settingsLastAccounts :: ![ProjectAccount]
    , settingsMaxConcurrentAgents :: !(Maybe Int)
    } deriving (Eq, Show)

defaultProjectSettings :: ProjectSettings
defaultProjectSettings = ProjectSettings
    { settingsVersion = settingsSchemaVersion
    , settingsAutoApprove = False
    , settingsLastModel = Nothing
    , settingsLastAccounts = []
    , settingsMaxConcurrentAgents = Nothing
    }

instance ToJSON ProjectAccount where
    toJSON account = object
        [ "provider" .= providerSlug account.projectAccountProvider
        , "selectionId" .= account.projectAccountSelectionId
        , "accountId" .= account.projectAccountId
        ]

projectAccountDecoder :: Hermes.Decoder ProjectAccount
projectAccountDecoder = Hermes.object do
        providerText <- Hermes.atKey "provider" Hermes.text
        provider <- case parseProvider providerText of
            Just parsed -> pure parsed
            Nothing -> fail ("unknown provider: " <> Text.unpack providerText)
        selectionId <- Hermes.atKey "selectionId" Hermes.text
        accountId <- defaultKey "" "accountId" Hermes.text
        if Text.null (Text.strip selectionId)
            then fail "account selection id must not be empty"
            else pure ProjectAccount
                { projectAccountProvider = provider
                , projectAccountSelectionId = selectionId
                , projectAccountId = accountId
                }

instance ToJSON ProjectModel where
    toJSON model = object
        [ "provider" .= providerSlug target.targetProvider
        , "connection" .= target.targetConnectionId
        , "model" .= target.targetModelId
        , "transportModel" .= Just target.targetWireModelId
        , "dialect" .= dialectSlug target.targetDialect
        ]
      where
        target = model.projectModelTarget

projectModelDecoder :: Hermes.Decoder ProjectModel
projectModelDecoder = Hermes.object do
        providerText <- Hermes.atKey "provider" Hermes.text
        provider <- case parseProvider providerText of
            Just parsed -> pure parsed
            Nothing -> fail ("unknown provider: " <> Text.unpack providerText)
        model <- Hermes.atKey "model" Hermes.text
        connection <- fromMaybe (providerSlug provider)
            <$> optionalKey "connection" Hermes.text
        transportModel <- fromMaybe model
            <$> optionalKey "transportModel" Hermes.text
        dialectText <- optionalKey "dialect" Hermes.text
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
        if Text.null (Text.strip connection)
            then fail "connection must not be empty"
            else if Text.null (Text.strip model)
                then fail "model must not be empty"
                else pure ProjectModel
                { projectModelTarget = ModelTarget
                    { targetProvider = provider
                    , targetConnectionId = connection
                    , targetModelId = model
                    , targetWireModelId = transportModel
                    , targetDialect = dialect
                    }
                }

instance ToJSON ProjectSettings where
    toJSON settings = object
        [ "version" .= settings.settingsVersion
        , "autoApprove" .= settings.settingsAutoApprove
        , "lastModel" .= settings.settingsLastModel
        , "lastAccounts" .= settings.settingsLastAccounts
        , "maxConcurrentAgents" .= settings.settingsMaxConcurrentAgents
        ]

projectSettingsDecoder :: Hermes.Decoder ProjectSettings
projectSettingsDecoder = Hermes.object do
        version <- defaultKey settingsSchemaVersion "version" Hermes.int
        autoApprove <- defaultKey False "autoApprove" Hermes.bool
        lastModelValue <- optionalKey "lastModel" (lenient projectModelDecoder)
        lastAccountsValue <- defaultKey [] "lastAccounts"
            (Hermes.list (lenient projectAccountDecoder))
        maxConcurrentAgents <- optionalKey "maxConcurrentAgents" Hermes.int
        pure ProjectSettings
            { settingsVersion = version
            , settingsAutoApprove = autoApprove
            -- A malformed or obsolete model selection should not discard
            -- unrelated project settings such as auto-approve.
            , settingsLastModel = lastModelValue >>= id
            , settingsLastAccounts = mapMaybe id lastAccountsValue
            , settingsMaxConcurrentAgents = maxConcurrentAgents
            }

lenient :: Hermes.Decoder a -> Hermes.Decoder (Maybe a)
lenient decoder =
    Hermes.withOwnedRawJson \raw ->
        pure $ either (const Nothing) Just
            (Hermes.decodeEither decoder raw)

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
                    case decodeLazy projectSettingsDecoder bytes of
                        Left _ -> defaultProjectSettings
                        Right settings -> settings

-- | User-level settings under @~/.haskell-agent/settings.json@.
loadUserSettings :: OsPath -> IO ProjectSettings
loadUserSettings = loadProjectSettings

-- | Prefer the checkout's last model. A missing checkout value falls back to
-- the user-level last model so a freshly created worktree inherits the model
-- from the previous session instead of catalog or auth defaults.
withInheritedLastModel
    :: ProjectSettings
    -> ProjectSettings
    -> ProjectSettings
withInheritedLastModel project user =
    case project.settingsLastModel of
        Just _ -> project
        Nothing -> project { settingsLastModel = user.settingsLastModel }

-- | Persist the project-wide auto-approve flag, creating @.haskell-agent@ as needed.
saveProjectAutoApprove :: OsPath -> Bool -> IO ()
saveProjectAutoApprove projectRoot autoApprove =
    updateProjectSettings projectRoot \settings ->
        settings { settingsAutoApprove = autoApprove }

-- | Persist the project's concurrent subagent cap.
saveProjectMaxConcurrentAgents :: OsPath -> Int -> IO ()
saveProjectMaxConcurrentAgents projectRoot limit =
    updateProjectSettings projectRoot \settings ->
        settings { settingsMaxConcurrentAgents = Just (max 1 limit) }

-- | Remember the last successfully used account for a provider.
saveProjectAccount
    :: OsPath
    -> Provider
    -> Text
    -> Text
    -> IO ()
saveProjectAccount projectRoot provider selectionId accountId =
    updateProjectSettings projectRoot \settings ->
        settings
            { settingsLastAccounts =
                ProjectAccount
                    { projectAccountProvider = provider
                    , projectAccountSelectionId = selectionId
                    , projectAccountId = accountId
                    }
                    : filter
                        ((/= provider) . (.projectAccountProvider))
                        settings.settingsLastAccounts
            }

projectAccountFor
    :: Provider
    -> ProjectSettings
    -> Maybe ProjectAccount
projectAccountFor provider settings =
    case filter
        ((== provider) . (.projectAccountProvider))
        settings.settingsLastAccounts of
        account : _ -> Just account
        [] -> Nothing

-- | Remember the most recently selected provider/model pair for this project.
saveProjectModel
    :: OsPath
    -> ModelTarget
    -> IO ()
saveProjectModel projectRoot target =
    updateProjectSettings projectRoot \settings ->
        settings
            { settingsLastModel = Just ProjectModel
                { projectModelTarget = target }
            }

-- | Whether a live model/provider switch may update inherited settings.
-- Startup and resume targets are not switch events and must not be persisted;
-- delegated/background transitions use 'SessionLocalSwitch'.
data ModelSwitchScope
    = TopLevelSwitch
    | SessionLocalSwitch
    deriving (Eq, Show)

-- | Persist a top-level switch on the current checkout and as the user-level
-- default. Session-local switches retain their target only in session state.
persistModelSwitch
    :: ModelSwitchScope
    -> OsPath
    -- ^ User home (@~/.haskell-agent/settings.json@).
    -> OsPath
    -- ^ Checkout root.
    -> ModelTarget
    -> IO ()
persistModelSwitch SessionLocalSwitch _ _ _ = pure ()
persistModelSwitch TopLevelSwitch home projectRoot target = do
    saveProjectModel projectRoot target
    saveProjectModel home target

projectModelProvider :: ProjectSettings -> Maybe Provider
projectModelProvider settings =
    (.projectModelTarget.targetProvider) <$> settings.settingsLastModel

-- | Return the remembered model only when it belongs to the active provider.
projectModelFor :: Provider -> ProjectSettings -> Maybe Text
projectModelFor provider settings = do
    remembered <- settings.settingsLastModel
    if remembered.projectModelTarget.targetProvider == provider
        then Just remembered.projectModelTarget.targetModelId
        else Nothing

projectDialectFor :: Provider -> ProjectSettings -> Maybe DialectId
projectDialectFor provider settings = do
    remembered <- settings.settingsLastModel
    if remembered.projectModelTarget.targetProvider == provider
        then Just remembered.projectModelTarget.targetDialect
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
