-- | Typed, revision-checked administration of the machine-wide MCP catalog.
--
-- Environment values are accepted on writes but deliberately absent from
-- 'McpAdminServer': callers can inspect only their key names.
module Agent.CLI.McpAdmin
    ( McpAdminError(..)
    , McpAdminSnapshot(..)
    , McpAdminServer(..)
    , McpAdminServerInput(..)
    , addMcpAdminServer
    , editMcpAdminServer
    , listMcpAdminServers
    , readMcpAdminServer
    , removeMcpAdminServer
    , restartMcpAdminServer
    , setMcpAdminServerEnabled
    ) where

import Agent.CLI.Config
    ( HarnessConfig(..)
    , McpServerConfig(..)
    , harnessConfigPath
    , loadHarnessConfig
    , saveHarnessConfig
    )
import Agent.CLI.PrivateFileLock (withPrivateFileLock)
import Control.Monad (when)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word64)
import System.Directory.OsPath (doesFileExist, getModificationTime)
import System.OsPath
    ( OsPath
    , unsafeEncodeUtf
    , (</>)
    )

data McpAdminError
    = McpAdminConflict !Word64
    | McpAdminNotFound !Text
    | McpAdminAlreadyExists !Text
    | McpAdminInvalid !Text
    deriving (Eq, Show)

data McpAdminSnapshot a = McpAdminSnapshot
    { mcpAdminRevision :: !Word64
    , mcpAdminValue :: !a
    }
    deriving (Eq, Show)

data McpAdminServer = McpAdminServer
    { mcpAdminName :: !Text
    , mcpAdminEnabled :: !Bool
    , mcpAdminCommand :: !Text
    , mcpAdminArgs :: ![Text]
    , mcpAdminCwd :: !(Maybe Text)
    , mcpAdminEnvKeys :: ![Text]
    , mcpAdminStartupTimeoutSeconds :: !Int
    , mcpAdminRequestTimeoutSeconds :: !Int
    }
    deriving (Eq, Show)

data McpAdminServerInput = McpAdminServerInput
    { mcpAdminInputCommand :: !Text
    , mcpAdminInputArgs :: ![Text]
    , mcpAdminInputCwd :: !(Maybe Text)
    , mcpAdminInputEnv :: !(Map Text Text)
    , mcpAdminInputStartupTimeoutSeconds :: !Int
    , mcpAdminInputRequestTimeoutSeconds :: !Int
    }
    deriving (Eq)

instance Show McpAdminServerInput where
    show input =
        "McpAdminServerInput { mcpAdminInputCommand = "
            <> show input.mcpAdminInputCommand
            <> ", mcpAdminInputArgs = "
            <> show input.mcpAdminInputArgs
            <> ", mcpAdminInputCwd = "
            <> show input.mcpAdminInputCwd
            <> ", mcpAdminInputEnv = <redacted:"
            <> show (Map.size input.mcpAdminInputEnv)
            <> " entries>, mcpAdminInputStartupTimeoutSeconds = "
            <> show input.mcpAdminInputStartupTimeoutSeconds
            <> ", mcpAdminInputRequestTimeoutSeconds = "
            <> show input.mcpAdminInputRequestTimeoutSeconds
            <> " }"

listMcpAdminServers
    :: OsPath -> IO (Either McpAdminError (McpAdminSnapshot [McpAdminServer]))
listMcpAdminServers home =
    loadSnapshot home \config ->
        [ publicServer name server
        | (name, server) <- Map.toAscList config.configMcpServers
        ]

readMcpAdminServer
    :: OsPath
    -> Text
    -> IO (Either McpAdminError (McpAdminSnapshot McpAdminServer))
readMcpAdminServer home name = do
    loaded <- loadSnapshot home (.configMcpServers)
    pure do
        snapshot <- loaded
        server <- maybe
            (Left (McpAdminNotFound name))
            Right
            (Map.lookup name snapshot.mcpAdminValue)
        pure snapshot
            { mcpAdminValue = publicServer name server }

addMcpAdminServer
    :: OsPath
    -> Word64
    -> Text
    -> McpAdminServerInput
    -> IO (Either McpAdminError (McpAdminSnapshot McpAdminServer))
addMcpAdminServer home expected name input =
    mutate home expected \config -> do
        when (Map.member name config.configMcpServers) $
            Left (McpAdminAlreadyExists name)
        let server = inputServer True input
        pure
            ( config
                { configMcpServers =
                    Map.insert name server config.configMcpServers
                }
            , publicServer name server
            )

-- | Validate a restart request against the current catalog. The native engine
-- performs the process-cache restart after this check succeeds.
restartMcpAdminServer
    :: OsPath
    -> Word64
    -> Text
    -> IO (Either McpAdminError (McpAdminSnapshot McpAdminServer))
restartMcpAdminServer home expected name =
    withPrivateFileLock (adminLockPath home) do
        readMcpAdminServer home name >>= \case
            Left err -> pure (Left err)
            Right snapshot
                | snapshot.mcpAdminRevision /= expected ->
                    pure (Left (McpAdminConflict snapshot.mcpAdminRevision))
                | otherwise -> pure (Right snapshot)

editMcpAdminServer
    :: OsPath
    -> Word64
    -> Text
    -> McpAdminServerInput
    -> IO (Either McpAdminError (McpAdminSnapshot McpAdminServer))
editMcpAdminServer home expected name input =
    mutate home expected \config -> do
        existing <- maybe
            (Left (McpAdminNotFound name))
            Right
            (Map.lookup name config.configMcpServers)
        let server = inputServer existing.mcpEnabled input
        pure
            ( config
                { configMcpServers =
                    Map.insert name server config.configMcpServers
                }
            , publicServer name server
            )

setMcpAdminServerEnabled
    :: OsPath
    -> Word64
    -> Text
    -> Bool
    -> IO (Either McpAdminError (McpAdminSnapshot McpAdminServer))
setMcpAdminServerEnabled home expected name enabled =
    mutate home expected \config -> do
        existing <- maybe
            (Left (McpAdminNotFound name))
            Right
            (Map.lookup name config.configMcpServers)
        let server = existing { mcpEnabled = enabled }
        pure
            ( config
                { configMcpServers =
                    Map.insert name server config.configMcpServers
                }
            , publicServer name server
            )

removeMcpAdminServer
    :: OsPath
    -> Word64
    -> Text
    -> IO (Either McpAdminError (McpAdminSnapshot ()))
removeMcpAdminServer home expected name =
    mutate home expected \config -> do
        when (not (Map.member name config.configMcpServers)) $
            Left (McpAdminNotFound name)
        pure
            ( config
                { configMcpServers =
                    Map.delete name config.configMcpServers
                }
            , ()
            )

loadSnapshot
    :: OsPath
    -> (HarnessConfig -> a)
    -> IO (Either McpAdminError (McpAdminSnapshot a))
loadSnapshot home project =
    loadHarnessConfig home >>= \case
        Left err -> pure (Left (McpAdminInvalid err))
        Right config -> do
            revision <- configRevision home
            pure (Right McpAdminSnapshot
                { mcpAdminRevision = revision
                , mcpAdminValue = project config
                })

mutate
    :: OsPath
    -> Word64
    -> (HarnessConfig -> Either McpAdminError (HarnessConfig, a))
    -> IO (Either McpAdminError (McpAdminSnapshot a))
mutate home expected change =
    withPrivateFileLock (adminLockPath home) do
        loaded <- loadHarnessConfig home
        case loaded of
            Left err -> pure (Left (McpAdminInvalid err))
            Right config -> do
                current <- configRevision home
                if expected /= current
                    then pure (Left (McpAdminConflict current))
                    else case change config of
                        Left err -> pure (Left err)
                        Right (updated, value) ->
                            saveHarnessConfig home updated >>= \case
                                Left err ->
                                    pure (Left (McpAdminInvalid err))
                                Right () -> do
                                    revision <- configRevision home
                                    pure (Right McpAdminSnapshot
                                        { mcpAdminRevision = revision
                                        , mcpAdminValue = value
                                        })

adminLockPath :: OsPath -> OsPath
adminLockPath home =
    home
        </> unsafeEncodeUtf ".haskell-agent"
        </> unsafeEncodeUtf "config.admin.lock"

-- Atomic config replacement changes the inode timestamp. The nanosecond token
-- detects native and existing-CLI edits without deriving any public value from
-- the (potentially secret-bearing) config bytes.
configRevision :: OsPath -> IO Word64
configRevision home = do
    let path = harnessConfigPath home
    exists <- doesFileExist path
    if not exists
        then pure 0
        else do
            modified <- getModificationTime path
            pure . fromIntegral . max (0 :: Integer) . floor $
                utcTimeToPOSIXSeconds modified * 1000000000

publicServer :: Text -> McpServerConfig -> McpAdminServer
publicServer name server = McpAdminServer
    { mcpAdminName = name
    , mcpAdminEnabled = server.mcpEnabled
    , mcpAdminCommand = server.mcpCommand
    , mcpAdminArgs = server.mcpArgs
    , mcpAdminCwd = server.mcpCwd
    , mcpAdminEnvKeys = Map.keys server.mcpEnv
    , mcpAdminStartupTimeoutSeconds = server.mcpStartupTimeoutSeconds
    , mcpAdminRequestTimeoutSeconds = server.mcpRequestTimeoutSeconds
    }

inputServer :: Bool -> McpAdminServerInput -> McpServerConfig
inputServer enabled input = McpServerConfig
    { mcpEnabled = enabled
    , mcpCommand = Text.strip input.mcpAdminInputCommand
    , mcpArgs = input.mcpAdminInputArgs
    , mcpCwd = input.mcpAdminInputCwd
    , mcpEnv = input.mcpAdminInputEnv
    , mcpStartupTimeoutSeconds =
        input.mcpAdminInputStartupTimeoutSeconds
    , mcpRequestTimeoutSeconds =
        input.mcpAdminInputRequestTimeoutSeconds
    }
