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
    , loadHarnessConfigSnapshot
    , modifyHarnessConfig
    , withHarnessConfigSnapshot
    )
import Agent.MCP (McpProtocolPreference(..))
import Control.Monad (when)
import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import System.OsPath (OsPath)

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

-- | Validate a restart request and perform the process-cache restart while
-- holding the shared config lock. A catalog mutation therefore happens wholly
-- before or after the restart's revision check and cannot make the accepted
-- restart stale between validation and its side effect.
restartMcpAdminServer
    :: OsPath
    -> Word64
    -> Text
    -> IO ()
    -> IO (Either McpAdminError (McpAdminSnapshot McpAdminServer))
restartMcpAdminServer home expected name restart =
    withHarnessConfigSnapshot home
        (\current config ->
            if current /= expected
                then pure (Left (McpAdminConflict current))
                else case Map.lookup name config.configMcpServers of
                    Nothing -> pure (Left (McpAdminNotFound name))
                    Just server -> do
                        restart
                        pure (Right McpAdminSnapshot
                            { mcpAdminRevision = current
                            , mcpAdminValue = publicServer name server
                            }))
        >>= \case
            Left err -> pure (Left (McpAdminInvalid err))
            Right result -> pure result

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
        when (existing.mcpUrl /= Nothing) $
            Left (McpAdminInvalid
                "remote HTTP MCP servers cannot be edited through this API")
        let server = existing
                { mcpCommand = Text.strip input.mcpAdminInputCommand
                , mcpArgs = input.mcpAdminInputArgs
                , mcpCwd = input.mcpAdminInputCwd
                , mcpEnv = input.mcpAdminInputEnv
                , mcpStartupTimeoutSeconds =
                    input.mcpAdminInputStartupTimeoutSeconds
                , mcpRequestTimeoutSeconds =
                    input.mcpAdminInputRequestTimeoutSeconds
                }
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
    loadHarnessConfigSnapshot home >>= \case
        Left err -> pure (Left (McpAdminInvalid err))
        Right (revision, config) ->
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
    modifyHarnessConfig home
        (\current config ->
            if expected /= current
                then Left ("conflict:" <> Text.pack (show current))
                else first renderMutationError (change config))
        >>= \case
                Left err -> pure (Left (parseMutationError err))
                Right (revision, _, value) ->
                    pure (Right McpAdminSnapshot
                        { mcpAdminRevision = revision
                        , mcpAdminValue = value
                        })
  where
    renderMutationError = \case
        McpAdminNotFound name -> "not-found:" <> name
        McpAdminAlreadyExists name -> "exists:" <> name
        McpAdminInvalid err -> "invalid:" <> err
        McpAdminConflict revision ->
            "conflict:" <> Text.pack (show revision)
    parseMutationError err
        | Just raw <- Text.stripPrefix "conflict:" err
        , [(revision, "")] <- reads (Text.unpack raw) =
            McpAdminConflict revision
        | Just name <- Text.stripPrefix "not-found:" err =
            McpAdminNotFound name
        | Just name <- Text.stripPrefix "exists:" err =
            McpAdminAlreadyExists name
        | Just invalid <- Text.stripPrefix "invalid:" err =
            McpAdminInvalid invalid
        | otherwise = McpAdminInvalid err

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
    , mcpUrl = Nothing
    , mcpCommand = Text.strip input.mcpAdminInputCommand
    , mcpArgs = input.mcpAdminInputArgs
    , mcpCwd = input.mcpAdminInputCwd
    , mcpEnv = input.mcpAdminInputEnv
    , mcpStartupTimeoutSeconds =
        input.mcpAdminInputStartupTimeoutSeconds
    , mcpRequestTimeoutSeconds =
        input.mcpAdminInputRequestTimeoutSeconds
    , mcpOAuth = Nothing
    , mcpProtocol = McpProtocolAuto
    }
