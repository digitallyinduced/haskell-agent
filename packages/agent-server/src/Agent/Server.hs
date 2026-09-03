-- | Public entry point for the local agent server.
module Agent.Server
    ( runServer
    , module Agent.Server.Application
    , module Agent.Server.Backend
    , module Agent.Server.Config
    , module Agent.Server.Supervisor
    , module Agent.Server.Tenant
    , module Agent.Server.Types
    ) where

import Agent.Server.Application
import Agent.Server.Backend
import Agent.Server.Config
import Agent.Server.Runtime
    ( closeServerRuntime
    , openServerRuntime
    , serverRuntimeBackend
    )
import Agent.Server.Supervisor
import Agent.Server.Tenant hiding (resolveTenantWorkspacePath)
import Agent.Server.Types
import Control.Exception.Safe (bracket)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.String (fromString)
import Paths_agent_server (getDataFileName)
import System.Exit (die)
import Network.Wai.Handler.Warp
    ( defaultSettings
    , runSettings
    , setGracefulShutdownTimeout
    , setHost
    , setPort
    , setTimeout
    )

runServer :: IO ()
runServer = do
    rawConfig <- parseServerConfig
    resolveServerConfig rawConfig >>= \case
        Left err -> die ("agent-server: " <> show err)
        Right config -> do
            openApiPath <- getDataFileName "openapi.json"
            openApi <- LazyByteString.readFile openApiPath
            openServerRuntime config >>= \case
                Left err -> die ("agent-server: " <> show err)
                Right runtime ->
                    bracket
                        (pure runtime)
                        closeServerRuntime
                        \ownedRuntime -> do
                            let backend =
                                    serverRuntimeBackend ownedRuntime
                                Backend
                                    { backendTurnBoundaryGuard =
                                        turnBoundaryGuard
                                    , backendRunTurn = runTurn
                                    } = backend
                                supervisorConfig = SupervisorConfig
                                    { supervisorMaxConcurrentTurns =
                                        config.resolvedMaxConcurrentTurns
                                    , supervisorMaxConcurrentTurnsPerTenant =
                                        config.resolvedMaxConcurrentTurnsPerTenant
                                    , supervisorMaxQueuedTurns =
                                        config.resolvedMaxQueuedTurns
                                    , supervisorMaxQueuedTurnsPerTenant =
                                        config.resolvedMaxQueuedTurnsPerTenant
                                    , supervisorMaxEventSubscribers =
                                        config.resolvedMaxEventSubscribers
                                    , supervisorMaxEventSubscribersPerTenant =
                                        config.resolvedMaxEventSubscribersPerTenant
                                    , supervisorEventReplayLimit =
                                        config.resolvedEventReplayLimit
                                    }
                            bracket
                                (newSupervisorWithBoundaryGuard
                                    supervisorConfig
                                    turnBoundaryGuard
                                    runTurn)
                                closeSupervisor
                                \supervisor -> do
                                    application <-
                                        newApplication
                                            ApplicationConfig
                                                { applicationMaximumRequestBytes =
                                                    config.resolvedMaximumRequestBytes
                                                , applicationOpenApiDocument =
                                                    openApi
                                                }
                                            config.resolvedAuth
                                            backend
                                            supervisor
                                    putStrLn
                                        ("agent-server listening on http://"
                                            <> config.resolvedHost
                                            <> ":"
                                            <> show config.resolvedPort)
                                    runSettings
                                        ( setGracefulShutdownTimeout (Just 10)
                                            $ setTimeout 30
                                            $ setHost
                                                (fromString
                                                    config.resolvedHost)
                                            $ setPort
                                                config.resolvedPort
                                                defaultSettings
                                        )
                                        application
