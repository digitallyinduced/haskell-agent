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
    , installSessionEventSink
    , openServerRuntime
    , serverRuntimeBackend
    )
import Agent.Server.SessionSetup (SessionEventSink (..))
import Agent.Server.Supervisor
import Agent.Server.Tenant hiding (resolveTenantWorkspacePath)
import Agent.Server.Types
import Control.Concurrent (myThreadId, throwTo)
import Control.Exception.Safe (bracket)
import Control.Monad (unless)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.IORef (atomicModifyIORef', newIORef)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.IO qualified as TextIO
import Paths_agent_server (getDataFileName)
import System.Exit (ExitCode (..), die)
import System.IO (stderr)
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
            onTurnOwnerLost <- exitOnTurnOwnerLoss
            openServerRuntime config onTurnOwnerLost >>= \case
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
                                    , backendTurnPersistence =
                                        turnPersistence
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
                                (newSupervisorWithBoundaryGuardAndPersistence
                                    supervisorConfig
                                    turnBoundaryGuard
                                    turnPersistence
                                    runTurn)
                                closeSupervisor
                                \supervisor -> do
                                    installSessionEventSink ownedRuntime $
                                        SessionEventSink
                                            { emitSessionEvent =
                                                \boundary sessionId eventType payload ->
                                                    publishEvent
                                                        supervisor
                                                        boundary
                                                        eventType
                                                        Nothing
                                                        (Just sessionId)
                                                        payload
                                            }
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

{- | Leave the process once a turn owner has lost its liveness fence.

That owner identity must never be revived, so it refuses every new turn for
the rest of the process lifetime; in multi-tenant mode its tenant would stay
unusable until an unrelated restart. The main thread instead unwinds in order
(closing the supervisor and runtimes) and the process exits non-zero, so the
service manager starts a fresh process whose new owner recovers the durable
turns. Only the first loss is acted upon; a second one must not interrupt the
shutdown that is already under way.
-}
exitOnTurnOwnerLoss :: IO (Text -> IO ())
exitOnTurnOwnerLoss = do
    mainThread <- myThreadId
    requested <- newIORef False
    pure \reason -> do
        alreadyRequested <-
            atomicModifyIORef' requested (True,)
        unless alreadyRequested do
            TextIO.hPutStrLn stderr
                ( "agent-server: "
                    <> reason
                    <> "; exiting so the service manager restarts the server"
                    <> " under a fresh turn owner"
                )
            throwTo mainThread (ExitFailure turnOwnerLostExitCode)

-- | EX_TEMPFAIL: the server can work again once it is restarted.
turnOwnerLostExitCode :: Int
turnOwnerLostExitCode = 75
