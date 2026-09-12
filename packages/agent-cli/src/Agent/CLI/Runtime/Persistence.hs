-- | CLI policy and presentation adapters for shared persistence preparation.
module Agent.CLI.Runtime.Persistence
    ( persistenceRequest
    , announceResumedSession
    , shouldPersist
    ) where

import Agent.Runtime.Models (ModelTarget)
import Agent.CLI.Options (CliOptions(..), isOneShot)
import Agent.CLI.Render (putTextLn)
import Agent.Runtime.Session (SessionMeta(..), SessionTurn)
import Agent.Runtime.Session.Preparation (PersistenceRequest(..))
import Agent.CLI.Session.Runtime.Types (StartupRuntime(..))
import Agent.CLI.Style (glyphSession, roleMuted)
import Agent.CLI.Terminal (resolveColor)
import Agent.CLI.TUI.App (emitUiEvent)
import Agent.Store.Postgres.Connection (StorePool)
import Agent.TUI.Model (UiEvent(..))
import Data.Text (Text)
import System.OsPath (OsPath)

persistenceRequest
    :: StorePool -> CliOptions -> OsPath -> ModelTarget -> Maybe Text -> Bool
    -> OsPath -> Text -> Maybe Text -> Maybe (SessionMeta, [SessionTurn])
    -> PersistenceRequest
persistenceRequest pool options root target gatewayIdentity retargetResumed
        cwd effort prompt resumed =
    PersistenceRequest
        { persistencePool = pool
        , persistenceRoot = root
        , persistenceTarget = target
        , persistenceGatewayIdentity = gatewayIdentity
        , persistenceRetargetResumed = retargetResumed
        , persistenceCwd = cwd
        , persistenceEffort = effort
        , persistencePrompt = prompt
        , persistenceResumed = fst <$> resumed
        , persistenceEnabled = shouldPersist options
        }

announceResumedSession :: StartupRuntime -> SessionMeta -> IO ()
announceResumedSession startup meta = do
    let message = "session: " <> meta.metaId <> " (resumed)"
    case startup.startupFullscreen of
        Nothing -> do
            color <- resolveColor startup.startupStderr
            putTextLn startup.startupStderr
                (roleMuted color (glyphSession <> message))
        Just runtime ->
            emitUiEvent runtime (UiSystemMessage message)

shouldPersist :: CliOptions -> Bool
shouldPersist options = not (isOneShot options) || options.optSaveSession
