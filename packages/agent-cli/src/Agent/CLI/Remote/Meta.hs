-- | The configuration planner has no access to the coding conversation.
module Agent.CLI.Remote.Meta (runRemoteMeta) where

import Agent.CLI.MetaConsole
import Agent.CLI.Runtime.MetaConsole (applyMetaConfigActions, collectMetaSecretsWith, isMetaConfigAction)
import Agent.CLI.Remote.Transport
import Agent.CLI.Remote.Turn (runRemoteQuestion)
import Agent.CLI.Remote.Ui
import Agent.CLI.Secret (promptSecretLine)
import Agent.CLI.TUI.App (requestFullscreenSecret)
import Agent.CLI.Login (runLoginManager)
import Agent.CLI.McpCatalog (runMcpCommand)
import Agent.CLI.Options (McpCommand(..))
import Agent.Runtime.Config (updateHarnessConfig)
import Agent.Runtime.Session.Settings (SessionOperation(..))
import Agent.Server.Client (AgentServerClient)
import Control.Exception.Safe (throwIO)
import Control.Monad (forM_, when)
import Data.Aeson (object, (.=))
import Data.IORef (readIORef)
import Data.Text (Text)
import System.Directory.OsPath (getHomeDirectory)

runRemoteMeta :: RemoteUi -> AgentServerClient -> (Text -> IO ()) -> Text -> IO ()
runRemoteMeta ui client command request = do
    session <- readIORef ui.remoteCurrentSession
    models <- remoteModels client
    let context = object
            [ "model" .= session.remoteModel, "effort" .= session.remoteEffort
            , "availableModels" .= map (.remoteModelId) models ]
    let prompt = metaConsoleInstructions <> "\n\n" <> metaConsolePrompt context request
    answer <- runRemoteQuestion ui client PrivateQuestion prompt
    plan <- case decodeMetaPlan answer of
        Right value -> pure value
        Left err -> runRemoteQuestion ui client PrivateQuestion
            (prompt <> "\nThe previous plan was invalid: " <> err
                <> "\nReturn a valid JSON plan.") >>= either (throwIO . RemoteFailure) pure . decodeMetaPlan
    remoteNotice ui plan.metaSummary
    forM_ (metaPlanPreviews plan) (remoteNotice ui)
    approved <- if metaPlanMutates plan
        then (== Just 1) <$> remoteChoice ui "Apply these configuration changes?" "" ["Cancel", "Apply"]
        else pure True
    when approved do
        secrets <- collectMetaSecretsWith secret plan.metaActions >>= either (throwIO . RemoteFailure) pure
        when (any isMetaConfigAction plan.metaActions) do
            home <- getHomeDirectory
            updateHarnessConfig home (applyMetaConfigActions secrets plan.metaActions)
                >>= either (throwIO . RemoteFailure) (const (pure ()))
        forM_ plan.metaActions \action -> case action of
            MetaSessionCommand text -> command text
            MetaConnectAccount _ -> runLoginManager ui.remoteUseColor
            MetaSelectAccount _ _ -> runLoginManager ui.remoteUseColor
            MetaLoginMcpOAuth name -> runMcpCommand (McpLogin name [])
            MetaClarify text -> remoteNotice ui text
            MetaInform text -> remoteNotice ui text
            _ -> pure ()
  where
    secret title body = case ui.remoteFullscreen of
        Just runtime -> requestFullscreenSecret runtime title body
        Nothing -> promptSecretLine ui.remoteStdin title (Just body)
