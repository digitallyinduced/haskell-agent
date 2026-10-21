-- | Audio stays in the terminal; delegated coding turns use the local service.
module Agent.CLI.Remote.Voice (runRemoteVoice) where

import Agent.Accounts.Auth (LoadedAuth(..), loadDirectOpenAiAuth)
import Agent.Cancel
import Agent.CLI.Interrupt (withTurnCancel)
import Agent.CLI.Remote.Transport
import Agent.CLI.Remote.Turn
import Agent.CLI.Remote.Ui
import Agent.CLI.Remote.History
import Agent.CLI.Voice.Audio (runVoiceAudio)
import Agent.CLI.Voice.Transport
import Agent.Error (ApiError(..))
import Agent.OpenAI.Live
import Agent.OpenAI.Live.Call
import Agent.Provider
import Agent.Runtime.GatewayBoundary
import Agent.Runtime.GatewayClient
import Agent.Runtime.Session (SessionTurn(..))
import Agent.Runtime.Session.Settings (SessionOperation(..))
import Agent.Server.Client (AgentServerClient)
import Control.Concurrent.Async (race_)
import Control.Exception.Safe (bracket_, throwIO)
import Data.IORef
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text qualified as Text

runRemoteVoice :: RemoteUi -> AgentServerClient -> IO ()
runRemoteVoice ui client = do
    session <- readIORef ui.remoteCurrentSession
    scoped <- withGatewayTurnBoundary (GatewayBoundary session.remoteGatewayIdentity) do
        case session.remoteGatewayIdentity of
            Just _ -> loadGatewayCredential >>= \case
                Right (Just gateway) -> pure (Right (runGatewayVoiceConversation
                    gateway.gatewayBaseUrl gateway.gatewayAccessToken defaultLiveConfig))
                _ -> pure (Left (CredentialError "Reconnect the organization gateway before starting voice."))
            Nothing -> loadDirectOpenAiAuth >>= \case
                Right auth | tokenProviderBillingMode auth.loadedTokenProvider == SubscriptionBilled ->
                    getNextToken auth.loadedTokenProvider Nothing >>= \case
                        Right credential -> pure (Right (runCodexVoiceConversation
                            (tokenProvider SubscriptionBilled (\case
                                Nothing -> pure (Right credential)
                                Just _ -> pure (Left (CredentialError "Voice authentication failed."))))
                            defaultLiveConfig))
                        Left err -> pure (Left err)
                _ -> pure (Left (CredentialError "Voice requires a local ChatGPT sign-in."))
    transport <- case scoped of
        Left err -> throwIO (RemoteFailure (renderGatewayBoundaryError err))
        Right (Left err) -> throwIO (RemoteFailure (Text.pack (show err)))
        Right (Right value) -> pure value
    cancel <- newCancelFlag
    let delegate task progress = do
            _ <- progress "The coding agent is working. Complete any approvals in the terminal."
            before <- loadRemoteTranscript client session.remoteId
            runRemoteTurn ui client ConversationTurn task []
            after <- loadRemoteTranscript client session.remoteId
            pure $ maybe "The coding turn finished without a text answer." (Text.take 16000) $
                listToMaybe (reverse (mapMaybe (.turnAssistantText) (map snd (drop (length before) after))))
        notify LiveStarted = remoteNotice ui "Voice connected. Use headphones; Ctrl-C to hang up."
        notify _ = pure ()
    bracket_ (writeIORef ui.remoteCancel (Just cancel)) (writeIORef ui.remoteCancel Nothing) $
        withTurnCancel ui.remoteInterrupt cancel $
            runLiveCallWith transport delegate notify
                (\call -> race_ (waitCancel cancel >> stopLiveCall call) (runVoiceAudio call))
                >>= either (const (remoteFailure ui "Voice call failed. Check sign-in, microphone permission and audio devices."))
                    (const (remoteNotice ui "Voice call ended."))
