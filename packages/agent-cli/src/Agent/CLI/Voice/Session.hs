-- | Voice calls occupy the REPL command scope; delegated turns are therefore
-- serialized with typed turns and retain the existing tools and approvals.
module Agent.CLI.Voice.Session (runTerminalVoiceCall, runSessionVoiceCall) where

import Agent.Cancel (resetCancel, waitCancel)
import Agent.CLI.Auth (LoadedAuth(..), loadDirectOpenAiAuth)
import Agent.CLI.ActiveAccount (ActiveAccount(..), readActiveAccount)
import Agent.CLI.GatewayBoundary (GatewayBoundary(..), withGatewayTurnBoundary, renderGatewayBoundaryError)
import Agent.CLI.GatewayClient (GatewayCredential(..), loadGatewayCredential, validateGatewayCredential)
import Agent.CLI.Interrupt (withTurnCancel)
import Agent.CLI.ProviderTransition (TurnResult(..))
import Agent.CLI.Render (RenderConfig(..), putTextLn)
import Agent.CLI.SessionEnv (SessionEnv(..))
import Agent.CLI.TUI.App (emitUiEvent)
import Agent.CLI.Turn (runOneTurn)
import Agent.CLI.Voice.Audio (runVoiceAudio)
import Agent.CLI.Voice.Transport (runCodexVoiceConversation, runGatewayVoiceConversation)
import Agent.Error (ApiError(..))
import Agent.Loop (LoopConfig(..), TurnInput(..))
import Agent.OpenAI.Live
import Agent.OpenAI.Live.Call
import qualified Agent.OpenAI.Auth as OpenAI
import Agent.Provider
    ( Credential(..), Provider(..), BillingMode(..), TokenProvider
    , getNextToken, tokenProvider, tokenProviderBillingMode )
import Agent.Responses.Types
import qualified Agent.Runtime.SessionState as State
import Agent.TUI.Model (UiEvent(..))
import Control.Concurrent.Async (race_)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

runTerminalVoiceCall :: SessionEnv -> IO ()
runTerminalVoiceCall env = do
    result <- runSessionVoiceCall env runVoiceAudio announce
    case result of
        Left (CredentialError message) -> announce message
        _ -> pure ()
  where
    announce message = case env.sessionFullscreen of
        Just runtime -> emitUiEvent runtime (UiSystemMessage message)
        Nothing -> putTextLn env.sessionRender.renderStdout message

-- | The caller owns admission/serialization. Native hosts supply scoped audio
-- devices; network credentials and coding tools remain in Haskell.
runSessionVoiceCall :: SessionEnv -> (LiveCall -> IO ()) -> (Text -> IO ()) -> IO (Either ApiError ())
runSessionVoiceCall env devices announce = do
    -- Snapshot the admitted credential, but do not hold a turn lease across
    -- delegation: runOneTurn takes its own lease and could otherwise deadlock
    -- behind a waiting credential writer. The socket pins this credential;
    -- delegated turns independently revalidate the session boundary.
    scoped <- withGatewayTurnBoundary (GatewayBoundary env.sessionGatewayIdentity) resolveTransport
    case scoped of
        Left problem -> pure (Left (CredentialError (renderGatewayBoundaryError problem)))
        Right resolved -> runResolved resolved
  where
    resolveTransport = case env.sessionGatewayIdentity of
        Just _ -> loadGatewayCredential >>= \case
            Right (Just gateway) -> case validateGatewayCredential gateway of
                Left problem -> pure (Left (CredentialError problem))
                Right () -> pure (Right
                    (runGatewayVoiceConversation gateway.gatewayBaseUrl gateway.gatewayAccessToken defaultLiveConfig,
                     "organization gateway"))
            _ -> pure (Left (CredentialError "The session gateway credential is unavailable; reconnect to the gateway."))
        Nothing -> fmap (fmap (\(provider, label) ->
            (runCodexVoiceConversation provider defaultLiveConfig, label))) (resolveVoiceAccount env)
    runResolved = \case
        Left err -> pure (Left (voiceCredentialError err))
        Right (transport, label) -> do
            let cancel = env.sessionLoop.loopCancel
            resetCancel cancel
            announce ("Connecting voice via " <> label <> "…")
            result <- withTurnCancel env.sessionInterrupt cancel $
                runLiveCallWith transport delegate notify
                    (\call -> race_ (waitCancel cancel >> stopLiveCall call) (devices call))
            case result of
                Left (CredentialError _) -> pure ()
                -- The scoped voice transports construct these diagnostics
                -- locally; they exclude upstream bodies, SDP and exceptions.
                Left (ConnectionError message) -> announce ("Voice call failed: " <> message)
                Left _ -> announce "Voice call failed. Check OpenAI Live model access, microphone permission, and audio device availability."
                Right () -> announce "Voice call ended."
            pure result
    notify LiveStarted = announce "Voice connected. Use headphones; Ctrl-C or Stop to hang up."
    notify _ = pure ()
    delegate :: Text -> (Text -> IO ()) -> IO Text
    delegate task progress = do
        before <- State.readSessionTranscript env.sessionState
        progress "The coding agent is working. Any required approvals must be completed in the application."
        result <- runOneTurn env task [UserMessage task]
        case result of
            TurnSucceeded -> do
                after <- State.readSessionTranscript env.sessionState
                -- Only newly committed assistant messages, never reasoning or
                -- tool output, may be returned as the delegation's answer.
                pure $ maybe "The coding turn completed without a text answer." (Text.take 16_000) $
                    listToMaybe (reverse
                        [ contentText message.content
                        | item@(MessageItem message) <- after
                        , item `notElem` before
                        , message.role == RoleAssistant
                        , message.phase /= Just "commentary"
                        ])
            TurnCancelled -> pure "The coding task was cancelled."
            _ -> pure "The coding task did not complete. Check the application for details before retrying."
    contentText (MessageContentText value) = value
    contentText (MessageContentParts parts) = Text.intercalate "\n"
        [value | OutputTextPart {text = value} <- parts]

-- | Reuse the live pool, including its cooldown state, and pin the call to
-- the account displayed by the session. A gateway has no local OpenAI pool:
-- never forward its credentials to the Codex endpoint.
resolveVoiceAccount :: SessionEnv -> IO (Either ApiError (TokenProvider, Text))
resolveVoiceAccount env = case (env.sessionProvider, env.sessionOpenAiPool, env.sessionTokenProvider) of
    (OpenAIProvider, Just pool, Just source)
        | tokenProviderBillingMode source == SubscriptionBilled -> do
            account <- readActiveAccount env.sessionAccount
            OpenAI.getAccessTokenForAccount pool account.activeAccountId >>= \case
                Left err -> pure (Left err)
                Right (accessToken, accountId) -> do
                    let credential = Credential { accessToken, accountId, leaseId = Nothing, provider = OpenAIProvider }
                    label <- env.sessionAccountLabel credential
                    pure (Right (pinned credential, label))
    _ -> loadDirectOpenAiAuth >>= \case
        Left _ -> pure (Left (CredentialError "Voice calls require a local ChatGPT sign-in."))
        Right auth
            | tokenProviderBillingMode auth.loadedTokenProvider /= SubscriptionBilled ->
                pure (Left (CredentialError "Voice calls require a local ChatGPT sign-in."))
            | otherwise -> getNextToken auth.loadedTokenProvider Nothing >>= \case
                Left err -> pure (Left err)
                Right credential -> do
                    label <- auth.loadedAccountLabel credential
                    pure (Right (pinned credential, label))
  where
    -- Signaling and sideband must use the same identity; do not rotate on a
    -- second checkout or retry an ambiguously created call on another account.
    pinned credential = tokenProvider SubscriptionBilled \case
        Nothing -> pure (Right credential)
        Just _ -> pure (Left (CredentialError "Voice authentication failed. Reconnect before retrying."))

voiceCredentialError :: ApiError -> ApiError
voiceCredentialError CredentialsExhausted{} = CredentialError
    "The voice account is currently rate-limited or out of usage. Check /usage and select an available ChatGPT account with /accounts."
voiceCredentialError _ = CredentialError
    "Could not acquire the voice account. Check the selected ChatGPT account with /accounts and sign in again if needed."
