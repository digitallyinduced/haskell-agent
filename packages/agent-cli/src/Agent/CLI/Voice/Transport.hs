-- | Codex OAuth signaling plus WebRTC media, adapted to the shared call scope.
module Agent.CLI.Voice.Transport (runCodexVoiceConversation, runGatewayVoiceConversation) where

import Agent.Error (ApiError(..))
import Agent.OpenAI.Live
import Agent.OpenAI.Live.Signaling (withCodexLiveCall, withGatewayLiveCallPreparing)
import Data.Text (Text)
import Agent.Provider (BillingMode(..), TokenProvider, tokenProviderBillingMode)
import qualified Agent.WebRTC as Media
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM
import Control.Exception.Safe (tryAny)
import Control.Monad (forever)
import System.Timeout (timeout)

runCodexVoiceConversation
    :: TokenProvider -> LiveConfig -> IO LiveInput -> (LiveEvent -> IO ())
    -> IO (Either ApiError ())
runCodexVoiceConversation provider config next receive
    | tokenProviderBillingMode provider /= SubscriptionBilled =
        pure (Left (CredentialError "Voice requires a local ChatGPT sign-in, not an API key."))
    | otherwise = runVoiceConversationWith (\prepare use -> prepare >>= \offer -> withCodexLiveCall provider config offer use) next receive

runGatewayVoiceConversation
    :: Text -> Text -> LiveConfig -> IO LiveInput -> (LiveEvent -> IO ())
    -> IO (Either ApiError ())
runGatewayVoiceConversation baseUrl bearer config =
    runVoiceConversationWith (withGatewayLiveCallPreparing baseUrl bearer config)

runVoiceConversationWith
    :: (IO Text -> (Text -> (IO () -> IO LiveInput -> (LiveEvent -> IO ()) -> IO ()) -> IO ()) -> IO (Either ApiError ()))
    -> IO LiveInput -> (LiveEvent -> IO ()) -> IO (Either ApiError ())
runVoiceConversationWith signaling next receive = do
        outcome <- tryAny $ Media.withPeer \peer -> do
            signaling (Media.createOffer peer) \answer sideband -> do
                Media.setRemoteAnswer peer answer
                context <- newTBQueueIO 8
                ready <- newEmptyTMVarIO
                let onSideband LiveStarted = pure ()
                    onSideband (LiveAudio _) = pure () -- audio is exclusively WebRTC
                    onSideband event = receive event
                    control = sideband (atomically (putTMVar ready ()))
                        (atomically (readTBQueue context)) onSideband
                    input = next >>= \case
                        LiveHangUp -> pure ()
                        LiveInputAudio bytes -> Media.pushAudio peer bytes >> input
                        message@(LiveContext _ _ _) -> atomically (writeTBQueue context message) >> input
                    media = do
                        connected <- timeout 20_000_000 do
                            atomically (readTMVar ready)
                            Media.awaitConnected peer
                        case connected of
                            Nothing -> fail "Voice connection timed out"
                            Just () -> receive LiveStarted
                        race_ input (forever (Media.pullAudio peer >>= receive . LiveAudio))
                -- A failure/hangup in either plane joins all other workers.
                race_ control media
        pure $ case outcome of
            Left _ -> Left (ConnectionError "Voice media could not connect or was interrupted.")
            Right result -> result
