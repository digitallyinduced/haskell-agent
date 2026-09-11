-- | Application-facing call lifetime and bounded audio queues. Device owners
-- run inside the supplied scope and await the server acknowledgement before
-- opening the microphone.
module Agent.OpenAI.Live.Call
    ( LiveCall
    , runLiveCall
    , runLiveCallWith
    , submitLiveAudio
    , submitLiveAudioWhenReady
    , awaitLiveStarted
    , readLiveAudio
    , awaitLivePlaybackReset
    , readLiveAudioGeneration
    , stopLiveCall
    ) where

import Agent.Error (ApiError(..))
import Agent.OpenAI.Live
import Agent.OpenAI.Live.Delegation (withLiveDelegation)
import Agent.Provider (TokenProvider)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM
import Control.Exception.Safe (finally, tryAny)
import Control.Monad (unless, void, when)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

-- Constructors remain private: callers cannot bypass accounting or retain an
-- unbounded queue. Audio is copied at ingress to avoid retaining larger slices.
data LiveCall = LiveCall
    { capture :: !(TBQueue BS.ByteString)
    , captureBytes :: !(TVar Int)
    , playback :: !(TBQueue BS.ByteString)
    , playbackBytes :: !(TVar Int)
    , stopped :: !(TVar Bool)
    , started :: !(TMVar ())
    , playbackGeneration :: !(TVar Integer)
    , inputActive :: !(TVar Bool)
    , assistantGeneration :: !(TVar (Maybe Integer))
    , speakerSuppressed :: !(TVar Bool)
    }

-- | Nonblocking, suitable for a device callback (but not hard real-time code).
-- False means stopped, malformed samples, or overrun; the device owner should
-- stop the call rather than accumulate or silently retransmit old microphone
-- data. Each submission contains at most 500ms of PCM16 mono 24kHz audio.
submitLiveAudio :: LiveCall -> BS.ByteString -> IO Bool
submitLiveAudio = submitAudio False

-- | Device warmup variant: valid samples before connection are discarded
-- successfully, never buffered or transmitted. Closed calls still reject.
submitLiveAudioWhenReady :: LiveCall -> BS.ByteString -> IO Bool
submitLiveAudioWhenReady = submitAudio True

submitAudio :: Bool -> LiveCall -> BS.ByteString -> IO Bool
submitAudio discardPreparing call bytes
    | BS.null bytes || odd (BS.length bytes) || BS.length bytes > 24_000 = pure False
    | otherwise = atomically do
        closed <- readTVar call.stopped
        ready <- not <$> isEmptyTMVar call.started
        size <- readTVar call.captureBytes
        full <- isFullTBQueue call.capture
        if not closed && not ready && discardPreparing
            then pure True
        else if closed || not ready || full || size + BS.length bytes > 48_000
            then pure False
            else do
                writeTBQueue call.capture (BS.copy bytes)
                writeTVar call.captureBytes (size + BS.length bytes)
                pure True

-- | Blocks until playback is available or the call closes. Nothing discards
-- pending playback on hangup; the device owner must also stop its player.
readLiveAudio :: LiveCall -> IO (Maybe BS.ByteString)
readLiveAudio call = atomically do
    closed <- readTVar call.stopped
    if closed then pure Nothing else do
        bytes <- readTBQueue call.playback
        modifyTVar' call.playbackBytes (subtract (BS.length bytes))
        pure (Just bytes)

-- | Independent of the audio queue: a blocked device write must not prevent
-- interruption. Device owners cancel and join the old playback scope before
-- starting its replacement. The microphone scope is unaffected.
awaitLivePlaybackReset :: LiveCall -> Integer -> IO (Maybe Integer)
awaitLivePlaybackReset call previous = atomically do
    closed <- readTVar call.stopped
    generation <- readTVar call.playbackGeneration
    if closed then pure Nothing else do
        check (generation /= previous)
        pure (Just generation)

-- | A replaced player cannot take audio belonging to the next generation.
readLiveAudioGeneration :: LiveCall -> Integer -> IO (Maybe BS.ByteString)
readLiveAudioGeneration call expected = atomically do
    closed <- readTVar call.stopped
    generation <- readTVar call.playbackGeneration
    if closed || generation /= expected then pure Nothing else do
        bytes <- readTBQueue call.playback
        modifyTVar' call.playbackBytes (subtract (BS.length bytes))
        pure (Just bytes)

-- Codex's frameless protocol interleaves transcript roles. Role changes are
-- not turn boundaries; an old assistant's final chunk must not unmute it.
updatePlaybackTurn :: LiveCall -> LiveRole -> Bool -> Text -> STM ()
updatePlaybackTurn call role complete content = do
    generation <- readTVar call.playbackGeneration
    active <- readTVar call.inputActive
    let hasText = not (Text.null (Text.strip content))
    case role of
        LiveUser -> do
            unless (active || not hasText) do
                writeTVar call.playbackGeneration (generation + 1)
                writeTVar call.speakerSuppressed True
                void (flushTBQueue call.playback)
                writeTVar call.playbackBytes 0
            writeTVar call.inputActive (not complete && (active || hasText))
            if complete && not hasText
                then writeTVar call.speakerSuppressed False
                else pure ()
        LiveAssistant -> do
            owner <- readTVar call.assistantGeneration
            let currentOwner = case owner of
                    Nothing | hasText && not complete -> Just generation
                    _ -> owner
            when (hasText && currentOwner == Just generation) $
                writeTVar call.speakerSuppressed False
            writeTVar call.assistantGeneration (if complete then Nothing else currentOwner)
        LiveDeveloper -> pure ()

-- | Idempotent and nonblocking. Also interrupts connection establishment.
stopLiveCall :: LiveCall -> IO ()
stopLiveCall call = atomically (writeTVar call.stopped True)

-- | The host can install its hangup control immediately, then wait here before
-- acquiring audio devices. False means the call was stopped before startup.
awaitLiveStarted :: LiveCall -> IO Bool
awaitLiveStarted call = atomically do
    closed <- readTVar call.stopped
    if closed then pure False else readTMVar call.started >> pure True

runLiveCall
    :: TokenProvider
    -> LiveConfig
    -> (Text -> (Text -> IO ()) -> IO Text)
    -> (LiveEvent -> IO ())
    -> (LiveCall -> IO ())
    -> IO (Either ApiError ())
runLiveCall provider config = runLiveCallWith (runLiveConversation provider config)

-- | Injectable transport for lifecycle tests. Devices may warm up in parallel
-- using submitLiveAudioWhenReady to discard setup audio. Workers must be
-- structured children. All workers are joined before this function returns.
-- The event sink must be bounded/nonblocking; audio is delivered only through
-- readLiveAudio, never duplicated into the UI sink. Delegated work continues
-- using the host's normal approval policy. Cancelling its waiter on hangup
-- does not by itself authorize cancelling an unrelated coding turn.
runLiveCallWith
    :: (IO LiveInput -> (LiveEvent -> IO ()) -> IO (Either ApiError ()))
    -> (Text -> (Text -> IO ()) -> IO Text)
    -> (LiveEvent -> IO ())
    -> (LiveCall -> IO ())
    -> IO (Either ApiError ())
runLiveCallWith connect delegate notify devices = do
    call <- LiveCall <$> newTBQueueIO 100 <*> newTVarIO 0
        <*> newTBQueueIO 100 <*> newTVarIO 0 <*> newTVarIO False <*> newEmptyTMVarIO
        <*> newTVarIO 0 <*> newTVarIO False <*> newTVarIO Nothing <*> newTVarIO False
    context <- newTBQueueIO 8
    let next = atomically do
            closed <- readTVar call.stopped
            if closed then pure LiveHangUp else
                readTBQueue context `orElse` do
                    bytes <- readTBQueue call.capture
                    modifyTVar' call.captureBytes (subtract (BS.length bytes))
                    pure (LiveInputAudio bytes)
        receive event = case event of
            LiveStarted -> do
                atomically (void (tryPutTMVar call.started ()))
                notify event
            LiveAudio bytes -> atomically do
                closed <- readTVar call.stopped
                suppressed <- readTVar call.speakerSuppressed
                unless (closed || suppressed) do
                    size <- readTVar call.playbackBytes
                    full <- isFullTBQueue call.playback
                    if full || size + BS.length bytes > 96_000
                        then throwSTM (userError "Voice playback queue exceeded its limit")
                        else do
                            writeTBQueue call.playback bytes
                            writeTVar call.playbackBytes (size + BS.length bytes)
            LiveTranscript role complete content -> do
                atomically (updatePlaybackTurn call role complete content)
                notify event
            _ -> notify event
        send value = do
            case value of
                LiveContext _ _ content -> unless (BS.length (Text.encodeUtf8 content) <= 65_536)
                    (fail "Voice context exceeds size limit")
                _ -> pure ()
            atomically do
                closed <- readTVar call.stopped
                unless closed (writeTBQueue context value)
        conversation = withLiveDelegation delegate send receive (connect next)
        deviceScope = devices call
        waitForStop = atomically (readTVar call.stopped >>= check)
        close = do
            stopLiveCall call
            atomically do
                void (flushTBQueue call.capture)
                void (flushTBQueue call.playback)
                writeTVar call.captureBytes 0
                writeTVar call.playbackBytes 0
    outcome <- tryAny $ (race waitForStop (race conversation deviceScope) >>= \case
        Left () -> pure (Right ())
        Right (Left result) -> pure result
        Right (Right ()) -> pure (Right ())) `finally` close
    pure $ case outcome of
        Left _ -> Left (ConnectionError "Voice call stopped because its transport or audio device failed.")
        Right result -> result
