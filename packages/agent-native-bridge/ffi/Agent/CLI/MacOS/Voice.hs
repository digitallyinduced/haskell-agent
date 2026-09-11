{-# LANGUAGE ForeignFunctionInterface #-}
module Agent.CLI.MacOS.Voice (runNativeVoiceAudio) where

import Agent.CLI.MacOS.EngineState (Engine(..))
import Agent.CLI.MacOS.TurnState (NativeTurnOptions(..), VoiceAudioCallback)
import Agent.OpenAI.Live.Call
import Control.Concurrent.STM
import Control.Concurrent.Async (race)
import Control.Exception.Safe (bracket, finally, tryAny)
import Control.Monad (unless, void)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as Text
import Data.Word (Word8)
import Foreign hiding (void)
import Foreign.C.Types

foreign import ccall "dynamic" invokeAudio :: FunPtr VoiceAudioCallback -> VoiceAudioCallback
foreign export ccall ha_engine_stage_voice :: Ptr () -> Ptr Word8 -> CSize -> FunPtr VoiceAudioCallback -> Ptr () -> IO CInt
foreign export ccall ha_voice_submit_audio :: Ptr () -> Ptr Word8 -> CSize -> IO CInt

-- Staging must follow stage_turn_options; it never creates an unadmitted turn.
ha_engine_stage_voice :: Ptr () -> Ptr Word8 -> CSize -> FunPtr VoiceAudioCallback -> Ptr () -> IO CInt
ha_engine_stage_voice pointer ident len callback context
    | pointer == nullPtr || ident == nullPtr || len == 0 || len > 512 || callback == nullFunPtr = pure 1
    | otherwise = do
        result <- tryAny do
            engine <- deRefStablePtr (castPtrToStablePtr pointer :: StablePtr Engine)
            bytes <- BS.packCStringLen (castPtr ident, fromIntegral len)
            case Text.decodeUtf8' bytes of
                Left _ -> pure 2
                Right key -> atomically do
                    options <- readTVar engine.engineStagedTurnOptions
                    case Map.lookup key options of
                        Nothing -> pure 2
                        Just old -> do
                            writeTVar engine.engineStagedTurnOptions $ Map.insert key
                                (old {nativeTurnVoice = Just (callback, context)}) options
                            pure 0
        pure (either (const 3) id result)

ha_voice_submit_audio :: Ptr () -> Ptr Word8 -> CSize -> IO CInt
ha_voice_submit_audio pointer bytes len
    | pointer == nullPtr || bytes == nullPtr || len == 0 || len > 24_000 || odd len = pure 1
    | otherwise = do
        result <- tryAny do
            call <- deRefStablePtr (castPtrToStablePtr pointer :: StablePtr LiveCall)
            pcm <- BS.packCStringLen (castPtr bytes, fromIntegral len)
            accepted <- submitLiveAudioWhenReady call pcm
            unless accepted (stopLiveCall call)
            pure (if accepted then 0 else 2)
        pure (either (const 3) id result)

runNativeVoiceAudio :: (FunPtr VoiceAudioCallback, Ptr ()) -> LiveCall -> IO ()
runNativeVoiceAudio (callback, context) call = do
    bracket (newStablePtr call) freeStablePtr \stable -> do
        let handle = castStablePtrToPtr stable
            send event bytes len = invokeAudio callback context event handle bytes len
            playback generation = readLiveAudioGeneration call generation >>= \case
                Nothing -> pure ()
                Just bytes -> do
                    status <- BS.useAsCStringLen bytes \(ptr, size) ->
                        send 1 (castPtr ptr) (fromIntegral size)
                    unless (status == 0) (fail "Native voice playback failed")
                    playback generation
            playbackScope generation = do
                _ <- race (awaitLivePlaybackReset call generation) (playback generation)
                awaitLivePlaybackReset call generation >>= \case
                    Nothing -> pure ()
                    Just next -> do
                        status <- send 3 nullPtr 0
                        unless (status == 0) (fail "Native voice playback reset failed")
                        playbackScope next
        (do
            status <- send 0 nullPtr 0
            unless (status == 0) (fail "Native voice capture failed")
            playbackScope 0)
            `finally` void (send 2 nullPtr 0)
