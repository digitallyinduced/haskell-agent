{-# LANGUAGE BlockArguments, ForeignFunctionInterface, LambdaCase, NumericUnderscores #-}
-- | Scoped device I/O. Join all users before leaving a device scope.
module Agent.WebRTC.Audio
    ( Capture, Playback, withCapture, readCapture, withPlayback, writePlayback, monitorPlayback ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Exception.Safe (bracket)
import Control.Monad (when)
import qualified Data.ByteString as BS
import Foreign
import Foreign.C

data NativeAudio
newtype Device = Device (MVar (Maybe (Ptr NativeAudio)))
newtype Capture = Capture Device
newtype Playback = Playback Device

withDevice :: CInt -> (Device -> IO a) -> IO a
withDevice capture = bracket acquire release
  where
    acquire = do
        pointer <- nativeNew capture
        when (pointer == nullPtr) (fail "Voice audio device could not be initialized")
        Device <$> newMVar (Just pointer)
    release (Device cell) = modifyMVar_ cell \case
        Nothing -> pure Nothing
        Just pointer -> nativeFree pointer >> pure Nothing

withPointer :: Device -> (Ptr NativeAudio -> IO a) -> IO a
withPointer (Device cell) use = withMVar cell (maybe (fail "Voice audio device is closed") use)

withCapture :: (Capture -> IO a) -> IO a
withCapture use = withDevice 1 (use . Capture)

withPlayback :: (Playback -> IO a) -> IO a
withPlayback use = withDevice 0 (use . Playback)

readCapture :: Capture -> IO BS.ByteString
readCapture capture@(Capture device) = do
    bytes <- withPointer device \pointer -> allocaBytes 24_000 \buffer -> do
        count <- nativeRead pointer buffer 24_000
        when (count < 0) (fail "Voice capture failed or exceeded its queue limit")
        BS.packCStringLen (buffer, fromIntegral count)
    if BS.null bytes then threadDelay 5_000 >> readCapture capture else pure bytes

writePlayback :: Playback -> BS.ByteString -> IO ()
writePlayback (Playback device) bytes = withPointer device \pointer ->
    BS.useAsCStringLen bytes \(buffer, count) -> do
        result <- nativeWrite pointer buffer (fromIntegral count)
        when (result /= 1) (fail "Voice playback failed or exceeded its queue limit")

-- Device errors must also surface while waiting for the next server packet.
monitorPlayback :: Playback -> IO ()
monitorPlayback playback@(Playback device) = do
    result <- withPointer device nativeState
    when (result < 0) (fail "Voice playback device disconnected")
    threadDelay 20_000
    monitorPlayback playback

foreign import ccall safe "agent_audio_new" nativeNew :: CInt -> IO (Ptr NativeAudio)
foreign import ccall safe "agent_audio_free" nativeFree :: Ptr NativeAudio -> IO ()
foreign import ccall safe "agent_audio_read" nativeRead :: Ptr NativeAudio -> CString -> CSize -> IO CInt
foreign import ccall safe "agent_audio_write" nativeWrite :: Ptr NativeAudio -> CString -> CSize -> IO CInt
foreign import ccall safe "agent_audio_state" nativeState :: Ptr NativeAudio -> IO CInt
