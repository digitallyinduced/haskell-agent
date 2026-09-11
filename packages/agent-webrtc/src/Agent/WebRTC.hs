{-# LANGUAGE BlockArguments, ForeignFunctionInterface, LambdaCase, NumericUnderscores #-}
-- | Provider-independent, scoped PCM16/24kHz/mono WebRTC media. No devices,
-- credentials, HTTP or tool execution. Join child workers before leaving scope.
module Agent.WebRTC
    ( Peer, withPeer, createOffer, createAnswer, setRemoteOffer, setRemoteAnswer
    , awaitConnected, pushAudio, pullAudio
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Exception.Safe (bracket)
import Control.Monad (unless, void, when)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Foreign hiding (void)
import Foreign.C
import System.Timeout (timeout)

data NativePeer
newtype Peer = Peer (MVar (Maybe (Ptr NativePeer)))

withPeer :: (Peer -> IO a) -> IO a
withPeer = bracket acquire release
  where
    acquire = do
        pointer <- nativeNew
        when (pointer == nullPtr) (fail "WebRTC media plugins could not be initialized")
        Peer <$> newMVar (Just pointer)
    release (Peer cell) = modifyMVar_ cell \case
        Nothing -> pure Nothing
        Just pointer -> nativeFree pointer >> pure Nothing

withPointer :: Peer -> (Ptr NativePeer -> IO a) -> IO a
withPointer (Peer cell) use = withMVar cell (maybe (fail "WebRTC peer is closed") use)

createOffer, createAnswer :: Peer -> IO Text
createOffer = createDescription True
createAnswer = createDescription False

createDescription :: Bool -> Peer -> IO Text
createDescription offer peer = withPointer peer \pointer -> do
    requireSuccess =<< nativeCreate pointer (boolean offer)
    sdp <- pollBytes pointer (nativeOperation pointer)
    BS.useAsCString sdp \text -> requireSuccess =<< nativeSet pointer text (boolean offer) 1
    void (pollBytes pointer (nativeOperation pointer))
    gathered <- pollBytes pointer (nativeLocal pointer)
    either (const (fail "Invalid WebRTC SDP encoding")) pure (Text.decodeUtf8' gathered)

setRemoteOffer, setRemoteAnswer :: Peer -> Text -> IO ()
setRemoteOffer = setRemote True
setRemoteAnswer = setRemote False

setRemote :: Bool -> Peer -> Text -> IO ()
setRemote offer peer description = withPointer peer \pointer -> do
    let bytes = Text.encodeUtf8 description
    when (BS.null bytes || BS.length bytes > 65_536 || BS.elem 0 bytes) (fail "Invalid WebRTC SDP size")
    BS.useAsCString bytes \text -> requireSuccess =<< nativeSet pointer text (boolean offer) 0
    void (pollBytes pointer (nativeOperation pointer))

-- Polling native operations makes setup cancellation interruptible. No blocking
-- GstPromise wait is entered until the promise has already completed.
pollBytes :: Ptr NativePeer -> (CString -> CSize -> IO CInt) -> IO BS.ByteString
pollBytes pointer operation = bounded $ allocaBytes 65_536 \buffer -> do
    let loop = do
            void (checkState pointer)
            count <- operation buffer 65_536
            if count < 0 then fail "WebRTC negotiation failed"
            else if count == 0 then threadDelay 10_000 >> loop
            else BS.packCStringLen (buffer, fromIntegral count)
    loop

awaitConnected :: Peer -> IO ()
awaitConnected peer = bounded loop
  where
    loop = do
        ready <- withPointer peer checkState
        unless ready (threadDelay 10_000 >> loop)

pushAudio :: Peer -> BS.ByteString -> IO ()
pushAudio peer bytes = withPointer peer \pointer -> do
    when (BS.null bytes || odd (BS.length bytes) || BS.length bytes > 24_000) (fail "Invalid WebRTC PCM frame")
    void (checkState pointer)
    BS.useAsCStringLen bytes \(buffer, count) -> requireSuccess =<< nativePush pointer buffer (fromIntegral count)

pullAudio :: Peer -> IO BS.ByteString
pullAudio peer = do
    bytes <- withPointer peer \pointer -> allocaBytes 24_000 \buffer -> do
        void (checkState pointer)
        count <- nativePull pointer buffer 24_000
        if count < 0 then fail "Invalid WebRTC playback frame"
        else BS.packCStringLen (buffer, fromIntegral count)
    if BS.null bytes then threadDelay 5_000 >> pullAudio peer else pure bytes

checkState :: Ptr NativePeer -> IO Bool
checkState pointer = do
    state <- nativeState pointer
    when (state < 0) (fail "WebRTC media disconnected")
    pure (state == 1)

bounded :: IO a -> IO a
bounded action = timeout 15_000_000 action >>= maybe (fail "WebRTC negotiation timed out") pure

boolean :: Bool -> CInt
boolean value = if value then 1 else 0

requireSuccess :: CInt -> IO ()
requireSuccess value = unless (value == 1) (fail "WebRTC operation failed or audio queue overran")

foreign import ccall safe "agent_peer_new" nativeNew :: IO (Ptr NativePeer)
foreign import ccall safe "agent_peer_free" nativeFree :: Ptr NativePeer -> IO ()
foreign import ccall safe "agent_peer_create_description" nativeCreate :: Ptr NativePeer -> CInt -> IO CInt
foreign import ccall safe "agent_peer_set_description" nativeSet :: Ptr NativePeer -> CString -> CInt -> CInt -> IO CInt
foreign import ccall safe "agent_peer_operation" nativeOperation :: Ptr NativePeer -> CString -> CSize -> IO CInt
foreign import ccall safe "agent_peer_local_description" nativeLocal :: Ptr NativePeer -> CString -> CSize -> IO CInt
foreign import ccall safe "agent_peer_state" nativeState :: Ptr NativePeer -> IO CInt
foreign import ccall safe "agent_peer_push" nativePush :: Ptr NativePeer -> CString -> CSize -> IO CInt
foreign import ccall safe "agent_peer_pull" nativePull :: Ptr NativePeer -> CString -> CSize -> IO CInt
