{-# LANGUAGE ForeignFunctionInterface #-}
module Agent.CLI.MacOS.MobileGatewayBridge where

import qualified Agent.CLI.MacOS.MobileGateway as Mobile
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception.Safe (tryAny, throwIO, fromException, bracketOnError)
import Control.Monad (void, when, forM_)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Word (Word8, Word64)
import Foreign hiding (void)
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)

-- One completion callback shape, carrying only typed result fields:
-- opaque handle, UTF-8 value/name, or an encrypted binary frame.
type Callback = Ptr () -> CInt -> Word64 -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> IO ()
foreign import ccall "dynamic" invoke :: FunPtr Callback -> Callback

data Resource = Session Mobile.MobileSession | Relay Mobile.MobileRelay
resources :: TVar (Word64, Map.Map Word64 Resource)
resources = unsafePerformIO $ newTVarIO (1, Map.empty)
{-# NOINLINE resources #-}

insertResource :: Resource -> IO Word64
insertResource resource = atomically do
    (next, entries) <- readTVar resources
    when (next == maxBound || Map.size entries >= 1024) $ throwSTM Mobile.MobileUnavailable
    writeTVar resources (next + 1, Map.insert next resource entries)
    pure next

sessionFor :: Word64 -> IO Mobile.MobileSession
sessionFor handle = atomically do
    (_, entries) <- readTVar resources
    case Map.lookup handle entries of
        Just (Session session) -> pure session
        _ -> throwSTM Mobile.AccountUnavailable

relayFor :: Word64 -> IO Mobile.MobileRelay
relayFor handle = atomically do
    (_, entries) <- readTVar resources
    case Map.lookup handle entries of
        Just (Relay relay) -> pure relay
        _ -> throwSTM Mobile.AccountUnavailable

deliver :: FunPtr Callback -> Ptr () -> CInt -> Word64 -> BS.ByteString -> BS.ByteString -> IO ()
deliver callback context status handle value name =
    BS.useAsCStringLen value \(p,n) ->
        BS.useAsCStringLen name \(q,m) ->
            invoke callback context status handle (castPtr p) (fromIntegral n) (castPtr q) (fromIntegral m)

start :: FunPtr Callback -> Ptr () -> IO (Word64, BS.ByteString) -> IO CInt
start callback context action
    | callback == nullFunPtr = pure 1
    | otherwise = do
        void $ forkIO do
            result <- tryAny action
            case result of
                Right (handle, bytes) -> deliver callback context 0 handle bytes BS.empty
                Left err -> deliver callback context
                    (case fromException err of
                        Just Mobile.AccountUnavailable -> 2
                        Just Mobile.InvalidMobileInput -> 1
                        _ -> 3) 0 BS.empty BS.empty
        pure 0

input :: Ptr Word8 -> CSize -> IO Text
input ptr size
    | size > 4096 || (ptr == nullPtr && size /= 0) = throwIO Mobile.InvalidMobileInput
    | otherwise = do
        bytes <- if size == 0 then pure BS.empty else BS.packCStringLen (castPtr ptr, fromIntegral size)
        either (const $ throwIO Mobile.InvalidMobileInput) pure (Text.decodeUtf8' bytes)

foreign export ccall ha_mobile_session_open :: FunPtr Callback -> Ptr () -> IO CInt
ha_mobile_session_open :: FunPtr Callback -> Ptr () -> IO CInt
ha_mobile_session_open callback context = start callback context $
    bracketOnError Mobile.openSession Mobile.closeSession \session -> do
        handle <- insertResource (Session session)
        pure (handle, Text.encodeUtf8 $ Mobile.sessionBaseURL session)

foreign export ccall ha_mobile_handle_close :: Word64 -> IO CInt
ha_mobile_handle_close :: Word64 -> IO CInt
ha_mobile_handle_close handle = do
    result <- tryAny do
        resource <- atomically do
            (next, entries) <- readTVar resources
            writeTVar resources (next, Map.delete handle entries)
            pure $ Map.lookup handle entries
        case resource of
            Just (Session session) -> Mobile.closeSession session
            Just (Relay relay) -> Mobile.closeRelay relay
            Nothing -> pure ()
    pure $ either (const 3) (const 0) result

foreign export ccall ha_mobile_runner_register :: Word64 -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> FunPtr Callback -> Ptr () -> IO CInt
ha_mobile_runner_register :: Word64 -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> FunPtr Callback -> Ptr () -> IO CInt
ha_mobile_runner_register handle p n q m callback context = do
    copied <- tryAny $ (,) <$> input p n <*> input q m
    start callback context do
        (deviceID, name) <- either throwIO pure copied
        session <- sessionFor handle
        runner <- Mobile.registerRunner session deviceID name
        pure (0, Text.encodeUtf8 runner)

foreign export ccall ha_mobile_pairings_list :: Word64 -> FunPtr Callback -> Ptr () -> IO CInt
ha_mobile_pairings_list :: Word64 -> FunPtr Callback -> Ptr () -> IO CInt
ha_mobile_pairings_list handle callback context = start callback context do
    session <- sessionFor handle
    rows <- Mobile.listPairings session
    forM_ rows \(identifier, name) ->
        deliver callback context 4 0 (Text.encodeUtf8 identifier) (Text.encodeUtf8 name)
    pure (0, BS.empty)

pairingOperation :: (Mobile.MobileSession -> Text -> IO ()) -> Word64 -> Ptr Word8 -> CSize -> FunPtr Callback -> Ptr () -> IO CInt
pairingOperation action handle p n callback context = do
    copied <- tryAny $ input p n
    start callback context do
        pairing <- either throwIO pure copied
        session <- sessionFor handle
        action session pairing
        pure (0, BS.empty)

foreign export ccall ha_mobile_pairing_revoke :: Word64 -> Ptr Word8 -> CSize -> FunPtr Callback -> Ptr () -> IO CInt
ha_mobile_pairing_revoke :: Word64 -> Ptr Word8 -> CSize -> FunPtr Callback -> Ptr () -> IO CInt
ha_mobile_pairing_revoke = pairingOperation Mobile.revokePairing

foreign export ccall ha_mobile_pairing_wake :: Word64 -> Ptr Word8 -> CSize -> FunPtr Callback -> Ptr () -> IO CInt
ha_mobile_pairing_wake :: Word64 -> Ptr Word8 -> CSize -> FunPtr Callback -> Ptr () -> IO CInt
ha_mobile_pairing_wake = pairingOperation Mobile.wakePairing

foreign export ccall ha_mobile_relay_open :: Word64 -> Ptr Word8 -> CSize -> FunPtr Callback -> Ptr () -> IO CInt
ha_mobile_relay_open :: Word64 -> Ptr Word8 -> CSize -> FunPtr Callback -> Ptr () -> IO CInt
ha_mobile_relay_open handle p n callback context = do
    copied <- tryAny $ input p n
    start callback context do
        pairing <- either throwIO pure copied
        session <- sessionFor handle
        bracketOnError (Mobile.openRelay session pairing) Mobile.closeRelay \relay -> do
            identifier <- insertResource (Relay relay)
            pure (identifier, BS.empty)

foreign export ccall ha_mobile_relay_send :: Word64 -> Ptr Word8 -> CSize -> FunPtr Callback -> Ptr () -> IO CInt
ha_mobile_relay_send :: Word64 -> Ptr Word8 -> CSize -> FunPtr Callback -> Ptr () -> IO CInt
ha_mobile_relay_send handle p n callback context
    | n > 1048576 || (p == nullPtr && n /= 0) = pure 1
    | otherwise = do
        bytes <- if n == 0 then pure BS.empty else BS.packCStringLen (castPtr p, fromIntegral n)
        start callback context do
            relay <- relayFor handle
            Mobile.sendRelay relay bytes
            pure (0, BS.empty)

foreign export ccall ha_mobile_relay_receive :: Word64 -> FunPtr Callback -> Ptr () -> IO CInt
ha_mobile_relay_receive :: Word64 -> FunPtr Callback -> Ptr () -> IO CInt
ha_mobile_relay_receive handle callback context = start callback context do
    relay <- relayFor handle
    bytes <- Mobile.receiveRelay relay
    pure (0, bytes)
