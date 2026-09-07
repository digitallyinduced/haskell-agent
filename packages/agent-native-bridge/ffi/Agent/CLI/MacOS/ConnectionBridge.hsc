{-# LANGUAGE ForeignFunctionInterface #-}
module Agent.CLI.MacOS.ConnectionBridge
    ( ConnectionCallback, ConnectionSecretCallback
    , sendConnectionResult, connectionSecretStore, decodeConnectionAnswers
    , withSnapshot
    ) where

#include "HaskellAgentBridge.h"

import Agent.Integration.Connection
import Agent.CLI.MacOS.Marshalling (withText, decodeUtf8Input)
import Data.Bifunctor (first)
import Control.Exception.Safe (finally)
import Control.Monad (forM)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Word (Word8, Word32)
import Foreign
import Foreign.C.Types

type ConnectionCallback =
    Ptr () -> CInt -> Ptr () -> Ptr Word8 -> CSize -> IO ()
type ConnectionSecretCallback =
    Ptr () -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> IO CInt
foreign import ccall "dynamic" invokeConnectionCallback
    :: FunPtr ConnectionCallback -> ConnectionCallback
foreign import ccall "dynamic" invokeSecretCallback
    :: FunPtr ConnectionSecretCallback -> ConnectionSecretCallback

connectionSecretStore :: FunPtr ConnectionSecretCallback -> Ptr () -> ConnectionSecretStore
connectionSecretStore callback context scope
    | callback == nullFunPtr = pure (Left "A native secure store is required.")
    | otherwise = withTextBytes scope \bytes len ->
        allocaBytes 32 \out -> flip finally (fillBytes out 0 32) do
            fillBytes out 0 32
            status <- invokeSecretCallback callback context bytes len out 32
            if status /= 0
                then pure (Left "The native secure store is unavailable.")
                else Right <$> BS.packCStringLen (castPtr out, 32)

decodeConnectionAnswers :: Ptr () -> CSize -> IO (Either Text [ConnectionAnswer])
decodeConnectionAnswers pointer count
    | count > 64 || (count > 0 && pointer == nullPtr) =
        pure (Left "Invalid connection answers.")
    | otherwise = fmap sequence $ forM [0 .. fromIntegral count - 1] \index -> do
        let row = pointer `plusPtr` (index * #{size ha_connection_answer})
        identifier <- readSlice (row `plusPtr` #{offset ha_connection_answer, identifier})
        value <- readSlice (row `plusPtr` #{offset ha_connection_answer, value})
        pure (ConnectionAnswer <$> identifier <*> value)
  where
    readSlice row = do
        bytes <- #{peek ha_utf8_slice, bytes} row
        len <- #{peek ha_utf8_slice, length} row :: IO CSize
        if len > 16384 || (len > 0 && bytes == nullPtr)
            then pure (Left "Invalid connection answer.")
            else if len == 0 then pure (Right "")
            else first (const "Invalid UTF-8 connection answer.") <$> decodeUtf8Input bytes (fromIntegral len)

withTextBytes :: Text -> (Ptr Word8 -> CSize -> IO a) -> IO a
withTextBytes text action = withText text \bytes len -> action (castPtr bytes) len

sendConnectionResult :: FunPtr ConnectionCallback -> Ptr ()
    -> Either Text ConnectionSnapshot -> IO ()
sendConnectionResult callback context = \case
    Left err -> withTextBytes err \bytes len ->
        invokeConnectionCallback callback context (-1) nullPtr bytes len
    Right snapshot -> withSnapshot snapshot \pointer ->
        invokeConnectionCallback callback context 0 pointer nullPtr 0

withSlice :: Ptr a -> Text -> IO b -> IO b
withSlice pointer value action = withTextBytes value \bytes len -> do
    #{poke ha_utf8_slice, bytes} pointer bytes
    #{poke ha_utf8_slice, length} pointer len
    action

withRows :: Int -> [a] -> (Ptr () -> a -> IO b -> IO b) -> (Ptr () -> CSize -> IO b) -> IO b
withRows stride rows write action =
    allocaBytes (max 1 (stride * length rows)) \pointer ->
        let go _ [] = action pointer (fromIntegral (length rows))
            go index (row:rest) = write (pointer `plusPtr` (stride * index)) row (go (index + 1) rest)
        in go 0 rows

withField :: Ptr () -> ConnectionField -> IO a -> IO a
withField pointer field action =
    withSlice (pointer `plusPtr` #{offset ha_connection_field, identifier}) field.connectionFieldId $
    withSlice (pointer `plusPtr` #{offset ha_connection_field, label}) field.connectionFieldLabel $ do
        #{poke ha_connection_field, kind} pointer (fromIntegral (fromEnum field.connectionFieldKind) :: CInt)
        #{poke ha_connection_field, required} pointer (if field.connectionFieldRequired then 1 else 0 :: CInt)
        action

withItem :: Ptr () -> ConnectionItem -> IO a -> IO a
withItem pointer item action =
    withSlice (pointer `plusPtr` #{offset ha_connection_item, identifier}) item.connectionItemId $
    withSlice (pointer `plusPtr` #{offset ha_connection_item, title}) item.connectionItemTitle $
    withSlice (pointer `plusPtr` #{offset ha_connection_item, detail}) item.connectionItemDetail $ do
        #{poke ha_connection_item, selected} pointer (if item.connectionItemSelected then 1 else 0 :: CInt)
        action

withSnapshot :: ConnectionSnapshot -> (Ptr () -> IO a) -> IO a
withSnapshot snapshot action = allocaBytes #{size ha_connection_snapshot} \pointer ->
    withSlice (pointer `plusPtr` #{offset ha_connection_snapshot, session_id}) snapshot.connectionSessionId $
    withSlice (pointer `plusPtr` #{offset ha_connection_snapshot, title}) snapshot.connectionTitle $
    withSlice (pointer `plusPtr` #{offset ha_connection_snapshot, message}) snapshot.connectionMessage $
    withSlice (pointer `plusPtr` #{offset ha_connection_snapshot, redirect_url}) snapshot.connectionRedirectUrl $
    withRows #{size ha_connection_field} snapshot.connectionFields withField \fields fieldCount ->
    withRows #{size ha_connection_item} snapshot.connectionItems withItem \items itemCount -> do
        #{poke ha_connection_snapshot, phase} pointer (fromIntegral (fromEnum snapshot.connectionPhase) :: CInt)
        #{poke ha_connection_snapshot, poll_after_milliseconds} pointer
            (fromIntegral (max 0 (min 300000 snapshot.connectionPollAfterMilliseconds)) :: Word32)
        #{poke ha_connection_snapshot, fields} pointer fields
        #{poke ha_connection_snapshot, field_count} pointer fieldCount
        #{poke ha_connection_snapshot, items} pointer items
        #{poke ha_connection_snapshot, item_count} pointer itemCount
        action pointer
