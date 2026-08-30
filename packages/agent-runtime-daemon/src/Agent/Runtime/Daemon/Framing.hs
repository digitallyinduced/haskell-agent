module Agent.Runtime.Daemon.Framing
    ( FrameError (..)
    , defaultMaximumFrameBytes
    , sendJSONFrame
    , receiveJSONFrame
    ) where

import Control.Exception.Safe (Exception, throwIO)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict', encode)
import Data.Bits ((.|.), shiftL, shiftR)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Word (Word32, Word8)
import Network.Socket (Socket)
import qualified Network.Socket.ByteString as Socket

data FrameError
    = FrameClosed
    | FrameTooLarge Int Int
    | FrameInvalidJSON String
    deriving stock (Eq, Show)

instance Exception FrameError

defaultMaximumFrameBytes :: Int
defaultMaximumFrameBytes = 1_048_576

sendJSONFrame :: ToJSON value => Int -> Socket -> value -> IO ()
sendJSONFrame maximumBytes socket value = do
    let body = LBS.toStrict (encode value)
        bodyLength = BS.length body
    if bodyLength > maximumBytes
        then throwIO (FrameTooLarge bodyLength maximumBytes)
        else Socket.sendAll socket (encodeLength bodyLength <> body)

receiveJSONFrame :: FromJSON value => Int -> Socket -> IO value
receiveJSONFrame maximumBytes socket = do
    header <- receiveExactly socket 4
    let bodyLength = decodeLength header
    if bodyLength > maximumBytes
        then throwIO (FrameTooLarge bodyLength maximumBytes)
        else do
            body <- receiveExactly socket bodyLength
            either (throwIO . FrameInvalidJSON) pure (eitherDecodeStrict' body)

receiveExactly :: Socket -> Int -> IO BS.ByteString
receiveExactly socket expected = go [] expected
  where
    go chunks remaining
        | remaining == 0 = pure (BS.concat (reverse chunks))
        | otherwise = do
            chunk <- Socket.recv socket remaining
            if BS.null chunk
                then throwIO FrameClosed
                else go (chunk : chunks) (remaining - BS.length chunk)

encodeLength :: Int -> BS.ByteString
encodeLength lengthValue =
    let word = fromIntegral lengthValue :: Word32
     in BS.pack
            [ byte (word `shiftR` 24)
            , byte (word `shiftR` 16)
            , byte (word `shiftR` 8)
            , byte word
            ]
  where
    byte :: Word32 -> Word8
    byte = fromIntegral

decodeLength :: BS.ByteString -> Int
decodeLength bytes =
    fromIntegral $
        foldl
            (\accumulator octet -> (accumulator `shiftL` 8) .|. fromIntegral octet)
            (0 :: Word32)
            (BS.unpack bytes)
