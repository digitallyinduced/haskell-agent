-- | Scoped, replayable microphone capture independent of provider startup.
module Agent.CLI.Dictation.Capture (withBufferedCapture) where

import Control.Concurrent.Async (wait, waitEitherCatch, withAsync)
import Control.Concurrent.STM
    ( atomically, newTVarIO, readTVar, retry, throwSTM, writeTVar )
import Control.Exception.Safe (throwIO)
import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import qualified Data.Sequence as Seq

-- | Start capture before running the consumer (including its authentication
-- and connection setup). Each invocation of the supplied producer replays the
-- same recording from the beginning, so an authentication retry cannot lose
-- the opening words or reopen the microphone after the user has stopped.
--
-- Retention is bounded in bytes. Overflow fails explicitly instead of silently
-- dropping speech or blocking capture behind a stalled network connection.
-- Both workers are cancelled and joined when this scope exits.
withBufferedCapture
    :: Int
    -> IO ()
    -> ((BS.ByteString -> IO ()) -> IO ())
    -> (((BS.ByteString -> IO ()) -> IO ()) -> IO a)
    -> IO a
withBufferedCapture byteLimit onRecording capture consume = do
    buffer <- newTVarIO (Seq.empty, 0 :: Int, False)
    let append bytes = unless (BS.null bytes) do
            first <- atomically do
                (chunks, size, done) <- readTVar buffer
                when (BS.length bytes > byteLimit - size) $
                    throwSTM (userError "Dictation audio buffer limit exceeded")
                writeTVar buffer
                    (chunks Seq.|> BS.copy bytes, size + BS.length bytes, done)
                pure (Seq.null chunks)
            when first onRecording
        record = do
            capture append
            atomically do
                (chunks, size, _) <- readTVar buffer
                writeTVar buffer (chunks, size, True)
        replay send = loop 0
          where
            loop index = do
                next <- atomically do
                    (chunks, _, done) <- readTVar buffer
                    case Seq.lookup index chunks of
                        Just bytes -> pure (Just bytes)
                        Nothing | done -> pure Nothing
                        Nothing -> retry
                case next of
                    Nothing -> pure ()
                    Just bytes -> send bytes >> loop (index + 1)
    withAsync record \recorder ->
        withAsync (consume replay) \consumer ->
            waitEitherCatch recorder consumer >>= \case
                Left (Left err) -> throwIO err
                Left (Right ()) -> wait consumer
                Right (Left err) -> throwIO err
                Right (Right result) -> pure result
