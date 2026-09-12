-- | Sequential WebSocket transcription plumbing. Providers retain their event
-- decoders, transcript reducers, and session/child ownership.
module Agent.Transcription.Receive
    ( ReceiveStep(..)
    , receiveEvent
    , awaitReady
    , receiveTranscripts
    , receiveLoop
    , waitForCompletion
    , waitForResult
    ) where

import qualified Agent.Json.Decode as Json
import Control.Concurrent.MVar (MVar, takeMVar, tryPutMVar)
import Control.Exception.Safe (SomeException, throwIO, tryAny)
import Control.Monad (void)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.WebSockets as WS
import qualified System.Timeout as Timeout

-- | Updated provider state and an optional transcript notification. Completion
-- includes provider errors, whose interpretation remains with the provider.
data ReceiveStep state
    = Continue state (Maybe Text)
    | Complete state (Maybe Text)

-- | Malformed messages are ignored; transport exceptions still propagate.
-- The decoder session must belong to this sequential receive stream.
receiveEvent
    :: Json.DecoderSession
    -> Json.Decoder event
    -> WS.Connection
    -> IO (Maybe event)
receiveEvent session decoder connection = do
    bytes <- WS.receiveData connection
    either (const Nothing) Just <$> Json.decodeIO session decoder (LBS.toStrict bytes)

-- | Bound the whole readiness handshake, not each individual read. Unrelated
-- events (including malformed messages) do not reset the timeout.
awaitReady
    :: Int
    -> String
    -> IO (Maybe event)
    -> (event -> Maybe (Either Text ()))
    -> IO ()
awaitReady timeoutMicros timeoutMessage receive classify =
    Timeout.timeout timeoutMicros loop >>= maybe (fail timeoutMessage) pure
  where
    loop = do
        event <- receive
        case event >>= classify of
            Nothing -> loop
            Just (Left message) -> fail (Text.unpack message)
            Just (Right ()) -> pure ()

-- | The receiver owns accumulation; only the final state crosses threads.
-- Synchronous read/reducer errors are published, and synchronous notification
-- failures are ignored. Cancellation is never swallowed by either 'tryAny'.
-- The caller must scope this action (e.g. with 'Control.Concurrent.Async.withAsync').
receiveTranscripts
    :: IO (Maybe event)
    -> (event -> state -> ReceiveStep state)
    -> state
    -> MVar (Either SomeException state)
    -> (Text -> IO ())
    -> IO ()
receiveTranscripts receive step initial finished onTranscript =
    tryAny (receiveLoop receive step initial onTranscript) >>= void . tryPutMVar finished

-- | Accumulate in the calling thread, allowing a scoped Async to own the
-- result instead of publishing through a separate completion channel.
receiveLoop
    :: IO (Maybe event)
    -> (event -> state -> ReceiveStep state)
    -> state
    -> (Text -> IO ())
    -> IO state
receiveLoop receive step initial onTranscript =
    loop initial
  where
    loop previous =
        receive >>= \case
            Nothing -> loop previous
            Just event ->
                case step event previous of
                    Continue current notification ->
                        notify notification >> loop current
                    Complete current notification ->
                        notify notification >> pure current
    notify = mapM_ (void . tryAny . onTranscript)

-- | Wait for the receiver's result, preserving its original exception.
waitForCompletion
    :: Int
    -> String
    -> MVar (Either SomeException state)
    -> IO state
waitForCompletion timeoutMicros timeoutMessage finished =
    waitForResult timeoutMicros timeoutMessage (takeMVar finished)

-- | Bound a provider's completion wait without changing its cancellation
-- policy. The supplied action may wait on an MVar or a scoped Async.
waitForResult
    :: Int
    -> String
    -> IO (Either SomeException state)
    -> IO state
waitForResult timeoutMicros timeoutMessage await =
    Timeout.timeout timeoutMicros await >>= \case
        Nothing -> fail timeoutMessage
        Just result -> either throwIO pure result
