-- | A bounded, single-consumer event pump with adjacent-event coalescing.
--
-- Producers may enqueue concurrently, but the sink is always invoked by one
-- worker. A sink failure is published through STM so blocked producers wake
-- up rather than waiting forever for queue capacity.
module Agent.Loop.EventPump
    ( EventPump
    , EventPumpFailure(..)
    , emitAppendedText
    , emitEvent
    , emitLatestText
    , flushEventPump
    , newEventPump
    , runEventPump
    , waitEventPumpFailure
    ) where

import Agent.TextBuffer
    ( TextBuffer
    , appendTextBuffer
    , emptyTextBuffer
    , textBufferToText
    )
import Control.Concurrent.Async (Async, race, waitCatch)
import Control.Concurrent.STM
    ( STM
    , TBQueue
    , TMVar
    , TVar
    , atomically
    , modifyTVar'
    , newEmptyTMVarIO
    , newTBQueueIO
    , newTVar
    , orElse
    , putTMVar
    , readTBQueue
    , readTMVar
    , readTVar
    , retry
    , tryPutTMVar
    , writeTBQueue
    , writeTVar
    )
import Control.Exception (AsyncException, toException)
import qualified Control.Exception as Exception
import Control.Exception.Safe
    ( SomeException
    , catchAsync
    , isAsyncException
    , throwIO
    , tryAny
    )
import Control.Monad (void)
import Data.Text (Text)

data EventPumpCommand key event
    = DeliverEvent !event
    | DeliverCoalescedEvent !(CoalescedEvent key event)
    | FlushEvents !(TMVar ())

data CoalescedEvent key event
    = AppendedEvent !key !(Text -> event) !(TVar TextBuffer)
    | ReplacedEvent !key !(Text -> event) !(TVar Text)

data EventPumpFailure
    = EventPumpSyncFailure !SomeException
    | EventPumpAsyncFailure !AsyncException

data EventPump key event = EventPump
    { eventPumpQueue :: !(TBQueue (EventPumpCommand key event))
    , eventPumpFailure :: !(TMVar EventPumpFailure)
    , eventPumpTail :: !(TVar (Maybe (CoalescedEvent key event)))
    , eventPumpSink :: !(event -> IO ())
    }

eventQueueCapacity :: Int
eventQueueCapacity = 256

newEventPump :: (event -> IO ()) -> IO (EventPump key event)
newEventPump sink = do
    queue <- newTBQueueIO (fromIntegral eventQueueCapacity)
    failure <- newEmptyTMVarIO
    tailEvent <- atomically (newTVar Nothing)
    pure EventPump
        { eventPumpQueue = queue
        , eventPumpFailure = failure
        , eventPumpTail = tailEvent
        , eventPumpSink = sink
        }

runEventPump :: Eq key => EventPump key event -> IO ()
runEventPump pump = go
  where
    go =
        atomically (readTBQueue pump.eventPumpQueue) >>= \case
            DeliverEvent event ->
                deliver event
            DeliverCoalescedEvent pending -> do
                event <- atomically do
                    current <- readTVar pump.eventPumpTail
                    whenSTM (sameCoalescedEvent current pending) $
                        writeTVar pump.eventPumpTail Nothing
                    materializeCoalescedEvent pending
                deliver event
            FlushEvents flushed -> do
                atomically (putTMVar flushed ())
                go
    deliver event = do
        (tryAny (pump.eventPumpSink event) >>= \case
            Right () -> go
            Left exception -> do
                atomically $
                    recordEventPumpFailure
                        pump
                        (EventPumpSyncFailure exception)
                atomically retry)
            `catchAsync` \(exception :: AsyncException) -> do
                atomically $
                    recordEventPumpFailure
                        pump
                        (EventPumpAsyncFailure exception)
                Exception.throwIO exception

-- | Enqueue an event that forms a coalescing boundary.
emitEvent :: EventPump key event -> event -> IO ()
emitEvent pump event =
    enqueueEventPumpCommand pump (DeliverEvent event)

flushEventPump
    :: EventPump key event
    -> IO (Either EventPumpFailure ())
flushEventPump pump = do
    flushed <- newEmptyTMVarIO
    tryAny (enqueueEventPumpCommand pump (FlushEvents flushed)) >>= \case
        Left exception
            | isAsyncException exception ->
                Exception.throwIO exception
            | otherwise ->
                pure (Left (EventPumpSyncFailure exception))
        Right () ->
            atomically $
                (Left <$> readTMVar pump.eventPumpFailure)
                    `orElse`
                (readTMVar flushed >> pure (Right ()))

enqueueEventPumpCommand
    :: EventPump key event
    -> EventPumpCommand key event
    -> IO ()
enqueueEventPumpCommand pump command =
    atomically
        ( (Left <$> readTMVar pump.eventPumpFailure)
            `orElse`
          (do
            clearEventPumpTail pump
            writeTBQueue pump.eventPumpQueue command
            pure (Right ()))
        ) >>= either throwEventPumpFailure pure

-- | Enqueue text, appending it to an adjacent event with the same key.
emitAppendedText
    :: Eq key
    => EventPump key event
    -> key
    -> (Text -> event)
    -> Text
    -> IO ()
emitAppendedText pump key build text =
    atomically
        ( (Left <$> readTMVar pump.eventPumpFailure)
            `orElse`
          (do
            current <- readTVar pump.eventPumpTail
            case current of
                Just (AppendedEvent currentKey _ buffer)
                    | currentKey == key -> do
                        modifyTVar' buffer (appendTextBuffer text)
                        pure (Right ())
                _ -> do
                    buffer <- newTVar
                        (appendTextBuffer text emptyTextBuffer)
                    let pending = AppendedEvent key build buffer
                    writeTVar pump.eventPumpTail (Just pending)
                    writeTBQueue pump.eventPumpQueue
                        (DeliverCoalescedEvent pending)
                    pure (Right ()))
        ) >>= either throwEventPumpFailure pure

-- | Enqueue text, replacing an adjacent snapshot with the same key.
emitLatestText
    :: Eq key
    => EventPump key event
    -> key
    -> (Text -> event)
    -> Text
    -> IO ()
emitLatestText pump key build text =
    atomically
        ( (Left <$> readTMVar pump.eventPumpFailure)
            `orElse`
          (do
            current <- readTVar pump.eventPumpTail
            case current of
                Just (ReplacedEvent currentKey _ snapshot)
                    | currentKey == key -> do
                        writeTVar snapshot text
                        pure (Right ())
                _ -> do
                    snapshot <- newTVar text
                    let pending = ReplacedEvent key build snapshot
                    writeTVar pump.eventPumpTail (Just pending)
                    writeTBQueue pump.eventPumpQueue
                        (DeliverCoalescedEvent pending)
                    pure (Right ()))
        ) >>= either throwEventPumpFailure pure

sameCoalescedEvent
    :: Eq key
    => Maybe (CoalescedEvent key event)
    -> CoalescedEvent key event
    -> Bool
sameCoalescedEvent current pending =
    case (current, pending) of
        ( Just (AppendedEvent leftKey _ leftBuffer)
          , AppendedEvent rightKey _ rightBuffer
          ) ->
            leftKey == rightKey && leftBuffer == rightBuffer
        ( Just (ReplacedEvent leftKey _ leftSnapshot)
          , ReplacedEvent rightKey _ rightSnapshot
          ) ->
            leftKey == rightKey && leftSnapshot == rightSnapshot
        _ -> False

materializeCoalescedEvent :: CoalescedEvent key event -> STM event
materializeCoalescedEvent = \case
    AppendedEvent _ build buffer ->
        build . textBufferToText <$> readTVar buffer
    ReplacedEvent _ build snapshot ->
        build <$> readTVar snapshot

clearEventPumpTail :: EventPump key event -> STM ()
clearEventPumpTail pump =
    readTVar pump.eventPumpTail >>= \case
        Nothing -> pure ()
        Just _ -> writeTVar pump.eventPumpTail Nothing

whenSTM :: Bool -> STM () -> STM ()
whenSTM True action = action
whenSTM False _ = pure ()

waitEventPumpFailure
    :: Async ()
    -> EventPump key event
    -> IO EventPumpFailure
waitEventPumpFailure worker pump =
    race
        (atomically (readTMVar pump.eventPumpFailure))
        (waitCatch worker) >>= \case
            Left failure -> pure failure
            Right (Left exception)
                | isAsyncException exception ->
                    Exception.throwIO exception
                | otherwise ->
                    pure (EventPumpSyncFailure exception)
            Right (Right ()) ->
                pure . EventPumpSyncFailure . toException $
                    userError "event pump stopped unexpectedly"

throwEventPumpFailure :: EventPumpFailure -> IO a
throwEventPumpFailure = \case
    EventPumpSyncFailure exception -> throwIO exception
    EventPumpAsyncFailure exception -> Exception.throwIO exception

recordEventPumpFailure :: EventPump key event -> EventPumpFailure -> STM ()
recordEventPumpFailure pump failure =
    void (tryPutTMVar pump.eventPumpFailure failure)
