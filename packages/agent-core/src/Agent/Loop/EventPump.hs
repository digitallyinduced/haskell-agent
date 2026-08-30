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
    , textBufferChunkCount
    , textBufferLength
    , textBufferToText
    )
import Control.Concurrent.Async (Async, race, waitCatch)
import Control.Concurrent.STM
    ( STM
    , TBQueue
    , TMVar
    , TVar
    , atomically
    , check
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
import qualified Data.Text as Text

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
    , eventPumpTailBytes :: !(TVar Int)
    , eventPumpQueuedPayloadBytes :: !(TVar Int)
    , eventPumpSink :: !(event -> IO ())
    }

eventQueueCapacity :: Int
eventQueueCapacity = 256

-- | Bound mutable payload hidden behind the final queue node. Queue capacity
-- alone is insufficient because adjacent streaming events update that node's
-- TVar without consuming another 'TBQueue' slot.
eventTailPayloadBudgetBytes :: Int
eventTailPayloadBudgetBytes = 8 * 1024 * 1024

newEventPump :: (event -> IO ()) -> IO (EventPump key event)
newEventPump sink = do
    queue <- newTBQueueIO (fromIntegral eventQueueCapacity)
    failure <- newEmptyTMVarIO
    tailEvent <- atomically (newTVar Nothing)
    tailBytes <- atomically (newTVar 0)
    queuedPayloadBytes <- atomically (newTVar 0)
    pure EventPump
        { eventPumpQueue = queue
        , eventPumpFailure = failure
        , eventPumpTail = tailEvent
        , eventPumpTailBytes = tailBytes
        , eventPumpQueuedPayloadBytes = queuedPayloadBytes
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
                    whenSTM (sameCoalescedEvent current pending) do
                        writeTVar pump.eventPumpTail Nothing
                        writeTVar pump.eventPumpTailBytes 0
                    payloadBytes <- coalescedPayloadBytes pending
                    event <- materializeCoalescedEvent pending
                    releaseQueuedPayloadBytes pump payloadBytes
                    pure event
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
emitAppendedText pump key build = go
  where
    -- Splitting makes a single provider delta larger than the budget progress
    -- instead of retrying forever. Copy retained slices so they cannot keep an
    -- oversized source buffer alive.
    go text
        | Text.null text = enqueueChunk text
        | Text.length text <= eventTailPayloadBudgetCodeUnits =
            enqueueChunk text
        | otherwise = do
            let (chunk0, rest) =
                    Text.splitAt eventTailPayloadBudgetCodeUnits text
            enqueueChunk (Text.copy chunk0)
            if Text.null rest then pure () else go rest

    enqueueChunk text =
        atomically
            ( (Left <$> readTMVar pump.eventPumpFailure)
                `orElse`
              (do
                current <- readTVar pump.eventPumpTail
                case current of
                    Just (AppendedEvent currentKey _ buffer)
                        | currentKey == key -> do
                            buffered <- readTVar buffer
                            let payloadBytes =
                                    logicalTextBufferBytes
                                        ( textBufferLength buffered
                                            + Text.length text
                                        )
                                        ( textBufferChunkCount buffered
                                            + if Text.null text then 0 else 1
                                        )
                            reserveTailPayloadBytes pump False payloadBytes
                            writeTVar buffer
                                (appendTextBuffer (Text.copy text) buffered)
                            pure (Right ())
                    _ -> do
                        clearEventPumpTail pump
                        buffer <- newTVar
                            (appendTextBuffer
                                (Text.copy text)
                                emptyTextBuffer)
                        let pending = AppendedEvent key build buffer
                            payloadBytes =
                                logicalTextBufferBytes
                                    (Text.length text)
                                    (if Text.null text then 0 else 1)
                        writeTVar pump.eventPumpTail (Just pending)
                        reserveTailPayloadBytes pump False payloadBytes
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
    let retained = Text.copy text
    in
    atomically
        ( (Left <$> readTMVar pump.eventPumpFailure)
            `orElse`
          (do
            current <- readTVar pump.eventPumpTail
            case current of
                Just (ReplacedEvent currentKey _ snapshot)
                    | currentKey == key -> do
                        writeTVar snapshot retained
                        reserveTailPayloadBytes
                            pump
                            True
                            (logicalTextBytes retained)
                        pure (Right ())
                _ -> do
                    clearEventPumpTail pump
                    snapshot <- newTVar retained
                    let pending = ReplacedEvent key build snapshot
                    writeTVar pump.eventPumpTail (Just pending)
                    reserveTailPayloadBytes
                        pump
                        True
                        (logicalTextBytes retained)
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

coalescedPayloadBytes :: CoalescedEvent key event -> STM Int
coalescedPayloadBytes = \case
    AppendedEvent _ _ buffer -> do
        buffered <- readTVar buffer
        pure
            (logicalTextBufferBytes
                (textBufferLength buffered)
                (textBufferChunkCount buffered))
    ReplacedEvent _ _ snapshot ->
        logicalTextBytes <$> readTVar snapshot

clearEventPumpTail :: EventPump key event -> STM ()
clearEventPumpTail pump =
    readTVar pump.eventPumpTail >>= \case
        Nothing -> pure ()
        Just _ -> do
            writeTVar pump.eventPumpTail Nothing
            writeTVar pump.eventPumpTailBytes 0

reserveTailPayloadBytes
    :: EventPump key event
    -> Bool
    -> Int
    -> STM ()
reserveTailPayloadBytes pump allowInitialOversize bytes = do
    oldTailBytes <- readTVar pump.eventPumpTailBytes
    queuedBytes <- readTVar pump.eventPumpQueuedPayloadBytes
    let retainedQueuedBytes = max 0 (queuedBytes - oldTailBytes)
        newQueuedBytes =
            saturatingPayloadAdd retainedQueuedBytes bytes
        initialOversize =
            allowInitialOversize
                && queuedBytes == 0
                && oldTailBytes == 0
    check
        ( newQueuedBytes <= eventTailPayloadBudgetBytes
            || initialOversize
        )
    writeTVar pump.eventPumpTailBytes bytes
    writeTVar pump.eventPumpQueuedPayloadBytes newQueuedBytes

releaseQueuedPayloadBytes :: EventPump key event -> Int -> STM ()
releaseQueuedPayloadBytes pump bytes = do
    queuedBytes <- readTVar pump.eventPumpQueuedPayloadBytes
    writeTVar pump.eventPumpQueuedPayloadBytes
        (max 0 (queuedBytes - bytes))

-- A four-byte estimate is conservative for UTF-8 while remaining constant
-- time for buffers whose code-unit count is already tracked.
logicalTextBytes :: Text -> Int
logicalTextBytes = logicalTextBytesFromLength . Text.length

logicalTextBufferBytes :: Int -> Int -> Int
logicalTextBufferBytes size chunks =
    saturatingPayloadAdd
        (logicalTextBytesFromLength size)
        (saturatingPayloadMultiply chunks 64)

logicalTextBytesFromLength :: Int -> Int
logicalTextBytesFromLength size
    | size >= maxBound `div` 4 = maxBound
    | otherwise = size * 4

saturatingPayloadMultiply :: Int -> Int -> Int
saturatingPayloadMultiply left right
    | left <= 0 || right <= 0 = 0
    | left > maxBound `div` right = maxBound
    | otherwise = left * right

saturatingPayloadAdd :: Int -> Int -> Int
saturatingPayloadAdd left right
    | right > maxBound - left = maxBound
    | otherwise = left + right

eventTailPayloadBudgetCodeUnits :: Int
eventTailPayloadBudgetCodeUnits =
    (eventTailPayloadBudgetBytes - 64) `div` 4

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
