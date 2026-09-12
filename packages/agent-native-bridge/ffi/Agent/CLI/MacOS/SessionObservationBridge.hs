{-# LANGUAGE ForeignFunctionInterface #-}

-- | Typed read-only session observation. The foreign handle owns one worker;
-- cancellation races a scoped socket reader, and destruction joins that worker.
module Agent.CLI.MacOS.SessionObservationBridge
    ( SessionObservationCallback
    , ha_session_observation_start
    , ha_session_observation_cancel
    , ha_session_observation_destroy
    , deliverUpdate
    ) where

import Agent.CLI.MacOS.Marshalling (decodeUtf8Input, withText)
import qualified Agent.Runtime.Session.Observation as Observation
import Control.Concurrent (MVar, newEmptyMVar, putMVar, readMVar, tryPutMVar)
import Control.Concurrent.Async (Async, asyncWithUnmask, cancel, race, waitCatch)
import Control.Exception.Safe (mask, onException, tryAny)
import Control.Monad (void, when)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8, Word32, Word64)
import Foreign
    ( FunPtr, Ptr, castPtr, castPtrToStablePtr, castStablePtrToPtr
    , deRefStablePtr, freeStablePtr, newStablePtr, nullFunPtr, nullPtr, poke
    )
import Foreign.C.Types (CInt(..), CSize(..))

type SessionObservationCallback =
    Ptr () -> CInt
    -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize
    -> Word64 -> Int64 -> Int64
    -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize
    -> Word32 -> IO ()

foreign import ccall "dynamic"
    invokeSessionObservationCallback
        :: FunPtr SessionObservationCallback -> SessionObservationCallback

foreign export ccall ha_session_observation_start
    :: Ptr Word8 -> CSize -> FunPtr SessionObservationCallback
    -> Ptr () -> Ptr (Ptr ()) -> IO CInt

foreign export ccall ha_session_observation_cancel :: Ptr () -> IO ()
foreign export ccall ha_session_observation_destroy :: Ptr () -> IO ()

data SessionObservationHandle = SessionObservationHandle
    { cancellation :: !(MVar ())
    , worker :: !(Async ())
    }

ha_session_observation_start
    :: Ptr Word8 -> CSize -> FunPtr SessionObservationCallback
    -> Ptr () -> Ptr (Ptr ()) -> IO CInt
ha_session_observation_start sessionBytes sessionLength callback context outHandle = do
    when (outHandle /= nullPtr) (poke outHandle nullPtr)
    if sessionBytes == nullPtr || sessionLength == 0 || sessionLength > 4096
        || callback == nullFunPtr || outHandle == nullPtr
    then pure 1
    else decodeUtf8Input sessionBytes (fromIntegral sessionLength) >>= \case
        Left _ -> pure 1
        Right sessionID
            | Text.any (== '\0') sessionID -> pure 1
            | otherwise -> do
                started <- tryAny (startObservation sessionID callback context outHandle)
                pure (either (const 2) (const 0) started)

startObservation
    :: Text -> FunPtr SessionObservationCallback -> Ptr () -> Ptr (Ptr ())
    -> IO ()
startObservation sessionID callback context outHandle =
    -- The only masking is the foreign ownership transfer: the worker must be
    -- registered in the handle before it can receive cancellation or callbacks.
    -- Socket acquisition and consumption run unmasked inside the scoped race.
    mask \_ -> do
        gate <- newEmptyMVar
        cancellation <- newEmptyMVar
        worker <- asyncWithUnmask \unmask -> do
            readMVar gate
            result <- tryAny (unmask (race
                (readMVar cancellation)
                (Observation.observeSession sessionID
                    (deliverUpdate callback context))))
            case result of
                Left exception -> deliverTerminal callback context 103
                    (Text.pack (show exception))
                Right (Left ()) -> deliverTerminal callback context 102 ""
                Right (Right ()) -> deliverTerminal callback context 103
                    "Session observation ended unexpectedly."
        let handle = SessionObservationHandle{cancellation, worker}
        stable <- newStablePtr handle `onException` cancel worker
        (poke outHandle (castStablePtrToPtr stable) >> putMVar gate ())
            `onException` (cancel worker >> freeStablePtr stable)

ha_session_observation_cancel :: Ptr () -> IO ()
ha_session_observation_cancel pointer =
    when (pointer /= nullPtr) do
        handle <- deRefStablePtr (castPtrToStablePtr pointer)
        void (tryPutMVar (handle :: SessionObservationHandle).cancellation ())

ha_session_observation_destroy :: Ptr () -> IO ()
ha_session_observation_destroy pointer =
    when (pointer /= nullPtr) do
        let stable = castPtrToStablePtr pointer
        handle <- deRefStablePtr stable
        void (tryPutMVar (handle :: SessionObservationHandle).cancellation ())
        void (waitCatch handle.worker)
        freeStablePtr stable

deliverTerminal
    :: FunPtr SessionObservationCallback -> Ptr () -> CInt -> Text -> IO ()
deliverTerminal callback context kind text =
    deliverFields callback context kind "" "" 0 0 0 text "" "" "" 0

deliverUpdate
    :: FunPtr SessionObservationCallback -> Ptr ()
    -> Observation.SessionObservationUpdate -> IO ()
deliverUpdate callback context = \case
    Observation.ObservationUnavailable ->
        deliverTerminal callback context 100 "Live updates are unavailable from this CLI."
    Observation.ObservationDisconnected ->
        deliverTerminal callback context 101 "The CLI disconnected. Reconnecting…"
    Observation.ObservationFrame frame -> do
        let send kind text callID name summary flags =
                deliverFields callback context kind frame.ownerID frame.turnID
                    frame.sequence frame.generationStart frame.durableTurnCount text callID name summary flags
            stateFlags = case frame.state of
                Observation.ObservationRunning -> 0
                Observation.ObservationWaiting -> 1
                Observation.ObservationCompleted -> 2
                Observation.ObservationInterrupted -> 3
            flags = stateFlags + if frame.truncated then 256 else 0
        when frame.reset (send 1 frame.userText "" "" "" flags)
        mapM_ (deliverEvent send) frame.events
        when (frame.state == Observation.ObservationCompleted
                || frame.state == Observation.ObservationInterrupted) $
            send 8 "" "" "" "" flags
        send 2 "" "" "" "" flags

deliverEvent
    :: (CInt -> Text -> Text -> Text -> Text -> Word32 -> IO ())
    -> Observation.SessionObservationEvent -> IO ()
deliverEvent send event =
    let message kind text = send kind text "" "" "" 0
        flags = (if event.argumentsEncrypted then 1 else 0)
            + (if event.isTruncated then 2 else 0)
            + (if event.isAsync then 4 else 0)
            + (if event.isError then 8 else 0)
        tool kind = send kind event.text event.identifier event.name event.summary flags
    in case event.kind of
        Observation.ObservationText -> message 3 event.text
        Observation.ObservationReasoning -> message 4 event.text
        Observation.ObservationPlan -> message 3 event.text
        Observation.ObservationActivity -> message 5 event.text
        Observation.ObservationWarning -> message 5 event.text
        Observation.ObservationResponseRestarted -> message 11 event.text
        Observation.ObservationResponseDiscarded -> message 12 ""
        Observation.ObservationResponseFailed -> message 13 event.text
        Observation.ObservationToolStarted ->
            send 6 event.arguments event.identifier event.name event.summary flags
        Observation.ObservationToolUpdated ->
            send 6 event.arguments event.identifier event.name event.summary flags
        Observation.ObservationToolFinished -> tool 7
        Observation.ObservationToolOutput -> tool 9
        Observation.ObservationToolRetracted -> tool 10
        Observation.ObservationAgentStarted ->
            message 5 ("Agent started: " <> event.text)
        Observation.ObservationAgentOutput -> message 5 event.text
        Observation.ObservationAgentFinished -> message 5 event.text

deliverFields
    :: FunPtr SessionObservationCallback -> Ptr () -> CInt
    -> Text -> Text -> Word64 -> Int64 -> Int64 -> Text -> Text -> Text -> Text
    -> Word32 -> IO ()
deliverFields callback context kind ownerID turnID sequence generationStart durableTurnCount
    text callID toolName toolSummary flags =
    withText ownerID \ownerPointer ownerLength ->
    withText turnID \turnPointer turnLength ->
    withText text \textPointer textLength ->
    withText callID \callPointer callLength ->
    withText toolName \namePointer nameLength ->
    withText toolSummary \summaryPointer summaryLength ->
        invokeSessionObservationCallback callback context kind
            (castPtr ownerPointer) ownerLength
            (castPtr turnPointer) turnLength sequence generationStart durableTurnCount
            (castPtr textPointer) textLength
            (castPtr callPointer) callLength
            (castPtr namePointer) nameLength
            (castPtr summaryPointer) summaryLength flags
