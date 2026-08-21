-- | Double Ctrl-C: first press soft-cancels a turn (or warns at the idle
-- prompt); a second press forces process exit with the usual --resume hint.
module Agent.CLI.Interrupt
    ( InterruptState
    , CtrlCContext(..)
    , CtrlCDecision(..)
    , IdleCtrlCResult(..)
    , exitConfirmWindow
    , decideCtrlC
    , newInterruptState
    , withCtrlCHandler
    , withTurnCancel
    , noteIdleCtrlC
    , isWrappedUserInterrupt
    , noteFullscreenCtrlC
    ) where

import Agent.Cancel (CancelFlag, isCancelled, requestCancel)
import Control.Concurrent (ThreadId, myThreadId, throwTo)
import Control.Exception
    ( AsyncException(UserInterrupt)
    , fromException
    , toException
    )
import Control.Exception.Safe
    ( SomeException
    , SyncExceptionWrapper(..)
    , bracket
    , bracket_
    , catchIO
    )
import Control.Monad (void)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import System.Posix.Signals
    ( Handler(..)
    , installHandler
    , sigINT
    )

-- | How long after a warning a second Ctrl-C still means exit.
exitConfirmWindow :: NominalDiffTime
exitConfirmWindow = 2

data CtrlCContext
    = Idle
    -- | @True@ when the active turn's cancel flag is already latched.
    | TurnActive Bool
    deriving (Eq, Show)

data CtrlCDecision
    = SoftCancel
    | WarnExit
    | ForceExit
    deriving (Eq, Show)

-- | Result of Ctrl-C at the idle REPL prompt.
data IdleCtrlCResult
    = ContinuePrompt
    | QuitProcess
    deriving (Eq, Show)

-- | Pure policy used by the SIGINT handler and idle-prompt catcher.
decideCtrlC :: CtrlCContext -> Bool -> CtrlCDecision
decideCtrlC Idle withinWindow
    | withinWindow = ForceExit
    | otherwise = WarnExit
decideCtrlC (TurnActive alreadyCancelled) _
    | alreadyCancelled = ForceExit
    | otherwise = SoftCancel

data InterruptState = InterruptState
    { interruptActiveCancel :: !(IORef (Maybe CancelFlag))
    , interruptLastWarn :: !(IORef (Maybe UTCTime))
    , interruptOnMessage :: !(Text -> IO ())
    }

-- | @onMessage@ prints user-facing hints (already styled by the caller).
newInterruptState :: (Text -> IO ()) -> IO InterruptState
newInterruptState onMessage = do
    active <- newIORef Nothing
    lastWarn <- newIORef Nothing
    pure InterruptState
        { interruptActiveCancel = active
        , interruptLastWarn = lastWarn
        , interruptOnMessage = onMessage
        }

-- | Install a SIGINT handler for the dynamic extent of @action@.
-- Restores the previous handler afterward. Force-exit rethrows
-- 'UserInterrupt' on the thread that entered this wrapper.
--
-- The inline editor reads Ctrl-C directly while a prompt is active; use
-- 'noteIdleCtrlC' from that path instead.
withCtrlCHandler :: InterruptState -> IO a -> IO a
withCtrlCHandler state action = do
    mainTid <- myThreadId
    let handler = Catch (onSigInt mainTid state)
    bracket
        (installHandler sigINT handler Nothing)
        (\previous -> void (installHandler sigINT previous Nothing))
        (\_ -> action)

-- | Mark @cancel@ as the in-flight turn target for soft Ctrl-C.
withTurnCancel :: InterruptState -> CancelFlag -> IO a -> IO a
withTurnCancel state cancel =
    bracket_
        (writeIORef state.interruptActiveCancel (Just cancel))
        (writeIORef state.interruptActiveCancel Nothing)

-- | Apply idle Ctrl-C policy from the inline editor.
noteIdleCtrlC :: InterruptState -> IO IdleCtrlCResult
noteIdleCtrlC state = do
    now <- getCurrentTime
    withinWindow <- isWithinWarnWindow state now
    case decideCtrlC Idle withinWindow of
        WarnExit -> do
            writeIORef state.interruptLastWarn (Just now)
            notify state "Press Ctrl-C again to exit"
            pure ContinuePrompt
        ForceExit -> do
            writeIORef state.interruptLastWarn Nothing
            pure QuitProcess
        SoftCancel ->
            pure ContinuePrompt

-- | Apply the same double-Ctrl-C policy when a retained TUI owns stdin.
-- The caller renders the returned decision in its own UI.
noteFullscreenCtrlC :: InterruptState -> IO CtrlCDecision
noteFullscreenCtrlC state = do
    now <- getCurrentTime
    mCancel <- readIORef state.interruptActiveCancel
    withinWindow <- isWithinWarnWindow state now
    context <- case mCancel of
        Nothing -> pure Idle
        Just cancel ->
            TurnActive <$> isCancelled cancel
    let decision = decideCtrlC context withinWindow
    case decision of
        SoftCancel -> do
            case mCancel of
                Just cancel -> requestCancel cancel
                Nothing -> pure ()
            writeIORef state.interruptLastWarn (Just now)
        WarnExit ->
            writeIORef state.interruptLastWarn (Just now)
        ForceExit ->
            writeIORef state.interruptLastWarn Nothing
    pure decision

onSigInt :: ThreadId -> InterruptState -> IO ()
onSigInt mainTid state = do
    now <- getCurrentTime
    mCancel <- readIORef state.interruptActiveCancel
    withinWindow <- isWithinWarnWindow state now
    ctx <- case mCancel of
        Nothing -> pure Idle
        Just cancel -> do
            already <- isCancelled cancel
            pure (TurnActive already)
    case decideCtrlC ctx withinWindow of
        SoftCancel -> do
            case mCancel of
                Just cancel -> requestCancel cancel
                Nothing -> pure ()
            writeIORef state.interruptLastWarn (Just now)
            notify state "Interrupted; press Ctrl-C again to exit"
        WarnExit -> do
            writeIORef state.interruptLastWarn (Just now)
            notify state "Press Ctrl-C again to exit"
        ForceExit -> do
            writeIORef state.interruptLastWarn Nothing
            throwTo mainTid UserInterrupt

isWithinWarnWindow :: InterruptState -> UTCTime -> IO Bool
isWithinWarnWindow state now = do
    lastWarn <- readIORef state.interruptLastWarn
    pure $ case lastWarn of
        Just t -> diffUTCTime now t <= exitConfirmWindow
        Nothing -> False

notify :: InterruptState -> Text -> IO ()
notify state msg =
    -- Best-effort: never let printing from the signal thread fail the handler.
    state.interruptOnMessage msg `catchIO` \_ -> pure ()

-- | Recognize a 'UserInterrupt' thrown with safe-exceptions' 'throwIO'.
isWrappedUserInterrupt :: SomeException -> Bool
isWrappedUserInterrupt e =
    case fromException e of
        Just (SyncExceptionWrapper wrapped) ->
            case fromException (toException wrapped) of
                Just UserInterrupt -> True
                _ -> False
        Nothing -> False
