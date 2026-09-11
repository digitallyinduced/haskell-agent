-- | Double Ctrl-C: first press soft-cancels a turn (or warns at the idle
-- prompt); a second press requests session exit with the usual --resume hint.
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
    , resetIdleTurnCancel
    , noteIdleCtrlC
    , isWrappedUserInterrupt
    , catchUserInterrupt
    , retryUserInterruptOnce
    , noteFullscreenCtrlC
    ) where

import Agent.Cancel (CancelFlag, isCancelled, requestCancel, resetCancel)
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
    , catchAny
    , catchAsync
    , catchIO
    , mask
    , throwIO
    )
import Control.Monad (void)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import System.Posix.Signals
    ( Handler(..)
    , installHandler
    , sigHUP
    , sigINT
    , sigTERM
    )

-- | How long after a warning a second Ctrl-C still means exit.
exitConfirmWindow :: NominalDiffTime
exitConfirmWindow = 2

data CtrlCContext
    = Idle
    -- | @True@ when the active turn's cancel flag is already latched.
    | TurnActive Bool
    -- | A confirmed quit is already in flight; further signals stay ForceExit
    -- instead of returning to the idle warning.
    | Exiting
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
decideCtrlC Exiting _ = ForceExit

data InterruptState = InterruptState
    { interruptActiveCancel :: !(IORef (Maybe CancelFlag))
    , interruptLastWarn :: !(IORef (Maybe UTCTime))
    , interruptExiting :: !(IORef Bool)
    , interruptOnMessage :: !(Text -> IO ())
    }

-- | @onMessage@ prints user-facing hints (already styled by the caller).
newInterruptState :: (Text -> IO ()) -> IO InterruptState
newInterruptState onMessage = do
    active <- newIORef Nothing
    lastWarn <- newIORef Nothing
    exiting <- newIORef False
    pure InterruptState
        { interruptActiveCancel = active
        , interruptLastWarn = lastWarn
        , interruptExiting = exiting
        , interruptOnMessage = onMessage
        }

-- | Install SIGINT/SIGHUP/SIGTERM handlers for the dynamic extent of @action@.
-- Restores the previous handlers afterward. Force-exit rethrows
-- 'UserInterrupt' on the thread that entered this wrapper. This session-level
-- handler must never terminate its host process: the owner may be GHCi or an
-- embedded runtime, and remains responsible for releasing session resources.
--
-- The inline editor reads Ctrl-C directly while a prompt is active; use
-- 'noteIdleCtrlC' from that path instead. Fullscreen raw mode uses
-- 'noteFullscreenCtrlC'.
withCtrlCHandler :: InterruptState -> IO a -> IO a
withCtrlCHandler state action = do
    mainTid <- myThreadId
    let sigint = Catch (onSigInt mainTid state)
        hangup = Catch (onHangup mainTid state)
    withHandler sigINT sigint $
        withHandler sigHUP hangup $
            withHandler sigTERM hangup action
  where
    -- Nest acquisition so a later installation failure also restores every
    -- handler already installed.
    withHandler signal handler continuation =
        bracket
            (installHandler signal handler Nothing)
            (\previous -> void (installHandler signal previous Nothing))
            (const continuation)

-- | Mark @cancel@ as the in-flight turn target for soft Ctrl-C.
-- Nested work (for example voice delegations) must not erase a parent hangup.
resetIdleTurnCancel :: InterruptState -> CancelFlag -> IO ()
resetIdleTurnCancel state cancel = do
    active <- readIORef state.interruptActiveCancel
    case active of
        Nothing -> resetCancel cancel
        Just _ -> pure ()

withTurnCancel :: InterruptState -> CancelFlag -> IO a -> IO a
withTurnCancel state cancel action =
    bracket
        (do
            previous <- readIORef state.interruptActiveCancel
            writeIORef state.interruptActiveCancel (Just cancel)
            pure previous)
        (writeIORef state.interruptActiveCancel)
        (const action)

-- | Apply idle Ctrl-C policy from the inline editor.
noteIdleCtrlC :: InterruptState -> IO IdleCtrlCResult
noteIdleCtrlC state = do
    now <- getCurrentTime
    context <- ctrlCContext state
    withinWindow <- isWithinWarnWindow state now
    case decideCtrlC context withinWindow of
        WarnExit -> do
            writeIORef state.interruptLastWarn (Just now)
            notify state "Press Ctrl-C again to exit"
            pure ContinuePrompt
        ForceExit -> do
            armForceExit state
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
    context <- ctrlCContext state
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
            armForceExit state
    pure decision

onSigInt :: ThreadId -> InterruptState -> IO ()
onSigInt mainTid state = do
    now <- getCurrentTime
    mCancel <- readIORef state.interruptActiveCancel
    withinWindow <- isWithinWarnWindow state now
    ctx <- ctrlCContext state
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
        ForceExit ->
            requestForceExit mainTid state

-- | Closing the terminal or SIGTERM is a confirmed quit: there is no second
-- keypress, and GHCi ignores SIGHUP by default so the agent must handle it.
onHangup :: ThreadId -> InterruptState -> IO ()
onHangup = requestForceExit

ctrlCContext :: InterruptState -> IO CtrlCContext
ctrlCContext state = do
    exiting <- readIORef state.interruptExiting
    if exiting
        then pure Exiting
        else do
            mCancel <- readIORef state.interruptActiveCancel
            case mCancel of
                Nothing -> pure Idle
                Just cancel ->
                    TurnActive <$> isCancelled cancel

armForceExit :: InterruptState -> IO ()
armForceExit state = do
    writeIORef state.interruptExiting True
    writeIORef state.interruptLastWarn Nothing

requestForceExit :: ThreadId -> InterruptState -> IO ()
requestForceExit mainTid state = do
    armForceExit state
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

-- | Handle both forms in which Ctrl-C can reach the CLI: asynchronously from
-- the RTS/SIGINT handler, or synchronously wrapped by safe-exceptions.
catchUserInterrupt :: IO a -> IO a -> IO a
catchUserInterrupt action onInterrupt =
    (action `catchAny` handleSyncException) `catchAsync` handleAsyncException
  where
    handleAsyncException (e :: AsyncException) =
        case e of
            UserInterrupt -> onInterrupt
            _ -> throwIO e
    handleSyncException (e :: SomeException)
        | isWrappedUserInterrupt e = onInterrupt
        | otherwise = throwIO e

-- | Retry an idempotent action after at most one user interrupt. Both attempts
-- run in the caller's original masking state, so another interrupt during the
-- recovery attempt propagates instead of disabling force-exit indefinitely.
retryUserInterruptOnce :: IO a -> IO a
retryUserInterruptOnce action =
    mask \restore ->
        catchUserInterrupt (restore action) (restore action)
