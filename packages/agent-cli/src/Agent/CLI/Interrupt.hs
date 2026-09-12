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
    , withSessionInterrupts
    , withSessionInterruptScope
    , requestSessionExit
    , withTurnCancel
    , resetIdleTurnCancel
    , noteIdleCtrlC
    , isWrappedUserInterrupt
    , catchUserInterrupt
    , retryUserInterruptOnce
    , noteFullscreenCtrlC
    ) where

import Agent.Cancel (CancelFlag, isCancelled, requestCancel, resetCancel)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVarMasked, modifyMVarMasked_, withMVar)
import Control.Concurrent.Async
    ( AsyncCancelled(..), cancel, waitCatch, withAsync, withAsyncWithUnmask, waitSTM )
import Control.Concurrent.STM
    ( TMVar, atomically, newEmptyTMVarIO, readTMVar, tryPutTMVar
    , isEmptyTMVar, takeTMVar, newTBQueueIO, readTBQueue, writeTBQueue, isFullTBQueue
    , orElse
    )
import Control.Exception
    ( AsyncException(UserInterrupt)
    , fromException
    , toException
    )
import Control.Exception.Safe
    ( SomeException
    , SyncExceptionWrapper(..)
    , bracket
    , catchAny
    , catchAsync
    , catchIO
    , mask
    , throwIO
    )
import Control.Monad (forever, unless, void)
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

-- | Pure policy shared by signal and keyboard input.
decideCtrlC :: CtrlCContext -> Bool -> CtrlCDecision
decideCtrlC Idle withinWindow
    | withinWindow = ForceExit
    | otherwise = WarnExit
decideCtrlC (TurnActive alreadyCancelled) _
    | alreadyCancelled = ForceExit
    | otherwise = SoftCancel
decideCtrlC Exiting _ = ForceExit

data InterruptState = InterruptState
    { interruptPolicy :: !(MVar InterruptPolicy)
    , interruptExit :: !(TMVar ())
    , interruptOnMessage :: !(Text -> IO ())
    }

data SessionEvent a = ExitRequested | CtrlCReceived | SessionCompleted a

-- Transitions and turn-target handoffs are serialized. No callback or blocking
-- IO runs while holding this lock: CancelFlag operations only update STM.
data InterruptPolicy = InterruptPolicy
    { activeCancel :: !(Maybe CancelFlag)
    , lastWarn :: !(Maybe UTCTime)
    }

-- | @onMessage@ prints user-facing hints (already styled by the caller).
newInterruptState :: (Text -> IO ()) -> IO InterruptState
newInterruptState onMessage = do
    policy <- newMVar (InterruptPolicy Nothing Nothing)
    exiting <- newEmptyTMVarIO
    pure InterruptState
        { interruptPolicy = policy
        , interruptExit = exiting
        , interruptOnMessage = onMessage
        }

-- | Own a foreground session until completion or a confirmed exit request.
-- Signal callbacks only publish intent. Keyboard adapters use the same exit
-- latch; neither adapter throws exceptions into the session or kills its host.
-- On exit, notify the UI owner before scoped cancellation joins the worker.
-- 'Nothing' is a requested quit, not a provider failure or restart.
--
-- Cancellation joins are deliberately not advertised as bounded: finalizers
-- must be interruptible. Abandoning a worker would let it use released session
-- resources. Background/embedded callers should not install process signals.
withSessionInterrupts :: InterruptState -> IO () -> IO a -> IO (Maybe a)
withSessionInterrupts state onStop = withSessionInterruptScope state onStop id

-- | Keep adapters installed through owner-side reporting as well as worker
-- teardown. The wrapper runs outside the canceled worker scope, so it can
-- report a quit or failure after joining without exposing the host's handlers.
withSessionInterruptScope
    :: InterruptState
    -> IO ()
    -> (IO (Maybe a) -> IO b)
    -> IO a
    -> IO b
withSessionInterruptScope state onStop around action = do
    signals <- newTBQueueIO 8
    notices <- newEmptyTMVarIO
    let sigint = Catch $ atomically do
            full <- isFullTBQueue signals
            exiting <- not <$> isEmptyTMVar state.interruptExit
            unless (full || exiting) (writeTBQueue signals ())
        hangup = Catch (requestSessionExit state)
        -- Rendering a notice must not prevent the next signal from quitting.
        renderNotices = forever (atomically (takeTMVar notices) >>= notify state)
        publishNotice message = atomically $ void (tryPutTMVar notices message)
        supervise worker renderer = do
            event <- atomically $
                (readTMVar state.interruptExit >> pure ExitRequested)
                    `orElse` (readTBQueue signals >> pure CtrlCReceived)
                    `orElse` (SessionCompleted <$> waitSTM worker)
                    `orElse` (waitSTM renderer >> pure ExitRequested)
            case event of
                ExitRequested -> do
                    onStop
                    cancel worker
                    -- Scoped cancellation joins the worker, but its exception
                    -- is otherwise discarded. Preserve failed resource cleanup
                    -- rather than reporting a successful requested quit.
                    waitCatch worker >>= \case
                        Right _ -> pure Nothing
                        Left exception -> case fromException exception of
                            Just AsyncCancelled -> pure Nothing
                            Nothing -> throwIO exception
                SessionCompleted result -> pure (Just result)
                CtrlCReceived -> do
                    noteCtrlC state >>= \case
                        SoftCancel -> publishNotice "Interrupted; press Ctrl-C again to exit"
                        WarnExit -> publishNotice "Press Ctrl-C again to exit"
                        ForceExit -> pure ()
                    supervise worker renderer
    withHandler sigINT sigint $
        withHandler sigHUP hangup $
            withHandler sigTERM hangup $
                around $
                withAsyncWithUnmask (\unmask -> unmask action) \worker ->
                    withAsync renderNotices (supervise worker)
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
    withMVar state.interruptPolicy \policy ->
        case policy.activeCancel of
            Nothing -> resetCancel cancel
            Just _ -> pure ()

withTurnCancel :: InterruptState -> CancelFlag -> IO a -> IO a
withTurnCancel state cancel action =
    bracket
        (modifyMVarMasked state.interruptPolicy \policy ->
            pure (policy{activeCancel = Just cancel}, policy.activeCancel))
        (\previous -> modifyMVarMasked_ state.interruptPolicy \policy ->
            pure policy{activeCancel = previous})
        (const action)

-- | Apply idle Ctrl-C policy from the inline editor.
noteIdleCtrlC :: InterruptState -> IO IdleCtrlCResult
noteIdleCtrlC state = do
    noteCtrlC state >>= \case
        WarnExit -> do
            notify state "Press Ctrl-C again to exit"
            pure ContinuePrompt
        ForceExit -> pure QuitProcess
        SoftCancel ->
            pure ContinuePrompt

-- | Apply the same double-Ctrl-C policy when a retained TUI owns stdin.
-- The caller renders the returned decision in its own UI.
noteFullscreenCtrlC :: InterruptState -> IO CtrlCDecision
noteFullscreenCtrlC = noteCtrlC

-- | One transition shared by terminal keys and the session supervisor.
noteCtrlC :: InterruptState -> IO CtrlCDecision
noteCtrlC state =
    modifyMVarMasked state.interruptPolicy \policy -> do
        now <- getCurrentTime
        exiting <- atomically (not <$> isEmptyTMVar state.interruptExit)
        context <- if exiting then pure Exiting else
            maybe (pure Idle) (fmap TurnActive . isCancelled) policy.activeCancel
        let withinWindow = case policy.lastWarn of
                Just previous -> let elapsed = diffUTCTime now previous
                    in elapsed >= 0 && elapsed <= exitConfirmWindow
                Nothing -> False
            decision = decideCtrlC context withinWindow
        case decision of
            SoftCancel -> mapM_ requestCancel policy.activeCancel
            WarnExit -> pure ()
            ForceExit -> requestSessionExit state
        pure (policy{lastWarn = if decision == ForceExit then Nothing else Just now}, decision)

-- | Nonblocking, idempotent session shutdown. Safe for signal adapters and
-- independent of turn cancellation, so nested turn scopes cannot erase it.
requestSessionExit :: InterruptState -> IO ()
requestSessionExit state = atomically $ void (tryPutTMVar state.interruptExit ())

notify :: InterruptState -> Text -> IO ()
notify state msg =
    -- Best-effort notice rendering; signal callbacks never print.
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
