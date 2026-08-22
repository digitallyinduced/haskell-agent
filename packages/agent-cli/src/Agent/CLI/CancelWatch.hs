-- | Watch stdin for a bare Esc during an agent turn and soft-cancel.
module Agent.CLI.CancelWatch
    ( StdinGate
    , newStdinGate
    , withEscCancel
    , withStdinPaused
    , isBareEscape
    , readBareEscape
    ) where

import Agent.Cancel (CancelFlag, requestCancel)
import Control.Concurrent (ThreadId, myThreadId, threadDelay)
import Control.Concurrent.Async
    ( Async
    , cancel
    , race
    , waitCatch
    , withAsync
    )
import Control.Concurrent.STM
    ( STM
    , TVar
    , atomically
    , modifyTVar'
    , newTVar
    , newTVarIO
    , readTVar
    , readTVarIO
    , retry
    , throwSTM
    , writeTVar
    )
import Control.Exception.Safe
    ( SomeException
    , bracket
    , bracket_
    , finally
    , onException
    , throwIO
    , tryAny
    )
import Control.Monad (unless, void, when)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import System.IO
    ( BufferMode(..)
    , Handle
    , hGetBuffering
    , hIsTerminalDevice
    , hSetBuffering
    , hWaitForInput
    , stdin
    )
import System.Posix.IO (stdInput)
import System.Posix.Terminal
    ( TerminalAttributes
    , TerminalMode(..)
    , TerminalState(..)
    , getTerminalAttributes
    , setTerminalAttributes
    , withMinInput
    , withMode
    , withTime
    , withoutMode
    )

-- | Exclusive ownership of stdin shared by the Esc watcher and interactive
-- prompts. Prompt waiters take priority over another watcher poll, and
-- ownership is reentrant for nested prompt helpers on the same thread.
data StdinGate = StdinGate
    !(TVar (Maybe (ThreadId, Int)))
    !(TVar Int)

newStdinGate :: IO StdinGate
newStdinGate =
    atomically $
        StdinGate <$> newTVar Nothing <*> newTVar 0

-- | Run @action@ while Esc on a TTY stdin requests soft cancel.
-- Restores the pre-call terminal attributes afterward. Non-TTY is a no-op.
--
-- @stdinGate@ gives either the watcher or a prompt exclusive ownership of
-- stdin. The watcher holds it across readiness checks and non-blocking reads,
-- so a prompt never has to rely on a timed pause being observed.
--
-- The watcher is never 'killThread'ed while blocked on stdin: that can leave
-- the Handle lock stuck and break the next haskeline prompt (arrow keys print
-- as raw CSI like @^[OA@). Reads are non-blocking after a bounded poll, so
-- normal cleanup asks the watcher to stop and joins it before restoring stdin.
withEscCancel :: CancelFlag -> StdinGate -> IO a -> IO a
withEscCancel cancelFlag stdinGate action = do
    tty <- hIsTerminalDevice stdin
    if not tty
        then action
        else do
            stopped <- newTVarIO False
            bracket
                (prepareRawTerminal stdinGate)
                (uncurry (restoreOwnedTerminalState stdinGate))
                \_ ->
                    withAsync (escLoop cancelFlag stdinGate stopped) \watcher ->
                        action `finally` stopWatcher stopped watcher

-- | Own stdin for the duration of an interactive prompt. On a TTY this also
-- switches from the Esc watcher's raw mode to cooked input and restores the
-- exact prior state afterward. Non-TTY prompts are still serialized.
withStdinPaused :: StdinGate -> IO a -> IO a
withStdinPaused stdinGate action =
    withPromptOwnership stdinGate do
        tty <- hIsTerminalDevice stdin
        if not tty
            then action
            else do
                -- Snapshot the active (raw) attrs so nested uses restore the
                -- state established by their immediate caller.
                activeTerm <- getTerminalAttributes stdInput
                activeBuf <- hGetBuffering stdin
                let restore =
                        restoreTerminalState activeTerm activeBuf
                    enter = do
                        setTerminalAttributes
                            stdInput
                            (cookedAttrs activeTerm)
                            Immediately
                        hSetBuffering stdin LineBuffering
                (enter `onException` restore)
                    >> action `finally` restore

rawAttrs :: TerminalAttributes -> TerminalAttributes
rawAttrs oldTerm =
    flip withMinInput 1
        . flip withTime 0
        . flip withoutMode EnableEcho
        -- Non-canonical so VMIN/VTIME expose a lone Esc.
        . flip withoutMode ProcessInput
        -- Keep Ctrl-C as SIGINT / UserInterrupt.
        . flip withMode KeyboardInterrupts
        $ oldTerm

cookedAttrs :: TerminalAttributes -> TerminalAttributes
cookedAttrs term =
    flip withMode ProcessInput
        . flip withMode EnableEcho
        . flip withMode EchoLF
        $ term

-- | True when @bytes@ is a lone ESC, not the start of a CSI/SS3 sequence.
isBareEscape :: String -> Bool
isBareEscape = \case
    "\ESC" -> True
    _ -> False

-- | After an ESC byte has been consumed, wait briefly for a CSI/SS3
-- continuation. Every read is non-blocking, so a truncated @ESC O@ sequence
-- cannot strand the watcher while its owner is trying to restore the terminal.
readBareEscape :: Handle -> IO Bool
readBareEscape handle =
    waitForByte handle escapeContinuationMs >>= \case
        Nothing -> pure True
        Just byte
            | byte == csiByte ->
                drainCsi handle maxCsiBytes >> pure False
            | byte == ss3Byte ->
                void (waitForByte handle escapeContinuationMs) >> pure False
            | otherwise ->
                pure False

escLoop :: CancelFlag -> StdinGate -> TVar Bool -> IO ()
escLoop cancelFlag stdinGate stopped = go
  where
    go = do
        done <- readTVarIO stopped
        unless done do
            continue <- withWatcherOwnership stdinGate do
                stoppedBeforePoll <- readTVarIO stopped
                if stoppedBeforePoll
                    then pure False
                    else do
                        ready <- hWaitForInput stdin 100
                        stoppedAfterPoll <- readTVarIO stopped
                        if stoppedAfterPoll
                            then pure False
                            else if not ready
                                then pure True
                                else
                                    readByteNow stdin >>= \case
                                        Just byte | byte == escapeByte ->
                                            readBareEscape stdin >>= \case
                                                True -> do
                                                    requestCancel cancelFlag
                                                    pure False
                                                False -> pure True
                                        _ -> pure True
            when continue go

prepareRawTerminal
    :: StdinGate
    -> IO (TerminalAttributes, BufferMode)
prepareRawTerminal stdinGate =
    withPromptOwnership stdinGate do
        oldTerm <- getTerminalAttributes stdInput
        oldBuf <- hGetBuffering stdin
        let restore = restoreTerminalState oldTerm oldBuf
            enter = do
                setTerminalAttributes stdInput (rawAttrs oldTerm) Immediately
                hSetBuffering stdin NoBuffering
        (enter `onException` restore)
            >> pure (oldTerm, oldBuf)

restoreOwnedTerminalState
    :: StdinGate
    -> TerminalAttributes
    -> BufferMode
    -> IO ()
restoreOwnedTerminalState stdinGate attributes buffering =
    withPromptOwnership stdinGate $
        restoreTerminalState attributes buffering

withPromptOwnership :: StdinGate -> IO a -> IO a
withPromptOwnership stdinGate action = do
    owner <- myThreadId
    bracket_
        (acquirePromptOwnership stdinGate owner)
        (releaseOwnership stdinGate owner)
        action

withWatcherOwnership :: StdinGate -> IO a -> IO a
withWatcherOwnership stdinGate action = do
    owner <- myThreadId
    bracket_
        (atomically (acquireWatcherOwnership stdinGate owner))
        (releaseOwnership stdinGate owner)
        action

acquirePromptOwnership :: StdinGate -> ThreadId -> IO ()
acquirePromptOwnership stdinGate@(StdinGate _ promptWaiters) owner = do
    atomically $ modifyTVar' promptWaiters (+ 1)
    atomically acquireAndUnregister
        `onException` atomically unregister
  where
    acquireAndUnregister = do
        acquireOwnership stdinGate owner
        unregister
    unregister = do
        waiting <- readTVar promptWaiters
        if waiting <= 0
            then throwSTM $
                userError "stdin prompt waiter count underflow"
            else writeTVar promptWaiters (waiting - 1)

acquireWatcherOwnership :: StdinGate -> ThreadId -> STM ()
acquireWatcherOwnership stdinGate@(StdinGate _ promptWaiters) owner = do
    currentOwner <- stdinOwner stdinGate
    case currentOwner of
        Just (heldBy, _)
            | heldBy == owner ->
                acquireOwnership stdinGate owner
        _ -> do
            waiting <- readTVar promptWaiters
            if waiting == 0
                then acquireOwnership stdinGate owner
                else retry

acquireOwnership :: StdinGate -> ThreadId -> STM ()
acquireOwnership (StdinGate ownerRef _) owner = do
    currentOwner <- readTVar ownerRef
    case currentOwner of
        Nothing ->
            writeTVar ownerRef (Just (owner, 1))
        Just (heldBy, depth)
            | heldBy == owner ->
                writeTVar ownerRef (Just (owner, depth + 1))
            | otherwise ->
                retry

stdinOwner :: StdinGate -> STM (Maybe (ThreadId, Int))
stdinOwner (StdinGate ownerRef _) =
    readTVar ownerRef

releaseOwnership :: StdinGate -> ThreadId -> IO ()
releaseOwnership (StdinGate ownerRef _) owner =
    atomically do
        currentOwner <- readTVar ownerRef
        case currentOwner of
            Just (heldBy, depth)
                | heldBy == owner
                , depth > 1 ->
                    writeTVar ownerRef (Just (owner, depth - 1))
                | heldBy == owner ->
                    writeTVar ownerRef Nothing
            _ ->
                throwSTM $
                    userError "stdin ownership released by a non-owner"

stopWatcher :: TVar Bool -> Async () -> IO ()
stopWatcher stopped watcher = do
    atomically $ writeTVar stopped True
    race
        (threadDelay watcherStopTimeoutMicros)
        (waitCatch watcher)
        >>= \case
            Right _ -> pure ()
            Left () -> do
                cancel watcher
                void (waitCatch watcher)

restoreTerminalState :: TerminalAttributes -> BufferMode -> IO ()
restoreTerminalState attributes buffering = do
    terminalResult <-
        tryAny (setTerminalAttributes stdInput attributes Immediately)
    bufferingResult <- tryAny (hSetBuffering stdin buffering)
    rethrowFirst terminalResult bufferingResult
  where
    rethrowFirst
        :: Either SomeException ()
        -> Either SomeException ()
        -> IO ()
    rethrowFirst (Left exception) _ = throwIO exception
    rethrowFirst (Right ()) (Left exception) = throwIO exception
    rethrowFirst (Right ()) (Right ()) = pure ()

waitForByte :: Handle -> Int -> IO (Maybe Word8)
waitForByte handle timeoutMs = do
    ready <- hWaitForInput handle timeoutMs
    if ready
        then readByteNow handle
        else pure Nothing

readByteNow :: Handle -> IO (Maybe Word8)
readByteNow handle = do
    bytes <- BS.hGetNonBlocking handle 1
    pure (fst <$> BS.uncons bytes)

drainCsi :: Handle -> Int -> IO ()
drainCsi _ remaining | remaining <= 0 = pure ()
drainCsi handle remaining =
    waitForByte handle escapeContinuationMs >>= \case
        Just byte
            | isCsiFinal byte -> pure ()
            | otherwise -> drainCsi handle (remaining - 1)
        Nothing -> pure ()

isCsiFinal :: Word8 -> Bool
isCsiFinal byte = byte >= 0x40 && byte <= 0x7e

escapeByte, csiByte, ss3Byte :: Word8
escapeByte = 0x1b
csiByte = 0x5b
ss3Byte = 0x4f

escapeContinuationMs :: Int
escapeContinuationMs = 50

maxCsiBytes :: Int
maxCsiBytes = 16

watcherStopTimeoutMicros :: Int
watcherStopTimeoutMicros = 2_000_000
