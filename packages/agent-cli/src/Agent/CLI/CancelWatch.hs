-- | Watch stdin for a bare Esc during an agent turn and soft-cancel.
module Agent.CLI.CancelWatch
    ( withEscCancel
    , withStdinPaused
    , isBareEscape
    ) where

import Agent.Cancel (CancelFlag, requestCancel)
import Control.Concurrent (forkIO, killThread)
import Control.Exception.Safe (bracket, finally)
import Control.Monad (when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO
    ( BufferMode(..)
    , Handle
    , hGetBuffering
    , hGetChar
    , hIsTerminalDevice
    , hReady
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

-- | Run @action@ while Esc on a TTY stdin requests soft cancel.
-- Restores terminal attributes afterward. Non-TTY is a no-op wrapper.
--
-- @paused@ suspends stdin reads (see 'withStdinPaused') so approval prompts
-- can use cooked 'getLine' without the watcher stealing keystrokes.
withEscCancel :: CancelFlag -> IORef Bool -> IO a -> IO a
withEscCancel cancel paused action = do
    tty <- hIsTerminalDevice stdin
    if not tty
        then action
        else do
            oldTerm <- getTerminalAttributes stdInput
            oldBuf <- hGetBuffering stdin
            stopped <- newIORef False
            let enter = do
                    setTerminalAttributes stdInput (rawAttrs oldTerm) Immediately
                    hSetBuffering stdin NoBuffering
                restore = do
                    setTerminalAttributes stdInput oldTerm Immediately
                    hSetBuffering stdin oldBuf
            bracket enter (const restore) \() -> do
                tid <- forkIO (escLoop cancel paused stopped)
                action `finally` do
                    writeIORef stopped True
                    -- Unblock a stuck hWaitForInput by killing the watcher.
                    killThread tid

-- | Temporarily stop the Esc watcher and restore cooked stdin for approval.
withStdinPaused :: IORef Bool -> IO a -> IO a
withStdinPaused paused action = do
    tty <- hIsTerminalDevice stdin
    if not tty
        then action
        else do
            rawTerm <- getTerminalAttributes stdInput
            rawBuf <- hGetBuffering stdin
            let cooked =
                    flip withMode ProcessInput
                        . flip withMode EnableEcho
                        . flip withMode EchoLF
                        $ rawTerm
                enter = do
                    writeIORef paused True
                    setTerminalAttributes stdInput cooked Immediately
                    hSetBuffering stdin LineBuffering
                restore = do
                    setTerminalAttributes stdInput (rawAttrs rawTerm) Immediately
                    hSetBuffering stdin rawBuf
                    writeIORef paused False
            bracket enter (const restore) (\_ -> action)

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

-- | True when @bytes@ is a lone ESC, not the start of a CSI/SS3 sequence.
isBareEscape :: String -> Bool
isBareEscape = \case
    "\ESC" -> True
    _ -> False

escLoop :: CancelFlag -> IORef Bool -> IORef Bool -> IO ()
escLoop cancel paused stopped = go
  where
    go = do
        done <- readIORef stopped
        if done
            then pure ()
            else do
                isPaused <- readIORef paused
                if isPaused
                    then hWaitForInput stdin 100 >> go
                    else do
                        ready <- hWaitForInput stdin 100
                        done' <- readIORef stopped
                        if done'
                            then pure ()
                            else if not ready
                                then go
                                else do
                                    -- Re-check pause: approval may have started
                                    -- between wait and read.
                                    isPaused' <- readIORef paused
                                    if isPaused'
                                        then go
                                        else do
                                            c <- hGetChar stdin
                                            if c /= '\ESC'
                                                then go
                                                else do
                                                    -- Peek for CSI / SS3 so arrows do not cancel.
                                                    more <- hWaitForInput stdin 50
                                                    if not more
                                                        then requestCancel cancel
                                                        else do
                                                            c2 <- hGetChar stdin
                                                            case c2 of
                                                                '[' -> drainCsi stdin >> go
                                                                'O' -> do
                                                                    _ <- hGetChar stdin
                                                                    go
                                                                _ -> go

drainCsi :: Handle -> IO ()
drainCsi h = go
  where
    go = do
        ready <- hReady h
        when ready do
            c <- hGetChar h
            if c >= '@' && c <= '~'
                then pure ()
                else go
