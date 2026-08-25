-- | Watch stdin for a bare Esc during an agent turn and soft-cancel.
module Agent.CLI.CancelWatch
    ( drainEscapeContinuation
    , withEscCancel
    , withStdinPaused
    , isBareEscape
    ) where

import Agent.Cancel (CancelFlag, requestCancel)
import Agent.CLI.Terminal (rawAttrs)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception.Safe (bracket, finally)
import Control.Monad (void, when)
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
    , withMode
    )

-- | Run @action@ while Esc on a TTY stdin requests soft cancel.
-- Restores the pre-call terminal attributes afterward. Non-TTY is a no-op.
--
-- @paused@ suspends stdin reads (see 'withStdinPaused') so approval prompts
-- can use cooked 'getLine' without the watcher stealing keystrokes.
--
-- The watcher is never 'killThread'ed while blocked on stdin: that can leave
-- the Handle lock stuck and break the next haskeline prompt (arrow keys print
-- as raw CSI like @^[OA@). Instead we set a stop flag and wait for the
-- watcher's short poll timeout to exit.
withEscCancel :: CancelFlag -> IORef Bool -> IO a -> IO a
withEscCancel cancel paused action = do
    tty <- hIsTerminalDevice stdin
    if not tty
        then action
        else do
            oldTerm <- getTerminalAttributes stdInput
            oldBuf <- hGetBuffering stdin
            stopped <- newIORef False
            done <- newEmptyMVar
            let enter = do
                    setTerminalAttributes stdInput (rawAttrs oldTerm) Immediately
                    hSetBuffering stdin NoBuffering
                restore = do
                    -- Ask the watcher to exit, then wait for its poll loop.
                    writeIORef stopped True
                    void (takeMVar done)
                    setTerminalAttributes stdInput oldTerm Immediately
                    hSetBuffering stdin oldBuf
            bracket enter (const restore) \() -> do
                _ <- forkIO $
                    escLoop cancel paused stopped
                        `finally` putMVar done ()
                action

-- | Temporarily stop the Esc watcher and restore cooked stdin for approval.
withStdinPaused :: IORef Bool -> IO a -> IO a
withStdinPaused paused action = do
    tty <- hIsTerminalDevice stdin
    if not tty
        then action
        else do
            -- Snapshot the active (raw) attrs so we can put them back.
            activeTerm <- getTerminalAttributes stdInput
            activeBuf <- hGetBuffering stdin
            let cooked = cookedAttrs activeTerm
                enter = do
                    writeIORef paused True
                    -- Give the watcher one poll interval to observe @paused@
                    -- before we start reading approval input.
                    threadDelay 120000
                    setTerminalAttributes stdInput cooked Immediately
                    hSetBuffering stdin LineBuffering
                restore = do
                    setTerminalAttributes stdInput activeTerm Immediately
                    hSetBuffering stdin activeBuf
                    writeIORef paused False
            bracket enter (const restore) (\_ -> action)

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
                                                            drainEscapeContinuation stdin c2
                                                            go

-- | Consume the remainder of an escape sequence without ever waiting
-- indefinitely for a fragmented terminal input. SS3 sequences normally have
-- one byte after @ESC O@, but terminals, tmux, or a truncated paste may omit
-- it while the writer remains open.
drainEscapeContinuation :: Handle -> Char -> IO ()
drainEscapeContinuation h = \case
    '[' -> drainCsi h
    'O' -> do
        ready <- hWaitForInput h 50
        when ready (void (hGetChar h))
    _ -> pure ()

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
