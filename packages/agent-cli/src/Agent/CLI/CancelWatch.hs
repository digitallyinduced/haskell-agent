-- | Watch stdin for a bare Esc during an agent turn and soft-cancel.
module Agent.CLI.CancelWatch
    ( withEscCancel
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
    ( TerminalMode(..)
    , TerminalState(..)
    , getTerminalAttributes
    , setTerminalAttributes
    , withMinInput
    , withTime
    , withoutMode
    )

-- | Run @action@ while Esc on a TTY stdin requests soft cancel.
-- Restores terminal attributes afterward. Non-TTY is a no-op wrapper.
withEscCancel :: CancelFlag -> IO a -> IO a
withEscCancel cancel action = do
    tty <- hIsTerminalDevice stdin
    if not tty
        then action
        else do
            oldTerm <- getTerminalAttributes stdInput
            oldBuf <- hGetBuffering stdin
            stopped <- newIORef False
            let enter = do
                    let raw =
                            flip withMinInput 1
                                . flip withTime 0
                                . flip withoutMode EnableEcho
                                -- Keep ProcessInput so Ctrl-C still becomes SIGINT.
                                $ oldTerm
                    setTerminalAttributes stdInput raw Immediately
                    hSetBuffering stdin NoBuffering
                restore = do
                    setTerminalAttributes stdInput oldTerm Immediately
                    hSetBuffering stdin oldBuf
            bracket enter (const restore) \() -> do
                tid <- forkIO (escLoop cancel stopped)
                action `finally` do
                    writeIORef stopped True
                    -- Unblock a stuck hWaitForInput by killing the watcher.
                    killThread tid

-- | True when @bytes@ is a lone ESC, not the start of a CSI/SS3 sequence.
isBareEscape :: String -> Bool
isBareEscape = \case
    "\ESC" -> True
    _ -> False

escLoop :: CancelFlag -> IORef Bool -> IO ()
escLoop cancel stopped = go
  where
    go = do
        done <- readIORef stopped
        if done
            then pure ()
            else do
                ready <- hWaitForInput stdin 100
                done' <- readIORef stopped
                if done'
                    then pure ()
                    else if not ready
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
