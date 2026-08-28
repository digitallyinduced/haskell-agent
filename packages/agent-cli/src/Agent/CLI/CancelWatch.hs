-- | Watch stdin for Esc during an agent turn and soft-cancel. Terminals with
-- the Kitty keyboard protocol active encode the Esc key as @CSI 27 u@ rather
-- than a bare ESC byte, so the watcher must decode that sequence too.
module Agent.CLI.CancelWatch
    ( EscapeContinuation(..)
    , readEscapeContinuation
    , escapeContinuationCancels
    , withEscCancel
    , withStdinPaused
    , isBareEscape
    ) where

import Agent.Cancel (CancelFlag, requestCancel)
import Agent.CLI.Input.KeyDecoder (kittyRelease, parseKittyKey)
import Agent.CLI.Input.Types (KittyKey(..))
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
                                                    -- Peek for CSI / SS3 so arrows do not cancel,
                                                    -- but a Kitty-encoded Esc key press must.
                                                    more <- hWaitForInput stdin 50
                                                    if not more
                                                        then requestCancel cancel
                                                        else do
                                                            c2 <- hGetChar stdin
                                                            continuation <-
                                                                readEscapeContinuation stdin c2
                                                            if escapeContinuationCancels continuation
                                                                then requestCancel cancel
                                                                else go

-- | The tail of an escape sequence read after a leading ESC byte.
data EscapeContinuation
    = EscapeCsi !String
    -- ^ A CSI body, including its final byte when one arrived in time.
    | EscapeOther
    deriving (Eq, Show)

-- | Consume the remainder of an escape sequence without ever waiting
-- indefinitely for a fragmented terminal input. SS3 sequences normally have
-- one byte after @ESC O@, but terminals, tmux, or a truncated paste may omit
-- it while the writer remains open. CSI bytes are read with short timed waits
-- rather than a non-blocking peek: a tail that trickles in must still be
-- consumed here, or it leaks into the next stdin reader as typed text.
readEscapeContinuation :: Handle -> Char -> IO EscapeContinuation
readEscapeContinuation h = \case
    '[' -> EscapeCsi <$> readCsiTail h
    'O' -> do
        ready <- hWaitForInput h 50
        when ready (void (hGetChar h))
        pure EscapeOther
    _ -> pure EscapeOther

readCsiTail :: Handle -> IO String
readCsiTail h = go (0 :: Int) []
  where
    go count reversed
        | count >= 32 = pure (reverse reversed)
        | otherwise = do
            ready <- hWaitForInput h 50
            if not ready
                then pure (reverse reversed)
                else do
                    c <- hGetChar h
                    let next = c : reversed
                    if c >= '@' && c <= '~'
                        then pure (reverse next)
                        else go (count + 1) next

-- | Kitty's keyboard protocol encodes the Esc key as @CSI 27 u@. A press (or
-- repeat) of that key must cancel exactly like a bare ESC byte; releases and
-- every other sequence (arrows, function keys) must not.
escapeContinuationCancels :: EscapeContinuation -> Bool
escapeContinuationCancels = \case
    EscapeCsi body -> case parseKittyKey body of
        Just KittyKey{kittyCodepoint, kittyEvent} ->
            kittyCodepoint == 27 && kittyEvent /= kittyRelease
        Nothing -> False
    EscapeOther -> False
