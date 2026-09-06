-- | Watch stdin for Esc during an agent turn and soft-cancel. Terminals with
-- the Kitty keyboard protocol active encode the Esc key as @CSI 27 u@ rather
-- than a bare ESC byte, so the watcher must decode that sequence too.
module Agent.CLI.CancelWatch
    ( EscapeContinuation(..)
    , readEscapeContinuation
    , escapeContinuationCancels
    , StdinControl
    , newStdinControl
    , withStdinOwnership
    , withEscCancel
    , withStdinPaused
    , isBareEscape
    ) where

import Agent.Cancel (CancelFlag, requestCancel)
import Agent.CLI.Input.KeyDecoder (kittyRelease, parseKittyKey)
import Agent.CLI.Input.Types (KittyKey(..))
import Agent.CLI.Terminal (rawAttrs)
import Control.Concurrent (forkIOWithUnmask)
import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar, putMVar, readMVar, withMVar)
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

-- | Shared ownership of stdin between the turn watcher and interactive prompts.
-- A reader owns the lease for its entire input operation, including any
-- fragmented escape sequence. Prompt entry therefore acknowledges that the
-- watcher has finished reading instead of guessing from a polling interval.
newtype StdinControl = StdinControl (MVar ())

newStdinControl :: IO StdinControl
newStdinControl = StdinControl <$> newMVar ()

-- | Serialize stdin readers. Do not nest ownership of the same control.
withStdinOwnership :: StdinControl -> IO a -> IO a
withStdinOwnership (StdinControl owner) action = withMVar owner (const action)

-- | Run @action@ while Esc on a TTY stdin requests soft cancel.
-- Restores the pre-call terminal attributes afterward. Non-TTY is a no-op.
--
-- The watcher is never killed while blocked on stdin: that can leave the
-- Handle lock stuck. Its owner asks it to stop and joins it before restoring
-- terminal settings. A prompt can acquire stdin with 'withStdinPaused'.
withEscCancel :: CancelFlag -> StdinControl -> IO a -> IO a
withEscCancel cancel control action = do
    tty <- hIsTerminalDevice stdin
    if not tty
        then action
        else bracket snapshot restore \(oldTerm, _) -> do
            withStdinOwnership control do
                setTerminalAttributes stdInput (rawAttrs oldTerm) Immediately
                hSetBuffering stdin NoBuffering
            stopped <- newIORef False
            done <- newEmptyMVar
            let start = forkIOWithUnmask \unmask ->
                    unmask (escLoop cancel control stopped)
                        `finally` putMVar done ()
                stop _ = do
                    writeIORef stopped True
                    readMVar done
            bracket start stop (const action)
  where
    snapshot = withStdinOwnership control $
        (,) <$> getTerminalAttributes stdInput <*> hGetBuffering stdin
    restore (term, buffering) = withStdinOwnership control do
        setTerminalAttributes stdInput term Immediately
        hSetBuffering stdin buffering

-- | Own stdin and restore cooked input for the duration of a prompt. Taking
-- the lease waits for any in-flight watcher read to finish. Brackets release
-- ownership and restore terminal settings on errors or cancellation as well.
withStdinPaused :: StdinControl -> IO a -> IO a
withStdinPaused control action = withStdinOwnership control do
    tty <- hIsTerminalDevice stdin
    if not tty
        then action
        else bracket snapshot restore \(activeTerm, _) -> do
            setTerminalAttributes stdInput (cookedAttrs activeTerm) Immediately
            hSetBuffering stdin LineBuffering
            action
  where
    snapshot = (,) <$> getTerminalAttributes stdInput <*> hGetBuffering stdin
    restore (term, buffering) = do
        setTerminalAttributes stdInput term Immediately
        hSetBuffering stdin buffering

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

escLoop :: CancelFlag -> StdinControl -> IORef Bool -> IO ()
escLoop cancel control stopped = go
  where
    go = do
        continue <- withStdinOwnership control do
            done <- readIORef stopped
            if done then pure False else readKey
        when continue go

    readKey = do
        ready <- hWaitForInput stdin 100
        done <- readIORef stopped
        if done
            then pure False
            else if not ready
                then pure True
                else do
                    c <- hGetChar stdin
                    if c /= '\ESC'
                        then pure True
                        else do
                            more <- hWaitForInput stdin 50
                            cancels <- if not more
                                then pure True
                                else do
                                    c2 <- hGetChar stdin
                                    escapeContinuationCancels
                                        <$> readEscapeContinuation stdin c2
                            when cancels (requestCancel cancel)
                            pure (not cancels)

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
