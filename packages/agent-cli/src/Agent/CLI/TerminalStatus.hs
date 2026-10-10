-- | Process-scoped OSC 7501 lifecycle reporting. Only fixed protocol tokens
-- are emitted; prompts, tool arguments and other user data never enter OSC.
module Agent.CLI.TerminalStatus
    ( withTerminalStatus
    , withTerminalWorking
    , withTerminalStatusSuspended
    , beginTerminalInputWait
    , endTerminalInputWait
    , terminalStatusSequence
    , TerminalStatus(..)
    ) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
import Control.Exception.Safe (bracket_, catchIO)
import Control.Monad (when)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import System.IO (hFlush, hIsTerminalDevice, stdout)
import System.IO.Unsafe (unsafePerformIO)

data TerminalStatus = TerminalIdle | TerminalWorking | TerminalBlocked | TerminalClear
    deriving (Eq, Show)

terminalStatusSequence :: TerminalStatus -> Text
terminalStatusSequence status = case status of
    TerminalClear -> "\ESC]7501;state=clear\ESC\\"
    _ -> "\ESC]7501;state=" <> name <> ":app=haskell-agent\ESC\\"
  where
    name = case status of
        TerminalIdle -> "idle"
        TerminalWorking -> "working"
        TerminalBlocked -> "blocked"
        TerminalClear -> "clear"

data StatusState = StatusState !Bool !Int !Int !Int

-- The CLI owns one foreground terminal. Counters preserve overlapping scopes:
-- finishing one nested operation must not clear another operation's status.
{-# NOINLINE terminalStatusState #-}
terminalStatusState :: MVar StatusState
terminalStatusState = unsafePerformIO (newMVar (StatusState False 0 0 0))

statusOf :: StatusState -> TerminalStatus
statusOf (StatusState enabled working blocked suspended)
    | not enabled || suspended > 0 = TerminalClear
    | blocked > 0 = TerminalBlocked
    | working > 0 = TerminalWorking
    | otherwise = TerminalIdle

modifyStatus :: (StatusState -> StatusState) -> IO ()
modifyStatus update = modifyMVar_ terminalStatusState \previous -> do
    let next = update previous
    when (statusOf previous /= statusOf next) $
        -- Status is ancillary; a closed terminal must not fail a model turn.
        -- Text IO on an unbuffered terminal can write one character at a time.
        -- Emit one strict byte string so concurrent Vty output cannot split OSC.
        (ByteString.hPut stdout (Text.encodeUtf8 (terminalStatusSequence (statusOf next))) >> hFlush stdout)
            `catchIO` const (pure ())
    pure next

withTerminalStatus :: Bool -> IO a -> IO a
withTerminalStatus interactive action = do
    terminal <- hIsTerminalDevice stdout
    if not interactive || not terminal then action else
        bracket_
            (modifyStatus (const (StatusState True 0 0 0)))
            (modifyStatus (const (StatusState False 0 0 0)))
            action

withTerminalWorking :: IO a -> IO a
withTerminalWorking = bracket_ (change 1) (change (-1))
  where
    change delta = modifyStatus \(StatusState enabled working blocked suspended) ->
        StatusState enabled (max 0 (working + delta)) blocked suspended

beginTerminalInputWait, endTerminalInputWait :: IO ()
beginTerminalInputWait = changeInputWait 1
endTerminalInputWait = changeInputWait (-1)

changeInputWait :: Int -> IO ()
changeInputWait delta = modifyStatus \(StatusState enabled working blocked suspended) ->
    StatusState enabled working (max 0 (blocked + delta)) suspended

withTerminalStatusSuspended :: IO a -> IO a
withTerminalStatusSuspended = bracket_ (change 1) (change (-1))
  where
    change delta = modifyStatus \(StatusState enabled working blocked suspended) ->
        StatusState enabled working blocked (max 0 (suspended + delta))
