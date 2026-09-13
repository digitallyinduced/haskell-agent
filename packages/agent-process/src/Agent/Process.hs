module Agent.Process
    ( ProcessTerminationPolicy(..)
    , defaultProcessTerminationPolicy
    , terminateThenKillPolicy
    , terminateProcessGroup
    , terminateProcessGroupWith
    , terminateProcessGroupWithEscalation
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception.Safe (SomeException, try)
import Control.Monad (unless, void)
import Data.Either (isRight)
import GHC.Clock (getMonotonicTimeNSec)
import System.Posix.Signals
    ( nullSignal
    , Signal
    , sigINT
    , sigKILL
    , sigTERM
    , signalProcessGroup
    )
import System.Posix.Types (ProcessGroupID)
import System.Process
    ( ProcessHandle
    , getProcessExitCode
    , terminateProcess
    )

data ProcessTerminationPolicy = ProcessTerminationPolicy
    { firstSignal :: !Signal
    , firstWaitMilliseconds :: !Int
    , secondSignal :: !(Maybe Signal)
    , secondWaitMilliseconds :: !Int
    }

-- | The normal policy used by shared process owners.
defaultProcessTerminationPolicy :: ProcessTerminationPolicy
defaultProcessTerminationPolicy = ProcessTerminationPolicy
    { firstSignal = sigINT
    , firstWaitMilliseconds = 250
    , secondSignal = Just sigTERM
    , secondWaitMilliseconds = 750
    }

-- | Compatibility policy for callers that historically sent TERM, waited
-- two seconds, and then escalated directly to KILL.
terminateThenKillPolicy :: ProcessTerminationPolicy
terminateThenKillPolicy = ProcessTerminationPolicy
    { firstSignal = sigTERM
    , firstWaitMilliseconds = 2_000
    , secondSignal = Nothing
    , secondWaitMilliseconds = 0
    }

-- | Stop a child process and its descendants with the shared default policy.
terminateProcessGroup
    :: Maybe ProcessGroupID
    -> ProcessHandle
    -> IO ()
terminateProcessGroup = terminateProcessGroupWith defaultProcessTerminationPolicy

-- | Stop a child process and its descendants with an explicit escalation
-- policy.
terminateProcessGroupWith
    :: ProcessTerminationPolicy
    -> Maybe ProcessGroupID
    -> ProcessHandle
    -> IO ()
terminateProcessGroupWith policy =
    terminateProcessGroupWithEscalation policy (pure ())

-- | Stop a child process and its descendants, running an action when the
-- first grace period expires and escalation begins. Exceptions from the
-- notification action are ignored so diagnostics cannot prevent cleanup.
terminateProcessGroupWithEscalation
    :: ProcessTerminationPolicy
    -> IO ()
    -> Maybe ProcessGroupID
    -> ProcessHandle
    -> IO ()
terminateProcessGroupWithEscalation policy onEscalation groupId processHandle = do
    alive <- processGroupAlive groupId processHandle
    whenAlive alive do
        signalGroup (firstSignal policy)
        interrupted <- waitForProcessGroupExit
            groupId
            processHandle
            (firstWaitMilliseconds policy)
        unless interrupted do
            void $ try @_ @SomeException onEscalation
            mapM_ signalGroup (secondSignal policy)
            void $ try @_ @SomeException (terminateProcess processHandle)
            terminated <- waitForProcessGroupExit
                groupId
                processHandle
                (secondWaitMilliseconds policy)
            unless terminated do
                signalGroup sigKILL
                void $ try @_ @SomeException (terminateProcess processHandle)
                void $ waitForProcessGroupExit groupId processHandle 1_000
  where
    whenAlive True action = action
    whenAlive False _ = pure ()

    signalGroup signal =
        case groupId of
            Just pid ->
                void $ try @_ @SomeException (signalProcessGroup signal pid)
            Nothing ->
                void $ try @_ @SomeException (terminateProcess processHandle)

waitForProcessGroupExit
    :: Maybe ProcessGroupID
    -> ProcessHandle
    -> Int
    -> IO Bool
waitForProcessGroupExit groupId processHandle timeoutMs = do
    started <- getMonotonicTimeNSec
    go (toInteger started + toInteger (max 0 timeoutMs) * 1_000_000)
  where
    -- Sleep durations are lower bounds: scheduler delays and Darwin timer
    -- coalescing must not extend each grace period by another polling tick.
    go deadline = do
        alive <- processGroupAlive groupId processHandle
        now <- getMonotonicTimeNSec
        if not alive
            then pure True
            else if toInteger now >= deadline
                then pure False
                else do
                    let remainingUs = (deadline - toInteger now + 999) `div` 1_000
                    threadDelay (fromInteger (min 10_000 remainingUs))
                    go deadline

processGroupAlive
    :: Maybe ProcessGroupID
    -> ProcessHandle
    -> IO Bool
processGroupAlive groupId processHandle = do
    processExit <- getProcessExitCode processHandle
    case groupId of
        Nothing ->
            pure case processExit of
                Nothing -> True
                Just _ -> False
        Just pid ->
            isRight
                <$> try @_ @SomeException
                    (signalProcessGroup nullSignal pid)
