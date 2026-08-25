module Agent.Process
    ( ProcessTerminationPolicy(..)
    , defaultProcessTerminationPolicy
    , terminateThenKillPolicy
    , terminateProcessGroup
    , terminateProcessGroupWith
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception.Safe (SomeException, try)
import Control.Monad (unless, void)
import Data.Either (isRight)
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
terminateProcessGroupWith policy groupId processHandle = do
    alive <- processGroupAlive groupId processHandle
    whenAlive alive do
        signalGroup (firstSignal policy)
        interrupted <- waitForProcessGroupExit
            groupId
            processHandle
            (firstWaitMilliseconds policy)
        unless interrupted do
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
waitForProcessGroupExit groupId processHandle timeoutMs =
    go (max 0 timeoutMs)
  where
    go remaining = do
        alive <- processGroupAlive groupId processHandle
        if not alive
            then pure True
            else if remaining <= 0
                then pure False
                else do
                    let delayMs = min 10 remaining
                    threadDelay (delayMs * 1_000)
                    go (remaining - delayMs)

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
