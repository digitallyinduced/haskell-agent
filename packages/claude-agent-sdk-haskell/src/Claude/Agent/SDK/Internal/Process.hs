module Claude.Agent.SDK.Internal.Process
    ( terminateProcessGroup
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception.Safe (SomeException, try)
import Control.Monad (unless, void)
import Data.Either (isRight)
import System.Posix.Signals
    ( nullSignal
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

-- | Stop a child process and its descendants with bounded escalation.
terminateProcessGroup
    :: Maybe ProcessGroupID
    -> ProcessHandle
    -> IO ()
terminateProcessGroup groupId processHandle = do
    alive <- processGroupAlive groupId processHandle
    whenAlive alive do
        signalGroup sigINT
        interrupted <- waitForProcessGroupExit groupId processHandle 250
        unless interrupted do
            signalGroup sigTERM
            void $ try @_ @SomeException (terminateProcess processHandle)
            terminated <- waitForProcessGroupExit
                groupId
                processHandle
                750
            unless terminated do
                signalGroup sigKILL
                void $ try @_ @SomeException (terminateProcess processHandle)
                void $
                    waitForProcessGroupExit
                        groupId
                        processHandle
                        1_000
  where
    whenAlive True action = action
    whenAlive False _ = pure ()

    signalGroup signal =
        case groupId of
            Just pid ->
                void $
                    try @_ @SomeException
                        (signalProcessGroup signal pid)
            Nothing ->
                void $
                    try @_ @SomeException
                        (terminateProcess processHandle)

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
