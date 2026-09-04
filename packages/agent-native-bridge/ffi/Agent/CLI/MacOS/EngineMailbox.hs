module Agent.CLI.MacOS.EngineMailbox
    ( EngineMailbox
    , acceptEngineCommand
    , closeEngineMailbox
    , drainEngineCommands
    , newEngineMailboxIO
    , readEngineCommand
    ) where

import Control.Concurrent.STM
    ( STM
    , TQueue
    , TVar
    , flushTQueue
    , newTQueueIO
    , newTVarIO
    , readTQueue
    , readTVar
    , writeTQueue
    , writeTVar
    )

-- | A queue with an atomic acceptance boundary. Closing and enqueueing share
-- one STM linearization point, so a command reported as accepted is always
-- ordered before the closing command.
data EngineMailbox command = EngineMailbox
    { mailboxCommands :: !(TQueue command)
    , mailboxAccepting :: !(TVar Bool)
    }

newEngineMailboxIO :: IO (EngineMailbox command)
newEngineMailboxIO =
    EngineMailbox <$> newTQueueIO <*> newTVarIO True

acceptEngineCommand :: EngineMailbox command -> command -> STM Bool
acceptEngineCommand mailbox command = do
    accepting <- readTVar mailbox.mailboxAccepting
    if accepting
        then writeTQueue mailbox.mailboxCommands command >> pure True
        else pure False

-- | Stop accepting commands and enqueue the closing command. Returns 'True'
-- only for the transaction that closes the mailbox.
closeEngineMailbox :: EngineMailbox command -> command -> STM Bool
closeEngineMailbox mailbox command = do
    accepting <- readTVar mailbox.mailboxAccepting
    if accepting
        then do
            writeTVar mailbox.mailboxAccepting False
            writeTQueue mailbox.mailboxCommands command
            pure True
        else pure False

readEngineCommand :: EngineMailbox command -> STM command
readEngineCommand mailbox = readTQueue mailbox.mailboxCommands

drainEngineCommands :: EngineMailbox command -> STM [command]
drainEngineCommands mailbox = flushTQueue mailbox.mailboxCommands
