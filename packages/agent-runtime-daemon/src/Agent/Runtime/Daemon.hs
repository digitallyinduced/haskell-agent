module Agent.Runtime.Daemon
    ( DaemonConfig (..)
    , defaultDaemonConfig
    , runDaemon
    , runTaskDaemon
    , module Agent.Runtime.Daemon.Journal
    , module Agent.Runtime.Daemon.Protocol
    , module Agent.Runtime.Daemon.Server
    , module Agent.Runtime.Daemon.Supervisor
    , module Agent.Runtime.Daemon.Task
    , module Agent.Runtime.Daemon.TaskAdapter
    , module Agent.Runtime.Daemon.TaskScheduler
    ) where

import System.FilePath (takeDirectory, (</>))

import Agent.Runtime.Daemon.Journal
import Agent.Runtime.Daemon.Protocol
import Agent.Runtime.Daemon.Server
import Agent.Runtime.Daemon.Socket
import Agent.Runtime.Daemon.Supervisor
import Agent.Runtime.Daemon.Task
import Agent.Runtime.Daemon.TaskAdapter
import Agent.Runtime.Daemon.TaskScheduler

data DaemonConfig = DaemonConfig
    { socket :: SocketConfig
    , server :: ServerConfig
    , journal :: JournalConfig
    }
    deriving stock (Eq, Show)

defaultDaemonConfig :: IO DaemonConfig
defaultDaemonConfig = do
    socket <- defaultSocketConfig
    let journal = defaultJournalConfig (takeDirectory socket.path </> "journal")
    pure DaemonConfig {socket, server = defaultServerConfig, journal}

runDaemon :: DaemonConfig -> Supervisor -> IO ()
runDaemon config supervisor =
    withUnixListener config.socket $ \listener -> do
        journal <- openJournal config.journal
        runServerOnListener listener config.server journal supervisor

runTaskDaemon :: DaemonConfig -> TaskRunner -> IO ()
runTaskDaemon config runner =
    withUnixListener config.socket $ \listener -> do
        journal <- openJournal config.journal
        withTaskAdapter journal runner $
            runServerOnListener listener config.server journal
