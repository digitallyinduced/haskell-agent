module Agent.Tools.CodeMode.IsolatedHost.Types where

import Prelude
import Control.Concurrent.Async (Async)
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM (TMVar, TQueue)
import Data.Aeson (Value)
import Data.IORef (IORef)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import System.IO (Handle)
import System.Posix.Types (ProcessGroupID)
import System.Process (CreateProcess, ProcessHandle)

type CodeModeToolHandler = Text -> Value -> IO (Either Text Value)

data ImageDetailVisibility = ImageDetailVisible | ImageDetailHidden
    deriving (Eq, Show)

data CodeModeConfig = CodeModeConfig
    { bunExecutable :: !FilePath
    , workerScript :: !FilePath
    , startupTimeoutMs :: !Int
    , maxActiveCells :: !Int
    , maxSourceBytes :: !Int
    , toolHandler :: !CodeModeToolHandler
    , notifyHandler :: !(Text -> IO ())
    , imageDetailVisibility :: !ImageDetailVisibility
    , workerPoolSize :: !Int
    -- | Mandatory containment configuration for resolved runtime/script paths.
    -- Returning Left fails startup; the host never retries without containment.
    , prepareWorkerProcess :: !(FilePath -> FilePath -> IO (Either Text CreateProcess))
    }

defaultCodeModeConfig
    :: (FilePath -> FilePath -> IO (Either Text CreateProcess))
    -> FilePath -> CodeModeToolHandler -> CodeModeConfig
defaultCodeModeConfig prepare script handler = CodeModeConfig
    { bunExecutable = "bun"
    , workerScript = script
    , startupTimeoutMs = 3000
    , maxActiveCells = 1
    , maxSourceBytes = 1024 * 1024
    , toolHandler = handler
    , notifyHandler = \_ -> pure ()
    , imageDetailVisibility = ImageDetailVisible
    , workerPoolSize = 1
    , prepareWorkerProcess = prepare
    }

data CodeModeError
    = CodeModeStartupError !Text
    | CodeModeProtocolError !Text
    | CodeModeExecutionError !Text
    | CodeModeResourceError !Text
    | CodeModeUnknownCell !Text
    | CodeModeBusyObserver !Text
    | CodeModeClosedCell !Text
    deriving (Eq, Show)

data CodeModeResult
    = CodeModeFinished { cellId :: !Text, cellValue :: !Value }
    | CodeModeFailed { cellId :: !Text, cellValue :: !Value, cellError :: !Text }
    | CodeModeRunning { cellId :: !Text, cellOutput :: !Value }
    | CodeModeTerminated { cellId :: !Text, cellValue :: !Value }
    deriving (Eq, Show)

data CellOutcome = CellSucceeded !Value | CellFailed !Value !Text

data Worker = Worker
    { workerInput :: !Handle
    , workerOutput :: !Handle
    , workerError :: !Handle
    , workerProcess :: !ProcessHandle
    , workerGroup :: !(Maybe ProcessGroupID)
    , workerWriter :: !(MVar ())
    , workerErrorReader :: !(Async ())
    }

data Cell = Cell
    { cellIdentifier :: !Text
    , cellStartedAt :: !UTCTime
    , cellWorker :: !Worker
    , cellResult :: !(TMVar (Either CodeModeError CellOutcome))
    , cellYields :: !(TQueue Value)
    , cellContent :: !(TQueue Value)
    , cellMonitor :: !(Async ())
    , cellCallbacks :: !(MVar [Async ()])
    , cellObservation :: !(MVar Bool)
    }

data HostState = HostState
    { hostClosed :: !Bool
    , hostCells :: !(Map Text Cell)
    , hostIdle :: ![Worker]
    }

data CodeModeHost = CodeModeHost
    { hostConfig :: !CodeModeConfig
    , hostState :: !(MVar HostState)
    , hostNextId :: !(IORef Int)
    , hostStoredValues :: !(MVar (Map Text Value))
    }
