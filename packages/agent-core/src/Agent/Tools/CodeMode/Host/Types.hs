{-# LANGUAGE NoFieldSelectors #-}

module Agent.Tools.CodeMode.Host.Types where

import Control.Concurrent.Async (Async)
import Control.Concurrent.MVar
    ( MVar
    )
import Control.Concurrent.STM
    ( TMVar
    , TQueue
    )
import Data.Aeson (Value)
import qualified Data.Map.Strict as Map
import Data.IORef (IORef)
import Data.Text (Text)
import System.IO (Handle)
import System.Process (ProcessHandle)

type CodeModeToolHandler =
    Text -> Value -> IO (Either Text Value)

data ImageDetailVisibility
    = ImageDetailVisible
    | ImageDetailHidden
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
    -- | Maximum idle Bun processes retained between cells. Set to zero to
    -- retain the legacy one-process-per-cell behavior.
    , workerPoolSize :: !Int
    }

defaultCodeModeConfig :: FilePath -> CodeModeToolHandler -> CodeModeConfig
defaultCodeModeConfig script handler = CodeModeConfig
    { bunExecutable = "bun"
    , workerScript = script
    , startupTimeoutMs = 3000
    , maxActiveCells = 64
    , maxSourceBytes = 1024 * 1024
    , toolHandler = handler
    , notifyHandler = \_ -> pure ()
    , imageDetailVisibility = ImageDetailVisible
    , workerPoolSize = 2
    }

data CodeModeError
    = CodeModeStartupError !Text
    | CodeModeProtocolError !Text
    | CodeModeExecutionError !Text
    | CodeModeResourceError !Text
    | CodeModeUnknownCell !Text
    | CodeModeBusyObserver !Text
    | CodeModeAlreadyTerminating !Text
    | CodeModeClosedCell !Text
    deriving (Eq, Show)

data CodeModeResult
    = CodeModeFinished
        { cellId :: !Text
        , cellValue :: !Value
        }
    | CodeModeFailed
        { cellId :: !Text
        , cellValue :: !Value
        , cellError :: !Text
        }
    | CodeModeRunning
        { cellId :: !Text
        , cellOutput :: !Value
        }
    | CodeModeTerminated
        { cellId :: !Text
        , cellValue :: !Value
        }
    deriving (Eq, Show)

data CellOutcome
    = CellSucceeded !Value
    | CellFailed !Value !Text

data CellObservation
    = CellIdle
    | CellObserved
    | CellTerminating
    | CellClosed

data Cell = Cell
    { cellIdentifier :: !Text
    , cellInput :: !Handle
    , cellOutput :: !Handle
    , cellErrorOutput :: !Handle
    , cellProcess :: !ProcessHandle
    , cellWriterLock :: !(MVar ())
    , cellResult :: !(TMVar (Either CodeModeError CellOutcome))
    , cellYields :: !(TQueue Value)
    , cellContent :: !(TQueue Value)
    , cellMonitor :: !(Async ())
    , cellStderr :: !(Async Text)
    , cellCallbacks :: !(MVar [Async ()])
    , cellObservation :: !(MVar CellObservation)
    }

data IdleWorker = IdleWorker
    !Handle
    !Handle
    !Handle
    !ProcessHandle
    !(MVar ())
    !(Async Text)

data WorkerPool = WorkerPool
    { poolIdle :: ![IdleWorker]
    , poolFiller :: !(Maybe (Async ()))
    , poolClosed :: !Bool
    }

data CodeModeHost = CodeModeHost
    { hostConfig :: !CodeModeConfig
    , hostCells :: !(MVar (Map.Map Text Cell))
    , hostNextId :: !(IORef Int)
    , hostStoredValues :: !(MVar (Map.Map Text Value))
    , hostWorkerPool :: !(MVar WorkerPool)
    }
