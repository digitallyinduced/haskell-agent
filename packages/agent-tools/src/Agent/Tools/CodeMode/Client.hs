-- | Typed client commands for a 'CodeModeHost'.
--
-- This small layer is intended for tool adapters: they decode model-facing
-- arguments into one of these commands, while process and JSON-RPC details
-- remain private to the host.
module Agent.Tools.CodeMode.Client
    ( CodeModeCommand(..)
    , runCodeModeCommand
    ) where

import Agent.Tools.CodeMode.Host
    ( CodeModeError
    , CodeModeHost
    , CodeModeResult
    , execCodeCell
    , terminateCodeCell
    , waitCodeCell
    )
import Data.Text (Text)

data CodeModeCommand
    = ExecCell
        { source :: !Text
        , availableTools :: ![Text]
        , yieldTimeMs :: !Int
        }
    | WaitCell
        { cellId :: !Text
        , yieldTimeMs :: !Int
        }
    | TerminateCell
        { cellId :: !Text
        }
    deriving (Eq, Show)

runCodeModeCommand
    :: CodeModeHost
    -> CodeModeCommand
    -> IO (Either CodeModeError CodeModeResult)
runCodeModeCommand host = \case
    ExecCell{..} ->
        execCodeCell host source availableTools yieldTimeMs
    WaitCell{..} ->
        waitCodeCell host cellId yieldTimeMs
    TerminateCell{..} ->
        terminateCodeCell host cellId
