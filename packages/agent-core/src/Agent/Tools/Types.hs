module Agent.Tools.Types
    ( AppTool(..)
    , AppToolKind(..)
    , ToolEnv(..)
    , defaultToolEnv
    , appToolHandlers
    , toolAllowsWithoutPrompt
    ) where

import Agent.Cancel (CancelFlag, newCancelFlag)
import Agent.OsPath (OsPath)
import Agent.ToolDSL (PropertySchema)
import Agent.ToolDispatch (ToolCall, ToolHandler)
import Data.Text (Text)
import System.OsPath (dropTrailingPathSeparator)

data AppToolKind
    = JsonFunction
    | FreeformApplyPatch
    deriving (Eq, Show)

data AppTool = AppTool
    { appToolName :: !Text
    , appToolDescription :: !Text
    , appToolParameters :: ![PropertySchema]
    , appToolHandler :: !ToolHandler
    , appToolKind :: !AppToolKind
    , appToolReadOnly :: !Bool
    -- | Optional per-call override. When present, it decides whether this
    -- specific invocation is treated as read-only for approval.
    , appToolIsReadOnlyCall :: !(Maybe (ToolCall -> IO Bool))
    }

data ToolEnv = ToolEnv
    { toolCwd :: !OsPath
    , toolStdoutCap :: !Int
      -- | Soft-cancel latch for the active turn. Shell tools race against it.
    , toolCancel :: !CancelFlag
    }

defaultToolEnv :: OsPath -> IO ToolEnv
defaultToolEnv cwd = do
    cancel <- newCancelFlag
    pure ToolEnv
        { toolCwd = dropTrailingPathSeparator cwd
        , toolStdoutCap = 100000
        , toolCancel = cancel
        }

appToolHandlers :: [AppTool] -> [ToolHandler]
appToolHandlers = map (.appToolHandler)

-- | Static read-only flag, or a dynamic per-call classifier when registered.
toolAllowsWithoutPrompt :: AppTool -> ToolCall -> IO Bool
toolAllowsWithoutPrompt tool call = case tool.appToolIsReadOnlyCall of
    Just classify -> classify call
    Nothing -> pure tool.appToolReadOnly
