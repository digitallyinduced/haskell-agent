module Agent.Tools.Types
    ( AppTool(..)
    , AppToolKind(..)
    , ToolEnv(..)
    , defaultToolEnv
    , appToolHandlers
    ) where

import Agent.ToolDSL (PropertySchema)
import Agent.ToolDispatch (ToolHandler)
import Data.Text (Text)
import System.FilePath (dropTrailingPathSeparator)

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
    }

data ToolEnv = ToolEnv
    { toolCwd :: !FilePath
    , toolStdoutCap :: !Int
    } deriving (Eq, Show)

defaultToolEnv :: FilePath -> ToolEnv
defaultToolEnv cwd = ToolEnv
    { toolCwd = dropTrailingPathSeparator cwd
    , toolStdoutCap = 100000
    }

appToolHandlers :: [AppTool] -> [ToolHandler]
appToolHandlers = map (.appToolHandler)
