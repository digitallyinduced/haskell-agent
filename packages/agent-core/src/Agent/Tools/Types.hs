module Agent.Tools.Types
    ( AppTool(..)
    , ToolSchema(..)
    , ApprovalRule(..)
    , ToolRegistry
    , ToolEnv(..)
    , defaultToolEnv
    , jsonAppTool
    , freeformApplyPatchAppTool
    , mkToolRegistry
    , toolRegistryTools
    , lookupRegisteredTool
    , dispatchRegisteredToolCall
    , jsonToolParameters
    , appToolHandlers
    , toolAllowsWithoutPrompt
    ) where

import Agent.Cancel (CancelFlag, newCancelFlag)
import System.OsPath (OsPath)
import Agent.ToolDSL (PropertySchema)
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallResult
    , ToolDispatchConfig
    , ToolHandler
    , canonicalToolName
    , dispatchToolHandler
    , handlerName
    )
import Control.Monad (foldM)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import System.OsPath (dropTrailingPathSeparator)

-- | Provider-facing schema. The sum prevents freeform tools from carrying
-- meaningless JSON parameters.
data ToolSchema
    = JsonFunctionSchema ![PropertySchema]
    | FreeformApplyPatchSchema
    deriving (Eq, Show)

-- | Whether a call may run without generic user approval.
data ApprovalRule
    = AlwaysReadOnly
    | AlwaysPrompt
    | ClassifyReadOnly !(ToolCall -> IO Bool)

data AppTool = AppTool
    { appToolName :: !Text
    , appToolDescription :: !Text
    , appToolSchema :: !ToolSchema
    , appToolHandler :: !ToolHandler
    , appToolApproval :: !ApprovalRule
    }

-- | Registration order is retained for stable provider schemas while lookup is
-- canonical and validated once at construction.
data ToolRegistry = ToolRegistry
    { registryTools :: ![AppTool]
    , registryByName :: !(Map.Map Text AppTool)
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

jsonAppTool
    :: Text
    -> Text
    -> [PropertySchema]
    -> ApprovalRule
    -> ToolHandler
    -> AppTool
jsonAppTool name description parameters approval handler = AppTool
    { appToolName = name
    , appToolDescription = description
    , appToolSchema = JsonFunctionSchema parameters
    , appToolHandler = handler
    , appToolApproval = approval
    }

freeformApplyPatchAppTool
    :: Text
    -> Text
    -> ApprovalRule
    -> ToolHandler
    -> AppTool
freeformApplyPatchAppTool name description approval handler = AppTool
    { appToolName = name
    , appToolDescription = description
    , appToolSchema = FreeformApplyPatchSchema
    , appToolHandler = handler
    , appToolApproval = approval
    }

mkToolRegistry :: [AppTool] -> Either Text ToolRegistry
mkToolRegistry tools = do
    byName <- foldM insertTool Map.empty tools
    pure ToolRegistry
        { registryTools = tools
        , registryByName = byName
        }
  where
    insertTool :: Map.Map Text AppTool -> AppTool -> Either Text (Map.Map Text AppTool)
    insertTool current tool
        | Text.null (Text.strip tool.appToolName) =
            Left "tool name must not be empty"
        | handlerName tool.appToolHandler /= tool.appToolName =
            Left $
                "tool handler name "
                    <> handlerName tool.appToolHandler
                    <> " does not match registered name "
                    <> tool.appToolName
        | Map.member key current =
            Left ("duplicate canonical tool name: " <> key)
        | otherwise = Right (Map.insert key tool current)
      where
        key = canonicalToolName tool.appToolName

toolRegistryTools :: ToolRegistry -> [AppTool]
toolRegistryTools = (.registryTools)

lookupRegisteredTool :: Text -> ToolRegistry -> Maybe AppTool
lookupRegisteredTool name registry =
    Map.lookup (canonicalToolName name) registry.registryByName

dispatchRegisteredToolCall
    :: ToolDispatchConfig
    -> ToolRegistry
    -> ToolCall
    -> IO ToolCallResult
dispatchRegisteredToolCall config registry call =
    dispatchToolHandler config
        ((.appToolHandler) <$> lookupRegisteredTool call.name registry)
        call

jsonToolParameters :: AppTool -> Maybe [PropertySchema]
jsonToolParameters tool = case tool.appToolSchema of
    JsonFunctionSchema parameters -> Just parameters
    FreeformApplyPatchSchema -> Nothing

-- | Compatibility helper for direct handler consumers. New dispatch paths
-- should retain and use 'ToolRegistry' instead.
appToolHandlers :: [AppTool] -> [ToolHandler]
appToolHandlers = map (.appToolHandler)

toolAllowsWithoutPrompt :: AppTool -> ToolCall -> IO Bool
toolAllowsWithoutPrompt tool call = case tool.appToolApproval of
    AlwaysReadOnly -> pure True
    AlwaysPrompt -> pure False
    ClassifyReadOnly classify -> classify call
