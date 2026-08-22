module Agent.Tools.Types
    ( AppTool(..)
    , ToolSchema(..)
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , ToolRegistry
    , ToolEnv(..)
    , defaultToolEnv
    , jsonTool
    , jsonAppTool
    , jsonAppToolWithExecution
    , freeformApplyPatchAppTool
    , freeformApplyPatchAppToolWithExecution
    , mkToolRegistry
    , toolRegistryTools
    , lookupRegisteredTool
    , toolExecutionPolicyFor
    , dispatchRegisteredToolCall
    , jsonToolParameters
    , appToolHandlers
    , toolAllowsWithoutPrompt
    , toolRequiresPerCallApproval
    ) where

import Agent.Cancel (CancelFlag, newCancelFlag)
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
import System.OsPath (OsPath, dropTrailingPathSeparator)

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
    | PromptEveryCall
    | ClassifyReadOnly !(ToolCall -> IO Bool)

-- | Whether a tool handler may overlap other handlers emitted in the same
-- model turn. Approval callbacks are always evaluated serially in call order.
--
-- This policy is intentionally turn-local: it does not coordinate separate
-- loops or background work that outlives a handler.
data ToolExecutionPolicy
    = ParallelSafe
    | TurnSequential
    deriving (Eq, Show)

data AppTool = AppTool
    { appToolName :: !Text
    , appToolDescription :: !Text
    , appToolSchema :: !ToolSchema
    , appToolHandler :: !ToolHandler
    , appToolApproval :: !ApprovalRule
    , appToolExecution :: !ToolExecutionPolicy
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

-- | Construct a JSON tool whose approval is selected from a simple read-only
-- flag. This is the common convenience shape used by provider tool surfaces.
jsonTool
    :: Text
    -> Text
    -> [PropertySchema]
    -> Bool
    -> ToolExecutionPolicy
    -> ToolHandler
    -> AppTool
jsonTool name description parameters readOnly execution =
    jsonAppToolWithExecution name description parameters
        (if readOnly then AlwaysReadOnly else AlwaysPrompt)
        execution

-- | Construct a JSON tool with the conservative turn-sequential default.
jsonAppTool
    :: Text
    -> Text
    -> [PropertySchema]
    -> ApprovalRule
    -> ToolHandler
    -> AppTool
jsonAppTool name description parameters approval =
    jsonAppToolWithExecution
        name description parameters approval TurnSequential

jsonAppToolWithExecution
    :: Text
    -> Text
    -> [PropertySchema]
    -> ApprovalRule
    -> ToolExecutionPolicy
    -> ToolHandler
    -> AppTool
jsonAppToolWithExecution
        name description parameters approval execution handler = AppTool
    { appToolName = name
    , appToolDescription = description
    , appToolSchema = JsonFunctionSchema parameters
    , appToolHandler = handler
    , appToolApproval = approval
    , appToolExecution = execution
    }

-- | Construct a freeform tool with the conservative turn-sequential default.
freeformApplyPatchAppTool
    :: Text
    -> Text
    -> ApprovalRule
    -> ToolHandler
    -> AppTool
freeformApplyPatchAppTool name description approval =
    freeformApplyPatchAppToolWithExecution
        name description approval TurnSequential

freeformApplyPatchAppToolWithExecution
    :: Text
    -> Text
    -> ApprovalRule
    -> ToolExecutionPolicy
    -> ToolHandler
    -> AppTool
freeformApplyPatchAppToolWithExecution
        name description approval execution handler = AppTool
    { appToolName = name
    , appToolDescription = description
    , appToolSchema = FreeformApplyPatchSchema
    , appToolHandler = handler
    , appToolApproval = approval
    , appToolExecution = execution
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

-- | Unknown tools are conservative barriers. Their dispatch will still
-- produce the normal unknown-tool result, but never overlap known work.
toolExecutionPolicyFor :: ToolRegistry -> ToolCall -> ToolExecutionPolicy
toolExecutionPolicyFor registry call =
    maybe TurnSequential (\tool -> tool.appToolExecution)
        (lookupRegisteredTool call.name registry)

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
    PromptEveryCall -> pure False
    ClassifyReadOnly classify -> classify call

-- | Whether a session-level “always allow this tool” choice must be ignored.
--
-- This is intended for unsandboxed interpreters where approving one source
-- program must not silently approve unrelated source submitted later.
toolRequiresPerCallApproval :: AppTool -> Bool
toolRequiresPerCallApproval tool = case tool.appToolApproval of
    PromptEveryCall -> True
    _ -> False
