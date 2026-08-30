module Agent.Tools.Types
    ( AppTool(..)
    , ToolSchema(..)
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , ToolRegistry
    , ToolEnv(..)
    , defaultToolEnv
    , setToolSkillRoots
    , setToolSessionTmp
    , jsonTool
    , jsonAppTool
    , jsonAppToolWithExecution
    , rawJsonAppTool
    , rawJsonAppToolWithExecution
    , freeformApplyPatchAppTool
    , freeformApplyPatchAppToolWithExecution
    , withToolResourceClaims
    , mkToolRegistry
    , toolRegistryTools
    , lookupRegisteredTool
    , toolAcceptsCall
    , toolExecutionPolicyFor
    , toolSchedulingPlanFor
    , dispatchRegisteredToolCall
    , jsonToolParameters
    , appToolHandlers
    , toolAllowsWithoutPrompt
    ) where

import Agent.Cancel (CancelFlag, newCancelFlag)
import Agent.ToolDSL (PropertySchema)
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , ToolCallResult
    , ToolDispatchConfig
    , ToolHandler
    , canonicalToolName
    , dispatchToolHandler
    , handlerName
    )
import Agent.Tools.Scheduling
    ( ToolAccess(..)
    , ToolResource(..)
    , ToolResourceClaim(..)
    , ToolSchedulingPlan(..)
    )
import Control.Exception.Safe (tryAny)
import Control.Monad (foldM)
import Data.Aeson (Value)
import Data.IORef (IORef, newIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import System.OsPath (OsPath, dropTrailingPathSeparator)

-- | Provider-facing schema. The sum prevents freeform tools from carrying
-- meaningless JSON parameters.
data ToolSchema
    = JsonFunctionSchema ![PropertySchema]
    | RawJsonFunctionSchema !Value
    | FreeformApplyPatchSchema
    -- | The provider-native computer tool. Keeping this distinct prevents an
    -- unrelated function or MCP tool named @computer@ from being projected as
    -- a hosted desktop-control surface.
    | HostedComputerSchema
    deriving (Eq, Show)

-- | Whether a call may run without generic user approval.
data ApprovalRule
    = AlwaysReadOnly
    | AlwaysPrompt
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

type ToolResourceResolver =
    ToolCall -> IO (Either Text [ToolResourceClaim])

data AppTool = AppTool
    { appToolName :: !Text
    , appToolDescription :: !Text
    , appToolSchema :: !ToolSchema
    , appToolHandler :: !ToolHandler
    , appToolApproval :: !ApprovalRule
    , appToolExecution :: !ToolExecutionPolicy
    , appToolResourceClaims :: !(Maybe ToolResourceResolver)
    }

-- | Registration order is retained for stable provider schemas while lookup is
-- canonical and validated once at construction.
data ToolRegistry = ToolRegistry
    { registryTools :: ![AppTool]
    , registryByName :: !(Map.Map Text AppTool)
    }

data ToolEnv = ToolEnv
    { toolCwd :: !OsPath
    , toolAllowedRoots :: !(IORef [OsPath])
      -- | Additional non-session filesystem roots. The current
      -- 'toolSessionTmp' is always allowed implicitly.
    , toolSkillRoots :: !(IORef [OsPath])
      -- | Directories belonging to the currently discovered skill catalog.
      -- Kept separate so catalog refreshes can replace them without
      -- disturbing other explicitly allowed roots.
    , toolSessionTmp :: !(IORef (Maybe OsPath))
    , toolStdoutCap :: !Int
      -- | Soft-cancel latch for the active turn. Shell tools race against it.
    , toolCancel :: !CancelFlag
    }

defaultToolEnv :: OsPath -> IO ToolEnv
defaultToolEnv cwd = do
    cancel <- newCancelFlag
    allowedRoots <- newIORef []
    skillRoots <- newIORef []
    sessionTmp <- newIORef Nothing
    pure ToolEnv
        { toolCwd = dropTrailingPathSeparator cwd
        , toolAllowedRoots = allowedRoots
        , toolSkillRoots = skillRoots
        , toolSessionTmp = sessionTmp
        , toolStdoutCap = 100000
        , toolCancel = cancel
        }

-- | Replace the directories exposed for the current skill catalog.
setToolSkillRoots :: ToolEnv -> [OsPath] -> IO ()
setToolSkillRoots env = writeIORef env.toolSkillRoots

-- | Change the private scratch directory used by subsequent filesystem and
-- shell calls. The resolver treats this value as an allowed root directly, so
-- changing it cannot get out of sync with a separately maintained roots list.
setToolSessionTmp :: ToolEnv -> Maybe OsPath -> IO ()
setToolSessionTmp env = writeIORef env.toolSessionTmp

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
    , appToolResourceClaims = Nothing
    }

-- | Construct a JSON tool from an already-built JSON Schema value. Dynamic
-- tool providers such as MCP use this path so their schemas remain lossless.
rawJsonAppTool
    :: Text
    -> Text
    -> Value
    -> ApprovalRule
    -> ToolHandler
    -> AppTool
rawJsonAppTool name description parameters approval =
    rawJsonAppToolWithExecution
        name description parameters approval TurnSequential

rawJsonAppToolWithExecution
    :: Text
    -> Text
    -> Value
    -> ApprovalRule
    -> ToolExecutionPolicy
    -> ToolHandler
    -> AppTool
rawJsonAppToolWithExecution
        name description parameters approval execution handler = AppTool
    { appToolName = name
    , appToolDescription = description
    , appToolSchema = RawJsonFunctionSchema parameters
    , appToolHandler = handler
    , appToolApproval = approval
    , appToolExecution = execution
    , appToolResourceClaims = Nothing
    }

withToolResourceClaims
    :: ToolResourceResolver
    -> AppTool
    -> AppTool
withToolResourceClaims resolver tool =
    tool { appToolResourceClaims = Just resolver }

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
    , appToolResourceClaims = Nothing
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

toolSchedulingPlanFor
    :: ToolRegistry
    -> ToolCall
    -> IO ToolSchedulingPlan
toolSchedulingPlanFor registry call =
    case lookupRegisteredTool call.name registry of
        Nothing -> pure ToolExclusive
        Just tool -> case tool.appToolResourceClaims of
            Just resolve -> do
                resolved <- tryAny (resolve call)
                pure $ case resolved of
                    Left _ -> ToolExclusive
                    Right (Left _) -> ToolExclusive
                    Right (Right claims) -> ToolResourceClaims claims
            Nothing -> pure $ case tool.appToolExecution of
                ParallelSafe ->
                    ToolResourceClaims
                        [ToolResourceClaim ToolRead ToolAllPaths]
                TurnSequential -> ToolExclusive

dispatchRegisteredToolCall
    :: ToolDispatchConfig
    -> ToolRegistry
    -> ToolCall
    -> IO ToolCallResult
dispatchRegisteredToolCall config registry call =
    dispatchToolHandler config
        (lookupRegisteredTool call.name registry >>= \tool ->
            if toolAcceptsCall tool call
                then Just tool.appToolHandler
                else Nothing)
        call

-- | Provider-native call kinds may only reach their matching hosted handler.
-- This prevents a function/custom tool that happens to share the hosted name
-- from invoking desktop control, and prevents native calls from reaching an
-- ordinary function handler.
toolAcceptsCall :: AppTool -> ToolCall -> Bool
toolAcceptsCall tool call =
    case (tool.appToolSchema, call.callKind) of
        (HostedComputerSchema, ComputerCallKind) -> True
        (HostedComputerSchema, _) -> False
        (_, ComputerCallKind) -> False
        _ -> True

jsonToolParameters :: AppTool -> Maybe [PropertySchema]
jsonToolParameters tool = case tool.appToolSchema of
    JsonFunctionSchema parameters -> Just parameters
    RawJsonFunctionSchema _ -> Nothing
    FreeformApplyPatchSchema -> Nothing
    HostedComputerSchema -> Nothing

-- | Compatibility helper for direct handler consumers. New dispatch paths
-- should retain and use 'ToolRegistry' instead.
appToolHandlers :: [AppTool] -> [ToolHandler]
appToolHandlers = map (.appToolHandler)

toolAllowsWithoutPrompt :: AppTool -> ToolCall -> IO Bool
toolAllowsWithoutPrompt tool call = case tool.appToolApproval of
    AlwaysReadOnly -> pure True
    AlwaysPrompt -> pure False
    ClassifyReadOnly classify -> classify call
