module Agent.Tools.Types
    ( AppTool(..)
    , ToolSchema(..)
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , ToolRegistry
    , ToolEnv(..)
    , addToolAllowedRoot
    , defaultToolEnv
    , setToolRootAccessRequest
    , setToolSkillRoots
    , setToolSessionTmp
    , jsonTool
    , jsonAppTool
    , jsonAppToolWithExecution
    , rawJsonAppTool
    , rawJsonAppToolWithExecution
    , freeformApplyPatchAppTool
    , freeformApplyPatchAppToolWithExecution
    , freeformGrammarAppToolWithExecution
    , withToolResourceClaims
    , mkToolRegistry
    , toolRegistryTools
    , lookupRegisteredTool
    , toolAcceptsCall
    , toolExecutionPolicyFor
    , toolSchedulingPlanFor
    , dispatchRegisteredToolCall
    , dispatchRegisteredToolCallDetailed
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
    , ToolDispatchOutcome
    , ToolDispatchConfig
    , ToolHandler
    , canonicalToolName
    , dispatchToolHandler
    , dispatchToolHandlerDetailed
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
import Data.IORef (IORef, atomicModifyIORef', newIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import System.OsPath
    ( OsPath
    , dropTrailingPathSeparator
    , equalFilePath
    )

-- | Provider-facing schema. The sum prevents freeform tools from carrying
-- meaningless JSON parameters.
data ToolSchema
    = JsonFunctionSchema ![PropertySchema]
    | RawJsonFunctionSchema !Value
    | FreeformApplyPatchSchema
    -- | Freeform custom tool with an explicit provider grammar
    -- (@format.type = "grammar"@). The fields are the grammar syntax
    -- (for example @"lark"@) and its definition text.
    | FreeformGrammarSchema !Text !Text
    -- | Provider-hosted desktop control. Keeping this distinct prevents an
    -- unrelated function or MCP tool named @computer@ from acquiring it.
    | HostedComputerSchema
    deriving (Eq, Show)

-- | Whether a call may run without generic user approval.
data ApprovalRule
    = AlwaysReadOnly
    -- | Host-authorized effect that is intentionally exempt from the generic
    -- mutation prompt (for example a paid provider capability).
    | AlwaysAllowed
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
      -- 'toolSessionTmp' is always allowed implicitly and receives absolute
      -- @/tmp@ and @/private/tmp@ filesystem-tool paths by default.
    , toolRootAccessRequest :: !(IORef (Maybe (OsPath -> IO Bool)))
      -- | Optional session-local callback used when a path falls outside
      -- the configured roots. An approved path is added to
      -- 'toolAllowedRoots' by the filesystem resolver.
    , toolSkillRoots :: !(IORef [OsPath])
      -- | Directories belonging to the currently discovered skill catalog.
      -- Kept separate so catalog refreshes can replace them without
      -- disturbing other explicitly allowed roots.
    , toolSessionTmp :: !(IORef (Maybe OsPath))
    , toolOutputInlineCap :: !Int
    , toolOutputPreviewCap :: !Int
    , toolOutputArtifactCap :: !Int
    , toolStdoutCap :: !Int
      -- | Soft-cancel latch for the active turn. Shell tools race against it.
    , toolCancel :: !CancelFlag
    }

defaultToolEnv :: OsPath -> IO ToolEnv
defaultToolEnv cwd = do
    cancel <- newCancelFlag
    allowedRoots <- newIORef []
    rootAccessRequest <- newIORef Nothing
    skillRoots <- newIORef []
    sessionTmp <- newIORef Nothing
    pure ToolEnv
        { toolCwd = dropTrailingPathSeparator cwd
        , toolAllowedRoots = allowedRoots
        , toolRootAccessRequest = rootAccessRequest
        , toolSkillRoots = skillRoots
        , toolSessionTmp = sessionTmp
        , toolOutputInlineCap = 50 * 1024
        , toolOutputPreviewCap = 8 * 1024
        , toolOutputArtifactCap = 64 * 1024 * 1024
        , toolStdoutCap = 16 * 1024
        , toolCancel = cancel
        }

-- | Install the session-local callback used to request access to an
-- additional filesystem root. The callback should perform any human-facing
-- approval and return whether the requested root may be added.
setToolRootAccessRequest :: ToolEnv -> Maybe (OsPath -> IO Bool) -> IO ()
setToolRootAccessRequest env = writeIORef env.toolRootAccessRequest

-- | Add a canonical directory to the roots available for this session.
-- Duplicate roots are ignored so repeated approvals remain idempotent.
addToolAllowedRoot :: ToolEnv -> OsPath -> IO ()
addToolAllowedRoot env root =
    atomicModifyIORef' env.toolAllowedRoots \roots ->
        (if any (equalFilePath root) roots then roots else roots <> [root], ())

-- | Replace the directories exposed for the current skill catalog.
setToolSkillRoots :: ToolEnv -> [OsPath] -> IO ()
setToolSkillRoots env = writeIORef env.toolSkillRoots

-- | Change the private scratch directory used by subsequent filesystem and
-- shell calls. The resolver treats this value as an allowed root and the
-- target of the system-temp aliases directly, so changing it cannot get out of
-- sync with a separately maintained roots list.
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

-- | Construct a freeform tool that advertises an explicit grammar.
freeformGrammarAppToolWithExecution
    :: Text
    -> Text
    -> Text
    -> Text
    -> ApprovalRule
    -> ToolExecutionPolicy
    -> ToolHandler
    -> AppTool
freeformGrammarAppToolWithExecution
        name description syntax definition approval execution handler = AppTool
    { appToolName = name
    , appToolDescription = description
    , appToolSchema = FreeformGrammarSchema syntax definition
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
        (acceptedHandler registry call)
        call

dispatchRegisteredToolCallDetailed
    :: ToolDispatchConfig
    -> ToolRegistry
    -> ToolCall
    -> IO ToolDispatchOutcome
dispatchRegisteredToolCallDetailed config registry call =
    dispatchToolHandlerDetailed config
        (acceptedHandler registry call)
        call

acceptedHandler :: ToolRegistry -> ToolCall -> Maybe ToolHandler
acceptedHandler registry call = do
    tool <- lookupRegisteredTool call.name registry
    if toolAcceptsCall tool call
        then Just tool.appToolHandler
        else Nothing

-- | Provider-native calls may only reach their matching hosted handler.
toolAcceptsCall :: AppTool -> ToolCall -> Bool
toolAcceptsCall tool call =
    case (tool.appToolSchema, call.callKind) of
        (HostedComputerSchema, ComputerCallKind) -> True
        (HostedComputerSchema, ComputerFunctionCallKind) -> True
        (HostedComputerSchema, _) -> False
        (_, ComputerCallKind) -> False
        (_, ComputerFunctionCallKind) -> False
        _ -> True

jsonToolParameters :: AppTool -> Maybe [PropertySchema]
jsonToolParameters tool = case tool.appToolSchema of
    JsonFunctionSchema parameters -> Just parameters
    RawJsonFunctionSchema _ -> Nothing
    FreeformApplyPatchSchema -> Nothing
    FreeformGrammarSchema _ _ -> Nothing
    HostedComputerSchema -> Nothing

-- | Compatibility helper for direct handler consumers. New dispatch paths
-- should retain and use 'ToolRegistry' instead.
appToolHandlers :: [AppTool] -> [ToolHandler]
appToolHandlers = map (.appToolHandler)

toolAllowsWithoutPrompt :: AppTool -> ToolCall -> IO Bool
toolAllowsWithoutPrompt tool call = case tool.appToolApproval of
    AlwaysReadOnly -> pure True
    AlwaysAllowed -> pure True
    AlwaysPrompt -> pure False
    ClassifyReadOnly classify -> classify call
