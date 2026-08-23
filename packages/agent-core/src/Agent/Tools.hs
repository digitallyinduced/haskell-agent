-- | Registered application tools: schema fragments plus a dispatch handler.
--
-- Provider-specific surfaces live in 'Agent.Tools.Grok' and
-- 'Agent.Tools.Codex'. OpenRouter reuses the Grok JSON function tools.
-- Shared filesystem, process, and GHCi helpers are in 'Agent.Tools.IO' and
-- 'Agent.Tools.Ghci'.
module Agent.Tools
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
    , appToolHandlers
    , CodingTools(..)
    , codingToolsFor
    , codingToolsForWithHaskellProgram
    , codingToolsForWithTypes
    , codingToolsForWithTypesAndHaskellProgram
    , filterChildGrokTools
    ) where

import Agent.Provider (Provider(..))
import Agent.ResourceScope
    ( allocateResource
    , closeResourceScope
    , newResourceScope
    )
import Agent.Tools.Codex (codexTools)
import Agent.Tools.Codex.Shell
    ( closeCodexShellSession
    , newCodexShellSession
    )
import Agent.Tools.Ghci
    ( closeGhciSession
    , newGhciProgramSession
    , newGhciSession
    )
import Agent.Tools.Grok (closeGrokSession, filterGrokToolsForType, grokTools, newGrokSession)
import Agent.Tools.Grok.Task (GrokSubagentSpecs)
import Agent.Tools.HaskellProgram (haskellProgramTool)
import Agent.Tools.MultiAgents (MultiAgentContext)
import Agent.Tools.PlanMode (PlanModeEnv, PlanModeHooks, newPlanModeEnv)
import Agent.Tools.Types
import Control.Exception.Safe (onException)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)

-- | Tools plus the session plan-mode env (shared across providers).
data CodingTools = CodingTools
    { codingAppTools :: ![AppTool]
    , codingPlanMode :: !PlanModeEnv
    , codingClose :: !(IO ())
      -- | Grok/OpenRouter: model-facing type and runtime overrides per child.
    , codingAgentTypes :: !GrokSubagentSpecs
    }

-- | Tools advertised for a model vendor. Surfaces are never mixed.
-- Every provider gets a persistent GHCi session for 'run_ghci'.
-- OpenAI keeps managed long-running shell processes. Grok/OpenRouter also keep
-- persistent cwd/environment shell state. 'codingClose' closes owned sessions;
-- run it in 'finally'.
codingToolsFor
    :: Provider
    -> ToolEnv
    -> Maybe PlanModeHooks
    -> Maybe MultiAgentContext
    -> IO CodingTools
codingToolsFor provider env hooks multi = do
    codingToolsForWithHaskellProgram True provider env hooks multi

codingToolsForWithHaskellProgram
    :: Bool
    -> Provider
    -> ToolEnv
    -> Maybe PlanModeHooks
    -> Maybe MultiAgentContext
    -> IO CodingTools
codingToolsForWithHaskellProgram includeHaskellProgram provider env hooks multi = do
    typesRef <- newIORef Map.empty
    codingToolsForWithTypesAndHaskellProgram
        includeHaskellProgram provider env hooks multi typesRef

-- | Same as 'codingToolsFor', but reuses an existing agent-type map so the
-- host can wire resume-from-disk before tools are built.
codingToolsForWithTypes
    :: Provider
    -> ToolEnv
    -> Maybe PlanModeHooks
    -> Maybe MultiAgentContext
    -> GrokSubagentSpecs
    -> IO CodingTools
codingToolsForWithTypes provider env hooks multi typesRef = do
    codingToolsForWithTypesAndHaskellProgram
        True provider env hooks multi typesRef

codingToolsForWithTypesAndHaskellProgram
    :: Bool
    -> Provider
    -> ToolEnv
    -> Maybe PlanModeHooks
    -> Maybe MultiAgentContext
    -> GrokSubagentSpecs
    -> IO CodingTools
codingToolsForWithTypesAndHaskellProgram
        includeHaskellProgram provider env hooks multi typesRef = do
    resources <- newResourceScope
    flip onException (closeResourceScope resources) do
        plan <- newPlanModeEnv env.toolCwd hooks
        case provider of
            XAIProvider ->
                grokCodingTools resources plan
            OpenRouterProvider ->
                grokCodingTools resources plan
            OpenAIProvider -> do
                (_, shellSession) <- allocateResource resources
                    (newCodexShellSession env)
                    closeCodexShellSession
                (_, ghci) <- allocateResource resources
                    (newGhciSession env)
                    closeGhciSession
                programTools <- haskellProgramTools resources plan
                tools <- codexTools env shellSession ghci plan multi
                pure CodingTools
                    { codingAppTools =
                        tools ++ programTools
                    , codingPlanMode = plan
                    , codingClose = closeResourceScope resources
                    , codingAgentTypes = typesRef
                    }
  where
    grokCodingTools resources plan = do
        (_, session) <- allocateResource resources
            (newGrokSession env)
            closeGrokSession
        (_, ghci) <- allocateResource resources
            (newGhciSession env)
            closeGhciSession
        programTools <- haskellProgramTools resources plan
        pure CodingTools
            { codingAppTools =
                grokTools session ghci plan multi typesRef
                    ++ programTools
            , codingPlanMode = plan
            , codingClose = closeResourceScope resources
            , codingAgentTypes = typesRef
            }

    haskellProgramTools resources plan
        | not includeHaskellProgram = pure []
        | otherwise = do
            (_, programGhci) <- allocateResource resources
                (newGhciProgramSession env)
                closeGhciSession
            pure [haskellProgramTool programGhci plan]

-- | Re-export for CLI child runners.
filterChildGrokTools :: Text -> [AppTool] -> [AppTool]
filterChildGrokTools = filterGrokToolsForType

