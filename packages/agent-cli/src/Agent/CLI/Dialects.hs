module Agent.CLI.Dialects
    ( CodingTools(..)
    , codingToolsFor
    , codingToolsForWithTypes
    , filterBashTools
    , filterChildGrokTools
    , filterGhciTools
    , isBashTool
    , isBashToolName
    , isGhciTool
    , isGhciToolName
    , formatAgentsMdForDialect
    , globalAgentsHomeDir
    ) where

import Agent.Codex.Dialect.ProjectInstructions (formatCodexAgentsMd)
import Agent.Codex.Dialect.Runtime
    ( CodexCodingTools(..)
    , newCodexCodingTools
    )
import Agent.Dialect
    ( Dialect
    , InstructionHomeStyle(..)
    , ProjectInstructionStyle(..)
    , ToolSurface(..)
    , dialectInstructionHomeStyle
    , dialectProjectInstructionStyle
    , dialectToolSurface
    )
import Agent.GrokBuild.Dialect.ProjectInstructions (formatGrokAgentsMd)
import Agent.GrokBuild.Dialect.Runtime
    ( GrokCodingTools(..)
    , GrokRuntimeControl
    , newGrokCodingTools
    )
import Agent.GrokBuild.Dialect.Task
    ( GrokSubagentSpecs
    , filterGrokToolsForType
    )
import Agent.ProjectInstructions (LoadedAgentsMd)
import Agent.Tools.MultiAgents (MultiAgentContext)
import Agent.Tools.PlanMode
    ( PlanModeEnv
    , PlanModeHooks
    , newPlanModeEnv
    )
import Agent.Tools.Secret
    ( SecretPromptHooks
    , askSecretTool
    , closeSecretStore
    , newSecretStore
    )
import Agent.Tools.Types (AppTool(..), ToolEnv(..))
import Control.Exception.Safe (finally, onException)
import Data.IORef (newIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import System.OsPath (OsPath, unsafeEncodeUtf, (</>))

data CodingTools = CodingTools
    { codingAppTools :: ![AppTool]
    , codingPlanMode :: !PlanModeEnv
    , codingSuspendGhci :: !(IO ())
    , codingClose :: !(IO ())
    , codingAgentTypes :: !GrokSubagentSpecs
    , codingGrokRuntime :: !(Maybe GrokRuntimeControl)
    }

codingToolsFor
    :: Dialect
    -> ToolEnv
    -> Maybe PlanModeHooks
    -> Maybe SecretPromptHooks
    -> Maybe MultiAgentContext
    -> IO CodingTools
codingToolsFor dialect env planHooks secretHooks multi = do
    typesRef <- newIORef Map.empty
    codingToolsForWithTypes
        dialect env planHooks secretHooks multi typesRef

codingToolsForWithTypes
    :: Dialect
    -> ToolEnv
    -> Maybe PlanModeHooks
    -> Maybe SecretPromptHooks
    -> Maybe MultiAgentContext
    -> GrokSubagentSpecs
    -> IO CodingTools
codingToolsForWithTypes
        dialect env planHooks secretHooks multi typesRef = do
    secretStore <- traverse (newSecretStore env) secretHooks
    let closeSecrets = mapM_ closeSecretStore secretStore
        secretTools = maybe [] (pure . askSecretTool) secretStore
        finish tools plan suspendGhci close agentTypes grokRuntime =
            CodingTools
                { codingAppTools = tools <> secretTools
                , codingPlanMode = plan
                , codingSuspendGhci = suspendGhci
                , codingClose = close `finally` closeSecrets
                , codingAgentTypes = agentTypes
                , codingGrokRuntime = grokRuntime
                }
    flip onException closeSecrets $ case dialectToolSurface dialect of
        CodexToolSurface -> do
            coding <- newCodexCodingTools env planHooks multi
            pure $
                finish
                    coding.codexAppTools
                    coding.codexPlanMode
                    coding.codexSuspendGhci
                    coding.codexClose
                    typesRef
                    Nothing
        GrokBuildToolSurface -> do
            coding <- newGrokCodingTools env planHooks multi typesRef
            pure $
                finish
                    coding.grokAppTools
                    coding.grokPlanMode
                    coding.grokSuspendGhci
                    coding.grokClose
                    coding.grokAgentTypes
                    (Just coding.grokRuntimeControl)
        ClaudeCodeToolSurface -> do
            plan <- newPlanModeEnv env.toolCwd planHooks
            pure $
                finish
                    []
                    plan
                    (pure ())
                    (pure ())
                    typesRef
                    Nothing

filterChildGrokTools :: Text -> [AppTool] -> [AppTool]
filterChildGrokTools = filterGrokToolsForType

filterBashTools :: Bool -> [AppTool] -> [AppTool]
filterBashTools True = id
filterBashTools False = filter (not . isBashTool)

filterGhciTools :: Bool -> [AppTool] -> [AppTool]
filterGhciTools True = id
filterGhciTools False = filter (not . isGhciTool)

isBashTool :: AppTool -> Bool
isBashTool = isBashToolName . (.appToolName)

isBashToolName :: Text -> Bool
isBashToolName name =
    name `elem`
        ["shell_command", "write_stdin", "run_terminal_cmd"]

isGhciTool :: AppTool -> Bool
isGhciTool = isGhciToolName . (.appToolName)

isGhciToolName :: Text -> Bool
isGhciToolName = (== "run_ghci")

formatAgentsMdForDialect :: Dialect -> OsPath -> LoadedAgentsMd -> Maybe Text
formatAgentsMdForDialect dialect cwd loaded =
    case dialectProjectInstructionStyle dialect of
        CodexProjectInstructions -> formatCodexAgentsMd cwd loaded
        GrokProjectInstructions -> formatGrokAgentsMd loaded

globalAgentsHomeDir :: Dialect -> OsPath -> OsPath
globalAgentsHomeDir dialect home =
    home </> case dialectInstructionHomeStyle dialect of
        CodexInstructionHome -> unsafeEncodeUtf ".codex"
        GrokInstructionHome -> unsafeEncodeUtf ".grok"
        HarnessInstructionHome -> unsafeEncodeUtf ".haskell-agent"
        ClaudeInstructionHome -> unsafeEncodeUtf ".claude"
