module Agent.CLI.Dialects
    ( CodingTools(..)
    , codingToolsFor
    , codingToolsForWithTypes
    , filterChildGrokTools
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
    , newGrokCodingTools
    )
import Agent.GrokBuild.Dialect.Task
    ( GrokSubagentSpecs
    , filterGrokToolsForType
    )
import Agent.ProjectInstructions (LoadedAgentsMd)
import Agent.Tools.MultiAgents (MultiAgentContext)
import Agent.Tools.PlanMode (PlanModeEnv, PlanModeHooks)
import Agent.Tools.Secret
    ( SecretPromptHooks
    , askSecretTool
    , closeSecretStore
    , newSecretStore
    )
import Agent.Tools.Types (AppTool, ToolEnv)
import Control.Exception.Safe (finally, onException)
import Data.IORef (newIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import System.OsPath (OsPath, unsafeEncodeUtf, (</>))

data CodingTools = CodingTools
    { codingAppTools :: ![AppTool]
    , codingPlanMode :: !PlanModeEnv
    , codingClose :: !(IO ())
    , codingAgentTypes :: !GrokSubagentSpecs
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
        finish tools plan close agentTypes =
            CodingTools
                { codingAppTools = tools <> secretTools
                , codingPlanMode = plan
                , codingClose = close `finally` closeSecrets
                , codingAgentTypes = agentTypes
                }
    flip onException closeSecrets $ case dialectToolSurface dialect of
        CodexToolSurface -> do
            coding <- newCodexCodingTools env planHooks multi
            pure $
                finish
                    coding.codexAppTools
                    coding.codexPlanMode
                    coding.codexClose
                    typesRef
        GrokBuildToolSurface -> do
            coding <- newGrokCodingTools env planHooks multi typesRef
            pure $
                finish
                    coding.grokAppTools
                    coding.grokPlanMode
                    coding.grokClose
                    coding.grokAgentTypes

filterChildGrokTools :: Text -> [AppTool] -> [AppTool]
filterChildGrokTools = filterGrokToolsForType

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
