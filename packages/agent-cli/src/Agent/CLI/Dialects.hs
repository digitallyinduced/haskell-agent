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
import Agent.Tools.Types (AppTool, ToolEnv)
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
    -> Maybe MultiAgentContext
    -> IO CodingTools
codingToolsFor dialect env hooks multi = do
    typesRef <- newIORef Map.empty
    codingToolsForWithTypes dialect env hooks multi typesRef

codingToolsForWithTypes
    :: Dialect
    -> ToolEnv
    -> Maybe PlanModeHooks
    -> Maybe MultiAgentContext
    -> GrokSubagentSpecs
    -> IO CodingTools
codingToolsForWithTypes dialect env hooks multi typesRef =
    case dialectToolSurface dialect of
        CodexToolSurface -> do
            coding <- newCodexCodingTools env hooks multi
            pure CodingTools
                { codingAppTools = coding.codexAppTools
                , codingPlanMode = coding.codexPlanMode
                , codingClose = coding.codexClose
                , codingAgentTypes = typesRef
                }
        GrokBuildToolSurface -> do
            coding <- newGrokCodingTools env hooks multi typesRef
            pure CodingTools
                { codingAppTools = coding.grokAppTools
                , codingPlanMode = coding.grokPlanMode
                , codingClose = coding.grokClose
                , codingAgentTypes = coding.grokAgentTypes
                }

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
