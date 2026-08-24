module Agent.CLI.Dialects
    ( CodingTools(..)
    , codingToolsFor
    , codingToolsForWithTypes
    , codingToolsForWithTypesAndGhciHelpers
    , filterBashTools
    , filterChildGrokTools
    , filterGhciTools
    , filterReadOnlyTools
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
    , newCodexCodingToolsWithGhciHelpers
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
    , newGrokCodingToolsWithGhciHelpers
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
import Agent.Tools.Types (AppTool(..), ApprovalRule(..), ToolEnv(..))
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
    codingToolsForWithTypesAndGhciHelpers
        dialect env planHooks secretHooks multi typesRef []

codingToolsForWithTypesAndGhciHelpers
    :: Dialect
    -> ToolEnv
    -> Maybe PlanModeHooks
    -> Maybe SecretPromptHooks
    -> Maybe MultiAgentContext
    -> GrokSubagentSpecs
    -> [Text]
    -> IO CodingTools
codingToolsForWithTypesAndGhciHelpers
        dialect env planHooks secretHooks multi typesRef ghciHelpers = do
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
            coding <-
                if null ghciHelpers
                    then newCodexCodingTools env planHooks multi
                    else newCodexCodingToolsWithGhciHelpers
                        env planHooks multi ghciHelpers
            pure $
                finish
                    coding.codexAppTools
                    coding.codexPlanMode
                    coding.codexClose
                    typesRef
        GrokBuildToolSurface -> do
            coding <-
                if null ghciHelpers
                    then newGrokCodingTools env planHooks multi typesRef
                    else newGrokCodingToolsWithGhciHelpers
                        env planHooks multi typesRef ghciHelpers
            pure $
                finish
                    coding.grokAppTools
                    coding.grokPlanMode
                    coding.grokClose
                    coding.grokAgentTypes
        ClaudeCodeToolSurface -> do
            plan <- newPlanModeEnv env.toolCwd planHooks
            pure $
                finish
                    []
                    plan
                    (pure ())
                    typesRef

filterChildGrokTools :: Text -> [AppTool] -> [AppTool]
filterChildGrokTools = filterGrokToolsForType

filterBashTools :: Bool -> [AppTool] -> [AppTool]
filterBashTools True = id
filterBashTools False = filter (not . isBashTool)

filterGhciTools :: Bool -> [AppTool] -> [AppTool]
filterGhciTools True = id
filterGhciTools False = filter (not . isGhciTool)

filterReadOnlyTools :: [AppTool] -> [AppTool]
filterReadOnlyTools = filter \tool -> case tool.appToolApproval of
    AlwaysReadOnly -> True
    ClassifyReadOnly _ -> False
    AlwaysPrompt -> False

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
