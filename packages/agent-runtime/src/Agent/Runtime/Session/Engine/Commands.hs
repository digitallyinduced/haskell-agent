-- | Local terminal control requests execute within the session's tool scope.
module Agent.Runtime.Session.Engine.Commands (runHarnessCommand) where

import Agent.MCP qualified as MCP
import Agent.GrokBuild.Dialect.Runtime (GrokRuntimeControl(..))
import Agent.GrokBuild.Dialect.Workflow (workflowRunSnapshots, formatWorkflowRuns)
import Agent.Runtime.Session.Engine.Context
import Agent.Runtime.Session.Engine.Tools
import Agent.Runtime.Session (SessionMeta(..))
import Agent.Runtime.Session.Transfer (exportSessionTransfer, SessionTransferEnvelope(..))
import Agent.Store.Postgres (trustedPool)
import Agent.Runtime.Skills.Content (resolveSkillContent)
import Agent.Skills
import Agent.Subagents (listAgents)
import Agent.Tools.PlanMode (readPlanMarkdown)
import Agent.Runtime.Tools.Dialects (CodingTools(..))
import Agent.Tools.Types (AppTool(..))
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Encoding
import Data.ByteString.Lazy qualified as Lazy

runHarnessCommand :: EngineContext -> EngineTools -> Text -> IO (Either Text Text)
runHarnessCommand context runtime request = case eitherDecodeStrict' (Encoding.encodeUtf8 request)
    >>= parseEither parser of
        Left _ -> pure (Left "Invalid local session command.")
        Right (command, name, arguments, server, pairs) -> case (command :: Text) of
            "catalog" -> do
                tools <- runtime.engineReadTools
                pure $ Right $ Encoding.decodeUtf8 $ Lazy.toStrict $ encode $ object
                    [ "tools" .= map (.appToolName) tools
                    , "skills" .= [object
                        [ "name" .= skill.skillName, "summary" .= skill.skillDescription
                        , "argumentHint" .= skill.skillArgumentHint
                        ] | skill <- runtime.engineSkillCatalog.catalogSkills] ]
            "skills" -> pure $ Right $ Text.intercalate "\n"
                ["$" <> skill.skillName <> "  " <> skill.skillDescription
                | skill <- runtime.engineSkillCatalog.catalogSkills]
            "skill" -> case resolveSkillInvocation
                    (buildSkillInvocations [] runtime.engineSkillCatalog) name of
                Left err -> pure (Left err)
                Right invocation -> resolveSkillContent (Just runtime.engineMcp.sessionFleet) invocation.invocationSkill
                    >>= pure . fmap (\content -> formatSkillActivation invocation content arguments)
            "mcp" -> do
                states <- MCP.mcpFleetStatuses runtime.engineMcp.sessionFleet
                pure $ Right $ Text.intercalate "\n"
                    [state.mcpStatusName <> ": " <> Text.pack (show state.mcpStatusState)
                        <> " (" <> Text.pack (show state.mcpStatusToolCount) <> " tools)"
                    | state <- states]
            "mcpPrompt" -> MCP.mcpFleetGetPrompt runtime.engineMcp.sessionFleet server name pairs
                >>= pure . fmap MCP.renderMcpPromptResult
            "plan" -> Right <$> readPlanMarkdown runtime.engineCoding.codingPlanMode
            "agents" -> Right . Text.pack . show <$> listAgents runtime.engineRegistry Nothing
            "workflows" -> case runtime.engineCoding.codingGrokRuntime >>= (.grokWorkflowRuntime) of
                Nothing -> pure (Left "Workflow commands are unavailable for this provider.")
                Just workflow -> Right . formatWorkflowRuns <$> workflowRunSnapshots workflow
            "context" -> do
                tools <- runtime.engineReadTools
                pure (Right ("Available tools: " <> Text.intercalate ", " (map (.appToolName) tools)))
            "transfer" -> case context.engineResumed of
                Nothing -> pure (Left "Session transfer requires a persisted session.")
                Just (meta, _) -> exportSessionTransfer (trustedPool context.engineStore) context.engineRoot meta.metaId
                    >>= pure . fmap (Encoding.decodeUtf8 . Lazy.toStrict . encode . (.transferSession))
            _ -> pure (Left "Unknown local session command.")
  where
    parser = withObject "session command" \fields -> (,,,,)
        <$> fields .: "command" <*> fields .:? "name" .!= ""
        <*> fields .:? "arguments" .!= "" <*> fields .:? "server" .!= ""
        <*> fields .:? "pairs" .!= []
