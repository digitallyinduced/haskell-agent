-- | Session wiring for Codex catalog models: per-model metadata lookup and
-- the code-mode (@tool_mode: code_mode_only@) tool surface.
--
-- The nested-invoke slot decouples construction order: the code-mode tool set
-- is built during orchestration (where the wire tool list and instructions
-- are assembled), while the approval-aware nested dispatcher only exists once
-- the session runner is live. Until the runner installs it, nested calls fail
-- closed.
module Agent.CLI.CodeModeRuntime
    ( CodeModeSessionRuntime(..)
    , CodeModeNestedSlot
    , CodexCatalogSession(..)
    , newCodeModeNestedSlot
    , setCodeModeNestedInvoke
    , loadCodexCatalogModelInfo
    , codexCatalogDefaultEffort
    , codeModeSessionRuntimeFor
    ) where

import Agent.CLI.Models (modelsCacheFilePath)
import Agent.Dialect
    ( Dialect
    , PromptStyle(..)
    , dialectPromptStyle
    )
import Agent.OpenAI.Models
    ( ModelInfo(..)
    , ModelsManagerOptions(..)
    , RefreshStrategy(..)
    , defaultModelsManagerOptions
    , defaultReasoningEffortForInfo
    , getModelInfo
    , newModelsManager
    , reasoningEffortText
    , refreshModelCatalog
    , toolModeForInfo
    )
import Agent.Provider (Provider(..))
import Agent.ToolDispatch (ToolCall(..))
import Agent.Tools.CodeMode.Host
    ( ImageDetailVisibility(..)
    , codeModeWorkerPath
    )
import Agent.Tools.CodeMode.Tool
    ( CodeModeNamespace(..)
    , CodeModeNestedSpec(..)
    , CodeModeToolSet(..)
    , ToolMode(..)
    , newCodeModeToolSet
    )
import Agent.Tools.MultiAgents
    ( multiAgentNamespace
    , multiAgentToolNames
    )
import Agent.Tools.Types (AppTool(..))
import Control.Exception.Safe (tryAny)
import Data.IORef
    ( IORef
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.Text (Text)
import System.OsPath (OsPath)

-- | Late-bound nested dispatcher for code-mode tool calls.
newtype CodeModeNestedSlot =
    CodeModeNestedSlot (IORef (ToolCall -> IO (Either Text Text)))

newCodeModeNestedSlot :: IO CodeModeNestedSlot
newCodeModeNestedSlot =
    CodeModeNestedSlot
        <$> newIORef \_call ->
            pure (Left "code mode is still starting; retry this call")

setCodeModeNestedInvoke
    :: CodeModeNestedSlot
    -> (ToolCall -> IO (Either Text Text))
    -> IO ()
setCodeModeNestedInvoke (CodeModeNestedSlot ref) = writeIORef ref

invokeThroughSlot
    :: CodeModeNestedSlot
    -> ToolCall
    -> IO (Either Text Text)
invokeThroughSlot (CodeModeNestedSlot ref) call = do
    invoke <- readIORef ref
    invoke call

-- | Session-scoped catalog-instruction context: how to rebuild instructions
-- for a changed tool surface, and the generated environment-context block
-- replayed when generated context is rebuilt after /clear.
data CodexCatalogSession = CodexCatalogSession
    { catalogInstructionsFor :: !([Text] -> Maybe OsPath -> Text)
    , catalogEnvironmentContext :: !Text
    }

data CodeModeSessionRuntime = CodeModeSessionRuntime
    { codeModeWireTools :: ![AppTool]
      -- ^ @exec@ and @wait@: the only locally dispatched tools advertised on
      -- the wire in code-only mode.
    , codeModeNestedSlot :: !CodeModeNestedSlot
    , codeModeNestedToolNames :: ![Text]
    , codeModeClose :: !(IO ())
    }

-- | Resolve catalog metadata for the active model without touching the
-- network: bundled catalog overlaid with the local disk cache. Only Codex
-- prompt-style OpenAI sessions consult the catalog, and unknown slugs (which
-- resolve to fallback metadata) yield 'Nothing' so the established prompt and
-- tool behavior is retained.
loadCodexCatalogModelInfo
    :: FilePath
    -> Provider
    -> Dialect
    -> Text
    -> IO (Maybe ModelInfo)
loadCodexCatalogModelInfo stateDir provider dialect model
    | provider /= OpenAIProvider = pure Nothing
    | dialectPromptStyle dialect /= CodexPromptStyle = pure Nothing
    | otherwise =
        tryAny load >>= \case
            Left _ -> pure Nothing
            Right info
                | info.usedFallbackModelMetadata -> pure Nothing
                | otherwise -> pure (Just info)
  where
    load = do
        manager <- newModelsManager defaultModelsManagerOptions
            { cachePath = Just (modelsCacheFilePath stateDir)
            }
        _ <- refreshModelCatalog manager RefreshOffline
        getModelInfo manager model

-- | Catalog default reasoning effort for a model, as CLI effort text.
codexCatalogDefaultEffort :: Maybe ModelInfo -> Maybe Text
codexCatalogDefaultEffort info =
    reasoningEffortText
        <$> (info >>= defaultReasoningEffortForInfo)

-- | Build the code-mode session runtime when the catalog selects
-- @code_mode_only@ for this model. 'Right Nothing' keeps conventional tools;
-- 'Left' reports why code mode could not start (the caller should warn and
-- fall back to direct tools rather than refuse to start the session).
codeModeSessionRuntimeFor
    :: Maybe ModelInfo
    -> [AppTool]
    -> IO (Either Text (Maybe CodeModeSessionRuntime))
codeModeSessionRuntimeFor maybeInfo nestedTools =
    case maybeInfo of
        Nothing -> pure (Right Nothing)
        Just info ->
            case toolModeForInfo ConventionalToolMode info of
                ConventionalToolMode -> pure (Right Nothing)
                -- Mixed code mode (direct tools plus exec) is not implemented;
                -- no shipped catalog model requests it.
                CodeToolMode -> pure (Right Nothing)
                CodeOnlyToolMode -> do
                    slot <- newCodeModeNestedSlot
                    workerPath <- codeModeWorkerPath
                    built <- newCodeModeToolSet
                        CodeOnlyToolMode
                        (imageDetailVisibilityFor info)
                        workerPath
                        (invokeThroughSlot slot)
                        (map nestedSpecFor nestedTools)
                    pure $ case built of
                        Left err -> Left err
                        Right toolSet -> Right $ Just CodeModeSessionRuntime
                            { codeModeWireTools = toolSet.codeModeTools
                            , codeModeNestedSlot = slot
                            , codeModeNestedToolNames =
                                toolSet.codeModeNestedToolNames
                            , codeModeClose = toolSet.closeCodeModeToolSet
                            }

imageDetailVisibilityFor :: ModelInfo -> ImageDetailVisibility
imageDetailVisibilityFor _info = ImageDetailVisible

nestedSpecFor :: AppTool -> CodeModeNestedSpec
nestedSpecFor tool = CodeModeNestedSpec
    { nestedSpecTool = tool
    , nestedSpecNamespace =
        if tool.appToolName `elem` multiAgentToolNames
            then Just CodeModeNamespace
                { namespaceName = multiAgentNamespace
                , namespaceDescription =
                    "Tools for spawning and managing sub-agents."
                }
            else Nothing
    }
