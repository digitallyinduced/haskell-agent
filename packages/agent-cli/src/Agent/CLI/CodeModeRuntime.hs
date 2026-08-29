-- | Session wiring for Codex catalog models: per-model metadata lookup and
-- direct or code-only tool surfaces.
--
-- The nested-invoke slot decouples construction order: the code-mode tool set
-- is built during orchestration (where the wire tool list and instructions
-- are assembled), while the approval-aware nested dispatcher only exists once
-- the session runner is live. Until the runner installs it, nested calls fail
-- closed.
module Agent.CLI.CodeModeRuntime
    ( CodeModeSessionRuntime(..)
    , CodeModeToolProjection(..)
    , CodeModeNestedSlot
    , CodexCatalogSession(..)
    , newCodeModeNestedSlot
    , setCodeModeNestedInvoke
    , loadCodexCatalogModelInfo
    , codexCatalogDefaultEffort
    , projectCodeModeTools
    , codeModeSessionRuntimeFor
    ) where

import Agent.CLI.Models (modelsCacheFilePath)
import Agent.Dialect
    ( Dialect
    , PromptStyle(..)
    , dialectPromptStyle
    )
import Agent.OpenAI.ImageGeneration
    ( imageGenerationNamespace
    , imageGenerationNamespaceDescription
    , imageGenerationToolName
    )
import Agent.OpenAI.Models
    ( ModelInfo(..)
    , ModelsClientConfig(..)
    , ModelsManagerOptions(..)
    , RefreshStrategy(..)
    , defaultModelsBaseUrl
    , defaultModelsManagerOptions
    , defaultReasoningEffortForInfo
    , getModelInfo
    , modelsCacheKeyForCredential
    , modelsEndpointClient
    , newModelsManager
    , packageClientVersion
    , reasoningEffortText
    , refreshModelCatalog
    , toolModeForInfo
    )
import Agent.Provider
    ( BillingMode(..)
    , Provider(..)
    , TokenProvider
    , getNextToken
    , tokenProviderBillingMode
    )
import Agent.ToolDispatch (ToolCall(..), ToolCallResult)
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
    CodeModeNestedSlot
        (IORef (ToolCall -> IO (Either Text ToolCallResult)))

newCodeModeNestedSlot :: IO CodeModeNestedSlot
newCodeModeNestedSlot =
    CodeModeNestedSlot
        <$> newIORef \_call ->
            pure (Left "code mode is still starting; retry this call")

setCodeModeNestedInvoke
    :: CodeModeNestedSlot
    -> (ToolCall -> IO (Either Text ToolCallResult))
    -> IO ()
setCodeModeNestedInvoke (CodeModeNestedSlot ref) = writeIORef ref

invokeThroughSlot
    :: CodeModeNestedSlot
    -> ToolCall
    -> IO (Either Text ToolCallResult)
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
      -- ^ The @exec@ and @wait@ code-mode entry points.
    , codeModeDirectTools :: ![AppTool]
      -- ^ Conventional tools that remain provider-visible alongside code mode.
    , codeModeNestedSlot :: !CodeModeNestedSlot
    , codeModeNestedToolNames :: ![Text]
    , codeModeClose :: !(IO ())
    }

-- | Provider-visible versus JavaScript-nested tools for one catalog mode.
--
-- For code-only models we deliberately retain the native shell entry points
-- as direct tools: shell execution already has a purpose-built process/session
-- API, so forcing a single command through a JavaScript cell adds no
-- orchestration value. They remain in the nested set as well, since code mode
-- may need to compose shell calls with other tools in one JavaScript cell.
data CodeModeToolProjection = CodeModeToolProjection
    { directCodeModeTools :: ![AppTool]
    , nestedCodeModeTools :: ![AppTool]
    }

projectCodeModeTools :: ToolMode -> [AppTool] -> CodeModeToolProjection
projectCodeModeTools mode tools = case mode of
    ConventionalToolMode -> CodeModeToolProjection tools []
    CodeToolMode -> CodeModeToolProjection tools tools
    CodeOnlyToolMode -> CodeModeToolProjection
        { directCodeModeTools = filter isDirectShellTool tools
        , nestedCodeModeTools = tools
        }
  where
    isDirectShellTool tool =
        tool.appToolName `elem` ["shell_command", "write_stdin"]

-- | Resolve catalog metadata for the active model. With ChatGPT credentials
-- the live @/models@ catalog is fetched at session start (Codex parity:
-- five-minute disk cache, five-second request timeout, ETag-conditional
-- requests, bundled catalog as fallback). Without credentials, the bundled
-- catalog plus any fresh disk cache is used offline. Only Codex prompt-style
-- OpenAI sessions consult the catalog, and unknown slugs (which resolve to
-- fallback metadata) yield 'Nothing' so the established prompt and tool
-- behavior is retained.
loadCodexCatalogModelInfo
    :: FilePath
    -> Provider
    -> Dialect
    -> Maybe TokenProvider
    -> Text
    -> IO (Maybe ModelInfo)
loadCodexCatalogModelInfo stateDir provider dialect tokenProvider model
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
        (options, strategy) <- managerOptionsFor
        manager <- newModelsManager options
        _ <- refreshModelCatalog manager strategy
        getModelInfo manager model

    offline = pure
        ( defaultModelsManagerOptions
            { cachePath = Just (modelsCacheFilePath stateDir)
            }
        , RefreshOffline
        )

    managerOptionsFor = case tokenProvider of
        Nothing -> offline
        Just provider' ->
            getNextToken provider' Nothing >>= \case
                Left _ -> offline
                Right credential -> pure
                    ( defaultModelsManagerOptions
                        { endpointClient = Just
                            (modelsEndpointClient
                                ModelsClientConfig
                                    { baseUrl = defaultModelsBaseUrl
                                    , clientVersion = packageClientVersion
                                    }
                                provider')
                        , cachePath = Just (modelsCacheFilePath stateDir)
                        , cacheKey =
                            modelsCacheKeyForCredential
                                defaultModelsBaseUrl
                                credential
                        , remoteCatalogAuthoritative =
                            tokenProviderBillingMode provider'
                                == SubscriptionBilled
                        }
                    , RefreshOnlineIfUncached
                    )

-- | Catalog default reasoning effort for a model, as CLI effort text.
codexCatalogDefaultEffort :: Maybe ModelInfo -> Maybe Text
codexCatalogDefaultEffort info =
    reasoningEffortText
        <$> (info >>= defaultReasoningEffortForInfo)

-- | Build the code-mode session runtime when the catalog selects code mode.
-- 'Right Nothing' keeps conventional tools;
-- 'Left' reports why code mode could not start (the caller should warn and
-- fall back to direct tools rather than refuse to start the session).
codeModeSessionRuntimeFor
    :: Maybe ModelInfo
    -> [AppTool]
    -> IO (Either Text (Maybe CodeModeSessionRuntime))
codeModeSessionRuntimeFor maybeInfo tools =
    case maybeInfo of
        Nothing -> pure (Right Nothing)
        Just info ->
            case toolModeForInfo ConventionalToolMode info of
                ConventionalToolMode -> pure (Right Nothing)
                -- Mixed mode also augments every direct tool description with
                -- its JavaScript invocation. Keep the established direct-mode
                -- fallback until that provider projection is implemented.
                CodeToolMode -> pure (Right Nothing)
                CodeOnlyToolMode ->
                    buildRuntime
                        info
                        CodeOnlyToolMode
                        (projectCodeModeTools CodeOnlyToolMode tools)

buildRuntime
    :: ModelInfo
    -> ToolMode
    -> CodeModeToolProjection
    -> IO (Either Text (Maybe CodeModeSessionRuntime))
buildRuntime info mode projection = do
    slot <- newCodeModeNestedSlot
    workerPath <- codeModeWorkerPath
    built <- newCodeModeToolSet
        mode
        (imageDetailVisibilityFor info)
        workerPath
        (invokeThroughSlot slot)
        (map nestedSpecFor projection.nestedCodeModeTools)
    pure $ case built of
        Left err -> Left err
        Right toolSet -> Right $ Just CodeModeSessionRuntime
            { codeModeWireTools = toolSet.codeModeTools
            , codeModeDirectTools = projection.directCodeModeTools
            , codeModeNestedSlot = slot
            , codeModeNestedToolNames = toolSet.codeModeNestedToolNames
            , codeModeClose = toolSet.closeCodeModeToolSet
            }

imageDetailVisibilityFor :: ModelInfo -> ImageDetailVisibility
imageDetailVisibilityFor _info = ImageDetailVisible

nestedSpecFor :: AppTool -> CodeModeNestedSpec
nestedSpecFor tool = CodeModeNestedSpec
    { nestedSpecTool = tool
    , nestedSpecNamespace =
        if tool.appToolName == imageGenerationToolName
            then Just CodeModeNamespace
                { namespaceName = imageGenerationNamespace
                , namespaceDescription = imageGenerationNamespaceDescription
                }
            else if tool.appToolName `elem` multiAgentToolNames
                then Just CodeModeNamespace
                { namespaceName = multiAgentNamespace
                , namespaceDescription =
                    "Tools for spawning and managing sub-agents."
                }
                else Nothing
    }
