-- | Resolve one session without terminal state or process-global cwd changes.
module Agent.Runtime.Session.Engine.Context
    ( EngineContext(..), EngineError(..), requireEngine, withEngineContext
    ) where

import Agent.Accounts.Auth
    ( LoadedAuth(..), loadAuth, gatewayLoadedAuthForProvider, staticCredentialProvider )
import Agent.Dialect (dialectForId, Dialect)
import Agent.Provider (Provider(..), Credential(..), BillingMode(..))
import Agent.ReasoningEffort (reasoningEffortText)
import Agent.Runtime.Config (HarnessConfig, loadHarnessConfig)
import Agent.Runtime.Gateway.Models
    ( loadGatewayModelOptionsWithCredentialAt, selectGatewayModelOption )
import Agent.Runtime.GatewayClient
    ( GatewayCredential, gatewayCredentialIdentity, loadGatewayCredentialAt )
import Agent.Runtime.Host
import Agent.Runtime.ModelConfig
import Agent.Runtime.Models
import Agent.Runtime.Options (defaultEffortFor)
import Agent.Runtime.Project (resolveProjectRoot)
import Agent.Runtime.Request
import Agent.Runtime.Session
import Agent.Runtime.Session.Settings
import Agent.Runtime.SessionLock (acquireSessionLock, releaseSessionLock)
import Agent.OsPath (unsafeToFilePath)
import Agent.Store.Postgres (Store, trustedPool)
import Control.Applicative ((<|>))
import Control.Exception.Safe (Exception, bracket, throwIO)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory.OsPath (getHomeDirectory)
import System.Environment (lookupEnv)
import System.OsPath (OsPath)

newtype EngineError = EngineError Text deriving (Show)
instance Exception EngineError

requireEngine :: Either Text a -> IO a
requireEngine = either (throwIO . EngineError) pure

data EngineContext = EngineContext
    { engineProcess :: !AgentProcessRuntime
    , engineHooks :: !NativeRunHooks
    , engineSettings :: !SessionSettings
    , engineRequest :: !NativeTurnRequest
    , engineHome :: !OsPath
    , engineCwd :: !OsPath
    , engineProjectRoot :: !OsPath
    , engineStore :: !Store
    , engineRoot :: !OsPath
    , engineTarget :: !ModelTarget
    , engineDialect :: !Dialect
    , engineEffort :: !Text
    , engineCatalog :: !ModelCatalog
    , engineHarness :: !HarnessConfig
    , engineGateway :: !(Maybe GatewayCredential)
    , engineGatewayModels :: !(Maybe [ModelOption])
    , engineLoaded :: !LoadedAuth
    , engineCustomConnection :: !(Maybe ResponsesConnection)
    , engineCustomBearer :: !(Maybe Text)
    , engineResumed :: !(Maybe (SessionMeta, [SessionTurn]))
    }

withEngineContext
    :: AgentProcessRuntime -> NativeRunHooks -> SessionSettings -> NativeTurnRequest
    -> (EngineContext -> IO a) -> IO a
withEngineContext engineProcess engineHooks settings engineRequest use = do
    _ <- requireEngine (validateNativeTurnRequest engineRequest)
    engineSettings <- requireEngine (validateSessionSettings settings)
    engineHome <- case nativePreparedDiscovery engineHooks.nativeWorkspaceDiscovery of
        Just context -> pure context.nativeDiscoveryHome
        Nothing -> maybe getHomeDirectory pure engineHooks.nativeHome
    let engineCwd = engineRequest.nativeTurnCwd
        engineRoot = sessionsRoot engineHome
    engineStore <- maybe (throwIO (EngineError "the session host must supply a database store"))
        pure engineHooks.nativeDatabaseStore
    engineHarness <- loadHarnessConfig engineHome >>= requireEngine
    engineGateway <- loadGatewayCredentialAt engineHome >>= requireEngine
    let catalogRoot = maybe engineCwd (.nativeDiscoveryCatalogRoot)
            (nativePreparedDiscovery engineHooks.nativeWorkspaceDiscovery)
    (engineCatalog, gatewayModels) <-
        loadGatewayModelOptionsWithCredentialAt engineHome catalogRoot engineGateway
            >>= requireEngine
    let engineGatewayModels = gatewayModels
    let resume = case engineRequest.nativeTurnSession of
            NativeNewSession -> pure Nothing
            NativeResumeSession identifier ->
                Just <$> (loadActiveSession (trustedPool engineStore) engineRoot identifier
                    >>= requireEngine)
    let prepare = do
            engineResumed <- resume
            let saved = fst <$> engineResumed
                requestedProvider = engineRequest.nativeTurnProvider <|> ((.metaProvider) <$> saved)
                requestedModel = engineRequest.nativeTurnModel <|> ((.metaModel) <$> saved)
            savedTarget <- traverse (\meta -> requireEngine $
                resolveSavedModelTarget engineCatalog (isJust engineGateway)
                    meta.metaProvider meta.metaConnection meta.metaModel
                    meta.metaTransportModel meta.metaDialect) saved
            engineTarget <- case gatewayModels of
                Just models -> (.modelTarget) <$> requireEngine
                    (selectGatewayModelOption models requestedModel requestedProvider
                        (maybe [] pure savedTarget))
                Nothing -> pure $ fromMaybe
                    (maybe
                        (maybe (defaultModelOptionFor engineCatalog
                            (fromMaybe OpenAIProvider requestedProvider))
                            (rawModelOption (fromMaybe OpenAIProvider requestedProvider)) requestedModel)
                        (\target -> ModelOption target Nothing Nothing Nothing) savedTarget).modelTarget
                    ((.modelTarget) <$> (requestedModel >>= resolveConfiguredModel engineCatalog))
            let engineDialect = dialectForId engineTarget.targetDialect
                engineEffort = reasoningEffortText $ fromMaybe
                    (defaultEffortFor engineTarget.targetProvider)
                    engineRequest.nativeTurnEffort
                engineCustomConnection = case catalogConnection engineCatalog engineTarget.targetConnectionId of
                    Just ModelConnection{connectionKind = CustomResponsesConnection connection} -> Just connection
                    _ -> Nothing
            (engineLoaded, engineCustomBearer) <- case engineCustomConnection of
                Just connection -> loadCustomAuth engineTarget.targetConnectionId connection
                Nothing -> do
                    auth <- case engineGateway of
                        Just credential -> requireEngine
                            (gatewayLoadedAuthForProvider (Just engineTarget.targetProvider) credential)
                        Nothing -> loadAuth (Just engineTarget.targetProvider) >>= requireEngine
                    pure (auth, Nothing)
            engineProjectRoot <- case nativePreparedDiscovery engineHooks.nativeWorkspaceDiscovery of
                Just context -> pure context.nativeDiscoveryProjectRoot
                Nothing -> resolveProjectRoot engineCwd
            let context = EngineContext{..}
            use context
    case engineRequest.nativeTurnSession of
        NativeNewSession -> prepare
        NativeResumeSession identifier -> do
            directory <- requireEngine (sessionDirForId engineRoot identifier)
            bracket (acquireSessionLock directory identifier >>= requireEngine)
                releaseSessionLock (const prepare)

loadCustomAuth :: Text -> ResponsesConnection -> IO (LoadedAuth, Maybe Text)
loadCustomAuth identifier connection = do
    value <- maybe (pure Nothing) (lookupEnv . Text.unpack) connection.responsesApiKeyEnv
    token <- case value of
        Just raw | not (null raw) -> pure (Text.pack raw)
        _ | connection.responsesApiKeyOptional -> pure ""
        _ -> throwIO (EngineError ("custom connection " <> identifier <> " requires its configured API key environment variable"))
    let credential = Credential token identifier Nothing OpenRouterProvider
    pure (LoadedAuth
        { loadedProvider = OpenRouterProvider
        , loadedTokenProvider = staticCredentialProvider ApiBilled credential
        , loadedAccountLabel = const (pure identifier)
        , loadedSelectionId = Nothing
        , loadedOpenAiPool = Nothing
        }, if Text.null token then Nothing else Just token)
