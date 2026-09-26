module Agent.Telegram.Internal.Model
    ( modelCommand
    , selectedTargetForChat
    , retargetTelegramSession
    ) where

import Agent.Accounts.Gateway.Credentials (withGatewayCredentialLeaseAt)
import Agent.Runtime.GatewayClient
    ( gatewayCredentialIdentity
    , loadGatewayCredentialAt
    , newGatewayModelAccessWithStore
    , refreshGatewayModels
    )
import Agent.Runtime.Models
    ( ModelOption(..)
    , ModelTarget(..)
    , modelCatalog
    , resolveModelOptionById
    )
import Agent.Runtime.Session
    ( SessionHandle(..)
    , SessionMeta(..)
    , loadSessionHandle
    , sessionLegacySubagentTarget
    , writeSessionMeta
    )
import Agent.Runtime.Startup.Gateway (modelOptionsForGatewayModels)
import Agent.Telegram.Internal.Runtime.Types
import Agent.Telegram.Internal.Support (lookupBinding, modifyState)
import Agent.Telegram.Types
import Control.Concurrent.MVar (readMVar)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)

data AvailableModels = AvailableModels
    { availableOptions :: ![ModelOption]
    , availableGatewayIdentity :: !(Maybe Text)
    }

loadAvailableModels :: TelegramRuntime -> IO (Either Text AvailableModels)
loadAvailableModels runtime =
  withGatewayCredentialLeaseAt runtime.runtimeHome $
    loadGatewayCredentialAt runtime.runtimeHome >>= \case
        Left err -> pure (Left ("Could not load the organization gateway: " <> err))
        Right Nothing -> pure $ Right AvailableModels
            { availableOptions = modelCatalog runtime.runtimeModelCatalog
            , availableGatewayIdentity = Nothing
            }
        Right (Just credential) -> do
            access <- newGatewayModelAccessWithStore runtime.runtimePool credential
            refreshGatewayModels access >>= \case
                Left err -> pure (Left err)
                Right [] -> pure (Left
                    "The organization gateway does not offer any models.")
                Right models -> pure $ Right AvailableModels
                    { availableOptions = modelOptionsForGatewayModels
                        runtime.runtimeModelCatalog models
                    , availableGatewayIdentity =
                        Just (gatewayCredentialIdentity credential)
                    }

selectedTargetForChat
    :: TelegramRuntime
    -> TelegramChatKey
    -> IO (Either Text (Maybe (ModelTarget, Maybe Text)))
selectedTargetForChat runtime key = do
    state <- readMVar runtime.runtimeStateVar
    case Map.lookup key state.modelSelections of
        Nothing -> pure (Right Nothing)
        Just modelId -> loadAvailableModels runtime >>= \case
            Left err -> pure (Left err)
            Right available -> pure $ case
                    resolveModelOptionById available.availableOptions modelId of
                Nothing -> Left
                    ("Model " <> modelId <> " is no longer available. Use /model to choose another one.")
                Just option -> Right $ Just
                    (option.modelTarget, available.availableGatewayIdentity)

modelCommand :: TelegramRuntime -> TelegramChatKey -> Text -> IO Text
modelCommand runtime key rawModel =
    loadAvailableModels runtime >>= \case
        Left err -> pure ("Could not load available models: " <> err)
        Right available
            | Text.null requested -> showModels runtime key available
            | otherwise -> case resolveModelOptionById available.availableOptions requested of
                Nothing -> pure $
                    "Unknown model: " <> requested <> "\nAvailable models: "
                        <> renderModelIds available.availableOptions
                Just option -> do
                    retargetBoundSession runtime key option.modelTarget
                        available.availableGatewayIdentity
                    modifyState runtime \state -> state
                        { modelSelections = Map.insert key requested state.modelSelections }
                    pure $ "Switched this conversation to " <> requested
                        <> ". Send /retry to retry the last failed turn."
  where
    requested = Text.strip rawModel

showModels :: TelegramRuntime -> TelegramChatKey -> AvailableModels -> IO Text
showModels runtime key available = do
    state <- readMVar runtime.runtimeStateVar
    current <- case lookupBinding key state of
        Nothing -> pure $ Map.findWithDefault
            runtime.runtimeTarget.targetModelId key state.modelSelections
        Just sessionId ->
            loadSessionHandle runtime.runtimePool runtime.runtimeSessionsRoot sessionId
                >>= \case
                    Right (handle, _) -> pure handle.sessionMeta.metaModel
                    Left _ -> pure $ Map.findWithDefault
                        runtime.runtimeTarget.targetModelId key state.modelSelections
    pure $ "Current model: " <> current
        <> "\nAvailable models: " <> renderModelIds available.availableOptions
        <> "\nSwitch with /model MODEL."

renderModelIds :: [ModelOption] -> Text
renderModelIds = Text.intercalate ", " . map (.modelTarget.targetModelId)

retargetBoundSession
    :: TelegramRuntime -> TelegramChatKey -> ModelTarget -> Maybe Text -> IO ()
retargetBoundSession runtime key target gatewayIdentity = do
    state <- readMVar runtime.runtimeStateVar
    case lookupBinding key state of
        Nothing -> pure ()
        Just sessionId ->
            loadSessionHandle runtime.runtimePool runtime.runtimeSessionsRoot sessionId
                >>= \case
                    Left err -> fail (Text.unpack err)
                    Right (handle, _) -> do
                        _ <- retargetTelegramSession handle target gatewayIdentity
                        pure ()

retargetTelegramSession
    :: SessionHandle -> ModelTarget -> Maybe Text -> IO SessionHandle
retargetTelegramSession handle target gatewayIdentity = do
    now <- getCurrentTime
    let meta = handle.sessionMeta
        changed = meta.metaProvider /= target.targetProvider
            || meta.metaConnection /= target.targetConnectionId
            || meta.metaModel /= target.targetModelId
            || meta.metaTransportModel /= Just target.targetWireModelId
            || meta.metaDialect /= target.targetDialect
            || meta.metaGatewayIdentity /= gatewayIdentity
        next = meta
            { metaUpdatedAt = now
            , metaProvider = target.targetProvider
            , metaConnection = target.targetConnectionId
            , metaGatewayIdentity = gatewayIdentity
            , metaModel = target.targetModelId
            , metaTransportModel = Just target.targetWireModelId
            , metaDialect = target.targetDialect
            , metaLegacySubagentTarget = Just (sessionLegacySubagentTarget meta)
            , metaLastResponseId = if changed then Nothing else meta.metaLastResponseId
            , metaPromptSnapshot = if changed then Nothing else meta.metaPromptSnapshot
            }
    writeSessionMeta handle.sessionPool handle.sessionMetaPath next
    pure handle { sessionMeta = next }
