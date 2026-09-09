-- | Native model-picker context and JSON presentation. Callers supply the
-- credential snapshot while holding the gateway boundary critical section.
module Agent.CLI.MacOS.NativeModelCatalog
    ( loadNativeModelCatalog
    ) where

import Agent.CLI.MacOS.NativeGatewayBoundary (validateNativeSessionBoundary)
import Agent.CLI.MacOS.NativeRequest (ModelsListRequest(..))
import Agent.CLI.GatewayClient (GatewayCredential)
import Agent.CLI.GatewayModels (loadGatewayModelOptionsWithCredentialAt)
import Agent.CLI.ModelConfig
    ( CatalogModel(..), ModelCatalog, catalogModelForConnection )
import Agent.CLI.Models
    ( ModelOption(..)
    , ModelTarget(..)
    , PickerState(..)
    , defaultModelOptionFor
    , initialPickerStateForOptions
    , initialPickerStateResolved
    , resolveConfiguredModel
    , resolveModelOptionById
    , resolveModelOptionDialect
    , selectedOption
    )
import Agent.CLI.Project
    ( ProjectModel(..)
    , ProjectSettings(..)
    , inheritProjectLastModel
    , loadProjectSettings
    , resolveProjectRoot
    )
import Agent.CLI.Session (SessionMeta(..))
import Agent.Dialect (dialectSlug)
import Agent.Provider (Provider(..), providerSlug)
import Agent.Store.Postgres (Store, trustedPool)
import qualified Data.Aeson as Aeson
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.OsPath (OsPath, takeDirectory, unsafeEncodeUtf)

loadNativeModelCatalog
    :: Store
    -> OsPath
    -> Maybe GatewayCredential
    -> Maybe Text
    -> ModelsListRequest
    -> IO (Either Text Aeson.Value)
loadNativeModelCatalog
        store root gatewayCredential gatewayIdentity request = do
    let home = takeDirectory (takeDirectory root)
        requestedCwd = unsafeEncodeUtf request.modelsListCwd
    contextResult <- currentModelContext
        store
        root
        home
        requestedCwd
        gatewayIdentity
        request.modelsListSessionId
    case contextResult of
        Left err -> pure (Left err)
        Right (cwd, maybeTarget) ->
            loadGatewayModelOptionsWithCredentialAt
                home cwd gatewayCredential >>= \case
                Left err -> pure (Left err)
                Right (catalog, Just gatewayOptions) ->
                    case gatewayOptions of
                        [] -> pure
                            (Left
                                "The organization gateway does not offer any models.")
                        firstAvailable : _ -> do
                            let selected =
                                    fromMaybe firstAvailable $ do
                                        target <- maybeTarget
                                        resolveModelOptionById
                                            gatewayOptions
                                            target.targetModelId
                                target = selected.modelTarget
                            picker <- initialPickerStateForOptions
                                "organization gateway"
                                gatewayOptions
                                target.targetConnectionId
                                target.targetProvider
                                target.targetModelId
                                target.targetDialect
                            pure (Right (modelPickerJSON catalog picker))
                Right (catalog, Nothing) -> do
                    let configuredTarget = do
                            target <- maybeTarget
                            option <-
                                resolveConfiguredModel
                                    catalog
                                    target.targetModelId
                            if option.modelTarget.targetConnectionId
                                == target.targetConnectionId
                                then Just option
                                else Nothing
                    selected <- resolveModelOptionDialect $
                        fromMaybe (defaultModelOptionFor catalog OpenAIProvider)
                            configuredTarget
                    let target = selected.modelTarget
                    picker <- initialPickerStateResolved
                        catalog
                        target.targetConnectionId
                        target.targetProvider
                        target.targetModelId
                        target.targetDialect
                    pure (Right (modelPickerJSON catalog picker))

modelPickerJSON :: ModelCatalog -> PickerState -> Aeson.Value
modelPickerJSON catalog picker =
    Aeson.object
        [ "options" Aeson..=
            map (modelOptionJSON catalog) picker.pickerAll
        , "current" Aeson..=
            fmap (modelOptionJSON catalog) (selectedOption picker)
        ]

currentModelContext
    :: Store
    -> OsPath
    -> OsPath
    -> OsPath
    -> Maybe Text
    -> Maybe Text
    -> IO (Either Text (OsPath, Maybe ModelTarget))
currentModelContext store root home cwd gatewayIdentity = \case
    Just sessionId ->
        validateNativeSessionBoundary
            (trustedPool store)
            root
            gatewayIdentity
            sessionId >>= \case
                Left err -> pure (Left err)
                Right meta ->
                    pure
                        (Right
                            ( meta.metaCwd
                            , Just (sessionModelTarget meta)
                            ))
    Nothing -> do
        projectRoot <- resolveProjectRoot cwd
        checkoutSettings <- loadProjectSettings projectRoot
        settings <- inheritProjectLastModel home projectRoot checkoutSettings
        pure $ Right
            ( cwd
            , (.projectModelTarget) <$> settings.settingsLastModel
            )

sessionModelTarget :: SessionMeta -> ModelTarget
sessionModelTarget meta =
    ModelTarget
        { targetProvider = meta.metaProvider
        , targetConnectionId = meta.metaConnection
        , targetModelId = meta.metaModel
        , targetWireModelId =
            fromMaybe meta.metaModel meta.metaTransportModel
        , targetDialect = meta.metaDialect
        }

modelOptionJSON :: ModelCatalog -> ModelOption -> Aeson.Value
modelOptionJSON catalog option =
    let target = option.modelTarget
        configured =
            catalogModelForConnection
                catalog
                target.targetConnectionId
                target.targetModelId
    in Aeson.object
        [ "id" Aeson..= target.targetModelId
        , "provider" Aeson..= providerSlug target.targetProvider
        , "connection" Aeson..= target.targetConnectionId
        , "wireModel" Aeson..= target.targetWireModelId
        , "dialect" Aeson..= dialectSlug target.targetDialect
        , "label" Aeson..= option.modelLabel
        , "supportedReasoningEfforts" Aeson..=
            (configured >>= (.catalogModelReasoningEfforts))
        , "defaultReasoningEffort" Aeson..=
            (configured >>= (.catalogModelDefaultReasoningEffort))
        ]
