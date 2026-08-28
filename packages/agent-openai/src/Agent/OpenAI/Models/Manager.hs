-- | Bundled, cached, and remotely refreshed Codex model discovery.
module Agent.OpenAI.Models.Manager
    ( RefreshStrategy(..)
    , ModelsManagerOptions(..)
    , defaultModelsManagerOptions
    , ModelsManager
    , newModelsManager
    , newModelsManagerWithBundled
    , currentModelCatalog
    , refreshModelCatalog
    , refreshModelCatalogEither
    , refreshIfNewEtag
    , managerUsesChatGptAuth
    , listAvailableModels
    , getDefaultModel
    , getModelInfo
    , getCurrentEtag
    ) where

import Agent.Error (ApiError)
import Agent.OpenAI.Models.Bundled (loadBundledModelsOrThrow)
import Agent.OpenAI.Models.Cache
    ( ModelsCacheEntry(..)
    , ModelsCacheKey(..)
    , defaultModelsCacheTtl
    , loadFreshModelsCache
    , refreshModelsCacheTtl
    , storeModelsCache
    )
import Agent.OpenAI.Models.Client
    ( ModelsEndpointClient(..)
    , ModelsEndpointResponse(..)
    , ModelsFetchCondition(..)
    , packageClientVersion
    )
import Agent.OpenAI.Models.Types
    ( ModelInfo(..)
    , ModelPreset
    , ModelVisibility(..)
    , ModelsResponse(..)
    , availableModelPresets
    , defaultModelSlug
    , mergeModelCatalogs
    , modelInfoForSlug
    )
import Control.Applicative ((<|>))
import Control.Concurrent.MVar
import Data.Text (Text)
import Data.Time.Clock
    ( NominalDiffTime
    , getCurrentTime
    )
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

data RefreshStrategy
    = RefreshOnline
    | RefreshOffline
    | RefreshOnlineIfUncached
    deriving (Eq, Show)

data ModelsManagerOptions = ModelsManagerOptions
    { endpointClient :: !(Maybe ModelsEndpointClient)
    , cachePath :: !(Maybe FilePath)
    , cacheKey :: !ModelsCacheKey
    , clientVersion :: !Text
    , cacheTtl :: !NominalDiffTime
    , remoteCatalogAuthoritative :: !Bool
    }

defaultModelsManagerOptions :: ModelsManagerOptions
defaultModelsManagerOptions = ModelsManagerOptions
    { endpointClient = Nothing
    , cachePath = Nothing
    , cacheKey = ModelsCacheKey
        { providerId = "openai"
        , baseUrl = "https://chatgpt.com/backend-api/codex"
        , accountId = Nothing
        }
    , clientVersion = packageClientVersion
    , cacheTtl = defaultModelsCacheTtl
    , remoteCatalogAuthoritative = True
    }

data ModelsManagerState = ModelsManagerState
    { catalog :: !ModelsResponse
    , etag :: !(Maybe Text)
    , cacheKey :: !(Maybe ModelsCacheKey)
    }

data ModelsManager = ModelsManager
    { bundledCatalog :: !ModelsResponse
    , options :: !ModelsManagerOptions
    , state :: !(MVar ModelsManagerState)
    , refreshLock :: !(MVar ())
    }

newModelsManager :: ModelsManagerOptions -> IO ModelsManager
newModelsManager options = do
    bundled <- loadBundledModelsOrThrow
    newModelsManagerWithBundled bundled options

newModelsManagerWithBundled
    :: ModelsResponse
    -> ModelsManagerOptions
    -> IO ModelsManager
newModelsManagerWithBundled bundledCatalog options = do
    state <- newMVar ModelsManagerState
        { catalog = bundledCatalog
        , etag = Nothing
        , cacheKey = Nothing
        }
    refreshLock <- newMVar ()
    pure ModelsManager { .. }

currentModelCatalog :: ModelsManager -> IO ModelsResponse
currentModelCatalog manager = (.catalog) <$> readMVar manager.state

getCurrentEtag :: ModelsManager -> IO (Maybe Text)
getCurrentEtag manager = (.etag) <$> readMVar manager.state

refreshModelCatalog
    :: ModelsManager
    -> RefreshStrategy
    -> IO ModelsResponse
refreshModelCatalog manager strategy =
    refreshModelCatalogEither manager strategy >>= \case
        Right catalog -> pure catalog
        Left _ -> currentModelCatalog manager

refreshModelCatalogEither
    :: ModelsManager
    -> RefreshStrategy
    -> IO (Either ApiError ModelsResponse)
refreshModelCatalogEither manager strategy =
    withMVar manager.refreshLock \() ->
        refreshLocked manager strategy

refreshIfNewEtag :: ModelsManager -> Text -> IO ModelsResponse
refreshIfNewEtag manager observedEtag =
    withMVar manager.refreshLock \() -> do
        currentEtag <- getCurrentEtag manager
        if currentEtag == Just observedEtag
            then do
                touchCache manager
                currentModelCatalog manager
            else
                refreshLocked manager RefreshOnline >>= \case
                    Left _ -> currentModelCatalog manager
                    Right catalog -> pure catalog

listAvailableModels
    :: ModelsManager
    -> IO [ModelPreset]
listAvailableModels manager =
    availableModelPresets (managerUsesChatGptAuth manager)
        <$> currentModelCatalog manager

getDefaultModel
    :: ModelsManager
    -> Maybe Text
    -> IO (Maybe Text)
getDefaultModel manager requested =
    case requested of
        Just model -> pure (Just model)
        Nothing -> defaultModelSlug <$> listAvailableModels manager

managerUsesChatGptAuth :: ModelsManager -> Bool
managerUsesChatGptAuth manager =
    maybe False (.usesChatGptAuth) manager.options.endpointClient

getModelInfo :: ModelsManager -> Text -> IO ModelInfo
getModelInfo manager model =
    modelInfoForSlug model <$> currentModelCatalog manager

refreshLocked
    :: ModelsManager
    -> RefreshStrategy
    -> IO (Either ApiError ModelsResponse)
refreshLocked manager strategy =
    case strategy of
        RefreshOffline -> do
            loaded <- tryLoadCache manager
            maybe (currentModelCatalog manager) pure loaded >>= pure . Right
        RefreshOnlineIfUncached -> do
            loaded <- tryLoadCache manager
            case loaded of
                Just catalog -> pure (Right catalog)
                Nothing -> fetchRemote manager
        RefreshOnline -> fetchRemote manager

tryLoadCache :: ModelsManager -> IO (Maybe ModelsResponse)
tryLoadCache manager =
    case manager.options.cachePath of
        Nothing -> pure Nothing
        Just path -> do
            now <- getCurrentTime
            loadFreshModelsCache
                now
                manager.options.cacheTtl
                manager.options.cacheKey
                manager.options.clientVersion
                path >>= \case
                Left _ -> pure Nothing
                Right Nothing -> pure Nothing
                Right (Just entry) -> do
                    let catalog = applyRemoteCatalog manager ModelsResponse
                            { models = entry.models
                            , catalogGeneration = entry.catalogGeneration
                            }
                    modifyMVar_ manager.state \_ -> pure ModelsManagerState
                        { catalog
                        , etag = entry.etag
                        , cacheKey = entry.cacheKey
                        }
                    pure (Just catalog)

fetchRemote
    :: ModelsManager
    -> IO (Either ApiError ModelsResponse)
fetchRemote manager =
    case manager.options.endpointClient of
        Nothing -> Right <$> currentModelCatalog manager
        Just endpoint
            | not endpoint.allowsRemoteRefresh ->
                Right <$> currentModelCatalog manager
        Just endpoint -> do
            current <- readMVar manager.state
            let condition = ModelsFetchCondition
                    <$> current.etag
                    <*> current.cacheKey
            endpoint.fetchModels condition >>= \case
                Left err -> pure (Left err)
                Right ModelsNotModified{etag, cacheKey} -> do
                    let nextEtag = etag <|> current.etag
                    modifyMVar_ manager.state \current@ModelsManagerState{} ->
                        pure ModelsManagerState
                            { catalog = current.catalog
                            , etag = nextEtag
                            , cacheKey = Just cacheKey
                            }
                    touchCache manager
                    Right <$> currentModelCatalog manager
                Right ModelsFetched
                        { catalog = remote
                        , etag
                        , cacheKey
                        } -> do
                    let catalog = applyRemoteCatalog manager remote
                    modifyMVar_ manager.state \_ -> pure ModelsManagerState
                        { catalog
                        , etag
                        , cacheKey = Just cacheKey
                        }
                    storeCache manager cacheKey remote etag
                    pure (Right catalog)

applyRemoteCatalog :: ModelsManager -> ModelsResponse -> ModelsResponse
applyRemoteCatalog manager remote
    | manager.options.remoteCatalogAuthoritative
        && maybe False (.usesChatGptAuth) manager.options.endpointClient
        && any ((== ModelVisibilityList) . (.visibility)) remote.models =
            remote
    | otherwise =
        mergeModelCatalogs
            manager.bundledCatalog
            remote

storeCache
    :: ModelsManager
    -> ModelsCacheKey
    -> ModelsResponse
    -> Maybe Text
    -> IO ()
storeCache manager cacheKey catalog etag =
    case manager.options.cachePath of
        Nothing -> pure ()
        Just path -> do
            createDirectoryIfMissing True (takeDirectory path)
            fetchedAt <- getCurrentTime
            _ <- storeModelsCache path ModelsCacheEntry
                { fetchedAt
                , etag
                , clientVersion = Just manager.options.clientVersion
                , cacheKey = Just cacheKey
                , models = catalog.models
                , catalogGeneration = catalog.catalogGeneration
                }
            pure ()

touchCache :: ModelsManager -> IO ()
touchCache manager =
    case manager.options.cachePath of
        Nothing -> pure ()
        Just path -> do
            current <- readMVar manager.state
            now <- getCurrentTime
            _ <- refreshModelsCacheTtl
                now
                manager.options.cacheTtl
                (maybe manager.options.cacheKey id current.cacheKey)
                manager.options.clientVersion
                path
            pure ()
