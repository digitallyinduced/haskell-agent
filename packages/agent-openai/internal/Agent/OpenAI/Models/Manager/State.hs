-- | Pure catalog acceptance rules. The manager owns locking and executes cache
-- actions only after publishing the corresponding state update.
module Agent.OpenAI.Models.Manager.State
    ( ModelsManagerState(..)
    , CacheAction(..)
    , initialState
    , cachedState
    , fetchedState
    , notModifiedState
    , responseCacheAction
    , fetchCondition
    ) where

import Agent.OpenAI.Models.Cache (ModelsCacheEntry(..), ModelsCacheKey)
import Agent.OpenAI.Models.Client (ModelsEndpointResponse(..), ModelsFetchCondition(..))
import Agent.OpenAI.Models.Types
    ( ModelsResponse(..), ModelInfo(..), ModelVisibility(..), mergeModelCatalogs )
import Control.Applicative ((<|>))
import Data.Text (Text)

data ModelsManagerState = ModelsManagerState
    { catalog :: !ModelsResponse
    , etag :: !(Maybe Text)
    , cacheKey :: !(Maybe ModelsCacheKey)
    } deriving (Eq, Show)

data CacheAction
    = TouchCache
    -- Store the original remote catalog, never the bundled overlay.
    | StoreCache ModelsCacheKey ModelsResponse (Maybe Text)
    deriving (Eq, Show)

initialState :: ModelsResponse -> ModelsManagerState
initialState bundled = ModelsManagerState bundled Nothing Nothing

-- The Bool means authoritative ChatGPT discovery is enabled. A catalog without
-- a listed model still needs the bundled fallback, even in that mode.
acceptCatalog :: Bool -> ModelsResponse -> ModelsResponse -> ModelsResponse
acceptCatalog authoritative bundled remote
    | authoritative && any ((== ModelVisibilityList) . (.visibility)) remote.models = remote
    | otherwise = mergeModelCatalogs bundled remote

cachedState :: Bool -> ModelsResponse -> ModelsCacheEntry -> ModelsManagerState
cachedState authoritative bundled entry = ModelsManagerState
    { catalog = acceptCatalog authoritative bundled
        (ModelsResponse entry.models entry.catalogGeneration)
    , etag = entry.etag
    , cacheKey = entry.cacheKey
    }

fetchedState
    :: Bool -> ModelsResponse -> ModelsResponse -> Maybe Text -> ModelsCacheKey
    -> ModelsManagerState
fetchedState authoritative bundled remote etag key = ModelsManagerState
    { catalog = acceptCatalog authoritative bundled remote
    , etag
    , cacheKey = Just key
    }

-- ETag fallback uses the pre-request snapshot; the catalog is retained from
-- the state at commit. Keeping these inputs separate preserves the IO boundary.
notModifiedState
    :: Maybe Text -> Maybe Text -> ModelsCacheKey -> ModelsManagerState
    -> ModelsManagerState
notModifiedState previousEtag responseEtag key current = ModelsManagerState
    { catalog = current.catalog
    , etag = responseEtag <|> previousEtag
    , cacheKey = Just key
    }

responseCacheAction :: ModelsEndpointResponse -> CacheAction
responseCacheAction ModelsNotModified{} = TouchCache
responseCacheAction ModelsFetched{catalog, etag, cacheKey} =
    StoreCache cacheKey catalog etag

fetchCondition :: ModelsManagerState -> Maybe ModelsFetchCondition
fetchCondition current = ModelsFetchCondition <$> current.etag <*> current.cacheKey
