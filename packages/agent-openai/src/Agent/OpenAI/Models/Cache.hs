-- | Atomic, provider-scoped disk cache for model catalogs.
module Agent.OpenAI.Models.Cache
    ( ModelsCacheKey(..)
    , ModelsCacheEntry(..)
    , ModelsCacheError(..)
    , defaultModelsCacheTtl
    , loadFreshModelsCache
    , storeModelsCache
    , refreshModelsCacheTtl
    ) where

import Agent.FileRetry (writeLazyFileAtomically)
import Agent.OpenAI.Models.Types (ModelInfo)
import Control.Exception.Safe (SomeException, try)
import Data.Aeson (Object)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock
    ( NominalDiffTime
    , UTCTime
    , diffUTCTime
    )
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)
import System.Posix.Files (ownerReadMode, ownerWriteMode, unionFileModes)
import System.OsPath (unsafeEncodeUtf)

data ModelsCacheKey = ModelsCacheKey
    { providerId :: !Text
    , baseUrl :: !Text
    , accountId :: !(Maybe Text)
    } deriving (Eq, Show)

instance Aeson.FromJSON ModelsCacheKey where
    parseJSON = Aeson.withObject "ModelsCacheKey" \value ->
        ModelsCacheKey
            <$> value Aeson..:? "provider_id" Aeson..!= ""
            <*> value Aeson..:? "base_url" Aeson..!= ""
            <*> value Aeson..:? "account_id"

instance Aeson.ToJSON ModelsCacheKey where
    toJSON key = Aeson.object
        [ "provider_id" Aeson..= key.providerId
        , "base_url" Aeson..= key.baseUrl
        , "account_id" Aeson..= key.accountId
        ]

data ModelsCacheEntry = ModelsCacheEntry
    { fetchedAt :: !UTCTime
    , etag :: !(Maybe Text)
    , clientVersion :: !(Maybe Text)
    , cacheKey :: !(Maybe ModelsCacheKey)
    , models :: ![ModelInfo]
    , catalogExtraFields :: !Object
    } deriving (Eq, Show)

instance Aeson.FromJSON ModelsCacheEntry where
    parseJSON = Aeson.withObject "ModelsCacheEntry" \value ->
        ModelsCacheEntry
            <$> value Aeson..: "fetched_at"
            <*> value Aeson..:? "etag"
            <*> value Aeson..:? "client_version"
            <*> value Aeson..:? "cache_key"
            <*> value Aeson..: "models"
            <*> value Aeson..:? "catalog_extra_fields" Aeson..!= mempty

instance Aeson.ToJSON ModelsCacheEntry where
    toJSON entry = Aeson.object
        [ "fetched_at" Aeson..= entry.fetchedAt
        , "etag" Aeson..= entry.etag
        , "client_version" Aeson..= entry.clientVersion
        , "cache_key" Aeson..= entry.cacheKey
        , "models" Aeson..= entry.models
        , "catalog_extra_fields" Aeson..= entry.catalogExtraFields
        ]

newtype ModelsCacheError = ModelsCacheError
    { message :: Text
    } deriving (Eq, Show)

defaultModelsCacheTtl :: NominalDiffTime
defaultModelsCacheTtl = 300

loadFreshModelsCache
    :: UTCTime
    -> NominalDiffTime
    -> ModelsCacheKey
    -> Text
    -> FilePath
    -> IO (Either ModelsCacheError (Maybe ModelsCacheEntry))
loadFreshModelsCache now ttl expectedKey expectedVersion path =
    loadModelsCache path >>= \case
        Left err -> pure (Left err)
        Right Nothing -> pure (Right Nothing)
        Right (Just entry)
            | entry.clientVersion /= Just expectedVersion ->
                pure (Right Nothing)
            | entry.cacheKey /= Just expectedKey ->
                pure (Right Nothing)
            | ttl <= 0 || diffUTCTime now entry.fetchedAt > ttl ->
                pure (Right Nothing)
            | otherwise -> pure (Right (Just entry))

storeModelsCache
    :: FilePath
    -> ModelsCacheEntry
    -> IO (Either ModelsCacheError ())
storeModelsCache path entry =
    try @_ @SomeException do
        createDirectoryIfMissing True (takeDirectory path)
        writeLazyFileAtomically
            (unsafeEncodeUtf path)
            (ownerReadMode `unionFileModes` ownerWriteMode)
            (Aeson.encode entry)
    >>= \case
        Left err -> pure (Left (cacheError err))
        Right () -> pure (Right ())

refreshModelsCacheTtl
    :: UTCTime
    -> NominalDiffTime
    -> ModelsCacheKey
    -> Text
    -> FilePath
    -> IO (Either ModelsCacheError Bool)
refreshModelsCacheTtl now ttl expectedKey expectedVersion path =
    loadModelsCache path >>= \case
        Left err -> pure (Left err)
        Right Nothing -> pure (Right False)
        Right (Just entry)
            | entry.clientVersion /= Just expectedVersion ->
                pure (Right False)
            | entry.cacheKey /= Just expectedKey ->
                pure (Right False)
            | ttl > 0
                && diffUTCTime now entry.fetchedAt <= ttl / 2 ->
                pure (Right True)
            | otherwise ->
                fmap (fmap (const True)) $
                    storeModelsCache path entry { fetchedAt = now }

loadModelsCache
    :: FilePath
    -> IO (Either ModelsCacheError (Maybe ModelsCacheEntry))
loadModelsCache path =
    try @_ @SomeException do
        exists <- doesFileExist path
        if not exists
            then pure Nothing
            else do
                bytes <- LBS.readFile path
                case Aeson.eitherDecode bytes of
                    Left err -> ioError (userError err)
                    Right entry -> pure (Just entry)
    >>= \case
        Left err -> pure (Left (cacheError err))
        Right result -> pure (Right result)

cacheError :: Show error => error -> ModelsCacheError
cacheError = ModelsCacheError . Text.pack . show
