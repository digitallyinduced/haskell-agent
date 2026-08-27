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
import qualified Agent.Json.Decode as Json
import Agent.OpenAI.Models.Types (ModelInfo, modelInfoDecoder)
import Control.Monad (join)
import Control.Exception.Safe (SomeException, try)
import Data.Aeson (Object)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
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
                case Json.decodeEither modelsCacheEntryDecoder (LBS.toStrict bytes) of
                    Left err -> ioError
                        (userError
                            (Text.unpack (Json.jsonErrorMessage err)))
                    Right entry -> pure (Just entry)
    >>= \case
        Left err -> pure (Left (cacheError err))
        Right result -> pure (Right result)

cacheError :: Show error => error -> ModelsCacheError
cacheError = ModelsCacheError . Text.pack . show

modelsCacheEntryDecoder :: Json.Decoder ModelsCacheEntry
modelsCacheEntryDecoder = Json.object do
    fetchedAt <- Json.atKey "fetched_at" Json.utcTime
    etag <- optionalField "etag" Json.text
    clientVersion <- optionalField "client_version" Json.text
    cacheKey <- optionalField "cache_key" modelsCacheKeyDecoder
    models <- Json.atKey "models" (Json.list modelInfoDecoder)
    catalogExtraFields <-
        maybe mempty id <$> Json.atKeyOptional
            "catalog_extra_fields"
            catalogExtraFieldsDecoder
    pure ModelsCacheEntry
        { .. }

modelsCacheKeyDecoder :: Json.Decoder ModelsCacheKey
modelsCacheKeyDecoder = Json.object $
    ModelsCacheKey
        <$> defaultField "provider_id" Json.text ""
        <*> defaultField "base_url" Json.text ""
        <*> optionalField "account_id" Json.text

optionalField
    :: Text
    -> Json.Decoder value
    -> Json.FieldsDecoder (Maybe value)
optionalField key decoder =
    join <$> Json.atKeyOptional key (Json.nullable decoder)

defaultField
    :: Text
    -> Json.Decoder value
    -> value
    -> Json.FieldsDecoder value
defaultField key decoder fallback =
    maybe fallback id <$> optionalField key decoder

catalogExtraFieldsDecoder :: Json.Decoder Object
catalogExtraFieldsDecoder = Json.object do
    catalogGeneration <-
        optionalField "catalog_generation" Json.scientific
    pure $ maybe mempty
        (KeyMap.singleton "catalog_generation" . Aeson.Number)
        catalogGeneration
