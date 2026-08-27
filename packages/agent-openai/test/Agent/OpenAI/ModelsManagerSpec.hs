module Agent.OpenAI.ModelsManagerSpec (spec) where

import Agent.Error (ApiError(..))
import Agent.OpenAI.Models
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import qualified Data.Aeson as Aeson
import Data.Bits ((.&.))
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import Data.Time.Clock
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (fileID, fileMode, getFileStatus)
import Test.Hspec

spec :: Spec
spec = do
    describe "model cache" do
        it "accepts fresh matching entries and rejects stale/version/key mismatches" do
            withSystemTempDirectory "models-cache" \directory -> do
                now <- getCurrentTime
                let path = directory </> "models_cache.json"
                    key = testCacheKey "account-a"
                    entry = cacheEntry now key "1.2.3" [testModel "cached" 0]
                storeModelsCache path entry `shouldReturn` Right ()
                mode <- fileMode <$> getFileStatus path
                mode .&. 0o777 `shouldBe` 0o600
                loadFreshModelsCache now 300 key "1.2.3" path
                    `shouldReturn` Right (Just entry)
                loadFreshModelsCache
                    now
                    300
                    key
                    "9.9.9"
                    path
                    `shouldReturn` Right Nothing
                loadFreshModelsCache
                    now
                    300
                    (testCacheKey "account-b")
                    "1.2.3"
                    path
                    `shouldReturn` Right Nothing
                let stale = entry { fetchedAt = addUTCTime (-301) now }
                storeModelsCache path stale `shouldReturn` Right ()
                loadFreshModelsCache now 300 key "1.2.3" path
                    `shouldReturn` Right Nothing
                let renewedAt = addUTCTime 600 now
                refreshModelsCacheTtl renewedAt 300 key "1.2.3" path
                    `shouldReturn` Right True
                renewed <- loadFreshModelsCache
                    renewedAt
                    300
                    key
                    "1.2.3"
                    path
                renewed `shouldBe` Right
                    (Just stale { fetchedAt = renewedAt })

        it "does not rewrite a cache entry during the first half of its TTL" do
            withSystemTempDirectory "models-cache" \directory -> do
                now <- getCurrentTime
                let path = directory </> "models_cache.json"
                    key = testCacheKey "account-a"
                    entry = cacheEntry now key "1.2.3" [testModel "cached" 0]
                    observedAt = addUTCTime 149 now
                storeModelsCache path entry `shouldReturn` Right ()
                inodeBefore <- fileID <$> getFileStatus path
                refreshModelsCacheTtl observedAt 300 key "1.2.3" path
                    `shouldReturn` Right True
                inodeAfter <- fileID <$> getFileStatus path
                inodeAfter `shouldBe` inodeBefore
                loadFreshModelsCache observedAt 300 key "1.2.3" path
                    `shouldReturn` Right (Just entry)

        it "treats corrupt cache contents as a recoverable cache error" do
            withSystemTempDirectory "models-cache" \directory -> do
                let path = directory </> "models_cache.json"
                LBS.writeFile path "{not-json"
                now <- getCurrentTime
                result <- loadFreshModelsCache
                    now
                    300
                    (testCacheKey "account-a")
                    "1.2.3"
                    path
                result `shouldSatisfy` \case
                    Left _ -> True
                    Right _ -> False

        it "treats a cache entry without a model snapshot as corrupt" do
            withSystemTempDirectory "models-cache" \directory -> do
                let path = directory </> "models_cache.json"
                LBS.writeFile path
                    "{\"fetched_at\":\"2026-08-23T12:00:00Z\"}"
                now <- getCurrentTime
                result <- loadFreshModelsCache
                    now
                    300
                    (testCacheKey "account-a")
                    "1.2.3"
                    path
                result `shouldSatisfy` \case
                    Left _ -> True
                    Right _ -> False

    describe "ModelsManager" do
        it "preserves an explicitly requested OpenAI model and otherwise uses catalog priority" do
            manager <- newModelsManagerWithBundled
                (catalogOf
                    [ testModel "provider-default" 0
                    , testModel "provider-secondary" 1
                    ])
                defaultModelsManagerOptions
            getDefaultModel manager (Just "custom/model")
                `shouldReturn` Just "custom/model"
            getDefaultModel manager Nothing
                `shouldReturn` Just "provider-default"

        it "uses a fresh cache for OnlineIfUncached without fetching" do
            withSystemTempDirectory "models-manager" \directory -> do
                calls <- newIORef (0 :: Int)
                now <- getCurrentTime
                let path = directory </> "models_cache.json"
                    key = testCacheKey "account-a"
                    cached = testModel "cached" 0
                    cachedExtras =
                        KeyMap.singleton
                            "catalog_generation"
                            (Aeson.toJSON (7 :: Int))
                _ <- storeModelsCache path
                    (cacheEntry now key "1.2.3" [cached])
                        { catalogExtraFields = cachedExtras }
                manager <- newModelsManagerWithBundled
                    (catalogOf [testModel "bundled" 10])
                    defaultModelsManagerOptions
                        { endpointClient = Just (countingEndpoint calls [])
                        , cachePath = Just path
                        , cacheKey = key
                        , clientVersion = "1.2.3"
                        }
                catalog <- refreshModelCatalog manager RefreshOnlineIfUncached
                map (.slug) catalog.models `shouldBe` ["cached"]
                catalog.extraFields `shouldBe` cachedExtras
                readIORef calls `shouldReturn` 0

        it "falls back to bundled models when the endpoint fails" do
            let bundled = catalogOf [testModel "bundled" 0]
                endpoint = testEndpoint True True \_ ->
                    pure (Left (ConnectionError "offline"))
            manager <- newModelsManagerWithBundled bundled
                defaultModelsManagerOptions
                    { endpointClient = Just endpoint
                    }
            catalog <- refreshModelCatalog manager RefreshOnline
            catalog `shouldBe` bundled

        it "uses visible remote models as authoritative when configured" do
            calls <- newIORef (0 :: Int)
            let remote = ModelsResponse
                    { models = [testModel "remote" 0]
                    , extraFields =
                        KeyMap.singleton
                            "catalog_generation"
                            (Aeson.toJSON (7 :: Int))
                    }
            manager <- newModelsManagerWithBundled
                (catalogOf [testModel "bundled" 10])
                defaultModelsManagerOptions
                    { endpointClient = Just (countingEndpoint
                        calls
                        [ ModelsFetched
                            { catalog = remote
                            , etag = Nothing
                            , cacheKey = testCacheKey "account-a"
                            }
                        ])
                    , remoteCatalogAuthoritative = True
                    }
            catalog <- refreshModelCatalog manager RefreshOnline
            map (.slug) catalog.models `shouldBe` ["remote"]
            catalog.extraFields `shouldBe` remote.extraFields

        it "merges visible remote models when the endpoint is not using ChatGPT auth" do
            calls <- newIORef (0 :: Int)
            let remote = catalogOf [testModel "api-remote" 0]
                endpoint =
                    testEndpoint True False \_ -> do
                        atomicModifyIORef' calls \count -> (count + 1, ())
                        pure $ Right ModelsFetched
                            { catalog = remote
                            , etag = Nothing
                            , cacheKey = testCacheKey "api-key"
                            }
            manager <- newModelsManagerWithBundled
                (catalogOf [testModel "bundled" 10])
                defaultModelsManagerOptions
                    { endpointClient = Just endpoint
                    , remoteCatalogAuthoritative = True
                    }
            catalog <- refreshModelCatalog manager RefreshOnline
            map (.slug) catalog.models
                `shouldBe` ["bundled", "api-remote"]
            readIORef calls `shouldReturn` 1

        it "skips remote refresh when the credential source cannot use the Codex backend" do
            calls <- newIORef (0 :: Int)
            let endpoint =
                    testEndpoint False False \_ -> do
                        atomicModifyIORef' calls \count -> (count + 1, ())
                        pure $ Right ModelsFetched
                            { catalog = catalogOf [testModel "remote" 0]
                            , etag = Nothing
                            , cacheKey = testCacheKey "api-key"
                            }
                bundled = catalogOf [testModel "bundled" 10]
            manager <- newModelsManagerWithBundled bundled
                defaultModelsManagerOptions
                    { endpointClient = Just endpoint
                    }
            refreshModelCatalog manager RefreshOnline `shouldReturn` bundled
            readIORef calls `shouldReturn` 0

        it "overlays hidden remote models onto the bundled fallback" do
            let bundledModel = testModel "bundled" 10
                hiddenRemote =
                    (testModel "remote-hidden" 0)
                        { visibility = ModelVisibilityHide }
            calls <- newIORef (0 :: Int)
            manager <- newModelsManagerWithBundled
                (catalogOf [bundledModel])
                defaultModelsManagerOptions
                    { endpointClient = Just (countingEndpoint calls
                        [ ModelsFetched
                            { catalog = catalogOf [hiddenRemote]
                            , etag = Nothing
                            , cacheKey = testCacheKey "account-a"
                            }
                        ])
                    , remoteCatalogAuthoritative = True
                    }
            catalog <- refreshModelCatalog manager RefreshOnline
            map (.slug) catalog.models
                `shouldBe` ["bundled", "remote-hidden"]

        it "deduplicates concurrent refreshes for a changed ETag" do
            calls <- newIORef (0 :: Int)
            responses <- newIORef
                [ ModelsFetched
                    { catalog = catalogOf [testModel "old" 0]
                    , etag = Just "\"etag-1\""
                    , cacheKey = testCacheKey "account-a"
                    }
                , ModelsFetched
                    { catalog = catalogOf [testModel "new" 0]
                    , etag = Just "\"etag-2\""
                    , cacheKey = testCacheKey "account-a"
                    }
                ]
            let endpoint = testEndpoint True True \_ -> do
                    atomicModifyIORef' calls \count -> (count + 1, ())
                    threadDelay 10_000
                    atomicModifyIORef' responses \case
                        response : rest -> (rest, Right response)
                        [] ->
                            ( []
                            , Right ModelsNotModified
                                { etag = Just "\"etag-2\""
                                , cacheKey = testCacheKey "account-a"
                                }
                            )
            manager <- newModelsManagerWithBundled
                (catalogOf [testModel "bundled" 10])
                defaultModelsManagerOptions
                    { endpointClient = Just endpoint
                    }
            _ <- refreshModelCatalog manager RefreshOnline
            _ <- mapConcurrently
                (const (refreshIfNewEtag manager "\"etag-2\""))
                [1 .. 8 :: Int]
            readIORef calls `shouldReturn` 2
            (map (.slug) . (.models)) <$> currentModelCatalog manager
                `shouldReturn` ["new"]
            getCurrentEtag manager `shouldReturn` Just "\"etag-2\""

        it "renews cache freshness without fetching when the ETag is unchanged" do
            withSystemTempDirectory "models-manager" \directory -> do
                calls <- newIORef (0 :: Int)
                let path = directory </> "models_cache.json"
                    key = testCacheKey "account-a"
                    endpoint = countingEndpoint calls
                        [ ModelsFetched
                            { catalog = catalogOf [testModel "remote" 0]
                            , etag = Just "\"etag-1\""
                            , cacheKey = key
                            }
                        ]
                manager <- newModelsManagerWithBundled
                    (catalogOf [testModel "bundled" 10])
                    defaultModelsManagerOptions
                        { endpointClient = Just endpoint
                        , cachePath = Just path
                        , cacheKey = key
                        , clientVersion = "1.2.3"
                        }
                _ <- refreshModelCatalog manager RefreshOnline
                before <- getCurrentTime
                let staleAt = addUTCTime (-301) before
                    staleEntry = ModelsCacheEntry
                        { fetchedAt = staleAt
                        , etag = Just "\"etag-1\""
                        , clientVersion = Just "1.2.3"
                        , cacheKey = Just key
                        , models = [testModel "remote" 0]
                        , catalogExtraFields = KeyMap.empty
                        }
                storeModelsCache path staleEntry `shouldReturn` Right ()
                _ <- refreshIfNewEtag manager "\"etag-1\""
                readIORef calls `shouldReturn` 1
                cached <- loadFreshModelsCache
                    before
                    300
                    key
                    "1.2.3"
                    path
                cached `shouldSatisfy` \case
                    Right (Just _) -> True
                    _ -> False

        it "stores the cache under the account that actually fetched the catalog" do
            withSystemTempDirectory "models-manager" \directory -> do
                let path = directory </> "models_cache.json"
                    configuredKey = testCacheKey "account-a"
                    fetchedKey = testCacheKey "account-b"
                    endpoint =
                        testEndpoint True True \_ ->
                            pure $ Right ModelsFetched
                                { catalog = catalogOf [testModel "remote" 0]
                                , etag = Just "\"etag-b\""
                                , cacheKey = fetchedKey
                                }
                manager <- newModelsManagerWithBundled
                    (catalogOf [testModel "bundled" 10])
                    defaultModelsManagerOptions
                        { endpointClient = Just endpoint
                        , cachePath = Just path
                        , cacheKey = configuredKey
                        , clientVersion = "1.2.3"
                        }
                _ <- refreshModelCatalog manager RefreshOnline
                now <- getCurrentTime
                fetched <- loadFreshModelsCache
                    now
                    300
                    fetchedKey
                    "1.2.3"
                    path
                fetched `shouldSatisfy` \case
                        Right (Just entry) ->
                            map (.slug) entry.models == ["remote"]
                                && entry.etag == Just "\"etag-b\""
                        _ -> False
                loadFreshModelsCache now 300 configuredKey "1.2.3" path
                    `shouldReturn` Right Nothing

countingEndpoint
    :: IORef Int
    -> [ModelsEndpointResponse]
    -> ModelsEndpointClient
countingEndpoint calls initialResponses =
    testEndpoint True True \_ -> do
        atomicModifyIORef' calls \count -> (count + 1, ())
        pure $ Right $ case initialResponses of
            response : _ -> response
            [] -> ModelsNotModified
                { etag = Nothing
                , cacheKey = testCacheKey "account-a"
                }

testEndpoint
    :: Bool
    -> Bool
    -> ( Maybe ModelsFetchCondition
        -> IO (Either ApiError ModelsEndpointResponse)
       )
    -> ModelsEndpointClient
testEndpoint allowsRemoteRefresh usesChatGptAuth fetchModels =
    ModelsEndpointClient
    { fetchModels
    , allowsRemoteRefresh
    , usesChatGptAuth
    }

testCacheKey :: Text -> ModelsCacheKey
testCacheKey account = ModelsCacheKey
    { providerId = "openai"
    , baseUrl = "https://example.test/codex"
    , accountId = Just account
    }

cacheEntry
    :: UTCTime
    -> ModelsCacheKey
    -> Text
    -> [ModelInfo]
    -> ModelsCacheEntry
cacheEntry fetchedAt cacheKey clientVersion models = ModelsCacheEntry
    { fetchedAt
    , etag = Just "\"etag\""
    , clientVersion = Just clientVersion
    , cacheKey = Just cacheKey
    , models
    , catalogExtraFields = KeyMap.empty
    }

catalogOf :: [ModelInfo] -> ModelsResponse
catalogOf models = ModelsResponse
    { models
    , extraFields = KeyMap.empty
    }

testModel :: Text -> Int -> ModelInfo
testModel slug priority =
    (fallbackModelInfo slug)
        { displayName = slug
        , visibility = ModelVisibilityList
        , priority
        , usedFallbackModelMetadata = False
        }
