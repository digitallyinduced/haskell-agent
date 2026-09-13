module Agent.OpenAI.ModelsManagerSpec (spec) where

import Agent.Error (ApiError(..))
import Agent.OpenAI.Models
import qualified Agent.OpenAI.Models.Manager.State as State
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently, withAsync, wait, cancel)
import Control.Concurrent.MVar
import Control.Exception.Safe (throwIO)
import Data.Bits ((.&.))
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Text (Text)
import Data.Time.Clock
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (fileID, fileMode, getFileStatus)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = do
    describe "pure model state" do
        it "tracks fetched, 304, and untagged replacement sequences with scoped conditions" do
            let bundled = catalogOf [testModel "bundled" 0]
                remote = catalogOf [testModel "remote" 1]
                keyA = testCacheKey "a"
                keyB = testCacheKey "b"
                initial = State.initialState bundled
                fetched = State.fetchedState True bundled remote (Just "v1") keyA
                renewed = State.notModifiedState fetched.etag Nothing keyB fetched
                retagged = State.notModifiedState renewed.etag (Just "v2") keyB renewed
                replaced = State.fetchedState False bundled remote Nothing keyA
            State.fetchCondition initial `shouldBe` Nothing
            State.fetchCondition fetched `shouldBe` Just (ModelsFetchCondition "v1" keyA)
            renewed.catalog `shouldBe` remote
            State.fetchCondition renewed `shouldBe` Just (ModelsFetchCondition "v1" keyB)
            State.fetchCondition retagged `shouldBe` Just (ModelsFetchCondition "v2" keyB)
            State.fetchCondition replaced `shouldBe` Nothing
            replaced.cacheKey `shouldBe` Just keyA
            map (.slug) replaced.catalog.models `shouldBe` ["bundled", "remote"]
            State.responseCacheAction (ModelsFetched remote Nothing keyA)
                `shouldBe` State.StoreCache keyA remote Nothing
            State.responseCacheAction (ModelsNotModified Nothing keyB)
                `shouldBe` State.TouchCache

        it "accepts cached metadata verbatim and applies the same fallback policy as fetching" do
            let bundled = catalogOf [testModel "bundled" 0]
                hidden = (testModel "hidden" 1) { visibility = ModelVisibilityHide }
                entry = (cacheEntry (read "2026-09-12 00:00:00 UTC")
                    (testCacheKey "a") "1" [hidden]) { cacheKey = Nothing, catalogGeneration = Just 9 }
                cached = State.cachedState True bundled entry
                fetched = State.fetchedState True bundled
                    (ModelsResponse entry.models entry.catalogGeneration) entry.etag (testCacheKey "a")
                renewed = State.notModifiedState cached.etag Nothing (testCacheKey "b") cached
            cached.catalog `shouldBe` fetched.catalog
            map (.slug) cached.catalog.models `shouldBe` ["bundled", "hidden"]
            cached.catalog.catalogGeneration `shouldBe` Just 9
            cached.cacheKey `shouldBe` Nothing
            State.fetchCondition cached `shouldBe` Nothing
            State.fetchCondition renewed `shouldBe` Just (ModelsFetchCondition "\"etag\"" (testCacheKey "b"))

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
        it "keeps bundled state with no endpoint for every strategy" do
            let bundled = catalogOf [testModel "bundled" 0]
            manager <- newModelsManagerWithBundled bundled defaultModelsManagerOptions
            mapM_ (\strategy -> refreshModelCatalogEither manager strategy `shouldReturn` Right bundled)
                [RefreshOnline, RefreshOffline, RefreshOnlineIfUncached]
            getCurrentEtag manager `shouldReturn` Nothing

        it "does not fetch on an offline miss but fetches on an online-if-uncached miss" do
            calls <- newIORef (0 :: Int)
            let bundled = catalogOf [testModel "bundled" 0]
                remote = catalogOf [testModel "remote" 1]
            manager <- newModelsManagerWithBundled bundled defaultModelsManagerOptions
                { endpointClient = Just (countingEndpoint calls
                    [ModelsFetched remote (Just "v1") (testCacheKey "a")]) }
            refreshModelCatalog manager RefreshOffline `shouldReturn` bundled
            readIORef calls `shouldReturn` 0
            refreshModelCatalog manager RefreshOnlineIfUncached `shouldReturn` remote
            readIORef calls `shouldReturn` 1

        it "retains committed state on endpoint errors and exceptions and releases the refresh lock" do
            calls <- newIORef (0 :: Int)
            let remote = catalogOf [testModel "remote" 0]
                key = testCacheKey "a"
                endpoint = testEndpoint True True \condition -> do
                    n <- atomicModifyIORef' calls \n -> (n + 1, n)
                    if n == 0 then pure (Right (ModelsFetched remote (Just "v1") key))
                    else do
                        condition `shouldBe` Just (ModelsFetchCondition "v1" key)
                        if n == 1 then pure (Left (ConnectionError "offline"))
                        else if n == 2 then throwIO (userError "fetch failed")
                        else pure (Right (ModelsNotModified Nothing key))
            manager <- newModelsManagerWithBundled (catalogOf []) defaultModelsManagerOptions
                { endpointClient = Just endpoint }
            refreshModelCatalog manager RefreshOnline `shouldReturn` remote
            refreshModelCatalogEither manager RefreshOnline
                `shouldReturn` Left (ConnectionError "offline")
            refreshModelCatalog manager RefreshOnline `shouldThrow` anyIOException
            currentModelCatalog manager `shouldReturn` remote
            getCurrentEtag manager `shouldReturn` Just "v1"
            timeout 1_000_000 (refreshModelCatalog manager RefreshOnline)
                `shouldReturn` Just remote

        it "commits before a cache-directory exception and can refresh again afterwards" do
            withSystemTempDirectory "models-manager" \directory -> do
                let blocked = directory </> "file"
                    remote = catalogOf [testModel "remote" 0]
                    key = testCacheKey "a"
                writeFile blocked "not a directory"
                manager <- newModelsManagerWithBundled (catalogOf []) defaultModelsManagerOptions
                    { endpointClient = Just (testEndpoint True True
                        (\_ -> pure (Right (ModelsFetched remote (Just "v1") key))))
                    , cachePath = Just (blocked </> "cache.json")
                    }
                refreshModelCatalog manager RefreshOnline `shouldThrow` anyIOException
                currentModelCatalog manager `shouldReturn` remote
                getCurrentEtag manager `shouldReturn` Just "v1"
                -- The unchanged-ETag touch tolerates cache IO errors; it does not store.
                timeout 1_000_000 (refreshIfNewEtag manager "v1") `shouldReturn` Just remote

        it "stores only remote data then touches a 304 cache without replacing its ETag" do
            withSystemTempDirectory "models-manager" \directory -> do
                calls <- newIORef (0 :: Int)
                let path = directory </> "cache.json"
                    key = testCacheKey "a"
                    bundled = catalogOf [testModel "bundled" 0]
                    remote = catalogOf [testModel "remote" 1]
                    endpoint = testEndpoint True True \condition -> do
                        n <- atomicModifyIORef' calls \n -> (n + 1, n)
                        if n == 0 then pure (Right (ModelsFetched remote (Just "v1") key))
                        else do
                            condition `shouldBe` Just (ModelsFetchCondition "v1" key)
                            pure (Right (ModelsNotModified (if n == 1 then Nothing else Just "v2") key))
                manager <- newModelsManagerWithBundled bundled defaultModelsManagerOptions
                    { endpointClient = Just endpoint, cachePath = Just path
                    , cacheKey = key, clientVersion = "1", remoteCatalogAuthoritative = False }
                merged <- refreshModelCatalog manager RefreshOnline
                map (.slug) merged.models `shouldBe` ["bundled", "remote"]
                now <- getCurrentTime
                disk <- loadFreshModelsCache now 300 key "1" path
                case disk of
                    Right (Just entry) -> do
                        entry.models `shouldBe` remote.models
                        entry.etag `shouldBe` Just "v1"
                        storeModelsCache path entry { fetchedAt = addUTCTime (-301) now }
                            `shouldReturn` Right ()
                    _ -> expectationFailure ("missing fetched cache: " <> show disk)
                refreshModelCatalog manager RefreshOnline `shouldReturn` merged
                getCurrentEtag manager `shouldReturn` Just "v1"
                touched <- loadFreshModelsCache now 300 key "1" path
                touched `shouldSatisfy` \case
                    Right (Just entry) -> entry.etag == Just "v1"
                        && entry.models == remote.models && entry.fetchedAt >= now
                    _ -> False
                refreshModelCatalog manager RefreshOnline `shouldReturn` merged
                getCurrentEtag manager `shouldReturn` Just "v2"
                loadFreshModelsCache now 300 key "1" path `shouldReturn` touched

        it "leaves reads available during a fetch and releases the lock after cancellation" do
            started <- newEmptyMVar
            blocked <- newEmptyMVar
            calls <- newIORef (0 :: Int)
            let bundled = catalogOf [testModel "bundled" 0]
                remote = catalogOf [testModel "remote" 0]
                endpoint = testEndpoint True True \_ -> do
                    n <- atomicModifyIORef' calls \n -> (n + 1, n)
                    if n == 0 then putMVar started () >> takeMVar blocked else pure ()
                    pure (Right (ModelsFetched remote (Just "v1") (testCacheKey "a")))
            manager <- newModelsManagerWithBundled bundled defaultModelsManagerOptions
                { endpointClient = Just endpoint }
            withAsync (refreshModelCatalog manager RefreshOnline) \first -> do
                timeout 1_000_000 (takeMVar started) `shouldReturn` Just ()
                timeout 1_000_000 (currentModelCatalog manager) `shouldReturn` Just bundled
                cancel first
            getCurrentEtag manager `shouldReturn` Nothing
            withAsync (refreshModelCatalog manager RefreshOnline) \second ->
                timeout 1_000_000 (wait second) `shouldReturn` Just remote

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
                _ <- storeModelsCache path
                    (cacheEntry now key "1.2.3" [cached])
                        { catalogGeneration = Just 7 }
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
                catalog.catalogGeneration `shouldBe` Just 7
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
                    , catalogGeneration = Just 7
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
            catalog.catalogGeneration `shouldBe` remote.catalogGeneration

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
                        , catalogGeneration = Nothing
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
    , catalogGeneration = Nothing
    }

catalogOf :: [ModelInfo] -> ModelsResponse
catalogOf models = ModelsResponse
    { models
    , catalogGeneration = Nothing
    }

testModel :: Text -> Int -> ModelInfo
testModel slug priority =
    (fallbackModelInfo slug)
        { displayName = slug
        , visibility = ModelVisibilityList
        , priority
        , usedFallbackModelMetadata = False
        }
