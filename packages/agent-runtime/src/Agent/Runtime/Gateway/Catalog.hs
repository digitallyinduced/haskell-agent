-- | Authorized model catalog and opaque connection-bound service access.
module Agent.Runtime.Gateway.Catalog
    ( GatewayModel(..)
    , GatewayModelCatalogResponse(..)
    , GatewayModelProtocol(..)
    , GatewayModelProvider(..)
    , GatewayModelAccess
    , newGatewayModelAccess
    , newGatewayModelAccessWithStore
    , newGatewayModelAccessWithStoreAndFetch
    , GatewayModelFetchFailure(..)
    , newGatewayModelAccessWithStoreAndResult
    , newGatewayModelAccessWith
    , newGatewayModelAccessWithDictation
    , newGatewayModelAccessWithUsage
    , refreshGatewayModels
    , cachedGatewayModels
    , gatewayModelIds
    , fetchGatewayModels
    , fetchGatewayUsage
    , cachedGatewayUsage
    , transcribeGatewayPcm
    ) where

import Agent.Accounts.Gateway.Credentials (validateGatewayCredential)
import Agent.Runtime.Gateway.Dictation (transcribeGatewayPcmWith)
import Agent.Runtime.Gateway.Usage (fetchGatewayUsageWithCredential)
import Agent.ClientIdentity (gatewayUserAgent)
import Agent.OpenAI.Usage (UsageSnapshot)
import Agent.Server.Client.GatewayIdentity (GatewayCredential(..), gatewayCredentialIdentity)
import Agent.Store.Postgres.Connection (StorePool)
import Agent.Store.Postgres.ModelCatalogCache qualified as Store
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception.Safe (tryAny)
import Control.Monad (void)
import Data.Aeson ((.:), (.=))
import Data.Bifunctor (first)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isPrint, isSpace)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef')
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types
    ( hAccept
    , hAuthorization
    , statusCode
    , statusIsSuccessful
    )

data GatewayModelProtocol
    = GatewayResponsesProtocol
    | GatewayAnthropicProtocol
    deriving (Eq, Show)

-- | Provider identity is independent of the shared Responses wire protocol.
data GatewayModelProvider
    = GatewayOpenAIProvider
    | GatewayXAIProvider
    | GatewayAnthropicProvider
    deriving (Eq, Show)

data GatewayModel = GatewayModel
    { gatewayModelId :: !Text
    , gatewayModelProtocol :: !GatewayModelProtocol
    , gatewayModelProvider :: !GatewayModelProvider
    }
    deriving (Eq, Show)

newtype GatewayModelCatalogResponse = GatewayModelCatalogResponse
    { gatewayModelCatalogData :: [GatewayModel]
    }
    deriving (Eq, Show)

instance Aeson.FromJSON GatewayModelCatalogResponse where
    parseJSON =
        Aeson.withObject "GatewayModelCatalogResponse" \object ->
            GatewayModelCatalogResponse
                . normalizeGatewayModels
                <$> object .: "data"

instance Aeson.FromJSON GatewayModel where
    parseJSON =
        Aeson.withObject "GatewayModel" \object -> do
            modelId <- object .: "id"
            protocol <- object .: "protocol"
            provider <- object .: "provider"
            if compatibleGatewayProtocol provider protocol
                then pure (GatewayModel modelId protocol provider)
                else fail "Gateway model provider and protocol are incompatible."

instance Aeson.ToJSON GatewayModel where
    toJSON model = Aeson.object
        [ "id" .= model.gatewayModelId
        , "protocol" .= (case model.gatewayModelProtocol of
            GatewayResponsesProtocol -> "responses" :: Text
            GatewayAnthropicProtocol -> "anthropic")
        , "provider" .= (case model.gatewayModelProvider of
            GatewayOpenAIProvider -> "openai" :: Text
            GatewayXAIProvider -> "xai"
            GatewayAnthropicProvider -> "anthropic")
        ]

instance Aeson.FromJSON GatewayModelProvider where
    parseJSON =
        Aeson.withText "GatewayModelProvider" \case
            "openai" -> pure GatewayOpenAIProvider
            "xai" -> pure GatewayXAIProvider
            "anthropic" -> pure GatewayAnthropicProvider
            _ -> fail "Gateway model provider is invalid."

compatibleGatewayProtocol :: GatewayModelProvider -> GatewayModelProtocol -> Bool
compatibleGatewayProtocol provider protocol =
    protocol == case provider of
        GatewayOpenAIProvider -> GatewayResponsesProtocol
        GatewayXAIProvider -> GatewayResponsesProtocol
        GatewayAnthropicProvider -> GatewayAnthropicProtocol

instance Aeson.FromJSON GatewayModelProtocol where
    parseJSON =
        Aeson.withText "GatewayModelProtocol" \case
            "responses" -> pure GatewayResponsesProtocol
            "anthropic" -> pure GatewayAnthropicProtocol
            _ -> fail "Gateway model protocol is invalid."

-- | A gateway-scoped model catalog and its most recently successful refresh.
--
-- The constructor is deliberately hidden: callers can list models but cannot
-- accidentally inspect or log the credential captured by its fetch action.
data GatewayModelAccess = GatewayModelAccess
    { gatewayModelFetch :: !(IO (Either GatewayModelFetchFailure [GatewayModel]))
    , gatewayUsageFetch :: !(Text -> IO (Either Text UsageSnapshot))
    , gatewayUsageCache :: !(IORef (Map.Map Text UsageSnapshot))
    , gatewayModelCache :: !(IORef (Maybe [GatewayModel]))
    , gatewayModelRefreshLock :: !(MVar ())
    , gatewayModelPersist :: !(Maybe [GatewayModel] -> IO ())
    , gatewayDictation
        :: !(((BS.ByteString -> IO ()) -> IO ())
            -> (Text -> IO ())
            -> IO (Either Text Text))
    }

data GatewayModelFetchFailure
    = GatewayModelAuthorizationRejected !Text
    | GatewayModelRefreshFailed !Text
    deriving (Eq, Show)

gatewayModelFailureMessage :: GatewayModelFetchFailure -> Text
gatewayModelFailureMessage = \case
    GatewayModelAuthorizationRejected message -> message
    GatewayModelRefreshFailed message -> message

-- | Construct a cached model-list handle for a validated gateway credential.
newGatewayModelAccess :: GatewayCredential -> IO GatewayModelAccess
newGatewayModelAccess credential =
    newGatewayModelAccessWithActions
        (fetchGatewayModelsResult credential)
        (fetchGatewayUsageWithCredential credential)
        (transcribeGatewayPcmWith credential)

-- | Hydrate presentation data without contacting the gateway. The exact
-- credential identity prevents cache reuse across accounts or organizations,
-- including when a bearer is replaced at the same endpoint.
newGatewayModelAccessWithStore :: StorePool -> GatewayCredential -> IO GatewayModelAccess
newGatewayModelAccessWithStore pool credential =
    newGatewayModelAccess credential >>= attachGatewayModelStore pool credential

-- | Injectable persistent catalog transport. A fresh handle is always created:
-- an existing credential-bound handle cannot be rebound to another cache key.
newGatewayModelAccessWithStoreAndFetch
    :: StorePool
    -> GatewayCredential
    -> IO (Either Text [GatewayModel])
    -> IO GatewayModelAccess
newGatewayModelAccessWithStoreAndFetch pool credential fetch =
    newGatewayModelAccessWith fetch >>= attachGatewayModelStore pool credential

-- | Trusted typed transport injection for authorization-invalidation tests.
newGatewayModelAccessWithStoreAndResult
    :: StorePool
    -> GatewayCredential
    -> IO (Either GatewayModelFetchFailure [GatewayModel])
    -> IO GatewayModelAccess
newGatewayModelAccessWithStoreAndResult pool credential fetch =
    newGatewayModelAccessWithActions fetch unavailableGatewayUsage unavailableGatewayDictation
        >>= attachGatewayModelStore pool credential

attachGatewayModelStore :: StorePool -> GatewayCredential -> GatewayModelAccess -> IO GatewayModelAccess
attachGatewayModelStore pool credential access = do
    let identity = gatewayCredentialIdentity credential
    stored <- tryAny (Store.loadModelCatalogCache pool identity)
    let models = case stored of
            Right (Right (Just payload)) ->
                normalizeGatewayModels <$> Aeson.decodeStrict' (TextEncoding.encodeUtf8 payload)
            _ -> Nothing
        persist value = void $ tryAny $ case value of
            Nothing -> Store.deleteModelCatalogCache pool identity
            Just catalog ->
                Store.upsertModelCatalogCache pool identity
                    (TextEncoding.decodeUtf8 (LBS.toStrict (Aeson.encode catalog)))
    writeIORef access.gatewayModelCache models
    pure access { gatewayModelPersist = persist }

-- | Injectable constructor used by tests and alternative trusted transports.
-- The resulting value remains opaque, so the fetch action cannot be read back
-- or accidentally included in diagnostics.
newGatewayModelAccessWith
    :: IO (Either Text [GatewayModel])
    -> IO GatewayModelAccess
newGatewayModelAccessWith fetch =
    newGatewayModelAccessWithUsage
        fetch
        unavailableGatewayUsage

-- | Injectable usage transport used by tests and trusted alternative
-- gateways. Model aliases are passed through exactly and the decoded snapshot
-- uses the same type as a direct OpenAI connection.
newGatewayModelAccessWithUsage
    :: IO (Either Text [GatewayModel])
    -> (Text -> IO (Either Text UsageSnapshot))
    -> IO GatewayModelAccess
newGatewayModelAccessWithUsage fetch usage =
    newGatewayModelAccessWithActions
        (first GatewayModelRefreshFailed <$> fetch)
        usage
        unavailableGatewayDictation

-- | Injectable constructor for tests and trusted alternative gateway
-- transports. The action stays opaque with the credential-bearing model
-- access handle.
newGatewayModelAccessWithDictation
    :: IO (Either Text [GatewayModel])
    -> (((BS.ByteString -> IO ()) -> IO ())
        -> (Text -> IO ())
        -> IO (Either Text Text))
    -> IO GatewayModelAccess
newGatewayModelAccessWithDictation fetch dictation =
    newGatewayModelAccessWithActions
        (first GatewayModelRefreshFailed <$> fetch)
        unavailableGatewayUsage
        dictation

newGatewayModelAccessWithActions
    :: IO (Either GatewayModelFetchFailure [GatewayModel])
    -> (Text -> IO (Either Text UsageSnapshot))
    -> (((BS.ByteString -> IO ()) -> IO ())
        -> (Text -> IO ())
        -> IO (Either Text Text))
    -> IO GatewayModelAccess
newGatewayModelAccessWithActions fetch usage dictation = do
    cache <- newIORef Nothing
    usageCache <- newIORef Map.empty
    refreshLock <- newMVar ()
    pure GatewayModelAccess
        { gatewayModelFetch = fetch
        , gatewayUsageFetch = usage
        , gatewayUsageCache = usageCache
        , gatewayModelCache = cache
        , gatewayModelRefreshLock = refreshLock
        , gatewayModelPersist = \_ -> pure ()
        , gatewayDictation = dictation
        }

unavailableGatewayUsage
    :: Text
    -> IO (Either Text UsageSnapshot)
unavailableGatewayUsage _ =
    pure $
        Left "Usage is not available through this gateway connection."

unavailableGatewayDictation
    :: ((BS.ByteString -> IO ()) -> IO ())
    -> (Text -> IO ())
    -> IO (Either Text Text)
unavailableGatewayDictation _ _ =
    pure $
        Left "Dictation is not available through this gateway connection."

-- | Fetch usage through the transport captured by this gateway connection.
fetchGatewayUsage
    :: GatewayModelAccess
    -> Text
    -> IO (Either Text UsageSnapshot)
fetchGatewayUsage access model
    | Text.null model =
        pure (Left "Gateway usage requires a model alias.")
    | otherwise =
        tryAny (access.gatewayUsageFetch model) >>= \case
            Left _ ->
                pure (Left "Could not refresh organization gateway usage.")
            Right result -> do
                case result of
                    Left _ -> pure ()
                    Right snapshot ->
                        atomicModifyIORef' access.gatewayUsageCache \cached ->
                            (Map.insert model snapshot cached, ())
                pure result

-- | Last successful presentation snapshot for this exact connection and alias.
cachedGatewayUsage :: GatewayModelAccess -> Text -> IO (Maybe UsageSnapshot)
cachedGatewayUsage access model =
    Map.lookup model <$> readIORef access.gatewayUsageCache

-- | Record PCM through the opaque, gateway-bound dictation action.
transcribeGatewayPcm
    :: GatewayModelAccess
    -> ((BS.ByteString -> IO ()) -> IO ())
    -> (Text -> IO ())
    -> IO (Either Text Text)
transcribeGatewayPcm access = access.gatewayDictation

-- | Refresh the gateway's authorized model aliases.
--
-- Cached lists are presentation data, never authorization. Transient failures
-- retain the last successful list; explicit authentication rejection clears it.
refreshGatewayModels
    :: GatewayModelAccess
    -> IO (Either Text [GatewayModel])
refreshGatewayModels
        GatewayModelAccess
            { gatewayModelFetch
            , gatewayModelCache
            , gatewayModelRefreshLock
            , gatewayModelPersist
            } =
    withMVar gatewayModelRefreshLock \_ ->
        tryAny gatewayModelFetch >>= \case
            Left _ ->
                pure (Left "Could not refresh organization gateway models.")
            Right result ->
                case result of
                    Left err -> do
                        case err of
                            GatewayModelAuthorizationRejected _ -> do
                                writeIORef gatewayModelCache Nothing
                                gatewayModelPersist Nothing
                            GatewayModelRefreshFailed _ -> pure ()
                        pure (Left (gatewayModelFailureMessage err))
                    Right models -> do
                        let normalized = normalizeGatewayModels models
                        writeIORef gatewayModelCache (Just normalized)
                        gatewayModelPersist (Just normalized)
                        pure (Right normalized)

-- | Read the most recent successful gateway refresh without issuing I/O.
cachedGatewayModels :: GatewayModelAccess -> IO (Maybe [GatewayModel])
cachedGatewayModels GatewayModelAccess { gatewayModelCache } =
    readIORef gatewayModelCache

-- | Fetch the aliases currently offered to this gateway credential.
--
-- Errors deliberately omit exception and response-body detail: both may
-- contain external content, while request headers contain the bearer token.
fetchGatewayModels :: GatewayCredential -> IO (Either Text [GatewayModel])
fetchGatewayModels credential =
    first gatewayModelFailureMessage <$> fetchGatewayModelsResult credential

fetchGatewayModelsResult
    :: GatewayCredential
    -> IO (Either GatewayModelFetchFailure [GatewayModel])
fetchGatewayModelsResult credential =
    case validateGatewayCredential credential of
        Left _ -> pure (Left (GatewayModelAuthorizationRejected "Gateway credential is invalid."))
        Right () -> do
            response <- tryAny do
                userAgent <- gatewayUserAgent
                manager <- newTlsManager
                initial <-
                    HTTP.parseRequest
                        (Text.unpack
                            (Text.dropWhileEnd (== '/')
                                (Text.strip credential.gatewayBaseUrl)
                                <> "/v1/models"))
                HTTP.httpLbs
                    initial
                        { HTTP.method = "GET"
                        , HTTP.requestHeaders =
                            [ ( hAuthorization
                              , "Bearer "
                                    <> TextEncoding.encodeUtf8
                                        credential.gatewayAccessToken
                              )
                            , (hAccept, "application/json")
                            , ("User-Agent", userAgent)
                            ]
                        , HTTP.checkResponse = \_ _ -> pure ()
                        -- Never follow a redirect with the gateway bearer.
                        , HTTP.redirectCount = 0
                        , HTTP.responseTimeout =
                            HTTP.responseTimeoutMicro (5 * 1_000_000)
                        }
                    manager
            pure case response of
                Left _ ->
                    Left (GatewayModelRefreshFailed "Could not reach the gateway models endpoint.")
                Right value
                    | statusIsSuccessful (HTTP.responseStatus value) ->
                        case
                            Aeson.eitherDecodeStrict'
                                (LBS.toStrict (HTTP.responseBody value))
                                :: Either String GatewayModelCatalogResponse
                            of
                            Left _ ->
                                Left (GatewayModelRefreshFailed
                                    "Gateway returned an unreadable models response.")
                            Right catalog -> Right catalog.gatewayModelCatalogData
                    | otherwise ->
                        let code = statusCode (HTTP.responseStatus value)
                            failure = if code == 401 || code == 403
                                then GatewayModelAuthorizationRejected
                                else GatewayModelRefreshFailed
                        in Left $ failure $
                            "Gateway models returned HTTP "
                                <> Text.pack
                                    (show
                                        (statusCode
                                            (HTTP.responseStatus value)))

gatewayModelIds :: [GatewayModel] -> [Text]
gatewayModelIds = fmap (.gatewayModelId)

normalizeGatewayModels :: [GatewayModel] -> [GatewayModel]
normalizeGatewayModels = go Set.empty
  where
    go _ [] = []
    go seen (model : rest)
        | Text.null modelId = go seen rest
        | Text.any (\char -> isSpace char || not (isPrint char)) modelId =
            go seen rest
        | modelId `Set.member` seen = go seen rest
        | otherwise =
            model { gatewayModelId = modelId }
                : go (Set.insert modelId seen) rest
      where
        modelId = Text.strip model.gatewayModelId
