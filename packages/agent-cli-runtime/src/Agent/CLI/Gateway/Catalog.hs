-- | Authorized model catalog and opaque connection-bound service access.
module Agent.CLI.Gateway.Catalog
    ( GatewayModel(..)
    , GatewayModelCatalogResponse(..)
    , GatewayModelProtocol(..)
    , GatewayModelAccess
    , newGatewayModelAccess
    , newGatewayModelAccessWith
    , newGatewayModelAccessWithDictation
    , newGatewayModelAccessWithUsage
    , refreshGatewayModels
    , cachedGatewayModels
    , gatewayModelIds
    , fetchGatewayModels
    , fetchGatewayUsage
    , transcribeGatewayPcm
    ) where

import Agent.CLI.Gateway.Credentials (validateGatewayCredential)
import Agent.CLI.Gateway.Dictation (transcribeGatewayPcmWith)
import Agent.CLI.Gateway.Usage (fetchGatewayUsageWithCredential)
import Agent.OpenAI.Usage (UsageSnapshot)
import Agent.Server.Client.GatewayIdentity (GatewayCredential(..))
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception.Safe (tryAny)
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isPrint, isSpace)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
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

data GatewayModel = GatewayModel
    { gatewayModelId :: !Text
    , gatewayModelProtocol :: !GatewayModelProtocol
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
        Aeson.withObject "GatewayModel" \object ->
            GatewayModel
                <$> object .: "id"
                <*> object .: "protocol"

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
    { gatewayModelFetch :: !(IO (Either Text [GatewayModel]))
    , gatewayUsageFetch :: !(Text -> IO (Either Text UsageSnapshot))
    , gatewayModelCache :: !(IORef (Maybe [GatewayModel]))
    , gatewayModelRefreshLock :: !(MVar ())
    , gatewayDictation
        :: !(((BS.ByteString -> IO ()) -> IO ())
            -> (Text -> IO ())
            -> IO (Either Text Text))
    }

-- | Construct a cached model-list handle for a validated gateway credential.
newGatewayModelAccess :: GatewayCredential -> IO GatewayModelAccess
newGatewayModelAccess credential =
    newGatewayModelAccessWithActions
        (fetchGatewayModels credential)
        (fetchGatewayUsageWithCredential credential)
        (transcribeGatewayPcmWith credential)

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
        fetch
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
        fetch
        unavailableGatewayUsage
        dictation

newGatewayModelAccessWithActions
    :: IO (Either Text [GatewayModel])
    -> (Text -> IO (Either Text UsageSnapshot))
    -> (((BS.ByteString -> IO ()) -> IO ())
        -> (Text -> IO ())
        -> IO (Either Text Text))
    -> IO GatewayModelAccess
newGatewayModelAccessWithActions fetch usage dictation = do
    cache <- newIORef Nothing
    refreshLock <- newMVar ()
    pure GatewayModelAccess
        { gatewayModelFetch = fetch
        , gatewayUsageFetch = usage
        , gatewayModelCache = cache
        , gatewayModelRefreshLock = refreshLock
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
            Right result -> pure result

-- | Record PCM through the opaque, gateway-bound dictation action.
transcribeGatewayPcm
    :: GatewayModelAccess
    -> ((BS.ByteString -> IO ()) -> IO ())
    -> (Text -> IO ())
    -> IO (Either Text Text)
transcribeGatewayPcm access = access.gatewayDictation

-- | Refresh the gateway's authorized model aliases.
--
-- A failed refresh deliberately clears the previous value.  Continuing to
-- show a stale authorization list would let an organization revocation look
-- like an available model.
refreshGatewayModels
    :: GatewayModelAccess
    -> IO (Either Text [GatewayModel])
refreshGatewayModels
        GatewayModelAccess
            { gatewayModelFetch
            , gatewayModelCache
            , gatewayModelRefreshLock
            } =
    withMVar gatewayModelRefreshLock \_ ->
        tryAny gatewayModelFetch >>= \case
            Left _ -> do
                writeIORef gatewayModelCache Nothing
                pure (Left "Could not refresh organization gateway models.")
            Right result ->
                case result of
                    Left err -> do
                        writeIORef gatewayModelCache Nothing
                        pure (Left err)
                    Right models -> do
                        let normalized = normalizeGatewayModels models
                        writeIORef gatewayModelCache (Just normalized)
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
    case validateGatewayCredential credential of
        Left _ -> pure (Left "Gateway credential is invalid.")
        Right () -> do
            response <- tryAny do
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
                    Left "Could not reach the gateway models endpoint."
                Right value
                    | statusIsSuccessful (HTTP.responseStatus value) ->
                        case
                            Aeson.eitherDecodeStrict'
                                (LBS.toStrict (HTTP.responseBody value))
                                :: Either String GatewayModelCatalogResponse
                            of
                            Left _ ->
                                Left
                                    "Gateway returned an unreadable models response."
                            Right catalog
                                | null catalog.gatewayModelCatalogData ->
                                    Left "Gateway returned an empty model catalog."
                                | otherwise ->
                                    Right catalog.gatewayModelCatalogData
                    | otherwise ->
                        Left $
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
