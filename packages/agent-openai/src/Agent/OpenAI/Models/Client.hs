-- | OpenAI-compatible @/models@ endpoint client.
module Agent.OpenAI.Models.Client
    ( ModelsFetchCondition(..)
    , ModelsEndpointResponse(..)
    , ModelsEndpointClient(..)
    , ModelsClientConfig(..)
    , defaultModelsBaseUrl
    , packageClientVersion
    , modelsEndpointClient
    , modelsCacheKeyForCredential
    , listModelsWithProviderAt
    , listModelsWithCredentialAt
    ) where

import Agent.Error (ApiError(..), ErrorType(..))
import qualified Agent.Json.Decode as Json
import Agent.OpenAI.Error (classifyHttpFailure)
import Agent.OpenAI.Models.Cache (ModelsCacheKey(..))
import Agent.OpenAI.Models.Types (ModelsResponse, modelsResponseDecoder)
import Agent.Provider
    ( BillingMode(..)
    , Credential(..)
    , Provider(..)
    , TokenProvider
    , runWithTokenProvider
    , tokenProviderBillingMode
    )
import Control.Applicative ((<|>))
import Control.Exception.Safe (tryAny)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text (lenientDecode)
import Data.Version (versionBranch)
import Network.HTTP.Simple
import qualified Network.HTTP.Client as HttpClient
import Paths_agent_openai (version)

-- | A conditional catalog request is only valid for the exact provider,
-- endpoint, and ChatGPT account that produced the cached ETag.
data ModelsFetchCondition = ModelsFetchCondition
    { etag :: !Text
    , cacheKey :: !ModelsCacheKey
    } deriving (Eq, Show)

data ModelsEndpointResponse
    = ModelsFetched
        { catalog :: !ModelsResponse
        , etag :: !(Maybe Text)
        , cacheKey :: !ModelsCacheKey
        }
    | ModelsNotModified
        { etag :: !(Maybe Text)
        , cacheKey :: !ModelsCacheKey
        }
    deriving (Eq, Show)

data ModelsEndpointClient = ModelsEndpointClient
    { fetchModels
        :: Maybe ModelsFetchCondition
        -> IO (Either ApiError ModelsEndpointResponse)
    -- | Whether the current credential source may access the Codex backend
    -- model catalog. A normal API-key provider must not probe this endpoint.
    , allowsRemoteRefresh :: !Bool
    -- | Whether visible remote models may replace the bundled catalog instead
    -- of being merged into it.
    , usesChatGptAuth :: !Bool
    }

data ModelsClientConfig = ModelsClientConfig
    { baseUrl :: !Text
    , clientVersion :: !Text
    } deriving (Eq, Show)

defaultModelsBaseUrl :: Text
defaultModelsBaseUrl = "https://chatgpt.com/backend-api/codex"

packageClientVersion :: Text
packageClientVersion =
    Text.pack
        (intercalate "." (map show (take 3 (versionBranch version <> repeat 0))))

modelsEndpointClient
    :: ModelsClientConfig
    -> TokenProvider
    -> ModelsEndpointClient
modelsEndpointClient config provider = ModelsEndpointClient
    { fetchModels = \condition ->
        runWithTokenProvider provider \credential ->
            let actualKey =
                    modelsCacheKeyForCredential config.baseUrl credential
                knownEtag = condition >>= \cached ->
                    if cached.cacheKey == actualKey
                        then Just cached.etag
                        else Nothing
            in listModelsWithCredentialAt
                config.baseUrl
                config.clientVersion
                knownEtag
                credential
    , allowsRemoteRefresh = subscriptionAuth
    , usesChatGptAuth = subscriptionAuth
    }
  where
    subscriptionAuth =
        tokenProviderBillingMode provider == SubscriptionBilled

listModelsWithProviderAt
    :: Text
    -> Text
    -> Maybe Text
    -> TokenProvider
    -> IO (Either ApiError ModelsEndpointResponse)
listModelsWithProviderAt baseUrl clientVersion knownEtag provider =
    runWithTokenProvider provider \credential ->
        listModelsWithCredentialAt
            baseUrl
            clientVersion
            knownEtag
            credential

listModelsWithCredentialAt
    :: Text
    -> Text
    -> Maybe Text
    -> Credential
    -> IO (Either ApiError ModelsEndpointResponse)
listModelsWithCredentialAt baseUrl clientVersion knownEtag credential =
    case credential.provider of
        XAIProvider -> pure $ Left $ ProviderError ApiErrorType
            "XAI credentials must be used through agent-xai"
            Nothing
        OpenRouterProvider -> pure $ Left $ ProviderError ApiErrorType
            "OpenRouter credentials must be used through agent-openrouter"
            Nothing
        ClaudeCodeProvider -> pure $ Left $ ProviderError ApiErrorType
            "Claude Code credentials must be used through agent-claude"
            Nothing
        OpenAIProvider ->
            tryAny requestModels >>= \case
                Left err -> pure $ Left $ ConnectionError
                    ("Codex models request failed: " <> Text.pack (show err))
                Right result -> pure result
  where
    requestModels = do
        let responseCacheKey =
                modelsCacheKeyForCredential baseUrl credential
        baseRequest <- parseRequest (Text.unpack baseUrl)
        let endpointPath =
                BS8.dropWhileEnd (== '/') (HttpClient.path baseRequest)
                    <> "/models"
            request =
                setRequestQueryString
                    ( getRequestQueryString baseRequest
                        <> [ ( "client_version"
                             , Just (Text.encodeUtf8 clientVersion)
                             )
                           ]
                    )
                    (setRequestPath endpointPath baseRequest)
        response <- httpLBS
            $ conditionalHeader knownEtag
            $ accountHeader credential.accountId
            $ setRequestHeader "Authorization"
                ["Bearer " <> Text.encodeUtf8 credential.accessToken]
            $ setRequestHeader "User-Agent"
                ["haskell-agent/" <> Text.encodeUtf8 clientVersion]
            $ setRequestHeader "Originator" ["haskell-agent"]
            $ setRequestHeader "Accept" ["application/json"]
            $ setRequestResponseTimeout
                (HttpClient.responseTimeoutMicro (5 * 1_000_000))
            $ request
        let status = getResponseStatusCode response
            body = getResponseBody response
            responseEtag = case getResponseHeader "ETag" response of
                value : _ ->
                    Just (Text.decodeUtf8With Text.lenientDecode value)
                [] -> Nothing
        pure $ case status of
            304 -> Right ModelsNotModified
                { etag = responseEtag <|> knownEtag
                , cacheKey = responseCacheKey
                }
            _ | status >= 200 && status < 300 ->
                case Json.decodeEither modelsResponseDecoder (LBS.toStrict body) of
                    Left err -> Left $ JsonDecodeError
                        ("Invalid Codex models response: "
                            <> Json.jsonErrorMessage err)
                        (bodyText body)
                    Right catalog -> Right ModelsFetched
                        { catalog
                        , etag = responseEtag
                        , cacheKey = responseCacheKey
                        }
            _ -> Left (classifyHttpFailure status (bodyText body))

modelsCacheKeyForCredential :: Text -> Credential -> ModelsCacheKey
modelsCacheKeyForCredential baseUrl credential = ModelsCacheKey
    { providerId = case credential.provider of
        OpenAIProvider -> "openai"
        XAIProvider -> "xai"
        OpenRouterProvider -> "openrouter"
        ClaudeCodeProvider -> "claude-code"
    , baseUrl
    , accountId =
        let account = Text.strip credential.accountId
        in if Text.null account then Nothing else Just account
    }

conditionalHeader :: Maybe Text -> Request -> Request
conditionalHeader knownEtag =
    maybe id
        (\value -> setRequestHeader "If-None-Match" [Text.encodeUtf8 value])
        knownEtag

accountHeader :: Text -> Request -> Request
accountHeader account request
    | Text.null (Text.strip account) = request
    | otherwise =
        setRequestHeader
            "ChatGPT-Account-ID"
            [Text.encodeUtf8 account]
            request

bodyText :: LBS.ByteString -> Text
bodyText =
    Text.decodeUtf8With Text.lenientDecode . LBS.toStrict
