module Agent.OpenAI.Client
    ( CodexRequestOptions(..)
    , defaultCodexRequestOptions
    , remoteCompactionV2RequestOptions
    , createCodexMessage
    , createCodexMessageWithProvider
    , createCodexMessageWithProviderWithOptions
    , createCodexMessageWithProviderAt
    , createCodexMessageWithProviderAtWithOptions
    , defaultCodexBaseUrl
    , retryTransientCodexResultWithPolicy
    ) where

import Agent.OpenAI.Auth (Pool)
import Agent.OpenAI.Credential (poolTokenProvider)
import Agent.Error
import Agent.OpenAI.Error (classifyHttpFailure)
import Agent.OpenAI.Features
    ( betaFeaturesHeaderValue
    , remoteCompactionV2Feature
    )
import Agent.OpenAI.Http (decodeCodexHttpBody)
import Agent.OpenAI.Request (sanitizeCodexRequest)
import Agent.Provider
    ( Credential(..)
    , Provider(..)
    , TokenProvider
    , runWithTokenProvider
    )
import Agent.Retry (handleSyncExceptions)
import qualified Agent.Responses.Types as OpenAI
import Control.Monad (forM_, when)
import Control.Retry
    ( RetryPolicyM
    , exponentialBackoff
    , limitRetries
    , retrying
    )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text (lenientDecode)
import qualified Network.URI as URI

import OpenSSL
import qualified System.IO.Streams as Streams
import Network.Http.Client

-- | ChatGPT Codex REST prefix. 'createCodexMessage' and
-- 'createCodexMessageWithProvider' POST to @{defaultCodexBaseUrl}/responses@.
-- Point 'createCodexMessageWithProviderAt' at another OpenAI-compatible
-- Responses host (for example an llm-router @{origin}/v1@) to reuse the same
-- request/SSE client without the ChatGPT broker.
defaultCodexBaseUrl :: Text
defaultCodexBaseUrl = "https://chatgpt.com/backend-api/codex"

-- | Per-request Codex transport controls that are not part of the public
-- Responses JSON schema.
data CodexRequestOptions = CodexRequestOptions
    { betaFeatures :: ![Text]
    } deriving (Eq, Show)

defaultCodexRequestOptions :: CodexRequestOptions
defaultCodexRequestOptions = CodexRequestOptions
    { betaFeatures = [remoteCompactionV2Feature]
    }

-- | Normal Responses transport options for the @compaction_trigger@ protocol.
remoteCompactionV2RequestOptions :: CodexRequestOptions
remoteCompactionV2RequestOptions = defaultCodexRequestOptions

-- | Send a request to the Codex Responses API and parse the response.
-- Serialises 'OpenAI.ResponseCreateParams' via its 'ToJSON' instance, POSTs to
-- @/responses@, extracts the final @response.completed@ SSE event, and
-- decodes as 'OpenAI.Response'. Retries up to 3 times on transient errors.
--
-- On a 401/403, force-refreshes the rejected account immediately and retries
-- once with its rotated token before failing over. This covers tokens that
-- ChatGPT invalidates before their JWT expiry. On rate-limit errors (HTTP 429, @rate_limit_error@, or
-- @usage_limit_reached@), marks the offending account as cooling down and
-- retries with a different account from the pool. For
-- @usage_limit_reached@ the cooldown honours the server's
-- @resets_in_seconds@ exactly, so an exhausted 5h window is skipped for its
-- full duration rather than the short default.
createCodexMessage :: Pool -> OpenAI.ResponseCreateParams -> IO (Either ApiError OpenAI.Response)
createCodexMessage pool request = do
    provider <- poolTokenProvider pool
    createCodexMessageWithProvider provider request

-- | Provider-based REST client. Account selection, cooldowns, credential
-- refresh, and broker feedback live behind 'TokenProvider'; the transport only
-- reports structured account failures and retries the request with the
-- returned credential. XAI credentials belong to the @agent-xai@ transport
-- package and are rejected at this OpenAI-specific boundary.
createCodexMessageWithProvider
    :: TokenProvider
    -> OpenAI.ResponseCreateParams
    -> IO (Either ApiError OpenAI.Response)
createCodexMessageWithProvider =
    createCodexMessageWithProviderWithOptions defaultCodexRequestOptions

-- | Provider-based REST client with Codex-specific transport options.
createCodexMessageWithProviderWithOptions
    :: CodexRequestOptions
    -> TokenProvider
    -> OpenAI.ResponseCreateParams
    -> IO (Either ApiError OpenAI.Response)
createCodexMessageWithProviderWithOptions options =
    createCodexMessageWithProviderAtWithOptions
        options
        defaultCodexBaseUrl

-- | Like 'createCodexMessageWithProvider', but POST to
-- @{baseUrl}/responses@ instead of the ChatGPT Codex backend.
--
-- @baseUrl@ is the Responses API prefix with no trailing slash, e.g.
-- @https://llm-router.example/v1@. Empty @accountId@ credentials omit the
-- ChatGPT-only @chatgpt-account-id@ header so a static bearer can talk to a
-- compatible proxy without a broker-issued account.
createCodexMessageWithProviderAt
    :: Text
    -> TokenProvider
    -> OpenAI.ResponseCreateParams
    -> IO (Either ApiError OpenAI.Response)
createCodexMessageWithProviderAt =
    createCodexMessageWithProviderAtWithOptions defaultCodexRequestOptions

-- | Like 'createCodexMessageWithProviderAt', with additional Codex transport
-- headers such as @x-codex-beta-features@.
createCodexMessageWithProviderAtWithOptions
    :: CodexRequestOptions
    -> Text
    -> TokenProvider
    -> OpenAI.ResponseCreateParams
    -> IO (Either ApiError OpenAI.Response)
createCodexMessageWithProviderAtWithOptions options baseUrl provider request =
    runWithTokenProvider provider \credential ->
        case credential.provider of
            XAIProvider -> pure $ Left $ ProviderError ApiErrorType
                "XAI credentials must be used through agent-xai"
                Nothing
            OpenRouterProvider -> pure $ Left $ ProviderError ApiErrorType
                "OpenRouter credentials must be used through agent-openrouter"
                Nothing
            ClaudeCodeProvider -> pure $ Left $ ProviderError ApiErrorType
                "Claude Code subscription sessions must use agent-claude"
                Nothing
            OpenAIProvider ->
                retryTransientCodexResultWithPolicy transientResultPolicy $
                    handleSyncExceptions
                        (ConnectionError
                            . ("Codex request failed: " <>)
                            . Text.pack
                            . show) $
                        makeCodexRequest
                            options
                            baseUrl
                            credential.accessToken
                            credential.accountId
                            request
  where
    transientResultPolicy = exponentialBackoff 5_000_000 <> limitRetries 3

-- | Retry short-lived provider and transport failures. Quota and rate-limit
-- errors remain excluded by 'isInlineRetryableProviderError'.
retryTransientCodexResultWithPolicy
    :: RetryPolicyM IO
    -> IO (Either ApiError value)
    -> IO (Either ApiError value)
retryTransientCodexResultWithPolicy policy request =
    retrying policy shouldRetry (const request)
  where
    shouldRetry _retryStatus = \case
        Left apiError | isInlineRetryableProviderError apiError -> pure True
        _ -> pure False

makeCodexRequest
    :: CodexRequestOptions
    -> Text
    -> Text
    -> Text
    -> OpenAI.ResponseCreateParams
    -> IO (Either ApiError OpenAI.Response)
makeCodexRequest options baseUrl accessToken accountId request = do
    -- The ChatGPT Codex HTTP endpoint only serves Responses requests as SSE
    -- and rejects an omitted/false stream flag. WebSocket callers do not need
    -- this field, so enforce it at the HTTP transport boundary.
    let requestBody = Aeson.toJSON
            (sanitizeCodexRequest request) { OpenAI.stream = Just True }
        url = Text.dropWhileEnd (== '/') baseUrl <> "/responses"

    case URI.parseURI (Text.unpack url) of
        Nothing -> pure $ Left (JsonDecodeError ("Invalid URL: " <> url) "")
        Just uri -> do
            let path = Text.encodeUtf8 (Text.pack uri.uriPath)
            withOpenSSL $
                withConnection (establishConnection (Text.encodeUtf8 url)) $ \conn -> do
                    let req = buildRequest1 $ do
                                http POST path
                                setContentType "application/json"
                                Network.Http.Client.setHeader "Authorization" ("Bearer " <> Text.encodeUtf8 accessToken)
                                -- ChatGPT's Codex backend requires this header.
                                -- OpenAI-compatible proxies (llm-router) do not.
                                when (not (Text.null (Text.strip accountId))) $
                                    Network.Http.Client.setHeader "chatgpt-account-id" (Text.encodeUtf8 accountId)
                                forM_ (betaFeaturesHeaderValue options.betaFeatures) \features ->
                                    Network.Http.Client.setHeader
                                        "x-codex-beta-features"
                                        (Text.encodeUtf8 features)
                    sendRequest conn req (jsonBody requestBody)
                    receiveResponse conn responseHandler

responseHandler :: Response -> Streams.InputStream BS.ByteString -> IO (Either ApiError OpenAI.Response)
responseHandler response stream = do
    let status = getStatusCode response
    bytes <- Streams.fold mappend mempty stream
    let bodyText = Text.decodeUtf8With Text.lenientDecode bytes
    if status >= 200 && status < 300
        then pure (decodeCodexHttpBody bodyText)
        else pure $ Left (classifyHttpFailure status bodyText)
