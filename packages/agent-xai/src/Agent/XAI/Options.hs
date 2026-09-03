-- | Configuration for the xAI Grok subscription proxy.
module Agent.XAI.Options
    ( ClientOptions(..)
    , defaultClientOptions
    , clientOptionsFromEnv
    , defaultGrokClientVersion
    , grokAuthenticateResponseValue
    , grokClientIdentifier
    , grokTokenAuthValue
    , grokUserAgent
    ) where

import Agent.Provider.Options
    ( lookupIntEnv
    , lookupNonEmptyEnv
    , parseModelOverrides
    )
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified System.Info as SysInfo

-- | Process identity Grok Build sends as @x-grok-client-identifier@.
grokClientIdentifier :: Text
grokClientIdentifier = "grok-shell"

-- | Current Grok Build crate version (@xai-grok-version@). The proxy
-- version-gates on @x-grok-client-version@.
defaultGrokClientVersion :: Text
defaultGrokClientVersion = "1.0.8"

-- | Value Grok Build injects as @X-XAI-Token-Auth@ on cli-chat-proxy.
grokTokenAuthValue :: Text
grokTokenAuthValue = "xai-grok-cli"

-- | Value Grok Build injects as @x-authenticateresponse@ on cli-chat-proxy.
grokAuthenticateResponseValue :: Text
grokAuthenticateResponseValue = "authenticate-response"

-- | User-Agent Grok Build sends when the origin product is @grok-shell@.
grokUserAgent :: Text -> Text
grokUserAgent version =
    grokClientIdentifier
        <> "/"
        <> version
        <> " ("
        <> grokPlatformOs
        <> "; "
        <> grokPlatformArch
        <> ")"

grokPlatformOs :: Text
grokPlatformOs = case SysInfo.os of
    "darwin" -> "macos"
    "mingw32" -> "windows"
    other -> Text.pack other

grokPlatformArch :: Text
grokPlatformArch = case SysInfo.arch of
    "arm64" -> "aarch64"
    other -> Text.pack other

data ClientOptions = ClientOptions
    { baseUrl :: !String
      -- ^ Subscription proxy base URL including the @/v1@ segment.
    , modelOverrides :: !(Map Text Text)
      -- ^ Exact-match request-model to xAI-model overrides.
    , defaultModel :: !Text
      -- ^ Target for non-Grok model names without an explicit override.
    , requestTimeoutSeconds :: !Int
      -- ^ Full-response timeout. Reasoning turns can stream for minutes.
    , clientVersion :: !Text
      -- ^ Value for @x-grok-client-version@.
    , hostedXSearchEnabled :: !Bool
      -- ^ Whether the wire adapter may inject provider-hosted @x_search@.
    } deriving (Eq, Show)

defaultClientOptions :: ClientOptions
defaultClientOptions = ClientOptions
    { baseUrl = "https://cli-chat-proxy.grok.com/v1"
    , modelOverrides = Map.empty
    , defaultModel = "grok-4.6"
    , requestTimeoutSeconds = 600
    , clientVersion = defaultGrokClientVersion
    , hostedXSearchEnabled = True
    }

-- | Load optional transport overrides from the environment.
clientOptionsFromEnv :: IO ClientOptions
clientOptionsFromEnv = do
    baseUrl <- lookupNonEmptyEnv "XAI_GROK_BASE_URL"
    modelMap <- lookupNonEmptyEnv "XAI_GROK_MODEL_MAP"
    defaultModel <- lookupNonEmptyEnv "XAI_GROK_DEFAULT_MODEL"
    timeoutSeconds <- lookupIntEnv "XAI_GROK_TIMEOUT_SECONDS"
    clientVersion <- lookupNonEmptyEnv "XAI_GROK_CLIENT_VERSION"
    pure ClientOptions
        { baseUrl = Maybe.fromMaybe defaultClientOptions.baseUrl baseUrl
        , modelOverrides = maybe Map.empty (parseModelOverrides . Text.pack) modelMap
        , defaultModel = maybe defaultClientOptions.defaultModel Text.pack defaultModel
        , requestTimeoutSeconds = Maybe.fromMaybe defaultClientOptions.requestTimeoutSeconds
            timeoutSeconds
        , clientVersion = maybe defaultClientOptions.clientVersion Text.pack clientVersion
        , hostedXSearchEnabled = defaultClientOptions.hostedXSearchEnabled
        }
