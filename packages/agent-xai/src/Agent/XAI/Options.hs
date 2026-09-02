-- | Configuration for the xAI Grok subscription proxy.
module Agent.XAI.Options
    ( ClientOptions(..)
    , defaultClientOptions
    , clientOptionsFromEnv
    , defaultGrokClientVersion
    , grokAutoCompactThresholdPercent
    , grokAutoCompactTokenLimit
    , grokAuthenticateResponseValue
    , grokClientIdentifier
    , grokDefaultContextWindow
    , grokServerCompactionAtTokens
    , grokServerCompactionsRemaining
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

-- | Context window advertised by the current Grok 4.5/4.6 model catalog.
grokDefaultContextWindow :: Int
grokDefaultContextWindow = 500_000

-- | Grok 4.5/4.6 override the Grok Build global 85% fallback with 80%.
grokAutoCompactThresholdPercent :: Text -> Int
grokAutoCompactThresholdPercent model
    | model `elem` ["grok-4.5", "grok-4.6"] = 80
    | otherwise = 85

-- | Resolve the client-side automatic compaction threshold for a model.
grokAutoCompactTokenLimit :: Text -> Int -> Int
grokAutoCompactTokenLimit model contextWindow =
    max 1 $
        contextWindow * grokAutoCompactThresholdPercent model `div` 100

-- | Server hint enabled by the current Grok model catalog. Unknown future
-- models keep the global client-side fallback but do not receive metadata that
-- their server configuration has not opted into.
grokServerCompactionAtTokens :: Text -> Maybe Int
grokServerCompactionAtTokens model
    | model `elem` ["grok-4.5", "grok-4.6"] =
        Just (grokAutoCompactTokenLimit model grokDefaultContextWindow)
    | otherwise = Nothing

-- | Current Grok metadata uses a fixed integer, rather than the dynamic
-- boolean mode that changes from one to zero after a compaction.
grokServerCompactionsRemaining :: Text -> Maybe Int
grokServerCompactionsRemaining model =
    1 <$ grokServerCompactionAtTokens model

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
    , autoCompactTokenLimit :: !(Maybe Int)
      -- ^ Explicit automatic-compaction threshold. 'Nothing' uses the
      -- model-specific Grok Build default.
    , requestTimeoutSeconds :: !Int
      -- ^ Full-response timeout. Reasoning turns can stream for minutes.
    , clientVersion :: !Text
      -- ^ Value for @x-grok-client-version@.
    } deriving (Eq, Show)

defaultClientOptions :: ClientOptions
defaultClientOptions = ClientOptions
    { baseUrl = "https://cli-chat-proxy.grok.com/v1"
    , modelOverrides = Map.empty
    , defaultModel = "grok-4.6"
    , autoCompactTokenLimit = Nothing
    , requestTimeoutSeconds = 600
    , clientVersion = defaultGrokClientVersion
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
        , autoCompactTokenLimit = Nothing
        , requestTimeoutSeconds = Maybe.fromMaybe defaultClientOptions.requestTimeoutSeconds
            timeoutSeconds
        , clientVersion = maybe defaultClientOptions.clientVersion Text.pack clientVersion
        }
