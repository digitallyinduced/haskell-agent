-- | Configuration for the xAI Grok subscription proxy.
module Agent.XAI.Options
    ( ClientOptions(..)
    , defaultClientOptions
    , clientOptionsFromEnv
    ) where

import Agent.Provider.Options
    ( lookupIntEnv
    , lookupNonEmptyEnv
    , parseModelOverrides
    )
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text

data ClientOptions = ClientOptions
    { baseUrl :: !String
      -- ^ Subscription proxy base URL including the @/v1@ segment.
    , modelOverrides :: ![(Text, Text)]
      -- ^ Exact-match request-model to xAI-model overrides.
    , defaultModel :: !Text
      -- ^ Target for non-Grok model names without an explicit override.
    , requestTimeoutSeconds :: !Int
      -- ^ Full-response timeout. Reasoning turns can stream for minutes.
    , clientVersion :: !Text
      -- ^ Value for @x-grok-client-version@.
    } deriving (Eq, Show)

defaultClientOptions :: ClientOptions
defaultClientOptions = ClientOptions
    { baseUrl = "https://cli-chat-proxy.grok.com/v1"
    , modelOverrides = []
    , defaultModel = "grok-4.6"
    , requestTimeoutSeconds = 600
    , clientVersion = "0.2.118"
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
        , modelOverrides = maybe [] (parseModelOverrides . Text.pack) modelMap
        , defaultModel = maybe defaultClientOptions.defaultModel Text.pack defaultModel
        , requestTimeoutSeconds = Maybe.fromMaybe defaultClientOptions.requestTimeoutSeconds
            timeoutSeconds
        , clientVersion = maybe defaultClientOptions.clientVersion Text.pack clientVersion
        }
