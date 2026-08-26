-- | Configuration for the OpenRouter Responses transport.
module Agent.OpenRouter.Options
    ( ClientOptions(..)
    , defaultClientOptions
    , clientOptionsFromEnv
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

data ClientOptions = ClientOptions
    { baseUrl :: !String
      -- ^ OpenRouter API prefix including the @/v1@ segment.
    , modelOverrides :: !(Map Text Text)
      -- ^ Exact-match request-model to OpenRouter-model overrides.
    , defaultModel :: !Text
      -- ^ Target when the request model is absent or not an OpenRouter slug.
    , requestTimeoutSeconds :: !Int
      -- ^ Full-response timeout. Reasoning turns can stream for minutes.
    , httpReferer :: !(Maybe Text)
      -- ^ Optional @HTTP-Referer@ for OpenRouter app rankings.
    , appTitle :: !(Maybe Text)
      -- ^ Optional @X-Title@ for OpenRouter app rankings.
    } deriving (Eq, Show)

defaultClientOptions :: ClientOptions
defaultClientOptions = ClientOptions
    { baseUrl = "https://openrouter.ai/api/v1"
    , modelOverrides = Map.empty
    , defaultModel = "openai/gpt-5.1"
    , requestTimeoutSeconds = 600
    , httpReferer = Nothing
    , appTitle = Nothing
    }

-- | Load optional transport overrides from the environment.
clientOptionsFromEnv :: IO ClientOptions
clientOptionsFromEnv = do
    baseUrl <- lookupNonEmptyEnv "OPENROUTER_BASE_URL"
    modelMap <- lookupNonEmptyEnv "OPENROUTER_MODEL_MAP"
    defaultModel <- lookupNonEmptyEnv "OPENROUTER_DEFAULT_MODEL"
    timeoutSeconds <- lookupIntEnv "OPENROUTER_TIMEOUT_SECONDS"
    httpReferer <- lookupNonEmptyEnv "OPENROUTER_HTTP_REFERER"
    appTitle <- lookupNonEmptyEnv "OPENROUTER_APP_TITLE"
    pure ClientOptions
        { baseUrl = Maybe.fromMaybe defaultClientOptions.baseUrl baseUrl
        , modelOverrides = maybe Map.empty (parseModelOverrides . Text.pack) modelMap
        , defaultModel = maybe defaultClientOptions.defaultModel Text.pack defaultModel
        , requestTimeoutSeconds = Maybe.fromMaybe defaultClientOptions.requestTimeoutSeconds
            timeoutSeconds
        , httpReferer = Text.pack <$> httpReferer
        , appTitle = Text.pack <$> appTitle
        }
