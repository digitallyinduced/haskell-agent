-- | Configuration for the native Gemini GenerateContent transports.
module Agent.Gemini.Options
    ( ClientOptions(..)
    , defaultClientOptions
    , clientOptionsFromEnv
    ) where

import Agent.Provider.Options (lookupIntEnv, lookupNonEmptyEnv)
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text

data ClientOptions = ClientOptions
    { baseUrl :: !String
      -- ^ Google Generative Language API prefix, including @/v1beta@.
    , codeAssistBaseUrl :: !String
      -- ^ Gemini Code Assist API prefix, including @/v1internal@.
    , defaultModel :: !Text
      -- ^ Model used when a request omits its model.
    , requestTimeoutSeconds :: !Int
      -- ^ Header and per-stream-read timeout.
    } deriving (Eq, Show)

defaultClientOptions :: ClientOptions
defaultClientOptions = ClientOptions
    { baseUrl = "https://generativelanguage.googleapis.com/v1beta"
    , codeAssistBaseUrl = "https://cloudcode-pa.googleapis.com/v1internal"
    , defaultModel = "gemini-3.7-flash"
    , requestTimeoutSeconds = 600
    }

-- | Load optional endpoint, model, and timeout overrides.
clientOptionsFromEnv :: IO ClientOptions
clientOptionsFromEnv = do
    baseUrl <- lookupNonEmptyEnv "GEMINI_BASE_URL"
    codeAssistBaseUrl <-
        lookupNonEmptyEnv "GEMINI_CODE_ASSIST_BASE_URL"
    defaultModel <- lookupNonEmptyEnv "GEMINI_DEFAULT_MODEL"
    timeoutSeconds <- lookupIntEnv "GEMINI_TIMEOUT_SECONDS"
    pure ClientOptions
        { baseUrl = Maybe.fromMaybe defaultClientOptions.baseUrl baseUrl
        , codeAssistBaseUrl = Maybe.fromMaybe
            defaultClientOptions.codeAssistBaseUrl
            codeAssistBaseUrl
        , defaultModel =
            maybe defaultClientOptions.defaultModel Text.pack defaultModel
        , requestTimeoutSeconds =
            Maybe.fromMaybe defaultClientOptions.requestTimeoutSeconds
                timeoutSeconds
        }
