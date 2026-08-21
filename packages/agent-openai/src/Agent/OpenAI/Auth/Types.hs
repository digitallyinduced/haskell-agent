module Agent.OpenAI.Auth.Types (AuthState(..)) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)

-- | In-memory auth state for a single ChatGPT account.
data AuthState = AuthState
    { accessToken  :: !Text
    , refreshToken :: !Text
    , accountId    :: !Text
    , idToken      :: !(Maybe Text)
    , lastRefresh  :: !UTCTime
    } deriving (Show)
