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
    }

-- Keep bearer, refresh, and identity tokens out of logs and test failures.
instance Show AuthState where
    show state =
        "AuthState { accessToken = <redacted>, refreshToken = <redacted>"
            <> ", accountId = " <> show state.accountId
            <> ", idToken = "
            <> maybe "Nothing" (const "Just <redacted>") state.idToken
            <> ", lastRefresh = " <> show state.lastRefresh
            <> " }"
