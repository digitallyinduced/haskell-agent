-- | Automatic resumption after a provider usage-window exhaustion.
--
-- Brief cooldowns are retried silently by "Agent.CLI.ProviderFallback". When
-- a usage window is exhausted for longer, no other provider can take over, and
-- the session is interactive, the session waits for the provider's reset
-- deadline and then resumes the interrupted work by itself instead of leaving
-- the transcript stalled until the user notices.
module Agent.CLI.UsageLimitRecovery
    ( maximumAutomaticUsageLimitWait
    , minimumAutomaticUsageLimitWait
    , usageLimitResetDeadline
    , usageLimitResumeAt
    , usageLimitResumeMessage
    , usageLimitResumeTimeText
    , usageLimitWaitText
    ) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Runtime.Duration (formatDuration)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (LocalTime(..), TimeZone, utcToLocalTime)

-- | Longest usage-window reset the session waits for by itself. Weekly
-- subscription windows are the longest documented provider reset; anything
-- further away is more likely a malformed deadline than a real reset.
maximumAutomaticUsageLimitWait :: NominalDiffTime
maximumAutomaticUsageLimitWait = 8 * 24 * 3600

-- | Shortest automatic wait. A deadline that has already passed, or a rounded
-- provider timestamp, must not turn into an immediate resubmission loop.
minimumAutomaticUsageLimitWait :: NominalDiffTime
minimumAutomaticUsageLimitWait = 15

-- | The provider's stated reset deadline for a usage-window exhaustion.
-- Errors without a definite reset time do not qualify for automatic
-- resumption; the user keeps the manual retry.
usageLimitResetDeadline :: UTCTime -> ApiError -> Maybe UTCTime
usageLimitResetDeadline now = \case
    CredentialsExhausted{retryAt} -> Just retryAt
    ProviderError UsageLimitReached _ (Just seconds) -> Just (after seconds)
    ProviderError RateLimitError _ (Just seconds) -> Just (after seconds)
    _ -> Nothing
  where
    after seconds = addUTCTime (fromIntegral (max 0 seconds)) now

-- | When the session should resume by itself after the given failure, or
-- 'Nothing' when the failure is not a usage-window exhaustion with a
-- reasonable reset deadline.
usageLimitResumeAt :: UTCTime -> ApiError -> Maybe UTCTime
usageLimitResumeAt now err = do
    deadline <- usageLimitResetDeadline now err
    let wait = diffUTCTime deadline now
    if wait > maximumAutomaticUsageLimitWait
        then Nothing
        else Just (addUTCTime (max minimumAutomaticUsageLimitWait wait) now)

-- | Continuation prompt submitted on the user's behalf once the limit resets.
-- It is a real user turn so the model learns why the previous turn stopped
-- and the durable transcript records the interruption.
usageLimitResumeMessage :: Text
usageLimitResumeMessage =
    "I hit my usage limit while you were working, but it has reset now. \
    \Please continue from where you left off."

-- | Local wall-clock presentation of the resume deadline. The calendar day is
-- included only when the deadline is not on the same local day as now.
usageLimitResumeTimeText :: TimeZone -> UTCTime -> UTCTime -> Text
usageLimitResumeTimeText zone now resumeAt =
    Text.pack (formatTime defaultTimeLocale pattern local)
  where
    local = utcToLocalTime zone resumeAt
    sameDay = localDay (utcToLocalTime zone now) == localDay local
    pattern
        | sameDay = "%H:%M"
        | otherwise = "%a %b %-d %H:%M"

-- | Live status text while waiting for the reset. The remaining time uses the
-- coarse duration format because usage windows are typically hours long.
usageLimitWaitText :: Text -> Int -> Text
usageLimitWaitText resumeTimeText remainingSeconds =
    "Usage limit reached; resuming automatically at "
        <> resumeTimeText
        <> " (in "
        <> formatDuration (fromIntegral (max 0 remainingSeconds))
        <> ") · Esc to cancel"
