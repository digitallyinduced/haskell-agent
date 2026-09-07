module Agent.CLI.Login.Internal.Device
    ( DevicePollReadiness(..)
    , DevicePollSchedule
    , advanceDevicePollSchedule
    , advanceGatewayPollSchedule
    , authorizationPendingNotice
    , authorizationSlowDownNotice
    , deviceAuthorizationBody
    , deviceAuthorizationDefaultTimeoutSeconds
    , devicePollReadiness
    , initialDevicePollSchedule
    , pollWaitNotice
    ) where

import Agent.CLI.Login.Internal.Dashboard (markdownText)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime)

data DevicePollSchedule = DevicePollSchedule
    { devicePollIntervalSeconds :: !Int
    , devicePollNextAt :: !UTCTime
    , devicePollExpiresAt :: !UTCTime
    } deriving (Eq, Show)

data DevicePollReadiness
    = DevicePollReady
    | DevicePollWait !Int
    | DevicePollExpired
    deriving (Eq, Show)

-- | Start a bounded device-code flow. The first explicit check is allowed
-- immediately; every pending response schedules the next permitted check.
initialDevicePollSchedule
    :: UTCTime
    -> Int
    -> Int
    -> DevicePollSchedule
initialDevicePollSchedule startedAt intervalSeconds expiresInSeconds =
    DevicePollSchedule
        { devicePollIntervalSeconds = max 1 intervalSeconds
        , devicePollNextAt = startedAt
        , devicePollExpiresAt =
            addUTCTime
                (fromIntegral (max 0 expiresInSeconds))
                startedAt
        }

devicePollReadiness
    :: UTCTime
    -> DevicePollSchedule
    -> DevicePollReadiness
devicePollReadiness now schedule
    | now >= schedule.devicePollExpiresAt =
        DevicePollExpired
    | now < schedule.devicePollNextAt =
        DevicePollWait
            (max 1 (ceiling
                (diffUTCTime schedule.devicePollNextAt now)))
    | otherwise =
        DevicePollReady

-- | Record a pending poll. A @slow_down@ response adds the cumulative
-- five-second backoff required by RFC 8628.
advanceDevicePollSchedule
    :: Bool
    -> UTCTime
    -> DevicePollSchedule
    -> DevicePollSchedule
advanceDevicePollSchedule slowedDown polledAt schedule =
    schedule
        { devicePollIntervalSeconds = intervalSeconds
        , devicePollNextAt =
            addUTCTime (fromIntegral intervalSeconds) polledAt
        }
  where
    intervalSeconds =
        schedule.devicePollIntervalSeconds
            + if slowedDown then 5 else 0

advanceGatewayPollSchedule
    :: Bool
    -> Maybe Int
    -> UTCTime
    -> DevicePollSchedule
    -> DevicePollSchedule
advanceGatewayPollSchedule slowedDown serverInterval polledAt schedule =
    schedule
        { devicePollIntervalSeconds = intervalSeconds
        , devicePollNextAt =
            addUTCTime (fromIntegral intervalSeconds) polledAt
        }
  where
    requestedInterval =
        maybe schedule.devicePollIntervalSeconds (max 1) serverInterval
    intervalSeconds
        | slowedDown =
            max requestedInterval
                (schedule.devicePollIntervalSeconds + 5)
        | otherwise = requestedInterval

deviceAuthorizationBody
    :: Text
    -> Text
    -> Text
    -> Maybe Text
    -> Text
deviceAuthorizationBody provider url userCode notice =
    Text.intercalate "\n\n" $
        [ "1. [Open the " <> provider <> " sign-in page](" <> url <> ")."
        , "2. Enter this one-time code:"
        , "`" <> markdownText 100 userCode <> "`"
        , "3. Return here and choose **Check authorization**."
        ]
            <> maybe [] (pure . markdownText 300) notice

deviceAuthorizationDefaultTimeoutSeconds :: Int
deviceAuthorizationDefaultTimeoutSeconds = 15 * 60

authorizationPendingNotice :: Text
authorizationPendingNotice =
    "Authorization is still pending. Finish the browser step, then check again."

pollWaitNotice :: Int -> Text
pollWaitNotice seconds =
    "Please wait "
        <> Text.pack (show seconds)
        <> " seconds before checking authorization again."

authorizationSlowDownNotice :: Int -> Text
authorizationSlowDownNotice seconds =
    "xAI asked this client to slow down. The next check is available in "
        <> Text.pack (show seconds)
        <> " seconds."
