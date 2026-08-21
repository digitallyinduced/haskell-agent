-- | Format Codex usage windows for @/usage@.
module Agent.CLI.Usage
    ( AccountUsageLine(..)
    , formatAccountUsage
    , formatUsageReport
    , formatUsageWindow
    , shortAccountId
    ) where

import Agent.CLI.Style (roleMuted, roleSuccess, roleWarn)
import Agent.Error (ApiError)
import Agent.OpenAI.Usage (UsageLimit(..), UsageSnapshot(..), UsageWindow(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (NominalDiffTime, UTCTime(..), addUTCTime, diffUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

data AccountUsageLine = AccountUsageLine
    { usageAccountId :: !Text
    , usageCooldownUntil :: !(Maybe UTCTime)
    , usageResult :: !(Either ApiError UsageSnapshot)
    }
    deriving (Show)

formatUsageReport :: Bool -> UTCTime -> [AccountUsageLine] -> Text
formatUsageReport color now lines_ =
    if null lines_
        then roleMuted color "usage: no ChatGPT accounts in the current pool"
        else Text.intercalate "\n\n" (map (formatAccountUsage color now) lines_)

formatAccountUsage :: Bool -> UTCTime -> AccountUsageLine -> Text
formatAccountUsage color now line =
    let header = roleMuted color ("account " <> shortAccountId line.usageAccountId)
        cooldown = case line.usageCooldownUntil of
            Just until_
                | until_ > now ->
                    Just $
                        roleWarn color
                            ("pacing until " <> formatClock until_
                                <> " (" <> formatDuration (diffUTCTime until_ now) <> ")")
                | otherwise -> Nothing
            Nothing -> Nothing
        body = case line.usageResult of
            Left err -> [roleWarn color ("fetch failed: " <> Text.pack (show err))]
            Right snapshot ->
                let plan = roleMuted color ("plan " <> snapshot.planType)
                    windows = case snapshot.rateLimit of
                        Nothing -> [roleMuted color "no rate-limit windows"]
                        Just UsageLimit
                            { allowed = _
                            , limitReached
                            , primaryWindow
                            , secondaryWindow
                            } ->
                            catWindows
                                [ fmap (("5h  " <>) . formatUsageWindow color)
                                    primaryWindow
                                , fmap (("7d  " <>) . formatUsageWindow color)
                                    secondaryWindow
                                ]
                                <> if limitReached
                                    then [roleWarn color "limit reached"]
                                    else []
                in plan : windows
    in Text.intercalate "\n  " (header : maybe id (:) cooldown body)

formatUsageWindow :: Bool -> UsageWindow -> Text
formatUsageWindow color window =
    let remaining = max 0 (100 - window.usedPercent)
        remainingText = Text.pack (show remaining) <> "% reserve"
        styled
            | remaining <= 10 = roleWarn color remainingText
            | otherwise = roleSuccess color remainingText
        untilReset =
            if remaining <= 0
                then "exhausted until reset in "
                    <> formatSeconds window.resetAfterSeconds
                else "lasts until reset in "
                    <> formatSeconds window.resetAfterSeconds
    in styled
        <> roleMuted color
            (" · "
                <> untilReset
                <> " ("
                <> formatEpoch window.resetAt
                <> ")")

shortAccountId :: Text -> Text
shortAccountId accountId
    | Text.length accountId <= 12 = accountId
    | otherwise = Text.take 8 accountId <> "…"

catWindows :: [Maybe Text] -> [Text]
catWindows = concatMap maybeToList

maybeToList :: Maybe a -> [a]
maybeToList = \case
    Nothing -> []
    Just value -> [value]

formatSeconds :: Int -> Text
formatSeconds total =
    formatDuration (fromIntegral (max 0 total))

formatDuration :: NominalDiffTime -> Text
formatDuration delta =
    let seconds = max 0 (round delta :: Int)
        hours = seconds `div` 3600
        minutes = (seconds `mod` 3600) `div` 60
    in if hours >= 24
        then Text.pack (show (hours `div` 24)) <> "d " <> Text.pack (show (hours `mod` 24)) <> "h"
        else if hours > 0
            then Text.pack (show hours) <> "h " <> Text.pack (show minutes) <> "m"
            else if minutes > 0
                then Text.pack (show minutes) <> "m"
                else Text.pack (show seconds) <> "s"

formatClock :: UTCTime -> Text
formatClock = Text.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M"

formatEpoch :: Int -> Text
formatEpoch epoch =
    formatClock (posixSecondsToUTCTime (fromIntegral epoch))

posixSecondsToUTCTime :: Integer -> UTCTime
posixSecondsToUTCTime seconds =
    addUTCTime (fromIntegral seconds) unixEpoch

unixEpoch :: UTCTime
unixEpoch = UTCTime (fromGregorian 1970 1 1) 0
