-- | Format Codex usage windows for @/usage@.
module Agent.CLI.Usage
    ( AccountUsageLine(..)
    , formatAccountUsage
    , formatDuration
    , formatGrokLimitStatus
    , formatOpenAiLimitStatus
    , formatOpenRouterLimitStatus
    , formatModelUsageSummary
    , formatUsageReport
    , formatUsageSummary
    , formatUsageWindow
    , shortAccountId
    ) where

import Agent.CLI.Duration (formatDuration)
import Agent.CLI.Error (formatApiErrorInlineAt)
import Agent.CLI.Style (roleMuted, roleSuccess, roleWarn)
import Agent.Error (ApiError)
import Agent.OpenAI.Usage (UsageLimit(..), UsageSnapshot(..), UsageWindow(..))
import Agent.OpenRouter.Usage (OpenRouterUsage(..))
import Agent.TUI.Model (PromptLimitStatus(..))
import Agent.XAI.Usage (GrokUsageSnapshot, weeklyLimitLeft)
import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), addUTCTime, diffUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

data AccountUsageLine = AccountUsageLine
    { usageAccountId :: !Text
    , usageCooldownUntil :: !(Maybe UTCTime)
    , usageResult :: !(Either ApiError UsageSnapshot)
    }
    deriving (Show)

formatGrokLimitStatus :: GrokUsageSnapshot -> Maybe PromptLimitStatus
formatGrokLimitStatus snapshot =
    percentLimitStatus "Weekly limit left" <$> weeklyLimitLeft snapshot

formatOpenAiLimitStatus :: UsageSnapshot -> Maybe PromptLimitStatus
formatOpenAiLimitStatus snapshot = do
    limits <- snapshot.rateLimit
    window <- longestUsageWindow limits
    pure $
        percentLimitStatus
            (usageWindowLimitLabel window)
            (remainingWindowPercent window)

formatOpenRouterLimitStatus :: OpenRouterUsage -> Maybe PromptLimitStatus
formatOpenRouterLimitStatus usage =
    keyLimitStatus <|> creditStatus
  where
    keyRemaining =
        usage.keyLimitRemaining
            <|> ((-) <$> usage.keyLimit <*> usage.keyUsage)
    keyLimitStatus = case (usage.keyLimit, keyRemaining) of
        (Just limit, Just remaining)
            | limit > 0 ->
                Just $
                    percentLimitStatus
                        "Key limit left"
                        (remainingPercent remaining limit)
        (_, Just remaining) ->
            Just (amountLimitStatus "Key limit left" remaining False)
        _ -> Nothing
    creditStatus = do
        credits <- usage.totalCredits
        spent <- usage.totalUsage
        let remaining = max 0 (credits - spent)
            warning
                | credits <= 0 = remaining <= 0
                | otherwise = remainingPercent remaining credits <= 10
        pure (amountLimitStatus "Credits left" remaining warning)

percentLimitStatus :: Text -> Int -> PromptLimitStatus
percentLimitStatus label remaining =
    let clamped = max 0 (min 100 remaining)
    in PromptLimitStatus
        { promptLimitText =
            label <> ": " <> Text.pack (show clamped) <> "%"
        , promptLimitWarning = clamped <= 10
        }

remainingWindowPercent :: UsageWindow -> Int
remainingWindowPercent window =
    max 0 (100 - window.usedPercent)

remainingPercent :: Real a => a -> a -> Int
remainingPercent remaining total =
    max 0 $
        min 100 $
            round
                (100 * toRational remaining / toRational total)

amountLimitStatus :: Real a => Text -> a -> Bool -> PromptLimitStatus
amountLimitStatus label remaining warning =
    PromptLimitStatus
        { promptLimitText = label <> ": $" <> formatMoney remaining
        , promptLimitWarning = warning
        }

formatMoney :: Real a => a -> Text
formatMoney amount =
    let cents = max 0 (round (toRational amount * 100) :: Integer)
        (dollars, remainder) = cents `divMod` 100
        centsText
            | remainder < 10 = "0" <> Text.pack (show remainder)
            | otherwise = Text.pack (show remainder)
    in Text.pack (show dollars) <> "." <> centsText

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
            Left err ->
                [ roleWarn color
                    ("couldn't load usage: " <> formatApiErrorInlineAt now err)
                ]
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
                                [ formatLabeledUsageWindow color
                                    <$> primaryWindow
                                , formatLabeledUsageWindow color
                                    <$> secondaryWindow
                                ]
                                <> if limitReached
                                    then [roleWarn color "limit reached"]
                                    else []
                in plan : windows
    in Text.intercalate "\n  " (header : maybe id (:) cooldown body)

-- | Compact usage detail suitable for a one-line account picker row.
formatUsageSummary
    :: UTCTime
    -> Maybe UTCTime
    -> Either ApiError UsageSnapshot
    -> Text
formatUsageSummary now cooldownUntil result =
    Text.intercalate " · " $
        cooldown <> case result of
            Left _ -> ["usage unavailable"]
            Right snapshot ->
                plan snapshot <> windows snapshot <> limit snapshot
  where
    cooldown = case cooldownUntil of
        Just until_
            | until_ > now ->
                ["pacing " <> formatDuration (diffUTCTime until_ now)]
        _ -> []
    plan snapshot
        | Text.null (Text.strip snapshot.planType) = []
        | otherwise = [snapshot.planType]
    windows snapshot = case snapshot.rateLimit of
        Nothing -> []
        Just usageLimit ->
            catWindows
                [ summarizeWindow <$> usageLimit.primaryWindow
                , summarizeWindow <$> usageLimit.secondaryWindow
                ]
    limit snapshot = case snapshot.rateLimit of
        Just usageLimit
            | usageLimit.limitReached -> ["limit reached"]
        _ -> []
    summarizeWindow window =
        usageWindowShortLabel window
            <> " "
            <> Text.pack (show (max 0 (100 - window.usedPercent)))
            <> "% left"

-- | Compact gateway-model usage suitable for a model-picker row.
formatModelUsageSummary :: UsageSnapshot -> Maybe Text
formatModelUsageSummary snapshot = do
    limits <- snapshot.rateLimit
    let windows =
            [ window
            | Just window <-
                [limits.primaryWindow, limits.secondaryWindow]
            ]
        summaries = map summarizeWindow windows
        parts
            | limits.limitReached = "limit reached" : summaries
            | otherwise = summaries
    case parts of
        [] -> Nothing
        _ -> Just (Text.intercalate " · " parts)
  where
    summarizeWindow window =
        usageWindowShortLabel window
            <> " "
            <> Text.pack (show (max 0 (100 - window.usedPercent)))
            <> "% left"

longestUsageWindow :: UsageLimit -> Maybe UsageWindow
longestUsageWindow limits =
    case (limits.primaryWindow, limits.secondaryWindow) of
        (Nothing, Nothing) -> Nothing
        (Just primary, Nothing) -> Just primary
        (Nothing, Just secondary) -> Just secondary
        (Just primary, Just secondary)
            | primary.limitWindowSeconds >= secondary.limitWindowSeconds ->
                Just primary
            | otherwise -> Just secondary

formatLabeledUsageWindow :: Bool -> UsageWindow -> Text
formatLabeledUsageWindow color window =
    usageWindowShortLabel window <> "  " <> formatUsageWindow color window

usageWindowLimitLabel :: UsageWindow -> Text
usageWindowLimitLabel window
    | window.limitWindowSeconds == secondsPerWeek = "Weekly limit left"
    | otherwise = usageWindowShortLabel window <> " limit left"

usageWindowShortLabel :: UsageWindow -> Text
usageWindowShortLabel window
    | seconds == secondsPerWeek = "7d"
    | seconds `mod` secondsPerDay == 0 =
        packCount (seconds `div` secondsPerDay) "d"
    | seconds `mod` secondsPerHour == 0 =
        packCount (seconds `div` secondsPerHour) "h"
    | seconds `mod` secondsPerMinute == 0 =
        packCount (seconds `div` secondsPerMinute) "m"
    | otherwise = packCount seconds "s"
  where
    seconds = max 0 window.limitWindowSeconds
    packCount count suffix = Text.pack (show count) <> suffix

secondsPerMinute, secondsPerHour, secondsPerDay, secondsPerWeek :: Int
secondsPerMinute = 60
secondsPerHour = 60 * secondsPerMinute
secondsPerDay = 24 * secondsPerHour
secondsPerWeek = 7 * secondsPerDay

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
