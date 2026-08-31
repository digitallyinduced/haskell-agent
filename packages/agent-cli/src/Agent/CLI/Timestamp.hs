-- | Wall-clock stamps on model-facing user messages (belege.ai style).
--
-- User turns sent after a pause may end with a compact local timestamp like
-- @[4:45pm CEST]@ so the agent can judge pauses between turns.
-- The REPL UI keeps the unstamped text; assistant replies are scrubbed if
-- the model echoes a stamp.
module Agent.CLI.Timestamp
    ( HourCycle(..)
    , currentShortMessageTimestamp
    , renderMessageTimestamp
    , renderContextualMessageTimestamp
    , renderShortMessageTimestamp
    , stampUserText
    , stampUserTextAt
    , stampTurnInputs
    , stampTurnInputsSince
    , shouldShowMessageTimestamp
    , stripBracketedTimestamps
    , timeContextGuidance
    ) where

import Agent.Loop (TurnInput, mapTurnInputUserText)
import Control.Exception.Safe (tryAny)
import Data.Char (isAsciiUpper, isDigit, isSpace, toLower)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime
    ( TimeZone
    , getCurrentTimeZone
    , getZonedTime
    , localDay
    , timeZoneName
    , utcToZonedTime
    , zonedTimeToLocalTime
    )
import System.Exit (ExitCode(..))
import System.Info (os)
import System.Process (readProcessWithExitCode)

data HourCycle = Hour12 | Hour24
    deriving (Eq, Show)

-- | Format @utc@ in @tz@ as @[YYYY-MM-DD HH:MM TZ]@.
renderMessageTimestamp :: TimeZone -> UTCTime -> Text
renderMessageTimestamp tz utc =
    let local = utcToZonedTime tz utc
    in Text.pack $
        formatTime defaultTimeLocale "[%Y-%m-%d %H:%M " local
            <> timeZoneName tz
            <> "]"

-- | Render the compact model-facing stamp. The date is only useful after the
-- conversation crosses a local calendar-day boundary.
renderContextualMessageTimestamp
    :: HourCycle
    -> TimeZone
    -> UTCTime
    -- ^ Conversation start.
    -> UTCTime
    -- ^ Message time.
    -> Text
renderContextualMessageTimestamp hourCycle tz startedAt messageAt =
    let
        startedLocal = utcToZonedTime tz startedAt
        messageLocal = utcToZonedTime tz messageAt
        includeDate =
            localDay (zonedTimeToLocalTime startedLocal)
                /= localDay (zonedTimeToLocalTime messageLocal)
        datePart
            | includeDate =
                formatTime defaultTimeLocale "%Y-%m-%d " messageLocal
            | otherwise = ""
        clockPart = case hourCycle of
            Hour12 -> formatTime defaultTimeLocale "%-I:%M%P" messageLocal
            Hour24 -> formatTime defaultTimeLocale "%H:%M" messageLocal
    in Text.pack $
        "[" <> datePart <> clockPart <> " " <> timeZoneName tz <> "]"

-- | Format a local time for the compact right-edge message label.
renderShortMessageTimestamp :: TimeZone -> UTCTime -> Text
renderShortMessageTimestamp tz utc =
    Text.pack $
        formatTime defaultTimeLocale "%-I:%M %p" (utcToZonedTime tz utc)

currentShortMessageTimestamp :: IO Text
currentShortMessageTimestamp =
    Text.pack . formatTime defaultTimeLocale "%-I:%M %p" <$> getZonedTime

-- | Append a local timestamp to @text@ (idempotent if one is already present).
stampUserText :: Text -> IO Text
stampUserText text = do
    now <- getCurrentTime
    tz <- getCurrentTimeZone
    pure (stampUserTextAt tz now text)

-- | Pure variant for tests.
stampUserTextAt :: TimeZone -> UTCTime -> Text -> Text
stampUserTextAt tz now text =
    let stripped = Text.stripEnd text
        stamp = renderMessageTimestamp tz now
    in if Text.null stripped
        then stamp
        else if hasTrailingTimestamp stripped
            then stripped
            else stripped <> " " <> stamp

-- | Stamp every human user payload. Tool results and other turn inputs are
-- left unchanged.
stampTurnInputs :: [TurnInput] -> IO [TurnInput]
stampTurnInputs inputs = do
    now <- getCurrentTime
    tz <- getCurrentTimeZone
    pure
        (map
            (mapTurnInputUserText (stampUserTextAt tz now))
            inputs)

-- | Stamp a turn only when more than one minute has elapsed since the previous
-- conversation activity.
stampTurnInputsSince
    :: UTCTime
    -- ^ Conversation start.
    -> Maybe UTCTime
    -- ^ Previous conversation activity.
    -> [TurnInput]
    -> IO [TurnInput]
stampTurnInputsSince startedAt previousAt inputs = do
    now <- getCurrentTime
    if not (shouldShowMessageTimestamp previousAt now)
        then pure inputs
        else do
            tz <- getCurrentTimeZone
            hourCycle <- systemHourCycle
            let stamp = renderContextualMessageTimestamp hourCycle tz startedAt now
            pure (map (mapTurnInputUserText (appendStamp stamp)) inputs)

shouldShowMessageTimestamp :: Maybe UTCTime -> UTCTime -> Bool
shouldShowMessageTimestamp Nothing _ = False
shouldShowMessageTimestamp (Just previousAt) now =
    diffUTCTime now previousAt > 60

appendStamp :: Text -> Text -> Text
appendStamp stamp text =
    let stripped = Text.stripEnd text
    in if Text.null stripped
        then stamp
        else if hasTrailingTimestamp stripped
            then stripped
            else stripped <> " " <> stamp

systemHourCycle :: IO HourCycle
systemHourCycle
    | os == "darwin" = do
        macHourCycle <- macOSHourCycle
        maybe localeHourCycle pure macHourCycle
    | otherwise = localeHourCycle

localeHourCycle :: IO HourCycle
localeHourCycle = do
    result <- tryAny (readProcessWithExitCode "locale" ["t_fmt"] "")
    pure $ case result of
        Right (ExitSuccess, patternText, _)
            | any (`Text.isInfixOf` Text.pack patternText)
                ["%I", "%l", "%r"] -> Hour12
        _ -> Hour24

macOSHourCycle :: IO (Maybe HourCycle)
macOSHourCycle = do
    forced <- readDefault "AppleICUForce24HourTime"
    case fmap (map toLower . trim) forced of
        Just value
            | value `elem` ["1", "true", "yes"] -> pure (Just Hour24)
            | value `elem` ["0", "false", "no"] -> pure (Just Hour12)
        _ -> do
            appleLocale <- readDefault "AppleLocale"
            pure (appleLocale >>= hourCycleForAppleLocale)
  where
    readDefault key = do
        result <-
            tryAny
                (readProcessWithExitCode
                    "defaults"
                    ["read", "NSGlobalDomain", key]
                    "")
        pure $ case result of
            Right (ExitSuccess, value, _) -> Just value
            _ -> Nothing

    hourCycleForAppleLocale locale
        | any (`Text.isInfixOf` Text.pack locale)
            ["_US", "_PH", "_CA", "_AU", "_NZ", "_IN"] =
            Just Hour12
        | otherwise = Nothing

    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

hasTrailingTimestamp :: Text -> Bool
hasTrailingTimestamp text =
    let stripped = Text.stripEnd text
    in case Text.breakOnEnd "[" stripped of
        (before, rest)
            | Text.null rest -> False
            -- 'breakOnEnd' keeps the '[' on @before@, so accept
            -- @"… ["@ or a stamp-only @"["@.
            | not (Text.isSuffixOf " [" before || before == "[") -> False
            | otherwise -> case matchTimestamp rest of
                Just leftover -> Text.null leftover
                Nothing -> False

-- | Remove belege-style @[YYYY-MM-DD HH:MM TZ]@ fragments the model may echo.
stripBracketedTimestamps :: Text -> Text
stripBracketedTimestamps = Text.strip . go
  where
    go :: Text -> Text
    go t = case Text.uncons t of
        Nothing -> t
        Just ('[', rest) -> case matchTimestamp rest of
            Just after -> go after
            Nothing -> Text.cons '[' (go rest)
        Just (c, rest) -> Text.cons c (go rest)

matchTimestamp :: Text -> Maybe Text
matchTimestamp s = do
    afterDate <- case Text.splitAt 10 s of
        (date, rest)
            | isDate date
            , Just (' ', after) <- Text.uncons rest -> Just after
        _ -> Just s
    afterTime <- matchClock afterDate
    afterSpace <- case Text.uncons afterTime of
        Just (' ', rest) -> Just rest
        _ -> Nothing
    let (tz, rest1) = Text.span isAsciiUpper afterSpace
    case Text.uncons rest1 of
        Just (']', rest2) | not (Text.null tz) -> Just rest2
        _ -> Nothing

matchClock :: Text -> Maybe Text
matchClock s =
    let
        (hour, afterHour) = Text.span isDigit s
        validHourWidth = Text.length hour == 1 || Text.length hour == 2
    in case Text.uncons afterHour of
        Just (':', afterColon)
            | validHourWidth
            , let minute = Text.take 2 afterColon
            , Text.length minute == 2
            , Text.all isDigit minute ->
                let afterMinute = Text.drop 2 afterColon
                in case Text.take 2 afterMinute of
                    suffix
                        | Text.toLower suffix `elem` ["am", "pm"] ->
                            Just (Text.drop 2 afterMinute)
                    _ | Text.length hour == 2 -> Just afterMinute
                    _ -> Nothing
        _ -> Nothing

isDate :: Text -> Bool
isDate d =
    Text.length d == 10
        && Text.all isDigit (Text.take 4 d)
        && Text.index d 4 == '-'
        && Text.all isDigit (Text.take 2 (Text.drop 5 d))
        && Text.index d 7 == '-'
        && Text.all isDigit (Text.take 2 (Text.drop 8 d))

-- | System-prompt note mirroring belege.ai Zeitkontext (English for this harness).
timeContextGuidance :: Text
timeContextGuidance =
    Text.unlines
        [ "Time context:"
        , "After pauses longer than one minute, user messages may end with a local timestamp such as [9:40pm EDT] or [21:40 CEST]."
        , "The date is included only after the conversation crosses into another local calendar day, for example [2026-09-01 9:40pm EDT]."
        , "Use those stamps to judge how much wall-clock time passed between turns."
        , "Never include such a bracketed timestamp in your own replies — it is metadata only, not part of the answer."
        , "When speaking times to the user, use the same local timezone — not UTC."
        ]
