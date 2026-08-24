-- | Wall-clock stamps on model-facing user messages (belege.ai style).
--
-- Each user turn sent to the model ends with a local timestamp like
-- @[2026-08-20 16:45 CEST]@ so the agent can judge pauses between turns.
-- The REPL UI keeps the unstamped text; assistant replies are scrubbed if
-- the model echoes a stamp.
module Agent.CLI.Timestamp
    ( currentShortMessageTimestamp
    , renderMessageTimestamp
    , renderShortMessageTimestamp
    , stampUserText
    , stampUserTextAt
    , stampTurnInputs
    , stripBracketedTimestamps
    , timeContextGuidance
    ) where

import Agent.Loop (TurnInput(..))
import Data.Char (isAsciiUpper, isDigit)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime
    ( TimeZone
    , getCurrentTimeZone
    , getZonedTime
    , timeZoneName
    , utcToZonedTime
    )

-- | Format @utc@ in @tz@ as @[YYYY-MM-DD HH:MM TZ]@.
renderMessageTimestamp :: TimeZone -> UTCTime -> Text
renderMessageTimestamp tz utc =
    let local = utcToZonedTime tz utc
    in Text.pack $
        formatTime defaultTimeLocale "[%Y-%m-%d %H:%M " local
            <> timeZoneName tz
            <> "]"

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

-- | Stamp every human 'UserMessage' / 'UserMultimodal' payload. Tool results
-- and other turn inputs are left unchanged.
stampTurnInputs :: [TurnInput] -> IO [TurnInput]
stampTurnInputs inputs = do
    now <- getCurrentTime
    tz <- getCurrentTimeZone
    pure (map (stampOne tz now) inputs)
  where
    stampOne tz now = \case
        UserMessage text -> UserMessage (stampUserTextAt tz now text)
        UserMultimodal{userText, userImages} ->
            UserMultimodal
                { userText = stampUserTextAt tz now userText
                , userImages
                }
        other -> other

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
matchTimestamp s
    | Text.length s < 18 = Nothing
    | not (isDate (Text.take 10 s)) = Nothing
    | Text.index s 10 /= ' ' = Nothing
    | not (isTime (Text.take 5 (Text.drop 11 s))) = Nothing
    | Text.index s 16 /= ' ' = Nothing
    | otherwise =
        let (tz, rest1) = Text.span isAsciiUpper (Text.drop 17 s)
        in case Text.uncons rest1 of
            Just (']', rest2) | not (Text.null tz) -> Just rest2
            _ -> Nothing

isDate :: Text -> Bool
isDate d =
    Text.length d == 10
        && Text.all isDigit (Text.take 4 d)
        && Text.index d 4 == '-'
        && Text.all isDigit (Text.take 2 (Text.drop 5 d))
        && Text.index d 7 == '-'
        && Text.all isDigit (Text.take 2 (Text.drop 8 d))

isTime :: Text -> Bool
isTime t =
    Text.length t == 5
        && Text.all isDigit (Text.take 2 t)
        && Text.index t 2 == ':'
        && Text.all isDigit (Text.take 2 (Text.drop 3 t))

-- | System-prompt note mirroring belege.ai Zeitkontext (English for this harness).
timeContextGuidance :: Text
timeContextGuidance =
    Text.unlines
        [ "Time context:"
        , "User messages in the conversation history end with a timestamp like [YYYY-MM-DD HH:MM CET] or [YYYY-MM-DD HH:MM CEST] (local timezone, including DST)."
        , "Use those stamps to judge how much wall-clock time passed between turns."
        , "Never include such a bracketed timestamp in your own replies — it is metadata only, not part of the answer."
        , "When speaking times to the user, use the same local timezone — not UTC."
        ]
