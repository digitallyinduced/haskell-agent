module Agent.Telegram.Internal.Text
    ( parseAllowedUsers
    , splitTelegramText
    ) where

import Agent.Telegram.Markdown (telegramRenderedLength)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read (readMaybe)

parseAllowedUsers :: Text -> Either Text (Set Integer)
parseAllowedUsers raw = do
    let values = filter (not . Text.null) $
            Text.strip <$> Text.splitOn "," raw
    users <- traverse parseUser values
    if null users
        then Left "TELEGRAM_ALLOWED_USERS must contain at least one numeric user ID"
        else Right (Set.fromList users)
  where
    parseUser value = case readMaybe (Text.unpack value) of
        Just userId | userId > 0 -> Right userId
        _ -> Left ("invalid Telegram user ID: " <> value)

splitTelegramText :: Int -> Text -> [Text]
splitTelegramText limit text
    | limit < 1 = []
    | Text.null text = []
    | telegramRenderedLength text <= limit = [text]
    | otherwise =
        let fitting = largestFittingPrefix text
            prefix = Text.take fitting text
            suffix = Text.drop fitting text
            splitAtBoundary
                | Text.null suffix = Text.length prefix
                | otherwise =
                    fromMaybe (Text.length prefix)
                        (preferredBoundary prefix)
            (chunk, rest) = Text.splitAt splitAtBoundary text
        in chunk : splitTelegramText limit rest
  where
    largestFittingPrefix value = search 1 (Text.length value)
      where
        search low high
            | low >= high = low
            | renderedLength midpoint <= limit = search midpoint high
            | otherwise = search low (midpoint - 1)
          where
            midpoint = low + (high - low + 1) `div` 2

        renderedLength =
            telegramRenderedLength . (`Text.take` value)

    preferredBoundary prefix =
        let reverseIndex character =
                (\index -> Text.length prefix - index - 1)
                    <$> Text.findIndex (== character) (Text.reverse prefix)
            newline = reverseIndex '\n'
            space = reverseIndex ' '
            boundary = max newline space
            minimumUseful = max 1 (limit `div` 2)
        in case boundary of
            Just index | index + 1 >= minimumUseful -> Just (index + 1)
            _ -> Nothing
