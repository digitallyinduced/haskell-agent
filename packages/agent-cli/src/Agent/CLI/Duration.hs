-- | Compact human-readable durations used by CLI status and error messages.
module Agent.CLI.Duration
    ( formatDuration
    ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (NominalDiffTime)

formatDuration :: NominalDiffTime -> Text
formatDuration delta =
    let seconds = max 0 (round delta :: Int)
        hours = seconds `div` 3600
        minutes = (seconds `mod` 3600) `div` 60
    in if hours >= 24
        then Text.pack (show (hours `div` 24))
            <> "d "
            <> Text.pack (show (hours `mod` 24))
            <> "h"
        else if hours > 0
            then Text.pack (show hours) <> "h " <> Text.pack (show minutes) <> "m"
            else if minutes > 0
                then Text.pack (show minutes) <> "m"
                else Text.pack (show seconds) <> "s"
