-- | Theme identifiers and configuration values, independent of any renderer.
module Agent.Theme
    ( ThemeKind(..)
    , themeKindText
    , parseThemeKind
    , themeKindRows
    , themeKindAt
    ) where

import Data.Text (Text)
import qualified Data.Text as Text

data ThemeKind
    = Auto
    | Midnight
    | Daylight
    | TokyoNight
    | RosePineMoon
    | OscuraMidnight
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

themeKindText :: ThemeKind -> Text
themeKindText = \case
    Auto -> "Auto"
    Midnight -> "Midnight"
    Daylight -> "Daylight"
    TokyoNight -> "Tokyo Night"
    RosePineMoon -> "Rose Pine Moon"
    OscuraMidnight -> "Oscura Midnight"

parseThemeKind :: Text -> Maybe ThemeKind
parseThemeKind raw =
    case Text.toCaseFold (Text.strip raw) of
        "auto" -> Just Auto
        "system" -> Just Auto
        "midnight" -> Just Midnight
        "night" -> Just Midnight
        "daylight" -> Just Daylight
        "day" -> Just Daylight
        "tokyonight" -> Just TokyoNight
        "tokyo-night" -> Just TokyoNight
        "tokyo night" -> Just TokyoNight
        "rosepine-moon" -> Just RosePineMoon
        "rose pine moon" -> Just RosePineMoon
        "rosé pine moon" -> Just RosePineMoon
        "oscuramidnight" -> Just OscuraMidnight
        "oscura-midnight" -> Just OscuraMidnight
        "oscura midnight" -> Just OscuraMidnight
        _ -> Nothing

themeKindRows :: [(Text, Text)]
themeKindRows =
    [ (themeKindText kind, themeDescription kind)
    | kind <- [Auto, Midnight, Daylight, TokyoNight, RosePineMoon, OscuraMidnight]
    ]
  where
    themeDescription = \case
        Auto -> "Use the terminal's native colors"
        Midnight -> "Dark blue-violet"
        Daylight -> "Light warm paper"
        TokyoNight -> "Dark indigo"
        RosePineMoon -> "Dark rose and lavender"
        OscuraMidnight -> "Deep black with cyan accents"

themeKindAt :: Int -> ThemeKind
themeKindAt index =
    [Auto, Midnight, Daylight, TokyoNight, RosePineMoon, OscuraMidnight]
        !! max 0 (min 5 index)
