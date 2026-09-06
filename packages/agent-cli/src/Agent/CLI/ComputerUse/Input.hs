module Agent.CLI.ComputerUse.Input
    ( ComputerKey(..)
    , Modifier(..)
    , MouseButton(..)
    , NamedKey(..)
    , normalizeComputerName
    , parseComputerKeyCombination
    , parseModifiers
    , parseMouseButton
    , validateKeys
    ) where

import Data.Text (Text)
import qualified Data.Text as Text

data MouseButton
    = MouseLeft
    | MouseRight
    | MouseMiddle
    | MouseBack
    | MouseForward
    deriving (Eq, Show)

data Modifier
    = ModifierMeta
    | ModifierControl
    | ModifierAlt
    | ModifierShift
    | ModifierFunction
    deriving (Eq, Show)

data NamedKey
    = KeyEnter
    | KeyTab
    | KeySpace
    | KeyBackspace
    | KeyEscape
    | KeyMeta
    | KeyShift
    | KeyCapsLock
    | KeyAlt
    | KeyControl
    | KeyHome
    | KeyPageUp
    | KeyDelete
    | KeyEnd
    | KeyPageDown
    | KeyLeft
    | KeyRight
    | KeyDown
    | KeyUp
    deriving (Eq, Show)

data ComputerKey
    = ComputerNamedKey !NamedKey
    -- | An unmodified text key. Preserve case and Unicode exactly.
    | ComputerTextKey !Text
    -- | A printable key used as part of a shortcut. These keys deliberately
    -- match the ANSI virtual-key set supported by the macOS backend.
    | ComputerShortcutKey !Text
    deriving (Eq, Show)

parseMouseButton :: Text -> Either Text MouseButton
parseMouseButton raw =
    case normalizeComputerName raw of
        "left" -> Right MouseLeft
        "right" -> Right MouseRight
        "wheel" -> Right MouseMiddle
        "middle" -> Right MouseMiddle
        "back" -> Right MouseBack
        "forward" -> Right MouseForward
        unsupported ->
            Left ("Unsupported computer mouse button: " <> unsupported)

parseModifiers :: [Text] -> Either Text [Modifier]
parseModifiers = traverse \raw ->
    case normalizeComputerName raw of
        "cmd" -> Right ModifierMeta
        "command" -> Right ModifierMeta
        "meta" -> Right ModifierMeta
        "ctrl" -> Right ModifierControl
        "control" -> Right ModifierControl
        "alt" -> Right ModifierAlt
        "option" -> Right ModifierAlt
        "shift" -> Right ModifierShift
        "fn" -> Right ModifierFunction
        "function" -> Right ModifierFunction
        unsupported ->
            Left ("Unsupported computer modifier: " <> unsupported)

parseComputerKeyCombination
    :: [Text]
    -> Either Text ([Modifier], ComputerKey)
parseComputerKeyCombination [] =
    Left "Computer key combination is empty."
parseComputerKeyCombination rawKeys
    | Just err <- validateKeys rawKeys = Left err
    | otherwise = do
        modifiers <- parseModifiers (init rawKeys)
        let rawKey = Text.strip (last rawKeys)
            key = normalizeComputerName rawKey
        computerKey <-
            case namedKey key of
                Just value -> Right (ComputerNamedKey value)
                Nothing
                    | Text.length rawKey == 1
                    , null modifiers ->
                        Right (ComputerTextKey rawKey)
                    | Text.length rawKey == 1
                    , key `elem` shortcutKeys ->
                        Right (ComputerShortcutKey key)
                    | otherwise ->
                        Left ("Unsupported computer key: " <> rawKey)
        pure (modifiers, computerKey)

validateKeys :: [Text] -> Maybe Text
validateKeys keys
    | not (null (drop 16 keys)) =
        Just "Computer action exceeds the 16-key limit."
    | any (not . Text.null . Text.drop 64) keys =
        Just "Computer key name exceeds 64 characters."
    | otherwise = Nothing

namedKey :: Text -> Maybe NamedKey
namedKey = \case
    "enter" -> Just KeyEnter
    "return" -> Just KeyEnter
    "tab" -> Just KeyTab
    "space" -> Just KeySpace
    "backspace" -> Just KeyBackspace
    "escape" -> Just KeyEscape
    "esc" -> Just KeyEscape
    "command" -> Just KeyMeta
    "cmd" -> Just KeyMeta
    "meta" -> Just KeyMeta
    "shift" -> Just KeyShift
    "capslock" -> Just KeyCapsLock
    "caps_lock" -> Just KeyCapsLock
    "option" -> Just KeyAlt
    "alt" -> Just KeyAlt
    "control" -> Just KeyControl
    "ctrl" -> Just KeyControl
    "home" -> Just KeyHome
    "pageup" -> Just KeyPageUp
    "page_up" -> Just KeyPageUp
    "delete" -> Just KeyDelete
    "del" -> Just KeyDelete
    "end" -> Just KeyEnd
    "pagedown" -> Just KeyPageDown
    "page_down" -> Just KeyPageDown
    "left" -> Just KeyLeft
    "arrowleft" -> Just KeyLeft
    "arrow_left" -> Just KeyLeft
    "right" -> Just KeyRight
    "arrowright" -> Just KeyRight
    "arrow_right" -> Just KeyRight
    "down" -> Just KeyDown
    "arrowdown" -> Just KeyDown
    "arrow_down" -> Just KeyDown
    "up" -> Just KeyUp
    "arrowup" -> Just KeyUp
    "arrow_up" -> Just KeyUp
    _ -> Nothing

shortcutKeys :: [Text]
shortcutKeys =
    [ "a", "s", "d", "f", "h", "g", "z", "x", "c", "v", "b"
    , "q", "w", "e", "r", "y", "t"
    , "1", "2", "3", "4", "6", "5", "=", "9", "7", "-", "8", "0"
    , "]", "o", "u", "[", "i", "p", "l", "j", "'", "k", ";", "\\"
    , ",", "/", "n", "m", ".", "`"
    ]

normalizeComputerName :: Text -> Text
normalizeComputerName = Text.toLower . Text.strip
