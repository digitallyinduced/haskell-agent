-- | Pure decoding for Kitty keyboard and terminal input sequences.
module Agent.CLI.Input.KeyDecoder
    ( kittyShift
    , kittyCtrl
    , kittySuper
    , kittyRelease
    , hasModifier
    , withoutKittyLockModifiers
    , parseKittyKey
    , parseKittyKeyFields
    , decodeKittyEditorKey
    , decodeKittyControl
    , decodeModifiedArrowKey
    , isClipboardPasteKey
    , isClipboardPasteCsiBody
    , isShiftEnterCsiBody
    , isShiftTabCsiBody
    , splitFields
    , listAt
    , stripFinal
    , readDecimal
    ) where

import Agent.CLI.Input.Types (EditorKey(..), KittyKey(..))
import Agent.CLI.Terminal (shiftEnterCsiBodies, shiftTabCsiBodies)
import Control.Monad (guard)
import Data.Bits ((.&.), complement)
import Data.Char (ord)
import Data.Maybe (fromMaybe)

kittyShift, kittyAlt, kittyCtrl, kittySuper, kittyRelease :: Int
kittyShift = 1
kittyAlt = 2
kittyCtrl = 4
kittySuper = 8
kittyRelease = 3

hasModifier :: Int -> Int -> Bool
hasModifier modifier modifiers = modifiers .&. modifier /= 0

-- Caps Lock and Num Lock describe terminal state, not shortcut modifiers.
withoutKittyLockModifiers :: Int -> Int
withoutKittyLockModifiers modifiers = modifiers .&. complement 192

isClipboardPasteKey :: Char -> Bool
isClipboardPasteKey = (== '\SYN')

isClipboardPasteCsiBody :: String -> Bool
isClipboardPasteCsiBody body =
    case parseKittyKey body of
        Just KittyKey{kittyCodepoint, kittyModifiers, kittyEvent}
            | kittyEvent /= kittyRelease ->
                kittyCodepoint == ord 'v'
                    && (hasModifier kittyCtrl kittyModifiers
                        || hasModifier kittySuper kittyModifiers)
        _ -> False

isShiftEnterCsiBody :: String -> Bool
isShiftEnterCsiBody body =
    body `elem` shiftEnterCsiBodies || isKittyShiftKey 13 body

isShiftTabCsiBody :: String -> Bool
isShiftTabCsiBody body =
    body `elem` shiftTabCsiBodies || isKittyShiftKey 9 body

isKittyShiftKey :: Int -> String -> Bool
isKittyShiftKey codepoint body =
    case parseKittyKey body of
        Just key ->
            key.kittyCodepoint == codepoint
                && withoutKittyLockModifiers key.kittyModifiers == kittyShift
                && key.kittyEvent /= kittyRelease
        Nothing -> False

parseKittyKey :: String -> Maybe KittyKey
parseKittyKey body = do
    raw <- stripFinal 'u' body
    parseKittyKeyFields raw

parseKittyKeyFields :: String -> Maybe KittyKey
parseKittyKeyFields raw = do
    let fields = splitFields ';' raw
        modifierField = fromMaybe "1" (listAt 1 fields)
        modifierParts = splitFields ':' modifierField
    guard (length fields <= 3 && length modifierParts <= 2)
    codeField <- listAt 0 fields
    let codeParts = splitFields ':' codeField
    guard (length codeParts <= 3)
    codepoint <- listAt 0 codeParts >>= readCodepoint
    mapM_ (\part -> if null part then Just () else () <$ readCodepoint part) (drop 1 codeParts)
    encodedModifiers <- listAt 0 modifierParts >>= readDefaultOne
    event <- maybe (Just 1) readDefaultOne (listAt 1 modifierParts)
    guard (encodedModifiers >= 1 && encodedModifiers <= 256 && event >= 1 && event <= 3)
    mapM_ (mapM_ readCodepoint . splitFields ':') (listAt 2 fields)
    pure KittyKey
        { kittyCodepoint = codepoint
        , kittyModifiers = encodedModifiers - 1
        , kittyEvent = event
        }
  where
    readDefaultOne "" = Just 1
    readDefaultOne value = readDecimal value
    readCodepoint value = do
        codepoint <- readDecimal value
        guard (codepoint <= 0x10ffff && not (codepoint >= 0xd800 && codepoint <= 0xdfff))
        pure codepoint

decodeKittyEditorKey :: String -> Maybe EditorKey
decodeKittyEditorKey body
    | isClipboardPasteCsiBody body = Just (EditorClipboardPaste Nothing)
    | otherwise = do
        KittyKey{kittyCodepoint, kittyModifiers, kittyEvent} <- parseKittyKey body
        if kittyEvent == kittyRelease
            then Just EditorIgnore
            else Just (decodeKittyControl kittyModifiers kittyCodepoint)

decodeKittyControl :: Int -> Int -> EditorKey
decodeKittyControl rawModifiers codepoint
    | codepoint == 13 && modifiers == kittyShift = EditorChar '\n'
    | codepoint == 9 && hasModifier kittyShift modifiers = EditorCycleMode
    | hasModifier kittyAlt modifiers && codepoint == ord 'b' = EditorWordLeft
    | hasModifier kittyAlt modifiers && codepoint == ord 'f' = EditorWordRight
    | codepoint == 127
        && any (`hasModifier` modifiers) [kittyAlt, kittyCtrl, kittySuper] = EditorKillWord
    | codepoint == 127 && modifiers == 0 = EditorBackspace
    | hasModifier kittyCtrl modifiers = case codepoint of
        97 -> EditorHome
        98 -> EditorLeft
        99 -> EditorInterrupt
        100 -> EditorEof
        101 -> EditorEnd
        102 -> EditorRight
        107 -> EditorKillEnd
        108 -> EditorClearScreen
        110 -> EditorDown
        112 -> EditorUp
        114 -> EditorDictate
        117 -> EditorKillStart
        119 -> EditorKillWord
        121 -> EditorYank
        _ -> EditorIgnore
    | codepoint == 27 = EditorEscape
    | otherwise = EditorIgnore
  where
    modifiers = withoutKittyLockModifiers rawModifiers

-- Modified arrows use CSI 1;modifier C/D, including Kitty event suffixes.
decodeModifiedArrowKey :: String -> Maybe EditorKey
decodeModifiedArrowKey body = do
    (direction, raw) <- case reverse body of
        'C' : rest -> Just (EditorWordRight, reverse rest)
        'D' : rest -> Just (EditorWordLeft, reverse rest)
        _ -> Nothing
    KittyKey{kittyCodepoint, kittyModifiers, kittyEvent} <- parseKittyKeyFields raw
    if kittyCodepoint == 1
        && any (`hasModifier` kittyModifiers) [kittyAlt, kittyCtrl]
        then Just (if kittyEvent == kittyRelease then EditorIgnore else direction)
        else Nothing

splitFields :: Eq a => a -> [a] -> [[a]]
splitFields separator = go
  where
    go xs =
        let (field, rest) = break (== separator) xs
        in field : case rest of
            [] -> []
            _ : remaining -> go remaining

listAt :: Int -> [a] -> Maybe a
listAt index xs
    | index < 0 = Nothing
    | otherwise = case drop index xs of
        value : _ -> Just value
        [] -> Nothing

stripFinal :: Eq a => a -> [a] -> Maybe [a]
stripFinal suffix xs =
    case reverse xs of
        lastValue : rest
            | lastValue == suffix -> Just (reverse rest)
        _ -> Nothing

readDecimal :: String -> Maybe Int
readDecimal input = do
    guard (not (null input) && all (\character -> character >= '0' && character <= '9') input)
    case reads input :: [(Integer, String)] of
        [(value, "")] | value <= toInteger (maxBound :: Int) -> Just (fromInteger value)
        _ -> Nothing
