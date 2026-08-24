-- | Pure decoding for Kitty keyboard and terminal input sequences.
module Agent.CLI.Input.KeyDecoder
    ( kittyShift
    , kittyCtrl
    , kittySuper
    , kittyRelease
    , hasModifier
    , parseKittyKey
    , parseKittyKeyFields
    , decodeKittyEditorKey
    , decodeKittyControl
    , isClipboardPasteKey
    , isClipboardPasteCsiBody
    , isShiftEnterCsiBody
    , splitFields
    , listAt
    , stripFinal
    , readDecimal
    ) where

import Agent.CLI.Input.Types (EditorKey(..), KittyKey(..))
import Agent.CLI.Terminal (shiftEnterCsiBodies)
import Data.Bits ((.&.))
import Data.Char (ord)
import Data.Maybe (fromMaybe)

kittyShift, kittyCtrl, kittySuper, kittyRelease :: Int
kittyShift = 1
kittyCtrl = 4
kittySuper = 8
kittyRelease = 3

hasModifier :: Int -> Int -> Bool
hasModifier modifier modifiers = modifiers .&. modifier /= 0

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
isShiftEnterCsiBody body = body `elem` shiftEnterCsiBodies

parseKittyKey :: String -> Maybe KittyKey
parseKittyKey body = do
    raw <- stripFinal 'u' body
    parseKittyKeyFields raw

parseKittyKeyFields :: String -> Maybe KittyKey
parseKittyKeyFields raw = do
    let fields = splitFields ';' raw
        modifierField = fromMaybe "1" (listAt 1 fields)
        modifierParts = splitFields ':' modifierField
    codeField <- listAt 0 fields
    codepoint <- readDecimal (takeWhile (/= ':') codeField)
    encodedModifiers <- listAt 0 modifierParts >>= readDecimal
    event <- maybe (Just 1) readDecimal (listAt 1 modifierParts)
    pure KittyKey
        { kittyCodepoint = codepoint
        , kittyModifiers = max 0 (encodedModifiers - 1)
        , kittyEvent = event
        }

decodeKittyEditorKey :: String -> Maybe EditorKey
decodeKittyEditorKey body
    | isClipboardPasteCsiBody body = Just (EditorClipboardPaste Nothing)
    | otherwise = do
        KittyKey{kittyCodepoint, kittyModifiers, kittyEvent} <- parseKittyKey body
        if kittyEvent == kittyRelease
            then Just EditorIgnore
            else Just (decodeKittyControl kittyModifiers kittyCodepoint)

decodeKittyControl :: Int -> Int -> EditorKey
decodeKittyControl modifiers codepoint
    | codepoint == 9 && hasModifier kittyShift modifiers = EditorCycleMode
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
        117 -> EditorKillStart
        119 -> EditorKillWord
        121 -> EditorYank
        _ -> EditorIgnore
    | codepoint == 27 = EditorEscape
    | otherwise = EditorIgnore

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
readDecimal input =
    case reads input of
        [(value, "")] -> Just value
        _ -> Nothing
