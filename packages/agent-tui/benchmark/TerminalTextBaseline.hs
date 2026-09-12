{-# LANGUAGE OverloadedStrings #-}
-- Preserved displayTerminalText implementation before the ASCII fast path.
-- Keep segmentation and sanitation together, as in production: moving the
-- helper functions across a module boundary changes GHC's allocation profile.
module TerminalTextBaseline
    ( displayTerminalText, displayTerminalChar, graphemeCellWidth
    , graphemeClusters, charCellWidth, isWideCharacter
    ) where

import Data.Char (GeneralCategory(..), chr, ord, generalCategory)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.Vty as V

displayTerminalText :: Text -> Text
displayTerminalText =
    Text.concat . map displayTerminalCluster . graphemeClusters
  where
    displayTerminalCluster cluster
        | needsVtyCompatibleWidth cluster
        , V.safeWctwidth preserved /= graphemeCellWidth cluster =
            compatibleFallback cluster
        | otherwise = preserved
      where
        preserved
            | '\x20e3' `Text.elem` cluster
            , not (isKeycapCluster (Text.unpack cluster)) =
                Text.concatMap displayInvalidKeycapChar cluster
            | Text.length cluster > 1
            , Text.any isEmojiCandidate cluster =
                Text.concatMap displayEmojiClusterChar cluster
            | otherwise =
                Text.concatMap displayOrdinaryClusterChar cluster
    displayEmojiClusterChar character
        | character == '\x200d'
            || isEmojiTag character
            || isVariationSelector character =
                Text.singleton character
        | otherwise = displayTerminalChar character
    displayInvalidKeycapChar character
        | character == '\x20e3' = "�"
        | isVariationSelector character = ""
        | otherwise = displayTerminalChar character
    displayOrdinaryClusterChar character
        | isVariationSelector character = ""
        | otherwise = displayTerminalChar character
    compatibleFallback cluster =
        case Text.find
            (\character ->
                V.safeWcwidth character == targetWidth
                    && displayTerminalChar character
                        == Text.singleton character)
            cluster of
            Just character -> Text.singleton character
            Nothing ->
                case fullWidthAscii cluster of
                    Just character
                        | V.safeWcwidth character == targetWidth ->
                            Text.singleton character
                    _
                        | targetWidth == 2 -> "？"
                        | targetWidth == 0 -> ""
                        | otherwise -> "�"
      where
        targetWidth = graphemeCellWidth cluster
    fullWidthAscii =
        fmap (chr . (+ 0xfee0) . ord)
            . Text.find
                (\character ->
                    character >= '!' && character <= '~')

isEmojiCandidate :: Char -> Bool
isEmojiCandidate character =
    let code = ord character
    in (code >= 0x1f000 && code <= 0x1faff)
        || (code >= 0x2600 && code <= 0x27ff)
        || code == 0x00a9 || code == 0x00ae || code == 0x3030
        || code == 0x303d || code == 0x3297 || code == 0x3299

needsVtyCompatibleWidth :: Text -> Bool
needsVtyCompatibleWidth cluster =
    any isEmojiCandidate characters || isKeycapCluster characters
  where
    characters = Text.unpack cluster

isRegionalIndicatorPair :: [Char] -> Bool
isRegionalIndicatorPair characters =
    length (filter isRegionalIndicator characters) == 2

hasEmojiTagSequence :: [Char] -> Bool
hasEmojiTagSequence characters =
    any isEmojiTag characters && any isEmojiCandidate characters

isKeycapCluster :: [Char] -> Bool
isKeycapCluster characters =
    '\x20e3' `elem` characters && any isKeycapBase characters

isKeycapBase :: Char -> Bool
isKeycapBase character =
    (character >= '0' && character <= '9')
        || character == '#' || character == '*'

isEmojiTag :: Char -> Bool
isEmojiTag character = ord character >= 0xe0020 && ord character <= 0xe007f

isVariationSelector :: Char -> Bool
isVariationSelector character =
    let code = ord character
    in (code >= 0xfe00 && code <= 0xfe0f)
        || (code >= 0xe0100 && code <= 0xe01ef)

displayTerminalChar :: Char -> Text
displayTerminalChar char
    | char == '\n' = "\n"
    | char == '\r' = "↵"
    | char == '\t' = "⇥"
    | isVariationSelector char = ""
    | code >= 0 && code <= 0x1f = Text.singleton (chr (0x2400 + code))
    | code == 0x7f = "␡"
    | code >= 0x80 && code <= 0x9f = "�"
    | generalCategory char == Format = "�"
    | otherwise = Text.singleton char
  where
    code = ord char

graphemeClusters :: Text -> [Text]
graphemeClusters = map Text.pack . go . Text.unpack
  where
    go [] = []
    go input =
        let (cluster, rest) = takeCluster input
        in cluster : go rest
    takeCluster [] = ([], [])
    takeCluster (character : rest)
        | isRegionalIndicator character =
            let (extensions, afterExtensions) = takeClusterExtensions True rest
            in case afterExtensions of
                next : remaining
                    | isRegionalIndicator next ->
                        let (nextExtensions, afterNext) = takeClusterExtensions True remaining
                        in (character : extensions <> (next : nextExtensions), afterNext)
                _ -> (character : extensions, afterExtensions)
        | otherwise =
            let (extensions, afterExtensions) =
                    takeClusterExtensions (isEmojiCandidate character) rest
                initial = character : extensions
            in takeEmojiJoins initial afterExtensions
    takeEmojiJoins cluster ('\x200d' : next : rest)
        | any isEmojiCandidate cluster
        , isEmojiCandidate next =
            let (extensions, remaining) = takeClusterExtensions True rest
            in takeEmojiJoins (cluster <> ('\x200d' : next : extensions)) remaining
    takeEmojiJoins cluster rest = (cluster, rest)
    takeClusterExtensions allowEmoji = goExtensions
      where
        goExtensions (character : rest)
            | isCombiningMark character =
                let (extensions, remaining) = goExtensions rest
                in (character : extensions, remaining)
            | allowEmoji
            , isEmojiModifier character || isEmojiTag character =
                let (extensions, remaining) = goExtensions rest
                in (character : extensions, remaining)
        goExtensions remaining = ([], remaining)

isCombiningMark :: Char -> Bool
isCombiningMark character =
    generalCategory character `elem` [NonSpacingMark, SpacingCombiningMark, EnclosingMark]

isRegionalIndicator :: Char -> Bool
isRegionalIndicator character =
    let code = ord character
    in code >= 0x1f1e6 && code <= 0x1f1ff

isEmojiModifier :: Char -> Bool
isEmojiModifier character =
    let code = ord character
    in code >= 0x1f3fb && code <= 0x1f3ff

graphemeCellWidth :: Text -> Int
graphemeCellWidth cluster
    | Text.null cluster = 0
    | isRegionalIndicatorPair characters = 2
    | isKeycapCluster characters = 2
    | any isEmojiModifier characters = 2
    | hasEmojiTagSequence characters = 2
    | '\xfe0f' `elem` characters, any isEmojiCandidate characters = 2
    | '\x200d' `elem` characters, any isEmojiCandidate characters = 2
    | otherwise = sum (map charCellWidth characters)
  where
    characters = Text.unpack cluster

charCellWidth :: Char -> Int
charCellWidth char
    | category `elem` [NonSpacingMark, SpacingCombiningMark, EnclosingMark] = 0
    | category `elem` [Control, Format, Surrogate, NotAssigned] = 0
    | isWideCharacter char = 2
    | otherwise = 1
  where
    category = generalCategory char

isWideCharacter :: Char -> Bool
isWideCharacter char =
    V.safeWcwidth char == 2
        || code >= 0x1f300 && code <= 0x1faff
  where
    code = ord char
