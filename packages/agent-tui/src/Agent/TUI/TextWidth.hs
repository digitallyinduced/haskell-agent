-- | Terminal-cell width classification shared by line editors and renderers.
module Agent.TUI.TextWidth
    ( charCellWidth
    , displayCharCellWidth
    , displayTerminalChar
    , displayTerminalText
    , graphemeCellWidth
    , graphemeClusters
    , splitTerminalGraphemeSuffix
    , terminalTextImage
    , clampGraphemeCursor
    , nextGraphemeBoundary
    , previousGraphemeBoundary
    , isWideCharacter
    ) where

import Data.Char
    ( GeneralCategory(..)
    , chr
    , generalCategory
    , ord
    )
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.Vty as V

-- | Width of a Unicode character before any control-character visualization.
--
-- Combining marks occupy no additional cells. Control and unassigned
-- characters are also zero-width; callers that render them as replacement
-- glyphs should measure the replacement instead.
charCellWidth :: Char -> Int
charCellWidth char
    | category `elem` [NonSpacingMark, SpacingCombiningMark, EnclosingMark] = 0
    | category `elem` [Control, Format, Surrogate, NotAssigned] = 0
    | isWideCharacter char = 2
    | otherwise = 1
  where
    category = generalCategory char

-- | Width used when a renderer displays terminal controls as visible
-- one-cell placeholders rather than treating them as zero-width input.
displayCharCellWidth :: Char -> Int
displayCharCellWidth char
    | char == '\n' || char == '\r' || char == '\t' = 1
    | code <= 0x1f || code == 0x7f = 1
    | code >= 0x80 && code <= 0x9f = 1
    | generalCategory char == Format = 1
    | otherwise = charCellWidth char
  where
    code = ord char

-- | Replace terminal control characters with visible, inert glyphs.
--
-- Newlines remain structural so width-aware renderers can split on them.
-- Every other replacement has the width reported by 'displayCharCellWidth'.
displayTerminalChar :: Char -> Text
displayTerminalChar char
    | char == '\n' = "\n"
    | char == '\r' = "↵"
    | char == '\t' = "⇥"
    | isVariationSelector char = ""
    | code >= 0 && code <= 0x1f =
        Text.singleton (chr (0x2400 + code))
    | code == 0x7f = "␡"
    | code >= 0x80 && code <= 0x9f = "�"
    | generalCategory char == Format = "�"
    | otherwise = Text.singleton char
  where
    code = ord char

displayTerminalText :: Text -> Text
displayTerminalText =
    Text.concat . map displayTerminalCluster . graphemeClusters
  where
    -- ZWJ and emoji tag characters are terminal formatting controls when
    -- they occur on their own, but are required source characters inside a
    -- recognized emoji grapheme. Other format characters remain inert
    -- replacement glyphs.
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

    -- Variation selectors only have meaning when attached to a compatible
    -- base character. Dropping a dangling selector prevents adjacent Vty
    -- text spans from accidentally forming a wider emoji grapheme after
    -- Brick has already split the source into separate widgets.
    displayOrdinaryClusterChar character
        | isVariationSelector character = ""
        | otherwise = displayTerminalChar character

    -- Vty 6 measures some modern terminal graphemes differently from the
    -- terminal itself (notably long family ZWJ sequences and keycaps). Such
    -- text cannot safely be placed in a Vty image: its span accounting and
    -- the terminal cursor would disagree. Preserve a representative glyph
    -- with the same target width instead.
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

-- | Split text into the terminal grapheme units that must not be wrapped or
-- truncated independently. This covers combining sequences and the emoji
-- constructions commonly emitted by terminals: ZWJ sequences, regional
-- indicator flags, skin-tone modifiers, keycaps, and emoji tag sequences.
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
            let (extensions, afterExtensions) =
                    takeClusterExtensions True rest
            in case afterExtensions of
                next : remaining
                    | isRegionalIndicator next ->
                        let (nextExtensions, afterNext) =
                                takeClusterExtensions True remaining
                        in ( character
                                : extensions
                                <> (next : nextExtensions)
                           , afterNext
                           )
                _ -> (character : extensions, afterExtensions)
        | otherwise =
            let (extensions, afterExtensions) =
                    takeClusterExtensions
                        (isEmojiCandidate character)
                        rest
                initial = character : extensions
            in takeEmojiJoins initial afterExtensions

    takeEmojiJoins cluster ('\x200d' : next : rest)
        | any isEmojiCandidate cluster
        , isEmojiCandidate next =
            let (extensions, remaining) =
                    takeClusterExtensions True rest
            in takeEmojiJoins
                (cluster <> ('\x200d' : next : extensions))
                remaining
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

-- | Hold a trailing cluster that may combine with the next streamed chunk.
--
-- This is intentionally narrower than general Unicode grapheme segmentation:
-- ordinary combining marks can still combine across terminal SGR sequences,
-- while emoji, flags, keycaps, and trailing ZWJ sequences must be sanitized as
-- a whole or they can be irreversibly replaced before the next chunk arrives.
splitTerminalGraphemeSuffix :: Text -> (Text, Text)
splitTerminalGraphemeSuffix text =
    case reverse (graphemeClusters text) of
        [] -> ("", "")
        lastCluster : previous
            | lastCluster == Text.singleton '\x200d'
            , previousCluster : _ <- previous ->
                splitAtSuffix (previousCluster <> lastCluster)
            | clusterMayExtend lastCluster ->
                splitAtSuffix lastCluster
            | otherwise -> (text, "")
  where
    splitAtSuffix suffix =
        (Text.dropEnd (Text.length suffix) text, suffix)

    clusterMayExtend cluster =
        Text.any isEmojiCandidate cluster
            || Text.any isKeycapBase cluster

-- | Width of one cluster as displayed by a modern terminal.
graphemeCellWidth :: Text -> Int
graphemeCellWidth cluster
    | Text.null cluster = 0
    | isRegionalIndicatorPair characters = 2
    | isKeycapCluster characters = 2
    | any isEmojiModifier characters = 2
    | hasEmojiTagSequence characters = 2
    | '\xfe0f' `elem` characters
    , any isEmojiCandidate characters = 2
    | '\x200d' `elem` characters
    , any isEmojiCandidate characters = 2
    | otherwise = sum (map charCellWidth characters)
  where
    characters = Text.unpack cluster

-- | Build a single-line Vty image after replacing any grapheme whose terminal
-- width disagrees with Vty's width model. This keeps Vty's span accounting,
-- our layout model, and the terminal cursor in agreement.
--
-- The input is sanitized with 'displayTerminalText' before it reaches Vty.
-- Callers should split structural newlines before constructing an image.
terminalTextImage :: V.Attr -> Text -> V.Image
terminalTextImage attr =
    V.text' attr . displayTerminalText

clampGraphemeCursor :: Text -> Int -> Int
clampGraphemeCursor text requested =
    last $
        takeWhile (<= bounded) (graphemeBoundaries text)
  where
    bounded = max 0 (min (Text.length text) requested)

previousGraphemeBoundary :: Text -> Int -> Int
previousGraphemeBoundary text requested =
    case takeWhile (< cursor) (graphemeBoundaries text) of
        [] -> 0
        boundaries -> last boundaries
  where
    cursor = clampGraphemeCursor text requested

nextGraphemeBoundary :: Text -> Int -> Int
nextGraphemeBoundary text requested =
    case dropWhile (<= bounded) (graphemeBoundaries text) of
        boundary : _ -> boundary
        [] -> Text.length text
  where
    bounded = max 0 (min (Text.length text) requested)

graphemeBoundaries :: Text -> [Int]
graphemeBoundaries =
    scanl
        (\offset cluster -> offset + Text.length cluster)
        0
        . graphemeClusters

isCombiningMark :: Char -> Bool
isCombiningMark character =
    generalCategory character
        `elem` [NonSpacingMark, SpacingCombiningMark, EnclosingMark]

isEmojiCandidate :: Char -> Bool
isEmojiCandidate character =
    let code = ord character
    in (code >= 0x1f000 && code <= 0x1faff)
        || (code >= 0x2600 && code <= 0x27ff)
        || code == 0x00a9
        || code == 0x00ae
        || code == 0x3030
        || code == 0x303d
        || code == 0x3297
        || code == 0x3299

isRegionalIndicator :: Char -> Bool
isRegionalIndicator character =
    let code = ord character
    in code >= 0x1f1e6 && code <= 0x1f1ff

isRegionalIndicatorPair :: [Char] -> Bool
isRegionalIndicatorPair characters =
    length (filter isRegionalIndicator characters) == 2

needsVtyCompatibleWidth :: Text -> Bool
needsVtyCompatibleWidth cluster =
    any isEmojiCandidate characters
        || isKeycapCluster characters
  where
    characters = Text.unpack cluster

isKeycapCluster :: [Char] -> Bool
isKeycapCluster characters =
    '\x20e3' `elem` characters
        && any isKeycapBase characters

isKeycapBase :: Char -> Bool
isKeycapBase character =
    (character >= '0' && character <= '9')
        || character == '#'
        || character == '*'

hasEmojiTagSequence :: [Char] -> Bool
hasEmojiTagSequence characters =
    any isEmojiTag characters
        && any isEmojiCandidate characters

isEmojiModifier :: Char -> Bool
isEmojiModifier character =
    let code = ord character
    in code >= 0x1f3fb && code <= 0x1f3ff

isEmojiTag :: Char -> Bool
isEmojiTag character =
    let code = ord character
    in code >= 0xe0020 && code <= 0xe007f

-- Variation selectors are meaningful only when they remain attached to the
-- character they modify. Keeping a standalone selector lets Vty merge it with
-- text from an adjacent widget, potentially changing that glyph's terminal
-- width after layout has already been calculated.
isVariationSelector :: Char -> Bool
isVariationSelector character =
    let code = ord character
    in (code >= 0xfe00 && code <= 0xfe0f)
        || (code >= 0xe0100 && code <= 0xe01ef)

-- | Report whether Vty's terminal-width table treats a character as wide.
--
-- Vty intentionally treats many standalone emoji code points as one cell,
-- while the harness has historically reserved two cells for the emoji block.
-- Keep that UI policy as a narrow override and delegate the rest of Unicode
-- width classification to the renderer's own table.
isWideCharacter :: Char -> Bool
isWideCharacter char =
    V.safeWcwidth char == 2
        || code >= 0x1f300 && code <= 0x1faff
  where
    code = ord char
