-- | Full-height scrollback accent rail, including a traveling brightness wave.
module Agent.TUI.Accent
    ( accentRail
    , waveHeader
    ) where

import Agent.TUI.Motion
    ( MotionGlyphSet
    , accentBarGlyph
    , waveBrightness
    , waveRowsFor
    )
import qualified Agent.TUI.Theme as Theme
import Brick
import qualified Data.Text as Text
import qualified Graphics.Vty as V

-- | Paint a one-column rail beside @content@, matching that widget's height.
--
-- The rail is Fixed vertically so it can live inside a vertical viewport.
-- @Nothing@ paints a static accent. @Just elapsedMillis@ walks a brightness
-- wave down the bar for running or streaming blocks.
accentRail
    :: MotionGlyphSet
    -> AttrName
    -> Bool
    -> V.Color
    -> Maybe Int
    -> Widget n
    -> Widget n
accentRail glyphs attrName colorEnabled trough waveElapsed content =
    Widget Greedy Fixed do
        context <- getContext
        attr <- lookupAttrName attrName
        let innerWidth = max 0 (context.availWidth - 1)
        inner <- render (hLimit innerWidth content)
        let
            height = max 1 (V.imageHeight inner.image)
            glyph = accentBarChar glyphs
            peak = Theme.wavePeakFor attrName
            rows = waveRowsFor height
            rail = case waveElapsed of
                Nothing ->
                    V.charFill attr glyph 1 height
                Just elapsedMillis ->
                    V.vertCat
                        [ Theme.waveCell
                            colorEnabled
                            trough
                            peak
                            (waveBrightness elapsedMillis row rows)
                            glyph
                        | row <- [0 .. height - 1]
                        ]
            image = V.horizJoin rail inner.image
        pure (addResultOffset (Location (1, 0)) inner) { image = image }

-- | Live header: the leading spinner rides the same wave as row 0 of the
-- rail; the rest of the title stays muted so the rail is what moves.
waveHeader :: AttrName -> Bool -> V.Color -> Int -> Text.Text -> Widget n
waveHeader attrName colorEnabled trough elapsedMillis title =
    case Text.uncons title of
        Nothing -> emptyWidget
        Just (glyph, rest) ->
            hBox
                [ raw
                    ( Theme.waveCell
                        colorEnabled
                        trough
                        (Theme.wavePeakFor attrName)
                        (waveBrightness elapsedMillis 0 (waveRowsFor 1))
                        glyph
                    )
                , withAttr Theme.mutedAttr (txtWrap rest)
                ]

accentBarChar :: MotionGlyphSet -> Char
accentBarChar glyphs =
    case Text.uncons (accentBarGlyph glyphs) of
        Just (character, _) -> character
        Nothing -> '|'
