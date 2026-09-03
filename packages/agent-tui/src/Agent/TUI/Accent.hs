-- | Full-height scrollback accent rail, including a traveling brightness wave.
module Agent.TUI.Accent
    ( accentRail
    , highlightImageRow
    , highlightWidgetRow
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
import Data.Bits ((.|.))
import qualified Data.Text as Text
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Image.Internal as ImageInternal

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
    -> Theme.ThemeKind
    -> Maybe Int
    -> Widget n
    -> Widget n
accentRail glyphs attrName colorEnabled trough theme waveElapsed content =
    Widget Greedy Fixed do
        context <- getContext
        attr <- lookupAttrName attrName
        let innerWidth = max 0 (context.availWidth - 1)
        inner <- render (hLimit innerWidth content)
        let
            height = max 1 (V.imageHeight inner.image)
            glyph = accentBarChar glyphs
            peak = Theme.wavePeakForTheme theme attrName
            rows = waveRowsFor height
            rail = case waveElapsed of
                Nothing ->
                    V.charFill attr glyph 1 height
                Just elapsedMillis ->
                    V.vertCat
                        [ Theme.waveCellForTheme
                            theme
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
waveHeader
    :: AttrName
    -> Bool
    -> V.Color
    -> Theme.ThemeKind
    -> Int
    -> Text.Text
    -> Widget n
waveHeader attrName colorEnabled trough theme elapsedMillis title =
    case Text.uncons title of
        Nothing -> emptyWidget
        Just (glyph, rest) ->
            hBox
                [ raw
                    ( Theme.waveCellForTheme
                        theme
                        colorEnabled
                        trough
                        (Theme.wavePeakForTheme theme attrName)
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

-- | Tint one row of a child widget with a hover surface.
--
-- Collapsed transcript items can span several lines (title, preview, overflow).
-- Hover paints only the row under the pointer, leaving semantic foregrounds
-- in place.
highlightWidgetRow :: AttrName -> Int -> Widget n -> Widget n
highlightWidgetRow attrName row widget =
    Widget Greedy Fixed do
        result <- render widget
        hoverAttr <- lookupAttrName attrName
        pure result { image = highlightImageRow hoverAttr row result.image }

-- | Apply a hover surface to a single row of a rendered image.
--
-- Background fills become spaces so the band is visible; existing foregrounds
-- and styles are preserved unless they would vanish into the hover background.
highlightImageRow :: V.Attr -> Int -> V.Image -> V.Image
highlightImageRow attr row img
    | height <= 0 || row < 0 || row >= height = img
    | otherwise =
        above V.<-> applyHoverSurface attr line V.<-> below
  where
    height = V.imageHeight img
    above
        | row == 0 = V.emptyImage
        | otherwise = V.cropBottom row img
    line = V.cropTop 1 (V.cropBottom (row + 1) img)
    below =
        let rest = height - row - 1
        in if rest <= 0 then V.emptyImage else V.cropTop rest img

applyHoverSurface :: V.Attr -> V.Image -> V.Image
applyHoverSurface hover = \case
    ImageInternal.HorizText attr txt outputWidth charWidth ->
        ImageInternal.HorizText
            (applyHoverAttr hover attr)
            txt
            outputWidth
            charWidth
    ImageInternal.HorizJoin partLeft partRight outputWidth outputHeight ->
        ImageInternal.HorizJoin
            (applyHoverSurface hover partLeft)
            (applyHoverSurface hover partRight)
            outputWidth
            outputHeight
    ImageInternal.VertJoin partTop partBottom outputWidth outputHeight ->
        ImageInternal.VertJoin
            (applyHoverSurface hover partTop)
            (applyHoverSurface hover partBottom)
            outputWidth
            outputHeight
    ImageInternal.BGFill width height ->
        V.charFill (applyHoverAttr hover V.defAttr) ' ' width height
    ImageInternal.Crop croppedImage leftSkip topSkip outputWidth outputHeight ->
        ImageInternal.Crop
            (applyHoverSurface hover croppedImage)
            leftSkip
            topSkip
            outputWidth
            outputHeight
    ImageInternal.EmptyImage -> ImageInternal.EmptyImage

applyHoverAttr :: V.Attr -> V.Attr -> V.Attr
applyHoverAttr hover attr =
    addHoverStyle hover (addHoverBackground hover attr)

addHoverBackground :: V.Attr -> V.Attr -> V.Attr
addHoverBackground hover attr =
    case V.attrBackColor hover of
        V.SetTo bg ->
            let painted = attr `V.withBackColor` bg
            in case V.attrForeColor attr of
                V.SetTo fg | fg == bg ->
                    painted
                        `V.withForeColor` V.white
                        `V.withStyle` mergeStyle V.dim (V.attrStyle painted)
                _ -> painted
        _ -> attr

addHoverStyle :: V.Attr -> V.Attr -> V.Attr
addHoverStyle hover attr =
    case V.attrStyle hover of
        V.SetTo extra ->
            attr `V.withStyle` mergeStyle extra (V.attrStyle attr)
        _ -> attr

mergeStyle :: V.Style -> V.MaybeDefault V.Style -> V.Style
mergeStyle extra = \case
    V.SetTo existing -> extra .|. existing
    _ -> extra
