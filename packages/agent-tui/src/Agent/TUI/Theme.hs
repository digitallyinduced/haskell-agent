-- | Semantic attributes for the retained fullscreen interface.
module Agent.TUI.Theme
    ( assistantAttr
    , baseAttr
    , borderActiveAttr
    , borderAttr
    , errorAttr
    , footerAttr
    , headerAttr
    , codeAttr
    , controlLinkActiveAttr
    , controlLinkAttr
    , controlLinkHoverAttr
    , dimAttr
    , emphasisAttr
    , headingAttr
    , inlineCodeAttr
    , lambdaDimAttr
    , lambdaGlowAttr
    , lambdaSparkAttr
    , lambdaTrailAttr
    , linkAttr
    , mutedAttr
    , selectedAttr
    , selectedMutedAttr
    , transcriptHoverAttr
    , transcriptHoverMutedAttr
    , transcriptHoverMutedCancelledAttr
    , transcriptHoverMutedItalicAttr
    , strongAttr
    , successAttr
    , syntaxAnnotationAttr
    , syntaxCommentAttr
    , syntaxErrorAttr
    , syntaxFunctionAttr
    , syntaxKeywordAttr
    , syntaxNormalAttr
    , syntaxNumberAttr
    , syntaxOperatorAttr
    , syntaxPreprocessorAttr
    , syntaxStringAttr
    , syntaxTypeAttr
    , syntaxVariableAttr
    , syntaxWarningAttr
    , syntaxClassAttr
    , thinkingAttr
    , thinkingBodyAttr
    , todoCancelledAttr
    , todoCompletedAttr
    , todoInProgressAttr
    , todoPendingAttr
    , toolAttr
    , userAttr
    , userMutedAttr
    , waitingDimAttr
    , waitingMidAttr
    , waitingPulseAttr
    , completionFlashAttr
    , monochrome
    , terminalDefault
    , interpolateForeground
    , runningWavePeak
    , thinkingWavePeak
    , waitingAccentPeak
    , waveCell
    , waveForeground
    , waveForegroundFrom
    , wavePeakFor
    , waveTrough
    , waveTroughFromColorFgBg
    ) where

import Agent.Syntax (SyntaxClass(..))
import Agent.TUI.Motion (MotionMode(..), pulseBrightness)
import Brick (AttrMap, AttrName, attrMap, attrName)
import Control.Applicative ((<|>))
import Data.Bits ((.|.))
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Text.Read (readMaybe)
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes.Color (Color(..))

baseAttr, headerAttr, footerAttr, mutedAttr :: AttrName
userAttr, userMutedAttr, assistantAttr, thinkingAttr, thinkingBodyAttr, toolAttr :: AttrName
todoPendingAttr, todoInProgressAttr, todoCompletedAttr, todoCancelledAttr :: AttrName
errorAttr, successAttr, selectedAttr, selectedMutedAttr, borderAttr, borderActiveAttr :: AttrName
transcriptHoverAttr :: AttrName
transcriptHoverMutedAttr, transcriptHoverMutedItalicAttr :: AttrName
transcriptHoverMutedCancelledAttr :: AttrName
headingAttr, codeAttr, dimAttr, emphasisAttr, inlineCodeAttr, linkAttr, strongAttr :: AttrName
controlLinkAttr, controlLinkHoverAttr, controlLinkActiveAttr :: AttrName
lambdaDimAttr, lambdaTrailAttr, lambdaGlowAttr, lambdaSparkAttr :: AttrName
syntaxNormalAttr, syntaxKeywordAttr, syntaxTypeAttr, syntaxFunctionAttr :: AttrName
syntaxVariableAttr, syntaxStringAttr, syntaxNumberAttr, syntaxCommentAttr :: AttrName
syntaxOperatorAttr, syntaxAnnotationAttr, syntaxPreprocessorAttr :: AttrName
syntaxWarningAttr, syntaxErrorAttr :: AttrName
waitingDimAttr, waitingMidAttr, completionFlashAttr :: AttrName
baseAttr = attrName "base"
headerAttr = attrName "header"
footerAttr = attrName "footer"
mutedAttr = attrName "muted"
userAttr = attrName "user"
userMutedAttr = attrName "user-muted"
assistantAttr = attrName "assistant"
thinkingAttr = attrName "thinking"
thinkingBodyAttr = attrName "thinking-body"
toolAttr = attrName "tool"
todoPendingAttr = attrName "todo-pending"
todoInProgressAttr = attrName "todo-in-progress"
todoCompletedAttr = attrName "todo-completed"
todoCancelledAttr = attrName "todo-cancelled"
errorAttr = attrName "error"
successAttr = attrName "success"
selectedAttr = attrName "selected"
selectedMutedAttr = attrName "selected-muted"
transcriptHoverAttr = attrName "transcript-hover"
transcriptHoverMutedAttr = attrName "transcript-hover-muted"
transcriptHoverMutedItalicAttr = attrName "transcript-hover-muted-italic"
transcriptHoverMutedCancelledAttr =
    attrName "transcript-hover-muted-cancelled"
borderAttr = attrName "border"
borderActiveAttr = attrName "border-active"
headingAttr = attrName "markdown-heading"
codeAttr = attrName "markdown-code"
dimAttr = attrName "dim"
emphasisAttr = attrName "markdown-emphasis"
inlineCodeAttr = attrName "markdown-inline-code"
lambdaDimAttr = attrName "lambda-dim"
lambdaTrailAttr = attrName "lambda-trail"
lambdaGlowAttr = attrName "lambda-glow"
lambdaSparkAttr = attrName "lambda-spark"
linkAttr = attrName "markdown-link"
strongAttr = attrName "markdown-strong"
controlLinkAttr = attrName "control-link"
controlLinkHoverAttr = attrName "control-link-hover"
controlLinkActiveAttr = attrName "control-link-active"
syntaxNormalAttr = attrName "syntax-normal"
syntaxKeywordAttr = attrName "syntax-keyword"
syntaxTypeAttr = attrName "syntax-type"
syntaxFunctionAttr = attrName "syntax-function"
syntaxVariableAttr = attrName "syntax-variable"
syntaxStringAttr = attrName "syntax-string"
syntaxNumberAttr = attrName "syntax-number"
syntaxCommentAttr = attrName "syntax-comment"
syntaxOperatorAttr = attrName "syntax-operator"
syntaxAnnotationAttr = attrName "syntax-annotation"
syntaxPreprocessorAttr = attrName "syntax-preprocessor"
syntaxWarningAttr = attrName "syntax-warning"
syntaxErrorAttr = attrName "syntax-error"

syntaxClassAttr :: SyntaxClass -> AttrName
syntaxClassAttr = \case
    SyntaxNormal -> syntaxNormalAttr
    SyntaxKeyword -> syntaxKeywordAttr
    SyntaxType -> syntaxTypeAttr
    SyntaxFunction -> syntaxFunctionAttr
    SyntaxVariable -> syntaxVariableAttr
    SyntaxString -> syntaxStringAttr
    SyntaxNumber -> syntaxNumberAttr
    SyntaxComment -> syntaxCommentAttr
    SyntaxOperator -> syntaxOperatorAttr
    SyntaxAnnotation -> syntaxAnnotationAttr
    SyntaxPreprocessor -> syntaxPreprocessorAttr
    SyntaxWarning -> syntaxWarningAttr
    SyntaxError -> syntaxErrorAttr
waitingDimAttr = attrName "waiting-dim"
waitingMidAttr = attrName "waiting-mid"
completionFlashAttr = attrName "completion-flash"

-- | Theme using only the terminal's default foreground/background and its
-- configurable ANSI palette. Ghostty themes therefore apply directly,
-- including automatic light/dark theme switching.
terminalDefault :: AttrMap
terminalDefault =
    attrMap V.defAttr
        [ (baseAttr, V.defAttr)
        , (headerAttr, V.defAttr `V.withStyle` V.bold)
        , (footerAttr, palette V.brightBlack)
        , (mutedAttr, palette V.brightBlack)
        , (userAttr, raisedPanelAttr `V.withStyle` V.bold)
        , (userMutedAttr, raisedPanelMutedAttr)
        , (assistantAttr, V.defAttr)
        , (thinkingAttr, palette V.yellow)
        , (thinkingBodyAttr, palette V.brightBlack `V.withStyle` (V.dim .|. V.italic))
        , (waitingDimAttr, palette V.brightBlack)
        , (waitingMidAttr, palette V.yellow)
        , (toolAttr, palette V.cyan)
        , (todoPendingAttr, V.defAttr)
        , (todoInProgressAttr, palette V.yellow `V.withStyle` V.bold)
        , (todoCompletedAttr, palette V.brightBlack)
        , (todoCancelledAttr, palette V.brightBlack `V.withStyle` V.strikethrough)
        , (errorAttr, palette V.red `V.withStyle` V.bold)
        , (successAttr, palette V.green)
        , (completionFlashAttr,
            palette V.brightGreen `V.withStyle` V.bold)
        , (selectedAttr, raisedPanelAttr `V.withStyle` V.bold)
        , (selectedMutedAttr, raisedPanelMutedAttr)
        , (transcriptHoverAttr, raisedPanelMutedAttr)
        , (transcriptHoverMutedAttr, V.defAttr `V.withStyle` V.dim)
        , (transcriptHoverMutedItalicAttr,
            V.defAttr `V.withStyle` (V.dim .|. V.italic))
        , (transcriptHoverMutedCancelledAttr,
            V.defAttr `V.withStyle` (V.dim .|. V.strikethrough))
        , (borderAttr, palette V.brightBlack `V.withStyle` V.dim)
        , (borderActiveAttr, palette V.brightBlack)
        , (headingAttr, palette V.magenta `V.withStyle` V.bold)
        , (codeAttr, palette V.cyan)
        , (dimAttr, palette V.brightBlack)
        , (emphasisAttr, V.defAttr `V.withStyle` V.italic)
        , (inlineCodeAttr, palette V.cyan `V.withStyle` V.reverseVideo)
        , (lambdaDimAttr, palette V.brightBlack)
        , (lambdaTrailAttr, palette V.brightBlack)
        , (lambdaGlowAttr, V.defAttr `V.withStyle` V.bold)
        , (lambdaSparkAttr,
            palette V.brightWhite `V.withStyle` V.bold)
        , (linkAttr, palette V.blue `V.withStyle` V.underline)
        , (strongAttr, V.defAttr `V.withStyle` V.bold)
        , (controlLinkAttr, palette V.brightBlack)
        , (controlLinkHoverAttr, V.defAttr `V.withStyle` V.underline)
        , (controlLinkActiveAttr,
            raisedPanelAttr `V.withStyle` V.bold)
        , (syntaxNormalAttr, palette V.cyan)
        , (syntaxKeywordAttr, palette V.magenta)
        , (syntaxTypeAttr, palette V.yellow)
        , (syntaxFunctionAttr, palette V.blue)
        , (syntaxVariableAttr, palette V.cyan)
        , (syntaxStringAttr, palette V.green)
        , (syntaxNumberAttr, palette V.brightMagenta)
        , (syntaxCommentAttr,
            palette V.brightBlack `V.withStyle` V.italic)
        , (syntaxOperatorAttr, palette V.brightYellow)
        , (syntaxAnnotationAttr, palette V.green)
        , (syntaxPreprocessorAttr, palette V.brightYellow)
        , (syntaxWarningAttr,
            palette V.yellow `V.withStyle` V.bold)
        , (syntaxErrorAttr,
            palette V.red `V.withStyle` V.bold)
        ]

monochrome :: AttrMap
monochrome =
    attrMap V.defAttr
        [ (baseAttr, V.defAttr)
        , (headerAttr, V.defAttr `V.withStyle` V.bold)
        , (footerAttr, V.defAttr)
        , (mutedAttr, V.defAttr)
        , (userAttr, V.defAttr `V.withStyle` V.bold)
        , (userMutedAttr, V.defAttr `V.withStyle` V.dim)
        , (assistantAttr, V.defAttr)
        , (thinkingAttr, V.defAttr)
        , (thinkingBodyAttr, V.defAttr `V.withStyle` (V.dim .|. V.italic))
        , (waitingDimAttr, V.defAttr)
        , (waitingMidAttr, V.defAttr)
        , (toolAttr, V.defAttr)
        , (todoPendingAttr, V.defAttr)
        , (todoInProgressAttr, V.defAttr `V.withStyle` V.bold)
        , (todoCompletedAttr, V.defAttr)
        , (todoCancelledAttr, V.defAttr `V.withStyle` V.strikethrough)
        , (errorAttr, V.defAttr `V.withStyle` V.bold)
        , (successAttr, V.defAttr)
        , (completionFlashAttr, V.defAttr `V.withStyle` V.bold)
        , (selectedAttr, V.defAttr
            `V.withStyle` (V.bold .|. V.reverseVideo))
        , (selectedMutedAttr, V.defAttr `V.withStyle` V.reverseVideo)
        , (transcriptHoverAttr, V.defAttr `V.withStyle` V.reverseVideo)
        , (transcriptHoverMutedAttr,
            V.defAttr `V.withStyle` (V.reverseVideo .|. V.dim))
        , (transcriptHoverMutedItalicAttr,
            V.defAttr `V.withStyle`
                (V.reverseVideo .|. V.dim .|. V.italic))
        , (transcriptHoverMutedCancelledAttr,
            V.defAttr `V.withStyle`
                (V.reverseVideo .|. V.dim .|. V.strikethrough))
        , (borderAttr, V.defAttr `V.withStyle` V.dim)
        , (borderActiveAttr, V.defAttr)
        , (headingAttr, V.defAttr `V.withStyle` V.bold)
        , (codeAttr, V.defAttr)
        , (dimAttr, V.defAttr)
        , (emphasisAttr, V.defAttr `V.withStyle` V.italic)
        , (inlineCodeAttr, V.defAttr `V.withStyle` V.reverseVideo)
        , (lambdaDimAttr, V.defAttr)
        , (lambdaTrailAttr, V.defAttr `V.withStyle` V.bold)
        , (lambdaGlowAttr, V.defAttr `V.withStyle` V.bold)
        , (lambdaSparkAttr, V.defAttr
            `V.withStyle` (V.bold .|. V.reverseVideo))
        , (linkAttr, V.defAttr `V.withStyle` V.underline)
        , (strongAttr, V.defAttr `V.withStyle` V.bold)
        , (controlLinkAttr, V.defAttr `V.withStyle` V.underline)
        , (controlLinkHoverAttr, V.defAttr
            `V.withStyle` (V.underline .|. V.bold))
        , (controlLinkActiveAttr, V.defAttr `V.withStyle` V.reverseVideo)
        , (syntaxNormalAttr, V.defAttr)
        , (syntaxKeywordAttr, V.defAttr `V.withStyle` V.bold)
        , (syntaxTypeAttr, V.defAttr `V.withStyle` V.bold)
        , (syntaxFunctionAttr, V.defAttr `V.withStyle` V.bold)
        , (syntaxVariableAttr, V.defAttr)
        , (syntaxStringAttr, V.defAttr)
        , (syntaxNumberAttr, V.defAttr)
        , (syntaxCommentAttr, V.defAttr `V.withStyle` V.italic)
        , (syntaxOperatorAttr, V.defAttr)
        , (syntaxAnnotationAttr, V.defAttr `V.withStyle` V.bold)
        , (syntaxPreprocessorAttr, V.defAttr `V.withStyle` V.bold)
        , (syntaxWarningAttr, V.defAttr `V.withStyle` V.bold)
        , (syntaxErrorAttr, V.defAttr
            `V.withStyle` (V.bold .|. V.reverseVideo))
        ]

palette :: V.Color -> V.Attr
palette = V.withForeColor V.defAttr

-- | A raised neutral surface made entirely from terminal palette slots.
-- Bright black supplies the surface, while the white slots stay readable on
-- themes whose default foreground is itself a muted gray.
raisedPanelAttr :: V.Attr
raisedPanelAttr =
    V.defAttr
        `V.withForeColor` V.brightWhite
        `V.withBackColor` V.brightBlack

raisedPanelMutedAttr :: V.Attr
raisedPanelMutedAttr =
    V.defAttr
        `V.withForeColor` V.white
        `V.withBackColor` V.brightBlack

-- | Default dark-page trough (#24283b). Live rails blend toward this so the
-- bar nearly vanishes when @COLORFGBG@ is unavailable.
waveTrough :: V.Color
waveTrough = RGBColor 36 40 59

-- | Magenta peak (#bb9af7) for live tool rails, distinct from static chrome.
runningWavePeak :: V.Color
runningWavePeak = RGBColor 187 154 247

-- | Quiet gray-blue peak (#3b4261) so reasoning rails sit behind tool rails.
thinkingWavePeak :: V.Color
thinkingWavePeak = RGBColor 59 66 97

-- | Blue peak (#7aa2f7) for "waiting on you" diamonds.
waitingAccentPeak :: V.Color
waitingAccentPeak = RGBColor 122 162 247

-- | Resolve the rail trough from @COLORFGBG@ (@fg;bg@ ANSI indexes). Missing
-- or unparsable values keep the default dark trough.
waveTroughFromColorFgBg :: Maybe String -> V.Color
waveTroughFromColorFgBg env =
    maybe waveTrough ansiIndexRgb (env >>= colorFgBgBackground)

colorFgBgBackground :: String -> Maybe Word8
colorFgBgBackground spec =
    case break (== ';') spec of
        (_, ';' : background) ->
            readMaybe (takeWhile (/= ';') background)
        _ ->
            Nothing

ansiIndexRgb :: Word8 -> V.Color
ansiIndexRgb index =
    let (red, green, blue)
            | index <= 15 = ansiRgb index
            | otherwise = color240Rgb index
    in RGBColor red green blue

waitingPulseAttr :: Bool -> MotionMode -> V.Color -> Int -> V.Attr
waitingPulseAttr False _ _ _ =
    V.defAttr
waitingPulseAttr True mode trough elapsedMillis
    | mode /= MotionFull =
        V.defAttr `V.withForeColor` waitingAccentPeak
    | otherwise =
        waveForegroundFrom
            True
            trough
            waitingAccentPeak
            (0.3 + 0.7 * pulseBrightness elapsedMillis)

-- | Paint a rail cell. Near-zero brightness uses a default-attr space so the
-- bar disappears into the real page even when the trough RGB is approximate.
-- Without color, keep the glyph on the default attribute so NO_COLOR is honored.
waveCell :: Bool -> V.Color -> V.Color -> Double -> Char -> V.Image
waveCell False _ _ _ glyph =
    V.char V.defAttr glyph
waveCell True trough peak brightness glyph
    | brightness <= 0.04 =
        V.char V.defAttr ' '
    | otherwise =
        V.char (waveForegroundFrom True trough peak brightness) glyph

wavePeakFor :: AttrName -> V.Color
wavePeakFor attr
    | attr == thinkingAttr = thinkingWavePeak
    | otherwise = runningWavePeak

-- | Paint a live accent cell at the given traveling-wave brightness.
--
-- When color is enabled, blends peak toward trough as 24-bit sRGB
-- (@RGBColor@, not Vty's linear conversion). Brightness 0 is the trough,
-- 1 is the peak. When color is disabled, keep the default attribute so
-- @NO_COLOR@ / monochrome maps are not bypassed by raw RGB.
waveForegroundFrom :: Bool -> V.Color -> V.Color -> Double -> V.Attr
waveForegroundFrom False _ _ _ =
    V.defAttr
waveForegroundFrom True trough peak brightness =
    case blendRgb trough peak (max 0.0 (min 1.0 brightness)) of
        Just blended -> V.defAttr `V.withForeColor` blended
        Nothing -> V.defAttr `V.withForeColor` peak

-- | Semantic-attribute variant used by tests and sheen helpers.
waveForeground :: V.Attr -> V.Attr -> Double -> V.Attr
waveForeground bgAttr fgAttr brightness =
    case attrColor (V.attrForeColor fgAttr) of
        Just fg ->
            let
                peak = brightenColor fg
                trough = fromMaybe waveTrough (backgroundColor bgAttr)
            in waveForegroundFrom True trough peak brightness
        Nothing ->
            styleSteps fgAttr (max 0.0 (min 1.0 brightness))

-- | Blend @from@'s foreground toward @to@'s foreground. Used for the empty
-- conversation sheen so the highlight sweeps instead of jumping between
-- four named attributes.
interpolateForeground :: Bool -> V.Attr -> V.Attr -> Double -> V.Attr
interpolateForeground False fromAttr toAttr opacity =
    if opacity >= 0.5 then toAttr else fromAttr
interpolateForeground True fromAttr toAttr opacity =
    case (attrColor (V.attrForeColor fromAttr), attrColor (V.attrForeColor toAttr)) of
        (Just from, Just to) ->
            case blendRgb from to opacity of
                Just blended -> withRgb toAttr blended
                Nothing ->
                    if opacity >= 0.5 then toAttr else fromAttr
        _ ->
            if opacity >= 0.5 then toAttr else fromAttr

backgroundColor :: V.Attr -> Maybe V.Color
backgroundColor attr =
    attrColor (V.attrBackColor attr) <|> attrColor (V.attrForeColor attr)

attrColor :: V.MaybeDefault V.Color -> Maybe V.Color
attrColor = \case
    V.SetTo color -> Just color
    _ -> Nothing

styleSteps :: V.Attr -> Double -> V.Attr
styleSteps attr brightness
    | brightness < 0.33 = attr `V.withStyle` V.dim
    | brightness > 0.66 = attr `V.withStyle` V.bold
    | otherwise = attr

brightenColor :: V.Color -> V.Color
brightenColor = \case
    ISOColor n | n < 8 -> ISOColor (n + 8)
    color -> color

withRgb :: V.Attr -> V.Color -> V.Attr
withRgb attr color =
    case attrColor (V.attrBackColor attr) of
        Just background ->
            V.defAttr
                `V.withForeColor` color
                `V.withBackColor` background
        Nothing ->
            V.defAttr `V.withForeColor` color

blendRgb :: V.Color -> V.Color -> Double -> Maybe V.Color
blendRgb base original opacity = do
    (baseR, baseG, baseB) <- colorRgb base
    (origR, origG, origB) <- colorRgb original
    let
        mix :: Word8 -> Word8 -> Word8
        mix from to =
            round
                (fromIntegral from
                    + (fromIntegral to - fromIntegral from) * clamped :: Double)
        clamped = max 0.0 (min 1.0 opacity)
    -- Emit sRGB bytes so truecolor terminals get a continuous gradient.
    pure (RGBColor (mix baseR origR) (mix baseG origG) (mix baseB origB))

colorRgb :: V.Color -> Maybe (Word8, Word8, Word8)
colorRgb = \case
    RGBColor red green blue -> Just (red, green, blue)
    ISOColor n -> Just (ansiRgb n)
    Color240 n -> Just (color240Rgb n)

-- | xterm defaults for ISO 0–15. Only used when blending truecolor cells;
-- palette-painted chrome stays on the named ANSI slots.
ansiRgb :: Word8 -> (Word8, Word8, Word8)
ansiRgb = \case
    0 -> (0, 0, 0)
    1 -> (128, 0, 0)
    2 -> (0, 128, 0)
    3 -> (128, 128, 0)
    4 -> (0, 0, 128)
    5 -> (128, 0, 128)
    6 -> (0, 128, 128)
    7 -> (192, 192, 192)
    8 -> (128, 128, 128)
    9 -> (255, 0, 0)
    10 -> (0, 255, 0)
    11 -> (255, 255, 0)
    12 -> (0, 0, 255)
    13 -> (255, 0, 255)
    14 -> (0, 255, 255)
    _ -> (255, 255, 255)

-- | 256-color cube + grayscale ramp.
color240Rgb :: Word8 -> (Word8, Word8, Word8)
color240Rgb index
    | index < 16 = ansiRgb index
    | index <= 231 =
        let
            n = index - 16
            cube = [0, 95, 135, 175, 215, 255] :: [Word8]
            at offset = cube !! fromIntegral offset
        in (at (n `div` 36), at ((n `mod` 36) `div` 6), at (n `mod` 6))
    | otherwise =
        let value = 8 + (index - 232) * 10
        in (value, value, value)
