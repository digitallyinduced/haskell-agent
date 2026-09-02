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
    , inspectAttr
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
    , toolPathAttr
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
    , waveCellForTheme
    , waveForeground
    , waveForegroundFrom
    , wavePeakFor
    , wavePeakForTheme
    , waveTroughForTheme
    , waveTrough
    , waveTroughFromColorFgBg
    , waitingPulseAttrForTheme
    , ThemeKind(..)
    , parseThemeKind
    , themeKindText
    , themeKindRows
    , themeKindAt
    , themeAttrMap
    ) where

import Agent.Syntax (SyntaxClass(..))
import Agent.TUI.Motion (MotionMode(..), pulseBrightness)
import Brick (AttrMap, AttrName, attrMap, attrName)
import Control.Applicative ((<|>))
import Data.Bits ((.|.))
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read (readMaybe)
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes.Color (Color(..))

-- | Built-in fullscreen color schemes.  'Auto' delegates to the terminal's
-- configured palette, preserving the pre-theme behavior.
data ThemeKind
    = Auto
    | Midnight
    | Daylight
    | TokyoNight
    | RosePineMoon
    | OscuraMidnight
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

data ThemePalette = ThemePalette
    { themePaletteBackground :: !V.Color
    , themePaletteForeground :: !V.Color
    , themePaletteMuted :: !V.Color
    , themePaletteAccent :: !V.Color
    , themePaletteLink :: !V.Color
    }

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

fixedThemePalette :: ThemeKind -> Maybe ThemePalette
fixedThemePalette = \case
    Auto -> Nothing
    Midnight -> Just (ThemePalette
        (RGBColor 26 27 38) (RGBColor 220 220 230) (RGBColor 120 125 145)
        (RGBColor 187 154 247) (RGBColor 122 162 247))
    Daylight -> Just (ThemePalette
        (RGBColor 250 247 242) (RGBColor 45 42 46) (RGBColor 110 105 100)
        (RGBColor 144 80 150) (RGBColor 45 100 170))
    TokyoNight -> Just (ThemePalette
        (RGBColor 26 27 38) (RGBColor 192 202 245) (RGBColor 86 95 137)
        (RGBColor 187 154 247) (RGBColor 122 162 247))
    RosePineMoon -> Just (ThemePalette
        (RGBColor 25 23 36) (RGBColor 224 222 244) (RGBColor 144 140 170)
        (RGBColor 235 188 186) (RGBColor 196 167 231))
    OscuraMidnight -> Just (ThemePalette
        (RGBColor 8 12 18) (RGBColor 220 235 245) (RGBColor 105 130 145)
        (RGBColor 65 210 190) (RGBColor 80 170 255))

baseAttr, headerAttr, footerAttr, mutedAttr :: AttrName
userAttr, userMutedAttr, assistantAttr, thinkingAttr, thinkingBodyAttr, toolAttr, toolPathAttr :: AttrName
inspectAttr :: AttrName
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
inspectAttr = attrName "inspect"
toolPathAttr = attrName "tool-path"
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
        , (inspectAttr, palette V.brightBlack `V.withStyle` V.bold)
        , (toolPathAttr, palette V.brightYellow)
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
        , (inspectAttr, V.defAttr `V.withStyle` V.bold)
        , (toolPathAttr, V.defAttr `V.withStyle` V.underline)
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

-- | Resolve a selectable theme to a Brick attribute map.  The semantic
-- attribute names intentionally stay identical across themes so renderers do
-- not need to know which palette is active.
themeAttrMap :: ThemeKind -> AttrMap
themeAttrMap theme =
    maybe terminalDefault themePaletteAttrMap (fixedThemePalette theme)

themePaletteAttrMap :: ThemePalette -> AttrMap
themePaletteAttrMap palette =
    mkTheme
        palette.themePaletteBackground
        palette.themePaletteForeground
        palette.themePaletteMuted
        palette.themePaletteAccent
        palette.themePaletteLink

mkTheme :: V.Color -> V.Color -> V.Color -> V.Color -> V.Color -> AttrMap
mkTheme background foreground muted accent link =
    attrMap (V.defAttr `V.withBackColor` background)
        [ (baseAttr, base)
        , (headerAttr, base `V.withStyle` V.bold)
        , (footerAttr, mutedA)
        , (mutedAttr, mutedA)
        , (userAttr, panel `V.withStyle` V.bold)
        , (userMutedAttr, panelMuted)
        , (assistantAttr, base)
        , (thinkingAttr, accentA)
        , (thinkingBodyAttr, mutedA `V.withStyle` (V.dim .|. V.italic))
        , (waitingDimAttr, mutedA)
        , (waitingMidAttr, accentA)
        , (toolAttr, linkA)
        , (inspectAttr, mutedA `V.withStyle` V.bold)
        , (toolPathAttr, yellowA)
        , (todoPendingAttr, base)
        , (todoInProgressAttr, accentA `V.withStyle` V.bold)
        , (todoCompletedAttr, mutedA)
        , (todoCancelledAttr, mutedA `V.withStyle` V.strikethrough)
        , (errorAttr, redA `V.withStyle` V.bold)
        , (successAttr, greenA)
        , (completionFlashAttr, greenA `V.withStyle` V.bold)
        , (selectedAttr, panel `V.withStyle` V.bold)
        , (selectedMutedAttr, panelMuted)
        , (borderAttr, mutedA `V.withStyle` V.dim)
        , (borderActiveAttr, mutedA)
        , (headingAttr, accentA `V.withStyle` V.bold)
        , (codeAttr, linkA)
        -- Overlay dimming uses 'forceAttr', so it must preserve the page
        -- background instead of falling back to the terminal's colors.
        , (dimAttr, mutedA `V.withBackColor` background)
        , (emphasisAttr, base `V.withStyle` V.italic)
        , (inlineCodeAttr, linkA `V.withStyle` V.reverseVideo)
        , (lambdaDimAttr, mutedA)
        , (lambdaTrailAttr, mutedA)
        , (lambdaGlowAttr, base `V.withStyle` V.bold)
        , (lambdaSparkAttr, base `V.withStyle` V.bold)
        , (linkAttr, linkA `V.withStyle` V.underline)
        , (strongAttr, base `V.withStyle` V.bold)
        , (controlLinkAttr, mutedA `V.withBackColor` background)
        , (controlLinkHoverAttr,
            panel `V.withStyle` V.underline)
        , (controlLinkActiveAttr, panel `V.withStyle` V.bold)
        , (syntaxNormalAttr, base)
        , (syntaxKeywordAttr, accentA)
        , (syntaxTypeAttr, yellowA)
        , (syntaxFunctionAttr, linkA)
        , (syntaxVariableAttr, cyanA)
        , (syntaxStringAttr, greenA)
        , (syntaxNumberAttr, accentA)
        , (syntaxCommentAttr, mutedA `V.withStyle` V.italic)
        , (syntaxOperatorAttr, yellowA)
        , (syntaxAnnotationAttr, greenA)
        , (syntaxPreprocessorAttr, yellowA)
        , (syntaxWarningAttr, yellowA `V.withStyle` V.bold)
        , (syntaxErrorAttr, redA `V.withStyle` V.bold)
        ]
  where
    base =
        V.defAttr
            `V.withForeColor` foreground
            `V.withBackColor` background
    panel =
        V.defAttr
            `V.withForeColor` foreground
            `V.withBackColor` lighten background
    panelMuted =
        V.defAttr
            `V.withForeColor` muted
            `V.withBackColor` lighten background
    mutedA =
        V.defAttr
            `V.withForeColor` muted
            `V.withBackColor` background
    accentA =
        V.defAttr
            `V.withForeColor` accent
            `V.withBackColor` background
    linkA =
        V.defAttr
            `V.withForeColor` link
            `V.withBackColor` background
    redA =
        V.defAttr
            `V.withForeColor` (RGBColor 220 100 120)
            `V.withBackColor` background
    greenA =
        V.defAttr
            `V.withForeColor` (RGBColor 100 210 150)
            `V.withBackColor` background
    yellowA =
        V.defAttr
            `V.withForeColor` (RGBColor 230 190 100)
            `V.withBackColor` background
    cyanA =
        V.defAttr
            `V.withForeColor` (RGBColor 90 200 220)
            `V.withBackColor` background

    lighten (RGBColor r g b) =
        RGBColor
            (lightenChannel r)
            (lightenChannel g)
            (lightenChannel b)
    lighten color = color

    lightenChannel :: Word8 -> Word8
    lightenChannel channel =
        fromIntegral
            (min (255 :: Int) (fromIntegral channel + 14))

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

-- | Quiet gray-blue peak (#3b4261) so reasoning and inspection rails sit
-- behind action-tool rails.
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

-- | A fixed theme owns the page background; automatic mode keeps the
-- terminal-derived trough so rails disappear into the surrounding terminal.
waveTroughForTheme :: ThemeKind -> V.Color -> V.Color
waveTroughForTheme theme terminalTrough =
    maybe
        terminalTrough
        (\palette -> palette.themePaletteBackground)
        (fixedThemePalette theme)

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
waitingPulseAttr =
    waitingPulseAttrForTheme Auto

waitingPulseAttrForTheme
    :: ThemeKind
    -> Bool
    -> MotionMode
    -> V.Color
    -> Int
    -> V.Attr
waitingPulseAttrForTheme _ False _ _ _ =
    V.defAttr
waitingPulseAttrForTheme theme True mode trough elapsedMillis =
    themeBackground theme trough
        (if mode /= MotionFull
            then V.defAttr `V.withForeColor` (waitingPeakForTheme theme)
            else
                waveForegroundFrom
                    True
                    trough
                    (waitingPeakForTheme theme)
                    (0.3 + 0.7 * pulseBrightness elapsedMillis))

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

waveCellForTheme
    :: ThemeKind
    -> Bool
    -> V.Color
    -> V.Color
    -> Double
    -> Char
    -> V.Image
waveCellForTheme _ False _ _ _ glyph =
    V.char V.defAttr glyph
waveCellForTheme theme True trough peak brightness glyph
    | brightness <= 0.04 =
        V.char (themeBackground theme trough V.defAttr) ' '
    | otherwise =
        V.char
            (themeBackground theme trough
                (waveForegroundFrom True trough peak brightness))
            glyph

wavePeakFor :: AttrName -> V.Color
wavePeakFor attr
    | attr == thinkingAttr || attr == inspectAttr = thinkingWavePeak
    | otherwise = runningWavePeak

wavePeakForTheme :: ThemeKind -> AttrName -> V.Color
wavePeakForTheme theme attr =
    case fixedThemePalette theme of
        Nothing -> wavePeakFor attr
        Just palette
            | attr == thinkingAttr || attr == inspectAttr ->
                palette.themePaletteMuted
            | otherwise -> palette.themePaletteAccent

waitingPeakForTheme :: ThemeKind -> V.Color
waitingPeakForTheme theme =
    maybe
        waitingAccentPeak
        (\palette -> palette.themePaletteLink)
        (fixedThemePalette theme)

themeBackground :: ThemeKind -> V.Color -> V.Attr -> V.Attr
themeBackground theme background attr =
    case fixedThemePalette theme of
        Nothing -> attr
        Just _ -> attr `V.withBackColor` background

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
