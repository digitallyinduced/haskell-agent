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
    , linkAttr
    , mutedAttr
    , selectedAttr
    , strongAttr
    , successAttr
    , thinkingAttr
    , toolAttr
    , userAttr
    , monochrome
    , solarizedDark
    ) where

import Brick (AttrMap, AttrName, attrMap, attrName)
import Data.Bits ((.|.))
import qualified Graphics.Vty as V

baseAttr, headerAttr, footerAttr, mutedAttr :: AttrName
userAttr, assistantAttr, thinkingAttr, toolAttr :: AttrName
errorAttr, successAttr, selectedAttr, borderAttr, borderActiveAttr :: AttrName
headingAttr, codeAttr, dimAttr, emphasisAttr, inlineCodeAttr, linkAttr, strongAttr :: AttrName
controlLinkAttr, controlLinkHoverAttr, controlLinkActiveAttr :: AttrName
baseAttr = attrName "base"
headerAttr = attrName "header"
footerAttr = attrName "footer"
mutedAttr = attrName "muted"
userAttr = attrName "user"
assistantAttr = attrName "assistant"
thinkingAttr = attrName "thinking"
toolAttr = attrName "tool"
errorAttr = attrName "error"
successAttr = attrName "success"
selectedAttr = attrName "selected"
borderAttr = attrName "border"
borderActiveAttr = attrName "border-active"
headingAttr = attrName "markdown-heading"
codeAttr = attrName "markdown-code"
dimAttr = attrName "dim"
emphasisAttr = attrName "markdown-emphasis"
inlineCodeAttr = attrName "markdown-inline-code"
linkAttr = attrName "markdown-link"
strongAttr = attrName "markdown-strong"
controlLinkAttr = attrName "control-link"
controlLinkHoverAttr = attrName "control-link-hover"
controlLinkActiveAttr = attrName "control-link-active"

solarizedDark :: AttrMap
solarizedDark =
    attrMap
        (V.defAttr
            `V.withForeColor` rgb 131 148 150
            `V.withBackColor` rgb 0 43 54)
        [ (baseAttr, V.defAttr
            `V.withForeColor` rgb 131 148 150
            `V.withBackColor` rgb 0 43 54)
        , (headerAttr, V.defAttr
            `V.withForeColor` rgb 147 161 161
            `V.withBackColor` rgb 0 43 54)
        , (footerAttr, V.defAttr
            `V.withForeColor` rgb 88 110 117
            `V.withBackColor` rgb 0 43 54)
        , (mutedAttr, V.defAttr
            `V.withForeColor` rgb 88 110 117
            `V.withBackColor` rgb 0 43 54)
        , (userAttr, V.defAttr
            `V.withForeColor` rgb 147 161 161
            `V.withBackColor` rgb 7 54 66)
        , (assistantAttr, V.defAttr
            `V.withForeColor` rgb 131 148 150
            `V.withBackColor` rgb 0 43 54)
        , (thinkingAttr, V.defAttr
            `V.withForeColor` rgb 181 137 0
            `V.withBackColor` rgb 0 43 54)
        , (toolAttr, V.defAttr
            `V.withForeColor` rgb 42 161 152
            `V.withBackColor` rgb 0 43 54)
        , (errorAttr, V.defAttr
            `V.withForeColor` rgb 220 50 47
            `V.withBackColor` rgb 0 43 54
            `V.withStyle` V.bold)
        , (successAttr, V.defAttr
            `V.withForeColor` rgb 133 153 0
            `V.withBackColor` rgb 0 43 54)
        , (selectedAttr, V.defAttr
            `V.withForeColor` rgb 147 161 161
            `V.withBackColor` rgb 7 54 66)
        , (borderAttr, V.defAttr
            `V.withForeColor` rgb 88 110 117
            `V.withBackColor` rgb 0 43 54)
        , (borderActiveAttr, V.defAttr
            `V.withForeColor` rgb 42 161 152
            `V.withBackColor` rgb 0 43 54)
        , (headingAttr, V.defAttr
            `V.withForeColor` rgb 211 54 130
            `V.withBackColor` rgb 0 43 54
            `V.withStyle` V.bold)
        , (codeAttr, V.defAttr
            `V.withForeColor` rgb 42 161 152
            `V.withBackColor` rgb 7 54 66)
        , (dimAttr, V.defAttr
            `V.withForeColor` rgb 88 110 117
            `V.withBackColor` rgb 0 43 54)
        , (emphasisAttr, V.defAttr
            `V.withForeColor` rgb 147 161 161
            `V.withBackColor` rgb 0 43 54
            `V.withStyle` V.italic)
        , (inlineCodeAttr, V.defAttr
            `V.withForeColor` rgb 42 161 152
            `V.withBackColor` rgb 7 54 66)
        , (linkAttr, V.defAttr
            `V.withForeColor` rgb 38 139 210
            `V.withBackColor` rgb 0 43 54
            `V.withStyle` V.underline)
        , (strongAttr, V.defAttr
            `V.withForeColor` rgb 147 161 161
            `V.withBackColor` rgb 0 43 54
            `V.withStyle` V.bold)
        , (controlLinkAttr, V.defAttr
            `V.withForeColor` rgb 38 139 210
            `V.withBackColor` rgb 0 43 54)
        , (controlLinkHoverAttr, V.defAttr
            `V.withForeColor` rgb 42 161 152
            `V.withBackColor` rgb 0 43 54
            `V.withStyle` V.underline)
        , (controlLinkActiveAttr, V.defAttr
            `V.withForeColor` rgb 0 43 54
            `V.withBackColor` rgb 38 139 210
            `V.withStyle` V.bold)
        ]

monochrome :: AttrMap
monochrome =
    attrMap V.defAttr
        [ (baseAttr, V.defAttr)
        , (headerAttr, V.defAttr `V.withStyle` V.bold)
        , (footerAttr, V.defAttr)
        , (mutedAttr, V.defAttr)
        , (userAttr, V.defAttr `V.withStyle` V.bold)
        , (assistantAttr, V.defAttr)
        , (thinkingAttr, V.defAttr)
        , (toolAttr, V.defAttr)
        , (errorAttr, V.defAttr `V.withStyle` V.bold)
        , (successAttr, V.defAttr)
        , (selectedAttr, V.defAttr `V.withStyle` V.reverseVideo)
        , (borderAttr, V.defAttr)
        , (borderActiveAttr, V.defAttr `V.withStyle` V.bold)
        , (headingAttr, V.defAttr `V.withStyle` V.bold)
        , (codeAttr, V.defAttr)
        , (dimAttr, V.defAttr)
        , (emphasisAttr, V.defAttr `V.withStyle` V.italic)
        , (inlineCodeAttr, V.defAttr `V.withStyle` V.reverseVideo)
        , (linkAttr, V.defAttr `V.withStyle` V.underline)
        , (strongAttr, V.defAttr `V.withStyle` V.bold)
        , (controlLinkAttr, V.defAttr `V.withStyle` V.underline)
        , (controlLinkHoverAttr, V.defAttr
            `V.withStyle` (V.underline .|. V.bold))
        , (controlLinkActiveAttr, V.defAttr `V.withStyle` V.reverseVideo)
        ]

rgb :: Int -> Int -> Int -> V.Color
rgb r g b =
    V.RGBColor
        (fromIntegral r)
        (fromIntegral g)
        (fromIntegral b)
