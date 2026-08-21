-- | Semantic attributes for the retained fullscreen interface.
module Agent.CLI.TUI.Theme
    ( assistantAttr
    , baseAttr
    , borderActiveAttr
    , borderAttr
    , errorAttr
    , footerAttr
    , headerAttr
    , codeAttr
    , headingAttr
    , mutedAttr
    , selectedAttr
    , successAttr
    , thinkingAttr
    , toolAttr
    , userAttr
    , solarizedDark
    ) where

import Brick (AttrMap, AttrName, attrMap, attrName)
import qualified Graphics.Vty as V

baseAttr, headerAttr, footerAttr, mutedAttr :: AttrName
userAttr, assistantAttr, thinkingAttr, toolAttr :: AttrName
errorAttr, successAttr, selectedAttr, borderAttr, borderActiveAttr :: AttrName
headingAttr, codeAttr :: AttrName
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
        ]

rgb :: Int -> Int -> Int -> V.Color
rgb r g b =
    V.RGBColor
        (fromIntegral r)
        (fromIntegral g)
        (fromIntegral b)
