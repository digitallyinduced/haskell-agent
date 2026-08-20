-- | Native terminal progress (Ghostty / Windows Terminal / ConEmu OSC 9;4).
--
-- Ghostty shows this as the macOS window/tab loading indicator (and a
-- colored progress overlay on the tab). State 3 is the indeterminate pulse
-- used while the agent is reasoning; state 0 removes it.
--
-- Unknown terminals ignore OSC 9;4. tmux needs DCS passthrough so the
-- outer emulator still sees the sequence.
module Agent.CLI.Progress
    ( osc9ProgressRemove
    , osc9ProgressIndeterminate
    , osc9ProgressSequence
    , wrapOscForTmux
    ) where

import Agent.CLI.ImagePreview (wrapTmuxPassthrough)
import Data.Text (Text)
import qualified Data.Text as Text

-- | OSC 9;4 states from ConEmu / Windows Terminal / Ghostty.
-- 0 remove, 1 set percent, 2 error, 3 indeterminate, 4 paused.
osc9ProgressSequence :: Int -> Maybe Int -> Text
osc9ProgressSequence state mPercent =
    "\ESC]9;4;"
        <> Text.pack (show state)
        <> case mPercent of
            Just percent -> ";" <> Text.pack (show (clampPercent percent))
            Nothing -> ""
        <> "\BEL"

osc9ProgressRemove :: Text
osc9ProgressRemove = osc9ProgressSequence 0 Nothing

-- | Indeterminate / busy pulse. Ghostty treats this as the native loading
-- spinner; Windows Terminal animates a cycling bar.
osc9ProgressIndeterminate :: Text
osc9ProgressIndeterminate = osc9ProgressSequence 3 Nothing

clampPercent :: Int -> Int
clampPercent n
    | n < 0 = 0
    | n > 100 = 100
    | otherwise = n

-- | tmux DCS passthrough so OSC 9;4 reaches Ghostty (or WT) outside tmux.
wrapOscForTmux :: Bool -> Text -> Text
wrapOscForTmux inTmux payload
    | inTmux = wrapTmuxPassthrough payload
    | otherwise = payload
