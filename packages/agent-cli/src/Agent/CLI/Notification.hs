-- | Native terminal desktop notifications for human-attention points.
--
-- Ghostty and iTerm-compatible terminals implement OSC 9 as a desktop
-- notification. Unknown terminals ignore it. tmux needs DCS passthrough (and
-- @allow-passthrough on@) so the outer terminal receives the OSC sequence.
module Agent.CLI.Notification
    ( AttentionRequest(..)
    , attentionNotificationSequence
    , notifyAttention
    ) where

import Agent.CLI.ImagePreview (wrapTmuxPassthrough)
import Control.Exception.Safe (tryIO)
import Control.Monad (void, when)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import System.Environment (lookupEnv)
import System.IO (Handle, hFlush, hIsTerminalDevice)

data AttentionRequest
    = InputRequested
    | PermissionRequested
    | PlanModeRequested
    deriving (Eq, Show)

attentionNotificationSequence :: AttentionRequest -> Text
attentionNotificationSequence request =
    "\ESC]9;" <> notificationTitle request <> "\ESC\\"

notificationTitle :: AttentionRequest -> Text
notificationTitle = \case
    InputRequested -> "Haskell Agent: input requested"
    PermissionRequested -> "Haskell Agent: permission required"
    PlanModeRequested -> "Haskell Agent: plan mode requested"

-- | Notify only when the destination is a TTY, keeping pipes and redirected
-- stderr free of terminal control sequences. Notification failures must never
-- interfere with the prompt that follows.
notifyAttention :: Handle -> AttentionRequest -> IO ()
notifyAttention handle request = do
    tty <- hIsTerminalDevice handle
    when tty do
        inTmux <- isJust <$> lookupEnv "TMUX"
        let raw = attentionNotificationSequence request
            sequence_
                | inTmux = wrapTmuxPassthrough raw
                | otherwise = raw
        void $ tryIO do
            Text.hPutStr handle sequence_
            hFlush handle
