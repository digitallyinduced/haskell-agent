-- | Small terminal capability helpers shared by CLI orchestration modules.
module Agent.CLI.Terminal
    ( resolveColor
    ) where

import System.Environment (lookupEnv)
import System.IO (Handle, hIsTerminalDevice)

-- | Color when the handle is a TTY and @NO_COLOR@ is unset.
resolveColor :: Handle -> IO Bool
resolveColor handle = do
    isTty <- hIsTerminalDevice handle
    noColor <- lookupEnv "NO_COLOR"
    pure (isTty && maybe True (const False) noColor)
