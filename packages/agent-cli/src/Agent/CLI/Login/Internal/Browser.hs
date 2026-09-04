module Agent.CLI.Login.Internal.Browser
    ( launchBrowserCommand
    , openBrowser
    ) where

import Agent.CLI.Environment (lookupNonEmpty)
import Control.Exception.Safe (tryAny)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Process
    ( CreateProcess(..)
    , StdStream(NoStream)
    , createProcess
    , proc
    )

openBrowser :: Text -> IO Bool
openBrowser url = do
    configured <- lookupNonEmpty "BROWSER"
    case configured of
        Just browser ->
            launchBrowserCommand browser url
        Nothing -> do
            opened <- launchBrowserCommand "open" url
            if opened
                then pure True
                else launchBrowserCommand "xdg-open" url

-- | Start the browser without waiting for its process to exit. Some browsers
-- stay in the foreground for the lifetime of the window; waiting here would
-- prevent the OAuth listener from ever accepting their callback.
launchBrowserCommand :: Text -> Text -> IO Bool
launchBrowserCommand command url =
    tryAny
        (createProcess
            (proc (Text.unpack command) [Text.unpack url])
                { std_in = NoStream
                , std_out = NoStream
                , std_err = NoStream
                , close_fds = True
                , create_group = True
                , new_session = True
                }) >>= \case
        Right _ -> pure True
        Left _ -> pure False
