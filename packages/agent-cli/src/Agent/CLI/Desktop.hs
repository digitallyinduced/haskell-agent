-- | Open persisted conversations in the native macOS desktop application.
module Agent.CLI.Desktop
    ( desktopBundleIdentifier
    , desktopConversationUrl
    , desktopOpenArguments
    , openDesktopConversation
    ) where

import Control.Exception.Safe
    ( displayException
    , tryAny
    )
import Data.Text (Text)
import qualified Data.Text as Text
import Network.URI (escapeURIString, isUnreserved)
import System.Exit (ExitCode(..))
import qualified System.Info as SystemInfo
import System.Process (readProcessWithExitCode)

desktopBundleIdentifier :: String
desktopBundleIdentifier = "dev.haskell-agent.macos"

-- | Deep-link contract consumed by the private native macOS application.
desktopConversationUrl :: Text -> Text
desktopConversationUrl sessionId =
    "haskell-agent://session/"
        <> Text.pack
            (escapeURIString isUnreserved (Text.unpack sessionId))

desktopOpenArguments :: Text -> [String]
desktopOpenArguments sessionId =
    [ "-b"
    , desktopBundleIdentifier
    , Text.unpack (desktopConversationUrl sessionId)
    ]

openDesktopConversation :: Text -> IO (Either Text ())
openDesktopConversation sessionId
    | SystemInfo.os /= "darwin" =
        pure (Left "/desktop is only available on macOS")
    | otherwise = do
        attempted <-
            tryAny
                (readProcessWithExitCode
                    "/usr/bin/open"
                    (desktopOpenArguments sessionId)
                    "")
        pure case attempted of
            Left exception ->
                Left
                    (desktopOpenFailure
                        (Text.pack (displayException exception)))
            Right (ExitSuccess, _, _) -> Right ()
            Right (ExitFailure _, _, errorOutput) ->
                Left
                    (desktopOpenFailure (Text.pack errorOutput))

desktopOpenFailure :: Text -> Text
desktopOpenFailure rawDetail =
    "could not open this conversation in Haskell Agent; \
    \make sure the desktop app is installed and up to date"
        <> case normalizedDetail of
            "" -> ""
            detail -> " (" <> detail <> ")"
  where
    normalizedDetail =
        Text.take 300 (Text.unwords (Text.words rawDetail))
