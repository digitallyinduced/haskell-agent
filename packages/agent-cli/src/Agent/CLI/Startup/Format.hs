-- | Pure formatting for startup diagnostics and repository chrome.
module Agent.CLI.Startup.Format
    ( formatRepositoryPath
    , formatStartupDuration
    , formatStartupTimings
    ) where

import Agent.OsPath (toText)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (NominalDiffTime)
import System.OsPath (OsPath)
import Text.Printf (printf)

formatStartupTimings :: [(Text, NominalDiffTime)] -> Text
formatStartupTimings timings =
    "startup: "
        <> Text.intercalate " · "
            [ label <> " " <> formatStartupDuration elapsed
            | (label, elapsed) <- sortOn snd timings
            ]

formatStartupDuration :: NominalDiffTime -> Text
formatStartupDuration elapsed
    | elapsed < 1 =
        Text.pack (show (round (elapsed * 1000) :: Int)) <> "ms"
    | otherwise =
        Text.pack (printf "%.2fs" (realToFrac elapsed :: Double))

formatRepositoryPath :: OsPath -> OsPath -> Text
formatRepositoryPath home cwd
    | cwdText == homeText = "~"
    | homePrefix `Text.isPrefixOf` cwdText =
        "~/" <> Text.drop (Text.length homePrefix) cwdText
    | otherwise = cwdText
  where
    homeText = Text.dropWhileEnd (== '/') (toText home)
    homePrefix = homeText <> "/"
    cwdText = toText cwd
