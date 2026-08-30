{-# LANGUAGE CPP #-}

-- | Pure formatting for startup diagnostics and repository chrome.
module Agent.CLI.Startup.Format
    ( BuildInfo(..)
    , agentBuildInfo
    , formatBuildInfo
    , formatBuildInfoCompact
    , formatRepositoryPath
    , formatStartupDuration
    , formatStartupTimings
    ) where

import Agent.OsPath (toText)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (NominalDiffTime)
import Data.Version (showVersion)
import System.OsPath (OsPath)
import Text.Printf (printf)
import qualified Paths_agent_cli as Paths

#ifndef AGENT_BUILD_COMMIT
#define AGENT_BUILD_COMMIT "development"
#endif

#ifndef AGENT_BUILD_DATE
#define AGENT_BUILD_DATE "local"
#endif

-- | Identity embedded into an agent-cli build.
data BuildInfo = BuildInfo
    { buildVersion :: !Text
    , buildCommit :: !Text
    , buildDate :: !Text
    } deriving (Eq, Show)

agentBuildInfo :: BuildInfo
agentBuildInfo =
    BuildInfo
        { buildVersion = Text.pack (showVersion Paths.version)
        , buildCommit = Text.pack AGENT_BUILD_COMMIT
        , buildDate = Text.pack AGENT_BUILD_DATE
        }

formatBuildInfo :: BuildInfo -> Text
formatBuildInfo info =
    "agent-cli "
        <> info.buildVersion
        <> " (commit "
        <> info.buildCommit
        <> ", built "
        <> info.buildDate
        <> ")"

formatBuildInfoCompact :: BuildInfo -> Text
formatBuildInfoCompact info =
    "v"
        <> info.buildVersion
        <> " · "
        <> info.buildCommit
        <> " · "
        <> info.buildDate

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
