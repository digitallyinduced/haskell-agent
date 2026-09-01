-- | Bundled release notes for the interactive changelog.
module Agent.CLI.Changelog
    ( loadReleaseNotes
    ) where

import Control.Exception.Safe (catchAny)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Paths_agent_cli (getDataFileName)

-- | Load the release notes shipped with this executable. Keeping the notes in
-- the package makes the changelog available without network access.
loadReleaseNotes :: IO Text
loadReleaseNotes = do
    path <- getDataFileName "data/CHANGELOG.md"
    Text.readFile path `catchAny` \_ ->
        pure "No release notes available (offline)."
