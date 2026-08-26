-- | Codex transport feature names shared by HTTP and WebSocket clients.
module Agent.OpenAI.Features
    ( remoteCompactionV2Feature
    , betaFeaturesHeaderValue
    ) where

import Data.Containers.ListUtils (nubOrd)
import Data.Text (Text)
import qualified Data.Text as Text

-- | Enables the @compaction_trigger@ protocol on normal Responses requests.
remoteCompactionV2Feature :: Text
remoteCompactionV2Feature = "remote_compaction_v2"

-- | Render a comma-separated @x-codex-beta-features@ value.
betaFeaturesHeaderValue :: [Text] -> Maybe Text
betaFeaturesHeaderValue features =
    case nubOrd (filter (not . Text.null) (map Text.strip features)) of
        [] -> Nothing
        values -> Just (Text.intercalate "," values)
