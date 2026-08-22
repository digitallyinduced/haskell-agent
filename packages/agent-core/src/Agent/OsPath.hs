-- | Conversions at 'Text'-oriented API boundaries.
--
-- Import 'OsPath' and the standard encoding functions directly from
-- "System.OsPath".
module Agent.OsPath
    ( fromText
    , toText
    ) where

import Data.Text (Text)
import qualified Data.Text as Text
import System.OsPath (OsPath)
import qualified System.OsPath as OsPath

-- | Pure UTF encoding for path values that originate as Unicode text.
fromText :: Text -> OsPath
fromText = OsPath.unsafeEncodeUtf . Text.unpack

-- | Render a path for human-readable output.
--
-- An undecodable path is represented with its escaped 'Show' form. This
-- fallback is suitable for diagnostics, never filesystem access.
toText :: OsPath -> Text
toText path =
    either (const (Text.pack (show path))) Text.pack (OsPath.decodeUtf path)
