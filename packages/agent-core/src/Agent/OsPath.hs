-- | Conversions at 'Text'-oriented API boundaries.
--
-- Import 'OsPath' and the standard encoding functions directly from
-- "System.OsPath".
module Agent.OsPath
    ( fromText
    , toText
    , unsafeToFilePath
    ) where

import Control.Exception.Safe (impureThrow)
import Data.Text (Text)
import qualified Data.Text as Text
import System.OsPath (OsPath, decodeUtf)
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

-- | Decode a UTF-encoded path for APIs that still require 'FilePath'.
--
-- Use only for paths known to have originated from UTF text. Invalid encoding
-- is treated as a programming error.
unsafeToFilePath :: OsPath -> FilePath
unsafeToFilePath = either impureThrow id . decodeUtf
