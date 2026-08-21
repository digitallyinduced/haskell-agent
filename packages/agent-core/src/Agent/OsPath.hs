-- | Common conversions at text-oriented API boundaries.
--
-- Filesystem-facing code should keep paths as 'OsPath'. Conversion back to
-- 'FilePath' is reserved for libraries such as @process@ that do not yet
-- expose an 'OsPath' API, and for human-readable output.
module Agent.OsPath
    ( OsPath
    , fromFilePath
    , fromText
    , toFilePath
    , toText
    ) where

import Data.Text (Text)
import qualified Data.Text as Text
import System.OsPath (OsPath)
import qualified System.OsPath as OsPath

fromFilePath :: FilePath -> OsPath
fromFilePath = OsPath.unsafeEncodeUtf

fromText :: Text -> OsPath
fromText = fromFilePath . Text.unpack

toFilePath :: OsPath -> FilePath
toFilePath path =
    either (const (show path)) id (OsPath.decodeUtf path)

toText :: OsPath -> Text
toText = Text.pack . toFilePath
