-- | Conversions at 'Text'-oriented API boundaries.
--
-- Import 'OsPath' and the standard encoding functions directly from
-- "System.OsPath".
module Agent.OsPath
    ( directoryChain
    , fromText
    , toText
    , unsafeToFilePath
    ) where

import Control.Exception.Safe (impureThrow)
import Data.Text (Text)
import qualified Data.Text as Text
import System.OsPath (OsPath, decodeUtf, takeDirectory)
import qualified System.OsPath as OsPath

-- | Directories from @root@ through @cwd@, inclusive.
--
-- If @cwd@ is not contained by @root@, return only @cwd@. Callers use this
-- fallback to avoid discovering files from unrelated filesystem ancestors.
-- Inputs are expected to be absolute and normalized by the caller.
directoryChain :: OsPath -> OsPath -> [OsPath]
directoryChain root cwd =
    maybe [cwd] reverse (walk cwd)
  where
    walk dir
        | dir == root = Just [dir]
        | parent == dir = Nothing
        | otherwise = (dir :) <$> walk parent
      where
        parent = takeDirectory dir

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
