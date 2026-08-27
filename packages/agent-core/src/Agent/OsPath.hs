-- | Conversions at 'Text'-oriented API boundaries.
--
-- Import 'OsPath' and the standard encoding functions directly from
-- "System.OsPath".
module Agent.OsPath
    ( directoryChain
    , fromText
    , normalizeLexically
    , relativeDisplayPath
    , toText
    , unsafeToFilePath
    ) where

import Control.Exception.Safe (impureThrow)
import Data.Text (Text)
import qualified Data.Text as Text
import System.OsPath
    ( OsPath
    , decodeUtf
    , dropTrailingPathSeparator
    , isAbsolute
    , joinPath
    , makeRelative
    , normalise
    , splitDirectories
    , takeDirectory
    , (</>)
    )
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

-- | Resolve @.@ and @..@ without touching the filesystem.
--
-- Use only for lexical display or containment. Symlinks can make a normalized
-- spelling name a different target than the OS would resolve.
normalizeLexically :: OsPath -> OsPath
normalizeLexically path =
    case foldl' step [] (splitDirectories (normalise path)) of
        [] -> dot
        components -> dropTrailingPathSeparator (joinPath components)
  where
    step acc component
        | component == dot = acc
        | component == dotdot =
            case acc of
                [] -> [dotdot]
                [root] | isAbsolute root -> acc
                prefix
                    | last prefix == dotdot -> prefix <> [dotdot]
                    | otherwise -> init prefix
        | otherwise = acc <> [component]

-- | Present @path@ relative to @workspace@ when it is inside that tree.
-- Already-relative paths are rewritten through the workspace so @src/../a@
-- becomes @a@. Paths outside the workspace stay absolute after normalization.
relativeDisplayPath :: OsPath -> OsPath -> Text
relativeDisplayPath workspace path
    | Text.null (toText path) = ""
    | Text.null (toText workspace) = toText (normalizeLexically path)
    | otherwise =
        let root = dropTrailingPathSeparator (normalizeLexically workspace)
            candidate = normalizeLexically path
            absolute =
                normalizeLexically $
                    if isAbsolute candidate then candidate else root </> candidate
            relative = makeRelative root absolute
        in if isInsideRelative relative
            then
                if isDot relative
                    then "."
                    else toText relative
            else toText absolute
  where
    isDot rel = rel == dot || Text.null (toText rel)
    isInsideRelative rel =
        not (isAbsolute rel)
            && case splitDirectories rel of
                first : _ | first == dotdot -> False
                _ -> True

dot :: OsPath
dot = fromText "."

dotdot :: OsPath
dotdot = fromText ".."

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
