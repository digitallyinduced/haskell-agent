-- | Discover AGENTS.md project instructions.
--
-- Discovery follows Codex's narrow rules (one file per directory from the
-- project root down to cwd, with an optional global home file). Dialect
-- packages own the model-facing formatting of the discovered documents.
module Agent.ProjectInstructions
    ( InstructionFile(..)
    , LoadedAgentsMd(..)
    , DiscoverOptions(..)
    , defaultDiscoverOptions
    , defaultProjectDocMaxBytes
    , discoverProjectInstructions
    , loadedInstructionFiles
    ) where

import Agent.FileRetry (retryOnFileBusy)
import Agent.OsPath (directoryChain, unsafeToFilePath)
import Control.Exception.Safe (tryAny)
import qualified Data.ByteString as BS
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncodingError
import qualified Data.Text.IO as Text
import System.Directory.OsPath (doesFileExist, doesPathExist)
import System.OsPath (OsPath, takeDirectory, unsafeEncodeUtf, (</>))

-- | One loaded instruction file and its absolute path.
data InstructionFile = InstructionFile
    { instructionPath :: !OsPath
    , instructionContent :: !Text
    } deriving (Eq, Show)

-- | Global home instructions plus project files from root -> cwd.
data LoadedAgentsMd = LoadedAgentsMd
    { loadedGlobal :: !(Maybe InstructionFile)
    , loadedProject :: ![InstructionFile]
    } deriving (Eq, Show)

loadedInstructionFiles :: LoadedAgentsMd -> [InstructionFile]
loadedInstructionFiles loaded =
    maybe id (:) loaded.loadedGlobal loaded.loadedProject

data DiscoverOptions = DiscoverOptions
    { discoverMaxBytes :: !Int
      -- ^ Soft budget across all loaded files. Content past the budget is
      -- truncated. Use @0@ to disable discovery.
    , discoverGlobalDir :: !(Maybe OsPath)
      -- ^ Optional home-scope directory (e.g. @~/.codex@ or @~/.grok@).
    , discoverRootMarkers :: ![OsPath]
      -- ^ Path segments that mark the project root. Default: @[".git"]@.
    } deriving (Eq, Show)

defaultProjectDocMaxBytes :: Int
defaultProjectDocMaxBytes = 32 * 1024

defaultDiscoverOptions :: DiscoverOptions
defaultDiscoverOptions = DiscoverOptions
    { discoverMaxBytes = defaultProjectDocMaxBytes
    , discoverGlobalDir = Nothing
    , discoverRootMarkers = [unsafeEncodeUtf ".git"]
    }

-- | Load global + project AGENTS.md files for @cwd@. Empty / unreadable files
-- are skipped. Project files are ordered root -> cwd.
discoverProjectInstructions :: DiscoverOptions -> OsPath -> IO LoadedAgentsMd
discoverProjectInstructions options cwd
    | options.discoverMaxBytes <= 0 =
        pure LoadedAgentsMd { loadedGlobal = Nothing, loadedProject = [] }
    | otherwise = do
        root <- findProjectRoot options.discoverRootMarkers cwd
        let dirs = directoryChain root cwd
        global <- maybe (pure Nothing) readPreferredAgentsMd options.discoverGlobalDir
        project <- mapMaybe id <$> traverse readPreferredAgentsMd dirs
        pure (applyByteBudget options.discoverMaxBytes LoadedAgentsMd
            { loadedGlobal = global
            , loadedProject = project
            })

findProjectRoot :: [OsPath] -> OsPath -> IO OsPath
findProjectRoot markers start = go start
  where
    go dir = do
        hit <- fmap or $ traverse (\marker -> doesPathExist (dir </> marker)) markers
        if hit
            then pure dir
            else do
                let parent = takeDirectory dir
                if parent == dir
                    then pure start
                    else go parent

-- | Prefer @AGENTS.override.md@ over @AGENTS.md@ in a directory.
readPreferredAgentsMd :: OsPath -> IO (Maybe InstructionFile)
readPreferredAgentsMd dir = do
    override <- readAgentsFile (dir </> unsafeEncodeUtf "AGENTS.override.md")
    case override of
        Just file -> pure (Just file)
        Nothing -> readAgentsFile (dir </> unsafeEncodeUtf "AGENTS.md")

readAgentsFile :: OsPath -> IO (Maybe InstructionFile)
readAgentsFile path = do
    exists <- doesFileExist path
    if not exists
        then pure Nothing
        else tryAny (retryOnFileBusy (Text.readFile (unsafeToFilePath path))) >>= \case
            Left _ -> pure Nothing
            Right text ->
                if Text.null (Text.strip text)
                    then pure Nothing
                    else pure $ Just InstructionFile
                        { instructionPath = path
                        , instructionContent = text
                        }

applyByteBudget :: Int -> LoadedAgentsMd -> LoadedAgentsMd
applyByteBudget maxBytes loaded =
    case go maxBytes (loadedInstructionFiles loaded) of
        [] -> LoadedAgentsMd Nothing []
        files@(first : rest) -> case loaded.loadedGlobal of
            Just global
                | first.instructionPath == global.instructionPath ->
                    LoadedAgentsMd (Just first) rest
            _ -> LoadedAgentsMd Nothing files
  where
    go _ [] = []
    go remaining (file : rest)
        | remaining <= 0 = []
        | otherwise =
            let content = file.instructionContent
                encoded = TextEncoding.encodeUtf8 content
                size = BS.length encoded
            in if size <= remaining
                then file : go (remaining - size) rest
                else
                    let truncated = TextEncoding.decodeUtf8With TextEncodingError.ignore
                            (BS.take remaining encoded)
                    in [file { instructionContent = truncated }]
