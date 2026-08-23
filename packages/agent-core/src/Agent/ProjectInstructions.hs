-- | Discover project instructions.
--
-- Codex keeps its narrow one-file-per-directory discovery contract. Grok-style
-- homes use the broader Grok Build compatibility surface: common Claude/agent
-- filenames plus vendor rules directories. Dialect packages own model-facing
-- formatting of the discovered documents.
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
import Agent.OsPath (directoryChain, toText, unsafeToFilePath)
import Control.Exception.Safe (tryAny)
import Control.Monad (filterM, foldM)
import qualified Data.ByteString as BS
import Data.List (sort)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncodingError
import qualified Data.Text.IO as Text
import System.Directory.OsPath
    ( canonicalizePath
    , doesDirectoryExist
    , doesFileExist
    , doesPathExist
    , listDirectory
    )
import System.OsPath
    ( OsPath
    , takeDirectory
    , takeExtension
    , takeFileName
    , unsafeEncodeUtf
    , (</>)
    )

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
      -- ^ Optional home-scope directory (e.g. @~/.codex@, @~/.grok@, or
      -- @~/.claude@).
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

-- | Load global + project instruction files for @cwd@. Empty / unreadable
-- files are skipped. Project files are ordered root -> cwd.
--
-- A @.codex@ global directory selects Codex's narrow discovery contract.
-- Other homes (including @.grok@, @.claude@, and @.haskell-agent@), and calls
-- without a global directory, use Grok-compatible discovery.
discoverProjectInstructions :: DiscoverOptions -> OsPath -> IO LoadedAgentsMd
discoverProjectInstructions options cwd
    | options.discoverMaxBytes <= 0 =
        pure LoadedAgentsMd { loadedGlobal = Nothing, loadedProject = [] }
    | otherwise = do
        root <- findProjectRoot options.discoverRootMarkers cwd
        let dirs = directoryChain root cwd
        loaded <-
            if usesCodexDiscovery options
                then do
                    global <-
                        maybe (pure Nothing) readPreferredAgentsMd
                            options.discoverGlobalDir
                    project <- mapMaybe id <$> traverse readPreferredAgentsMd dirs
                    pure LoadedAgentsMd
                        { loadedGlobal = global
                        , loadedProject = project
                        }
                else discoverGrokInstructions options dirs
        pure (applyByteBudget options.discoverMaxBytes loaded)

usesCodexDiscovery :: DiscoverOptions -> Bool
usesCodexDiscovery options =
    maybe False
        ((== unsafeEncodeUtf ".codex") . takeFileName)
        options.discoverGlobalDir

discoverGrokInstructions
    :: DiscoverOptions
    -> [OsPath]
    -> IO LoadedAgentsMd
discoverGrokInstructions options dirs = do
    home <- maybe (pure []) readGrokHomeInstructions options.discoverGlobalDir
    project <- concat <$> traverse readGrokDirectoryInstructions dirs
    combined <- dedupeInstructionFiles (home <> project)
    pure $ case (home, combined) of
        ([], _) -> LoadedAgentsMd
            { loadedGlobal = Nothing
            , loadedProject = combined
            }
        (_, first : rest) -> LoadedAgentsMd
            { loadedGlobal = Just first
            , loadedProject = rest
            }
        (_, []) -> LoadedAgentsMd
            { loadedGlobal = Nothing
            , loadedProject = []
            }

-- | Grok Build reads its own home first, followed by compatible Claude and
-- Cursor homes. For custom harness homes, only that explicit directory is
-- inspected.
readGrokHomeInstructions :: OsPath -> IO [InstructionFile]
readGrokHomeInstructions globalDir = do
    primary <- readGrokHomeRoot globalDir
    compatible <-
        if takeFileName globalDir == unsafeEncodeUtf ".grok"
            then concat <$> traverse readGrokHomeRoot
                [ takeDirectory globalDir </> unsafeEncodeUtf ".claude"
                , takeDirectory globalDir </> unsafeEncodeUtf ".cursor"
                ]
            else pure []
    pure (primary <> compatible)

readGrokHomeRoot :: OsPath -> IO [InstructionFile]
readGrokHomeRoot dir = do
    named <- readNamedInstructionFiles dir
    rules <- readRulesDirectory (dir </> unsafeEncodeUtf "rules")
    pure (named <> rules)

readGrokDirectoryInstructions :: OsPath -> IO [InstructionFile]
readGrokDirectoryInstructions dir = do
    named <- readNamedInstructionFiles dir
    rules <- concat <$> traverse
        (readRulesDirectory . (dir </>))
        grokProjectRulesDirectories
    pure (named <> rules)

grokProjectRulesDirectories :: [OsPath]
grokProjectRulesDirectories =
    [ unsafeEncodeUtf ".grok/rules"
    , unsafeEncodeUtf ".claude/rules"
    , unsafeEncodeUtf ".cursor/rules"
    ]

readNamedInstructionFiles :: OsPath -> IO [InstructionFile]
readNamedInstructionFiles dir = do
    preferredAgents <- readPreferredAgentsMd dir
    let names = case preferredAgents of
            Just file
                | takeFileName file.instructionPath
                    == unsafeEncodeUtf "AGENTS.override.md" ->
                    filter (not . isAgentsMdSpelling) grokInstructionNames
            _ -> grokInstructionNames
    other <- mapMaybe id <$> traverse
        (readAgentsFile . (dir </>))
        names
    dedupeInstructionFiles (maybe [] pure preferredAgents <> other)

isAgentsMdSpelling :: OsPath -> Bool
isAgentsMdSpelling name =
    Text.toLower (toText (takeFileName name)) == "agents.md"

-- | Current Grok Build compatibility filenames other than @AGENTS.md@, whose
-- place is occupied by @AGENTS.override.md@ when present.
grokInstructionNames :: [OsPath]
grokInstructionNames =
    [ unsafeEncodeUtf "Agents.md"
    , unsafeEncodeUtf "Claude.md"
    , unsafeEncodeUtf "CLAUDE.md"
    , unsafeEncodeUtf "CLAUDE.local.md"
    , unsafeEncodeUtf "AGENT.md"
    , unsafeEncodeUtf ".claude/CLAUDE.md"
    , unsafeEncodeUtf ".claude/CLAUDE.local.md"
    ]

readRulesDirectory :: OsPath -> IO [InstructionFile]
readRulesDirectory dir = do
    exists <- doesDirectoryExist dir
    if not exists
        then pure []
        else do
            entries <- sort <$> listDirectory dir
            markdown <- filterM
                (doesFileExist . (dir </>))
                [ name
                | name <- entries
                , Text.toLower (toText (takeExtension name)) == ".md"
                ]
            mapMaybe id <$> traverse (readAgentsFile . (dir </>)) markdown

-- | Case-insensitive filesystems can resolve several compatibility spellings
-- to the same file. Symlinked rule files can do the same. Keep the first
-- occurrence so order and precedence stay deterministic.
dedupeInstructionFiles :: [InstructionFile] -> IO [InstructionFile]
dedupeInstructionFiles files =
    reverse . snd <$> foldM step (Set.empty, []) files
  where
    step
        :: (Set.Set OsPath, [InstructionFile])
        -> InstructionFile
        -> IO (Set.Set OsPath, [InstructionFile])
    step (seen, kept) file = do
        canonical <-
            tryAny (canonicalizePath file.instructionPath) >>= \case
                Left _ -> pure file.instructionPath
                Right path -> pure path
        if Set.member canonical seen
            then pure (seen, kept)
            else pure (Set.insert canonical seen, file : kept)

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
