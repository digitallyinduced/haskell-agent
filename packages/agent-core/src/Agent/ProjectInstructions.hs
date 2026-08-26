-- | Discover project instructions.
--
-- Codex keeps its narrow one-file-per-directory discovery contract. Grok-style
-- homes use the broader Grok Build compatibility surface: common Claude/agent
-- filenames plus vendor rules directories. Dialect packages own model-facing
-- formatting of the discovered documents.
module Agent.ProjectInstructions
    ( InstructionFile(..)
    , InstructionWarning(..)
    , LoadedAgentsMd(..)
    , DiscoverOptions(..)
    , defaultDiscoverOptions
    , defaultProjectDocMaxBytes
    , discoverProjectInstructions
    , loadedInstructionFiles
    , loadedInstructionWarnings
    , nonEmptyInstructionContent
    ) where

import Agent.Concurrent (mapConcurrentlyBounded)
import Agent.FileRetry (retryOnFileBusy)
import Agent.OsPath (directoryChain, toText, unsafeToFilePath)
import Control.Concurrent.Async (concurrently)
import Control.Exception.Safe (SomeException, displayException, tryAny)
import qualified Data.ByteString as BS
import Data.List (sort)
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

-- | A discovered instruction path that existed but could not be used.
data InstructionWarning = InstructionWarning
    { instructionWarningPath :: !OsPath
    , instructionWarningMessage :: !Text
    } deriving (Eq, Show)

-- | Global home instructions plus project files from root -> cwd.
data LoadedAgentsMd = LoadedAgentsMd
    { loadedGlobal :: !(Maybe InstructionFile)
    , loadedProject :: ![InstructionFile]
    , loadedWarnings :: ![InstructionWarning]
    } deriving (Eq, Show)

-- | One discovery pass: loaded files plus warnings for paths that failed.
data InstructionLoad = InstructionLoad
    { loadFiles :: ![InstructionFile]
    , loadWarnings :: ![InstructionWarning]
    }

instance Semigroup InstructionLoad where
    InstructionLoad filesA warningsA <> InstructionLoad filesB warningsB =
        InstructionLoad (filesA <> filesB) (warningsA <> warningsB)

instance Monoid InstructionLoad where
    mempty = InstructionLoad [] []

loadedInstructionFiles :: LoadedAgentsMd -> [InstructionFile]
loadedInstructionFiles loaded =
    maybe id (:) loaded.loadedGlobal loaded.loadedProject

loadedInstructionWarnings :: LoadedAgentsMd -> [InstructionWarning]
loadedInstructionWarnings loaded = loaded.loadedWarnings

nonEmptyInstructionContent :: InstructionFile -> Maybe Text
nonEmptyInstructionContent file
    | Text.null (Text.strip file.instructionContent) = Nothing
    | otherwise = Just file.instructionContent

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

-- | Load global + project instruction files for @cwd@. Empty files are
-- skipped. Files that exist but cannot be read are reported in
-- 'loadedWarnings' instead of disappearing. Project files are ordered
-- root -> cwd.
--
-- A @.codex@ global directory selects Codex's narrow discovery contract.
-- Other homes (including @.grok@, @.claude@, and @.haskell-agent@), and calls
-- without a global directory, use Grok-compatible discovery.
discoverProjectInstructions :: DiscoverOptions -> OsPath -> IO LoadedAgentsMd
discoverProjectInstructions options cwd
    | options.discoverMaxBytes <= 0 =
        pure emptyLoadedAgentsMd
    | otherwise = do
        root <- findProjectRoot options.discoverRootMarkers cwd
        let dirs = directoryChain root cwd
        loaded <-
            if usesCodexDiscovery options
                then do
                    (global, projectFiles) <- concurrently
                        (maybe (pure mempty) readPreferredAgentsMd
                            options.discoverGlobalDir)
                        (mapConcurrentlyBounded instructionDirectoryConcurrency
                            readPreferredAgentsMd
                            dirs)
                    let project = mconcat projectFiles
                    pure $ loadedAgentsFromPreferred global project
                else discoverGrokInstructions options dirs
        pure (applyByteBudget options.discoverMaxBytes loaded)

emptyLoadedAgentsMd :: LoadedAgentsMd
emptyLoadedAgentsMd =
    LoadedAgentsMd
        { loadedGlobal = Nothing
        , loadedProject = []
        , loadedWarnings = []
        }

loadedAgentsFromPreferred :: InstructionLoad -> InstructionLoad -> LoadedAgentsMd
loadedAgentsFromPreferred global project =
    LoadedAgentsMd
        { loadedGlobal = case global.loadFiles of
            file : _ -> Just file
            [] -> Nothing
        , loadedProject = project.loadFiles
        , loadedWarnings = global.loadWarnings <> project.loadWarnings
        }

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
    (home, projectParts) <- concurrently
        (maybe (pure mempty) readGrokHomeInstructions options.discoverGlobalDir)
        (mapConcurrentlyBounded instructionDirectoryConcurrency
            readGrokDirectoryInstructions
            dirs)
    let project = mconcat projectParts
        combinedLoad = home <> project
    combined <- dedupeInstructionFiles combinedLoad.loadFiles
    pure $ case (home.loadFiles, combined) of
        ([], _) ->
            LoadedAgentsMd
                { loadedGlobal = Nothing
                , loadedProject = combined
                , loadedWarnings = combinedLoad.loadWarnings
                }
        (_, first : rest) ->
            LoadedAgentsMd
                { loadedGlobal = Just first
                , loadedProject = rest
                , loadedWarnings = combinedLoad.loadWarnings
                }
        (_, []) ->
            LoadedAgentsMd
                { loadedGlobal = Nothing
                , loadedProject = []
                , loadedWarnings = combinedLoad.loadWarnings
                }

-- | Grok Build reads its own home first, followed by compatible Claude and
-- Cursor homes. For custom harness homes, only that explicit directory is
-- inspected.
readGrokHomeInstructions :: OsPath -> IO InstructionLoad
readGrokHomeInstructions globalDir =
    mconcat
        <$> mapConcurrentlyBounded instructionDirectoryConcurrency
            readGrokHomeRoot
            roots
  where
    roots
        | takeFileName globalDir == unsafeEncodeUtf ".grok" =
            [ globalDir
            , takeDirectory globalDir </> unsafeEncodeUtf ".claude"
            , takeDirectory globalDir </> unsafeEncodeUtf ".cursor"
            ]
        | otherwise = [globalDir]

readGrokHomeRoot :: OsPath -> IO InstructionLoad
readGrokHomeRoot dir = do
    (named, rules) <- concurrently
        (readNamedInstructionFiles dir)
        (readRulesDirectory (dir </> unsafeEncodeUtf "rules"))
    pure (named <> rules)

readGrokDirectoryInstructions :: OsPath -> IO InstructionLoad
readGrokDirectoryInstructions dir = do
    (named, ruleGroups) <- concurrently
        (readNamedInstructionFiles dir)
        (mapConcurrentlyBounded instructionDirectoryConcurrency
            (readRulesDirectory . (dir </>))
            grokProjectRulesDirectories)
    pure (named <> mconcat ruleGroups)

grokProjectRulesDirectories :: [OsPath]
grokProjectRulesDirectories =
    [ unsafeEncodeUtf ".grok/rules"
    , unsafeEncodeUtf ".claude/rules"
    , unsafeEncodeUtf ".cursor/rules"
    ]

readNamedInstructionFiles :: OsPath -> IO InstructionLoad
readNamedInstructionFiles dir = do
    (preferredAgents, loadedOthers) <- concurrently
        (readPreferredAgentsMd dir)
        (mapConcurrentlyBounded instructionFileConcurrency
            (\name -> do
                loaded <- readAgentsFile (dir </> name)
                pure (name, loaded))
            grokInstructionNames)
    let usedOverride =
            any
                (\file ->
                    takeFileName file.instructionPath
                        == unsafeEncodeUtf "AGENTS.override.md")
                preferredAgents.loadFiles
        names =
            if usedOverride
                then filter (not . isAgentsMdSpelling) grokInstructionNames
                else grokInstructionNames
        allowedNames = Set.fromList names
        other =
            mconcat
                [ loaded
                | (name, loaded) <- loadedOthers
                , name `Set.member` allowedNames
                ]
        combined = preferredAgents <> other
    files <- dedupeInstructionFiles combined.loadFiles
    pure InstructionLoad
        { loadFiles = files
        , loadWarnings = combined.loadWarnings
        }

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

readRulesDirectory :: OsPath -> IO InstructionLoad
readRulesDirectory dir = do
    exists <- doesDirectoryExist dir
    if not exists
        then pure mempty
        else tryAny (listDirectory dir) >>= \case
            Left err ->
                pure (instructionIoWarning dir err)
            Right entries -> do
                let candidates =
                        [ name
                        | name <- sort entries
                        , Text.toLower (toText (takeExtension name)) == ".md"
                        ]
                classified <-
                    mapConcurrentlyBounded instructionFileConcurrency
                        (\name -> do
                            fileExists <- doesFileExist (dir </> name)
                            pure (name, fileExists))
                        candidates
                mconcat
                    <$> mapConcurrentlyBounded instructionFileConcurrency
                        (readAgentsFile . (dir </>))
                        [name | (name, True) <- classified]

-- | Case-insensitive filesystems can resolve several compatibility spellings
-- to the same file. Symlinked rule files can do the same. Keep the first
-- occurrence so order and precedence stay deterministic.
dedupeInstructionFiles :: [InstructionFile] -> IO [InstructionFile]
dedupeInstructionFiles files = do
    canonicals <-
        mapConcurrentlyBounded instructionFileConcurrency canonical files
    pure (reverse (snd (foldl step (Set.empty, []) (zip canonicals files))))
  where
    canonical :: InstructionFile -> IO OsPath
    canonical file =
        tryAny (canonicalizePath file.instructionPath) >>= \case
            Left _ -> pure file.instructionPath
            Right path -> pure path

    step
        :: (Set.Set OsPath, [InstructionFile])
        -> (OsPath, InstructionFile)
        -> (Set.Set OsPath, [InstructionFile])
    step (seen, kept) (canonical, file)
        | Set.member canonical seen = (seen, kept)
        | otherwise = (Set.insert canonical seen, file : kept)

findProjectRoot :: [OsPath] -> OsPath -> IO OsPath
findProjectRoot markers start = go start
  where
    go dir = do
        hit <- fmap or $
            mapConcurrentlyBounded instructionFileConcurrency
                (\marker -> doesPathExist (dir </> marker))
                markers
        if hit
            then pure dir
            else do
                let parent = takeDirectory dir
                if parent == dir
                    then pure start
                    else go parent

-- | Prefer @AGENTS.override.md@ over @AGENTS.md@ in a directory.
readPreferredAgentsMd :: OsPath -> IO InstructionLoad
readPreferredAgentsMd dir = do
    (override, base) <- concurrently
        (readAgentsFile (dir </> unsafeEncodeUtf "AGENTS.override.md"))
        (readAgentsFile (dir </> unsafeEncodeUtf "AGENTS.md"))
    pure $ case override.loadFiles of
        _ : _ ->
            InstructionLoad
                { loadFiles = override.loadFiles
                , loadWarnings = override.loadWarnings <> base.loadWarnings
                }
        [] ->
            base <> InstructionLoad [] override.loadWarnings

readAgentsFile :: OsPath -> IO InstructionLoad
readAgentsFile path = do
    exists <- doesFileExist path
    if not exists
        then pure mempty
        else tryAny (retryOnFileBusy (Text.readFile (unsafeToFilePath path))) >>= \case
            Left err ->
                pure (instructionIoWarning path err)
            Right text ->
                if Text.null (Text.strip text)
                    then pure mempty
                    else pure InstructionLoad
                        { loadFiles =
                            [ InstructionFile
                                { instructionPath = path
                                , instructionContent = text
                                }
                            ]
                        , loadWarnings = []
                        }

instructionIoWarning :: OsPath -> SomeException -> InstructionLoad
instructionIoWarning path err =
    InstructionLoad
        { loadFiles = []
        , loadWarnings =
            [ InstructionWarning
                { instructionWarningPath = path
                , instructionWarningMessage = Text.pack (displayException err)
                }
            ]
        }

applyByteBudget :: Int -> LoadedAgentsMd -> LoadedAgentsMd
applyByteBudget maxBytes loaded =
    case go maxBytes (loadedInstructionFiles loaded) of
        [] ->
            loaded
                { loadedGlobal = Nothing
                , loadedProject = []
                }
        files@(first : rest) -> case loaded.loadedGlobal of
            Just global
                | first.instructionPath == global.instructionPath ->
                    loaded
                        { loadedGlobal = Just first
                        , loadedProject = rest
                        }
            _ ->
                loaded
                    { loadedGlobal = Nothing
                    , loadedProject = files
                    }
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

instructionDirectoryConcurrency :: Int
instructionDirectoryConcurrency = 8

instructionFileConcurrency :: Int
instructionFileConcurrency = 16
