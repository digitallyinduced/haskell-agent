-- | Discover and format AGENTS.md project instructions.
--
-- Discovery follows Codex's narrow rules (one file per directory from the
-- project root down to cwd, with an optional global home file). Formatting is
-- provider-specific so OpenAI gets the Codex user-fragment shape and
-- xAI/OpenRouter get the Grok system-reminder shape.
module Agent.ProjectInstructions
    ( InstructionFile(..)
    , LoadedAgentsMd(..)
    , DiscoverOptions(..)
    , defaultDiscoverOptions
    , defaultProjectDocMaxBytes
    , discoverProjectInstructions
    , loadedInstructionFiles
    , formatCodexAgentsMd
    , formatGrokAgentsMd
    , formatAgentsMdForProvider
    , globalAgentsHomeDir
    ) where

import Agent.Provider (Provider(..))
import Control.Exception.Safe (tryAny)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (doesFileExist, doesPathExist)
import System.FilePath (takeDirectory, (</>))

-- | One loaded instruction file and its absolute path.
data InstructionFile = InstructionFile
    { instructionPath :: !FilePath
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
    , discoverGlobalDir :: !(Maybe FilePath)
      -- ^ Optional home-scope directory (e.g. @~/.codex@ or @~/.grok@).
    , discoverRootMarkers :: ![FilePath]
      -- ^ Path segments that mark the project root. Default: @[".git"]@.
    } deriving (Eq, Show)

defaultProjectDocMaxBytes :: Int
defaultProjectDocMaxBytes = 32 * 1024

defaultDiscoverOptions :: DiscoverOptions
defaultDiscoverOptions = DiscoverOptions
    { discoverMaxBytes = defaultProjectDocMaxBytes
    , discoverGlobalDir = Nothing
    , discoverRootMarkers = [".git"]
    }

-- | Provider-specific directory under the user home for global AGENTS.md.
globalAgentsHomeDir :: Provider -> FilePath -> FilePath
globalAgentsHomeDir provider home = case provider of
    OpenAIProvider -> home </> ".codex"
    XAIProvider -> home </> ".grok"
    OpenRouterProvider -> home </> ".grok"

-- | Load global + project AGENTS.md files for @cwd@. Empty / unreadable files
-- are skipped. Project files are ordered root -> cwd.
discoverProjectInstructions :: DiscoverOptions -> FilePath -> IO LoadedAgentsMd
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

findProjectRoot :: [FilePath] -> FilePath -> IO FilePath
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

directoryChain :: FilePath -> FilePath -> [FilePath]
directoryChain root cwd = reverse (go cwd)
  where
    go dir
        | dir == root = [dir]
        | takeDirectory dir == dir = [dir]
        | otherwise = dir : go (takeDirectory dir)

-- | Prefer @AGENTS.override.md@ over @AGENTS.md@ in a directory.
readPreferredAgentsMd :: FilePath -> IO (Maybe InstructionFile)
readPreferredAgentsMd dir = do
    override <- readAgentsFile (dir </> "AGENTS.override.md")
    case override of
        Just file -> pure (Just file)
        Nothing -> readAgentsFile (dir </> "AGENTS.md")

readAgentsFile :: FilePath -> IO (Maybe InstructionFile)
readAgentsFile path = do
    exists <- doesFileExist path
    if not exists
        then pure Nothing
        else tryAny (Text.readFile path) >>= \case
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
                size = Text.length content
            in if size <= remaining
                then file : go (remaining - size) rest
                else [file { instructionContent = Text.take remaining content }]

formatAgentsMdForProvider :: Provider -> FilePath -> LoadedAgentsMd -> Maybe Text
formatAgentsMdForProvider provider cwd loaded = case provider of
    OpenAIProvider -> formatCodexAgentsMd cwd loaded
    XAIProvider -> formatGrokAgentsMd loaded
    OpenRouterProvider -> formatGrokAgentsMd loaded

-- | Codex-style contextual user fragment.
formatCodexAgentsMd :: FilePath -> LoadedAgentsMd -> Maybe Text
formatCodexAgentsMd cwd loaded =
    case bodies of
        Nothing -> Nothing
        Just body ->
            Just $ Text.concat
                [ "# AGENTS.md instructions for "
                , Text.pack cwd
                , "\n\n<INSTRUCTIONS>\n"
                , body
                , "\n</INSTRUCTIONS>"
                ]
  where
    bodies = case (loaded.loadedGlobal >>= stripFile, mapMaybe stripFile loaded.loadedProject) of
        (Nothing, []) -> Nothing
        (Nothing, project) -> Just (Text.intercalate "\n\n" project)
        (Just global, []) -> Just global
        (Just global, project) ->
            Just $ global <> "\n\n--- project-doc ---\n\n" <> Text.intercalate "\n\n" project

stripFile :: InstructionFile -> Maybe Text
stripFile file =
    let text = file.instructionContent
    in if Text.null (Text.strip text) then Nothing else Just text

-- | Grok-style system-reminder block with per-file provenance.
formatGrokAgentsMd :: LoadedAgentsMd -> Maybe Text
formatGrokAgentsMd loaded =
    case filter nonempty (loadedInstructionFiles loaded) of
        [] -> Nothing
        kept ->
            Just $ Text.concat $
                [ "\n\n<system-reminder>\n"
                , "As you answer the user's questions, you can use the following context"
                , " (ordered from repo root to current directory - deeper files take precedence on conflicts):\n"
                ]
                <> concatMap renderFile kept
                <>
                [ "\nFollow these instructions exactly. When working in subdirectories not listed above, "
                , "check for additional project instruction files (AGENTS.md, Claude.md, etc.)."
                , "\n</system-reminder>"
                ]
  where
    nonempty :: InstructionFile -> Bool
    nonempty file = not (Text.null (Text.strip file.instructionContent))
    renderFile :: InstructionFile -> [Text]
    renderFile file =
        [ "\n## From: "
        , neutralizeReminderTags (Text.pack file.instructionPath)
        , "\n"
        , neutralizeReminderTags file.instructionContent
        , "\n"
        ]

neutralizeReminderTags :: Text -> Text
neutralizeReminderTags =
    Text.replace "<system-reminder" "&lt;system-reminder"
        . Text.replace "</system-reminder" "&lt;/system-reminder"
        . Text.replace "<system_reminder" "&lt;system_reminder"
        . Text.replace "</system_reminder" "&lt;/system_reminder"
