-- | Discover repository instructions for fresh or regenerated context.
module Agent.CLI.StartupContext
    ( AgentsContextNotice(..)
    , appendGeneratedContext
    , loadAgentsContext
    ) where

import Agent.CLI.Dialects
    ( formatAgentsMdForDialect
    , globalAgentsHomeDir
    )
import Agent.CLI.Options (CliOptions(..))
import Agent.CLI.Render (putTextLn)
import Agent.CLI.Style
    ( glyphSession
    , glyphWarn
    , roleMuted
    , roleWarn
    )
import Agent.CLI.Terminal (resolveColor)
import Agent.CLI.TUI.App
    ( FullscreenRuntime
    , emitUiEvent
    )
import Agent.Dialect (Dialect)
import Agent.OsPath (toText)
import Agent.ProjectInstructions
    ( DiscoverOptions(..)
    , InstructionWarning(..)
    , defaultDiscoverOptions
    , discoverProjectInstructions
    , loadedInstructionFiles
    , loadedInstructionWarnings
    )
import Agent.Responses.Types (ResponseItem)
import Agent.TUI.Model (UiEvent(..))
import Data.IORef
    ( IORef
    , newIORef
    )
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import System.IO (Handle)
import System.OsPath (OsPath)

data AgentsContextNotice
    = ReportAgentsContextLoaded
    | SuppressAgentsContextLoaded
    deriving (Eq, Show)

-- | Discover AGENTS.md when generated context is needed. Resumed transcripts
-- keep whatever instructions were already in history; callers pass an empty
-- history after a persisted transcript-replacement boundary. Regeneration can
-- suppress the successful-load notice while still reporting read warnings.
--
-- @extraContext@ carries additional generated context blocks — such as the
-- @<environment_context>@ fragment for catalog models — appended after the
-- AGENTS.md wrapper (or sent alone when no AGENTS.md applies).
loadAgentsContext
    :: Handle
    -> Maybe FullscreenRuntime
    -> AgentsContextNotice
    -> CliOptions
    -> Dialect
    -> OsPath
    -> OsPath
    -> [ResponseItem]
    -> Maybe Text
    -> Maybe Text
    -> IO (IORef (Maybe Text))
loadAgentsContext
        stderrHandle fullscreen notice options dialect home cwd
        initialItems initialPrevious extraContext
    | not (null initialItems) || isJust initialPrevious = newIORef Nothing
    | not options.optAgentsMd = newIORef combinedContext
    | otherwise = do
        let discoverOptions = DiscoverOptions
                { discoverMaxBytes = defaultDiscoverOptions.discoverMaxBytes
                , discoverGlobalDir = Just (globalAgentsHomeDir dialect home)
                , discoverRootMarkers = defaultDiscoverOptions.discoverRootMarkers
                }
        loaded <- discoverProjectInstructions discoverOptions cwd
        mapM_ (reportInstructionWarning stderrHandle fullscreen)
            (loadedInstructionWarnings loaded)
        let files = loadedInstructionFiles loaded
        case formatAgentsMdForDialect dialect cwd loaded of
            Nothing -> newIORef combinedContext
            Just text -> do
                case notice of
                    SuppressAgentsContextLoaded -> pure ()
                    ReportAgentsContextLoaded -> do
                        let message =
                                "agents.md: loaded "
                                    <> Text.pack (show (length files))
                                    <> if length files == 1 then " file" else " files"
                        case fullscreen of
                            Nothing -> do
                                color <- resolveColor stderrHandle
                                putTextLn stderrHandle
                                    (roleMuted color (glyphSession <> message))
                            Just runtime ->
                                emitUiEvent runtime (UiSystemMessage message)
                newIORef
                    (Just (text <> maybe "" ("\n\n" <>) combinedContext))
  where
    combinedContext =
        appendGeneratedContext extraContext options.optBundleContext

appendGeneratedContext :: Maybe Text -> Maybe Text -> Maybe Text
appendGeneratedContext first second =
    case (first, second) of
        (Nothing, Nothing) -> Nothing
        (Just text, Nothing) -> Just text
        (Nothing, Just text) -> Just text
        (Just before, Just after) -> Just (before <> "\n\n" <> after)

reportInstructionWarning
    :: Handle
    -> Maybe FullscreenRuntime
    -> InstructionWarning
    -> IO ()
reportInstructionWarning stderrHandle fullscreen warning = do
    let message =
            "agents.md ignored: "
                <> toText warning.instructionWarningPath
                <> ": "
                <> warning.instructionWarningMessage
    case fullscreen of
        Nothing -> do
            color <- resolveColor stderrHandle
            putTextLn stderrHandle (roleWarn color (glyphWarn <> message))
        Just runtime ->
            emitUiEvent runtime (UiErrorMessage message)
