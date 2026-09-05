-- | Discover repository instructions for fresh or regenerated context.
module Agent.CLI.StartupContext
    ( AgentsContextNotice(..)
    , preloadAgentsContext
    , loadAgentsContext
    , loadAgentsContextWithPreload
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
    , LoadedAgentsMd
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
        initialItems initialPrevious extraContext =
    loadAgentsContextWithPreload
        stderrHandle
        fullscreen
        notice
        options
        dialect
        home
        cwd
        initialItems
        initialPrevious
        extraContext
        Nothing

-- | Read project instructions without reporting or formatting them. Startup
-- can overlap this filesystem work with independent tool acquisition while
-- leaving all user-visible effects at the original context-install boundary.
preloadAgentsContext
    :: CliOptions
    -> Dialect
    -> OsPath
    -> OsPath
    -> IO (Maybe LoadedAgentsMd)
preloadAgentsContext options dialect home cwd
    | not options.optAgentsMd = pure Nothing
    | otherwise =
        Just <$> discoverProjectInstructions
            (agentsDiscoverOptions dialect home)
            cwd

-- | Finalize generated context from an optional startup preload. A preload
-- that becomes unnecessary because persisted history is reusable is discarded
-- before warnings or success notices are emitted.
loadAgentsContextWithPreload
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
    -> Maybe LoadedAgentsMd
    -> IO (IORef (Maybe Text))
loadAgentsContextWithPreload
        stderrHandle fullscreen notice options dialect home cwd
        initialItems initialPrevious extraContext preloaded
    | not (null initialItems) || isJust initialPrevious = newIORef Nothing
    | not options.optAgentsMd = newIORef extraContext
    | otherwise = do
        loaded <- case preloaded of
            Just value -> pure value
            Nothing ->
                discoverProjectInstructions
                    (agentsDiscoverOptions dialect home)
                    cwd
        mapM_ (reportInstructionWarning stderrHandle fullscreen)
            (loadedInstructionWarnings loaded)
        let files = loadedInstructionFiles loaded
        case formatAgentsMdForDialect dialect cwd loaded of
            Nothing -> newIORef extraContext
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
                    (Just (text <> maybe "" ("\n\n" <>) extraContext))

agentsDiscoverOptions :: Dialect -> OsPath -> DiscoverOptions
agentsDiscoverOptions dialect home =
    DiscoverOptions
        { discoverMaxBytes = defaultDiscoverOptions.discoverMaxBytes
        , discoverGlobalDir = Just (globalAgentsHomeDir dialect home)
        , discoverRootMarkers = defaultDiscoverOptions.discoverRootMarkers
        }

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
