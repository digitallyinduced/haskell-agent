-- | Discover repository instructions for a fresh session.
module Agent.CLI.StartupContext
    ( loadAgentsContext
    ) where

import Agent.CLI.Dialects
    ( formatAgentsMdForDialect
    , globalAgentsHomeDir
    )
import Agent.CLI.Options (CliOptions(..))
import Agent.CLI.Render (putTextLn)
import Agent.CLI.Style
    ( glyphSession
    , roleMuted
    )
import Agent.CLI.Terminal (resolveColor)
import Agent.CLI.TUI.App
    ( FullscreenRuntime
    , emitUiEvent
    )
import Agent.Dialect (Dialect)
import Agent.ProjectInstructions
    ( DiscoverOptions(..)
    , defaultDiscoverOptions
    , discoverProjectInstructions
    , loadedInstructionFiles
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
import System.IO (stderr)
import System.OsPath (OsPath)

-- | Discover AGENTS.md once for a fresh session. Resumed transcripts keep
-- whatever instructions were already in history.
loadAgentsContext
    :: Maybe FullscreenRuntime
    -> CliOptions
    -> Dialect
    -> OsPath
    -> OsPath
    -> [ResponseItem]
    -> Maybe Text
    -> IO (IORef (Maybe Text))
loadAgentsContext fullscreen options dialect home cwd initialItems initialPrevious
    | not options.optAgentsMd = newIORef Nothing
    | not (null initialItems) || isJust initialPrevious = newIORef Nothing
    | otherwise = do
        let discoverOptions = DiscoverOptions
                { discoverMaxBytes = defaultDiscoverOptions.discoverMaxBytes
                , discoverGlobalDir = Just (globalAgentsHomeDir dialect home)
                , discoverRootMarkers = defaultDiscoverOptions.discoverRootMarkers
                }
        loaded <- discoverProjectInstructions discoverOptions cwd
        let files = loadedInstructionFiles loaded
        case formatAgentsMdForDialect dialect cwd loaded of
            Nothing -> newIORef Nothing
            Just text -> do
                let message =
                        "agents.md: loaded "
                            <> Text.pack (show (length files))
                            <> if length files == 1 then " file" else " files"
                case fullscreen of
                    Nothing -> do
                        color <- resolveColor stderr
                        putTextLn stderr
                            (roleMuted color (glyphSession <> message))
                    Just runtime ->
                        emitUiEvent runtime (UiSystemMessage message)
                newIORef (Just text)
