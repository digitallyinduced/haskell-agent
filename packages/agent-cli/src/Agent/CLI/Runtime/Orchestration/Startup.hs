module Agent.CLI.Runtime.Orchestration.Startup
    ( clearNativeProgress
    , finishStartup
    , mcpToolCollision
    , reportStartupWarning
    , setNativeProgress
    , setStartupRepository
    ) where

import Agent.CLI.Progress
    ( osc9ProgressIndeterminate, osc9ProgressRemove, wrapOscForTmux )
import Agent.CLI.Startup.Auth ( recordStartupTiming )
import Agent.CLI.Startup.Format
    ( formatRepositoryPath, formatStartupDuration, formatStartupTimings )
import Agent.CLI.TUI.App ( FullscreenRuntime, emitUiEvent )
import Agent.CLI.Render ( putTextLn )
import Agent.CLI.Session.Runtime.Types ( StartupRuntime(..) )
import Agent.Tools.Types ( AppTool(..) )
import Agent.ToolDispatch ( canonicalToolName )
import Agent.TUI.Model ( UiEvent(..) )
import Control.Monad ( when )
import Data.IORef ( readIORef, writeIORef )
import Data.Maybe ( isJust )
import Data.Text ( Text )
import System.Environment ( lookupEnv )
import System.IO ( Handle, hFlush, hIsTerminalDevice )
import System.OsPath ( OsPath )

import qualified Agent.MCP as MCP
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as Text

finishStartup :: StartupRuntime -> IO ()
finishStartup startup = do
    writeIORef startup.startupFinished True
    recordStartupTiming startup.startupStartedAt startup.startupTimings "ready"
    case startup.startupFullscreen of
        Nothing -> pure ()
        Just runtime ->
            emitUiEvent runtime (UiSetNotice Nothing)
    lookupEnv "HASKELL_AGENT_STARTUP_TIMING" >>= \case
        Just "1" -> do
            timings <- readIORef startup.startupTimings
            syntaxLoadDuration <-
                readIORef startup.startupSyntaxLoadDuration
            let message =
                    formatStartupTimings timings
                        <> maybe
                            ""
                            (\duration ->
                                " · syntax highlighting "
                                    <> formatStartupDuration duration)
                            syntaxLoadDuration
            case startup.startupFullscreen of
                Nothing -> putTextLn startup.startupStderr message
                Just runtime -> emitUiEvent runtime (UiSystemMessage message)
        _ -> pure ()

reportStartupWarning :: StartupRuntime -> Text -> IO ()
reportStartupWarning startup message =
    case startup.startupFullscreen of
        Nothing -> putTextLn startup.startupStderr ("warning: " <> message)
        Just runtime ->
            emitUiEvent runtime (UiSystemMessage ("warning: " <> message))

mcpToolCollision :: [AppTool] -> [MCP.McpToolRegistration] -> Maybe Text
mcpToolCollision existingTools = go
  where
    existing =
        Map.fromList $
            ("web_search", "built-in web search")
                : [ (canonicalToolName tool.appToolName, "built-in tool")
                  | tool <- existingTools
                  ]

    go [] = Nothing
    go (registration : rest) =
        let tool = registration.mcpRegistrationTool
            name = canonicalToolName tool.appToolName
        in case Map.lookup name existing of
            Nothing -> go rest
            Just source ->
                Just $
                    "MCP tool "
                        <> tool.appToolName
                        <> " from server "
                        <> registration.mcpRegistrationServer
                        <> " conflicts with "
                        <> source

setStartupRepository
    :: Maybe FullscreenRuntime
    -> OsPath
    -> Text
    -> OsPath
    -> IO ()
setStartupRepository fullscreen home branch cwd =
    case fullscreen of
        Nothing -> pure ()
        Just runtime ->
            emitUiEvent runtime $
                UiSetRepository
                    branch
                    (formatRepositoryPath home cwd)

-- | Drop Ghostty / Windows Terminal native progress (OSC 9;4) on stderr.
-- Safe when the bar was never shown; unknown terminals ignore the sequence.
clearNativeProgress :: Handle -> IO ()
clearNativeProgress handle =
    setNativeProgress handle False

setNativeProgress :: Handle -> Bool -> IO ()
setNativeProgress handle active = do
    tty <- hIsTerminalDevice handle
    when tty do
        inTmux <- isJust <$> lookupEnv "TMUX"
        let sequence_
                | active = osc9ProgressIndeterminate
                | otherwise = osc9ProgressRemove
        Text.hPutStr handle (wrapOscForTmux inTmux sequence_)
        hFlush handle
