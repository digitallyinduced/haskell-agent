-- | Terminal commands operate on canonical history obtained from the service.
module Agent.CLI.Remote.Commands (presentationCommand) where

import Agent.CLI.Artifact (fencedCodeBlock, lastDiffBlock)
import Agent.CLI.Changelog (loadReleaseNotes)
import Agent.CLI.Command
import Agent.CLI.ExternalProgram
import Agent.CLI.Remote.History
import Agent.CLI.Remote.Transport
import Agent.CLI.Remote.Ui
import Agent.CLI.TUI.App (requestFullscreenDocument, requestFullscreenThemeChoice, enqueueAppEvent, withFullscreenSuspended)
import Agent.CLI.TUI.Types (AppEvent(..), FullscreenRuntime(..))
import Agent.CLI.Terminal (copyTerminalClipboard, detectTerminalCapabilities)
import Agent.CLI.TranscriptExport
import Agent.OsPath (toText)
import Agent.Runtime.Config
import Agent.Runtime.Session (SessionTurn(..))
import Agent.Server.Client (AgentServerClient)
import Agent.TUI.Theme
import Control.Exception.Safe (throwIO)
import Control.Monad (forM_, void)
import Data.IORef
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import System.Directory.OsPath (getHomeDirectory)
import System.IO (stdout)

presentationCommand :: RemoteUi -> AgentServerClient -> (Text -> IO ()) -> ReplAction -> IO Bool
presentationCommand ui client submit action = do
    session <- readIORef ui.remoteCurrentSession
    let turns = map snd <$> loadRemoteTranscript client session.remoteId
        document title body = case ui.remoteFullscreen of
            Just runtime -> void (requestFullscreenDocument runtime title body)
            Nothing -> remoteNotice ui body
        copy text = do
            terminal <- detectTerminalCapabilities stdout
            _ <- copyTerminalClipboard terminal stdout text
            remoteNotice ui "Copied to clipboard."
        responses = reverse . mapMaybe (.turnAssistantText) . visibleSessionTurns <$> turns
        selected index = responses >>= \values -> case drop (index - 1) values of
            value : _ | index > 0 -> pure value
            _ -> throwIO (RemoteFailure "No matching assistant response.")
        copyBlock select = selected 1 >>= maybe
            (throwIO (RemoteFailure "No matching code block.")) copy . select
        theme name = do
            chosen <- case name of
                Just value -> maybe (throwIO (RemoteFailure "Unknown theme.")) (pure . Just) (parseThemeKind value)
                Nothing -> case ui.remoteFullscreen of
                    Nothing -> remoteNotice ui "Theme selection requires fullscreen mode." >> pure Nothing
                    Just runtime -> fmap themeKindAt <$> requestFullscreenThemeChoice runtime 0 themeKindRows
            forM_ chosen \value -> do
                home <- getHomeDirectory
                updateHarnessConfig home (\config -> Right config{configTheme = value})
                    >>= either (throwIO . RemoteFailure) (const (pure ()))
                forM_ ui.remoteFullscreen (\runtime -> enqueueAppEvent runtime (AppSetTheme value))
    case action of
        ReplExport path -> do
            destination <- resolveExportPath session.remoteCwd
                (fromMaybe (defaultExportFileName session.remoteId) path) >>= either (throwIO . RemoteFailure) pure
            contents <- renderTranscriptMarkdown <$> turns
            saveTranscriptNoClobber destination contents >>= either (throwIO . RemoteFailure) (const (pure ()))
            remoteNotice ui ("Exported conversation to " <> toText destination)
            pure True
        ReplCopy request -> do
            text <- selected request.copyResponseIndex
            case request.copyDestination of
                Nothing -> copy text
                Just path -> do
                    destination <- resolveExportPath session.remoteCwd path >>= either (throwIO . RemoteFailure) pure
                    saveCopyText destination text >>= either (throwIO . RemoteFailure) (const (pure ()))
                    remoteNotice ui ("Saved to " <> toText destination)
            pure True
        ReplCopyCode index -> copyBlock (fencedCodeBlock index) >> pure True
        ReplCopyDiff -> copyBlock lastDiffBlock >> pure True
        ReplCopyPath -> copy (toText session.remoteCwd) >> pure True
        ReplCopySession -> copy session.remoteId >> pure True
        ReplChangelog -> loadReleaseNotes >>= document "Changelog" >> pure True
        ReplTranscript -> turns >>= document "Transcript" . renderTranscriptMarkdown >> pure True
        ReplFind query -> do
            needle <- maybe (remoteText ui "Find in this conversation:") (pure . Just) query
            forM_ needle \value -> do
                history <- turns
                let matches = filter (Text.isInfixOf (Text.toCaseFold value) . Text.toCaseFold . renderTranscriptMarkdown . pure) history
                document "Search results" (renderTranscriptMarkdown matches)
            pure True
        ReplEditPrompt -> do
            previous <- maybe "" fst <$> readIORef ui.remoteLastPrompt
            editor <- resolveExternalProgram [("VISUAL", "visual editor"), ("EDITOR", "editor")] "vi"
                >>= either (throwIO . RemoteFailure) pure
            let edit = withTemporaryTextFile "agent-prompt.txt" previous \path -> do
                    runExternalProgramOnFile editor path >>= either (throwIO . RemoteFailure) pure
                    normalizeEditedText <$> TextIO.readFile path
            edited <- maybe edit (\runtime -> withFullscreenSuspended runtime edit) ui.remoteFullscreen
            if Text.null edited then pure () else submit edited
            pure True
        ReplSelection ReplShowTheme -> theme Nothing >> pure True
        ReplSelection (ReplSetTheme name) -> theme (Just name) >> pure True
        ReplMouseCapture requested -> do
            case ui.remoteFullscreen of
                Nothing -> remoteNotice ui "Mouse capture requires fullscreen mode."
                Just runtime -> do
                    current <- readIORef runtime.runtimeMouseCapture
                    runtime.runtimeSetMouseCapture (fromMaybe (not current) requested)
            pure True
        _ -> pure False
