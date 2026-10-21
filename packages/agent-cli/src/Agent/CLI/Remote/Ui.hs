-- | Terminal-only state for a connected session. No provider, database or MCP
-- resource is acquired by this frontend.
module Agent.CLI.Remote.Ui
    ( RemoteUi(..), withRemoteUi, remotePrompt, readRemoteLine
    , emitRemoteLoop, remoteNotice, remoteFailure, remoteChoice, remoteText
    , handleRemoteHumanRequest, renderRemoteHistory, showRemoteImage, previewRemoteImages
    , updateRemoteCatalog
    , updateRemotePreferences
    ) where

import Agent.Cancel
import Agent.CLI.AgentViewport (AgentTarget(..))
import Agent.CLI.AppleFollowUp (maintainAppleFollowUpRouter)
import Agent.CLI.AppleTitle (probeAppleFoundationTitle)
import Agent.CLI.CancelWatch
import Agent.CLI.Dictation (dictationTargetForSession)
import Agent.CLI.Command (defaultSlashCatalog, SlashCatalog, SkillCommand(..), mkSlashCatalog, ReplAction(..))
import Agent.CLI.Input
import Agent.CLI.Interrupt
import Agent.CLI.Options
import Agent.CLI.McpElicitation (cliMcpElicitation)
import Agent.CLI.Session.Attachments (putImagePreview)
import Agent.CLI.Secret (promptSecretLine)
import Agent.CLI.Remote.Transport
import Agent.CLI.Remote.History
import Agent.CLI.TUI.History
import Agent.CLI.TUI.SessionHistory (sessionHistoryPage)
import Agent.Runtime.Session (SessionTurn(..), SessionTurnPage(..))
import Agent.CLI.Render
import Agent.CLI.Style (setCliWindowTitle)
import Agent.CLI.Terminal
import Agent.CLI.TerminalDiagnostics (getTerminalStderr)
import Agent.CLI.TUI.App
import Agent.CLI.TUI.Types (FullscreenInput(..), FullscreenSessionActions(..), FullscreenRuntime(..), AppEvent(..))
import Agent.CLI.TUI.Composer.Buffer qualified as Composer
import Agent.Loop
import Agent.OsPath (toText)
import Agent.Runtime.Session.Settings
import Agent.Runtime.Timestamp (MessageClock)
import Agent.Runtime.Config (HarnessConfig(..), loadHarnessConfig)
import Agent.Runtime.GatewayClient
    (loadGatewayCredentialAt, newGatewayModelAccess, gatewayCredentialIdentity)
import Agent.Runtime.Project (ProjectSettings(..), resolveProjectRoot, loadProjectSettings)
import Agent.Runtime.Session.Interaction
import Agent.Runtime.Session.SecretExchange (writeSecretExchange)
import Agent.Tools.Secret (SecretPrompt(..))
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.ByteString.Base64 qualified as Base64
import Data.Text.Encoding qualified as Encoding
import Agent.Runtime.Request qualified
import Agent.Server.Client
import Agent.TUI.Model
import Agent.TUI.Theme qualified
import Control.Concurrent.MVar (newMVar)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM
import Control.Exception.Safe (finally, catch, throwIO)
import Control.Monad (forM_, unless)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.IO (hIsTerminalDevice, stdin, stdout, Handle)
import System.Directory.OsPath (getHomeDirectory)

data RemoteUi = RemoteUi
    { remoteFullscreen :: !(Maybe FullscreenRuntime)
    , remoteRender :: !RenderConfig
    , remoteInterrupt :: !InterruptState
    , remoteStdin :: !StdinControl
    , remoteCancel :: !(IORef (Maybe CancelFlag))
    , remoteSettings :: !(IORef SessionSettings)
    , remoteCurrentSession :: !(IORef RemoteSession)
    , remoteImages :: !(IORef [ImageAttachment])
    , remoteLastPrompt :: !(IORef (Maybe (Text, [ImageAttachment])))
    , remoteOutput :: !Handle
    , remoteUseColor :: !Bool
    , remotePreviewId :: !(IORef Int)
    , remoteCatalog :: !(IORef SlashCatalog)
    , remoteFiles :: !(IORef [FileAttachment])
    , remoteLastFiles :: !(IORef [FileAttachment])
    , remoteClock :: !(Maybe MessageClock)
    , remoteCommands :: !(TQueue ReplAction)
    }

withRemoteUi :: CliOptions -> RemoteSession -> (RemoteUi -> IO a) -> IO a
withRemoteUi options session use = do
    remoteOutput <- getTerminalStderr
    remoteUseColor <- resolveColor stdout
    home <- getHomeDirectory
    harness <- loadHarnessConfig home >>= either (const (pure Nothing)) (pure . Just)
    project <- loadProjectSettings =<< resolveProjectRoot session.remoteCwd
    terminal <- detectTerminalCapabilities stdout
    stdinTty <- hIsTerminalDevice stdin
    stdoutTty <- hIsTerminalDevice stdout
    remoteStdin <- newStdinControl
    remoteCancel <- newIORef Nothing
    remoteCurrentSession <- newIORef session
    let settings = (sessionSettingsForOptions options)
            {sessionInteractionMode = if options.optYolo == Inherit && project.settingsAutoApprove
                then Agent.Runtime.Request.NativeYolo else (sessionSettingsForOptions options).sessionInteractionMode}
    remoteSettings <- newIORef settings
    remoteImages <- newIORef []
    remoteLastPrompt <- newIORef Nothing
    remotePreviewId <- newIORef 1
    remoteCatalog <- newIORef defaultSlashCatalog
    remoteFiles <- newIORef []
    remoteLastFiles <- newIORef []
    remoteCommands <- newTQueueIO
    let remoteClock = options.optMessageClock
    remoteInterrupt <- newInterruptState (Text.hPutStrLn remoteOutput)
    renderThinkingSpinner <- newIORef Nothing
    renderState <- newIORef emptyRenderState
    renderLock <- newMVar ()
    let remoteRender = RenderConfig
            { renderShowThinking = True, renderThinkingSpinner, renderState
            , renderColor = remoteUseColor, renderLock, renderStdout = stdout
            , renderStderr = remoteOutput, renderModel = (.remoteModel) <$> readIORef remoteCurrentSession
            , renderNativeProgress = terminal.terminalNativeProgress
            , renderMotionMode = options.optMotionMode, renderWorkspace = toText session.remoteCwd }
        cancel = readIORef remoteCancel >>= mapM_ requestCancel
        fullscreenEnabled = not (isOneShot options) && stdinTty && stdoutTty
            && options.optScreenMode /= ScreenMinimal
    remoteFullscreen <- if fullscreenEnabled
        then do
            input <- newFullscreenInputBuffer
            Just <$> newFullscreenRuntimeWithTheme (maybe Agent.TUI.Theme.Midnight (.configTheme) harness) input cancel
                (\_ -> cancel) (noteFullscreenCtrlC remoteInterrupt)
                (copyTerminalClipboard terminal stdout)
                (setCliWindowTitle stdoutTty stdout)
                (const (pure ()))
                (pure (AgentRoot, [])) (const (pure ())) (pure ()) (const (pure ()))
                options.optMotionMode remoteUseColor initialUiState
        else pure Nothing
    let ui = RemoteUi{..}
        action = do
            forM_ remoteFullscreen \runtime -> do
                atomically $ modifyTVar' runtime.runtimeSessionActions \actions -> actions
                    { sessionSteer = \_ text -> atomically $
                        Composer.promoteFullscreenInput runtime.runtimeInput
                            FullscreenInput
                                { fullscreenInputLine = ReplText text, fullscreenInputQueued = False
                                , fullscreenInputDisplay = Just text, fullscreenInputFromInbox = False }
                    , sessionBtw = atomically . writeTQueue remoteCommands . ReplBtw
                    , sessionImmediateCommand = atomically . writeTQueue remoteCommands
                    , sessionRecap = atomically (writeTQueue remoteCommands ReplRecap)
                    }
                emitUiEvent runtime (UiSetRepository "" (toText session.remoteCwd) (toText session.remoteCwd))
                emitUiEvent runtime (UiSetPrompt (remotePrompt session settings 0))
            case remoteFullscreen of
                Nothing -> use ui
                Just runtime -> withAsync
                    (probeAppleFoundationTitle >>= mapM_ (\path -> maintainAppleFollowUpRouter path
                        (writeIORef runtime.runtimeRouteFollowUp)
                        (enqueueAppEvent runtime . AppSetFollowUpRouting)))
                    (const (use ui))
    withCtrlCHandler remoteInterrupt $
        maybe action (\runtime -> runFullscreen runtime action) remoteFullscreen
            `finally` clearThinking remoteRender

remotePrompt :: RemoteSession -> SessionSettings -> Int -> PromptState
remotePrompt session settings attachments = PromptState
    { promptModel = session.remoteModel, promptEffort = session.remoteEffort
    , promptEffortOptions = ["minimal", "low", "medium", "high", "xhigh", "max"]
    , promptMode = if settings.sessionReadOnly then "read-only" else case settings.sessionInteractionMode of
        Agent.Runtime.Request.NativeAsk -> "ask"
        Agent.Runtime.Request.NativeYolo -> "yolo"
        Agent.Runtime.Request.NativePlan -> "plan"
    , promptAccount = "", promptAccountSelectable = False
    , promptUsage = session.remoteUsage, promptLimitStatus = Nothing, promptAttachments = attachments }

readRemoteLine :: RemoteUi -> Text -> IO ReplLine
readRemoteLine RemoteUi{..} draft = do
    session <- readIORef remoteCurrentSession
    settings <- readIORef remoteSettings
    images <- readIORef remoteImages
    catalog <- readIORef remoteCatalog
    case remoteFullscreen of
        Just runtime -> do
            gateway <- case session.remoteGatewayIdentity of
                Nothing -> pure Nothing
                Just expected -> do
                    home <- getHomeDirectory
                    loadGatewayCredentialAt home >>= \case
                        Right (Just credential) | gatewayCredentialIdentity credential == expected ->
                            Just <$> newGatewayModelAccess credential
                        _ -> throwIO (RemoteFailure "Reconnect the organization gateway before using this session.")
            atomically $ modifyTVar' runtime.runtimeSessionActions \actions -> actions
                {sessionDictationTarget = Just (dictationTargetForSession session.remoteProvider gateway)}
            readFullscreenLineWithCatalog runtime catalog (remotePrompt session settings (length images)) draft
        Nothing -> readReplLineWithCatalogForProvider session.remoteProvider
            catalog remoteInterrupt "> " draft

emitRemoteLoop :: RemoteUi -> LoopEvent -> IO ()
emitRemoteLoop RemoteUi{..} event = case remoteFullscreen of
    Just runtime -> emitUiEvent runtime (UiLoop event)
    Nothing -> renderEvent remoteRender event

remoteNotice :: RemoteUi -> Text -> IO ()
remoteNotice RemoteUi{..} message = case remoteFullscreen of
    Just runtime -> emitUiEvent runtime (UiSystemMessage message)
    Nothing -> Text.hPutStrLn remoteOutput message

previewRemoteImages :: RemoteUi -> [ImageAttachment] -> IO ()
previewRemoteImages ui images = case ui.remoteFullscreen of
    Just runtime -> setFullscreenImagePreviews runtime images
    Nothing -> putImagePreview ui.remotePreviewId ui.remoteUseColor images

updateRemoteCatalog :: RemoteUi -> Value -> IO (Either AgentServerClientError ())
updateRemoteCatalog ui value = case parseEither parse value of
    Left _ -> pure (Left (AgentServerProtocolError "invalid local tool catalog"))
    Right (tools, skills) -> do
        session <- readIORef ui.remoteCurrentSession
        writeIORef ui.remoteCatalog (mkSlashCatalog True session.remoteDialect tools skills [session.remoteModel])
        pure (Right ())
  where
    parse = withObject "local tool catalog" \fields -> (,)
        <$> fields .: "tools"
        <*> (fields .: "skills" >>= traverse (withObject "skill" \skill -> SkillCommand
            <$> skill .: "name" <*> skill .: "summary" <*> skill .:? "argumentHint" <*> skill .: "source"))

updateRemotePreferences :: RemoteUi -> Value -> IO (Either AgentServerClientError ())
updateRemotePreferences ui value = case parseEither parse value of
    Left _ -> pure (Left (AgentServerProtocolError "invalid local session preferences"))
    Right (allowed, mode, goal) -> do
        modifyIORef' ui.remoteSettings (\settings -> settings
            { sessionAllowedTools = allowed, sessionGoal = goal
            , sessionInteractionMode = case (mode :: Text) of
                "plan" -> Agent.Runtime.Request.NativePlan
                "yolo" -> Agent.Runtime.Request.NativeYolo
                _ -> Agent.Runtime.Request.NativeAsk })
        pure (Right ())
  where
    parse = withObject "local session preferences" \fields -> (,,)
        <$> fields .: "allowedTools" <*> fields .: "interactionMode" <*> fields .:? "goal"

showRemoteImage :: RemoteUi -> Value -> IO (Either AgentServerClientError ())
showRemoteImage ui value = case parseEither parse value of
    Left _ -> pure (Left (AgentServerProtocolError "invalid local image presentation"))
    Right (identifier, image) -> do
        case ui.remoteFullscreen of
            Just runtime -> showFullscreenToolImage runtime identifier image
            Nothing -> putImagePreview ui.remotePreviewId ui.remoteUseColor [image] >> pure (Right ())
        >>= pure . either (Left . AgentServerProtocolError) Right
  where
    parse = withObject "image presentation" \fields -> do
        identifier <- fields .: "callId"
        mime <- fields .: "mime"
        bytes <- fields .: "bytes" >>= either fail pure . Base64.decode . Encoding.encodeUtf8
        pure (identifier, ImageAttachment mime bytes)

remoteFailure :: RemoteUi -> Text -> IO ()
remoteFailure RemoteUi{..} message = case remoteFullscreen of
    Just runtime -> emitUiEvent runtime (UiErrorMessage message)
    Nothing -> Text.hPutStrLn remoteOutput message

remoteChoice :: RemoteUi -> Text -> Text -> [Text] -> IO (Maybe Int)
remoteChoice RemoteUi{..} title body options = case remoteFullscreen of
    Just runtime -> requestFullscreenChoiceWithBody runtime title body 0 [(option, "") | option <- options]
    Nothing -> withStdinPaused remoteStdin do
        clearThinking remoteRender
        Text.hPutStrLn remoteOutput (title <> if Text.null body then "" else "\n" <> body)
        selected <- readChoiceSelection (\_ -> id) options
        pure (selected >>= \value -> lookup value (zip options [0 ..]))

remoteText :: RemoteUi -> Text -> IO (Maybe Text)
remoteText RemoteUi{..} body = case remoteFullscreen of
    Just runtime -> requestFullscreenPlanningText runtime body
    Nothing -> withStdinPaused remoteStdin do
        clearThinking remoteRender
        Text.hPutStrLn remoteOutput body
        readApprovalLine "> "

handleRemoteHumanRequest :: RemoteUi -> AgentServerClient -> AgentServerHumanRequest -> IO ()
handleRemoteHumanRequest ui client request = do
    let options = request.agentServerRequestOptions
    response <- if request.agentServerRequestKind == AgentServerLocalInput
        then case decodeLocalInput request.agentServerRequestPrompt of
            Left _ -> pure (AgentServerResolveRequest "cancel" Nothing)
            Right (LocalElicitation elicitation) -> do
                fullscreen <- newIORef ui.remoteFullscreen
                reply <- cliMcpElicitation ui.remoteStdin fullscreen elicitation
                pure (AgentServerResolveRequest "answer" (Just (encodeElicitationReply reply)))
            Right (LocalSecret prompt path) -> do
                value <- case ui.remoteFullscreen of
                    Just runtime -> requestFullscreenSecret runtime prompt.secretPromptMessage
                        (fromMaybe "" prompt.secretPromptPurpose)
                    Nothing -> promptSecretLine ui.remoteStdin prompt.secretPromptMessage prompt.secretPromptPurpose
                case value of
                    Nothing -> pure (AgentServerResolveRequest "cancel" Nothing)
                    Just secret -> do
                        writeSecretExchange path secret
                        pure (AgentServerResolveRequest "answer" Nothing)
        else if null options
        then do
            value <- remoteText ui request.agentServerRequestPrompt
            pure (AgentServerResolveRequest (if value == Nothing then "cancel" else "answer") value)
        else do
            selected <- remoteChoice ui "Agent request" request.agentServerRequestPrompt options
            let decision = maybe (if "deny" `elem` options then "deny" else "cancel") (options !!) selected
            if decision `elem` ["custom", "request_changes"]
                then do
                    value <- remoteText ui (if decision == "custom" then "Your reply:" else "Requested changes:")
                    pure (AgentServerResolveRequest (if value == Nothing then "cancel" else decision) value)
                else pure (AgentServerResolveRequest decision Nothing)
    resolveAgentServerRequest client request.agentServerRequestId response >>= \case
        Left err -> remoteFailure ui (remoteErrorText err)
        Right _ -> pure ()

renderRemoteHistory :: RemoteUi -> AgentServerClient -> IO ()
renderRemoteHistory ui@RemoteUi{..} client = do
    session <- readIORef remoteCurrentSession
    page <- loadRemoteHistoryPage client session.remoteId Nothing Nothing 20
    case remoteFullscreen of
        Just runtime -> do
            let generation = HistoryGeneration 0
                load request = do
                    let cursor = (\(HistoryCursor value) -> value) <$> request.historyRequestCursor
                        (before, after) = case request.historyRequestDirection of
                            HistoryOlder -> (cursor, Nothing)
                            HistoryNewer -> (Nothing, cursor)
                    (Right . sessionHistoryPage request.historyRequestGeneration request.historyRequestDirection
                        <$> loadRemoteHistoryPage client session.remoteId before after 20)
                        `catch` \(RemoteFailure message) -> pure (Left message)
            reloadFullscreenHistorySource runtime session.remoteId load
                (sessionHistoryPage generation HistoryOlder page)
        Nothing -> forM_ page.pageTurns \(_, turn) -> do
            unless (Text.null turn.turnUserText) (Text.hPutStrLn stdout ("> " <> turn.turnUserText))
            forM_ turn.turnAssistantText (Text.hPutStrLn stdout)
            forM_ turn.turnError (remoteFailure ui)
