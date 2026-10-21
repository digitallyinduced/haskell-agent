-- | The terminal frontend owns presentation and an expiring server lease.
module Agent.CLI.Remote.Session (runRemoteSession) where

import Agent.CLI.Clipboard
import Agent.CLI.Afk
import Agent.CLI.Command
import Agent.CLI.Desktop (openDesktopConversation)
import Agent.CLI.GitDiff
import Agent.CLI.Input (ReplLine(..))
import Agent.CLI.Interrupt (catchUserInterrupt)
import Agent.CLI.Login (runLoginManager)
import Agent.CLI.Options
import Agent.CLI.Remote.Transport
import Agent.CLI.Remote.Turn
import Agent.CLI.Remote.Ui
import Agent.CLI.Remote.Commands (presentationCommand)
import Agent.CLI.Remote.Meta (runRemoteMeta)
import Agent.CLI.Remote.Voice (runRemoteVoice)
import Agent.CLI.Remote.History (loadRemoteTranscript)
import Agent.CLI.TranscriptExport (renderTranscriptMarkdown)
import Agent.CLI.Runtime.Types (DevResult(..))
import Agent.CLI.TUI.App (emitUiEvent, beginFullscreenLiveHistory, commitFullscreenImagePreviews)
import Agent.CLI.TUI.Types (FullscreenRuntime(..))
import Agent.CLI.TerminalDiagnostics (withInteractiveDiagnostics)
import Agent.CLI.TerminalStatus (withTerminalStatus)
import Agent.CLI.Worktree (createManagedWorktree)
import Agent.Loop (ImageAttachment, FileAttachment, turnInputImages, turnInputFiles)
import Agent.OsPath (toText, unsafeToFilePath)
import Agent.ReasoningEffort (reasoningEffortText)
import Agent.Runtime.ManagedTurn
import Agent.Runtime.Request
import Agent.Runtime.Session (SessionTurn(..))
import Agent.Runtime.Session (SessionTransfer(..), SessionMeta(..), sessionTransferDecoder, sessionsRoot, sessionDirForId)
import Agent.Runtime.Json (decodeLazy)
import Agent.Runtime.Project (saveUserTitleModel, loadUserSettings, ProjectSettings(..), TitleModelSetting(..))
import Agent.Runtime.Models (ModelTarget(..))
import Agent.Runtime.Session.Settings
import Agent.Server.Client
import Agent.Server.Client.Local
import Agent.TUI.Model (UiEvent(..))
import Control.Exception.Safe (bracket, catch, throwIO)
import Control.Concurrent.Async (withAsync, link)
import Control.Concurrent.STM (atomically, readTQueue)
import Control.Monad (forM_, unless, void, when, forever)
import Agent.CLI.TUI.Composer.Buffer qualified as Composer
import Data.Aeson (Value, object, (.=), encode)
import Data.ByteString.Lazy qualified as Lazy
import Data.Text.Encoding qualified as Encoding
import Data.Aeson qualified
import Data.IORef
import Data.Foldable qualified as Foldable
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory.OsPath (getCurrentDirectory, getHomeDirectory, makeAbsolute)
import System.OsPath (OsPath)

runRemoteSession :: CliOptions -> IO DevResult
runRemoteSession options =
    withTerminalStatus (not (isOneShot options)) $
    withInteractiveDiagnostics (not (isOneShot options)) $
    catchUserInterrupt action (pure DevQuit)
  where
    action = do
        service <- ensureLocalService >>= either
            (throwIO . RemoteFailure . Text.pack . show) pure
        withLocalClientLease service (\client lease -> do
            cwd <- makeAbsolute =<< maybe getCurrentDirectory pure options.optCwd
            loaded <- case options.optResume of
                Just (ResumeSession identifier) -> remoteSession client identifier
                Just ResumeLatest -> remoteSessions client >>= \case
                    [] -> throwIO (RemoteFailure "No saved session found.")
                    recent : _ -> pure recent
                Nothing -> do
                    workspace <- if options.optWorktree
                        then do
                            home <- getHomeDirectory
                            createManagedWorktree home cwd
                                >>= either (throwIO . RemoteFailure) pure
                        else pure cwd
                    createRemoteSession client options workspace
            _ <- attachLocalClientSession client lease loaded.remoteId >>= requireRemote
            session <- if options.optResume == Nothing then pure loaded else do
                selected <- case options.optModel of
                    Nothing -> pure loaded
                    Just name -> changeSelection client loaded "model" name
                case options.optEffort of
                    Nothing -> pure selected
                    Just effort -> changeSelection client selected "effort" (reasoningEffortText effort)
            withRemoteUi options session \ui -> withAsync (immediateCommands ui client lease) \worker -> do
                link worker
                unless (isOneShot options) (renderRemoteHistory ui client)
                initial <- initialPrompt options session.remoteCwd
                case initial of
                    Just (prompt, images, files) -> do
                        writeIORef ui.remoteFiles files
                        sendPrompt ui client prompt images
                        pure DevQuit
                    Nothing -> loop ui client lease ""
            ) >>= requireRemote
    immediateCommands ui client lease = forever do
        next <- atomically (readTQueue ui.remoteCommands)
        void (handle ui client lease next) `catch` \(RemoteFailure message) -> remoteFailure ui message
    loop ui client lease draft = do
        outcome <- (Right <$> readRemoteLine ui draft) `catch` \(RemoteFailure message) ->
            remoteFailure ui message >> pure (Left ())
        case outcome of
            Left () -> loop ui client lease draft
            Right line -> case line of
                ReplEof -> quit ui
                ReplQuitInterrupt -> quit ui
                ReplText text -> dispatch ui client lease text
                ReplPasted text -> loop ui client lease text
                ReplMeta text -> dispatch ui client lease ("/meta " <> text)
                ReplClipboardPaste text captured -> do
                    images <- maybe (readClipboardImagesForPaste >>= either (const (pure [])) pure) pure captured
                    addImages ui images
                    loop ui client lease text
                ReplClipboardPasteCaptured images -> addImages ui images >> loop ui client lease ""
                ReplRemoveCapturedImage image -> modifyIORef' ui.remoteImages (filter (/= image)) >> loop ui client lease ""
                ReplClipboardPasteOrText before text inserted -> loadImagesFromPastedText text >>= \case
                    Just images -> addImages ui images >> loop ui client lease before
                    Nothing -> loop ui client lease inserted
                ReplRemovePendingImage text index -> removeImage ui index >> loop ui client lease text
                ReplCycleMode text -> do
                    modifyIORef' ui.remoteSettings \settings -> settings
                        { sessionInteractionMode = case settings.sessionInteractionMode of
                            NativeAsk -> NativeYolo; NativeYolo -> NativePlan; NativePlan -> NativeAsk }
                    loop ui client lease text
                ReplChooseModel text -> chooseModel ui client >> loop ui client lease text
                ReplChooseEffort text -> do
                    choice <- remoteText ui "Reasoning effort:"
                    forM_ choice (control ui client "effort" . Just . textValue)
                    loop ui client lease text
                ReplChooseAccount text -> remoteNotice ui "Use /login to select the account." >> loop ui client lease text
    dispatch ui client lease text = do
        catalog <- readIORef ui.remoteCatalog
        result <- (handle ui client lease (parseReplLineWithCatalog catalog text)) `catch` \(RemoteFailure message) ->
            remoteFailure ui message >> pure Nothing
        maybe (loop ui client lease "") pure result
    handle ui client lease = \case
        ReplVoice -> runRemoteVoice ui client >> pure Nothing
        ReplMetaConsole request -> runRemoteMeta ui client (\line -> void (handle ui client lease (parseReplLine line))) request >> pure Nothing
        ReplViewPlan -> command ui client "plan" [] >>= notice ui
        ReplContext -> command ui client "context" [] >>= notice ui
        ReplAgents -> command ui client "agents" [] >>= notice ui
        ReplMcp -> command ui client "mcp" [] >>= notice ui
        ReplSkills _ -> command ui client "skills" [] >>= notice ui
        ReplInvokeSkill name arguments -> do
            expanded <- command ui client "skill" ["name" .= name, "arguments" .= arguments]
            sendPrompt ui client expanded []
            pure Nothing
        ReplMcpPrompt server name pairs -> do
            expanded <- command ui client "mcpPrompt" ["server" .= server, "name" .= name, "pairs" .= pairs]
            sendPrompt ui client expanded []
            pure Nothing
        ReplQuit -> Just <$> quit ui
        ReplReload -> Just . DevReload . (.remoteId) <$> readIORef ui.remoteCurrentSession
        ReplPrompt text -> do
            images <- atomicModifyIORef' ui.remoteImages (\current -> ([], current))
            unless (Text.null (Text.strip text) && null images) (sendPrompt ui client text images)
            pure Nothing
        ReplQueuedPrompt text -> handle ui client lease (ReplPrompt text)
        ReplExpandedPrompt _ expanded -> handle ui client lease (ReplPrompt expanded)
        ReplInit -> handle ui client lease (ReplPrompt initInstruction)
        ReplReview focus -> handle ui client lease (ReplPrompt ("Review the current changes. " <> fromMaybe "" focus))
        ReplHelp command -> notice ui (formatSlashHelp ui.remoteUseColor command)
        ReplCommandError message -> remoteFailure ui message >> pure Nothing
        ReplRetry -> readIORef ui.remoteLastPrompt >>= \case
            Nothing -> notice ui "No previous turn."
            Just (prompt, images) -> do
                readIORef ui.remoteLastFiles >>= writeIORef ui.remoteFiles
                sendPrompt ui client prompt images
                pure Nothing
        ReplBtw question -> sideQuestion ui client lease question >> pure Nothing
        ReplRecap -> sideQuestion ui client lease "Briefly recap the work so far and the next step." >> pure Nothing
        ReplCompact focus -> runRemoteTurn ui client CompactContext (fromMaybe "Compact the conversation context." focus) [] >> pure Nothing
        ReplPlan description -> do
            modifyIORef' ui.remoteSettings (\settings -> settings{sessionInteractionMode = NativePlan})
            forM_ description (\prompt -> sendPrompt ui client prompt [])
            pure Nothing
        ReplToggleAlwaysApprove -> do
            modifyIORef' ui.remoteSettings (\settings -> settings
                {sessionInteractionMode = if settings.sessionInteractionMode == NativeYolo then NativeAsk else NativeYolo})
            settings <- readIORef ui.remoteSettings
            notice ui (if settings.sessionInteractionMode == NativeYolo then "Auto-approval enabled." else "Ask before changes.")
        ReplEnableCodeMode -> modifyIORef' ui.remoteSettings (\settings -> settings{sessionCodeMode = "enabled"}) >> notice ui "Code mode enabled."
        ReplShowAgentLimit -> readIORef ui.remoteSettings >>= notice ui . Text.pack . show . (.sessionMaxConcurrentAgents)
        ReplSetAgentLimit limit -> modifyIORef' ui.remoteSettings (\settings -> settings{sessionMaxConcurrentAgents = Just limit}) >> pure Nothing
        ReplShowShell -> readIORef ui.remoteSettings >>= notice ui . Text.pack . show . (.sessionShellMode)
        ReplSetShell shell -> modifyIORef' ui.remoteSettings (\settings -> settings{sessionShellMode = case shell of
            ShellNone -> NativeShellNone; ShellBash -> NativeShellBash; ShellGhci -> NativeShellGhci; ShellBoth -> NativeShellBoth}) >> pure Nothing
        ReplToggleComputerUse -> modifyIORef' ui.remoteSettings (\settings -> settings{sessionComputerUse = not settings.sessionComputerUse}) >> pure Nothing
        ReplSetComputerUse enabled -> modifyIORef' ui.remoteSettings (\settings -> settings{sessionComputerUse = enabled}) >> pure Nothing
        ReplWorkflow workflow -> case workflow of
            ReplGoalSet _ objective budget expanded -> do
                modifyIORef' ui.remoteSettings (\settings -> settings{sessionGoal = Just (objective, budget, False)})
                sendPrompt ui client expanded []
                pure Nothing
            ReplGoalClear -> modifyIORef' ui.remoteSettings (\settings -> settings{sessionGoal = Nothing}) >> notice ui "Goal cleared."
            ReplGoalPause -> goalPaused ui True >> notice ui "Goal paused."
            ReplGoalResume -> goalPaused ui False >> notice ui "Goal resumed."
            ReplGoalStatus -> readIORef ui.remoteSettings >>= notice ui . maybe "No goal is active." (Text.pack . show) . (.sessionGoal)
            ReplWorkflowRuns -> command ui client "workflows" [] >>= notice ui
            ReplWorkflowManage operation target -> notice ui
                ("workflow_management_unsupported: /workflow " <> operation <> fromMaybe "" target)
        ReplUsage -> readIORef ui.remoteCurrentSession >>= notice ui . Text.pack . show . (.remoteUsage)
        ReplPermissions -> readIORef ui.remoteSettings >>= notice ui . Text.pack . show . (.sessionInteractionMode)
        ReplLogin -> runLoginManager ui.remoteUseColor >> pure Nothing
        ReplReloadAuth -> notice ui "Credentials are reloaded by the server for each turn."
        ReplDesktop -> readIORef ui.remoteCurrentSession >>= openDesktopConversation . (.remoteId)
            >>= either (remoteFailure ui) (const (pure ())) >> pure Nothing
        ReplDiff -> do
            session <- readIORef ui.remoteCurrentSession
            getGitDiff session.remoteCwd >>= \case
                Left err -> remoteFailure ui err
                Right GitDiffNotRepository -> remoteNotice ui "This directory is not a Git repository."
                Right (GitDiffOutput diff) -> remoteNotice ui (colorizeGitDiff ui.remoteUseColor diff)
            pure Nothing
        ReplHistory -> renderRemoteHistory ui client >> pure Nothing
        ReplSelection selection -> select ui client selection >> pure Nothing
        ReplAttachment attachment -> attachments ui attachment >> pure Nothing
        ReplSession session -> sessionAction ui client lease session
        ReplQueue -> do
            queued <- maybe (pure []) (fmap Foldable.toList . Composer.queuedFullscreenInputDisplays . (.runtimeInput)) ui.remoteFullscreen
            notice ui (if null queued then "No queued input." else Text.unlines queued)
        ReplShowTerminal -> notice ui "The session is attached to this terminal."
        action -> do
            handled <- presentationCommand ui client (\prompt -> sendPrompt ui client prompt []) action
            unless handled (remoteFailure ui ("Command is unavailable for this provider: " <> Text.pack (show action)))
            pure Nothing
    select ui client = \case
        ReplShowTitleModel -> do
            home <- getHomeDirectory
            settings <- loadUserSettings home
            models <- remoteModels client
            choice <- remoteChoice ui ("Title model · " <> Text.pack (show settings.settingsTitleModel))
                "" ("Automatic" : map (.remoteModelId) models)
            forM_ choice (\index -> if index == 0 then saveUserTitleModel home Nothing
                else saveTitleModel home (models !! (index - 1)))
        ReplSetTitleModel name -> do
            models <- remoteModels client
            case find ((== name) . (.remoteModelId)) models of
                Nothing -> throwIO (RemoteFailure "Unknown title model.")
                Just model -> getHomeDirectory >>= (\home -> saveTitleModel home model)
        ReplClearTitleModel -> getHomeDirectory >>= (\home -> saveUserTitleModel home Nothing)
        ReplToggleFast -> do
            modifyIORef' ui.remoteSettings (\settings -> settings{sessionFast = not settings.sessionFast})
            enabled <- (.sessionFast) <$> readIORef ui.remoteSettings
            remoteNotice ui (if enabled then "Fast mode enabled." else "Fast mode disabled.")
        ReplShowModel -> chooseModel ui client
        ReplSetModel name -> control ui client "model" (Just (textValue name))
        ReplShowEffort -> readIORef ui.remoteCurrentSession >>= remoteNotice ui . (.remoteEffort)
        ReplSetEffort effort -> control ui client "effort" (Just (textValue (reasoningEffortText effort)))
        action -> void (presentationCommand ui client (\prompt -> sendPrompt ui client prompt []) (ReplSelection action))
    chooseModel ui client = do
        models <- remoteModels client
        choice <- remoteChoice ui "Model" "" (map (.remoteModelId) models)
        forM_ choice (\index -> control ui client "model" (Just (textValue (models !! index).remoteModelId)))
    saveTitleModel home model = saveUserTitleModel home (Just (TitleModelPinned
        (ModelTarget model.remoteModelProvider model.remoteModelConnection model.remoteModelId
            model.remoteWireModel model.remoteModelDialect)))
    sessionAction ui client lease = \case
        ReplShowSession -> readIORef ui.remoteCurrentSession >>= notice ui . (.remoteId)
        ReplShowSessionInfo -> readIORef ui.remoteCurrentSession >>= notice ui . Text.pack . show
        ReplWorktree -> readIORef ui.remoteCurrentSession >>= notice ui . toText . (.remoteCwd)
        ReplRename title -> patch ui client (object ["title" .= title]) >> pure Nothing
        ReplRenameAuto -> do
            history <- loadRemoteTranscript client =<< ((.remoteId) <$> readIORef ui.remoteCurrentSession)
            title <- runRemoteQuestion ui client PrivateQuestion
                ("Return a short conversation title, without quotes or punctuation, for:\n"
                    <> Text.take 8000 (Text.intercalate "\n" [turn.turnUserText | (_, turn) <- history]))
            patch ui client (object ["title" .= Text.take 120 (Text.strip title)])
            pure Nothing
        ReplSearch query -> do
            sessions <- remoteSessions client
            let matches = filter (Text.isInfixOf (Text.toCaseFold query) . Text.toCaseFold . (.remoteTitle)) sessions
            choice <- remoteChoice ui "Matching sessions" "" (map (.remoteTitle) matches)
            forM_ choice (\index -> attach ui client lease (matches !! index) >> renderRemoteHistory ui client)
            pure Nothing
        ReplResume target -> resume ui client lease target >> pure Nothing
        ReplHome -> resume ui client lease Nothing >> pure Nothing
        ReplNew -> do
            current <- readIORef ui.remoteCurrentSession
            fresh <- createRemoteSession client options{optResume = Nothing, optModel = Just current.remoteModel} current.remoteCwd
            attach ui client lease fresh
            pure Nothing
        ReplFork request -> do
            source <- readIORef ui.remoteCurrentSession
            cwd <- if request.forkWorktree == Just True
                then do
                    home <- getHomeDirectory
                    createManagedWorktree home source.remoteCwd >>= either (throwIO . RemoteFailure) pure
                else pure source.remoteCwd
            fresh <- remoteJson client "POST" (sessionPath source <> "/fork") [201]
                (Just (object ["cwd" .= unsafeToFilePath cwd]))
            attach ui client lease fresh
            forM_ request.forkDirective (\prompt -> sendPrompt ui client prompt [])
            pure Nothing
        ReplClear -> control ui client "clear" Nothing >> pure Nothing
        ReplRewind -> do
            current <- readIORef ui.remoteCurrentSession
            history <- getAgentServerHistory client current.remoteId >>= requireRemote
            let prompts = filter (not . Text.null . (.historyTurnUserText) . (.historyItemTurn)) history.agentServerHistoryItems
            choice <- remoteChoice ui "Rewind before prompt" "" (map ((.historyTurnUserText) . (.historyItemTurn)) (reverse prompts))
            forM_ choice (\index -> control ui client "rewind" (Just (numberValue index)))
            pure Nothing
        ReplDelete -> do
            choice <- remoteChoice ui "Delete this session?" "" ["Cancel", "Delete"]
            if choice /= Just 1 then pure Nothing else do
                current <- readIORef ui.remoteCurrentSession
                void (remoteJson client "DELETE" (sessionPath current) [204] Nothing :: IO Value)
                pure (Just DevQuit)
        ReplAfk rawTarget -> do
            target <- either (throwIO . RemoteFailure) pure (parseAfkTarget rawTarget)
            current <- readIORef ui.remoteCurrentSession
            result <- case target of
                AfkLocal -> handoffLocal current.remoteId current.remoteCwd
                AfkRemote host path -> do
                    encoded <- command ui client "transfer" []
                    transfer <- either (throwIO . RemoteFailure) pure
                        (decodeLazy sessionTransferDecoder (Lazy.fromStrict (Encoding.encodeUtf8 encoded)))
                    home <- getHomeDirectory
                    directory <- either (throwIO . RemoteFailure) pure
                        (sessionDirForId (sessionsRoot home) transfer.transferMeta.metaId)
                    handoffRemote host path directory transfer
            message <- either (throwIO . RemoteFailure) pure result
            remoteNotice ui message
            pure (Just DevQuit)
    resume ui client lease target = do
        selected <- case target of
            Just identifier -> Just <$> remoteSession client identifier
            Nothing -> do
                sessions <- remoteSessions client
                choice <- remoteChoice ui "Resume session" "" [session.remoteTitle <> "  " <> session.remoteId | session <- sessions]
                pure ((sessions !!) <$> choice)
        forM_ selected (\session -> attach ui client lease session >> renderRemoteHistory ui client)
    attach ui client lease session = do
        previous <- readIORef ui.remoteCurrentSession
        _ <- attachLocalClientSession client lease session.remoteId >>= requireRemote
        when (previous.remoteId /= session.remoteId) $
            detachLocalClientSession client lease previous.remoteId >>= requireRemote >> pure ()
        writeIORef ui.remoteCurrentSession session
        forM_ ui.remoteFullscreen (\runtime -> emitUiEvent runtime UiConversationCleared)
    patch ui client body = do
        current <- readIORef ui.remoteCurrentSession
        refreshed <- remoteJson client "PATCH" (sessionPath current) [200] (Just body)
        writeIORef ui.remoteCurrentSession refreshed
    control :: RemoteUi -> AgentServerClient -> Text -> Maybe Value -> IO ()
    control ui client operation value = patch ui client (object ["control" .= object
        (["operation" .= (operation :: Text)] <> maybe [] (pure . ("value" .=)) value)])
    notice ui message = remoteNotice ui message >> pure Nothing
    goalPaused ui paused = modifyIORef' ui.remoteSettings (\settings -> settings
        {sessionGoal = fmap (\(objective, budget, _) -> (objective, budget, paused)) settings.sessionGoal})
    sideQuestion ui client lease question = do
        source <- readIORef ui.remoteCurrentSession
        turns <- listAgentServerTurns client source.remoteId >>= requireRemote
        if not (any ((`elem` [AgentServerTurnQueued, AgentServerTurnRunning, AgentServerTurnWaitingForInput]) . (.agentServerTurnStatus)) turns.agentServerTurns)
            then runRemoteTurn ui client SideQuestion question []
            else do
                history <- loadRemoteTranscript client source.remoteId
                let context = Text.takeEnd 120000 (renderTranscriptMarkdown (map snd history))
                    allocate = do
                        temporary <- createRemoteSession client options{optResume = Nothing, optModel = Just source.remoteModel} source.remoteCwd
                        _ <- attachLocalClientSession client lease temporary.remoteId >>= requireRemote
                        pure temporary
                    cleanup temporary = do
                        void (remoteJson client "DELETE" (sessionPath temporary) [204] Nothing :: IO Value)
                        void (detachLocalClientSession client lease temporary.remoteId >>= requireRemote)
                bracket allocate cleanup \temporary -> do
                    current <- newIORef temporary
                    cancel <- newIORef Nothing
                    answer <- runRemoteQuestion ui{remoteCurrentSession = current, remoteCancel = cancel}
                        client PrivateQuestion ("Read-only reference transcript:\n" <> context <> "\n\nSide question:\n" <> question)
                    remoteNotice ui answer
    command ui client name fields = runRemoteQuestion ui client HarnessCommand
        (Encoding.decodeUtf8 (Lazy.toStrict (encode (object (("command" .= (name :: Text)) : fields)))))
    quit ui = do
        session <- readIORef ui.remoteCurrentSession
        remoteNotice ui ("Resume with agent-cli --resume " <> session.remoteId)
        pure DevQuit

sessionPath :: RemoteSession -> Text
sessionPath session = "/v1/sessions/" <> segment session.remoteId

changeSelection :: AgentServerClient -> RemoteSession -> Text -> Text -> IO RemoteSession
changeSelection client session operation value =
    remoteJson client "PATCH" (sessionPath session) [200] (Just (object
        ["control" .= object ["operation" .= operation, "value" .= value]]))

textValue :: Text -> Value
textValue = Data.Aeson.String

numberValue :: Int -> Value
numberValue = Data.Aeson.toJSON

sendPrompt :: RemoteUi -> AgentServerClient -> Text -> [ImageAttachment] -> IO ()
sendPrompt ui client prompt images = do
    writeIORef ui.remoteLastPrompt (Just (prompt, images))
    forM_ ui.remoteFullscreen \runtime -> do
        beginFullscreenLiveHistory runtime
        commitFullscreenImagePreviews runtime prompt images
        emitUiEvent runtime (UiUserSubmitted prompt)
    runRemoteTurn ui client ConversationTurn prompt images

addImages :: RemoteUi -> [ImageAttachment] -> IO ()
addImages ui images = do
    current <- readIORef ui.remoteImages
    let (updated, _, countDropped, bytesDropped) = appendBoundedImageAttachments current images
    writeIORef ui.remoteImages updated
    previewRemoteImages ui updated
    when (countDropped > 0 || bytesDropped > 0) (remoteNotice ui "Some images exceeded the attachment limit.")

removeImage :: RemoteUi -> Int -> IO ()
removeImage ui index = modifyIORef' ui.remoteImages (\images -> take index images <> drop (index + 1) images)

attachments :: RemoteUi -> AttachmentAction -> IO ()
attachments ui = \case
    ReplClearAttachments -> writeIORef ui.remoteImages []
    ReplShowAttachments -> readIORef ui.remoteImages >>= remoteNotice ui . (<> " image(s) attached.") . Text.pack . show . length
    ReplRemoveAttachment index -> removeImage ui (index - 1)
    ReplPaste _ raw -> do
        loaded <- if Text.null raw then Just <$> (readClipboardImagesForPaste >>= either (throwIO . RemoteFailure) pure)
            else loadImagesFromPastedText raw
        maybe (remoteFailure ui "No image found.") (addImages ui) loaded

initialPrompt :: CliOptions -> OsPath -> IO (Maybe (Text, [ImageAttachment], [FileAttachment]))
initialPrompt options cwd = case options.optManagedTurnFile of
    Just path -> do
        request <- loadManagedTurnRequest path >>= either (throwIO . RemoteFailure) pure
        inputs <- managedTurnInputs cwd request
        pure (Just (request.managedTurnText, concatMap turnInputImages inputs, concatMap turnInputFiles inputs))
    Nothing -> case options.optPromptFile of
        Just path -> do
            request <- loadTextPrompt path
            pure (Just (request.managedTurnText, [], []))
        Nothing -> pure ((\prompt -> (prompt, [], [])) <$> options.optPrompt)
