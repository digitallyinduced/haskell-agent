-- | REPL submission and slash-command dispatch.
module Agent.CLI.Runtime.Repl.Commands
    ( handleReplLine
    , preparePromptSkillInputs
    , preparePromptSkillInputsWithPaste
    ) where

import Agent.CLI.AgentViewport
    ( AgentViewportEnv(viewportSelect, viewportEntries,
                       viewportSelected) )
import Agent.CLI.Approval ( setApprovalPolicy, toggleAlwaysApprove )
import Agent.CLI.Changelog (loadReleaseNotes)
import Agent.CLI.Clipboard ( loadImagesFromPastedText )
import Agent.CLI.Command
    ( AttachmentAction(ReplRemoveAttachment),
      formatSlashHelpWithCatalog,
      parseReplLineWithCatalog,
      ReplAction(..),
      ShellMode(ShellNone, ShellGhci, ShellBash, ShellBoth),
      SlashCatalog )
import Agent.CLI.Command.Instructions ( initInstruction )
import Agent.CLI.Compaction
    ( CompactOutcome(compactSummary, compactBeforeTokens,
                     compactAfterTokens, compactHistory) )
import Agent.CLI.Context ( formatContextReport )
import Agent.CLI.Desktop ( openDesktopConversation )
import Agent.CLI.ExternalProgram
    ( normalizeEditedText
    , resolveExternalProgram
    , runExternalProgramOnFile
    , withTemporaryTextFile
    )
import Agent.CLI.GitDiff
    ( GitDiffResult(..)
    , colorizeGitDiff
    , getGitDiff
    )
import Agent.CLI.Input
    ( formatPasteChip,
      readChoiceSelection,
      readReplHistory,
      submissionPromptText,
      truncateDisplayText,
      ReplLine(ReplText, ReplMeta, ReplEof, ReplQuitInterrupt, ReplCycleMode,
               ReplClipboardPaste, ReplClipboardPasteOrText, ReplChooseModel,
               ReplChooseEffort, ReplChooseAccount, ReplRemovePendingImage,
               ReplPasted) )
import Agent.CLI.GatewayClient ( loadGatewayCredential )
import Agent.CLI.Login
    ( runFullscreenLoginManager
    , runLoginManager
    )
import Agent.CLI.McpManager ( runMcpManager )
import Agent.CLI.Options
    ( ApprovalPolicy(..), gatewayRoutingChanged )
import Agent.CLI.Permission ( approvalPolicyOptions )
import Agent.CLI.Provider.Switch
    ( reloadAuth,
      reportProviderUnavailable,
      requestAutomaticProviderFallback )
import Agent.CLI.ProviderTransition
    ( PendingTurn
    , ProviderTransition
    , TurnResult(TurnProviderUnavailable)
    , resumePendingTurnIfPresent
    )
import Agent.CLI.Recap ( RecapKind(..), RecapRequest(..) )
import Agent.CLI.Render
    ( RenderConfig(..),
      clearThinking,
      putTextLn,
      renderEvent,
      renderPrintedText,
      resetRenderPrintedText )
import Agent.CLI.ReplMode ( replModeLabel )
import Agent.CLI.Review
    ( ReviewBranch(reviewBranchName)
    , ReviewCommit(reviewCommitHash, reviewCommitShortHash,
                   reviewCommitSubject)
    , ReviewTarget(..)
    , listReviewBranches
    , listReviewCommits
    , reviewPrompt
    )
import Agent.CLI.Runtime.Recap ( runSessionRecap )
import Agent.CLI.Runtime.Repl.Attachments
    ( ClipboardInput(..), handleAttachmentAction, handleClipboardInput )
import Agent.CLI.Runtime.Repl.Context
    ( ReplHandlerContext(..)
    , displayReplError
    , displayReplInfo
    , requestReplChoice
    , requestReplText
    , withReplSuspended
    )
import Agent.CLI.Runtime.Repl.MetaConsole ( handleMetaConsoleRequest )
import Agent.CLI.Runtime.Repl.Selection
    ( SelectionInput(..), handleSelectionAction, handleSelectionInput )
import Agent.CLI.Runtime.Repl.Session ( handleSessionAction )
import Agent.CLI.Runtime.Repl.Transcript
    ( TranscriptAction(..), handleTranscriptAction )
import Agent.CLI.Runtime.Repl.Workflow ( handleWorkflowAction )
import Agent.CLI.Runtime.Types
    ( RunResult(RunEnableCodeMode, RunFreshSession, RunRestart, RunUpdateAndRestart,
                RunSwitchProvider, RunReload, RunQuit) )
import Agent.CLI.Session
    ( TranscriptEffect(TranscriptReplace),
      appendTurnWithMetaUpdateIndexed,
      ensureSession,
      Persistence(..),
      PersistenceState(PersistenceActive),
      SessionHandle(sessionMeta, sessionDir),
      SessionMeta(metaId, metaLastResponseId),
      SessionTurn(turnUsage, SessionTurn, turnAt, turnUserText,
                  turnAssistantText, turnError, turnResponseId, turnEffect,
                  turnItems, turnDisplayItems, turnProviderTelemetry) )
import Agent.CLI.Session.Attachments ( queueAttachedImages )
import Agent.CLI.Session.Choices
    ( accountUsageText, showAccountUsage )
import Agent.CLI.Session.History
    ( modifyLiveAttachments, readLiveAttachments, readLiveTranscript )
import Agent.CLI.Session.Interaction ( runBtwQuestion )
import Agent.CLI.Session.Selection
    ( currentSessionId, pickAgentChoice )
import Agent.CLI.SessionEnv ( SessionEnv(..) )
import Agent.CLI.Skills
    ( formatSkillsListing
    , resolvePromptSkillMentions
    )
import Agent.CLI.Status ( applyReplMode, cycleReplInteraction )
import Agent.CLI.Style
    ( glyphOk, glyphSession, roleError, roleMuted, roleSuccess )
import Agent.CLI.TUI.App
    ( beginFullscreenLiveHistory,
      commitFullscreenImagePreviews,
      commitFullscreenHistoryTurn,
      emitUiEvent,
      requestFullscreenDocument,
      requestFullscreenFilterChoice,
      queuedFullscreenInputDisplays,
      setFullscreenImagePreviews )
import Agent.CLI.TUI.SessionHistory ( sessionHistoryTurn )
import Agent.CLI.TUI.Types
    ( FullscreenRuntime(runtimeInput)
    , HistoryCommit(..)
    )
import Agent.CLI.Terminal
    ( formatTerminalCapabilities
    , resolveColor
    )
import Agent.CLI.Turn ( runOneTurn )
import Agent.Loop
    ( LoopEvent(ActivityUpdated)
    , TurnAttachment(ImageAttachmentItem)
    , TurnInput(UserMessage)
    , userMessageWithAttachments
    )
import Agent.OpenAI.Compaction ( compactSessionUserText )
import Agent.OsPath ( toText, unsafeToFilePath )
import Agent.Provider ( Provider(ClaudeCodeProvider) )
import Agent.Responses.Types ( ResponseCreateParams(model) )
import Agent.Skills
    ( SkillInvocation(invocationSkill),
      formatSkillActivation,
      resolveSkillInvocation,
      Skill(skillName) )
import Agent.TUI.Model
    ( infoNotice,
      progressNotice,
      UiEvent(UiUserSubmitted, UiRecapStarted, UiSetNotice, UiErrorMessage,
              UiSystemMessage) )
import Agent.Tools.PlanMode
    ( PlanModeEnv(planStateRef, planSessionDir),
      activatePlanMode,
      planFilePath,
      readPlanMarkdown,
      PlanModeState(PlanPending) )
import Control.Exception ( AsyncException(UserInterrupt) )
import Control.Exception.Safe
    ( displayException, finally, throwIO, tryAny, tryIO )
import Control.Monad ( forM_, when )
import Data.Foldable ( toList )
import Data.IORef ( newIORef, readIORef, writeIORef )
import Data.List ( findIndex )
import Data.Maybe ( fromMaybe, isNothing )
import Data.Text ( Text )
import Data.Time.Clock ( getCurrentTime )
import System.IO ( stdout, hFlush, stderr )
import System.IO.Error ( isDoesNotExistError )
import System.OsPath ( OsPath, unsafeEncodeUtf, (</>) )
import System.Posix.Files ( getSymbolicLinkStatus )
import qualified Agent.MCP as MCP
import qualified Data.Text as Text
    ( intercalate, null, pack, replace, strip, toCaseFold )
import qualified Data.Text.IO as Text ( putStrLn, hPutStrLn, readFile )

handleReplLine
    :: SessionEnv
    -> (Text -> IO RunResult)
    -> (Bool -> TurnResult -> IO RunResult)
    -> (PendingTurn -> IO RunResult)
    -> SlashCatalog
    -> [SkillInvocation]
    -> Bool
    -> PlanModeState
    -> ApprovalPolicy
    -> ReplLine
    -> IO RunResult
handleReplLine
        env@SessionEnv
            { sessionCompact = compactRunner
            , sessionRender = render
            , sessionConversation = conversationRef
            , sessionContextOccupancy = contextOccupancyRef
            , sessionContextWindow = currentContextWindow
            , sessionProvider = provider
            , sessionPolicy = policyRef
            , sessionPersist = persist
            , sessionPlanMode = planMode
            , sessionProjectRoot = projectRoot
            , sessionCwd = cwd
            , sessionTokenProvider = tokenProvider
            , sessionOpenAiPool = openAiPool
            , sessionGatewayModels = gatewayModelsRef
            , sessionSkills = skillsRef
            , sessionSkillInvocations = skillInvocationsRef
            , sessionRefreshSkills = refreshSkills
            , sessionDraft = draftRef
            , sessionPreviewId = previewIdRef
            , sessionTerminal = terminal
            , sessionFullscreen = fullscreen
            , sessionAgentViewport = agentViewport
            }
        continueWith
        finishTurn
        retryPendingTurn
        slashCatalog
        skillInvocations
        stdoutColor
        planState
        policy = \case
    ReplEof -> do
        when (isNothing fullscreen) $
            putStrLn ""
        pure RunQuit
    ReplQuitInterrupt ->
        -- Let the orchestration boundary normalize confirmed Ctrl-C to the
        -- same graceful quit path used by :q and Ctrl-D.
        throwIO UserInterrupt
    ReplCycleMode keptDraft
        | provider == ClaudeCodeProvider -> do
            let message =
                    "Claude Code permissions are fixed when the provider starts; restart with --yolo or --no-yolo to change them."
            color <- resolveColor stderr
            displayInfo message $
                putTextLn stderr (roleMuted color message)
            continueWith keptDraft
        | otherwise -> do
            let next = cycleReplInteraction planState policy
            applyReplMode planMode policyRef projectRoot next
            case fullscreen of
                Just runtime ->
                    emitUiEvent runtime $
                        UiSetNotice $
                            Just $
                                infoNotice
                                    ("Switched to "
                                        <> replModeLabel next
                                        <> " mode.")
                Nothing -> do
                    -- Minimal editor advanced a line; replace its old chrome.
                    putStr "\ESC[2A\r\ESC[J"
                    hFlush stdout
            continueWith keptDraft
    ReplClipboardPaste draft images ->
        handleClipboardInput env continueWith stdoutColor
            (ClipboardPaste draft images)
    ReplClipboardPasteOrText draft pasted pastedDraft ->
        handleClipboardInput env continueWith stdoutColor
            (ClipboardPasteOrText draft pasted pastedDraft)
    ReplChooseModel keptDraft ->
        handleSelectionInput
            env
            (continueWith keptDraft)
            retryPendingTurn
            (ChooseModel keptDraft)
    ReplChooseEffort keptDraft ->
        handleSelectionInput
            env
            (continueWith keptDraft)
            retryPendingTurn
            (ChooseEffort keptDraft)
    ReplChooseAccount keptDraft ->
        handleSelectionInput
            env
            (continueWith keptDraft)
            retryPendingTurn
            (ChooseAccount keptDraft)
    ReplMeta request ->
        runMetaConsoleRequest request
    ReplRemovePendingImage keptDraft index ->
        handleAttachmentAction
            env
            finishTurn
            (continueWith keptDraft)
            (ReplRemoveAttachment index)
    ReplPasted pasted ->
        submitLine slashCatalog skillInvocations
            continue stdoutColor True pasted
    ReplText line ->
        submitLine slashCatalog skillInvocations
            continue stdoutColor False line
  where
    handlerContext =
        ReplHandlerContext
            { handlerSessionEnv = env
            , handlerContinueWith = continueWith
            , handlerStdoutColor = stdoutColor
            }
    submitLine
            slashCatalog skillInvocations
            continue color pasted line = do
        attachmentCount <- length <$> readLiveAttachments conversationRef
        case submissionPromptText attachmentCount line of
            Nothing -> continue
            Just promptLine -> do
                let stripped = Text.strip promptLine
                when pasted do
                    let chip = formatPasteChip stripped
                    when (chip /= stripped && isNothing fullscreen) do
                        Text.putStrLn (roleMuted color chip)
                case parseReplLineWithCatalog slashCatalog promptLine of
                    ReplQuit -> pure RunQuit
                    ReplReload -> requestReload fullscreen persist
                    ReplUpdateAndRestart ->
                        requestUpdateAndRestart fullscreen persist
                    ReplMetaConsole request ->
                        runMetaConsoleRequest request
                    ReplPrompt text ->
                        submitPrompt pasted continue color text
                    ReplExpandedPrompt original expanded ->
                        submitExpandedTurnWithPaste
                            pasted continue color original expanded
                    ReplInit ->
                        initializeProjectGuide continue color line
                    ReplReview review ->
                        submitReview continue color line review
                    ReplDiff ->
                        showWorkingTreeDiff continue color
                    ReplExport maybePath ->
                        handleTranscriptAction handlerContext
                            (ExportTranscript maybePath)
                    ReplPermissions ->
                        choosePermissions continue color
                    ReplInvokeSkill invocationName arguments ->
                        submitSkillInvocation
                            skillInvocations
                            continue
                            color
                            line
                            invocationName
                            arguments
                    ReplSkills reloadFirst ->
                        showSkills continue color reloadFirst
                    ReplShowShell ->
                        showShellMode continue color
                    ReplSetShell mode ->
                        setShellMode continue color mode
                    ReplToggleComputerUse -> do
                        enabled <- env.sessionComputerUseEnabled
                        message <-
                            env.sessionSetComputerUseEnabled (not enabled)
                        displayInfo message $
                            Text.putStrLn
                                (roleMuted color (glyphOk <> message))
                        continue
                    ReplSetComputerUse enabled -> do
                        message <- env.sessionSetComputerUseEnabled enabled
                        displayInfo message $
                            Text.putStrLn
                                (roleMuted color (glyphOk <> message))
                        continue
                    ReplAttachment action ->
                        handleAttachmentAction env finishTurn continue action
                    ReplShowAgentLimit ->
                        showAgentLimit continue
                    ReplSetAgentLimit limit ->
                        setAgentLimit continue limit
                    ReplAgents ->
                        chooseAgent continue
                    ReplMcp ->
                        manageMcpServers continue
                    ReplMcpPrompt server name arguments ->
                        submitMcpPrompt
                            continue color server name arguments
                    ReplWorkflow action ->
                        handleWorkflowAction env submitExpandedTurn color continue action
                    ReplCopy request ->
                        handleTranscriptAction handlerContext
                            (CopyResponse request)
                    ReplCopyCode index ->
                        handleTranscriptAction handlerContext
                            (CopyCodeBlock index)
                    ReplCopyDiff ->
                        handleTranscriptAction handlerContext CopyDiffBlock
                    ReplCopyPath ->
                        handleTranscriptAction handlerContext CopyWorktreePath
                    ReplCopySession ->
                        handleTranscriptAction handlerContext CopySessionId
                    ReplDesktop ->
                        openDesktopSession continue
                    ReplShowTerminal ->
                        showTerminalCapabilities continue color
                    ReplChangelog ->
                        showChangelog continue
                    ReplSelection action ->
                        handleSelectionAction env continue action
                    ReplEnableCodeMode ->
                        requestCodeModeRestart fullscreen persist
                    ReplToggleAlwaysApprove ->
                        toggleApprovalMode continue
                    ReplCompact focus ->
                        compactContext continue focus
                    ReplViewPlan ->
                        showSavedPlan continue
                    ReplPlan maybeDescription ->
                        enterPlanCommand continue maybeDescription
                    ReplQueue ->
                        showQueuedPrompts continue
                    ReplContext ->
                        showContextReport continue
                    ReplHistory ->
                        showPromptHistory continue color
                    ReplTranscript ->
                        handleTranscriptAction handlerContext ShowTranscript
                    ReplFind maybeQuery ->
                        handleTranscriptAction handlerContext
                            (FindTranscript maybeQuery)
                    ReplEditPrompt ->
                        editCurrentPrompt continue color
                    ReplBtw question -> do
                        runBtwQuestion True env question
                        continue
                    ReplRecap ->
                        requestSessionRecap continue
                    ReplRetry ->
                        resumePendingTurnIfPresent
                            env.sessionLastFailedTurn
                            retryPendingTurn
                            (do
                                displayInfo
                                    "No failed turn is available to retry."
                                    (pure ())
                                continue)
                    ReplSession action ->
                        handleSessionAction env slashCatalog continue action
                    ReplLogin ->
                        manageLogin continue
                    ReplUsage ->
                        showUsage continue
                    ReplReloadAuth ->
                        reloadProviderAuth continue
                    ReplHelp maybeName ->
                        showCommandHelp slashCatalog continue maybeName
                    ReplCommandError err ->
                        showCommandError continue err
    showShellMode next color = do
        mode <- env.sessionShellMode
        let message = "shell tools: " <> case mode of
                ShellGhci -> "ghci"
                ShellBash -> "bash"
                ShellBoth -> "ghci + bash"
                ShellNone -> "none"
        displayInfo message $
            Text.putStrLn
                (roleMuted color (glyphSession <> message))
        next
    setShellMode next color mode = do
        message <- env.sessionSetShellMode mode
        displayInfo message $
            Text.putStrLn (roleMuted color (glyphOk <> message))
        next
    showAgentLimit next = do
        limit <- env.sessionConcurrentLimit
        let message =
                "concurrent agent limit: " <> Text.pack (show limit)
        color <- resolveColor stdout
        displayInfo message $
            Text.putStrLn
                (roleMuted color (glyphSession <> message))
        next
    setAgentLimit next limit = do
        message <- env.sessionSetConcurrentLimit limit
        color <- resolveColor stdout
        displayInfo message $
            Text.putStrLn (roleMuted color (glyphOk <> message))
        next
    chooseAgent next =
        case agentViewport of
            Nothing -> next
            Just viewport -> do
                entries <- viewport.viewportEntries
                selected <- readIORef viewport.viewportSelected
                color <- resolveColor stderr
                pickAgentChoice
                    fullscreen color selected entries >>= \case
                        Nothing -> pure ()
                        Just target -> viewport.viewportSelect target
                next
    manageMcpServers next = do
        color <- resolveColor stderr
        restart <-
            legacy $
                runMcpManager
                    color
                    env.sessionHome
                    env.sessionMcpRegistrations
                    env.sessionMcpWarnings
        if restart
            then requestMcpRestart fullscreen persist
            else next
    submitMcpPrompt next color server name arguments = do
        outcome <- case env.sessionMcpFleet of
            Nothing -> pure (Left "no MCP servers are configured")
            Just fleet ->
                MCP.mcpFleetGetPrompt fleet server name arguments
        case outcome of
            Left err -> do
                displayError err $
                    Text.hPutStrLn stderr (roleError color err)
                next
            Right result ->
                submitExpandedTurn
                    next
                    color
                    ("/mcp prompt " <> server <> " " <> name)
                    (MCP.renderMcpPromptResult result)
    openDesktopSession next = do
        currentSessionId persist >>= \case
            Nothing -> do
                let err = "/desktop requires a persisted conversation"
                color <- resolveColor stderr
                displayError err $
                    Text.hPutStrLn stderr (roleError color err)
            Just sessionId ->
                openDesktopConversation sessionId >>= \case
                    Left err -> do
                        color <- resolveColor stderr
                        displayError err $
                            Text.hPutStrLn stderr (roleError color err)
                    Right () -> do
                        let message =
                                "opened conversation in Haskell Agent"
                        color <- resolveColor stderr
                        displayInfo message $
                            Text.hPutStrLn stderr
                                (roleSuccess color (glyphOk <> message))
        next
    showTerminalCapabilities next color = do
        let message = formatTerminalCapabilities terminal
        displayInfo message $
            Text.putStrLn (roleMuted color message)
        next
    showChangelog next = do
        releaseNotes <- loadReleaseNotes
        case fullscreen of
            Just runtime ->
                requestFullscreenDocument
                    runtime
                    "Release Notes"
                    releaseNotes
            Nothing ->
                displayInfo releaseNotes (Text.putStrLn releaseNotes)
        next
    toggleApprovalMode next
        | provider == ClaudeCodeProvider = do
            let message =
                    "Claude Code permissions are fixed for this provider session; restart with --yolo or --no-yolo."
            color <- resolveColor stderr
            displayInfo message $
                putTextLn stderr (roleMuted color message)
            next
        | otherwise = do
            message <- toggleAlwaysApprove policyRef projectRoot
            color <- resolveColor stderr
            displayInfo message $
                putTextLn stderr (roleMuted color message)
            next
    compactContext next focus = do
        color <- resolveColor stderr
        result <-
            withReplActivity "Compacting context…" $
                compactRunner focus
        case result of
            Left err -> do
                displayError err $
                    Text.hPutStrLn stderr (roleError color err)
                next
            Right outcome -> do
                persistCompactOutcome focus outcome
                let statsMessage =
                        "compacted "
                            <> Text.pack
                                (show outcome.compactBeforeTokens)
                            <> " → "
                            <> Text.pack
                                (show outcome.compactAfterTokens)
                            <> " tokens ("
                            <> Text.pack
                                (show (length outcome.compactHistory))
                            <> " items)"
                displayInfo statsMessage $
                    Text.hPutStrLn stderr
                        (roleMuted color (glyphSession <> statsMessage))
                next
    persistCompactOutcome focus outcome =
        case persist of
            PersistenceDisabled ->
                fullscreenEvent
                    (UiSystemMessage outcome.compactSummary)
            PersistenceEnabled slotRef -> do
                forM_ fullscreen beginFullscreenLiveHistory
                fullscreenEvent
                    (UiSystemMessage outcome.compactSummary)
                now <- getCurrentTime
                handle <- ensureSession slotRef
                let turn = SessionTurn
                        { turnAt = now
                        , turnUserText = compactSessionUserText focus
                        , turnAssistantText = Just outcome.compactSummary
                        , turnError = Nothing
                        , turnResponseId = Nothing
                        , turnEffect = TranscriptReplace
                        , turnItems = outcome.compactHistory
                        , turnDisplayItems = []
                        -- Compaction response usage is recorded immediately
                        -- by compactRunner, including response-level failures.
                        , turnUsage = Nothing
                        , turnProviderTelemetry = []
                        }
                (handle', turnIndex) <-
                    appendTurnWithMetaUpdateIndexed handle turn
                        \meta -> meta { metaLastResponseId = Nothing }
                writeIORef slotRef (PersistenceActive handle')
                forM_ fullscreen \runtime ->
                    commitFullscreenHistoryTurn
                        runtime
                        (sessionHistoryTurn turnIndex turn)
                        HistoryCommitAppend
    showSavedPlan next = do
        markdown <- readPlanMarkdown planMode
        if Text.null (Text.strip markdown)
            then do
                let message =
                        "No saved plan is available for this session."
                displayInfo message (Text.putStrLn message)
            else
                displayInfo markdown (Text.putStrLn markdown)
        next
    enterPlanCommand next maybeDescription
        | provider == ClaudeCodeProvider = do
            let message =
                    "Outer plan mode is unavailable for Claude Code because its tools run inside the Claude CLI."
            color <- resolveColor stderr
            displayInfo message $
                putTextLn stderr (roleMuted color message)
            next
        | otherwise =
            enterPlanFromSlash env maybeDescription >>= \case
                Just providerSwitch ->
                    pure (RunSwitchProvider providerSwitch)
                Nothing -> next
    showQueuedPrompts next = do
        prompts <- case fullscreen of
            Nothing -> pure []
            Just runtime ->
                toList
                    <$> queuedFullscreenInputDisplays runtime.runtimeInput
        let message = formatQueuedPrompts prompts
        displayInfo message (Text.putStrLn message)
        next
    showContextReport next = do
        currentParams <- readIORef env.sessionParams
        history <- readLiveTranscript conversationRef
        occupancy <- readIORef contextOccupancyRef
        contextWindow <- currentContextWindow
        activeTools <- env.sessionActiveToolNames
        let model = maybe "<unknown>" id currentParams.model
            message =
                formatContextReport
                    model
                    contextWindow
                    occupancy
                    currentParams
                    history
                    activeTools
        displayInfo message (Text.putStrLn message)
        next
    showPromptHistory next color = do
        prompts <-
            filter
                ((/= "/history") . Text.toCaseFold . Text.strip)
                <$> readReplHistory
        case prompts of
            [] -> do
                let message = "No prompt history is available."
                displayInfo message (Text.putStrLn message)
                next
            _ -> do
                selected <- case fullscreen of
                    Just runtime ->
                        requestFullscreenFilterChoice
                            runtime
                            "Prompt history"
                            0
                            [ (historyLabel prompt, "")
                            | prompt <- prompts
                            ]
                            >>= pure . (>>= (`listAt` prompts))
                    Nothing ->
                        readChoiceSelection
                            (\active prompt ->
                                (if active
                                    then roleSuccess color
                                    else roleMuted color)
                                    (historyLabel prompt))
                            prompts
                maybe next continueWith selected
    editCurrentPrompt next color =
        legacy editPrompt >>= \case
            Left err -> do
                displayError err $
                    Text.hPutStrLn stderr (roleError color err)
                next
            Right edited -> continueWith edited
    requestSessionRecap next =
        case fullscreen of
            Just runtime -> do
                emitUiEvent runtime UiRecapStarted
                env.sessionQueueRecap (RecapSession RecapManual)
                next
            Nothing -> do
                runSessionRecap True env RecapManual
                next
    manageLogin next = do
        gatewayBefore <- loadGatewayCredential
        case fullscreen of
            Just runtime -> runFullscreenLoginManager runtime
            Nothing -> do
                color <- resolveColor stderr
                runLoginManager color
        gatewayAfter <- loadGatewayCredential
        if gatewayRoutingChanged gatewayBefore gatewayAfter
            then requestGatewayRestart fullscreen cwd
            else next
    showUsage next = do
        readIORef gatewayModelsRef >>= \case
            Just _ ->
                displayInfo
                    "usage: managed by the organization gateway"
                    (Text.putStrLn
                        (roleMuted stdoutColor
                            "usage: managed by the organization gateway"))
            Nothing ->
                case fullscreen of
                    Nothing ->
                        showAccountUsage provider tokenProvider openAiPool
                    Just runtime ->
                        accountUsageText
                            False provider tokenProvider openAiPool
                            >>= emitUiEvent runtime . UiSystemMessage
        next
    reloadProviderAuth next = do
        reloadResult <- reloadAuth provider tokenProvider
        color <- resolveColor stderr
        case reloadResult of
            Left err ->
                displayError err $
                    putTextLn stderr (roleError color err)
            Right message ->
                displayInfo message $
                    putTextLn stderr (roleMuted color message)
        next
    showCommandHelp catalog next maybeName = do
        color <- resolveColor stdout
        displayInfo
            (formatSlashHelpWithCatalog False catalog maybeName) $
            Text.putStrLn
                (formatSlashHelpWithCatalog color catalog maybeName)
        next
    showCommandError next err = do
        color <- resolveColor stderr
        displayError err $
            Text.hPutStrLn stderr (roleError color err)
        next
    submitPrompt pasted next color text = do
        -- Native Cmd+V of a Finder image often pastes a path rather than
        -- bitmap bytes. Treat a prompt that is only image path(s) as an attach
        -- plus in-terminal preview, matching Grok Build's paste chip.
        pastedImages <- loadImagesFromPastedText text
        case pastedImages of
            Just images@(_:_) -> do
                message <- queueAttachedImages
                    conversationRef
                    previewIdRef
                    color
                    (isNothing fullscreen)
                    images
                syncFullscreenImagePreviews
                displayInfo message $
                    Text.putStrLn
                        (roleMuted color (glyphOk <> message))
                next
            _ -> do
                pendingImages <-
                    modifyLiveAttachments conversationRef \imgs -> ([], imgs)
                forM_ fullscreen \runtime ->
                    commitFullscreenImagePreviews runtime pendingImages
                resetRenderPrintedText render
                let turnInputs =
                        [ userMessageWithAttachments
                            text
                            (map ImageAttachmentItem pendingImages)
                        ]
                preparePromptSkillInputsWithPaste
                    env pasted text turnInputs >>= \case
                        Left err -> do
                            displayError err $
                                Text.hPutStrLn stderr (roleError color err)
                            next
                        Right skillInputs -> do
                            fullscreenEvent (UiUserSubmitted text)
                            result <- runOneTurn env text skillInputs
                            finishTurn False result
    initializeProjectGuide next color line = do
        let guidePath = cwd </> unsafeEncodeUtf "AGENTS.md"
        tryIO
            (getSymbolicLinkStatus
                (unsafeToFilePath guidePath)) >>= \case
            Left err
                | isDoesNotExistError err ->
                    submitExpandedTurn next color line initInstruction
                | otherwise -> do
                    let message =
                            "could not check AGENTS.md: "
                                <> Text.pack (displayException err)
                    displayError message $
                        Text.hPutStrLn stderr (roleError color message)
                    next
            Right _ -> do
                let message =
                        "AGENTS.md already exists; left it unchanged."
                displayInfo message $
                    Text.putStrLn
                        (roleMuted color (glyphSession <> message))
                next
    submitReview next color line = \case
        Just instructions ->
            submitExpandedTurn
                next
                color
                line
                (reviewPrompt (ReviewCustom instructions))
        Nothing ->
            chooseReviewTarget >>= \case
                Left err -> do
                    displayError err $
                        Text.hPutStrLn stderr (roleError color err)
                    next
                Right Nothing -> next
                Right (Just target) ->
                    submitExpandedTurn
                        next
                        color
                        line
                        (reviewPrompt target)
    showWorkingTreeDiff next color = do
        result <-
            withReplActivity "Loading Git diff…" $
                getGitDiff cwd
        case result of
            Left err -> do
                displayError err $
                    Text.hPutStrLn stderr (roleError color err)
                next
            Right GitDiffNotRepository -> do
                let message = "not a Git repository"
                displayError message $
                    Text.hPutStrLn stderr (roleError color message)
                next
            Right (GitDiffOutput diff)
                | Text.null (Text.strip diff) -> do
                    let message = "No working-tree changes."
                    displayInfo message $
                        Text.putStrLn
                            (roleMuted color (glyphSession <> message))
                    next
                | otherwise -> do
                    displayInfo diff $
                        Text.putStrLn (colorizeGitDiff color diff)
                    next
    choosePermissions next color
        | provider == ClaudeCodeProvider = do
            let message =
                    "Claude Code permissions are fixed for this provider session; restart with --yolo or --no-yolo."
            displayInfo message $
                Text.hPutStrLn stderr (roleMuted color message)
            next
        | otherwise = do
            current <- readIORef policyRef
            requestChoice
                "Permissions"
                "Choose how mutating tools are handled."
                (approvalPolicyIndex current)
                approvalPolicyRows >>= \case
                    Nothing -> next
                    Just index -> do
                        message <-
                            setApprovalPolicy
                                policyRef
                                projectRoot
                                (approvalPolicyAt index)
                        displayInfo message $
                            Text.hPutStrLn stderr
                                (roleMuted color (glyphOk <> message))
                        next
    submitSkillInvocation
            invocations next color line invocationName arguments =
        case resolveSkillInvocation invocations invocationName of
            Left err -> do
                displayError err $
                    Text.hPutStrLn stderr (roleError color err)
                next
            Right invocation -> do
                pendingImages <-
                    modifyLiveAttachments conversationRef \imgs -> ([], imgs)
                forM_ fullscreen \runtime ->
                    commitFullscreenImagePreviews runtime pendingImages
                let userText =
                        if Text.null arguments
                            then "Use the "
                                <> invocation.invocationSkill.skillName
                                <> " skill."
                            else arguments
                    userInput =
                        userMessageWithAttachments
                            userText
                            (map ImageAttachmentItem pendingImages)
                    skillInputs =
                        [ UserMessage
                            (formatSkillActivation invocation arguments)
                        , userInput
                        ]
                resetRenderPrintedText render
                fullscreenEvent (UiUserSubmitted line)
                result <- runOneTurn env line skillInputs
                finishTurn False result
    showSkills next color reloadFirst = do
        when reloadFirst (refreshSkills True)
        current <- readIORef skillsRef
        invocations <- readIORef skillInvocationsRef
        let listing = formatSkillsListing color current invocations
        displayInfo (formatSkillsListing False current invocations) $
            Text.putStrLn listing
        next
    submitExpandedTurn = submitExpandedTurnWithPaste False
    submitExpandedTurnWithPaste pasted next color original expanded = do
        pendingImages <-
            modifyLiveAttachments conversationRef \imgs -> ([], imgs)
        forM_ fullscreen \runtime ->
            commitFullscreenImagePreviews runtime pendingImages
        let turnInputs =
                [ userMessageWithAttachments
                    expanded
                    (map ImageAttachmentItem pendingImages)
                ]
        preparePromptSkillInputsWithPaste env pasted original turnInputs >>= \case
            Left err -> do
                displayError err $
                    Text.hPutStrLn stderr (roleError color err)
                next
            Right skillInputs -> do
                resetRenderPrintedText render
                fullscreenEvent (UiUserSubmitted original)
                result <- runOneTurn env original skillInputs
                finishTurn False result
    runMetaConsoleRequest =
        handleMetaConsoleRequest
            handlerContext
            slashCatalog
            (submitLine
                slashCatalog
                skillInvocations
                (pure RunQuit)
                stdoutColor
                False)

    continue = continueWith ""
    chooseReviewTarget =
        requestChoice
            "Review"
            "Select what the agent should review."
            0
            [ ( "Review against a base branch"
              , "Compare the current branch with a local base branch"
              )
            , ( "Review uncommitted changes"
              , "Inspect staged, unstaged, and untracked changes"
              )
            , ( "Review a commit"
              , "Inspect one recent commit"
              )
            , ( "Custom review instructions"
              , "Describe the review scope yourself"
              )
            ] >>= \case
                Nothing -> pure (Right Nothing)
                Just 0 -> do
                    branches <-
                        withReplActivity "Loading local branches…" $
                            listReviewBranches cwd
                    case branches of
                        Left err -> pure (Left err)
                        Right [] ->
                            pure
                                (Left
                                    "no other local branch is available as a review base")
                        Right available ->
                            requestChoice
                                "Review against a base branch"
                                "Choose the local branch to compare with HEAD."
                                0
                                [ (branch.reviewBranchName, "")
                                | branch <- available
                                ] >>= \case
                                    Nothing -> pure (Right Nothing)
                                    Just index ->
                                        pure $
                                            Right $
                                                ReviewBaseBranch
                                                    . (.reviewBranchName)
                                                    <$> indexMaybe index available
                Just 1 -> pure (Right (Just ReviewUncommitted))
                Just 2 -> do
                    commits <-
                        withReplActivity "Loading recent commits…" $
                            listReviewCommits cwd 50
                    case commits of
                        Left err -> pure (Left err)
                        Right [] ->
                            pure
                                (Left
                                    "no commits are available to review")
                        Right available ->
                            requestChoice
                                "Review a commit"
                                "Choose a recent commit."
                                0
                                [ ( commit.reviewCommitShortHash
                                        <> " "
                                        <> commit.reviewCommitSubject
                                  , commit.reviewCommitHash
                                  )
                                | commit <- available
                                ] >>= \case
                                    Nothing -> pure (Right Nothing)
                                    Just index ->
                                        pure $
                                            Right $
                                                ReviewCommitTarget
                                                    . (.reviewCommitHash)
                                                    <$> indexMaybe index available
                Just _ ->
                    requestText
                        "Custom review instructions"
                        "Describe what the agent should review."
                        "" >>= \case
                            Nothing -> pure (Right Nothing)
                            Just instructions ->
                                pure
                                    (Right
                                        (ReviewCustom
                                            <$> nonBlank instructions))
    requestChoice = requestReplChoice handlerContext
    requestText = requestReplText handlerContext
    approvalPolicyRows =
        [ (label, detail)
        | (_, label, detail) <- approvalPolicyOptions
        ]
    approvalPolicyIndex policy =
        fromMaybe 0 $
            findIndex
                (\(candidate, _, _) -> candidate == policy)
                approvalPolicyOptions
    approvalPolicyAt index =
        case indexMaybe index approvalPolicyOptions of
            Just (policy, _, _) -> policy
            Nothing -> PromptMutating
    indexMaybe index values
        | index < 0 = Nothing
        | otherwise =
            case drop index values of
                value : _ -> Just value
                [] -> Nothing
    nonBlank value
        | Text.null stripped = Nothing
        | otherwise = Just stripped
      where
        stripped = Text.strip value
    legacy = withReplSuspended handlerContext
    fullscreenEvent event = case fullscreen of
        Nothing -> pure ()
        Just runtime -> emitUiEvent runtime event
    syncFullscreenImagePreviews =
        forM_ fullscreen \runtime ->
            readLiveAttachments conversationRef
                >>= setFullscreenImagePreviews runtime
    displayInfo = displayReplInfo handlerContext
    displayError = displayReplError handlerContext
    withReplActivity message action = do
        case fullscreen of
            Nothing ->
                renderEvent render (ActivityUpdated message)
            Just runtime ->
                emitUiEvent runtime
                    (UiSetNotice (Just (progressNotice message)))
        action `finally`
            case fullscreen of
                Nothing -> clearThinking render
                Just runtime -> emitUiEvent runtime (UiSetNotice Nothing)
    editPrompt = do
        initialDraft <- readIORef draftRef
        outcome <- tryAny do
            resolveExternalProgram
                [("VISUAL", "$VISUAL"), ("EDITOR", "$EDITOR")]
                "vi" >>= \case
                    Left err -> pure (Left err)
                    Right program ->
                        withTemporaryTextFile
                            "agent-prompt-"
                            initialDraft
                            \path ->
                                runExternalProgramOnFile program path >>= \case
                                    Left err -> pure (Left err)
                                    Right () ->
                                        Right . normalizeEditedText
                                            <$> Text.readFile path
        pure $ case outcome of
            Left exception ->
                Left
                    ( "could not edit prompt: "
                        <> Text.pack (show exception)
                    )
            Right result -> result

    historyLabel =
        truncateDisplayText 120 . Text.replace "\n" " ↵ "

    listAt index values
        | index < 0 = Nothing
        | otherwise = case drop index values of
            value : _ -> Just value
            [] -> Nothing

formatQueuedPrompts :: [Text] -> Text
formatQueuedPrompts [] = "No prompts are queued."
formatQueuedPrompts prompts =
    "Queued prompts (" <> Text.pack (show (length prompts)) <> "):\n"
        <> Text.intercalate
            "\n"
            (zipWith formatPrompt [1 :: Int ..] prompts)
  where
    formatPrompt index prompt =
        Text.pack (show index)
            <> ". "
            <> Text.replace "\n" "\n   " prompt

requestReload
    :: Maybe FullscreenRuntime
    -> Persistence
    -> IO RunResult
requestReload fullscreen persist = do
    color <- resolveColor stderr
    let reportInfo message =
            case fullscreen of
                Nothing ->
                    putTextLn stderr
                        (roleMuted color (glyphSession <> message))
                Just runtime ->
                    emitUiEvent runtime (UiSystemMessage message)
        reportError message =
            case fullscreen of
                Nothing ->
                    putTextLn stderr (roleError color message)
                Just runtime ->
                    emitUiEvent runtime (UiErrorMessage message)
    case persist of
        PersistenceDisabled -> do
            reportError ":reload needs a persisted REPL session"
            pure RunQuit
        PersistenceEnabled slotRef -> do
            handle <- ensureSession slotRef
            reportInfo ("reloading; session " <> handle.sessionMeta.metaId)
            pure (RunReload handle.sessionMeta.metaId)

requestUpdateAndRestart
    :: Maybe FullscreenRuntime
    -> Persistence
    -> IO RunResult
requestUpdateAndRestart fullscreen persist = do
    color <- resolveColor stderr
    let reportInfo message =
            case fullscreen of
                Nothing ->
                    putTextLn stderr
                        (roleMuted color (glyphSession <> message))
                Just runtime ->
                    emitUiEvent runtime (UiSystemMessage message)
        reportError message =
            case fullscreen of
                Nothing ->
                    putTextLn stderr (roleError color message)
                Just runtime ->
                    emitUiEvent runtime (UiErrorMessage message)
    case persist of
        PersistenceDisabled -> do
            reportError "/update-and-restart needs a persisted session"
            pure RunQuit
        PersistenceEnabled slotRef -> do
            handle <- ensureSession slotRef
            reportInfo
                ("updating Haskell Agent; session "
                    <> handle.sessionMeta.metaId
                    <> " will resume")
            pure (RunUpdateAndRestart handle.sessionMeta.metaId)

requestMcpRestart
    :: Maybe FullscreenRuntime
    -> Persistence
    -> IO RunResult
requestMcpRestart fullscreen persist = do
    color <- resolveColor stderr
    let report message =
            case fullscreen of
                Nothing ->
                    putTextLn stderr
                        (roleMuted color (glyphSession <> message))
                Just runtime ->
                    emitUiEvent runtime (UiSystemMessage message)
    case persist of
        PersistenceDisabled -> do
            report
                "MCP configuration saved; restart the agent to apply it"
            pure RunQuit
        PersistenceEnabled slotRef -> do
            handle <- ensureSession slotRef
            report "restarting MCP servers…"
            pure (RunRestart handle.sessionMeta.metaId)

requestGatewayRestart
    :: Maybe FullscreenRuntime
    -> OsPath
    -> IO RunResult
requestGatewayRestart fullscreen cwd = do
    color <- resolveColor stderr
    let report message =
            case fullscreen of
                Nothing ->
                    putTextLn stderr
                        (roleMuted color (glyphSession <> message))
                Just runtime ->
                    emitUiEvent runtime (UiSystemMessage message)
    report
        "organization gateway routing changed; starting a fresh conversation…"
    pure (RunFreshSession cwd)

requestCodeModeRestart
    :: Maybe FullscreenRuntime
    -> Persistence
    -> IO RunResult
requestCodeModeRestart fullscreen persist = do
    color <- resolveColor stderr
    let report message =
            case fullscreen of
                Nothing ->
                    putTextLn stderr
                        (roleMuted color (glyphSession <> message))
                Just runtime ->
                    emitUiEvent runtime (UiSystemMessage message)
    case persist of
        PersistenceDisabled -> do
            report "code mode requires a persisted REPL session"
            pure RunQuit
        PersistenceEnabled slotRef -> do
            handle <- ensureSession slotRef
            report "enabling code mode…"
            pure (RunEnableCodeMode handle.sessionMeta.metaId)

enterPlanFromSlash :: SessionEnv -> Maybe Text -> IO (Maybe ProviderTransition)
enterPlanFromSlash env@SessionEnv
    { sessionPlanMode = planMode
    , sessionPersist = persist
    , sessionRender = render
    , sessionFullscreen = fullscreen
    } maybeDescription = do
    discardStore <- newIORef Nothing
    color <- resolveColor stderr
    let report message minimal = case fullscreen of
            Nothing -> putTextLn stderr (roleMuted color minimal)
            Just runtime -> emitUiEvent runtime (UiSystemMessage message)
    case persist of
        PersistenceEnabled slotRef -> do
            handle <- ensureSession slotRef
            writeIORef planMode.planSessionDir (Just handle.sessionDir)
            report
                ("session: " <> handle.sessionMeta.metaId)
                (glyphSession <> "session: " <> handle.sessionMeta.metaId)
        PersistenceDisabled -> pure ()
    case maybeDescription of
        Nothing -> do
            writeIORef planMode.planStateRef PlanPending
            let message =
                    "plan mode armed; send a prompt to activate \
                    \(or /plan <description>)"
            report message (glyphSession <> message)
            pure Nothing
        Just description -> do
            activatePlanMode planMode
            path <- planFilePath planMode
            let message = "plan mode on (" <> toText path <> ")"
            report message (glyphSession <> message)
            resetRenderPrintedText render
            case fullscreen of
                Nothing -> pure ()
                Just runtime ->
                    emitUiEvent runtime (UiUserSubmitted description)
            let planEnv = env { sessionStoreRoot = discardStore }
                inputs = [UserMessage description]
            result <- runOneTurn planEnv description inputs
            case result of
                TurnProviderUnavailable apiError pending ->
                    requestAutomaticProviderFallback
                        planEnv apiError pending >>= \case
                            Nothing -> do
                                reportProviderUnavailable fullscreen apiError
                                pure Nothing
                            Just providerTransition ->
                                pure (Just providerTransition)
                _ -> do
                    when (isNothing fullscreen) $
                        putTrailingNewline render
                    pure Nothing

preparePromptSkillInputs
    :: SessionEnv
    -> Text
    -> [TurnInput]
    -> IO (Either Text [TurnInput])
preparePromptSkillInputs env = preparePromptSkillInputsWithPaste env False

preparePromptSkillInputsWithPaste
    :: SessionEnv
    -> Bool
    -> Text
    -> [TurnInput]
    -> IO (Either Text [TurnInput])
preparePromptSkillInputsWithPaste env pasted prompt inputs = do
    invocations <- readIORef env.sessionSkillInvocations
    pure do
        selected <- resolvePromptSkillMentions pasted invocations prompt
        let activations =
                [ UserMessage (formatSkillActivation invocation prompt)
                | invocation <- selected
                ]
        pure (activations <> inputs)

putTrailingNewline :: RenderConfig -> IO ()
putTrailingNewline render = do
    didPrint <- renderPrintedText render
    when didPrint (putTextLn render.renderStdout "")
