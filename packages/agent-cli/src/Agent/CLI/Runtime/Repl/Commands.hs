-- | REPL submission and slash-command dispatch.
module Agent.CLI.Runtime.Repl.Commands
    ( handleReplLine
    , preparePromptSkillInputs
    ) where

import Agent.CLI.AccountPicker ()
import Agent.CLI.AccountSelection ()
import Agent.CLI.Afk ()
import Agent.CLI.AgentSessions ()
import Agent.CLI.AgentViewport
    ( AgentViewportEnv(viewportSelect, viewportEntries,
                       viewportSelected) )
import Agent.CLI.Approval ( toggleAlwaysApprove )
import Agent.CLI.Artifact ( fencedCodeBlock, lastDiffBlock )
import Agent.CLI.Auth ()
import Agent.CLI.Clipboard ( loadImagesFromPastedText )
import Agent.CLI.Command
    ( formatSlashHelpWithCatalog,
      parseReplLineWithCatalog,
      ReplAction(ReplCommandError, ReplQuit, ReplReload, ReplPrompt,
                 ReplExpandedPrompt, ReplInvokeSkill, ReplSkills, ReplShowShell,
                 ReplSetShell, ReplPaste, ReplShowAttachments, ReplClearAttachments,
                 ReplShowAgentLimit, ReplSetAgentLimit, ReplAgents, ReplMcp,
                 ReplGoalStatus, ReplGoalPause, ReplGoalResume, ReplGoalClear,
                 ReplGoalSet, ReplWorkflowRuns, ReplWorkflowManage, ReplCopyLast,
                 ReplCopyCode, ReplCopyDiff, ReplCopyPath, ReplCopySession,
                 ReplShowTerminal, ReplShowEffort, ReplSetEffort, ReplShowModel,
                 ReplSetModel, ReplToggleAlwaysApprove, ReplCompact, ReplPlan,
                 ReplBtw, ReplRecap, ReplResume, ReplSearch, ReplClear, ReplNew,
                 ReplShowSession, ReplShowSessionInfo, ReplAfk, ReplWorktree,
                 ReplRename, ReplRenameAuto, ReplLogin, ReplUsage, ReplReloadAuth,
                 ReplHelp),
      ShellMode(ShellNone, ShellGhci, ShellBash, ShellBoth),
      SlashCatalog )
import Agent.CLI.Compaction
    ( CompactOutcome(compactSummary, compactBeforeTokens,
                     compactAfterTokens, compactHistory) )
import Agent.CLI.Config ()
import Agent.CLI.Connectivity ()
import Agent.CLI.Database ()
import Agent.CLI.Database.Store ()
import Agent.CLI.Dialects ()
import Agent.CLI.Error ()
import Agent.CLI.GatewayBridge ()
import Agent.CLI.Input
    ( formatPasteChip,
      submissionPromptText,
      ReplLine(ReplText, ReplEof, ReplQuitInterrupt, ReplCycleMode,
               ReplClipboardPaste, ReplClipboardPasteOrText, ReplChooseModel,
               ReplChooseEffort, ReplChooseAccount, ReplPasted) )
import Agent.CLI.Interrupt ()
import Agent.CLI.LearnedSkills ()
import Agent.CLI.LearnedSkills.Store ()
import Agent.CLI.Login ( runLoginManager )
import Agent.CLI.Lsp ()
import Agent.CLI.ManagedTurn ()
import Agent.CLI.McpManager ( runMcpManager )
import Agent.CLI.McpStatus ()
import Agent.CLI.ModelConfig ()
import Agent.CLI.Models ()
import Agent.CLI.Options ( ApprovalPolicy )
import Agent.CLI.PendingInputs ()
import Agent.CLI.Plan ()
import Agent.CLI.Progress ()
import Agent.CLI.Project ()
import Agent.CLI.Prompt ()
import Agent.CLI.PromptHooks ()
import Agent.CLI.Provider.OpenAI ()
import Agent.CLI.Provider.Switch
    ( reloadAuth,
      reportProviderUnavailable,
      requestAutomaticProviderFallback )
import Agent.CLI.ProviderAvailability ()
import Agent.CLI.ProviderFallback ()
import Agent.CLI.ProviderTransition
    ( ProviderTransition, TurnResult(TurnProviderUnavailable) )
import Agent.CLI.Recap ( RecapKind(..), RecapRequest(..) )
import Agent.CLI.Render
    ( RenderConfig(..),
      clearThinking,
      putTextLn,
      renderEvent,
      renderPrintedText,
      resetRenderPrintedText )
import Agent.CLI.ReplMode ( replModeLabel )
import Agent.CLI.Request ()
import Agent.CLI.Runtime.HistorySource ()
import Agent.CLI.Runtime.Persistence ()
import Agent.CLI.Runtime.Recap ( runSessionRecap )
import Agent.CLI.Runtime.Repl.Attachments
    ( handleAttachmentAction, handleClipboardInput )
import Agent.CLI.Runtime.Repl.Selection
    ( handleSelectionAction, handleSelectionInput )
import Agent.CLI.Runtime.Repl.Session ( handleSessionAction )
import Agent.CLI.Runtime.Repl.Workflow ( handleWorkflowAction )
import Agent.CLI.Runtime.Types
    ( RunResult(RunRestart, RunSwitchProvider, RunReload, RunQuit) )
import Agent.CLI.Secret ()
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
                  turnItems) )
import Agent.CLI.Session.Attachments ( queueAttachedImages )
import Agent.CLI.Session.Choices
    ( accountUsageText, showAccountUsage )
import Agent.CLI.Session.History
    ( modifyLiveAttachments, readLiveAttachments )
import Agent.CLI.Session.Interaction ( runBtwQuestion )
import Agent.CLI.Session.Lifecycle ()
import Agent.CLI.Session.Runtime.Types ()
import Agent.CLI.Session.Selection
    ( currentSessionId, pickAgentChoice )
import Agent.CLI.SessionAdmin ()
import Agent.CLI.SessionEnv ( SessionEnv(..) )
import Agent.CLI.SessionLock ()
import Agent.CLI.SessionState ()
import Agent.CLI.SessionTitle ()
import Agent.CLI.Skills ( formatSkillsListing )
import Agent.CLI.Startup.Auth ()
import Agent.CLI.Startup.Format ()
import Agent.CLI.StartupContext ()
import Agent.CLI.Status ( applyReplMode, cycleReplInteraction )
import Agent.CLI.Style
    ( glyphOk, glyphSession, roleError, roleMuted, roleSuccess )
import Agent.CLI.Subagents.Runtime ()
import Agent.CLI.TUI.App
    ( FullscreenRuntime,
      commitFullscreenImagePreviews,
      commitFullscreenHistoryTurn,
      emitUiEvent,
      setFullscreenImagePreviews,
      withFullscreenSuspended )
import Agent.CLI.TUI.SessionHistory ( sessionHistoryTurn )
import Agent.CLI.TUI.Types ( HistoryCommit(..) )
import Agent.CLI.Terminal
    ( copyTerminalClipboard, formatTerminalCapabilities, resolveColor )
import Agent.CLI.Tools ()
import Agent.CLI.Turn ( runOneTurn )
import Agent.CLI.Usage ()
import Agent.CLI.WebFetch ()
import Agent.CLI.Worktree ()
import Agent.Cancel ()
import Agent.Claude ()
import Agent.Dialect ()
import Agent.Error ()
import Agent.Loop
    ( TurnInput(UserMessage, UserMultimodal, userText, userImages),
      LoopEvent(ActivityUpdated) )
import Agent.OpenAI.Compaction ( compactSessionUserText )
import Agent.OpenAI.Usage ()
import Agent.OpenAI.WebSocketClient ()
import Agent.OpenRouter.LoopBackend ()
import Agent.OsPath ( toText )
import Agent.Provider ( Provider(ClaudeCodeProvider) )
import Agent.Responses.GenericBackend ()
import Agent.Responses.GenericClient ()
import Agent.Responses.Types ()
import Agent.Skills
    ( SkillInvocation(invocationSkill),
      formatSkillActivation,
      resolveSkillInvocation,
      resolveSkillMentions,
      Skill(skillName) )
import Agent.Store.Postgres ()
import Agent.Store.Types ()
import Agent.Subagents ()
import Agent.Subagents.TaskPath ()
import Agent.TUI.Model
    ( infoNotice,
      progressNotice,
      UiEvent(UiUserSubmitted, UiRecapStarted, UiSetNotice, UiErrorMessage,
              UiSystemMessage) )
import Agent.TUI.Motion ()
import Agent.ToolDispatch ()
import Agent.Tools.MultiAgents ()
import Agent.Tools.PlanMode
    ( PlanModeEnv(planStateRef, planSessionDir),
      activatePlanMode,
      planFilePath,
      PlanModeState(PlanPending) )
import Agent.Tools.Secret ()
import Agent.Tools.Types ()
import Agent.XAI.LoopBackend ()
import Control.Applicative ()
import Control.Concurrent.Async ()
import Control.Concurrent.Chan ()
import Control.Concurrent.MVar ()
import Control.Concurrent.STM ()
import Control.Exception ( AsyncException(UserInterrupt) )
import Control.Exception.Safe ( finally, throwIO )
import Control.Monad ( when, forM_ )
import Data.IORef ( newIORef, readIORef, writeIORef )
import Data.List ()
import Data.Maybe ( isNothing )
import Data.Text ( Text )
import Data.Time.Clock ( getCurrentTime )
import System.Console.ANSI ()
import System.Console.ANSI.Codes ()
import System.Directory.OsPath ()
import System.Environment ()
import System.Exit ()
import System.IO ( stdout, hFlush, stderr )
import System.OsPath ()
import System.Posix.Files ()
import qualified Agent.Responses.GenericClient as GenericResponses
    ()
import qualified Agent.MCP as MCP ()
import qualified Data.Map.Strict as Map ()
import qualified Agent.OpenAI.Auth as OpenAI ()
import qualified Agent.OpenRouter as OpenRouter ()
import qualified Agent.OpenRouter.Usage as OpenRouterUsage ()
import qualified Agent.Provider as Provider ()
import qualified Agent.CLI.Session.Lifecycle as SessionLifecycle ()
import qualified Agent.CLI.Session.Runner as SessionRunner ()
import qualified Data.Set as Set ()
import qualified Data.Text as Text
    ( null, strip, pack )
import qualified Data.Text.IO as Text ( putStrLn, hPutStrLn )
import qualified Agent.XAI.Options as XAI ()
import qualified Agent.XAI.Usage as XAIUsage ()

handleReplLine
    :: SessionEnv
    -> (Text -> IO RunResult)
    -> (Bool -> TurnResult -> IO RunResult)
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
            , sessionProvider = provider
            , sessionPolicy = policyRef
            , sessionPersist = persist
            , sessionPlanMode = planMode
            , sessionProjectRoot = projectRoot
            , sessionCwd = cwd
            , sessionTokenProvider = tokenProvider
            , sessionOpenAiPool = openAiPool
            , sessionSkills = skillsRef
            , sessionSkillInvocations = skillInvocationsRef
            , sessionRefreshSkills = refreshSkills
            , sessionPreviewId = previewIdRef
            , sessionLastAssistant = lastAssistantRef
            , sessionTerminal = terminal
            , sessionFullscreen = fullscreen
            , sessionAgentViewport = agentViewport
            }
        continueWith
        finishTurn
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
        -- Confirmed double Ctrl-C: rethrow so withInterruptResume prints
        -- the --resume hint and the process exits.
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
    action@(ReplClipboardPaste _ _) ->
        handleClipboardInput env continueWith stdoutColor action
    action@(ReplClipboardPasteOrText _ _ _) ->
        handleClipboardInput env continueWith stdoutColor action
    action@(ReplChooseModel keptDraft) ->
        handleSelectionInput env (continueWith keptDraft) action
    action@(ReplChooseEffort keptDraft) ->
        handleSelectionInput env (continueWith keptDraft) action
    action@(ReplChooseAccount keptDraft) ->
        handleSelectionInput env (continueWith keptDraft) action
    ReplPasted pasted ->
        submitLine slashCatalog skillInvocations
            continue stdoutColor True pasted
    ReplText line ->
        submitLine slashCatalog skillInvocations
            continue stdoutColor False line
  where
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
                    ReplPrompt text -> do
                        -- Native Cmd+V of a Finder image often pastes a path
                        -- rather than bitmap bytes. Treat a prompt that is
                        -- only image path(s) as an attach + in-terminal preview,
                        -- matching Grok Build's paste chip.
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
                                        (roleMuted color
                                            (glyphOk <> message))
                                continue
                            _ -> do
                                pendingImages <- modifyLiveAttachments conversationRef \imgs -> ([], imgs)
                                forM_ fullscreen \runtime ->
                                    commitFullscreenImagePreviews runtime pendingImages
                                resetRenderPrintedText render
                                let turnInputs =
                                        if null pendingImages
                                            then [UserMessage text]
                                            else
                                                [ UserMultimodal
                                                    { userText = text
                                                    , userImages = pendingImages
                                                    }
                                                ]
                                preparePromptSkillInputs env text turnInputs >>= \case
                                    Left err -> do
                                        displayError err $
                                            Text.hPutStrLn stderr
                                                (roleError color err)
                                        continue
                                    Right skillInputs -> do
                                        fullscreenEvent (UiUserSubmitted text)
                                        result <- runOneTurn env text skillInputs
                                        finishTurn False result
                    ReplExpandedPrompt original expanded ->
                        submitExpandedTurn
                            continue color original expanded
                    ReplInvokeSkill invocationName arguments ->
                        case resolveSkillInvocation
                            skillInvocations invocationName of
                            Left err -> do
                                displayError err $
                                    Text.hPutStrLn stderr
                                        (roleError color err)
                                continue
                            Right invocation -> do
                                pendingImages <-
                                    modifyLiveAttachments conversationRef
                                        \imgs -> ([], imgs)
                                forM_ fullscreen \runtime ->
                                    commitFullscreenImagePreviews runtime pendingImages
                                let userText =
                                        if Text.null arguments
                                            then "Use the "
                                                <> invocation.invocationSkill.skillName
                                                <> " skill."
                                            else arguments
                                    userInput =
                                        if null pendingImages
                                            then UserMessage userText
                                            else UserMultimodal
                                                { userText = userText
                                                , userImages = pendingImages
                                                }
                                    skillInputs =
                                        [ UserMessage
                                            (formatSkillActivation
                                                invocation arguments)
                                        , userInput
                                        ]
                                resetRenderPrintedText render
                                fullscreenEvent (UiUserSubmitted line)
                                result <- runOneTurn env line skillInputs
                                finishTurn False result
                    ReplSkills reloadFirst -> do
                        when reloadFirst (refreshSkills True)
                        current <- readIORef skillsRef
                        invocations <- readIORef skillInvocationsRef
                        let listing =
                                formatSkillsListing color current invocations
                        displayInfo (formatSkillsListing False current invocations) $
                            Text.putStrLn listing
                        continue
                    ReplShowShell -> do
                        mode <- env.sessionShellMode
                        let message = "shell tools: " <> case mode of
                                ShellGhci -> "ghci"
                                ShellBash -> "bash"
                                ShellBoth -> "ghci + bash"
                                ShellNone -> "none"
                        displayInfo message $
                            Text.putStrLn
                                (roleMuted color (glyphSession <> message))
                        continue
                    ReplSetShell mode -> do
                        message <- env.sessionSetShellMode mode
                        displayInfo message $
                            Text.putStrLn
                                (roleMuted color (glyphOk <> message))
                        continue
                    action@ReplPaste{} -> handleAttachmentAction env finishTurn continue action
                    action@ReplShowAttachments -> handleAttachmentAction env finishTurn continue action
                    action@ReplClearAttachments -> handleAttachmentAction env finishTurn continue action
                    ReplShowAgentLimit -> do
                        limit <- env.sessionConcurrentLimit
                        let message =
                                "concurrent agent limit: "
                                    <> Text.pack (show limit)
                        color <- resolveColor stdout
                        displayInfo message $
                            Text.putStrLn
                                (roleMuted color (glyphSession <> message))
                        continue
                    ReplSetAgentLimit limit -> do
                        message <- env.sessionSetConcurrentLimit limit
                        color <- resolveColor stdout
                        displayInfo message $
                            Text.putStrLn
                                (roleMuted color (glyphOk <> message))
                        continue
                    ReplAgents -> do
                        case agentViewport of
                            Nothing -> continue
                            Just viewport -> do
                                entries <- viewport.viewportEntries
                                selected <- readIORef viewport.viewportSelected
                                color <- resolveColor stderr
                                pickAgentChoice
                                    fullscreen color selected entries >>= \case
                                    Nothing -> pure ()
                                    Just target ->
                                        viewport.viewportSelect target
                                continue
                    ReplMcp -> do
                        color <- resolveColor stderr
                        restart <-
                            legacy $
                                runMcpManager
                                    color
                                    env.sessionHome
                                    env.sessionMcpRegistrations
                                    env.sessionMcpWarnings
                        if restart
                            then requestMcpRestart
                                fullscreen persist
                            else continue
                    action@ReplGoalStatus -> handleWorkflowAction env submitExpandedTurn color continue action
                    action@ReplGoalPause -> handleWorkflowAction env submitExpandedTurn color continue action
                    action@ReplGoalResume -> handleWorkflowAction env submitExpandedTurn color continue action
                    action@ReplGoalClear -> handleWorkflowAction env submitExpandedTurn color continue action
                    action@ReplGoalSet{} -> handleWorkflowAction env submitExpandedTurn color continue action
                    action@ReplWorkflowRuns -> handleWorkflowAction env submitExpandedTurn color continue action
                    action@ReplWorkflowManage{} -> handleWorkflowAction env submitExpandedTurn color continue action
                    ReplCopyLast -> do
                        answer <- readIORef lastAssistantRef
                        copyCommand
                            "last response"
                            "no assistant response to copy"
                            answer
                        continue
                    ReplCopyCode index -> do
                        answer <- readIORef lastAssistantRef
                        let label =
                                "code block " <> Text.pack (show index)
                        copyCommand
                            label
                            (label <> " was not found")
                            (answer >>= fencedCodeBlock index)
                        continue
                    ReplCopyDiff -> do
                        answer <- readIORef lastAssistantRef
                        copyCommand
                            "diff block"
                            "no diff block was found"
                            (answer >>= lastDiffBlock)
                        continue
                    ReplCopyPath -> do
                        copyCommand
                            "worktree path"
                            "worktree path is unavailable"
                            (Just (toText cwd))
                        continue
                    ReplCopySession -> do
                        sessionId <- currentSessionId persist
                        copyCommand
                            "session id"
                            "this session has no persisted id yet"
                            sessionId
                        continue
                    ReplShowTerminal -> do
                        let message = formatTerminalCapabilities terminal
                        displayInfo message $
                            Text.putStrLn (roleMuted color message)
                        continue
                    action@ReplShowEffort -> handleSelectionAction env continue action
                    action@ReplSetEffort{} -> handleSelectionAction env continue action
                    action@ReplShowModel -> handleSelectionAction env continue action
                    action@ReplSetModel{} -> handleSelectionAction env continue action
                    ReplToggleAlwaysApprove
                        | provider == ClaudeCodeProvider -> do
                            let message =
                                    "Claude Code permissions are fixed for this provider session; restart with --yolo or --no-yolo."
                            color <- resolveColor stderr
                            displayInfo message $
                                putTextLn stderr (roleMuted color message)
                            continue
                        | otherwise -> do
                            message <- toggleAlwaysApprove policyRef projectRoot
                            color <- resolveColor stderr
                            displayInfo message $
                                putTextLn stderr (roleMuted color message)
                            continue
                    ReplCompact focus -> do
                        color <- resolveColor stderr
                        result <-
                            withReplActivity "Compacting context…" $
                                compactRunner focus
                        case result of
                            Left err -> do
                                displayError err $
                                    Text.hPutStrLn stderr (roleError color err)
                                continue
                            Right outcome -> do
                                fullscreenEvent
                                    (UiSystemMessage outcome.compactSummary)
                                let message =
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
                                displayInfo message $
                                    Text.hPutStrLn stderr
                                        (roleMuted color
                                            (glyphSession <> message))
                                case persist of
                                    PersistenceDisabled -> pure ()
                                    PersistenceEnabled slotRef -> do
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
                                                -- Compaction response usage is
                                                -- recorded immediately by
                                                -- compactRunner, including
                                                -- response-level failures.
                                                , turnUsage = Nothing
                                                }
                                        (handle', turnIndex) <-
                                            appendTurnWithMetaUpdateIndexed handle turn
                                                \meta -> meta
                                                    { metaLastResponseId = Nothing
                                                    }
                                        writeIORef slotRef
                                            (PersistenceActive handle')
                                        forM_ fullscreen \runtime ->
                                            commitFullscreenHistoryTurn
                                                runtime
                                                (sessionHistoryTurn turnIndex turn)
                                                HistoryCommitReplace
                                continue
                    ReplPlan _
                        | provider == ClaudeCodeProvider -> do
                            let message =
                                    "Outer plan mode is unavailable for Claude Code because its tools run inside the Claude CLI."
                            color <- resolveColor stderr
                            displayInfo message $
                                putTextLn stderr (roleMuted color message)
                            continue
                    ReplPlan maybeDescription ->
                        enterPlanFromSlash env maybeDescription >>= \case
                            Just providerSwitch ->
                                pure (RunSwitchProvider providerSwitch)
                            Nothing -> continue
                    ReplBtw question -> do
                        runBtwQuestion True env question
                        continue
                    ReplRecap ->
                        case fullscreen of
                            Just runtime -> do
                                emitUiEvent runtime UiRecapStarted
                                env.sessionQueueRecap (RecapSession RecapManual)
                                continue
                            Nothing -> do
                                runSessionRecap True env RecapManual
                                continue
                    action@ReplResume{} -> handleSessionAction env slashCatalog continue action
                    action@ReplSearch{} -> handleSessionAction env slashCatalog continue action
                    action@ReplClear -> handleSessionAction env slashCatalog continue action
                    action@ReplNew -> handleSessionAction env slashCatalog continue action
                    action@ReplShowSession -> handleSessionAction env slashCatalog continue action
                    action@ReplShowSessionInfo -> handleSessionAction env slashCatalog continue action
                    action@ReplAfk{} -> handleSessionAction env slashCatalog continue action
                    action@ReplWorktree -> handleSessionAction env slashCatalog continue action
                    action@ReplRename{} -> handleSessionAction env slashCatalog continue action
                    action@ReplRenameAuto -> handleSessionAction env slashCatalog continue action
                    ReplLogin -> do
                        color <- resolveColor stderr
                        legacy (runLoginManager color)
                        continue
                    ReplUsage -> do
                        case fullscreen of
                            Nothing ->
                                showAccountUsage
                                    provider tokenProvider openAiPool
                            Just runtime ->
                                accountUsageText
                                    False provider tokenProvider openAiPool
                                    >>= emitUiEvent runtime . UiSystemMessage
                        continue
                    ReplReloadAuth -> do
                        reloadResult <- reloadAuth provider tokenProvider
                        color <- resolveColor stderr
                        case reloadResult of
                            Left err ->
                                displayError err $
                                    putTextLn stderr (roleError color err)
                            Right message ->
                                displayInfo message $
                                    putTextLn stderr (roleMuted color message)
                        continue
                    ReplHelp maybeName -> do
                        color <- resolveColor stdout
                        displayInfo
                            (formatSlashHelpWithCatalog
                                False slashCatalog maybeName) $
                            Text.putStrLn
                                (formatSlashHelpWithCatalog
                                    color slashCatalog maybeName)
                        continue
                    ReplCommandError err -> do
                        color <- resolveColor stderr
                        displayError err $
                            Text.hPutStrLn stderr (roleError color err)
                        continue
    submitExpandedTurn next color original expanded = do
        pendingImages <-
            modifyLiveAttachments conversationRef \imgs -> ([], imgs)
        forM_ fullscreen \runtime ->
            commitFullscreenImagePreviews runtime pendingImages
        let turnInputs =
                if null pendingImages
                    then [UserMessage expanded]
                    else
                        [ UserMultimodal
                            { userText = expanded
                            , userImages = pendingImages
                            }
                        ]
        preparePromptSkillInputs env original turnInputs >>= \case
            Left err -> do
                displayError err $
                    Text.hPutStrLn stderr (roleError color err)
                next
            Right skillInputs -> do
                resetRenderPrintedText render
                fullscreenEvent (UiUserSubmitted original)
                result <- runOneTurn env original skillInputs
                finishTurn False result
    continue = continueWith ""
    legacy action = case fullscreen of
        Nothing -> action
        Just runtime -> withFullscreenSuspended runtime action
    fullscreenEvent event = case fullscreen of
        Nothing -> pure ()
        Just runtime -> emitUiEvent runtime event
    syncFullscreenImagePreviews =
        forM_ fullscreen \runtime ->
            readLiveAttachments conversationRef
                >>= setFullscreenImagePreviews runtime
    displayInfo message minimalAction = case fullscreen of
        Nothing -> minimalAction
        Just runtime -> emitUiEvent runtime (UiSystemMessage message)
    displayError message minimalAction = case fullscreen of
        Nothing -> minimalAction
        Just runtime -> emitUiEvent runtime (UiErrorMessage message)
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
    copyCommand label missing payload = case payload of
        Nothing ->
            displayError missing do
                color <- resolveColor stderr
                Text.hPutStrLn stderr (roleError color missing)
        Just value -> do
            copied <- copyTerminalClipboard terminal stdout value
            if copied
                then
                    let message = "copied " <> label
                    in displayInfo message do
                        color <- resolveColor stderr
                        Text.hPutStrLn stderr
                            (roleSuccess color (glyphOk <> message))
                else
                    displayError "terminal clipboard is unavailable" do
                        color <- resolveColor stderr
                        Text.hPutStrLn stderr
                            (roleError color
                                "terminal clipboard is unavailable")

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
preparePromptSkillInputs env prompt inputs = do
    invocations <- readIORef env.sessionSkillInvocations
    pure do
        selected <- resolveSkillMentions invocations prompt
        let activations =
                [ UserMessage (formatSkillActivation invocation prompt)
                | invocation <- selected
                ]
        pure (activations <> inputs)

putTrailingNewline :: RenderConfig -> IO ()
putTrailingNewline render = do
    didPrint <- renderPrintedText render
    when didPrint (putTextLn render.renderStdout "")
