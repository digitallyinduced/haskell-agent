-- | REPL submission and slash-command dispatch.
module Agent.CLI.Runtime.Repl.Commands
    ( handleReplLine
    , preparePromptSkillInputs
    , preparePromptSkillInputsWithPaste
    ) where

import Agent.CLI.AccountPicker ()
import Agent.CLI.AccountSelection ()
import Agent.CLI.Afk ()
import Agent.CLI.AgentSessions ()
import Agent.CLI.AgentViewport
    ( AgentViewportEnv(viewportSelect, viewportEntries,
                       viewportSelected) )
import Agent.CLI.Approval ( setApprovalPolicy, toggleAlwaysApprove )
import Agent.CLI.Artifact ( fencedCodeBlock, lastDiffBlock )
import Agent.CLI.Auth ()
import Agent.CLI.Changelog (loadReleaseNotes)
import Agent.CLI.Clipboard ( loadImagesFromPastedText )
import Agent.CLI.Command
    ( CopyRequest(..),
      formatSlashHelpWithCatalog,
      parseReplLineWithCatalog,
      ReplAction(ReplCommandError, ReplQuit, ReplReload,
                 ReplUpdateAndRestart, ReplPrompt, ReplExpandedPrompt,
                 ReplInvokeSkill, ReplSkills, ReplShowShell,
                 ReplSetShell, ReplPaste, ReplShowAttachments, ReplClearAttachments,
                 ReplRemoveAttachment,
                 ReplShowAgentLimit, ReplSetAgentLimit, ReplAgents, ReplMcp, ReplMcpPrompt,
                 ReplGoalStatus, ReplGoalPause, ReplGoalResume, ReplGoalClear,
                 ReplGoalSet, ReplWorkflowRuns, ReplWorkflowManage, ReplCopy,
                 ReplCopyCode, ReplCopyDiff, ReplCopyPath, ReplCopySession,
                 ReplDesktop, ReplShowTerminal, ReplChangelog,
                 ReplShowEffort, ReplSetEffort, ReplShowModel,
                 ReplSetModel, ReplShowTheme, ReplSetTheme,
                 ReplToggleFast, ReplEnableCodeMode,
                 ReplToggleAlwaysApprove, ReplCompact, ReplPlan,
                 ReplViewPlan, ReplQueue, ReplTranscript, ReplEditPrompt,
                 ReplContext, ReplHistory, ReplFind,
                 ReplBtw, ReplMetaConsole, ReplRecap, ReplRetry, ReplResume, ReplSearch,
                 ReplHome, ReplRewind, ReplClear, ReplNew, ReplDelete,
                 ReplShowSession, ReplShowSessionInfo, ReplAfk, ReplWorktree,
                 ReplRename, ReplRenameAuto, ReplInit, ReplReview, ReplDiff,
                 ReplFork, ReplExport, ReplPermissions,
                 ReplLogin, ReplUsage, ReplReloadAuth,
                 ReplHelp),
      ShellMode(ShellNone, ShellGhci, ShellBash, ShellBoth),
      SlashCatalog )
import Agent.CLI.Command.Instructions ( initInstruction )
import Agent.CLI.Compaction
    ( CompactOutcome(compactSummary, compactBeforeTokens,
                     compactAfterTokens, compactHistory) )
import Agent.CLI.Config
    ( HarnessConfig(..)
    , McpServerConfig(..)
    , loadHarnessConfig
    , updateHarnessConfig
    )
import Agent.CLI.Context ( formatContextReport )
import Agent.CLI.Transcript
    ( assistantResponseBodies
    , foldTranscriptTurns
    )
import qualified Agent.CLI.Transcript as Transcript
import Agent.CLI.Connectivity ()
import Agent.CLI.Database ()
import Agent.CLI.Database.Store ()
import Agent.CLI.Desktop ( openDesktopConversation )
import Agent.CLI.Dialects ()
import Agent.CLI.Error ()
import Agent.CLI.ExternalProgram
    ( normalizeEditedText
    , resolveExternalProgram
    , runExternalProgramOnFile
    , withTemporaryTextFile
    )
import Agent.CLI.GatewayBridge ()
import Agent.CLI.GitDiff
    ( GitDiffResult(..)
    , colorizeGitDiff
    , getGitDiff
    )
import Agent.CLI.Input
    ( formatPasteChip,
      readApprovalLine,
      readChoiceSelection,
      readChoiceSelectionAt,
      readReplHistory,
      readModalText,
      submissionPromptText,
      truncateDisplayText,
      ReplLine(ReplText, ReplMeta, ReplEof, ReplQuitInterrupt, ReplCycleMode,
               ReplClipboardPaste, ReplClipboardPasteOrText, ReplChooseModel,
               ReplChooseEffort, ReplChooseAccount, ReplRemovePendingImage,
               ReplPasted) )
import Agent.CLI.Interrupt ()
import Agent.CLI.GatewayClient ( loadGatewayCredential )
import Agent.CLI.LearnedSkills ()
import Agent.CLI.LearnedSkills.Store ()
import Agent.CLI.Login
    ( connectProviderAccount
    , runFullscreenLoginManager
    , runLoginManager
    )
import Agent.CLI.Lsp ()
import Agent.CLI.ManagedTurn ()
import Agent.CLI.McpManager ( runMcpManager )
import Agent.CLI.McpOAuth ( loginMcp )
import Agent.CLI.McpStatus ()
import Agent.CLI.ModelConfig ()
import Agent.CLI.Models ()
import Agent.CLI.Options ( ApprovalPolicy(..) )
import Agent.CLI.PendingInputs ()
import Agent.CLI.Permission ( approvalPolicyOptions )
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
import Agent.CLI.Request ()
import Agent.CLI.Review
    ( ReviewBranch(reviewBranchName)
    , ReviewCommit(reviewCommitHash, reviewCommitShortHash,
                   reviewCommitSubject)
    , ReviewTarget(..)
    , listReviewBranches
    , listReviewCommits
    , reviewPrompt
    )
import Agent.CLI.Runtime.HistorySource ()
import Agent.CLI.Runtime.MetaConsole
    ( MetaSecretValue(..)
    , applyMetaConfigActions
    , buildMetaContext
    , isMetaConfigAction
    , metaConfigRequiresRestart
    , runMetaPlanner
    )
import Agent.CLI.Runtime.Persistence ()
import Agent.CLI.Runtime.Recap ( runSessionRecap )
import Agent.CLI.Runtime.Repl.Attachments
    ( handleAttachmentAction, handleClipboardInput )
import Agent.CLI.Runtime.Repl.Selection
    ( handleSelectionAction, handleSelectionInput, selectRequestedAccount )
import Agent.CLI.Runtime.Repl.Session ( handleSessionAction )
import Agent.CLI.Runtime.Repl.Workflow ( handleWorkflowAction )
import Agent.CLI.Runtime.Types
    ( RunResult(RunEnableCodeMode, RunFreshSession, RunRestart, RunUpdateAndRestart,
                RunSwitchProvider, RunReload, RunQuit) )
import Agent.CLI.Secret ( promptSecretLine )
import Agent.CLI.Session
    ( TranscriptEffect(TranscriptReplace),
      appendTurnWithMetaUpdateIndexed,
      ensureSession,
      loadSession,
      Persistence(..),
      PersistenceState(PersistenceActive, PersistencePending),
      sessionsRoot,
      SessionHandle(sessionMeta, sessionDir),
      SessionMeta(metaId, metaLastResponseId),
      SessionTurn(turnUsage, SessionTurn, turnAt, turnUserText,
                  turnAssistantText, turnError, turnResponseId, turnEffect,
                  turnItems, turnProviderTelemetry) )
import Agent.CLI.Session.Attachments ( queueAttachedImages )
import Agent.CLI.Session.Choices
    ( accountUsageText, showAccountUsage )
import Agent.CLI.Session.History
    ( modifyLiveAttachments, readLiveAttachments, readLiveTranscript )
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
import Agent.CLI.Skills
    ( formatSkillsListing
    , resolvePromptSkillMentions
    )
import Agent.CLI.Startup.Auth ()
import Agent.CLI.Startup.Format ()
import Agent.CLI.StartupContext ()
import Agent.CLI.Status ( applyReplMode, cycleReplInteraction )
import Agent.CLI.Style
    ( glyphOk, glyphSession, roleError, roleMuted, roleSuccess )
import Agent.CLI.Subagents.Runtime ()
import Agent.CLI.TUI.App
    ( beginFullscreenLiveHistory,
      commitFullscreenImagePreviews,
      commitFullscreenHistoryTurn,
      emitUiEvent,
      requestFullscreenChoiceWithBody,
      requestFullscreenDocument,
      requestFullscreenFilterChoice,
      requestFullscreenSecret,
      requestFullscreenText,
      queuedFullscreenInputDisplays,
      setFullscreenImagePreviews,
      withFullscreenSuspended )
import Agent.CLI.TUI.SessionHistory ( sessionHistoryTurn )
import Agent.CLI.TUI.Types
    ( FullscreenRuntime(runtimeInput)
    , HistoryCommit(..)
    )
import Agent.CLI.Terminal
    ( copyTerminalClipboard
    , formatTerminalCapabilities
    , resolveColor
    )
import Agent.CLI.TranscriptExport
    ( defaultExportFileName
    , resolveExportPath
    , saveCopyText
    , saveTranscriptNoClobber
    )
import qualified Agent.CLI.TranscriptExport as TranscriptExport
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
    ( LoopEvent(ActivityUpdated)
    , TurnAttachment(ImageAttachmentItem)
    , TurnInput(UserMessage)
    , userMessageWithAttachments
    )
import Agent.OpenAI.Compaction ( compactSessionUserText )
import Agent.OpenAI.Usage ()
import Agent.OpenAI.WebSocketClient ()
import Agent.OpenRouter.LoopBackend ()
import Agent.OsPath ( toText, unsafeToFilePath )
import Agent.Provider ( Provider(ClaudeCodeProvider), providerSlug )
import Agent.Responses.GenericBackend ()
import Agent.Responses.GenericClient ()
import Agent.Responses.Types ( ResponseCreateParams(model) )
import Agent.Skills
    ( SkillInvocation(invocationSkill),
      formatSkillActivation,
      resolveSkillInvocation,
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
      readPlanMarkdown,
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
import Control.Exception.Safe
    ( displayException, finally, throwIO, tryAny, tryIO )
import Control.Monad ( foldM, forM_, unless, when )
import Data.Foldable ( toList )
import Data.IORef ( newIORef, readIORef, writeIORef )
import Data.List ( elemIndex, findIndex )
import Data.Maybe ( fromMaybe, isNothing )
import Data.Text ( Text )
import Data.Time.Clock ( getCurrentTime )
import System.Console.ANSI ()
import System.Console.ANSI.Codes ()
import System.Environment ()
import System.Exit ()
import System.IO ( stdout, hFlush, stderr )
import System.IO.Error ( isDoesNotExistError )
import System.OsPath ( OsPath, unsafeEncodeUtf, (</>) )
import System.Posix.Files ( getSymbolicLinkStatus )
import qualified Agent.Responses.GenericClient as GenericResponses
    ()
import qualified Agent.MCP as MCP
import qualified Agent.CLI.MetaConsole as Meta
    ( MetaAction(..)
    , MetaPlan(..)
    , formatMetaError
    , metaPlanMutates
    , metaPlanPreviews
    )
import qualified Data.Map.Strict as Map
import qualified Agent.OpenAI.Auth as OpenAI ()
import qualified Agent.OpenRouter as OpenRouter ()
import qualified Agent.OpenRouter.Usage as OpenRouterUsage ()
import qualified Agent.Provider as Provider ()
import qualified Agent.CLI.Session.Lifecycle as SessionLifecycle ()
import qualified Agent.CLI.Session.Runner as SessionRunner ()
import qualified Data.Set as Set ()
import qualified Data.Text as Text
    ( intercalate, map, null, pack, replace, strip, toCaseFold, toLower )
import qualified Data.Text.IO as Text ( putStrLn, hPutStrLn, readFile )
import qualified Agent.XAI.Options as XAI ()
import qualified Agent.XAI.Usage as XAIUsage ()

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
            , sessionDatabasePool = databasePool
            , sessionPlanMode = planMode
            , sessionProjectRoot = projectRoot
            , sessionCwd = cwd
            , sessionHome = home
            , sessionTokenProvider = tokenProvider
            , sessionOpenAiPool = openAiPool
            , sessionGatewayModels = gatewayModelsRef
            , sessionSkills = skillsRef
            , sessionSkillInvocations = skillInvocationsRef
            , sessionRefreshSkills = refreshSkills
            , sessionDraft = draftRef
            , sessionPreviewId = previewIdRef
            , sessionInterrupt = interrupt
            , sessionLastAssistant = lastAssistantRef
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
    action@(ReplClipboardPaste _ _) ->
        handleClipboardInput env continueWith stdoutColor action
    action@(ReplClipboardPasteOrText _ _ _) ->
        handleClipboardInput env continueWith stdoutColor action
    action@(ReplChooseModel keptDraft) ->
        handleSelectionInput
            env
            (continueWith keptDraft)
            retryPendingTurn
            action
    action@(ReplChooseEffort keptDraft) ->
        handleSelectionInput
            env
            (continueWith keptDraft)
            retryPendingTurn
            action
    action@(ReplChooseAccount keptDraft) ->
        handleSelectionInput
            env
            (continueWith keptDraft)
            retryPendingTurn
            action
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
                                        [ userMessageWithAttachments
                                            text
                                            (map
                                                ImageAttachmentItem
                                                pendingImages)
                                        ]
                                preparePromptSkillInputsWithPaste env pasted text turnInputs >>= \case
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
                        submitExpandedTurnWithPaste
                            pasted continue color original expanded
                    ReplInit -> do
                        let guidePath = cwd </> unsafeEncodeUtf "AGENTS.md"
                        tryIO
                            (getSymbolicLinkStatus
                                (unsafeToFilePath guidePath)) >>= \case
                            Left err
                                | isDoesNotExistError err ->
                                    submitExpandedTurn
                                        continue
                                        color
                                        line
                                        initInstruction
                                | otherwise -> do
                                    let message =
                                            "could not check AGENTS.md: "
                                                <> Text.pack
                                                    (displayException err)
                                    displayError message $
                                        Text.hPutStrLn stderr
                                            (roleError color message)
                                    continue
                            Right _ -> do
                                let message =
                                        "AGENTS.md already exists; left it unchanged."
                                displayInfo message $
                                    Text.putStrLn
                                        (roleMuted color
                                            (glyphSession <> message))
                                continue
                    ReplReview (Just instructions) ->
                        submitExpandedTurn
                            continue
                            color
                            line
                            (reviewPrompt (ReviewCustom instructions))
                    ReplReview Nothing ->
                        chooseReviewTarget >>= \case
                            Left err -> do
                                displayError err $
                                    Text.hPutStrLn stderr
                                        (roleError color err)
                                continue
                            Right Nothing -> continue
                            Right (Just target) ->
                                submitExpandedTurn
                                    continue
                                    color
                                    line
                                    (reviewPrompt target)
                    ReplDiff -> do
                        result <-
                            withReplActivity "Loading Git diff…" $
                                getGitDiff cwd
                        case result of
                            Left err -> do
                                displayError err $
                                    Text.hPutStrLn stderr
                                        (roleError color err)
                                continue
                            Right GitDiffNotRepository -> do
                                let message = "not a Git repository"
                                displayError message $
                                    Text.hPutStrLn stderr
                                        (roleError color message)
                                continue
                            Right (GitDiffOutput diff)
                                | Text.null (Text.strip diff) -> do
                                    let message =
                                            "No working-tree changes."
                                    displayInfo message $
                                        Text.putStrLn
                                            (roleMuted color
                                                (glyphSession <> message))
                                    continue
                                | otherwise -> do
                                    displayInfo diff $
                                        Text.putStrLn
                                            (colorizeGitDiff color diff)
                                    continue
                    ReplExport maybePath -> do
                        exportTranscript maybePath
                        continue
                    ReplPermissions
                        | provider == ClaudeCodeProvider -> do
                            let message =
                                    "Claude Code permissions are fixed for this provider session; restart with --yolo or --no-yolo."
                            displayInfo message $
                                Text.hPutStrLn stderr
                                    (roleMuted color message)
                            continue
                        | otherwise -> do
                            current <- readIORef policyRef
                            requestChoice
                                "Permissions"
                                "Choose how mutating tools are handled."
                                (approvalPolicyIndex current)
                                approvalPolicyRows >>= \case
                                    Nothing -> continue
                                    Just index -> do
                                        message <-
                                            setApprovalPolicy
                                                policyRef
                                                projectRoot
                                                (approvalPolicyAt index)
                                        displayInfo message $
                                            Text.hPutStrLn stderr
                                                (roleMuted color
                                                    (glyphOk <> message))
                                        continue
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
                                        userMessageWithAttachments
                                            userText
                                            (map
                                                ImageAttachmentItem
                                                pendingImages)
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
                    action@ReplRemoveAttachment{} -> handleAttachmentAction env finishTurn continue action
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
                    ReplMcpPrompt server name arguments -> do
                        outcome <- case env.sessionMcpFleet of
                            Nothing -> pure (Left "no MCP servers are configured")
                            Just fleet ->
                                MCP.mcpFleetGetPrompt fleet server name arguments
                        case outcome of
                            Left err -> do
                                displayError err $
                                    Text.hPutStrLn stderr (roleError color err)
                                continue
                            Right result ->
                                submitExpandedTurn
                                    continue
                                    color
                                    ("/mcp prompt " <> server <> " " <> name)
                                    (MCP.renderMcpPromptResult result)
                    action@ReplGoalStatus -> handleWorkflowAction env submitExpandedTurn color continue action
                    action@ReplGoalPause -> handleWorkflowAction env submitExpandedTurn color continue action
                    action@ReplGoalResume -> handleWorkflowAction env submitExpandedTurn color continue action
                    action@ReplGoalClear -> handleWorkflowAction env submitExpandedTurn color continue action
                    action@ReplGoalSet{} -> handleWorkflowAction env submitExpandedTurn color continue action
                    action@ReplWorkflowRuns -> handleWorkflowAction env submitExpandedTurn color continue action
                    action@ReplWorkflowManage{} -> handleWorkflowAction env submitExpandedTurn color continue action
                    ReplCopy request -> do
                        loadAssistantResponses >>= \case
                            Left err ->
                                displayError err do
                                    color <- resolveColor stderr
                                    Text.hPutStrLn stderr
                                        (roleError color err)
                            Right responses ->
                                case listAt
                                    (request.copyResponseIndex - 1)
                                    responses of
                                    Nothing -> do
                                        let available = length responses
                                            responseNoun
                                                | available == 1 =
                                                    "response is"
                                                | otherwise =
                                                    "responses are"
                                            message
                                                | available == 0 =
                                                    "no assistant response to copy"
                                                | otherwise =
                                                    "only "
                                                        <> Text.pack
                                                            (show available)
                                                        <> " assistant "
                                                        <> responseNoun
                                                        <> " available to copy"
                                        displayError message do
                                            color <- resolveColor stderr
                                            Text.hPutStrLn stderr
                                                (roleError color message)
                                    Just answer ->
                                        copyAssistantResponse request answer
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
                    ReplDesktop -> do
                        currentSessionId persist >>= \case
                            Nothing -> do
                                let err =
                                        "/desktop requires a persisted \
                                        \conversation"
                                color <- resolveColor stderr
                                displayError err $
                                    Text.hPutStrLn stderr (roleError color err)
                            Just sessionId ->
                                openDesktopConversation sessionId >>= \case
                                    Left err -> do
                                        color <- resolveColor stderr
                                        displayError err $
                                            Text.hPutStrLn stderr
                                                (roleError color err)
                                    Right () -> do
                                        let message =
                                                "opened conversation in \
                                                \Haskell Agent"
                                        color <- resolveColor stderr
                                        displayInfo message $
                                            Text.hPutStrLn stderr
                                                (roleSuccess color
                                                    (glyphOk <> message))
                        continue
                    ReplShowTerminal -> do
                        let message = formatTerminalCapabilities terminal
                        displayInfo message $
                            Text.putStrLn (roleMuted color message)
                        continue
                    ReplChangelog -> do
                        releaseNotes <- loadReleaseNotes
                        case fullscreen of
                            Just runtime -> do
                                requestFullscreenDocument
                                    runtime
                                    "Release Notes"
                                    releaseNotes
                            Nothing ->
                                displayInfo releaseNotes
                                    (Text.putStrLn releaseNotes)
                        continue
                    action@ReplShowEffort -> handleSelectionAction env continue action
                    action@ReplSetEffort{} -> handleSelectionAction env continue action
                    action@ReplToggleFast -> handleSelectionAction env continue action
                    action@ReplShowModel -> handleSelectionAction env continue action
                    action@ReplSetModel{} -> handleSelectionAction env continue action
                    action@ReplShowTheme -> handleSelectionAction env continue action
                    action@ReplSetTheme{} -> handleSelectionAction env continue action
                    ReplEnableCodeMode ->
                        requestCodeModeRestart fullscreen persist
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
                                case persist of
                                    PersistenceDisabled ->
                                        fullscreenEvent
                                            (UiSystemMessage
                                                outcome.compactSummary)
                                    PersistenceEnabled slotRef -> do
                                        forM_ fullscreen
                                            beginFullscreenLiveHistory
                                        fullscreenEvent
                                            (UiSystemMessage
                                                outcome.compactSummary)
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
                                                , turnProviderTelemetry = []
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
                                                HistoryCommitAppend
                                displayInfo statsMessage $
                                    Text.hPutStrLn stderr
                                        (roleMuted color
                                            (glyphSession <> statsMessage))
                                continue
                    ReplViewPlan -> do
                        markdown <- readPlanMarkdown planMode
                        if Text.null (Text.strip markdown)
                            then do
                                let message =
                                        "No saved plan is available for this session."
                                displayInfo message (Text.putStrLn message)
                            else
                                displayInfo markdown (Text.putStrLn markdown)
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
                    ReplQueue -> do
                        prompts <- case fullscreen of
                            Nothing -> pure []
                            Just runtime ->
                                toList
                                    <$> queuedFullscreenInputDisplays
                                        runtime.runtimeInput
                        let message = formatQueuedPrompts prompts
                        displayInfo message (Text.putStrLn message)
                        continue
                    ReplContext -> do
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
                        continue
                    ReplHistory -> do
                        prompts <-
                            filter
                                ((/= "/history")
                                    . Text.toCaseFold
                                    . Text.strip)
                                <$> readReplHistory
                        case prompts of
                            [] -> do
                                let message =
                                        "No prompt history is available."
                                displayInfo message (Text.putStrLn message)
                                continue
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
                                maybe continue continueWith selected
                    ReplTranscript -> do
                        outcome <- case persist of
                            PersistenceDisabled ->
                                pure (Right "No conversation transcript is available yet.")
                            PersistenceEnabled slotRef ->
                                readIORef slotRef >>= \case
                                    PersistencePending _ _ _ ->
                                        pure (Right "No conversation transcript is available yet.")
                                    PersistenceActive handle ->
                                        loadSession
                                            env.sessionDatabasePool
                                            (sessionsRoot env.sessionHome)
                                            handle.sessionMeta.metaId
                                            >>= \case
                                                Left err -> pure (Left err)
                                                Right (meta, turns) ->
                                                    let blocks =
                                                            foldTranscriptTurns
                                                                (zip [0 ..] turns)
                                                    in if null blocks
                                                        then pure (Right "No conversation transcript is available yet.")
                                                        else
                                                            legacy
                                                                (openPager
                                                                    (Transcript.renderTranscriptMarkdown
                                                                        meta
                                                                        blocks))
                                                                >>= \case
                                                                    Left err -> pure (Left err)
                                                                    Right () -> pure (Right "")
                        case outcome of
                            Left err -> do
                                displayError err $
                                    Text.hPutStrLn stderr
                                        (roleError color err)
                            Right message
                                | Text.null message -> pure ()
                                | otherwise ->
                                    displayInfo message (Text.putStrLn message)
                        continue
                    ReplFind maybeQuery ->
                        loadPersistedTranscript >>= \case
                            Left err -> do
                                displayError err $
                                    Text.hPutStrLn stderr
                                        (roleError color err)
                                continue
                            Right Nothing -> do
                                let message =
                                        "No conversation transcript is available yet."
                                displayInfo message (Text.putStrLn message)
                                continue
                            Right (Just (meta, blocks)) -> do
                                let query = fromMaybe "" maybeQuery
                                    matches =
                                        Transcript.searchTranscriptBlocks
                                            query
                                            blocks
                                if null matches
                                    then do
                                        let message =
                                                "No transcript blocks matched “"
                                                    <> query
                                                    <> "”."
                                        displayInfo message
                                            (Text.putStrLn message)
                                    else
                                        legacy
                                            (openPager
                                                (Transcript.renderTranscriptMarkdown
                                                    meta
                                                    matches))
                                            >>= \case
                                                Left err ->
                                                    displayError err $
                                                        Text.hPutStrLn stderr
                                                            (roleError color err)
                                                Right () -> pure ()
                                continue
                    ReplEditPrompt -> do
                        legacy editPrompt >>= \case
                            Left err -> do
                                displayError err $
                                    Text.hPutStrLn stderr
                                        (roleError color err)
                                continue
                            Right edited -> continueWith edited
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
                    ReplRetry ->
                        resumePendingTurnIfPresent
                            env.sessionLastFailedTurn
                            retryPendingTurn
                            (do
                                displayInfo
                                    "No failed turn is available to retry."
                                    (pure ())
                                continue)
                    action@ReplResume{} -> handleSessionAction env slashCatalog continue action
                    action@ReplSearch{} -> handleSessionAction env slashCatalog continue action
                    action@ReplHome -> handleSessionAction env slashCatalog continue action
                    action@ReplRewind -> handleSessionAction env slashCatalog continue action
                    action@ReplClear -> handleSessionAction env slashCatalog continue action
                    action@ReplNew -> handleSessionAction env slashCatalog continue action
                    action@ReplDelete -> handleSessionAction env slashCatalog continue action
                    action@ReplFork{} -> handleSessionAction env slashCatalog continue action
                    action@ReplShowSession -> handleSessionAction env slashCatalog continue action
                    action@ReplShowSessionInfo -> handleSessionAction env slashCatalog continue action
                    action@ReplAfk{} -> handleSessionAction env slashCatalog continue action
                    action@ReplWorktree -> handleSessionAction env slashCatalog continue action
                    action@ReplRename{} -> handleSessionAction env slashCatalog continue action
                    action@ReplRenameAuto -> handleSessionAction env slashCatalog continue action
                    ReplLogin -> do
                        gatewayBefore <- loadGatewayCredential
                        case fullscreen of
                            Just runtime ->
                                runFullscreenLoginManager runtime
                            Nothing -> do
                                color <- resolveColor stderr
                                runLoginManager color
                        gatewayAfter <- loadGatewayCredential
                        if gatewayAfter /= gatewayBefore
                            then requestGatewayRestart fullscreen cwd
                            else continue
                    ReplUsage -> do
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
                                        showAccountUsage
                                            provider tokenProvider openAiPool
                                    Just runtime ->
                                        accountUsageText
                                            False provider tokenProvider openAiPool
                                            >>= emitUiEvent runtime
                                                . UiSystemMessage
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
    runMetaConsoleRequest rawRequest
        | Text.null request = do
            displayError "Meta Console request must not be empty" do
                color <- resolveColor stderr
                Text.hPutStrLn stderr
                    (roleError color
                        "Meta Console request must not be empty")
            continue
        | otherwise =
            loadHarnessConfig env.sessionHome >>= \case
                Left err -> metaFailure err
                Right config -> do
                    context <- buildMetaContext env config
                    obtainMetaPlan context request (0 :: Int) >>= \case
                        Left err -> metaFailure err
                        Right Nothing -> do
                            displayInfo "Meta Console cancelled" do
                                color <- resolveColor stderr
                                Text.hPutStrLn stderr
                                    (roleMuted color
                                        "Meta Console cancelled")
                            continue
                        Right (Just plan) -> do
                            approved <- approveMetaPlan plan
                            if not approved
                                then do
                                    displayInfo
                                        "Meta Console changes cancelled"
                                        do
                                            color <-
                                                resolveColor stderr
                                            Text.hPutStrLn stderr
                                                (roleMuted color
                                                    "Meta Console changes cancelled")
                                    continue
                                else
                                    applyMetaPlan config plan
      where
        request = Text.strip rawRequest
    obtainMetaPlan context original clarificationCount =
        withReplActivity "Meta Console · interpreting…" $
            runMetaPlanner env context original >>= \case
                Left err ->
                    pure (Left (Meta.formatMetaError err))
                Right plan ->
                    case plan.metaActions of
                        [Meta.MetaClarify question]
                            | clarificationCount >= 2 ->
                                pure
                                    (Left
                                        "Meta Console still needs clarification after two replies")
                            | otherwise ->
                                askMetaClarification question >>= \case
                                    Nothing -> pure (Right Nothing)
                                    Just answer ->
                                        obtainMetaPlan
                                            context
                                            (original
                                                <> "\n\nClarification question: "
                                                <> question
                                                <> "\nClarification answer: "
                                                <> answer)
                                            (clarificationCount + 1)
                        _ -> pure (Right (Just plan))
    askMetaClarification question =
        case fullscreen of
            Just runtime ->
                requestFullscreenText
                    runtime
                    "Meta Console clarification"
                    question
                    ""
            Nothing ->
                readApprovalLine
                    ("\nMeta Console needs clarification:\n"
                        <> safeMetaText question
                        <> "\nanswer> ")
    approveMetaPlan plan
        | not (Meta.metaPlanMutates plan) = pure True
        | otherwise =
            readIORef policyRef >>= \case
                ApproveAll -> do
                    showMetaPreview "Meta Console will apply" plan
                    pure True
                DenyMutating -> do
                    displayError
                        "Meta Console changes are blocked by the current deny-mutations policy"
                        do
                            color <- resolveColor stderr
                            Text.hPutStrLn stderr
                                (roleError color
                                    "Meta Console changes are blocked by the current deny-mutations policy")
                    pure False
                PromptMutating ->
                    case fullscreen of
                        Just runtime ->
                            requestFullscreenChoiceWithBody
                                runtime
                                "Apply Meta Console changes?"
                                (metaPreviewBody plan)
                                0
                                [ ( "Apply changes"
                                  , "Execute only the typed actions shown above"
                                  )
                                , ( "Cancel"
                                  , "Leave configuration unchanged"
                                  )
                                ]
                                >>= pure . (== Just 0)
                        Nothing -> do
                            showMetaPreview "Meta Console proposes" plan
                            readApprovalLine
                                "Apply these changes? [y/N] "
                                >>= pure . maybe False isYes
    showMetaPreview heading plan =
        displayInfo (heading <> "\n" <> metaPreviewBody plan) do
            color <- resolveColor stderr
            Text.hPutStrLn stderr
                (roleMuted color
                    (heading <> "\n" <> metaPreviewBody plan))
    metaPreviewBody plan =
        safeMetaText plan.metaSummary
            <> "\n"
            <> Text.intercalate
                "\n"
                [ Text.pack (show index)
                    <> ". "
                    <> safeMetaText preview
                | (index, preview) <-
                    zip [(1 :: Int) ..] (Meta.metaPlanPreviews plan)
                ]
    applyMetaPlan initial plan =
        collectMetaSecrets plan.metaActions >>= \case
            Left err -> metaFailure err
            Right secrets -> do
                configResult <-
                    if any isMetaConfigAction plan.metaActions
                        then
                            updateHarnessConfig
                                env.sessionHome
                                (applyMetaConfigActions
                                    secrets
                                    plan.metaActions)
                        else pure (Right initial)
                case configResult of
                    Left err -> metaFailure err
                    Right appliedConfig ->
                        executeMetaHostActions
                            appliedConfig
                            plan.metaActions
                            >>= \case
                                Left err -> metaFailure err
                                Right terminalResult -> do
                                    let success =
                                            (if Meta.metaPlanMutates plan
                                                then "Meta Console applied\n"
                                                else "Meta Console\n")
                                                <> metaPreviewBody plan
                                    displayInfo success do
                                        color <- resolveColor stderr
                                        Text.hPutStrLn stderr
                                            (roleSuccess color
                                                (glyphOk <> success))
                                    case terminalResult of
                                        Just result -> pure result
                                        Nothing
                                            | metaPlanNeedsRestart
                                                plan.metaActions ->
                                                requestMetaRestart
                                                    fullscreen
                                                    persist
                                            | otherwise -> continue
    collectMetaSecrets =
        foldM collectOneSecret (Right [])
      where
        collectOneSecret (Left err) _ = pure (Left err)
        collectOneSecret (Right values) action = case action of
            Meta.MetaSetMcpSecretEnv server key ->
                promptMetaSecret
                    ("MCP " <> server <> " · " <> key)
                    ("Enter the value for environment variable "
                        <> key
                        <> " on MCP server "
                        <> server
                        <> ". It stays local and is never sent to the model.")
                    >>= \case
                        Nothing ->
                            pure
                                (Left
                                    ("secret input for MCP server '"
                                        <> server
                                        <> "' was cancelled"))
                        Just value ->
                            pure
                                (Right
                                    (values
                                        <> [ MetaMcpSecretValue
                                                server key value
                                           ]))
            Meta.MetaSetLspSecretEnv server key ->
                promptMetaSecret
                    ("LSP " <> server <> " · " <> key)
                    ("Enter the value for environment variable "
                        <> key
                        <> " on LSP server "
                        <> server
                        <> ". It stays local and is never sent to the model.")
                    >>= \case
                        Nothing ->
                            pure
                                (Left
                                    ("secret input for LSP server '"
                                        <> server
                                        <> "' was cancelled"))
                        Just value ->
                            pure
                                (Right
                                    (values
                                        <> [ MetaLspSecretValue
                                                server key value
                                           ]))
            _ -> pure (Right values)
    promptMetaSecret title body =
        case fullscreen of
            Just runtime ->
                requestFullscreenSecret runtime title body
            Nothing ->
                promptSecretLine
                    env.sessionEscPaused
                    body
                    (Just
                        "Meta Console configuration; the value is written only to the local config file")
    executeMetaHostActions config =
        foldM (executeOneMetaHostAction config) (Right Nothing)
    executeOneMetaHostAction _ (Left err) _ = pure (Left err)
    executeOneMetaHostAction _ result@(Right (Just _)) _ = pure result
    executeOneMetaHostAction config (Right Nothing) action = case action of
        Meta.MetaConnectAccount requestedProvider -> do
            color <- resolveColor stderr
            tryAny
                (legacy
                    (connectProviderAccount color requestedProvider))
                >>= \case
                    Left err ->
                        pure
                            (Left
                                ("Could not connect "
                                    <> providerSlug requestedProvider
                                    <> ": "
                                    <> Text.pack (displayException err)))
                    Right Nothing ->
                        pure
                            (Left
                                ("Connecting "
                                    <> providerSlug requestedProvider
                                    <> " was cancelled or did not complete"))
                    Right (Just _) -> pure (Right Nothing)
        Meta.MetaSelectAccount requestedProvider selector ->
            selectRequestedAccount env requestedProvider selector
        Meta.MetaLoginMcpOAuth name ->
            case Map.lookup name config.configMcpServers >>= (.mcpUrl) of
                Nothing ->
                    pure
                        (Left
                            ("Remote MCP server '"
                                <> name
                                <> "' is not configured"))
                Just url ->
                    tryAny (legacy (loginMcp url)) >>= \case
                        Left _ ->
                            pure
                                (Left
                                    "MCP OAuth login failed; the login flow did not complete")
                        Right () -> pure (Right Nothing)
        Meta.MetaSessionCommand command ->
            runMetaSessionCommand command
        Meta.MetaInform _ -> pure (Right Nothing)
        _ -> pure (Right Nothing)
    runMetaSessionCommand command =
        case parseReplLineWithCatalog slashCatalog command of
            action
                | safeMetaSessionAction action -> do
                    result <-
                        submitLine
                            slashCatalog
                            skillInvocations
                            (pure RunQuit)
                            stdoutColor
                            False
                            command
                    pure $
                        Right case result of
                            RunQuit -> Nothing
                            terminalResult -> Just terminalResult
            _ ->
                pure
                    (Left
                        ("Meta Console rejected unsupported session command: "
                            <> command))
    safeMetaSessionAction = \case
        ReplSetEffort{} -> True
        ReplToggleFast -> True
        ReplSetModel{} -> True
        ReplSetShell{} -> True
        ReplToggleAlwaysApprove -> True
        ReplSetAgentLimit{} -> True
        ReplEnableCodeMode -> True
        ReplSkills True -> True
        _ -> False
    metaPlanNeedsRestart actions =
        metaConfigRequiresRestart actions
            || any
                (\case
                    Meta.MetaLoginMcpOAuth{} -> True
                    _ -> False)
                actions
    metaFailure err = do
        let safeError = safeMetaText err
        displayError safeError do
            color <- resolveColor stderr
            Text.hPutStrLn stderr (roleError color safeError)
        continue
    isYes =
        (`elem` ["y", "yes"])
            . Text.toLower
            . Text.strip
    safeMetaText =
        Text.map
            (\character ->
                if character < ' ' && character `notElem` ['\n', '\t']
                    then ' '
                    else character)
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
    exportTranscript maybePath =
        loadActiveTranscript >>= \case
            Left err ->
                displayError err $
                    Text.hPutStrLn stderr (roleError stdoutColor err)
            Right (sessionId, turns) -> do
                let markdown = TranscriptExport.renderTranscriptMarkdown turns
                    defaultPath = defaultExportFileName sessionId
                case maybePath of
                    Just path -> saveExport markdown path
                    Nothing ->
                        requestChoice
                            "Export conversation"
                            "Copy the visible conversation or save it as Markdown."
                            0
                            [ ( "Copy Markdown to clipboard"
                              , "Copy the current visible transcript"
                              )
                            , ( "Save Markdown to a file"
                              , "Create a new file without overwriting"
                              )
                            ] >>= \case
                                Nothing -> pure ()
                                Just 0 ->
                                    copyCommand
                                        "conversation Markdown"
                                        "conversation is unavailable"
                                        (Just markdown)
                                Just _ ->
                                    requestText
                                        "Export path"
                                        "Relative paths use the current working directory. Existing files are never replaced."
                                        defaultPath >>= mapM_
                                            (\entered ->
                                                forM_
                                                    (nonBlank entered)
                                                    (saveExport markdown))
    loadActiveTranscript =
        case persist of
            PersistenceDisabled ->
                pure
                    (Left
                        "transcript export requires a persisted session")
            PersistenceEnabled slotRef ->
                readIORef slotRef >>= \case
                    PersistencePending{} ->
                        pure
                            (Left
                                "transcript export requires an active persisted session")
                    PersistenceActive handle ->
                        loadSession
                            databasePool
                            (sessionsRoot home)
                            handle.sessionMeta.metaId >>= \case
                                Left err -> pure (Left err)
                                Right (_, turns) ->
                                    pure
                                        (Right
                                            ( handle.sessionMeta.metaId
                                            , turns
                                            ))
    saveExport markdown rawPath =
        resolveExportPath cwd rawPath >>= \case
            Left err ->
                displayError err $
                    Text.hPutStrLn stderr
                        (roleError stdoutColor err)
            Right path ->
                saveTranscriptNoClobber path markdown >>= \case
                    Left err -> do
                        let message =
                                "could not export to "
                                    <> toText path
                                    <> ": "
                                    <> err
                        displayError message $
                            Text.hPutStrLn stderr
                                (roleError stdoutColor message)
                    Right () -> do
                        let message =
                                "exported conversation to " <> toText path
                        displayInfo message $
                            Text.hPutStrLn stderr
                                (roleSuccess stdoutColor
                                    (glyphOk <> message))
    requestChoice title body initial rows
        | null rows = pure Nothing
        | otherwise =
            case fullscreen of
                Just runtime ->
                    requestFullscreenChoiceWithBody
                        runtime
                        title
                        body
                        (max 0 (min (length rows - 1) initial))
                        rows
                Nothing -> do
                    color <- resolveColor stderr
                    Text.hPutStrLn stderr
                        (roleMuted color
                            (Text.intercalate
                                "\n"
                                (filter
                                    (not . Text.null)
                                    [title, body])))
                    let labels =
                            [ if Text.null detail
                                then label
                                else label <> " — " <> detail
                            | (label, detail) <- rows
                            ]
                    selected <-
                        readChoiceSelectionAt initial
                            (\active label ->
                                if active
                                    then roleSuccess color label
                                    else roleMuted color label)
                            labels
                    pure (selected >>= (`elemIndex` labels))
    requestText title body initial =
        case fullscreen of
            Just runtime ->
                requestFullscreenText runtime title body initial
            Nothing -> do
                color <- resolveColor stderr
                unless (Text.null (Text.strip body)) $
                    Text.hPutStrLn stderr (roleMuted color body)
                readModalText interrupt (title <> ": ") initial
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
    loadAssistantResponses =
        loadPersistedTranscript >>= \case
            Left err -> pure (Left err)
            Right persisted -> do
                latest <- readIORef lastAssistantRef
                let responses =
                        maybe
                            []
                            (assistantResponseBodies . snd)
                            persisted
                pure $
                    Right $
                        if null responses
                            then maybe [] pure latest
                            else responses
    copyAssistantResponse request answer = do
        let index = request.copyResponseIndex
            label
                | index == 1 = "last response"
                | otherwise =
                    "response " <> Text.pack (show index)
        case request.copyDestination of
            Nothing ->
                copyCommand
                    label
                    "no assistant response to copy"
                    (Just answer)
            Just rawPath ->
                resolveExportPath cwd rawPath >>= \case
                    Left err ->
                        displayError err do
                            color <- resolveColor stderr
                            Text.hPutStrLn stderr
                                (roleError color err)
                    Right path ->
                        saveCopyText path answer >>= \case
                            Left err ->
                                displayError err do
                                    color <- resolveColor stderr
                                    Text.hPutStrLn stderr
                                        (roleError color err)
                            Right () -> do
                                let message =
                                        "copied "
                                            <> label
                                            <> " to "
                                            <> toText path
                                displayInfo message do
                                    color <- resolveColor stderr
                                    Text.hPutStrLn stderr
                                        (roleSuccess color
                                            (glyphOk <> message))
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

    loadPersistedTranscript =
        case persist of
            PersistenceDisabled -> pure (Right Nothing)
            PersistenceEnabled slotRef ->
                readIORef slotRef >>= \case
                    PersistencePending{} -> pure (Right Nothing)
                    PersistenceActive handle ->
                        loadSession
                            databasePool
                            (sessionsRoot home)
                            handle.sessionMeta.metaId
                            >>= pure . fmap
                                (\(meta, turns) ->
                                    let blocks =
                                            foldTranscriptTurns
                                                (zip [0 ..] turns)
                                    in if null blocks
                                        then Nothing
                                        else Just (meta, blocks))

    openPager markdown = do
        outcome <- tryAny do
            resolveExternalProgram
                [("PAGER", "$PAGER")]
                "less -R" >>= \case
                    Left err -> pure (Left err)
                    Right program ->
                        withTemporaryTextFile
                            "agent-transcript-"
                            markdown
                            (runExternalProgramOnFile program)
        pure $ case outcome of
            Left exception ->
                Left
                    ( "could not open transcript: "
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

requestMetaRestart
    :: Maybe FullscreenRuntime
    -> Persistence
    -> IO RunResult
requestMetaRestart fullscreen persist = do
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
                "restart the agent to apply Meta Console changes"
            pure RunQuit
        PersistenceEnabled slotRef -> do
            handle <- ensureSession slotRef
            report "restarting to apply Meta Console changes…"
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
