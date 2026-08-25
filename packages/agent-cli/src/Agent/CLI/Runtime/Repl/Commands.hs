-- | REPL submission and slash-command dispatch.
module Agent.CLI.Runtime.Repl.Commands
    ( handleReplLine
    , preparePromptSkillInputs
    ) where

import Agent.CLI.AccountPicker
    ( AccountPickerOption(..),
      accountPickerMatches,
      accountPickerRow,
      loadAllAccountPickerOptions )
import Agent.CLI.AccountSelection ()
import Agent.CLI.Afk
    ( AfkTarget(..), handoffLocal, handoffRemote, parseAfkTarget )
import Agent.CLI.AgentSessions ()
import Agent.CLI.AgentViewport
    ( AgentViewportEnv(viewportSelect, viewportEntries,
                       viewportSelected) )
import Agent.CLI.Approval ( toggleAlwaysApprove )
import Agent.CLI.Artifact ( fencedCodeBlock, lastDiffBlock )
import Agent.CLI.Auth ()
import Agent.CLI.Clipboard
    ( formatImageSize,
      loadImagesFromPastedText,
      nonEmptyClipboardImages,
      readClipboardImagesForPaste,
      readClipboardImagesImageFirst )
import Agent.CLI.Command
    ( currentEffort,
      currentModel,
      formatSlashHelpWithCatalog,
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
      SlashCatalog(slashCatalogToolNames) )
import Agent.CLI.Compaction
    ( CompactOutcome(compactSummary, compactBeforeTokens,
                     compactAfterTokens, compactHistory) )
import Agent.CLI.Config ()
import Agent.CLI.Connectivity ()
import Agent.CLI.Database ()
import Agent.CLI.Database.Store ()
import Agent.CLI.Dialects ()
import Agent.CLI.Error ( formatApiErrorInlineAt )
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
import Agent.CLI.Login ( connectProviderAccount, runLoginManager )
import Agent.CLI.Lsp ()
import Agent.CLI.ManagedTurn ()
import Agent.CLI.McpManager ( runMcpManager )
import Agent.CLI.McpStatus ()
import Agent.CLI.ModelConfig ()
import Agent.CLI.Models
    ( modelTargetRequiresRebuild,
      rawModelOption,
      resolveConfiguredModel,
      resolveModelOptionDialect,
      ModelOption(modelTarget),
      ModelTarget(targetDialect, ModelTarget, targetProvider,
                  targetConnectionId, targetModelId, targetWireModelId) )
import Agent.CLI.Options ( ApprovalPolicy )
import Agent.CLI.PendingInputs ()
import Agent.CLI.Plan ()
import Agent.CLI.Progress ()
import Agent.CLI.Project ()
import Agent.CLI.Prompt ()
import Agent.CLI.PromptHooks ()
import Agent.CLI.Provider.OpenAI ()
import Agent.CLI.Provider.Switch
    ( applyModelChange,
      reloadAuth,
      reportProviderUnavailable,
      requestAccountProviderSwitch,
      requestAutomaticProviderFallback,
      requestModelTargetSwitch )
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
import Agent.CLI.Runtime.HistorySource
    ( reloadFullscreenHistoryForHandle )
import Agent.CLI.Runtime.Persistence ()
import Agent.CLI.Runtime.Recap ( runSessionRecap )
import Agent.CLI.Runtime.Types
    ( RunResult(RunRestart, RunSwitchProvider, RunSwitchWorktree,
                RunReload, RunQuit) )
import Agent.CLI.Secret ()
import Agent.CLI.Session
    ( appendTurnKeepTitleIndexed,
      appendTurnWithMetaUpdateIndexed,
      createSession,
      ensureSession,
      loadSession,
      removeSessionTemp,
      resetSessionTitleToAuto,
      sessionConversationText,
      sessionsRoot,
      setManualSessionTitle,
      writeSessionMeta,
      Persistence(..),
      PersistenceState(PersistenceActive, PersistencePending),
      SessionCreate(createCwd, SessionCreate, createPool, createEffort,
                    createTarget, createTitleHint, createTitleIsManual, createRoot),
      SessionHandle(sessionMeta, sessionPool, sessionMetaPath,
                    sessionTempDir, sessionDir),
      SessionMeta(metaId, metaLastResponseId, metaUpdatedAt,
                  metaInputTokens, metaOutputTokens, metaCachedTokens, metaLastRecap,
                  metaLastTurnSummary, metaLastRecapMainTurns, metaTransportModel,
                  metaCwd, metaTitle),
      SessionTransfer(transferTurns, SessionTransfer, transferMeta),
      SessionTurn(turnUsage, SessionTurn, turnAt, turnUserText,
                  turnAssistantText, turnError, turnResponseId, turnEffect,
                  turnItems),
      TranscriptEffect(TranscriptReplace, TranscriptReset) )
import Agent.CLI.Session.Attachments
    ( putImagePreview, queueAttachedImages, queueClipboardImages )
import Agent.CLI.Session.Choices
    ( accountUsageText,
      atMay,
      effortChoice,
      modelChoice,
      showAccountUsage )
import Agent.CLI.Session.History
    ( modifyLiveAttachments, readLiveAttachments )
import Agent.CLI.Session.Interaction
    ( runBtwQuestion, setSessionEffort )
import Agent.CLI.Session.Lifecycle ()
import Agent.CLI.Session.Runtime.Types ()
import Agent.CLI.Session.Selection
    ( currentSessionId,
      handleConversationSearch,
      handleResume,
      pickAgentChoice )
import Agent.CLI.SessionAdmin ()
import Agent.CLI.SessionEnv ( SessionEnv(..) )
import Agent.CLI.SessionLock ()
import Agent.CLI.SessionState ()
import Agent.CLI.SessionTitle
    ( invalidateSessionTitles, requestSessionTitle )
import Agent.CLI.Skills ( formatSkillsListing )
import Agent.CLI.Startup.Auth ()
import Agent.CLI.Startup.Format ()
import Agent.CLI.StartupContext ()
import Agent.CLI.Status
    ( applyReplMode, cycleReplInteraction, formatTokenUsage )
import Agent.CLI.Style
    ( cliWindowTitle,
      glyphOk,
      glyphSession,
      roleError,
      roleMuted,
      roleSuccess )
import Agent.CLI.Subagents.Runtime ()
import Agent.CLI.TUI.App
    ( FullscreenRuntime,
      commitFullscreenHistoryTurn,
      emitUiEvent,
      requestFullscreenChoiceWithBody,
      setFullscreenImagePreviews,
      withFullscreenSuspended )
import Agent.CLI.TUI.SessionHistory (sessionHistoryTurn)
import Agent.CLI.TUI.Types (HistoryCommit(..))
import Agent.CLI.Terminal
    ( copyTerminalClipboard, formatTerminalCapabilities, resolveColor )
import Agent.CLI.Tools ()
import Agent.CLI.Turn ( runOneTurn )
import Agent.CLI.Usage ()
import Agent.CLI.WebFetch ()
import Agent.CLI.Worktree ( createWorktree, worktreeRoot )
import Agent.Cancel ()
import Agent.Claude ()
import Agent.Dialect ( dialectId, dialectSlug )
import Agent.Error ()
import Agent.GrokBuild.Dialect.Goal
    ( activateGoal,
      clearGoal,
      formatGoalSnapshot,
      pauseGoal,
      readGoal,
      resumeGoal )
import Agent.GrokBuild.Dialect.Runtime ( GrokRuntimeControl(..) )
import Agent.GrokBuild.Dialect.Workflow
    ( formatWorkflowRuns, workflowRunSnapshots )
import Agent.Loop
    ( TurnInput(UserMessage, UserMultimodal, userText, userImages),
      LoopEvent(ActivityUpdated),
      ImageAttachment(imageBytes, imageMime) )
import Agent.OpenAI.Compaction
    ( clearSessionUserText,
      compactSessionUserText,
      newSessionUserText )
import Agent.OpenAI.Usage ()
import Agent.OpenAI.WebSocketClient ()
import Agent.OpenRouter.LoopBackend ()
import Agent.OsPath ( toText )
import Agent.Provider
    ( Provider(ClaudeCodeProvider, OpenAIProvider),
      providerSlug,
      tokenProviderBillingMode )
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
      UiEvent(UiUserSubmitted, UiRecapStarted, UiConversationCleared,
              UiSetNotice, UiErrorMessage, UiSystemMessage) )
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
import Data.List ( findIndex )
import Data.Maybe ( isNothing, fromMaybe, listToMaybe )
import Data.Text ( Text )
import Data.Time.Clock ( getCurrentTime )
import System.Console.ANSI ()
import System.Console.ANSI.Codes ()
import System.Directory.OsPath ()
import System.Environment ()
import System.Exit ()
import System.IO ( stdout, hFlush, stderr )
import System.OsPath ( takeDirectory )
import System.Posix.Files ()
import qualified Data.ByteString as BS ( length )
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
import qualified Data.Set as Set ( toAscList )
import qualified Data.Text as Text
    ( intercalate, null, strip, unlines, pack )
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
            , sessionConnection = connectionId
            , sessionModelCatalog = catalog
            , sessionDialect = dialect
            , sessionParams = paramsRef
            , sessionPolicy = policyRef
            , sessionPersist = persist
            , sessionDatabasePool = databasePool
            , sessionPlanMode = planMode
            , sessionProjectRoot = projectRoot
            , sessionCwd = cwd
            , sessionTokenProvider = tokenProvider
            , sessionOpenAiPool = openAiPool
            , sessionSkills = skillsRef
            , sessionSkillInvocations = skillInvocationsRef
            , sessionRefreshSkills = refreshSkills
            , sessionGrokRuntime = grokRuntime
            , sessionDraft = draftRef
            , sessionPreviewId = previewIdRef
            , sessionStoreRoot = storeRoot
            , sessionUsage = usageRef
            , sessionAccountId = accountIdRef
            , sessionAccountSelectionId = selectionRef
            , sessionSelectAccount = selectAccount
            , sessionLastAssistant = lastAssistantRef
            , sessionTerminal = terminal
            , sessionFullscreen = fullscreen
            , sessionSetWindowTitle = setWindowTitle
            , sessionAgentViewport = agentViewport
            , sessionReset = sessionReset
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
    ReplClipboardPaste keptDraft clipboardPasteImages -> do
        case clipboardPasteImages of
            Just images@(_:_) -> do
                message <- queueAttachedImages
                    conversationRef
                    previewIdRef
                    stdoutColor
                    (isNothing fullscreen)
                    images
                syncFullscreenImagePreviews
                fullscreenEvent (UiSetNotice Nothing)
                displayInfo message $
                    Text.putStrLn
                        (roleMuted stdoutColor
                            (glyphOk <> message))
            _ ->
                queueClipboardImages
                    conversationRef
                    previewIdRef
                    stdoutColor
                    (isNothing fullscreen)
                    >>= \case
                        Left err ->
                            displayError err do
                                errColor <- resolveColor stderr
                                Text.hPutStrLn stderr (roleError errColor err)
                        Right message -> do
                            syncFullscreenImagePreviews
                            displayInfo message $
                                Text.putStrLn
                                    (roleMuted stdoutColor
                                        (glyphOk <> message))
        continueWith keptDraft
    ReplClipboardPasteOrText keptDraft pasted pastedDraft -> do
        pastedImages <- loadImagesFromPastedText pasted
        imagesResult <- case pastedImages of
            Just images@(_:_) -> pure (Just images)
            _ ->
                nonEmptyClipboardImages
                    <$> readClipboardImagesImageFirst
        case imagesResult of
            Just images -> do
                message <- queueAttachedImages
                    conversationRef
                    previewIdRef
                    stdoutColor
                    (isNothing fullscreen)
                    images
                syncFullscreenImagePreviews
                fullscreenEvent (UiSetNotice Nothing)
                displayInfo message $
                    Text.putStrLn
                        (roleMuted stdoutColor
                            (glyphOk <> message))
                continueWith keptDraft
            _ -> do
                fullscreenEvent (UiSetNotice Nothing)
                continueWith pastedDraft
    ReplChooseModel keptDraft -> do
        writeIORef draftRef keptDraft
        chooseModel (continueWith keptDraft)
    ReplChooseEffort keptDraft ->
        chooseEffort (continueWith keptDraft)
    ReplChooseAccount keptDraft -> do
        writeIORef draftRef keptDraft
        chooseAccount (continueWith keptDraft)
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
                                    setFullscreenImagePreviews runtime []
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
                                    setFullscreenImagePreviews runtime []
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
                    ReplPaste pasteImmediate pasteCaption -> do
                        color <- resolveColor stdout
                        errColor <- resolveColor stderr
                        imagesResult <- readClipboardImagesForPaste
                        case imagesResult of
                            Left err -> do
                                displayError err $
                                    Text.hPutStrLn stderr
                                        (roleError errColor err)
                                continue
                            Right [] -> do
                                displayError "no image found on the clipboard" $
                                    Text.hPutStrLn stderr
                                        (roleError errColor
                                            "no image found on the clipboard")
                                continue
                            Right images -> do
                                let sizes =
                                        Text.intercalate ", "
                                            [ img.imageMime <> " (" <> formatImageSize (BS.length img.imageBytes) <> ")"
                                            | img <- images
                                            ]
                                if pasteImmediate
                                    then do
                                        let promptText =
                                                if Text.null pasteCaption
                                                    then "See attached image."
                                                    else pasteCaption
                                        when (isNothing fullscreen) $
                                            putImagePreview previewIdRef color images
                                        displayInfo ("pasted " <> sizes) $
                                            Text.putStrLn
                                                (roleMuted color
                                                    (glyphOk <> "pasted " <> sizes))
                                        resetRenderPrintedText render
                                        fullscreenEvent
                                            (UiUserSubmitted promptText)
                                        let turnInputs =
                                                [ UserMultimodal
                                                    { userText = promptText
                                                    , userImages = images
                                                    }
                                                ]
                                        result <- runOneTurn env promptText turnInputs
                                        finishTurn False result
                                    else do
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
                    ReplShowAttachments -> do
                        pending <- readLiveAttachments conversationRef
                        color <- resolveColor stdout
                        let message =
                                if null pending
                                    then "attachments: (none)"
                                    else "attachments: "
                                        <> Text.intercalate ", "
                                            [ img.imageMime
                                                <> " ("
                                                <> formatImageSize
                                                    (BS.length img.imageBytes)
                                                <> ")"
                                            | img <- pending
                                            ]
                        displayInfo message $
                            Text.putStrLn
                                (roleMuted color (glyphSession <> message))
                        continue
                    ReplClearAttachments -> do
                        modifyLiveAttachments conversationRef (\_ -> ([], ()))
                        forM_ fullscreen \runtime ->
                            setFullscreenImagePreviews runtime []
                        color <- resolveColor stdout
                        displayInfo "attachments cleared" $
                            Text.putStrLn
                                (roleMuted color
                                    (glyphOk <> "attachments cleared"))
                        continue
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
                    ReplGoalStatus -> do
                        color <- resolveColor stdout
                        case grokRuntime of
                            Nothing ->
                                displayError
                                    "goal commands are unavailable in this session" $
                                    Text.hPutStrLn stderr
                                        (roleError color
                                            "goal commands are unavailable in this session")
                            Just control ->
                                readGoal control.grokGoalRuntime >>= \case
                                    Nothing ->
                                        displayInfo "No goal is active." $
                                            Text.putStrLn
                                                (roleMuted color
                                                    "No goal is active.")
                                    Just goal -> do
                                        let message =
                                                formatGoalSnapshot goal
                                        displayInfo message $
                                            Text.putStrLn
                                                (roleMuted color message)
                        continue
                    ReplGoalPause -> do
                        color <- resolveColor stderr
                        case grokRuntime of
                            Nothing ->
                                displayError
                                    "goal commands are unavailable in this session" $
                                    Text.hPutStrLn stderr
                                        (roleError color
                                            "goal commands are unavailable in this session")
                            Just control ->
                                pauseGoal control.grokGoalRuntime >>= \case
                                    Left err ->
                                        displayError err $
                                            Text.hPutStrLn stderr
                                                (roleError color err)
                                    Right goal -> do
                                        let message =
                                                "Goal paused.\n"
                                                    <> formatGoalSnapshot goal
                                        displayInfo message $
                                            Text.hPutStrLn stderr
                                                (roleMuted color message)
                        continue
                    ReplGoalResume -> do
                        color <- resolveColor stderr
                        case grokRuntime of
                            Nothing ->
                                displayError
                                    "goal commands are unavailable in this session" $
                                    Text.hPutStrLn stderr
                                        (roleError color
                                            "goal commands are unavailable in this session")
                            Just control ->
                                resumeGoal control.grokGoalRuntime >>= \case
                                    Left err ->
                                        displayError err $
                                            Text.hPutStrLn stderr
                                                (roleError color err)
                                    Right goal -> do
                                        let message =
                                                "Goal resumed.\n"
                                                    <> formatGoalSnapshot goal
                                        displayInfo message $
                                            Text.hPutStrLn stderr
                                                (roleMuted color message)
                        continue
                    ReplGoalClear -> do
                        color <- resolveColor stderr
                        case grokRuntime of
                            Nothing ->
                                displayError
                                    "goal commands are unavailable in this session" $
                                    Text.hPutStrLn stderr
                                        (roleError color
                                            "goal commands are unavailable in this session")
                            Just control -> do
                                cleared <-
                                    clearGoal control.grokGoalRuntime
                                let message =
                                        if cleared
                                            then "Goal cleared."
                                            else "No goal was active."
                                displayInfo message $
                                    Text.hPutStrLn stderr
                                        (roleMuted color message)
                        continue
                    ReplGoalSet original objective budget expanded ->
                        case grokRuntime of
                            Nothing -> do
                                color <- resolveColor stderr
                                let err =
                                        "goal commands are unavailable in this session"
                                displayError err $
                                    Text.hPutStrLn stderr
                                        (roleError color err)
                                continue
                            Just control ->
                                activateGoal
                                    control.grokGoalRuntime
                                    objective
                                    budget >>= \case
                                        Left err -> do
                                            color <- resolveColor stderr
                                            displayError err $
                                                Text.hPutStrLn stderr
                                                    (roleError color err)
                                            continue
                                        Right _ ->
                                            submitExpandedTurn
                                                continue
                                                color
                                                original
                                                expanded
                    ReplWorkflowRuns -> do
                        color <- resolveColor stdout
                        case grokRuntime >>= (.grokWorkflowRuntime) of
                            Nothing ->
                                displayError
                                    "workflow commands are unavailable in this session" $
                                    Text.hPutStrLn stderr
                                        (roleError color
                                            "workflow commands are unavailable in this session")
                            Just runtime -> do
                                runs <- workflowRunSnapshots runtime
                                let message = formatWorkflowRuns runs
                                displayInfo message $
                                    Text.putStrLn
                                        (roleMuted color message)
                        continue
                    ReplWorkflowManage operation target -> do
                        color <- resolveColor stderr
                        let err =
                                "workflow_management_unsupported: /workflow "
                                    <> operation
                                    <> maybe "" (" " <>) target
                                    <> " is not supported by this host; use /workflow runs to inspect tracked runs."
                        displayError err $
                            Text.hPutStrLn stderr
                                (roleError color err)
                        continue

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
                    ReplShowEffort -> do
                        color <- resolveColor stdout
                        params <- readIORef paramsRef
                        let message = "effort: " <> currentEffort params
                        displayInfo message $
                            Text.putStrLn
                                (roleMuted color (glyphSession <> message))
                        continue
                    ReplSetEffort level -> do
                        setEffort level
                        continue
                    ReplShowModel -> do
                        chooseModel continue
                    ReplSetModel name -> do
                        color <- resolveColor stdout
                        let rawChoice = rawModelOption provider name
                        choice <-
                            resolveModelOptionDialect $
                                fromMaybe
                                    (rawChoice
                                        { modelTarget =
                                            rawChoice.modelTarget
                                                { targetConnectionId = connectionId
                                                , targetDialect = dialectId dialect
                                                }
                                        })
                                    (resolveConfiguredModel catalog name)
                        if modelTargetRequiresRebuild
                                connectionId provider (dialectId dialect) choice
                            then
                                requestModelTargetSwitch
                                    fullscreen choice persist >>= \case
                                    Left err -> do
                                        displayError err $
                                            Text.hPutStrLn stderr
                                                (roleError color err)
                                        continue
                                    Right result -> pure result
                            else do
                                message <- applyModelChange
                                    projectRoot provider connectionId name
                                    choice.modelTarget.targetWireModelId
                                    choice.modelTarget.targetDialect
                                    paramsRef render conversationRef persist
                                displayInfo message $
                                    Text.putStrLn
                                        (roleMuted color (glyphOk <> message))
                                continue
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
                                fullscreenEvent UiConversationCleared
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
                    ReplResume maybeId -> do
                        handleResume databasePool fullscreen maybeId persist >>= \case
                            Nothing -> continue
                            Just result -> pure result
                    ReplSearch query -> do
                        handleConversationSearch
                            databasePool fullscreen query persist >>= \case
                                Nothing -> continue
                                Just result -> pure result
                    ReplClear -> do
                        sessionReset
                        fullscreenEvent UiConversationCleared
                        color <- resolveColor stderr
                        message <- case persist of
                            PersistenceDisabled ->
                                pure "conversation cleared"
                            PersistenceEnabled slotRef -> do
                                now <- getCurrentTime
                                slot <- readIORef slotRef
                                case slot of
                                    PersistencePending _ _ _ ->
                                        pure "conversation cleared"
                                    PersistenceActive handle -> do
                                        let turn = SessionTurn
                                                { turnAt = now
                                                , turnUserText = clearSessionUserText
                                                , turnAssistantText =
                                                    Just "Conversation cleared."
                                                , turnError = Nothing
                                                , turnResponseId = Nothing
                                                , turnEffect = TranscriptReset
                                                , turnItems = []
                                                , turnUsage = Nothing
                                                }
                                        (handle', turnIndex) <-
                                            appendTurnKeepTitleIndexed handle turn
                                        let meta = handle'.sessionMeta
                                                { metaLastResponseId = Nothing
                                                , metaUpdatedAt = now
                                                , metaInputTokens = 0
                                                , metaOutputTokens = 0
                                                , metaCachedTokens = 0
                                                , metaLastRecap = Nothing
                                                , metaLastTurnSummary = Nothing
                                                , metaLastRecapMainTurns = 0
                                                }
                                        writeSessionMeta
                                            handle'.sessionPool
                                            handle'.sessionMetaPath
                                            meta
                                        writeIORef slotRef
                                            (PersistenceActive handle'{sessionMeta = meta})
                                        forM_ fullscreen \runtime ->
                                            commitFullscreenHistoryTurn
                                                runtime
                                                (sessionHistoryTurn turnIndex turn)
                                                HistoryCommitReset
                                        pure
                                            ("conversation cleared (session "
                                                <> meta.metaId
                                                <> ")")
                        displayInfo message $
                            Text.hPutStrLn stderr
                                (roleMuted color (glyphOk <> message))
                        continue
                    ReplNew -> do
                        sessionReset
                        fullscreenEvent UiConversationCleared
                        color <- resolveColor stderr
                        case persist of
                            PersistenceDisabled -> do
                                displayInfo "started a fresh conversation" $
                                    Text.hPutStrLn stderr
                                        (roleMuted color
                                            (glyphOk
                                                <> "started a fresh conversation"))
                                continue
                            PersistenceEnabled slotRef -> do
                                params <- readIORef paramsRef
                                slot <- readIORef slotRef
                                let model = currentModel params
                                    effort = currentEffort params
                                    create = case slot of
                                        PersistencePending pending _ _ ->
                                            pending
                                                { createTarget =
                                                    pending.createTarget
                                                        { targetModelId = model }
                                                , createEffort = effort
                                                , createTitleHint = Nothing
                                                , createTitleIsManual = False
                                                }
                                        PersistenceActive handle ->
                                            SessionCreate
                                                { createPool = handle.sessionPool
                                                , createRoot =
                                                    takeDirectory handle.sessionDir
                                                , createTarget = ModelTarget
                                                    { targetProvider = provider
                                                    , targetConnectionId =
                                                        connectionId
                                                    , targetModelId = model
                                                    , targetWireModelId =
                                                        fromMaybe
                                                            model
                                                            handle.sessionMeta.metaTransportModel
                                                    , targetDialect =
                                                        dialectId dialect
                                                    }
                                                , createCwd =
                                                    handle.sessionMeta.metaCwd
                                                , createEffort = effort
                                                , createTitleHint = Nothing
                                                , createTitleIsManual = False
                                                }
                                handle <- createSession create
                                case slot of
                                    PersistencePending pending sessionId _ -> do
                                        _ <- removeSessionTemp
                                            pending.createRoot
                                            sessionId
                                        pure ()
                                    PersistenceActive _ -> pure ()
                                now <- getCurrentTime
                                let turn = SessionTurn
                                        { turnAt = now
                                        , turnUserText = newSessionUserText
                                        , turnAssistantText =
                                            Just "Started a new session."
                                        , turnError = Nothing
                                        , turnResponseId = Nothing
                                        , turnEffect = TranscriptReset
                                        , turnItems = []
                                        , turnUsage = Nothing
                                        }
                                (handle', _) <-
                                    appendTurnKeepTitleIndexed handle turn
                                let meta = handle'.sessionMeta
                                env.sessionOnPersisted handle'
                                env.sessionSetTempDir handle'.sessionTempDir
                                writeIORef slotRef
                                    (PersistenceActive handle')
                                writeIORef env.sessionTitleTurnCount 0
                                writeIORef planMode.planSessionDir
                                    (Just handle'.sessionDir)
                                writeIORef storeRoot (Just handle'.sessionDir)
                                forM_ fullscreen \runtime ->
                                    reloadFullscreenHistoryForHandle
                                        runtime
                                        handle'
                                setWindowTitle
                                    (cliWindowTitle meta.metaCwd
                                        (Just meta.metaTitle))
                                let message = "new session: " <> meta.metaId
                                displayInfo message $
                                    Text.hPutStrLn stderr
                                        (roleMuted color
                                            (glyphOk <> message))
                                continue
                    ReplShowSession -> do
                        color <- resolveColor stdout
                        case persist of
                            PersistenceDisabled ->
                                displayInfo "session: (not persisted)" $
                                    Text.putStrLn
                                        (roleMuted color
                                            "session: (not persisted)")
                            PersistenceEnabled slotRef -> do
                                slot <- readIORef slotRef
                                case slot of
                                    PersistencePending _ _ _ ->
                                        displayInfo
                                            "session: (pending until first turn)" $
                                            Text.putStrLn
                                                (roleMuted color
                                                    "session: (pending until first turn)")
                                    PersistenceActive handle ->
                                        let message =
                                                "session: "
                                                    <> handle.sessionMeta.metaId
                                        in displayInfo message $
                                            Text.putStrLn
                                                (roleMuted color
                                                    (glyphSession <> message))
                        continue
                    ReplShowSessionInfo -> do
                        color <- resolveColor stdout
                        params <- readIORef paramsRef
                        usage <- readIORef usageRef
                        shellMode <- env.sessionShellMode
                        (persistenceState, sessionId, sessionTitle) <-
                            case persist of
                                PersistenceDisabled ->
                                    pure ("not_persisted", Nothing, Nothing)
                                PersistenceEnabled slotRef -> do
                                    slot <- readIORef slotRef
                                    pure $ case slot of
                                        PersistencePending _ pendingId _ ->
                                            ("pending", Just pendingId, Nothing)
                                        PersistenceActive handle ->
                                            ( "active"
                                            , Just handle.sessionMeta.metaId
                                            , Just handle.sessionMeta.metaTitle
                                            )
                        let toolNames =
                                Set.toAscList
                                    slashCatalog.slashCatalogToolNames
                            usageText =
                                let formatted = formatTokenUsage usage
                                in if Text.null formatted
                                    then "0 in · 0 out"
                                    else formatted
                            message = Text.unlines $
                                [ "session: "
                                    <> fromMaybe "(not persisted)" sessionId
                                , "state: " <> persistenceState
                                ]
                                    <> maybe
                                        []
                                        (\title -> ["title: " <> title])
                                        sessionTitle
                                    <> [ "provider: " <> providerSlug provider
                                       , "connection: " <> connectionId
                                       , "model: " <> currentModel params
                                       , "dialect: "
                                            <> dialectSlug
                                                (dialectId dialect)
                                       , "effort: " <> currentEffort params
                                       , "cwd: " <> toText cwd
                                       , "shell: "
                                            <> shellModeText shellMode
                                       , "tokens: " <> usageText
                                       , "tools: "
                                            <> if null toolNames
                                                then "(none)"
                                                else
                                                    Text.intercalate
                                                        ", "
                                                        toolNames
                                       ]
                        displayInfo message $
                            Text.putStrLn (roleMuted color message)
                        continue
                    ReplAfk rawTarget -> do
                        let failAfk err = do
                                color <- resolveColor stderr
                                displayError err $
                                    putTextLn stderr (roleError color err)
                                continue
                            finishAfk message = do
                                color <- resolveColor stderr
                                displayInfo message $
                                    putTextLn stderr
                                        (roleMuted color (glyphOk <> message))
                                pure RunQuit
                        case parseAfkTarget rawTarget of
                            Left err -> failAfk err
                            Right target -> case persist of
                                PersistenceDisabled ->
                                    failAfk "/afk requires a persisted interactive session"
                                PersistenceEnabled slotRef ->
                                    readIORef slotRef >>= \case
                                        PersistencePending _ _ _ ->
                                            failAfk
                                                "/afk is available after the first persisted turn"
                                        PersistenceActive handle ->
                                            case target of
                                                AfkLocal ->
                                                    handoffLocal
                                                        handle.sessionMeta.metaId
                                                        cwd >>= \case
                                                            Left err -> failAfk err
                                                            Right message ->
                                                                finishAfk message
                                                AfkRemote host path ->
                                                    loadSession
                                                        databasePool
                                                        (sessionsRoot env.sessionHome)
                                                        handle.sessionMeta.metaId
                                                        >>= \case
                                                            Left err -> failAfk err
                                                            Right (meta, turns) ->
                                                                handoffRemote
                                                                    host
                                                                    path
                                                                    handle.sessionDir
                                                                    SessionTransfer
                                                                        { transferMeta = meta
                                                                        , transferTurns = turns
                                                                        }
                                                                    >>= \case
                                                                        Left err -> failAfk err
                                                                        Right message ->
                                                                            finishAfk message
                    ReplWorktree -> do
                        result <- withReplActivity "Creating worktree…" $
                            createWorktree cwd (worktreeRoot env.sessionHome)
                        case result of
                            Left err -> do
                                color <- resolveColor stderr
                                displayError err $
                                    putTextLn stderr (roleError color err)
                                continue
                            Right path -> do
                                color <- resolveColor stderr
                                params <- readIORef paramsRef
                                let message = "worktree: " <> toText path
                                displayInfo message $
                                    putTextLn stderr
                                        (roleMuted color
                                            (glyphSession <> message))
                                pure
                                    (RunSwitchWorktree
                                        path
                                        provider
                                        (currentModel params)
                                        (currentEffort params))
                    ReplRename title -> do
                        color <- resolveColor stderr
                        case persist of
                            PersistenceDisabled ->
                                displayError
                                    "cannot rename a session that is not persisted" $
                                    putTextLn stderr
                                        (roleError color
                                            "cannot rename a session that is not persisted")
                            PersistenceEnabled slotRef ->
                                readIORef slotRef >>= \case
                                    PersistencePending pending sessionId tempDir -> do
                                        writeIORef slotRef
                                            (PersistencePending
                                                pending
                                                    { createTitleHint = Just title
                                                    , createTitleIsManual = True
                                                    }
                                                sessionId
                                                tempDir)
                                        setWindowTitle
                                            (cliWindowTitle pending.createCwd
                                                (Just title))
                                        let message = "session title: " <> title
                                        displayInfo message $
                                            putTextLn stderr
                                                (roleMuted color
                                                    (glyphOk <> message))
                                    PersistenceActive handle -> do
                                        invalidateSessionTitles
                                            env.sessionTitleManager
                                            handle.sessionMeta.metaId
                                        updated <- setManualSessionTitle title handle
                                        writeIORef slotRef (PersistenceActive updated)
                                        setWindowTitle
                                            (cliWindowTitle updated.sessionMeta.metaCwd
                                                (Just updated.sessionMeta.metaTitle))
                                        let message =
                                                "session title: "
                                                    <> updated.sessionMeta.metaTitle
                                        displayInfo message $
                                            putTextLn stderr
                                                (roleMuted color
                                                    (glyphOk <> message))
                        continue
                    ReplRenameAuto -> do
                        color <- resolveColor stderr
                        case persist of
                            PersistenceDisabled ->
                                displayError
                                    "cannot rename a session that is not persisted" $
                                    putTextLn stderr
                                        (roleError color
                                            "cannot rename a session that is not persisted")
                            PersistenceEnabled slotRef ->
                                readIORef slotRef >>= \case
                                    PersistencePending pending sessionId tempDir -> do
                                        writeIORef slotRef
                                            (PersistencePending
                                                pending
                                                    { createTitleHint = Nothing
                                                    , createTitleIsManual = False
                                                    }
                                                sessionId
                                                tempDir)
                                        setWindowTitle
                                            (cliWindowTitle pending.createCwd Nothing)
                                        displayInfo
                                            "automatic session titles enabled" $
                                            putTextLn stderr
                                                (roleMuted color
                                                    (glyphOk
                                                        <> "automatic session titles enabled"))
                                    PersistenceActive handle -> do
                                        invalidateSessionTitles
                                            env.sessionTitleManager
                                            handle.sessionMeta.metaId
                                        updated <- resetSessionTitleToAuto handle
                                        writeIORef slotRef (PersistenceActive updated)
                                        loadSession
                                            updated.sessionPool
                                            (takeDirectory updated.sessionDir)
                                            updated.sessionMeta.metaId
                                            >>= \case
                                                Left _ -> pure ()
                                                Right (_, turns) -> do
                                                    let source =
                                                            sessionConversationText turns
                                                    requestSessionTitle
                                                        env.sessionTitleManager
                                                        updated.sessionMeta.metaId
                                                        1
                                                        source
                                        displayInfo
                                            "automatic session titles enabled" $
                                            putTextLn stderr
                                                (roleMuted color
                                                    (glyphOk
                                                        <> "automatic session titles enabled"))
                        continue
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
            setFullscreenImagePreviews runtime []
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
    shellModeText = \case
        ShellGhci -> "ghci"
        ShellBash -> "bash"
        ShellBoth -> "ghci + bash"
        ShellNone -> "none"
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
    setEffort level = do
        color <- resolveColor stdout
        setSessionEffort env level
        displayInfo ("effort set to " <> level) $
            Text.putStrLn
                (roleMuted color
                    (glyphOk <> "effort set to " <> level))
    chooseEffort next = do
        params <- readIORef paramsRef
        effortChoice fullscreen (currentEffort params) >>= \case
            Nothing -> next
            Just level -> setEffort level >> next
    chooseModel next = do
        color <- resolveColor stderr
        params <- readIORef paramsRef
        let current = currentModel params
        modelChoice
            catalog fullscreen color connectionId provider current
                (dialectId dialect) >>= \case
            Nothing -> next
            Just rawChoice -> do
                choice <- resolveModelOptionDialect rawChoice
                if choice.modelTarget.targetProvider == provider
                    && choice.modelTarget.targetConnectionId == connectionId
                    && choice.modelTarget.targetModelId == current
                    && choice.modelTarget.targetDialect == dialectId dialect
                  then do
                    let message =
                            "model: "
                                <> connectionId
                                <> "/"
                                <> choice.modelTarget.targetModelId
                    displayInfo message $
                        Text.putStrLn
                            (roleMuted color
                                (glyphSession <> message))
                    next
                  else if not
                        (modelTargetRequiresRebuild
                            connectionId provider (dialectId dialect) choice)
                  then do
                    message <- applyModelChange
                        projectRoot provider connectionId
                        choice.modelTarget.targetModelId
                        choice.modelTarget.targetWireModelId
                        choice.modelTarget.targetDialect
                        paramsRef render conversationRef persist
                    displayInfo message $
                        Text.putStrLn
                            (roleMuted color
                                (glyphOk <> message))
                    next
                  else
                    requestModelTargetSwitch fullscreen choice persist >>= \case
                        Left err -> do
                            displayError err $
                                Text.hPutStrLn stderr
                                    (roleError color err)
                            next
                        Right result -> pure result
    chooseAccount next =
        case fullscreen of
            Just runtime -> do
                currentSelectionId <- readIORef selectionRef
                currentAccountId <- readIORef accountIdRef
                options <- withReplActivity
                    "Loading account usage…"
                    (loadAllAccountPickerOptions provider)
                let initial =
                        fromMaybe 0 $
                            findIndex
                                (accountPickerMatches
                                    provider
                                    currentSelectionId
                                    currentAccountId)
                                options
                requestFullscreenChoiceWithBody
                    runtime
                    "Accounts"
                    "Choose any account. Switching provider also switches to its default model."
                    initial
                    (map
                        (accountPickerRow
                            provider
                            currentSelectionId
                            currentAccountId)
                        options)
                    >>= \case
                        Just index
                            | Just option <- atMay index options ->
                                case option of
                                    AccountPickerAccount
                                        selectedProvider
                                        selectedBilling
                                        selectedSelectionId
                                        selectedAccountId
                                        selectedLabel
                                        _
                                            -- Claude exposes display metadata,
                                            -- not a stable account identity.
                                            -- Revalidate and restart even when
                                            -- the synthetic id still matches.
                                            | selectedProvider == provider
                                            , selectedProvider
                                                /= ClaudeCodeProvider
                                            , selectedAccountId
                                                == currentAccountId ->
                                                displayInfo
                                                    ("account: " <> selectedLabel)
                                                    (pure ())
                                                    >> next
                                            | otherwise ->
                                                chooseSelectedAccount
                                                    selectedProvider
                                                    selectedBilling
                                                    selectedSelectionId
                                                    selectedAccountId
                                                    selectedLabel
                                    AccountPickerConnect selectedProvider -> do
                                        color <- resolveColor stderr
                                        connected <-
                                            withFullscreenSuspended runtime $
                                                connectProviderAccount
                                                    color
                                                    selectedProvider
                                        case connected of
                                            Nothing -> next
                                            Just selectedAccountId -> do
                                                refreshed <-
                                                    loadAllAccountPickerOptions
                                                        provider
                                                case listToMaybe
                                                        [ account
                                                        | account@(AccountPickerAccount
                                                            accountProvider
                                                            _
                                                            _
                                                            accountId
                                                            _
                                                            _) <- refreshed
                                                        , accountProvider
                                                            == selectedProvider
                                                        , accountId
                                                            == selectedAccountId
                                                        ] of
                                                    Just
                                                        (AccountPickerAccount
                                                            accountProvider
                                                            billing
                                                            selectionId
                                                            accountId
                                                            label
                                                            _) ->
                                                        chooseSelectedAccount
                                                            accountProvider
                                                            billing
                                                            selectionId
                                                            accountId
                                                            label
                                                    _ -> do
                                                        displayError
                                                            "Connected account could not be loaded."
                                                            (pure ())
                                                        next
                        _ -> next
              where
                currentBilling =
                    tokenProviderBillingMode
                        <$> tokenProvider
                chooseSelectedAccount
                    selectedProvider
                    selectedBilling
                    selectedSelectionId
                    selectedAccountId
                    selectedLabel
                        | selectedProvider == provider
                        , Just selectedBilling == currentBilling
                        , Just select <- selectAccount =
                            let liveSelectionId =
                                    case selectedProvider of
                                        OpenAIProvider -> selectedAccountId
                                        _ -> selectedSelectionId
                            in select liveSelectionId >>= \case
                                Left err -> do
                                    now <- getCurrentTime
                                    let message =
                                            "could not select account: "
                                                <> formatApiErrorInlineAt
                                                    now
                                                    err
                                    displayError message (pure ())
                                    next
                                Right label -> do
                                    displayInfo
                                        ("account switched to " <> label)
                                        (pure ())
                                    next
                        | otherwise =
                            readIORef paramsRef >>= \params ->
                                requestAccountProviderSwitch
                                    catalog fullscreen provider connectionId
                                    (currentModel params) (dialectId dialect)
                                    selectedProvider selectedSelectionId
                                    selectedAccountId persist >>= \case
                                        Left err -> do
                                            displayError err (pure ())
                                            next
                                        Right result -> do
                                            displayInfo
                                                ("switching to "
                                                    <> selectedLabel
                                                    <> " ("
                                                    <> providerSlug
                                                        selectedProvider
                                                    <> ")")
                                                (pure ())
                                            pure result
            Nothing -> do
                displayError
                    "Account switching is unavailable for this session."
                    (pure ())
                next
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
