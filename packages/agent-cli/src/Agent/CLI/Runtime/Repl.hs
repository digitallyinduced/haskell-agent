-- | Interactive prompt loop and session continuation wiring.
module Agent.CLI.Runtime.Repl
    ( repl
    , replWithDraft
    , runPendingTurn
    , finishTurn
    , preparePromptSkillInputs
    , preparePromptSkillInputsWithPaste
    ) where

import Agent.CLI.AccountPicker ()
import Agent.CLI.AccountSelection ()
import Agent.CLI.Afk ()
import Agent.CLI.AgentSessions ()
import Agent.CLI.AgentViewport
    ( renderAgentViewportPanelFor,
      AgentViewportEnv(viewportSelected, viewportEntries) )
import Agent.CLI.Approval ()
import Agent.CLI.Artifact ()
import Agent.CLI.Auth ()
import Agent.CLI.Clipboard ()
import Agent.CLI.Command
    ( currentEffort, currentModel, mkSlashCatalog )
import Agent.ReasoningEffort (reasoningEffortText)
import Agent.CLI.Compaction ()
import Agent.CLI.Config ()
import Agent.Connectivity ()
import Agent.CLI.Database ()
import Agent.CLI.Database.Store ()
import Agent.CLI.Dialects ()
import Agent.CLI.Error ()
import Agent.CLI.Dictation
    ( dictationTargetForSession )
import Agent.CLI.GatewayClient
    ( cachedGatewayModels
    , fetchGatewayUsage
    , gatewayModelIds
    )
import Agent.CLI.GatewayBridge ()
import Agent.CLI.Input
    ( readReplLineWithCatalogForProvider
    , readReplLineWithCatalogForTarget
    )
import Agent.CLI.Interrupt ()
import Agent.CLI.LearnedSkills ()
import Agent.CLI.LearnedSkills.Store ()
import Agent.CLI.Login ()
import Agent.CLI.Lsp ()
import Agent.CLI.ManagedTurn ()
import Agent.CLI.McpManager ()
import Agent.CLI.McpStatus ()
import Agent.CLI.ModelConfig ()
import Agent.OpenAI.Models.Types (ModelInfo(..), modelServiceTierForRequest)
import Agent.CLI.Models ( catalogModelIds )
import Agent.CLI.Options ()
import Agent.CLI.PendingInputs ()
import Agent.CLI.SteeringInputs
    ( awaitBackgroundCompletion
    , hasBackgroundCompletions
    )
import Agent.CLI.Plan ()
import Agent.CLI.Progress ()
import Agent.CLI.Project ()
import Agent.CLI.Prompt ()
import Agent.CLI.PromptHooks ()
import Agent.CLI.Provider.OpenAI ()
import Agent.CLI.Provider.Switch
    ( reportProviderUnavailable, requestStartupProviderFallback )
import Agent.CLI.ProviderAvailability ()
import Agent.CLI.ProviderFallback ()
import Agent.CLI.ProviderTransition ( PendingTurn, TurnResult )
import Agent.CLI.Recap ()
import Agent.CLI.Render
    ( RenderConfig(..)
    , stateLastTokensPerSecond
    )
import Agent.CLI.ReplMode
    ( replModeFromState, ReplMode(ReplModeAlwaysApprove) )
import Agent.CLI.Request ()
import Agent.CLI.Runtime.Persistence ()
import Agent.CLI.Runtime.Recap ()
import Agent.CLI.Runtime.Repl.Commands
    ( handleReplLine
    , preparePromptSkillInputs
    , preparePromptSkillInputsWithPaste
    )
import Agent.CLI.Runtime.Types
    ( PendingTurnPresentation
    , RunResult(RunSwitchProvider)
    )
import Agent.CLI.Secret ()
import Agent.CLI.Session ()
import Agent.CLI.Session.Attachments ()
import Agent.CLI.Session.Choices ()
import Agent.CLI.Session.History ( readLiveAttachments )
import Agent.CLI.Session.Interaction
    ( buildPromptState
    , syncFullscreenContext
    )
import Agent.CLI.Session.Lifecycle ( SessionContinuation(..) )
import Agent.CLI.Session.Runtime.Types ()
import Agent.CLI.Session.Selection ()
import Agent.CLI.SessionAdmin ()
import Agent.CLI.SessionEnv ( SessionEnv(..) )
import Agent.CLI.SessionLock ()
import Agent.CLI.SessionState ()
import Agent.CLI.SessionTitle ()
import Agent.CLI.Skills ( skillInvocationCommand )
import Agent.CLI.Startup.Auth ()
import Agent.CLI.Startup.Format ()
import Agent.CLI.StartupContext ()
import Agent.CLI.Status ( formatReplStatusLine )
import Agent.CLI.Style
    ( beginBackground,
      endBackground,
      roleMuted,
      rolePrompt,
      roleSuccess,
      roleWarn,
      userBackground )
import Agent.CLI.Subagents.Runtime ()
import Agent.CLI.TUI.App
    ( emitUiEvent,
      readFullscreenLineOrWithCatalog,
      setFullscreenImagePreviews )
import Agent.CLI.Terminal
    ( emitTerminalSequence,
      osc133PromptEnd,
      osc133PromptStart,
      resolveColor,
      withSynchronizedOutput,
      TerminalCapabilities(terminalSemanticPrompts) )
import Agent.CLI.Tools ()
import Agent.CLI.Turn (runOneTurn)
import Agent.CLI.Usage
    ( formatGrokLimitStatus,
      formatOpenAiLimitStatus,
      formatOpenRouterLimitStatus )
import Agent.CLI.WebFetch ()
import Agent.CLI.Worktree ()
import Agent.Cancel ()
import Agent.Claude ()
import Agent.Dialect ( dialectId )
import Agent.Error (ApiError)
import Agent.GrokBuild.Dialect.Goal ()
import Agent.GrokBuild.Dialect.Runtime ()
import Agent.GrokBuild.Dialect.Workflow ()
import Agent.Loop ( emptyTokenUsage )
import Agent.OpenAI.Compaction ()
import Agent.OpenAI.Usage ( fetchUsage )
import Agent.OpenAI.WebSocketClient ()
import Agent.OpenRouter.LoopBackend ()
import Agent.OsPath ()
import Agent.Provider
    ( Provider(OpenRouterProvider, XAIProvider, OpenAIProvider),
      Credential(accessToken, accountId),
      getNextToken,
      tokenProviderBillingMode,
      BillingMode(SubscriptionBilled) )
import Agent.Responses.GenericBackend ()
import Agent.Responses.GenericClient ()
import Agent.Responses.Types ()
import Agent.Skills
    ( SkillInvocation(invocationSkill), Skill(skillUserInvocable) )
import Agent.Store.Postgres ()
import Agent.Store.Types ()
import Agent.Subagents ()
import Agent.Subagents.TaskPath ()
import Agent.TUI.Model
    ( UiEvent(UiSetPromptLimitStatus, UiSystemMessage) )
import Agent.TUI.Motion ()
import Agent.ToolDispatch ()
import Agent.Tools.MultiAgents ()
import Agent.Tools.PlanMode
    ( PlanModeEnv(planStateRef),
      PlanModeState(PlanPending, PlanActive) )
import Agent.Tools.Secret ()
import Agent.Tools.Types ()
import Agent.XAI.LoopBackend ()
import Control.Applicative ()
import Control.Concurrent.Async ( withAsync )
import Control.Concurrent.Chan ()
import Control.Concurrent.MVar ( withMVar )
import Control.Concurrent.STM (orElse, retry)
import Control.Exception ()
import Control.Exception.Safe ()
import Control.Monad ( when, forM_ )
import Data.IORef ( readIORef, writeIORef )
import Data.List ()
import Data.Maybe ( fromMaybe, isJust )
import Data.Text ( Text )
import Data.Time.Clock ()
import System.Console.ANSI ( getTerminalSize )
import System.Console.ANSI.Codes ( clearFromCursorToLineEndCode )
import System.Directory.OsPath ()
import System.Environment ()
import System.Exit ()
import System.IO ( stdout, hFlush )
import System.OsPath ()
import System.Posix.Files ()
import qualified Data.ByteString as BS ()
import qualified Agent.Responses.GenericClient as GenericResponses
    ()
import qualified Agent.MCP as MCP ()
import qualified Data.Map.Strict as Map ()
import qualified Agent.OpenAI.Auth as OpenAI ()
import qualified Agent.OpenRouter as OpenRouter ()
import qualified Agent.OpenRouter.Usage as OpenRouterUsage
    ( fetchOpenRouterUsage )
import qualified Agent.Provider as Provider ()
import qualified Agent.CLI.Session.Lifecycle as SessionLifecycle
    ( finishTurn, retryFailedTurn, runPendingTurn )
import qualified Agent.CLI.Session.Runner as SessionRunner ()
import qualified Data.Set as Set ()
import qualified Data.Text as Text ( null, strip, pack )
import qualified Data.Text.IO as Text ( putStr, putStrLn )
import qualified Agent.XAI.Options as XAI ()
import qualified Agent.XAI.Usage as XAIUsage ( fetchGrokUsage )

sessionContinuation :: SessionContinuation
sessionContinuation =
    SessionContinuation
        { resumeSession = repl
        , resumeSessionWithDraft = replWithDraft
        }

data ReplWake
    = ProviderUnavailableWake !ApiError
    | BackgroundCompletionWake

runPendingTurn
    :: PendingTurnPresentation
    -> SessionEnv
    -> PendingTurn
    -> IO RunResult
runPendingTurn = SessionLifecycle.runPendingTurn sessionContinuation

finishTurn
    :: SessionEnv
    -> Bool
    -> TurnResult
    -> IO RunResult
finishTurn = SessionLifecycle.finishTurn sessionContinuation

repl :: SessionEnv -> IO RunResult
repl env = replWithDraft env ""

replWithDraft :: SessionEnv -> Text -> IO RunResult
replWithDraft env@SessionEnv
    { sessionRender = render
    , sessionConversation = conversationRef
    , sessionProvider = provider
    , sessionModelCatalog = catalog
    , sessionGatewayModels = gatewayModelsRef
    , sessionDialect = dialect
    , sessionStartupUnavailable = startupUnavailableRef
    , sessionParams = paramsRef
    , sessionPolicy = policyRef
    , sessionPlanMode = planMode
    , sessionTokenProvider = tokenProvider
    , sessionSkillInvocations = skillInvocationsRef
    , sessionRefreshSkills = refreshSkills
    , sessionActiveToolNames = readActiveToolNames
    , sessionDraft = draftRef
    , sessionInterrupt = interrupt
    , sessionUsage = usageRef
    , sessionAccount = accountRef
    , sessionSelectAccount = selectAccount
    , sessionTerminal = terminal
    , sessionFullscreen = fullscreen
    , sessionAgentViewport = agentViewport
    } draft = do
    writeIORef draftRef draft
    refreshSkills False
    skillInvocations <- readIORef skillInvocationsRef
    let skillCommands =
            map skillInvocationCommand
                (filter (.invocationSkill.skillUserInvocable) skillInvocations)
    activeToolNames <- readActiveToolNames
    params <- readIORef paramsRef
    gatewayAccess <- readIORef gatewayModelsRef
    modelIds <- case gatewayAccess of
        Nothing -> pure (catalogModelIds catalog)
        Just access ->
            maybe [] gatewayModelIds <$> cachedGatewayModels access
    let slashCatalog =
            mkSlashCatalog
                (maybe False (\info ->
                    info.slug == currentModel params
                        && modelServiceTierForRequest info (Just "priority")
                            == Just "priority")
                    env.sessionModelInfo)
                (dialectId dialect) activeToolNames skillCommands
                modelIds
    stdoutColor <- resolveColor stdout
    planState <- readIORef planMode.planStateRef
    let planActive = planState == PlanActive
        planPending = planState == PlanPending
    policy <- readIORef policyRef
    pendingAttachments <- readLiveAttachments conversationRef
    let idleMode = replModeFromState planState policy
    usage <- readIORef usageRef
    account <- readIORef accountRef
    mlineResult <- case fullscreen of
        Just runtime -> do
            syncFullscreenContext env
            setFullscreenImagePreviews runtime pendingAttachments
            let promptState =
                    buildPromptState
                        (dialectId dialect)
                        params
                        planState
                        policy
                        account
                        (isJust selectAccount)
                        usage
                        (length pendingAttachments)
                readPrompt = do
                    startupUnavailable <- readIORef startupUnavailableRef
                    failedTurn <- readIORef env.sessionLastFailedTurn
                    let backgroundWake =
                            case failedTurn of
                                -- Preserve the user's retry candidate. Its
                                -- next retry or replacement turn will consume
                                -- the queued completion through normal
                                -- steering.
                                Just _ -> retry
                                Nothing ->
                                    BackgroundCompletionWake
                                        <$ awaitBackgroundCompletion
                                            env.sessionSteeringInputs
                        wake = case startupUnavailable of
                            Nothing -> backgroundWake
                            Just unavailable ->
                                (ProviderUnavailableWake <$> unavailable)
                                    `orElse` backgroundWake
                    readFullscreenLineOrWithCatalog
                        runtime
                        slashCatalog
                        promptState
                        draft
                        wake
            withAsync
                (refreshAccountLimit
                    gatewayAccess
                    (currentModel params)
                    runtime)
                \_ ->
                readPrompt
        Nothing -> Right <$> withMVar render.renderLock \_ -> do
            -- The inline editor redraws its ANSI frame with several writes.
            -- Keep the renderer out for the complete prompt lifetime so a
            -- late tool event cannot be spliced into the composer row.
            when terminal.terminalSemanticPrompts $
                emitTerminalSequence terminal stdout osc133PromptStart
            termCols <- fmap snd <$> getTerminalSize
            case agentViewport of
                Nothing -> pure ()
                Just viewport -> do
                    entries <- viewport.viewportEntries
                    selected <- readIORef viewport.viewportSelected
                    let panel =
                            renderAgentViewportPanelFor
                                stdoutColor
                                (fromMaybe 100 termCols)
                                selected
                                entries
                    when (not (Text.null panel)) (Text.putStrLn panel)
            -- Status sits on the line above λ in minimal mode.
            withSynchronizedOutput terminal stdout do
                savedRate <-
                    stateLastTokensPerSecond <$> readIORef render.renderState
                let tokenRate
                        | usage == emptyTokenUsage = Nothing
                        | otherwise = savedRate
                Text.putStrLn $ formatReplStatusLine stdoutColor termCols
                    (currentModel params)
                    (reasoningEffortText (currentEffort params))
                    idleMode
                    account
                    usage
                    tokenRate
                hFlush stdout
            let modeTag
                    | planActive = roleWarn stdoutColor "[plan] "
                    | planPending = roleMuted stdoutColor "[plan…] "
                    | idleMode == ReplModeAlwaysApprove =
                        roleSuccess stdoutColor "[yolo] "
                    | otherwise = ""
                chromePrompt =
                    beginBackground stdoutColor userBackground
                        <> modeTag
                        <> if null pendingAttachments
                            then ""
                            else roleMuted stdoutColor
                                ("[📎 " <> Text.pack (show (length pendingAttachments)) <> "] ")
                        <> rolePrompt stdoutColor "λ "
                        <> if stdoutColor
                            then Text.pack clearFromCursorToLineEndCode
                            else mempty
            result <- case gatewayAccess of
                Just gateway ->
                    readReplLineWithCatalogForTarget
                        (dictationTargetForSession
                            env.sessionProvider
                            (Just gateway))
                        slashCatalog
                        interrupt chromePrompt draft
                Nothing ->
                    readReplLineWithCatalogForProvider
                        provider
                        slashCatalog
                        interrupt chromePrompt draft
            when terminal.terminalSemanticPrompts $
                emitTerminalSequence terminal stdout osc133PromptEnd
            Text.putStr (endBackground stdoutColor)
            hFlush stdout
            pure result
    case mlineResult of
        Left BackgroundCompletionWake -> do
            pending <-
                hasBackgroundCompletions env.sessionSteeringInputs
            if not pending
                then replWithDraft env draft
                else do
                    forM_ fullscreen \runtime ->
                        emitUiEvent runtime
                            (UiSystemMessage
                                "Background task completed; resuming the agent.")
                    -- Keep the notice in the normal steering queue so it is
                    -- acknowledged only after the provider commits it.
                    result <- runOneTurn env "" []
                    finishTurn env False result
        Left (ProviderUnavailableWake apiError) -> do
            -- The startup check is one-shot. If no fallback account is usable,
            -- leave request-time error handling in charge of later submits.
            writeIORef startupUnavailableRef Nothing
            requestStartupProviderFallback env apiError >>= \case
                Just providerTransition ->
                    pure (RunSwitchProvider providerTransition)
                Nothing -> do
                    reportProviderUnavailable fullscreen apiError
                    replWithDraft env ""
        Right mline -> do
            -- Any user action wins the startup race. In particular, a prompt
            -- already submitted while the preflight was running proceeds on
            -- the selected provider and leaves request-time fallback in charge.
            writeIORef startupUnavailableRef Nothing
            handleReplLine
                env
                (replWithDraft env)
                (finishTurn env)
                (SessionLifecycle.retryFailedTurn sessionContinuation env)
                slashCatalog
                skillInvocations
                stdoutColor
                planState
                policy
                mline
  where
    refreshAccountLimit gatewayAccess model runtime =
        case (gatewayAccess, provider, tokenProvider) of
            (Just access, _, _) ->
                fetchGatewayUsage access model >>= \case
                    Left _ -> pure ()
                    Right snapshot ->
                        publish (formatOpenAiLimitStatus snapshot)
            (Nothing, XAIProvider, Just tokens)
                | tokenProviderBillingMode tokens == SubscriptionBilled ->
                    refreshWith
                        tokens
                        XAIUsage.fetchGrokUsage
                        formatGrokLimitStatus
            (Nothing, OpenAIProvider, Just tokens)
                | tokenProviderBillingMode tokens == SubscriptionBilled ->
                    getNextToken tokens Nothing >>= \case
                        Left _ -> pure ()
                        Right credential
                            | Text.null (Text.strip credential.accountId) ->
                                pure ()
                            | otherwise ->
                                fetchUsage
                                    credential.accessToken
                                    credential.accountId >>= \case
                                        Left _ -> pure ()
                                        Right snapshot ->
                                            publish
                                                (formatOpenAiLimitStatus snapshot)
            (Nothing, OpenRouterProvider, Just tokens) ->
                getNextToken tokens Nothing >>= \case
                    Left _ -> pure ()
                    Right credential ->
                        OpenRouterUsage.fetchOpenRouterUsage
                            credential.accessToken >>= \case
                                Left _ -> pure ()
                                Right snapshot ->
                                    publish
                                        (formatOpenRouterLimitStatus snapshot)
            _ -> pure ()
      where
        refreshWith tokens fetch formatStatus =
            getNextToken tokens Nothing >>= \case
                Left _ -> pure ()
                Right credential ->
                    fetch credential >>= \case
                        Left _ -> pure ()
                        Right snapshot -> publish (formatStatus snapshot)
        publish limitStatus =
            forM_
                limitStatus
                (emitUiEvent runtime . UiSetPromptLimitStatus . Just)
