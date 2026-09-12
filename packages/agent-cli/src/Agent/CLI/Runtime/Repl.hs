-- | Interactive prompt loop and session continuation wiring.
module Agent.CLI.Runtime.Repl
    ( repl
    , replWithDraft
    , runPendingTurn
    , finishTurn
    , preparePromptSkillInputs
    , preparePromptSkillInputsWithPaste
    ) where

import Agent.Runtime.Session.Request
    ( readSessionRequestParams
    )
import Agent.CLI.ActiveAccount
    ( ActiveAccount(..)
    , readActiveAccount
    )
import Agent.CLI.AgentViewport
    ( renderAgentViewportPanelFor,
      AgentViewportEnv(viewportSelected, viewportEntries) )
import Agent.CLI.Command
    ( currentEffort, currentModel, mkSlashCatalog, SlashCatalog )
import Agent.ReasoningEffort (reasoningEffortText)
import Agent.CLI.Dictation
    ( dictationTargetForSession )
import Agent.Runtime.GatewayClient
    ( GatewayModelAccess
    , cachedGatewayModels
    , fetchGatewayUsage
    , gatewayModelIds
    )
import Agent.CLI.Input
    ( ReplLine(ReplText)
    , readReplLineWithCatalogForProvider
    , readReplLineWithCatalogForTarget
    )
import Agent.OpenAI.Models.Types (ModelInfo(..), modelServiceTierForRequest)
import Agent.Runtime.Models ( catalogModelIds )
import Agent.CLI.SteeringInputs
    ( awaitSteeringInput
    , readSteeringInputs
    )
import Agent.CLI.Provider.Switch
    ( reportProviderUnavailable, requestStartupProviderFallback )
import Agent.CLI.ProviderTransition ( PendingTurn, TurnResult )
import Agent.CLI.Render
    ( RenderConfig(..)
    , stateLastTokensPerSecond
    )
import Agent.CLI.ReplMode
    ( replModeFromState, ReplMode(ReplModeAlwaysApprove) )
import Agent.CLI.Runtime.Repl.Commands
    ( handleReplLine
    , preparePromptSkillInputs
    , preparePromptSkillInputsWithPaste
    )
import Agent.CLI.Runtime.Types
    ( PendingTurnPresentation
    , RunResult(RunSwitchProvider)
    )
import Agent.CLI.Session.Interaction
    ( buildPromptState
    , syncFullscreenContext
    )
import Agent.CLI.Session.Lifecycle ( SessionContinuation(..) )
import Agent.CLI.SessionEnv ( SessionEnv(..), SessionInboxRuntime(..) )
import Agent.Runtime.SessionState qualified as RuntimeState
import Agent.CLI.Skills ( skillInvocationCommand )
import Agent.CLI.Status ( formatReplStatusLine, formatBackgroundTaskStatus )
import Agent.CLI.Style
    ( beginBackground,
      endBackground,
      roleMuted,
      rolePrompt,
      roleSuccess,
      roleWarn,
      userBackground )
import Agent.CLI.TUI.App
    ( FullscreenRuntime
    , emitUiEvent,
      readFullscreenLineOrWithCatalog,
      refreshFullscreenImagePreviews )
import Agent.CLI.Terminal
    ( emitTerminalSequence,
      osc133PromptEnd,
      osc133PromptStart,
      resolveColor,
      withSynchronizedOutput,
      TerminalCapabilities(terminalSemanticPrompts) )
import Agent.CLI.Turn (runOneTurn)
import Agent.CLI.Usage
    ( formatGrokLimitStatus,
      formatOpenAiLimitStatus,
      formatOpenRouterLimitStatus )
import Agent.Dialect ( dialectId )
import Agent.Error (ApiError)
import Agent.Loop ( emptyTokenUsage )
import Agent.OpenAI.Usage ( fetchUsage )
import Agent.Provider
    ( Provider(OpenRouterProvider, XAIProvider, OpenAIProvider),
      Credential(accessToken, accountId),
      getNextToken,
      tokenProviderBillingMode,
      BillingMode(SubscriptionBilled) )
import Agent.Skills
    ( SkillInvocation(invocationSkill), Skill(skillUserInvocable) )
import Agent.TUI.Model
    ( PromptState
    , UiEvent(UiSetPromptLimitStatus, UiSystemMessage, UiSetBackgroundTaskStatus) )
import Agent.Tools.PlanMode
    ( PlanModeEnv(planStateRef),
      PlanModeState(PlanPending, PlanActive) )
import Agent.Runtime.Session.Inbox (releaseInboxPending)
import Control.Concurrent.Async ( race, withAsync )
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (finally)
import Control.Concurrent.MVar ( withMVar )
import Control.Concurrent.STM (atomically, orElse, retry, takeTMVar, tryTakeTMVar)
import Control.Monad ( when, forM_ )
import Data.IORef ( atomicModifyIORef', readIORef, writeIORef )
import Data.Maybe ( fromMaybe, isJust )
import Data.Text ( Text )
import Data.Time.Clock (getCurrentTime)
import System.Console.ANSI ( getTerminalSize )
import System.Console.ANSI.Codes ( clearFromCursorToLineEndCode )
import System.IO ( stdout, hFlush )
import qualified Agent.OpenRouter.Usage as OpenRouterUsage
    ( fetchOpenRouterUsage )
import qualified Agent.CLI.Session.Lifecycle as SessionLifecycle
    ( finishTurn, retryFailedTurn, runPendingTurn )
import qualified Data.Text as Text ( null, strip, pack )
import qualified Data.Text.IO as Text ( putStr, putStrLn )
import qualified Agent.XAI.Usage as XAIUsage ( fetchGrokUsage )

sessionContinuation :: SessionContinuation
sessionContinuation =
    SessionContinuation
        { resumeSession = repl
        , resumeSessionWithDraft = replWithDraft
        }

data ReplWake
    = ProviderUnavailableWake !ApiError
    | SteeringInputWake

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
    , sessionState = sessionState
    , sessionProvider = provider
    , sessionModelCatalog = catalog
    , sessionGatewayModels = gatewayModelsRef
    , sessionDialect = dialect
    , sessionStartupUnavailable = startupUnavailableRef
    , sessionParams = paramsRef
    , sessionPolicy = policyRef
    , sessionPlanMode = planMode
    , sessionSkillInvocations = skillInvocationsRef
    , sessionRefreshSkills = refreshSkills
    , sessionActiveToolNames = readActiveToolNames
    , sessionDraft = draftRef
    , sessionInterrupt = interrupt
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
    params <- readSessionRequestParams paramsRef
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
    pendingAttachments <- RuntimeState.readSessionAttachments sessionState
    let idleMode = replModeFromState planState policy
    usage <- RuntimeState.readSessionUsage sessionState
    account <- (.activeAccountLabel) <$> readActiveAccount accountRef
    mlineResult <- case fullscreen of
        Just runtime -> do
            syncFullscreenContext env
            refreshFullscreenImagePreviews runtime pendingAttachments
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
            readFullscreenPrompt
                env
                runtime
                slashCatalog
                promptState
                draft
                gatewayAccess
                (currentModel params)
        Nothing -> Right <$> withMVar render.renderLock \_ -> do
            let inbox = env.sessionInboxRuntime.inboxInline
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
            let finishChrome = do
                    when terminal.terminalSemanticPrompts $
                        emitTerminalSequence terminal stdout osc133PromptEnd
                    Text.putStr (endBackground stdoutColor)
                    hFlush stdout
                readLine = case gatewayAccess of
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
            queued <- atomically (tryTakeTMVar inbox)
            case queued of
                Just text -> do
                    finishChrome
                    pure (ReplText text, True)
                Nothing -> do
                    outcome <- race (atomically (takeTMVar inbox)) readLine
                    finishChrome
                    pure $ case outcome of
                        Left text -> (ReplText text, True)
                        Right line -> (line, False)
    case mlineResult of
        Left SteeringInputWake -> do
            pending <-
                readSteeringInputs env.sessionSteeringInputs
            if null pending
                then replWithDraft env draft
                else do
                    forM_ fullscreen \runtime ->
                        emitUiEvent runtime
                            (UiSystemMessage
                                "Queued input received; resuming the agent.")
                    -- Keep inputs in the normal steering queue so each is
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
        Right (mline, fromInbox) -> do
            -- Any user action wins the startup race. In particular, a prompt
            -- already submitted while the preflight was running proceeds on
            -- the selected provider and leaves request-time fallback in charge.
            writeIORef startupUnavailableRef Nothing
            submitReplLine env fromInbox $
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

submitReplLine :: SessionEnv -> Bool -> IO RunResult -> IO RunResult
submitReplLine env fromInbox action = do
    when fromInbox $
        writeIORef env.sessionInboxRuntime.inboxTurn True
    result <- action
    leftover <- atomicModifyIORef'
        env.sessionInboxRuntime.inboxTurn
        (\active -> (False, active))
    when leftover $
        releaseInboxPending env.sessionInboxRuntime.inboxMessages
    pure result

readFullscreenPrompt
    :: SessionEnv
    -> FullscreenRuntime
    -> SlashCatalog
    -> PromptState
    -> Text
    -> Maybe GatewayModelAccess
    -> Text
    -> IO (Either ReplWake (ReplLine, Bool))
readFullscreenPrompt
    env
    runtime
    slashCatalog
    promptState
    draft
    gatewayAccess
    model =
    withAsync
        (refreshPromptAccountLimit env gatewayAccess model runtime)
        \_ -> do
            startupUnavailable <- readIORef env.sessionStartupUnavailable
            failedTurn <- readIORef env.sessionLastFailedTurn
            let steeringWake =
                    case failedTurn of
                        -- Preserve the user's retry candidate. Its next retry
                        -- or replacement turn will consume the queued
                        -- input through normal steering.
                        Just _ -> retry
                        Nothing ->
                            SteeringInputWake
                                <$ awaitSteeringInput
                                    env.sessionSteeringInputs
                wake = case startupUnavailable of
                    Nothing -> steeringWake
                    Just unavailable ->
                        (ProviderUnavailableWake <$> unavailable)
                            `orElse` steeringWake
            (withAsync
                (refreshBackgroundTaskStatus env runtime (not (isJust failedTurn)))
                \_ ->
                    readFullscreenLineOrWithCatalog
                        runtime
                        slashCatalog
                        promptState
                        draft
                        wake)
                `finally` emitUiEvent runtime (UiSetBackgroundTaskStatus [])

-- | Scoped to the idle prompt, so elapsed time remains live without keeping a
-- permanent polling worker or allowing a late update after a turn starts.
refreshBackgroundTaskStatus :: SessionEnv -> FullscreenRuntime -> Bool -> IO ()
refreshBackgroundTaskStatus env runtime canResume = refresh Nothing
  where
    refresh previous = do
        tasks <- env.sessionReadBackgroundTasks
        now <- getCurrentTime
        let rows = formatBackgroundTaskStatus now canResume tasks
        when (previous /= Just rows) $
            emitUiEvent runtime (UiSetBackgroundTaskStatus rows)
        threadDelay 1000000
        refresh (Just rows)

refreshPromptAccountLimit
    :: SessionEnv
    -> Maybe GatewayModelAccess
    -> Text
    -> FullscreenRuntime
    -> IO ()
refreshPromptAccountLimit
    SessionEnv
        { sessionProvider = provider
        , sessionTokenProvider = tokenProvider
        }
    gatewayAccess
    model
    runtime =
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
