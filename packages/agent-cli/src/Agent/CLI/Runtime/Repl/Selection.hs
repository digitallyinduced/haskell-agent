-- | Model, effort, and account selection commands.
module Agent.CLI.Runtime.Repl.Selection
    ( handleSelectionAction
    , handleSelectionInput
    , selectRequestedAccount
    ) where

import Agent.CLI.AccountPicker
    ( AccountPickerOption(..),
      accountPickerMatches,
      accountPickerMatchesRequest,
      accountPickerRow,
      loadAllAccountPickerOptions )
import Agent.CLI.AccountSelection ()
import Agent.CLI.Afk ()
import Agent.CLI.AgentSessions ()
import Agent.CLI.AgentViewport ()
import Agent.CLI.Approval ()
import Agent.CLI.Artifact ()
import Agent.CLI.Auth ( geminiAuthErrorNeedsReconnect )
import Agent.CLI.Clipboard ()
import Agent.CLI.Command
    ( currentEffort,
      currentModel,
      ReplAction(ReplSetModel, ReplShowEffort, ReplSetEffort, ReplToggleFast,
                 ReplShowModel) )
import Agent.CLI.Compaction ()
import Agent.CLI.Config ()
import Agent.CLI.Connectivity ()
import Agent.CLI.Database ()
import Agent.CLI.Database.Store ()
import Agent.CLI.Dialects ()
import Agent.CLI.Error ( formatApiErrorInlineAt )
import Agent.CLI.GatewayClient ( refreshGatewayModels )
import Agent.CLI.GatewayBridge ()
import Agent.CLI.Input
    ( ReplLine(ReplChooseAccount, ReplChooseModel, ReplChooseEffort) )
import Agent.CLI.Interrupt ()
import Agent.CLI.LearnedSkills ()
import Agent.CLI.LearnedSkills.Store ()
import Agent.CLI.Login ( connectProviderAccount )
import Agent.CLI.Lsp ()
import Agent.CLI.ManagedTurn ()
import Agent.CLI.McpManager ()
import Agent.CLI.McpStatus ()
import Agent.CLI.Models
    ( gatewayModelOptions,
      modelTargetRequiresRebuild,
      rawModelOption,
      resolveConfiguredModel,
      resolveModelOptionById,
      resolveModelOptionDialect,
      ModelOption(modelTarget),
      ModelTarget(targetDialect, targetProvider, targetModelId,
                  targetConnectionId, targetWireModelId) )
import Agent.CLI.Options
    ( normalizeReasoningEffortForDialect
    , reasoningEffortsForDialect
    )
import Agent.CLI.PendingInputs ()
import Agent.CLI.Plan ()
import Agent.CLI.Progress ()
import Agent.CLI.Project ()
import Agent.CLI.Prompt ()
import Agent.CLI.PromptHooks ()
import Agent.CLI.Provider.OpenAI ()
import Agent.CLI.Provider.Switch
    ( applyModelChange,
      requestAccountProviderSwitch,
      requestModelTargetSwitch )
import Agent.CLI.ProviderAvailability ()
import Agent.CLI.ProviderFallback ()
import Agent.CLI.ProviderTransition
    ( PendingTurn
    , ProviderTransition(..)
    , resumePendingTurnIfPresent
    )
import Agent.CLI.Recap ()
import Agent.CLI.Render ( clearThinking, renderEvent )
import Agent.CLI.ReplMode ()
import Agent.CLI.Request ()
import Agent.CLI.Runtime.HistorySource ()
import Agent.CLI.Runtime.Persistence ()
import Agent.CLI.Runtime.Recap ()
import Agent.CLI.Runtime.Types ( RunResult(..) )
import Agent.CLI.Secret ()
import Agent.CLI.Session ()
import Agent.CLI.Session.Attachments ()
import Agent.CLI.Session.Choices
    ( atMay, effortChoice, modelChoiceWithEffort )
import Agent.CLI.ModelPicker
    ( ModelPickerSelection(modelPickerEffort, modelPickerOption) )
import Agent.CLI.Session.History ()
import Agent.CLI.Session.Interaction ( setSessionEffort )
import Agent.CLI.Session.Lifecycle ()
import Agent.CLI.Session.Runtime.Types ()
import Agent.CLI.Session.Selection ()
import Agent.CLI.SessionAdmin ()
import Agent.CLI.SessionEnv ( SessionEnv(..) )
import Agent.CLI.SessionLock ()
import Agent.CLI.SessionState ()
import Agent.CLI.SessionTitle ()
import Agent.CLI.Skills ()
import Agent.CLI.Startup.Auth ()
import Agent.CLI.Startup.Format ()
import Agent.CLI.StartupContext ()
import Agent.CLI.Status ()
import Agent.CLI.Style
    ( glyphOk, glyphSession, roleError, roleMuted )
import Agent.CLI.Subagents.Runtime ()
import Agent.CLI.TUI.App
    ( emitUiEvent,
      requestFullscreenChoiceWithBody,
      withFullscreenSuspended )
import Agent.CLI.TUI.SessionHistory ()
import Agent.CLI.TUI.Types ()
import Agent.CLI.Terminal ( resolveColor )
import Agent.CLI.Tools ()
import Agent.CLI.Turn ()
import Agent.CLI.Usage ()
import Agent.CLI.WebFetch ()
import Agent.CLI.Worktree ()
import Agent.Cancel ()
import Agent.Claude ()
import Agent.Dialect ( dialectId )
import Agent.Error ()
import Agent.GrokBuild.Dialect.Goal ()
import Agent.GrokBuild.Dialect.Runtime ()
import Agent.GrokBuild.Dialect.Workflow ()
import Agent.Loop ( LoopEvent(ActivityUpdated) )
import Agent.OpenAI.Compaction ()
import Agent.OpenAI.Usage ()
import Agent.OpenAI.WebSocketClient ()
import Agent.OpenAI.Models.Types (ModelInfo(..), modelServiceTierForRequest)
import Agent.OpenRouter.LoopBackend ()
import Agent.OsPath ()
import Agent.Provider
    ( Provider(GeminiProvider, OpenAIProvider),
      providerSlug,
      tokenProviderBillingMode )
import Agent.ReasoningEffort (reasoningEffortText)
import Agent.Responses.GenericBackend ()
import Agent.Responses.GenericClient ()
import Agent.Responses.Types (ResponseCreateParams(..))
import Agent.Skills ()
import Agent.Store.Postgres ()
import Agent.Store.Types ()
import Agent.Subagents ()
import Agent.Subagents.TaskPath ()
import Agent.TUI.Model
    ( progressNotice,
      UiEvent(UiSetNotice, UiSystemMessage, UiErrorMessage) )
import Agent.TUI.Motion ()
import Agent.ToolDispatch ()
import Agent.Tools.MultiAgents ()
import Agent.Tools.PlanMode ()
import Agent.Tools.Secret ()
import Agent.Tools.Types ()
import Agent.XAI.LoopBackend ()
import Control.Applicative ()
import Control.Concurrent.Async ()
import Control.Concurrent.Chan ()
import Control.Concurrent.MVar ()
import Control.Concurrent.STM ()
import Control.Exception ()
import Control.Exception.Safe ( finally )
import Control.Monad ()
import Data.IORef ( readIORef, writeIORef )
import Data.List ( findIndex )
import Data.Maybe ( fromMaybe, listToMaybe )
import Data.Text ( Text )
import Data.Time.Clock ( getCurrentTime )
import System.Console.ANSI ()
import System.Console.ANSI.Codes ()
import System.Directory.OsPath ()
import System.Environment ()
import System.Exit ()
import System.IO ( stdout, stderr )
import System.OsPath ()
import System.Posix.Files ()
import qualified Data.ByteString as BS ()
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
import qualified Data.Text as Text ( stripPrefix )
import qualified Data.Text.IO as Text ( putStrLn, hPutStrLn )
import qualified Agent.XAI.Options as XAI ()
import qualified Agent.XAI.Usage as XAIUsage ()

handleSelectionInput
    :: SessionEnv
    -> IO RunResult
    -> (PendingTurn -> IO RunResult)
    -> ReplLine
    -> IO RunResult
handleSelectionInput env continue retryPendingTurn input =
    handleSelection env continue retryPendingTurn (Left input)

handleSelectionAction :: SessionEnv -> IO RunResult -> ReplAction -> IO RunResult
handleSelectionAction env continue action =
    handleSelection env continue (\_ -> continue) (Right action)

-- | Select an account requested by a trusted, typed Meta Console action.
--
-- Unlike the general account picker, this omits connect rows and filters every
-- candidate to the requested provider and optional exact label/id.  Returning
-- cancellation as 'Left' prevents the caller from reporting that the plan was
-- applied.
selectRequestedAccount
    :: SessionEnv
    -> Provider
    -> Maybe Text
    -> IO (Either Text (Maybe RunResult))
selectRequestedAccount env requestedProvider selector =
    readIORef env.sessionGatewayModels >>= \case
        Just _ ->
            pure $ Left
                "Account switching is unavailable while connected to the organization gateway. Disconnect the gateway first."
        Nothing ->
            selectRequestedAccountWithoutGateway
                env
                requestedProvider
                selector

selectRequestedAccountWithoutGateway
    :: SessionEnv
    -> Provider
    -> Maybe Text
    -> IO (Either Text (Maybe RunResult))
selectRequestedAccountWithoutGateway env requestedProvider selector =
    case env.sessionFullscreen of
        Nothing ->
            pure
                (Left
                    "Account selection needs the fullscreen account picker; use /login in minimal mode")
        Just runtime -> do
            currentSelectionId <- readIORef env.sessionAccountSelectionId
            currentAccountId <- readIORef env.sessionAccountId
            loaded <- withActivity runtime $
                loadAllAccountPickerOptions env.sessionProvider
            let options =
                    filter
                        (accountPickerMatchesRequest
                            requestedProvider
                            selector)
                        loaded
                initial =
                    fromMaybe 0 $
                        findIndex
                            (accountPickerMatches
                                env.sessionProvider
                                currentSelectionId
                                currentAccountId)
                            options
            case options of
                [] -> pure (Left (noMatchingAccountMessage selector))
                _ ->
                    requestFullscreenChoiceWithBody
                        runtime
                        (providerSlug requestedProvider <> " accounts")
                        "Choose the requested account. Only exact provider, label, and id matches are shown."
                        initial
                        (map
                            (accountPickerRow
                                env.sessionProvider
                                currentSelectionId
                                currentAccountId)
                            options)
                        >>= \case
                            Nothing ->
                                pure (Left "Account selection was cancelled.")
                            Just index ->
                                case atMay index options of
                                    Just (AccountPickerAccount
                                        selectedProvider
                                        selectedBilling
                                        selectedSelectionId
                                        selectedAccountId
                                        selectedLabel
                                        _) ->
                                            applySelectedAccount
                                                runtime
                                                selectedProvider
                                                selectedBilling
                                                selectedSelectionId
                                                selectedAccountId
                                                selectedLabel
                                    _ ->
                                        pure
                                            (Left
                                                "The requested account selection is no longer available.")
  where
    withActivity runtime action = do
        emitUiEvent runtime
            (UiSetNotice
                (Just (progressNotice "Loading account usage…")))
        action `finally`
            emitUiEvent runtime (UiSetNotice Nothing)
    noMatchingAccountMessage = \case
        Nothing ->
            "No connected "
                <> providerSlug requestedProvider
                <> " account is available; connect one first."
        Just requested ->
            "No connected "
                <> providerSlug requestedProvider
                <> " account exactly matches '"
                <> requested
                <> "'."
    applySelectedAccount
            runtime
            selectedProvider
            selectedBilling
            selectedSelectionId
            selectedAccountId
            selectedLabel
        | selectedProvider == env.sessionProvider
        , Just selectedBilling
            == (tokenProviderBillingMode <$> env.sessionTokenProvider)
        , Just select <- env.sessionSelectAccount =
            let liveSelectionId =
                    case selectedProvider of
                        OpenAIProvider -> selectedAccountId
                        _ -> selectedSelectionId
            in select liveSelectionId >>= \case
                Left err -> do
                    now <- getCurrentTime
                    pure
                        (Left
                            ("could not select account: "
                                <> formatApiErrorInlineAt now err))
                Right label -> do
                    emitUiEvent runtime
                        (UiSystemMessage
                            ("account switched to " <> label))
                    pure (Right Nothing)
        | otherwise = do
            params <- readIORef env.sessionParams
            currentAccount <- readIORef env.sessionAccount
            requestAccountProviderSwitch
                env.sessionModelCatalog
                (Just runtime)
                env.sessionProvider
                env.sessionConnection
                (currentModel params)
                currentAccount
                (dialectId env.sessionDialect)
                selectedProvider
                selectedSelectionId
                selectedAccountId
                selectedLabel
                env.sessionPersist
                >>= \case
                    Left err -> pure (Left err)
                    Right result -> do
                        emitUiEvent runtime
                            (UiSystemMessage
                                ("switching to "
                                    <> selectedLabel
                                    <> " ("
                                    <> providerSlug selectedProvider
                                    <> ")"))
                        pure (Right (Just result))

handleSelection
    :: SessionEnv
    -> IO RunResult
    -> (PendingTurn -> IO RunResult)
    -> Either ReplLine ReplAction
    -> IO RunResult
handleSelection
        env@SessionEnv
            { sessionRender = render
            , sessionConversation = conversationRef
            , sessionProvider = provider
            , sessionConnection = connectionId
            , sessionModelCatalog = catalog
            , sessionGatewayModels = gatewayModelsRef
            , sessionDialect = dialect
            , sessionParams = paramsRef
            , sessionPersist = persist
            , sessionProjectRoot = projectRoot
            , sessionHome = home
            , sessionDraft = draftRef
            , sessionAccountId = accountIdRef
            , sessionAccountSelectionId = selectionRef
            , sessionSelectAccount = selectAccount
            , sessionTokenProvider = tokenProvider
            , sessionFullscreen = fullscreen
            }
        continue
        retryPendingTurn = \case
    Left (ReplChooseModel keptDraft) -> do
        writeIORef draftRef keptDraft
        chooseModel continue
    Left (ReplChooseEffort _) -> chooseEffort continue
    Left (ReplChooseAccount keptDraft) -> do
        writeIORef draftRef keptDraft
        chooseAccount continue
    Right ReplShowEffort -> do
        color <- resolveColor stdout
        params <- readIORef paramsRef
        let message =
                "effort: "
                    <> reasoningEffortText
                        (normalizeReasoningEffortForDialect
                            (dialectId dialect)
                            (currentEffort params))
        displayInfo message $
            Text.putStrLn
                (roleMuted color (glyphSession <> message))
        continue
    Right (ReplSetEffort level) -> do
        setEffort level
        continue
    Right ReplToggleFast -> do
        color <- resolveColor stdout
        params <- readIORef paramsRef
        let enabled = params.serviceTier == Just "priority"
            supported = maybe False
                (\info ->
                    let ModelInfo { slug = infoSlug } = info
                    in infoSlug == currentModel params
                        && modelServiceTierForRequest info (Just "priority")
                            == Just "priority")
                env.sessionModelInfo
        if not supported
            then do
                let message = "fast mode is not available for the active model"
                displayError message $
                    Text.hPutStrLn stderr (roleError color message)
            else do
                let next = not enabled
                    message =
                        if next then "fast mode enabled"
                        else "fast mode disabled"
                writeIORef paramsRef
                    params { serviceTier = if next then Just "priority" else Nothing }
                displayInfo message $
                    Text.putStrLn (roleMuted color (glyphOk <> message))
        continue
    Right ReplShowModel -> do
        chooseModel continue
    Right (ReplSetModel name) -> do
        color <- resolveColor stdout
        resolveRequestedModel name >>= \case
            Left err -> do
                displayError err $
                    Text.hPutStrLn stderr (roleError color err)
                continue
            Right choice ->
                if modelTargetRequiresRebuild
                        connectionId provider (dialectId dialect) choice
                    then
                        switchModelTarget color choice Nothing continue
                    else do
                        message <- applyModelChange
                            home projectRoot provider connectionId
                            choice.modelTarget.targetModelId
                            choice.modelTarget.targetWireModelId
                            choice.modelTarget.targetDialect
                            paramsRef render conversationRef persist
                        displayInfo message $
                            Text.putStrLn
                                (roleMuted color (glyphOk <> message))
                        continue
    _ -> error "handleSelection: unsupported input"
  where
    displayInfo message minimalAction = case fullscreen of
        Nothing -> minimalAction
        Just runtime -> emitUiEvent runtime (UiSystemMessage message)
    displayError message minimalAction = case fullscreen of
        Nothing -> minimalAction
        Just runtime -> emitUiEvent runtime (UiErrorMessage message)
    withReplActivity message action = do
        case fullscreen of
            Nothing -> renderEvent render (ActivityUpdated message)
            Just runtime ->
                emitUiEvent runtime
                    (UiSetNotice (Just (progressNotice message)))
        action `finally`
            case fullscreen of
                Nothing -> clearThinking render
                Just runtime -> emitUiEvent runtime (UiSetNotice Nothing)
    setEffort level = do
        color <- resolveColor stdout
        let supported = reasoningEffortsForDialect (dialectId dialect)
            levelText = reasoningEffortText level
        if level `elem` supported
            then do
                setSessionEffort env level
                displayInfo ("effort set to " <> levelText) $
                    Text.putStrLn
                        (roleMuted color
                            (glyphOk <> "effort set to " <> levelText))
            else do
                let message =
                        "effort "
                            <> levelText
                            <> " is not supported by the active model"
                displayError message $
                    Text.hPutStrLn stderr (roleError color message)
    chooseEffort next = do
        params <- readIORef paramsRef
        effortChoice
            fullscreen
            (reasoningEffortsForDialect (dialectId dialect))
            (currentEffort params) >>= \case
            Nothing -> next
            Just level -> setEffort level >> next
    chooseModel next = do
        color <- resolveColor stderr
        params <- readIORef paramsRef
        gatewayAccess <- readIORef gatewayModelsRef
        let current = currentModel params
        modelChoiceWithEffort
            catalog gatewayAccess fullscreen color connectionId provider current
                (dialectId dialect) (currentEffort params) >>= \case
            Left err -> do
                displayError err $
                    Text.hPutStrLn stderr (roleError color err)
                next
            Right Nothing -> next
            Right (Just selection) -> do
                let rawChoice = selection.modelPickerOption
                    selectedEffort = selection.modelPickerEffort
                choice <- resolveModelOptionDialect rawChoice
                if choice.modelTarget.targetProvider == provider
                    && choice.modelTarget.targetConnectionId == connectionId
                    && choice.modelTarget.targetModelId == current
                    && choice.modelTarget.targetDialect == dialectId dialect
                  then do
                    setSessionEffort env selectedEffort
                    let message =
                            "model: "
                                <> connectionId
                                <> "/"
                                <> choice.modelTarget.targetModelId
                                <> " · effort "
                                <> reasoningEffortText selectedEffort
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
                        home projectRoot provider connectionId
                        choice.modelTarget.targetModelId
                        choice.modelTarget.targetWireModelId
                        choice.modelTarget.targetDialect
                        paramsRef render conversationRef persist
                    setSessionEffort env selectedEffort
                    displayInfo message $
                        Text.putStrLn
                            (roleMuted color
                                (glyphOk <> message))
                    next
                  else
                    switchModelTarget color choice (Just selectedEffort) next
    resolveRequestedModel name =
        readIORef gatewayModelsRef >>= \case
            Just access ->
                refreshGatewayModels access >>= \case
                    Left err -> pure (Left err)
                    Right modelIds ->
                        case
                            resolveModelOptionById
                                (gatewayModelOptions
                                    catalog
                                    OpenAIProvider
                                    modelIds)
                                name
                        of
                            Nothing ->
                                pure
                                    (Left
                                        ("Model '"
                                            <> name
                                            <> "' is not available through your organization gateway."))
                            Just choice ->
                                Right <$> resolveModelOptionDialect choice
            Nothing -> do
                let rawChoice = rawModelOption provider name
                    choice =
                        fromMaybe
                            (rawChoice
                                { modelTarget =
                                    rawChoice.modelTarget
                                        { targetConnectionId = connectionId
                                        , targetDialect = dialectId dialect
                                        }
                                })
                            (resolveConfiguredModel catalog name)
                Right <$> resolveModelOptionDialect choice
    switchModelTarget color choice selectedEffort next =
        requestModelTargetSwitch fullscreen choice persist >>= \case
            Left err
                | choice.modelTarget.targetProvider == GeminiProvider
                , modelAuthErrorNeedsConnect GeminiProvider err -> do
                    connected <- case fullscreen of
                        Nothing ->
                            connectProviderAccount color GeminiProvider
                        Just runtime ->
                            withFullscreenSuspended runtime $
                                connectProviderAccount color GeminiProvider
                    case connected of
                        Nothing -> next
                        Just _ ->
                            requestModelTargetSwitch
                                fullscreen choice persist >>= \case
                                Left retryErr -> do
                                    displayError retryErr $
                                        Text.hPutStrLn stderr
                                            (roleError color retryErr)
                                    next
                                Right result ->
                                    pure
                                        (applyTransitionEffort
                                            selectedEffort
                                            result)
            Left err -> do
                displayError err $
                    Text.hPutStrLn stderr (roleError color err)
                next
            Right result ->
                pure (applyTransitionEffort selectedEffort result)
    applyTransitionEffort selectedEffort = \case
        RunSwitchProvider transition ->
            RunSwitchProvider
                transition { transitionEffort = selectedEffort }
        result -> result
    attachPendingTurn result pending =
        case result of
            RunSwitchProvider transition ->
                RunSwitchProvider transition
                    { transitionPendingTurn = Just pending }
            other -> other
    modelAuthErrorNeedsConnect targetProvider message =
        geminiAuthErrorNeedsReconnect message
            || maybe False geminiAuthErrorNeedsReconnect
                (Text.stripPrefix
                    ( "cannot switch to "
                        <> providerSlug targetProvider
                        <> ": "
                    )
                    message)
    chooseAccount next = do
        gatewayAccess <- readIORef gatewayModelsRef
        case gatewayAccess of
            Just _ -> do
                displayError
                    "Account switching is unavailable while connected to the organization gateway. Disconnect the gateway first."
                    (pure ())
                next
            Nothing -> chooseAccountFromOptions next
    chooseAccountFromOptions next =
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
                                        _ ->
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
                                            Just connectedId -> do
                                                refreshed <-
                                                    loadAllAccountPickerOptions
                                                        provider
                                                case listToMaybe
                                                        [ account
                                                        | account@(AccountPickerAccount
                                                            accountProvider
                                                            _
                                                            selectionId
                                                            accountId
                                                            _
                                                            _) <- refreshed
                                                        , accountProvider
                                                            == selectedProvider
                                                        , selectionId
                                                            == connectedId
                                                            || accountId
                                                                == connectedId
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
                                    resumePendingTurnIfPresent
                                        env.sessionLastFailedTurn
                                        retryPendingTurn
                                        next
                        | otherwise = do
                            params <- readIORef paramsRef
                            currentAccount <- readIORef env.sessionAccount
                            requestAccountProviderSwitch
                                catalog fullscreen provider connectionId
                                (currentModel params) currentAccount
                                (dialectId dialect)
                                selectedProvider selectedSelectionId
                                selectedAccountId selectedLabel
                                persist >>= \case
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
                                        resumePendingTurnIfPresent
                                            env.sessionLastFailedTurn
                                            (pure . attachPendingTurn result)
                                            (pure result)
            Nothing -> do
                displayError
                    "Account switching is unavailable for this session."
                    (pure ())
                next
