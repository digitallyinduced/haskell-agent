-- | Model, effort, and account selection commands.
module Agent.CLI.Runtime.Repl.Selection
    ( handleSelectionAction
    , SelectionInput(..)
    , handleSelectionInput
    , selectRequestedAccount
    ) where

import Agent.CLI.Session.Request
    ( readSessionRequestParams
    , modifySessionRequestOptions
    )
import Agent.CLI.ActiveAccount
    ( ActiveAccount(..)
    , readActiveAccount
    )
import Agent.CLI.AccountPicker
    ( AccountPickerOption(..),
      accountPickerMatches,
      accountPickerMatchesRequest,
      accountPickerRow,
      loadAllAccountPickerOptions,
      loadAllAccountPickerOptionsCached )
import Agent.CLI.Auth ( geminiAuthErrorNeedsReconnect )
import Agent.CLI.Command
    ( currentEffort,
      currentModel,
      SelectionAction(ReplSetModel, ReplShowEffort, ReplSetEffort, ReplToggleFast,
                 ReplShowModel, ReplShowTheme, ReplSetTheme) )
import Agent.CLI.Config
    ( HarnessConfig(configTheme)
    , updateHarnessConfig
    )
import Agent.CLI.Error ( formatApiErrorInlineAt )
import Agent.CLI.GatewayClient ( refreshGatewayModels )
import Agent.CLI.GatewayModels (modelOptionsForGatewayModels)
import Agent.CLI.Login ( connectProviderAccount )
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
import Agent.CLI.Provider.Switch
    ( applyModelChange,
      requestAccountProviderSwitch,
      requestModelTargetSwitch )
import Agent.CLI.ProviderTransition
    ( PendingTurn
    , ProviderTransition(..)
    , resumePendingTurnIfPresent
    )
import Agent.CLI.Render ( clearThinking, renderEvent )
import Agent.CLI.Runtime.Types ( RunResult(..) )
import Agent.CLI.Session.Choices
    ( atMay, effortChoice, modelChoiceWithEffort )
import Agent.CLI.ModelPicker
    ( ModelPickerSelection(modelPickerEffort, modelPickerOption) )
import Agent.CLI.Session.Interaction ( setSessionEffort )
import Agent.CLI.SessionEnv ( SessionEnv(..) )
import Agent.Runtime.SessionState qualified as RuntimeState
import Agent.CLI.Session.Workspace (WorkspaceContext(..))
import Agent.CLI.Style
    ( glyphOk, glyphSession, roleError, roleMuted )
import Agent.CLI.TUI.App
    ( emitUiEvent,
      enqueueAppEvent,
      requestFullscreenThemeChoice,
      requestFullscreenChoiceWithBody,
      withFullscreenSuspended )
import Agent.CLI.TUI.Types
    ( AppEvent(AppSetTheme)
    , FullscreenRuntime(runtimeThemeRef)
    )
import Agent.CLI.Terminal ( resolveColor )
import Agent.Dialect ( dialectId )
import Agent.Loop ( LoopEvent(ActivityUpdated) )
import Agent.OpenAI.Models.Types (ModelInfo(..), modelServiceTierForRequest)
import Agent.Provider
    ( Provider(GeminiProvider, OpenAIProvider),
      providerSlug,
      tokenProviderBillingMode )
import Agent.ReasoningEffort (ReasoningEffort, reasoningEffortText)
import Agent.Responses.Types (ResponseCreateParams(..))
import Agent.TUI.Model
    ( progressNotice,
      UiEvent(UiSetNotice, UiSystemMessage, UiErrorMessage) )
import Agent.TUI.Theme
    ( ThemeKind
    , parseThemeKind
    , themeKindAt
    , themeKindRows
    , themeKindText
    )
import Control.Exception.Safe ( finally )
import Data.IORef ( readIORef, writeIORef )
import Data.List ( findIndex )
import Data.Maybe ( fromMaybe, listToMaybe )
import Data.Text ( Text )
import Data.Time.Clock ( getCurrentTime )
import System.IO ( stdout, stderr )
import qualified Data.Text as Text
import qualified Data.Text.IO as Text ( putStrLn, hPutStrLn )

-- | Selection shortcuts emitted by the interactive editor.
data SelectionInput
    = ChooseModel !Text
    | ChooseEffort !Text
    | ChooseAccount !Text

handleSelectionInput
    :: SessionEnv
    -> IO RunResult
    -> (PendingTurn -> IO RunResult)
    -> SelectionInput
    -> IO RunResult
handleSelectionInput env continue retryPendingTurn input =
    handleSelection env continue retryPendingTurn (Left input)

handleSelectionAction :: SessionEnv -> IO RunResult -> SelectionAction -> IO RunResult
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
            account <- readActiveAccount env.sessionAccount
            let currentSelectionId = account.activeSelectionId
                currentAccountId = account.activeAccountId
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
            params <- readSessionRequestParams env.sessionParams
            currentAccount <- (.activeAccountLabel) <$> readActiveAccount env.sessionAccount
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
    -> Either SelectionInput SelectionAction
    -> IO RunResult
handleSelection
        env@SessionEnv
            { sessionRender = render
            , sessionState = RuntimeState.SessionState
                { stateConversation = conversationRef }
            , sessionProvider = provider
            , sessionConnection = connectionId
            , sessionDialect = dialect
            , sessionParams = paramsRef
            , sessionPersist = persist
            , sessionWorkspace = WorkspaceContext{projectRoot, home}
            , sessionDraft = draftRef
            }
        continue
        retryPendingTurn = \case
    Left (ChooseModel keptDraft) -> do
        writeIORef draftRef keptDraft
        chooseModel env continue
    Left (ChooseEffort _) -> chooseEffort env continue
    Left (ChooseAccount keptDraft) -> do
        writeIORef draftRef keptDraft
        chooseAccount env retryPendingTurn continue
    Right ReplShowEffort -> do
        color <- resolveColor stdout
        params <- readSessionRequestParams paramsRef
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
        setEffort env level
        continue
    Right ReplToggleFast -> do
        color <- resolveColor stdout
        params <- readSessionRequestParams paramsRef
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
                modifySessionRequestOptions paramsRef $ \current ->
                    current { serviceTier = if next then Just "priority" else Nothing }
                displayInfo message $
                    Text.putStrLn (roleMuted color (glyphOk <> message))
        continue
    Right ReplShowModel -> do
        chooseModel env continue
    Right (ReplSetModel name) -> do
        color <- resolveColor stdout
        resolveRequestedModel env name >>= \case
            Left err -> do
                selectionError env err $
                    Text.hPutStrLn stderr (roleError color err)
                continue
            Right choice ->
                if modelTargetRequiresRebuild
                        connectionId provider (dialectId dialect) choice
                    then
                        switchModelTarget env color choice Nothing continue
                    else do
                        message <- applyModelChange
                            home projectRoot provider connectionId
                            choice.modelTarget.targetModelId
                            choice.modelTarget.targetWireModelId
                            choice.modelTarget.targetDialect
                            paramsRef render conversationRef persist
                        env.sessionRefreshRequestParams
                        displayInfo message $
                            Text.putStrLn
                                (roleMuted color (glyphOk <> message))
                        continue
    Right ReplShowTheme ->
        chooseTheme env continue
    Right (ReplSetTheme name) ->
        case parseThemeKind name of
            Nothing -> do
                color <- resolveColor stderr
                let message =
                        "unknown theme '"
                            <> Text.strip name
                            <> "' (try /theme for the picker)"
                displayError message $
                    Text.hPutStrLn stderr (roleError color message)
                continue
            Just theme ->
                setTheme env theme continue
  where
    displayInfo = selectionInfo env
    displayError = selectionError env

selectionInfo :: SessionEnv -> Text -> IO () -> IO ()
selectionInfo env message minimalAction = case env.sessionFullscreen of
    Nothing -> minimalAction
    Just runtime -> emitUiEvent runtime (UiSystemMessage message)

selectionError :: SessionEnv -> Text -> IO () -> IO ()
selectionError env message minimalAction = case env.sessionFullscreen of
    Nothing -> minimalAction
    Just runtime -> emitUiEvent runtime (UiErrorMessage message)

withReplActivity :: SessionEnv -> Text -> IO a -> IO a
withReplActivity env message action = do
    case env.sessionFullscreen of
        Nothing -> renderEvent env.sessionRender (ActivityUpdated message)
        Just runtime ->
            emitUiEvent runtime
                (UiSetNotice (Just (progressNotice message)))
    action `finally`
        case env.sessionFullscreen of
            Nothing -> clearThinking env.sessionRender
            Just runtime -> emitUiEvent runtime (UiSetNotice Nothing)

setEffort :: SessionEnv -> ReasoningEffort -> IO ()
setEffort env level = do
    color <- resolveColor stdout
    let supported = reasoningEffortsForDialect (dialectId env.sessionDialect)
        levelText = reasoningEffortText level
    if level `elem` supported
        then do
            setSessionEffort env level
            selectionInfo env ("effort set to " <> levelText) $
                Text.putStrLn
                    (roleMuted color
                        (glyphOk <> "effort set to " <> levelText))
        else do
            let message =
                    "effort "
                        <> levelText
                        <> " is not supported by the active model"
            selectionError env message $
                Text.hPutStrLn stderr (roleError color message)

chooseEffort :: SessionEnv -> IO RunResult -> IO RunResult
chooseEffort env next = do
    params <- readSessionRequestParams env.sessionParams
    effortChoice
        env.sessionFullscreen
        (reasoningEffortsForDialect (dialectId env.sessionDialect))
        (currentEffort params) >>= \case
        Nothing -> next
        Just level -> setEffort env level >> next

chooseModel :: SessionEnv -> IO RunResult -> IO RunResult
chooseModel env@SessionEnv
    { sessionModelCatalog = catalog
    , sessionGatewayModels = gatewayModelsRef
    , sessionFullscreen = fullscreen
    , sessionConnection = connectionId
    , sessionProvider = provider
    , sessionDialect = dialect
    , sessionParams = paramsRef
    , sessionWorkspace = WorkspaceContext{home, projectRoot}
    , sessionRender = render
    , sessionState = RuntimeState.SessionState
        { stateConversation = conversationRef }
    , sessionPersist = persist
    } next = do
    color <- resolveColor stderr
    params <- readSessionRequestParams paramsRef
    gatewayAccess <- readIORef gatewayModelsRef
    let current = currentModel params
    modelChoiceWithEffort
        catalog gatewayAccess fullscreen color connectionId provider current
            (dialectId dialect) (currentEffort params) >>= \case
        Left err -> do
            selectionError env err $
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
                selectionInfo env message $
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
                env.sessionRefreshRequestParams
                setSessionEffort env selectedEffort
                selectionInfo env message $
                    Text.putStrLn
                        (roleMuted color
                            (glyphOk <> message))
                next
              else
                switchModelTarget env color choice (Just selectedEffort) next

resolveRequestedModel :: SessionEnv -> Text -> IO (Either Text ModelOption)
resolveRequestedModel env name =
    readIORef env.sessionGatewayModels >>= \case
        Just access ->
            refreshGatewayModels access >>= \case
                Left err -> pure (Left err)
                Right models ->
                    case
                        resolveModelOptionById
                            (modelOptionsForGatewayModels env.sessionModelCatalog models)
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
            let rawChoice = rawModelOption env.sessionProvider name
                choice =
                    fromMaybe
                        (rawChoice
                            { modelTarget =
                                rawChoice.modelTarget
                                    { targetConnectionId = env.sessionConnection
                                    , targetDialect = dialectId env.sessionDialect
                                    }
                            })
                        (resolveConfiguredModel env.sessionModelCatalog name)
            Right <$> resolveModelOptionDialect choice

chooseTheme :: SessionEnv -> IO RunResult -> IO RunResult
chooseTheme env next =
    case env.sessionFullscreen of
        Nothing -> do
            color <- resolveColor stderr
            let message = "theme selection requires fullscreen mode"
            Text.hPutStrLn stderr (roleError color message)
            next
        Just runtime -> do
            current <- readIORef runtime.runtimeThemeRef
            let initial =
                    fromMaybe 0 $
                        findIndex
                            (== current)
                            (map themeKindAt [0 .. length themeKindRows - 1])
            requestFullscreenThemeChoice
                runtime
                initial
                themeKindRows >>= \case
                Nothing -> next
                Just index -> setTheme env (themeKindAt index) next

setTheme :: SessionEnv -> ThemeKind -> IO RunResult -> IO RunResult
setTheme env theme next =
    case env.sessionFullscreen of
        Nothing -> do
            color <- resolveColor stderr
            let message = "theme selection requires fullscreen mode"
            Text.hPutStrLn stderr (roleError color message)
            next
        Just runtime -> do
            updateHarnessConfig
                env.sessionWorkspace.home
                (\config -> Right config { configTheme = theme })
                >>= \case
                Left err -> do
                    color <- resolveColor stderr
                    Text.hPutStrLn stderr (roleError color err)
                    next
                Right _ -> do
                    enqueueAppEvent runtime (AppSetTheme theme)
                    emitUiEvent runtime
                        (UiSystemMessage
                            ("theme set to " <> themeKindText theme))
                    next

switchModelTarget
    :: SessionEnv
    -> Bool
    -> ModelOption
    -> Maybe ReasoningEffort
    -> IO RunResult
    -> IO RunResult
switchModelTarget env color choice selectedEffort next =
    requestModelTargetSwitch env.sessionFullscreen choice env.sessionPersist >>= \case
        Left err
            | choice.modelTarget.targetProvider == GeminiProvider
            , modelAuthErrorNeedsConnect GeminiProvider err -> do
                connected <- case env.sessionFullscreen of
                    Nothing ->
                        connectProviderAccount color GeminiProvider
                    Just runtime ->
                        withFullscreenSuspended runtime $
                            connectProviderAccount color GeminiProvider
                case connected of
                    Nothing -> next
                    Just _ ->
                        requestModelTargetSwitch
                            env.sessionFullscreen choice env.sessionPersist >>= \case
                            Left retryErr -> do
                                selectionError env retryErr $
                                    Text.hPutStrLn stderr
                                        (roleError color retryErr)
                                next
                            Right result ->
                                pure
                                    (applyTransitionEffort
                                        selectedEffort
                                        result)
        Left err -> do
            selectionError env err $
                Text.hPutStrLn stderr (roleError color err)
            next
        Right result ->
            pure (applyTransitionEffort selectedEffort result)

applyTransitionEffort :: Maybe ReasoningEffort -> RunResult -> RunResult
applyTransitionEffort selectedEffort = \case
    RunSwitchProvider transition ->
        RunSwitchProvider
            transition { transitionEffort = selectedEffort }
    result -> result

attachPendingTurn :: RunResult -> PendingTurn -> RunResult
attachPendingTurn result pending =
    case result of
        RunSwitchProvider transition ->
            RunSwitchProvider transition
                { transitionPendingTurn = Just pending }
        other -> other

modelAuthErrorNeedsConnect :: Provider -> Text -> Bool
modelAuthErrorNeedsConnect targetProvider message =
    geminiAuthErrorNeedsReconnect message
        || maybe False geminiAuthErrorNeedsReconnect
            (Text.stripPrefix
                ( "cannot switch to "
                    <> providerSlug targetProvider
                    <> ": "
                )
                message)

chooseAccount
    :: SessionEnv
    -> (PendingTurn -> IO RunResult)
    -> IO RunResult
    -> IO RunResult
chooseAccount env retryPendingTurn next = do
    gatewayAccess <- readIORef env.sessionGatewayModels
    case gatewayAccess of
        Just _ -> do
            selectionError env
                "Account switching is unavailable while connected to the organization gateway. Disconnect the gateway first."
                (pure ())
            next
        Nothing -> chooseAccountFromOptions env retryPendingTurn next

chooseAccountFromOptions
    :: SessionEnv
    -> (PendingTurn -> IO RunResult)
    -> IO RunResult
    -> IO RunResult
chooseAccountFromOptions env@SessionEnv
    { sessionFullscreen = fullscreen
    , sessionDatabasePool = databasePool
    , sessionProvider = provider
    , sessionTokenProvider = tokenProvider
    , sessionSelectAccount = selectAccount
    , sessionParams = paramsRef
    , sessionModelCatalog = catalog
    , sessionConnection = connectionId
    , sessionDialect = dialect
    , sessionPersist = persist
    } retryPendingTurn next =
    case fullscreen of
        Just runtime -> do
            account <- readActiveAccount env.sessionAccount
            let currentSelectionId = account.activeSelectionId
                currentAccountId = account.activeAccountId
            options <- withReplActivity env
                "Loading account usage…"
                (loadAllAccountPickerOptionsCached databasePool provider)
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
                                                loadAllAccountPickerOptionsCached
                                                    databasePool
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
                                                    selectionError env
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
                                selectionError env message (pure ())
                                next
                            Right label -> do
                                selectionInfo env
                                    ("account switched to " <> label)
                                    (pure ())
                                resumePendingTurnIfPresent
                                    env.sessionLastFailedTurn
                                    retryPendingTurn
                                    next
                    | otherwise = do
                        params <- readSessionRequestParams paramsRef
                        currentAccount <- (.activeAccountLabel) <$> readActiveAccount env.sessionAccount
                        requestAccountProviderSwitch
                            catalog fullscreen provider connectionId
                            (currentModel params) currentAccount
                            (dialectId dialect)
                            selectedProvider selectedSelectionId
                            selectedAccountId selectedLabel
                            persist >>= \case
                                Left err -> do
                                    selectionError env err (pure ())
                                    next
                                Right result -> do
                                    selectionInfo env
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
            selectionError env
                "Account switching is unavailable for this session."
                (pure ())
            next
