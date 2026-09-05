-- | Provider, model, and account transitions for a live CLI session.
module Agent.CLI.Provider.Switch
    ( accountSwitchTarget
    , applyModelChange
    , chooseStartupProviderTransition
    , continueAutomaticFallback
    , loadSelectedAccountAuth
    , prepareProviderTransition
    , prepareTransitionBackend
    , reloadAuth
    , reportProviderUnavailable
    , requestAccountProviderSwitch
    , requestAutomaticProviderFallback
    , requestModelTargetSwitch
    , requestStartupProviderFallback
    ) where

import Agent.CLI.AccountSelection
    ( SelectedAccount(..)
    , providerSupportsUsageAccountSelection
    , selectProviderAccount
    )
import Agent.CLI.Session.History
    ( LiveConversation
    , readLivePreviousResponseId
    , writeLivePreviousResponseId
    )
import Agent.CLI.Auth
    ( LoadedAuth(..)
    , gatewayAuthSelectionId
    , loadAuth
    , loadAuthForAccount
    )
import Agent.CLI.Error
    ( formatApiErrorAt
    , formatApiErrorInlineAt
    , formatApiErrorRetryCountdownParts
    )
import Agent.CLI.ModelConfig
    ( ModelCatalog
    , builtinConnectionId
    , connectionSupportsDialect
    , loadModelCatalogAt
    )
import Agent.CLI.Models
    ( ModelOption(..)
    , ModelTarget(..)
    , defaultModelOptionFor
    , rawModelOption
    , resolveModelOptionDialect
    )
import Agent.CLI.Project
    ( ModelSwitchScope(..)
    , ProjectAccount(..)
    , loadProjectSettings
    , projectAccountFor
    , resolveProjectRoot
    , persistModelSwitch
    )
import Agent.CLI.Request (setRequestModel)
import Agent.CLI.ProviderAvailability
    ( probeLoadedAutomaticAvailability
    , probeLoadedAvailability
    )
import Agent.CLI.ProviderFallback
    ( allowsAutomaticBillingFallback
    , fallbackCandidates
    )
import Agent.CLI.ProviderTransition
    ( PendingTurn
    , ProviderTransition(..)
    , TransitionCause(..)
    , transitionCommitsImmediately
    , withOptimisticPromptTarget
    )
import Agent.CLI.Render
    ( RenderConfig(..)
    , putTextLn
    )
import Agent.CLI.Runtime.Types
    ( RunResult(..)
    )
import Agent.CLI.Session
import Agent.CLI.SessionEnv (SessionEnv(..))
import Agent.CLI.Style
    ( glyphOk
    , glyphWarn
    , roleError
    , roleMuted
    , roleWarn
    )
import Agent.CLI.Terminal (resolveColor)
import Agent.CLI.TUI.App
    ( FullscreenRuntime
    , emitUiEvent
    )
import Agent.Claude
    ( ClaudeCodeAuth(..)
    , loadClaudeCodeAuth
    )
import Agent.Dialect
    ( DialectId
    , dialectSlug
    )
import Agent.Error
    ( ApiError(..)
    , CredentialExhaustionReason(..)
    )
import Agent.Loop (Backend(..), BackendMiddleware, backendWithCallbacks)
import Agent.Provider
    ( AccountFailure(..)
    , BillingMode(..)
    , Credential(..)
    , FailedCredential(..)
    , Provider(..)
    , TokenProvider
    , getNextToken
    , providerSlug
    , tokenProviderBillingMode
    )
import Agent.ReasoningEffort (reasoningEffortText)
import Agent.Responses.Types (ResponseCreateParams(model))
import Agent.TUI.Model (UiEvent(..))
import Control.Monad
    ( forM_
    , when
    )
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , modifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.Maybe
    ( fromMaybe
    , isJust
    )
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Data.Time.Clock
    ( diffUTCTime
    , getCurrentTime
    )
import System.Directory.OsPath
    ( getCurrentDirectory
    , getHomeDirectory
    )
import System.IO
    ( Handle
    , stderr
    , stdout
    )
import System.OsPath (OsPath)

loadSelectedAccountAuth
    :: Provider
    -> Text
    -> Text
    -> IO (Either Text LoadedAuth)
loadSelectedAccountAuth provider selectionId accountId =
    case provider of
        OpenAIProvider ->
            loadAuthForAccount
                OpenAIProvider
                (if selectionId == gatewayAuthSelectionId
                    then selectionId
                    else accountId)
        ClaudeCodeProvider ->
            loadAuth (Just ClaudeCodeProvider)
        _ -> loadAuthForAccount provider selectionId

reportProviderUnavailable
    :: Maybe FullscreenRuntime
    -> ApiError
    -> IO ()
reportProviderUnavailable fullscreen apiError = do
    now <- getCurrentTime
    let leading = "No usable fallback provider account is available.\n"
        message = leading <> formatApiErrorAt now apiError
    case fullscreen of
        Nothing -> do
            color <- resolveColor stderr
            putTextLn stderr (roleError color message)
        Just runtime ->
            case (apiError, formatApiErrorRetryCountdownParts apiError) of
                (CredentialsExhausted{retryAt}, Just (prefix, suffix)) ->
                    let remainingMillis =
                            max 0
                                (ceiling
                                    ( realToFrac (diffUTCTime retryAt now)
                                        * 1000
                                        :: Double
                                    ))
                    in emitUiEvent runtime
                        (UiRetryCountdown
                            (leading <> prefix)
                            remainingMillis
                            suffix)
                _ ->
                    emitUiEvent runtime (UiErrorMessage message)

applyModelChange
    :: OsPath
    -> OsPath
    -> Provider
    -> Text
    -> Text
    -> Text
    -> DialectId
    -> IORef ResponseCreateParams
    -> RenderConfig
    -> IORef LiveConversation
    -> Persistence
    -> IO Text
applyModelChange
        home projectRoot provider connection name transportModel dialectId
        paramsRef render previous persist = do
    modifyIORef' paramsRef (setRequestModel provider name)
    writeIORef render.renderModelRef name
    persistModelSwitch TopLevelSwitch home projectRoot ModelTarget
        { targetProvider = provider
        , targetConnectionId = connection
        , targetModelId = name
        , targetWireModelId = transportModel
        , targetDialect = dialectId
        }
    clearedChain <- case provider of
        OpenAIProvider ->
            do
                prev <- readLivePreviousResponseId previous
                writeLivePreviousResponseId previous Nothing
                pure (isJust prev)
        _ -> pure False
    case persist of
        PersistenceDisabled -> pure ()
        PersistenceEnabled slotRef -> do
            slot <- readIORef slotRef
            case slot of
                PersistencePending pending sessionId tempDir ->
                    writeIORef slotRef
                        (PersistencePending
                            pending
                                { createTarget = ModelTarget
                                    { targetProvider = provider
                                    , targetConnectionId = connection
                                    , targetModelId = name
                                    , targetWireModelId = transportModel
                                    , targetDialect = dialectId
                                    }
                                }
                            sessionId
                            tempDir)
                PersistenceActive handle -> do
                    let meta = handle.sessionMeta
                            { metaConnection = connection
                            , metaModel = name
                            , metaTransportModel = Just transportModel
                            , metaDialect = dialectId
                            }
                    writeSessionMeta
                        handle.sessionPool
                        handle.sessionMetaPath
                        meta
                    writeIORef slotRef
                        (PersistenceActive handle { sessionMeta = meta })
    pure $
        "model set to "
            <> name
            <> if clearedChain
                then " (conversation continued locally)"
                else ""

requestModelTargetSwitch
    :: Maybe FullscreenRuntime
    -> ModelOption
    -> Persistence
    -> IO (Either Text RunResult)
requestModelTargetSwitch fullscreen choice persist =
    prepareProviderTransition
        ManualTransition Set.empty Nothing choice persist >>= \case
            Left err -> pure (Left err)
            Right transition -> do
                color <- resolveColor stdout
                let message =
                        "switching to "
                            <> providerSlug choice.modelTarget.targetProvider
                            <> "/"
                            <> choice.modelTarget.targetModelId
                            <> " (conversation continued locally)"
                case fullscreen of
                    Nothing ->
                        Text.putStrLn
                            (roleMuted color (glyphOk <> message))
                    Just runtime ->
                        emitUiEvent runtime (UiSystemMessage message)
                pure (Right (RunSwitchProvider transition))

requestAccountProviderSwitch
    :: ModelCatalog
    -> Maybe FullscreenRuntime
    -> Provider
    -> Text
    -> Text
    -> Text
    -> DialectId
    -> Provider
    -> Text
    -> Text
    -> Text
    -> Persistence
    -> IO (Either Text RunResult)
requestAccountProviderSwitch
    catalog
    fullscreen
    currentProvider
    currentConnection
    currentModelId
    currentAccount
    currentDialect
    selectedProvider
    selectionId
    accountId
    selectedAccount
    persist = do
        currentTransportModel <-
            persistenceTransportModel currentModelId persist
        let rawChoice =
                accountSwitchTarget
                    catalog
                    currentProvider
                    currentConnection
                    currentModelId
                    currentTransportModel
                    currentDialect
                    selectedProvider
        choice <-
            if currentProvider == selectedProvider
                then pure rawChoice
                else resolveModelOptionDialect rawChoice
        let setPromptTarget modelId account =
                forM_ fullscreen $ \runtime ->
                    emitUiEvent runtime (UiSetPromptTarget modelId account)
        withOptimisticPromptTarget
            (setPromptTarget
                choice.modelTarget.targetModelId
                selectedAccount)
            (setPromptTarget currentModelId currentAccount) $
            validateSelectedAccountTarget
                selectedProvider
                selectionId
                accountId >>= \case
                Left err -> pure (Left err)
                Right () -> do
                    sessionId <- ensureTransitionSessionId persist
                    let transition = ProviderTransition
                            { transitionTarget = choice.modelTarget
                            , transitionEffort = Nothing
                            , transitionAccountSelectionId =
                                Just selectionId
                            , transitionAccountId = Just accountId
                            , transitionSessionId = sessionId
                            , transitionPendingTurn = Nothing
                            , transitionUnavailableProviders = Set.empty
                            , transitionCause = ManualTransition
                            , transitionAutomaticBilling = Nothing
                            }
                        modelMessage
                            | currentProvider == selectedProvider =
                                providerSlug selectedProvider
                                    <> "/"
                                    <> choice.modelTarget.targetModelId
                            | otherwise =
                                providerSlug selectedProvider
                                    <> "/"
                                    <> choice.modelTarget.targetModelId
                                    <> " (provider changed)"
                    color <- resolveColor stdout
                    case fullscreen of
                        Nothing ->
                            Text.putStrLn
                                (roleMuted color
                                    (glyphOk
                                        <> "switching to "
                                        <> modelMessage))
                        Just runtime ->
                            emitUiEvent runtime $
                                UiSystemMessage
                                    ("switching to " <> modelMessage)
                    pure (Right (RunSwitchProvider transition))

accountSwitchTarget
    :: ModelCatalog
    -> Provider
    -> Text
    -> Text
    -> Text
    -> DialectId
    -> Provider
    -> ModelOption
accountSwitchTarget
        catalog currentProvider currentConnection currentModelId
        currentTransportModel currentDialect
        selectedProvider =
    if currentProvider == selectedProvider
        then
            let current = rawModelOption selectedProvider currentModelId
            in current
                { modelTarget = current.modelTarget
                    { targetConnectionId = currentConnection
                    , targetWireModelId = currentTransportModel
                    , targetDialect = currentDialect
                    }
                }
        else
            defaultModelOptionFor catalog selectedProvider

persistenceTransportModel :: Text -> Persistence -> IO Text
persistenceTransportModel fallback = \case
    PersistenceDisabled -> pure fallback
    PersistenceEnabled slotRef ->
        readIORef slotRef >>= \case
            PersistencePending pending _ _ ->
                pure pending.createTarget.targetWireModelId
            PersistenceActive handle ->
                pure $
                    fromMaybe
                        fallback
                        handle.sessionMeta.metaTransportModel

validateSelectedAccountTarget
    :: Provider
    -> Text
    -> Text
    -> IO (Either Text ())
validateSelectedAccountTarget provider selectionId accountId =
    loadSelectedAccountAuth provider selectionId accountId >>= \case
        Left err ->
            pure $ Left $
                "cannot switch to "
                    <> providerSlug provider
                    <> " account: "
                    <> err
        Right loaded ->
            probeLoadedAvailability loaded >>= \case
                Left err -> do
                    now <- getCurrentTime
                    pure $ Left $
                        "cannot switch to "
                            <> providerSlug provider
                            <> " account: "
                            <> formatApiErrorInlineAt now err
                Right usable
                    | usable.loadedProvider /= provider ->
                        pure $ Left $
                            "cannot switch to "
                                <> providerSlug provider
                                <> " account: auth resolved "
                                <> providerSlug usable.loadedProvider
                    | otherwise -> pure (Right ())

requestAutomaticProviderFallback
    :: SessionEnv
    -> ApiError
    -> PendingTurn
    -> IO (Maybe ProviderTransition)
requestAutomaticProviderFallback env apiError pending = do
    readIORef env.sessionGatewayModels >>= \case
        Just _ -> pure Nothing
        Nothing -> do
            forM_ env.sessionFullscreen \runtime ->
                emitUiEvent runtime UiTurnRestarted
            sessionId <- ensureTransitionSessionId env.sessionPersist
            unavailable <- readIORef env.sessionUnavailableProviders
            currentModel <-
                fromMaybe "" . (.model) <$> readIORef env.sessionParams
            case env.sessionTokenProvider of
                Nothing -> pure Nothing
                Just tokenProvider ->
                    chooseAutomaticProviderTransition
                        env.sessionProviderFallback
                        env.sessionModelCatalog
                        env.sessionProjectRoot
                        env.sessionRender.renderStderr
                        env.sessionFullscreen
                        (tokenProviderBillingMode tokenProvider)
                        env.sessionProvider
                        currentModel
                        unavailable
                        sessionId
                        pending
                        apiError

requestStartupProviderFallback
    :: SessionEnv
    -> ApiError
    -> IO (Maybe ProviderTransition)
requestStartupProviderFallback env apiError = do
    readIORef env.sessionGatewayModels >>= \case
        Just _ -> pure Nothing
        Nothing -> do
            unavailable <- readIORef env.sessionUnavailableProviders
            currentModel <-
                fromMaybe "" . (.model) <$> readIORef env.sessionParams
            case env.sessionTokenProvider of
                Nothing -> pure Nothing
                Just tokenProvider ->
                    chooseStartupProviderTransition
                        env.sessionProviderFallback
                        env.sessionModelCatalog
                        env.sessionProjectRoot
                        env.sessionFullscreen
                        (tokenProviderBillingMode tokenProvider)
                        env.sessionProvider
                        currentModel
                        unavailable
                        Nothing
                        apiError

continueAutomaticFallback
    :: Bool
    -> Maybe OsPath
    -> Maybe OsPath
    -> Handle
    -> Maybe FullscreenRuntime
    -> ProviderTransition
    -> ApiError
    -> IO (Maybe ProviderTransition)
continueAutomaticFallback
        fallbackEnabled homeHint cwdHint stderrHandle fullscreen failed apiError
    | not fallbackEnabled = pure Nothing
    | otherwise = case ( failed.transitionAutomaticBilling
                       , failed.transitionPendingTurn
                       ) of
        (Just billing, Just pending) -> do
            home <- maybe getHomeDirectory pure homeHint
            cwd <- maybe getCurrentDirectory pure cwdHint
            loadModelCatalogAt home cwd >>= \case
                Left _ -> pure Nothing
                Right catalog ->
                    chooseAutomaticProviderTransition
                        True
                        catalog
                        cwd
                        stderrHandle
                        fullscreen
                        billing
                        failed.transitionTarget.targetProvider
                        failed.transitionTarget.targetModelId
                        failed.transitionUnavailableProviders
                        failed.transitionSessionId
                        pending
                        apiError
        _ -> pure Nothing

chooseAutomaticProviderTransition
    :: Bool
    -> ModelCatalog
    -> OsPath
    -> Handle
    -> Maybe FullscreenRuntime
    -> BillingMode
    -> Provider
    -> Text
    -> Set Provider
    -> Maybe Text
    -> PendingTurn
    -> ApiError
    -> IO (Maybe ProviderTransition)
chooseAutomaticProviderTransition
    fallbackEnabled catalog cwd stderrHandle fullscreen
        sourceBilling current currentModel unavailable0 sessionId pending apiError
    | not fallbackEnabled = pure Nothing
    | otherwise = tryCandidates unavailable0 candidates
  where
    candidates =
        fallbackCandidates
            catalog unavailable0 current currentModel apiError

    tryCandidates unavailable = \case
        [] -> pure Nothing
        rawChoice : rest -> do
            choice <- resolveModelOptionDialect rawChoice
            validateAutomaticProviderTarget
                cwd
                sourceBilling
                choice >>= \case
                Left err -> do
                    let failedProvider =
                            choice.modelTarget.targetProvider
                        unavailable' =
                            markUnavailable failedProvider unavailable
                        remaining =
                            filter
                                ((/= failedProvider)
                                    . (.modelTarget.targetProvider))
                                rest
                        message =
                            "skipping "
                            <> providerSlug failedProvider
                            <> ": "
                            <> err
                    case fullscreen of
                        Nothing -> do
                            color <- resolveColor stderrHandle
                            putTextLn stderrHandle (roleMuted color message)
                        Just runtime ->
                            emitUiEvent runtime (UiSystemMessage message)
                    tryCandidates unavailable' remaining
                Right selected -> do
                    let nextProvider = choice.modelTarget.targetProvider
                        unavailable' =
                            if nextProvider == current
                                then unavailable
                                else markUnavailable current unavailable
                        message
                            | nextProvider == current =
                                currentModel
                                    <> " unavailable; trying this turn with "
                                    <> choice.modelTarget.targetModelId
                            | otherwise =
                                providerSlug current
                                    <> " unavailable; trying this turn with "
                                    <> providerSlug nextProvider
                                    <> "/"
                                    <> choice.modelTarget.targetModelId
                    case fullscreen of
                        Nothing -> do
                            color <- resolveColor stderrHandle
                            putTextLn stderrHandle
                                (roleWarn color (glyphWarn <> message))
                        Just runtime ->
                            emitUiEvent runtime (UiSystemMessage message)
                    pure $ Just ProviderTransition
                        { transitionTarget = choice.modelTarget
                        , transitionEffort = Nothing
                        , transitionAccountSelectionId =
                            (.selectedSelectionId) <$> selected
                        , transitionAccountId =
                            (.selectedAccountId) <$> selected
                        , transitionSessionId = sessionId
                        , transitionPendingTurn = Just pending
                        , transitionUnavailableProviders = unavailable'
                        , transitionCause = AutomaticFallback
                        , transitionAutomaticBilling = Just sourceBilling
                        }

chooseStartupProviderTransition
    :: Bool
    -> ModelCatalog
    -> OsPath
    -> Maybe FullscreenRuntime
    -> BillingMode
    -> Provider
    -> Text
    -> Set Provider
    -> Maybe Text
    -> ApiError
    -> IO (Maybe ProviderTransition)
chooseStartupProviderTransition
    fallbackEnabled catalog cwd fullscreen sourceBilling current currentModel
        unavailable0 sessionId apiError
    | not fallbackEnabled = pure Nothing
    | otherwise = tryCandidates unavailable0 candidates
  where
    candidates =
        fallbackCandidates
            catalog unavailable0 current currentModel apiError

    tryCandidates unavailable = \case
        [] -> pure Nothing
        rawChoice : rest -> do
            choice <- resolveModelOptionDialect rawChoice
            validateAutomaticProviderTarget
                cwd
                sourceBilling
                choice >>= \case
                Left err -> do
                    let failedProvider =
                            choice.modelTarget.targetProvider
                        unavailable' =
                            markUnavailable failedProvider unavailable
                        remaining =
                            filter
                                ((/= failedProvider)
                                    . (.modelTarget.targetProvider))
                                rest
                        message =
                            "skipping "
                            <> providerSlug failedProvider
                            <> ": "
                            <> err
                    forM_ fullscreen \runtime ->
                        emitUiEvent runtime (UiSystemMessage message)
                    tryCandidates unavailable' remaining
                Right selected -> do
                    let nextProvider = choice.modelTarget.targetProvider
                        unavailable' =
                            if nextProvider == current
                                then unavailable
                                else markUnavailable current unavailable
                        message
                            | nextProvider == current =
                                currentModel
                                    <> " unavailable; switched to "
                                    <> choice.modelTarget.targetModelId
                            | otherwise =
                                providerSlug current
                                    <> " account unavailable; switched to "
                                    <> providerSlug nextProvider
                                    <> "/"
                                    <> choice.modelTarget.targetModelId
                    forM_ fullscreen \runtime ->
                        emitUiEvent runtime (UiSystemMessage message)
                    pure $ Just ProviderTransition
                        { transitionTarget = choice.modelTarget
                        , transitionEffort = Nothing
                        , transitionAccountSelectionId =
                            (.selectedSelectionId) <$> selected
                        , transitionAccountId =
                            (.selectedAccountId) <$> selected
                        , transitionSessionId = sessionId
                        , transitionPendingTurn = Nothing
                        , transitionUnavailableProviders = unavailable'
                        , transitionCause = AutomaticFallback
                        , transitionAutomaticBilling = Just sourceBilling
                        }

prepareProviderTransition
    :: TransitionCause
    -> Set Provider
    -> Maybe PendingTurn
    -> ModelOption
    -> Persistence
    -> IO (Either Text ProviderTransition)
prepareProviderTransition cause unavailable pending rawChoice persist = do
    choice <- resolveModelOptionDialect rawChoice
    validateProviderTarget choice >>= \case
        Left err -> pure (Left err)
        Right () -> do
            sessionId <- ensureTransitionSessionId persist
            pure $ Right ProviderTransition
                { transitionTarget = choice.modelTarget
                , transitionEffort = Nothing
                , transitionAccountSelectionId = Nothing
                , transitionAccountId = Nothing
                , transitionSessionId = sessionId
                , transitionPendingTurn = pending
                , transitionUnavailableProviders = unavailable
                , transitionCause = cause
                , transitionAutomaticBilling = Nothing
                }

validateProviderTarget :: ModelOption -> IO (Either Text ())
validateProviderTarget choice =
    if choice.modelTarget.targetConnectionId
        `notElem` map builtinConnectionId
            [OpenAIProvider, XAIProvider, OpenRouterProvider, GeminiProvider]
    then pure (Right ())
    else fmap (() <$) $
        loadValidatedProviderTarget probeLoadedAvailability choice

validateAutomaticProviderTarget
    :: OsPath
    -> BillingMode
    -> ModelOption
    -> IO (Either Text (Maybe SelectedAccount))
validateAutomaticProviderTarget cwd sourceBilling choice = do
    let provider = choice.modelTarget.targetProvider
    if not (providerSupportsUsageAccountSelection provider)
        then fmap (Nothing <$) $
            loadValidatedProviderTarget
                probeLoadedAutomaticAvailability
                choice
        else do
            settings <- resolveProjectRoot cwd >>= loadProjectSettings
            let rememberedIds = fmap
                    (\account ->
                        ( account.projectAccountSelectionId
                        , account.projectAccountId
                        ))
                    (projectAccountFor provider settings)
                requiredBilling = case sourceBilling of
                    SubscriptionBilled -> Just SubscriptionBilled
                    ApiBilled -> Nothing
            selectProviderAccount
                provider
                requiredBilling
                rememberedIds >>= \case
                    Left err -> pure (Left err)
                    Right selected ->
                        loadSelectedAccountAuth
                            provider
                            selected.selectedSelectionId
                            selected.selectedAccountId >>= \case
                                Left err -> pure (Left err)
                                Right loaded ->
                                    probeLoadedAutomaticAvailability loaded >>= \case
                                        Left err -> do
                                            now <- getCurrentTime
                                            pure $ Left $
                                                "cannot switch to "
                                                    <> providerSlug provider
                                                    <> ": "
                                                    <> formatApiErrorInlineAt now err
                                        Right usable
                                            | allowsAutomaticBillingFallback
                                                sourceBilling
                                                (tokenProviderBillingMode
                                                    usable.loadedTokenProvider) ->
                                                    pure (Right (Just selected))
                                            | otherwise ->
                                                pure $ Left
                                                    "automatic fallback from subscription \
                                                    \billing to API credits is disabled"

loadValidatedProviderTarget
    :: (LoadedAuth -> IO (Either ApiError LoadedAuth))
    -> ModelOption
    -> IO (Either Text LoadedAuth)
loadValidatedProviderTarget probeAvailability choice =
    if not
        (connectionSupportsDialect
            choice.modelTarget.targetConnectionId
            choice.modelTarget.targetProvider
            choice.modelTarget.targetDialect)
    then
        pure $ Left $
            "dialect "
                <> dialectSlug choice.modelTarget.targetDialect
                <> " is incompatible with provider "
                <> providerSlug choice.modelTarget.targetProvider
    else loadAuth (Just choice.modelTarget.targetProvider) >>= \case
        Left err -> pure $ Left $
            "cannot switch to "
                <> providerSlug choice.modelTarget.targetProvider
                <> ": "
                <> err
        Right loaded ->
            probeAvailability loaded >>= \case
                Left err -> do
                    now <- getCurrentTime
                    pure $ Left $
                        "cannot switch to "
                            <> providerSlug choice.modelTarget.targetProvider
                            <> ": "
                            <> formatApiErrorInlineAt now err
                Right usable
                    | usable.loadedProvider /= choice.modelTarget.targetProvider ->
                        pure $ Left $
                            "cannot switch to "
                                <> providerSlug choice.modelTarget.targetProvider
                                <> ": auth resolved "
                                <> providerSlug usable.loadedProvider
                    | otherwise -> pure (Right usable)

ensureTransitionSessionId
    :: Persistence
    -> IO (Maybe Text)
ensureTransitionSessionId PersistenceDisabled = pure Nothing
ensureTransitionSessionId (PersistenceEnabled slotRef) = do
    handle <- ensureSession slotRef
    pure (Just handle.sessionMeta.metaId)

commitProviderTransition
    :: ModelSwitchScope
    -> OsPath
    -> OsPath
    -> Maybe ProviderTransition
    -> Persistence
    -> IO ()
commitProviderTransition _ _ _ Nothing _ = pure ()
commitProviderTransition scope home projectRoot (Just transition) persist = do
    persistModelSwitch scope home projectRoot transition.transitionTarget
    case persist of
        PersistenceDisabled -> pure ()
        PersistenceEnabled slotRef -> do
            slot <- readIORef slotRef
            case slot of
                PersistencePending pending sessionId tempDir ->
                    writeIORef slotRef $ PersistencePending
                        pending
                            { createTarget = transition.transitionTarget
                            , createEffort =
                                maybe
                                    pending.createEffort
                                    reasoningEffortText
                                    transition.transitionEffort
                            }
                        sessionId
                        tempDir
                PersistenceActive handle -> do
                    now <- getCurrentTime
                    let previousMeta = handle.sessionMeta
                        meta = previousMeta
                            { metaProvider = transition.transitionTarget.targetProvider
                            , metaConnection =
                                transition.transitionTarget.targetConnectionId
                            , metaModel = transition.transitionTarget.targetModelId
                            , metaTransportModel =
                                Just transition.transitionTarget.targetWireModelId
                            , metaDialect = transition.transitionTarget.targetDialect
                            , metaEffort =
                                maybe
                                    previousMeta.metaEffort
                                    reasoningEffortText
                                    transition.transitionEffort
                            , metaLegacySubagentTarget =
                                Just
                                    (sessionLegacySubagentTarget previousMeta)
                            , metaLastResponseId = Nothing
                            , metaUpdatedAt = now
                            }
                    writeSessionMeta
                        handle.sessionPool
                        handle.sessionMetaPath
                        meta
                    writeIORef slotRef
                        (PersistenceActive handle { sessionMeta = meta })

prepareTransitionBackend
    :: ModelSwitchScope
    -> OsPath
    -> OsPath
    -> Maybe ProviderTransition
    -> Persistence
    -> Backend
    -> IO Backend
prepareTransitionBackend _ _ _ Nothing _ backend = pure backend
prepareTransitionBackend scope home projectRoot (Just transition) persist backend
    | transitionCommitsImmediately transition = do
        commitProviderTransition scope home projectRoot (Just transition) persist
        pure backend
    | otherwise = do
        committed <- newIORef False
        pure $
            commitBackendOnSuccess
                scope home projectRoot committed transition persist backend

commitBackendOnSuccess
    :: ModelSwitchScope
    -> OsPath
    -> OsPath
    -> IORef Bool
    -> ProviderTransition
    -> Persistence
    -> BackendMiddleware
commitBackendOnSuccess
        scope home projectRoot committed transition persist backend =
    backendWithCallbacks \state previous inputs callbacks -> do
        result <-
            backend.submitTurnWithCallbacks state previous inputs callbacks
        case result of
            Right _ -> do
                shouldCommit <- atomicModifyIORef' committed \done ->
                    (True, not done)
                when shouldCommit $
                    commitProviderTransition
                        scope home projectRoot (Just transition) persist
            Left _ -> pure ()
        pure result

markUnavailable :: Provider -> Set Provider -> Set Provider
markUnavailable = Set.insert

reloadAuth :: Provider -> Maybe TokenProvider -> IO (Either Text Text)
reloadAuth ClaudeCodeProvider _ =
    loadClaudeCodeAuth >>= \case
        Left err -> pure (Left ("reload-auth failed: " <> err))
        Right auth ->
            pure $ Right $
                "auth status rechecked (claude-code account "
                    <> auth.accountLabel
                    <> ")"
reloadAuth provider maybeTokenProvider =
    case maybeTokenProvider of
        Nothing ->
            pure $ Right $
                "reload-auth: OpenAI WebSocket auth is fixed for this process; "
                    <> "restart after refreshing ~/.codex/auth.json "
                    <> "(OAuth pools already rotate on handshake failure)"
        Just tokenProvider ->
            -- Force a disk/env re-read by rejecting the credential that is
            -- actually active. Switchable providers intentionally ignore failures
            -- from older accounts, so a fabricated empty account id is insufficient.
            getNextToken tokenProvider Nothing >>= \case
                Left err -> do
                    now <- getCurrentTime
                    pure $ Left $
                        "reload-auth failed: " <> formatApiErrorInlineAt now err
                Right current ->
                    getNextToken tokenProvider (Just FailedCredential
                        { credential = current
                        , failure = AccountAuthenticationRejected
                        , failureReason = ExhaustedByAuthentication
                            { exhaustionErrorType = Nothing
                            , exhaustionStatusCode = Nothing
                            }
                        }) >>= \case
                        Left err -> do
                            now <- getCurrentTime
                            pure $ Left $
                                "reload-auth failed: "
                                    <> formatApiErrorInlineAt now err
                        Right credential ->
                            pure $ Right $
                                "auth reloaded ("
                                    <> providerSlug provider
                                    <> " account "
                                    <> credential.accountId
                                    <> ")"
