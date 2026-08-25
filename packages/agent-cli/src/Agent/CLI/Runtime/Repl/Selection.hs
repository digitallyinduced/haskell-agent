-- | Model, effort, and account selection commands.
module Agent.CLI.Runtime.Repl.Selection
    ( handleSelectionAction
    , handleSelectionInput
    ) where

import Agent.CLI.AccountPicker
    ( AccountPickerOption(..),
      accountPickerMatches,
      accountPickerRow,
      loadAllAccountPickerOptions )
import Agent.CLI.AccountSelection ()
import Agent.CLI.Afk ()
import Agent.CLI.AgentSessions ()
import Agent.CLI.AgentViewport ()
import Agent.CLI.Approval ()
import Agent.CLI.Artifact ()
import Agent.CLI.Auth ()
import Agent.CLI.Clipboard ()
import Agent.CLI.Command
    ( currentEffort,
      currentModel,
      ReplAction(ReplSetModel, ReplShowEffort, ReplSetEffort,
                 ReplShowModel) )
import Agent.CLI.Compaction ()
import Agent.CLI.Config ()
import Agent.CLI.Connectivity ()
import Agent.CLI.Database ()
import Agent.CLI.Database.Store ()
import Agent.CLI.Dialects ()
import Agent.CLI.Error ( formatApiErrorInlineAt )
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
import Agent.CLI.ModelConfig ()
import Agent.CLI.Models
    ( modelTargetRequiresRebuild,
      rawModelOption,
      resolveConfiguredModel,
      resolveModelOptionDialect,
      ModelOption(modelTarget),
      ModelTarget(targetDialect, targetProvider, targetModelId,
                  targetConnectionId, targetWireModelId) )
import Agent.CLI.Options ()
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
import Agent.CLI.ProviderTransition ()
import Agent.CLI.Recap ()
import Agent.CLI.Render ( clearThinking, renderEvent )
import Agent.CLI.ReplMode ()
import Agent.CLI.Request ()
import Agent.CLI.Runtime.HistorySource ()
import Agent.CLI.Runtime.Persistence ()
import Agent.CLI.Runtime.Recap ()
import Agent.CLI.Runtime.Types ( RunResult )
import Agent.CLI.Secret ()
import Agent.CLI.Session ()
import Agent.CLI.Session.Attachments ()
import Agent.CLI.Session.Choices
    ( atMay, effortChoice, modelChoice )
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
import Agent.OpenRouter.LoopBackend ()
import Agent.OsPath ()
import Agent.Provider
    ( Provider(ClaudeCodeProvider, OpenAIProvider),
      providerSlug,
      tokenProviderBillingMode )
import Agent.Responses.GenericBackend ()
import Agent.Responses.GenericClient ()
import Agent.Responses.Types ()
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
import Data.Text ()
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
import qualified Data.Text as Text ()
import qualified Data.Text.IO as Text ( putStrLn, hPutStrLn )
import qualified Agent.XAI.Options as XAI ()
import qualified Agent.XAI.Usage as XAIUsage ()

handleSelectionInput :: SessionEnv -> IO RunResult -> ReplLine -> IO RunResult
handleSelectionInput env continue input =
    handleSelection env continue (Left input)

handleSelectionAction :: SessionEnv -> IO RunResult -> ReplAction -> IO RunResult
handleSelectionAction env continue action =
    handleSelection env continue (Right action)

handleSelection
    :: SessionEnv
    -> IO RunResult
    -> Either ReplLine ReplAction
    -> IO RunResult
handleSelection
        env@SessionEnv
            { sessionRender = render
            , sessionConversation = conversationRef
            , sessionProvider = provider
            , sessionConnection = connectionId
            , sessionModelCatalog = catalog
            , sessionDialect = dialect
            , sessionParams = paramsRef
            , sessionPersist = persist
            , sessionProjectRoot = projectRoot
            , sessionDraft = draftRef
            , sessionAccountId = accountIdRef
            , sessionAccountSelectionId = selectionRef
            , sessionSelectAccount = selectAccount
            , sessionTokenProvider = tokenProvider
            , sessionFullscreen = fullscreen
            }
        continue = \case
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
        let message = "effort: " <> currentEffort params
        displayInfo message $
            Text.putStrLn
                (roleMuted color (glyphSession <> message))
        continue
    Right (ReplSetEffort level) -> do
        setEffort level
        continue
    Right ReplShowModel -> do
        chooseModel continue
    Right (ReplSetModel name) -> do
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
