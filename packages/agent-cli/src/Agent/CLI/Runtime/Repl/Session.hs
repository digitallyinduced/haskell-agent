-- | Persisted-session, handoff, and worktree command handling.
module Agent.CLI.Runtime.Repl.Session
    ( handleSessionAction
    ) where

import Agent.CLI.AccountPicker ()
import Agent.CLI.AccountSelection ()
import Agent.CLI.Afk
    ( AfkTarget(..), handoffLocal, handoffRemote, parseAfkTarget )
import Agent.CLI.AgentSessions ()
import Agent.CLI.AgentViewport ()
import Agent.CLI.Approval ()
import Agent.CLI.Artifact ()
import Agent.CLI.Auth ()
import Agent.CLI.Clipboard ()
import Agent.CLI.Command
    ( currentEffort,
      currentModel,
      ForkRequest(..),
      ReplAction(ReplRenameAuto, ReplResume, ReplSearch, ReplHome, ReplRewind, ReplClear,
                 ReplNew, ReplDelete, ReplShowSession, ReplShowSessionInfo, ReplAfk,
                 ReplWorktree, ReplRename, ReplFork),
      ShellMode(ShellNone, ShellGhci, ShellBash, ShellBoth),
      SlashCatalog(slashCatalogToolNames) )
import Agent.CLI.Compaction ()
import Agent.CLI.Config ()
import Agent.CLI.Connectivity ()
import Agent.CLI.Database ()
import Agent.CLI.Database.Store ()
import Agent.CLI.Dialects ()
import Agent.CLI.Error ()
import Agent.CLI.GatewayBridge ()
import Agent.CLI.Input ( readChoiceSelection, readChoiceSelectionAt )
import Agent.CLI.Interrupt ()
import Agent.CLI.LearnedSkills ()
import Agent.CLI.LearnedSkills.Store ()
import Agent.CLI.Login ()
import Agent.CLI.Lsp ()
import Agent.CLI.ManagedTurn ()
import Agent.CLI.McpManager ()
import Agent.CLI.McpStatus ()
import Agent.CLI.ModelConfig ()
import Agent.CLI.Models
    ( ModelTarget(targetModelId, ModelTarget, targetProvider,
                  targetConnectionId, targetWireModelId, targetDialect) )
import Agent.CLI.Options ()
import Agent.CLI.PendingInputs ()
import Agent.CLI.Plan ()
import Agent.CLI.Progress ()
import Agent.CLI.Project ()
import Agent.CLI.Prompt ()
import Agent.CLI.PromptHooks ()
import Agent.CLI.Provider.OpenAI ()
import Agent.CLI.Provider.Switch ()
import Agent.CLI.ProviderAvailability ()
import Agent.CLI.ProviderFallback ()
import Agent.CLI.ProviderTransition ()
import Agent.CLI.Recap ()
import Agent.CLI.Render ( clearThinking, putTextLn, renderEvent )
import Agent.CLI.ReplMode ()
import Agent.CLI.Request ()
import Agent.CLI.Runtime.HistorySource
    ( reloadFullscreenHistoryForHandle )
import Agent.CLI.Runtime.Persistence ()
import Agent.CLI.Runtime.Recap ()
import Agent.CLI.Runtime.Types
    ( RunResult(RunDeleteSession, RunForkSession, RunSwitchWorktree, RunRestart,
                RunQuit) )
import Agent.CLI.Secret ()
import Agent.CLI.Session
    ( TranscriptEffect(TranscriptReset),
      appendTurnKeepTitleIndexed,
      appendTurnWithPromptResetIndexed,
      createSession,
      forkSessionAt,
      loadSession,
      rewindSession,
      removeSessionTemp,
      resetSessionTitleToAuto,
      sessionConversationText,
      sessionRewindChoices,
      sessionsRoot,
      setManualSessionTitle,
      writeSessionMeta,
      Persistence(PersistenceEnabled, PersistenceDisabled),
      PersistenceState(PersistenceActive, PersistencePending),
      SessionCreate(createCwd, SessionCreate, createPool, createEffort,
                    createTarget, createTitleHint, createTitleIsManual, createRoot),
      SessionHandle(sessionMeta, sessionPool, sessionMetaPath,
                    sessionTempDir, sessionDir),
      SessionMeta(metaTitle, metaLastResponseId, metaUpdatedAt,
                  metaInputTokens, metaOutputTokens, metaCachedTokens, metaLastRecap,
                  metaLastTurnSummary, metaLastRecapMainTurns, metaTransportModel,
                  metaTitleUserTurns, metaPromptSnapshot, metaId, metaCwd),
      SessionTransfer(transferTurns, SessionTransfer, transferMeta),
      SessionTurn(turnUsage, SessionTurn, turnAt, turnUserText,
                  turnAssistantText, turnError, turnResponseId, turnEffect,
                  turnItems, turnProviderTelemetry) )
import Agent.CLI.Session.Attachments ()
import Agent.CLI.Session.Choices ()
import Agent.CLI.Session.History ()
import Agent.CLI.Session.Interaction ()
import Agent.CLI.Session.Lifecycle ()
import Agent.CLI.Session.Runtime.Types ()
import Agent.CLI.Session.Selection
    ( handleConversationSearch, handleResume )
import Agent.CLI.SessionAdmin ()
import Agent.CLI.SessionEnv ( SessionEnv(..) )
import Agent.CLI.SessionLock ()
import Agent.CLI.SessionState ()
import Agent.CLI.SessionTitle
    ( invalidateSessionTitles, requestSessionTitle )
import Agent.CLI.Skills ()
import Agent.CLI.Startup.Auth ()
import Agent.CLI.Startup.Format ()
import Agent.CLI.StartupContext ()
import Agent.CLI.Status ( formatTokenUsageOrZero )
import Agent.CLI.Style
    ( cliWindowTitle, glyphOk, glyphSession, roleError, roleMuted, rolePrompt )
import Agent.CLI.Subagents.Runtime ()
import Agent.CLI.TUI.App
    ( commitFullscreenHistoryTurn
    , emitUiEvent
    , requestFullscreenChoiceWithBody
    )
import Agent.CLI.TUI.SessionHistory ( sessionHistoryTurn )
import Agent.CLI.TUI.Types ( HistoryCommit(..) )
import Agent.CLI.Terminal ( resolveColor )
import Agent.CLI.Tools ()
import Agent.CLI.Turn ()
import Agent.CLI.Usage ()
import Agent.CLI.WebFetch ()
import Agent.CLI.Worktree
    ( createManagedWorktreeWithProgress
    , removeWorktree
    , worktreeProgressMessage
    )
import Agent.Cancel ()
import Agent.Claude ()
import Agent.Dialect ( dialectId, dialectSlug )
import Agent.Error ()
import Agent.GrokBuild.Dialect.Goal ()
import Agent.GrokBuild.Dialect.Runtime ()
import Agent.GrokBuild.Dialect.Workflow ()
import Agent.Loop ( LoopEvent(ActivityUpdated) )
import Agent.OpenAI.Compaction
    ( clearSessionUserText, newSessionUserText )
import Agent.OpenAI.Usage ()
import Agent.OpenAI.WebSocketClient ()
import Agent.OpenRouter.LoopBackend ()
import Agent.OsPath ( toText )
import Agent.Provider ( providerSlug )
import Agent.ReasoningEffort (reasoningEffortText)
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
      UiEvent(UiConversationCleared, UiSystemMessage, UiErrorMessage,
              UiSetNotice) )
import Agent.TUI.Motion ()
import Agent.ToolDispatch ()
import Agent.Tools.MultiAgents ()
import Agent.Tools.PlanMode ( PlanModeEnv(planSessionDir) )
import Agent.Tools.Secret ()
import Agent.Tools.Types ()
import Agent.XAI.LoopBackend ()
import Control.Applicative ()
import Control.Concurrent.Async ()
import Control.Concurrent.Chan ()
import Control.Concurrent.MVar ()
import Control.Concurrent.STM ()
import Control.Exception ()
import Control.Exception.Safe ( finally, mask, onException )
import Control.Monad ( forM_, void )
import Data.IORef ( readIORef, writeIORef )
import Data.List ()
import Data.Maybe ( fromMaybe )
import Data.Text ()
import Data.Time.Clock ( getCurrentTime )
import System.Console.ANSI ()
import System.Console.ANSI.Codes ()
import System.Directory.OsPath ()
import System.Environment ()
import System.Exit ()
import System.IO ( stdout, stderr )
import System.OsPath ( takeDirectory )
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
import qualified Data.Set as Set ( toAscList )
import qualified Data.Text as Text
    ( intercalate, length, pack, strip, take, unlines, unwords, words )
import qualified Data.Text.IO as Text ( putStrLn, hPutStrLn )
import qualified Agent.XAI.Options as XAI ()
import qualified Agent.XAI.Usage as XAIUsage ()

handleSessionAction
    :: SessionEnv
    -> SlashCatalog
    -> IO RunResult
    -> ReplAction
    -> IO RunResult
handleSessionAction
        env@SessionEnv
            { sessionRender = render
            , sessionDatabasePool = databasePool
            , sessionFullscreen = fullscreen
            , sessionPersist = persist
            , sessionReset = sessionReset
            , sessionParams = paramsRef
            , sessionProvider = provider
            , sessionConnection = connectionId
            , sessionDialect = dialect
            , sessionPlanMode = planMode
            , sessionStoreRoot = storeRoot
            , sessionSetWindowTitle = setWindowTitle
            , sessionUsage = usageRef
            , sessionCwd = cwd
            }
        slashCatalog
        continue = \case
    ReplResume maybeId -> do
        handleResume databasePool fullscreen maybeId persist >>= \case
            Nothing -> continue
            Just result -> pure result
    ReplSearch query -> do
        handleConversationSearch
            databasePool fullscreen query persist >>= \case
                Nothing -> continue
                Just result -> pure result
    ReplHome ->
        handleResume databasePool fullscreen Nothing persist >>= \case
            Nothing -> continue
            Just result -> pure result
    ReplRewind -> do
        color <- resolveColor stderr
        let unavailable message = do
                displayError message $
                    putTextLn stderr (roleError color message)
                continue
        case persist of
            PersistenceDisabled ->
                unavailable "/rewind requires a persisted interactive session"
            PersistenceEnabled slotRef ->
                readIORef slotRef >>= \case
                    PersistencePending{} ->
                        unavailable
                            "/rewind is available after the first persisted turn"
                    PersistenceActive source ->
                        withReplActivity
                            (\report -> do
                                report "Loading rewind points…"
                                loadSession
                                    databasePool
                                    (takeDirectory source.sessionDir)
                                    source.sessionMeta.metaId)
                            >>= \case
                                Left err -> unavailable err
                                Right (meta, turns) ->
                                    case sessionRewindChoices turns of
                                        [] ->
                                            unavailable
                                                "No prompt is available to rewind."
                                        choices ->
                                            chooseRewindPoint color choices >>= \case
                                                Nothing -> continue
                                                Just (prompt, retained) ->
                                                    confirmRewind
                                                        color
                                                        prompt.turnUserText
                                                        >>= \case
                                                            False -> continue
                                                            True -> mask \restore -> do
                                                                result <-
                                                                    restore $
                                                                        withReplActivity \report -> do
                                                                            report "Rewinding conversation…"
                                                                            rewindSession
                                                                                source
                                                                                    { sessionMeta = meta }
                                                                                retained
                                                                case result of
                                                                    Left err ->
                                                                        restore
                                                                            (unavailable err)
                                                                    Right updated -> do
                                                                        writeIORef
                                                                            slotRef
                                                                            (PersistenceActive updated)
                                                                        writeIORef
                                                                            env.sessionTitleTurnCount
                                                                            updated.sessionMeta.metaTitleUserTurns
                                                                        writeIORef
                                                                            env.sessionDraft
                                                                            prompt.turnUserText
                                                                        invalidateSessionTitles
                                                                            env.sessionTitleManager
                                                                            updated.sessionMeta.metaId
                                                                        pure
                                                                            (RunRestart
                                                                                updated.sessionMeta.metaId)
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
                                , turnProviderTelemetry = []
                                }
                        (handle', turnIndex) <-
                            appendTurnWithPromptResetIndexed handle turn \meta ->
                                meta
                                    { metaLastResponseId = Nothing
                                    , metaInputTokens = 0
                                    , metaOutputTokens = 0
                                    , metaCachedTokens = 0
                                    , metaLastRecap = Nothing
                                    , metaLastTurnSummary = Nothing
                                    , metaLastRecapMainTurns = 0
                                    }
                        let meta = handle'.sessionMeta
                        writeIORef slotRef
                            (PersistenceActive handle')
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
                    effort = reasoningEffortText (currentEffort params)
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
                        , turnProviderTelemetry = []
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
    ReplDelete -> do
        color <- resolveColor stderr
        let unavailable message = do
                displayError message $
                    putTextLn stderr (roleError color message)
                continue
        case persist of
            PersistenceDisabled ->
                unavailable "/delete requires a persisted interactive session"
            PersistenceEnabled slotRef ->
                readIORef slotRef >>= \case
                    PersistencePending{} ->
                        unavailable
                            "/delete is available after the first persisted turn"
                    PersistenceActive handle ->
                        confirmDelete color handle.sessionMeta.metaId >>= \case
                            False -> continue
                            True -> do
                                let message =
                                        "deleting session "
                                            <> handle.sessionMeta.metaId
                                            <> " after shutdown…"
                                displayInfo message $
                                    putTextLn stderr
                                        (roleMuted color message)
                                pure
                                    (RunDeleteSession
                                        handle.sessionMeta.metaId
                                        cwd)
    ReplFork request -> do
        color <- resolveColor stderr
        let failFork message = do
                displayError message $
                    putTextLn stderr (roleError color message)
                continue
        case persist of
            PersistenceDisabled ->
                failFork "/fork requires a persisted interactive session"
            PersistenceEnabled slotRef ->
                readIORef slotRef >>= \case
                    PersistencePending{} ->
                        failFork
                            "/fork is available after the first persisted turn"
                    PersistenceActive source ->
                        chooseForkWorktree color request.forkWorktree >>= \case
                            Nothing -> continue
                            Just useWorktree -> mask \restore -> do
                                destination <-
                                    restore $
                                        if useWorktree
                                            then
                                                withReplActivity \report ->
                                                    createManagedWorktreeWithProgress
                                                        (report
                                                            . worktreeProgressMessage)
                                                        env.sessionHome
                                                        source.sessionMeta.metaCwd
                                                    >>= pure . fmap
                                                        (\path ->
                                                            (path, Just path))
                                            else
                                                pure
                                                    (Right
                                                        ( source.sessionMeta.metaCwd
                                                        , Nothing
                                                        ))
                                case destination of
                                    Left err -> failFork err
                                    Right (newCwd, worktreePath) -> do
                                        let root =
                                                takeDirectory source.sessionDir
                                            cleanup =
                                                cleanupForkWorktree
                                                    source.sessionMeta.metaCwd
                                                    worktreePath
                                        result <-
                                            restore
                                                (withReplActivity \report -> do
                                                    report "Forking session…"
                                                    loadSession
                                                        databasePool
                                                        root
                                                        source.sessionMeta.metaId
                                                        >>= \case
                                                            Left err ->
                                                                pure (Left err)
                                                            Right (meta, turns) ->
                                                                forkSessionAt
                                                                    root
                                                                    source
                                                                        { sessionMeta =
                                                                            meta
                                                                        }
                                                                    turns
                                                                    Nothing
                                                                    newCwd)
                                                `onException` cleanup
                                        case result of
                                            Left err -> do
                                                cleanup
                                                failFork err
                                            Right forked -> do
                                                let message =
                                                        "forked session: "
                                                            <> forked.sessionMeta.metaId
                                                displayInfo message $
                                                    putTextLn stderr
                                                        (roleMuted color
                                                            (glyphOk <> message))
                                                pure
                                                    (RunForkSession
                                                        forked.sessionMeta.metaId
                                                        request.forkDirective)
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
            usageText = formatTokenUsageOrZero usage
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
                       , "effort: "
                            <> reasoningEffortText (currentEffort params)
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
        result <- withReplActivity \report ->
            createManagedWorktreeWithProgress
                (report . worktreeProgressMessage)
                env.sessionHome
                cwd
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
    _ -> error "handleSessionAction: unsupported action"
  where
    fullscreenEvent event = case fullscreen of
        Nothing -> pure ()
        Just runtime -> emitUiEvent runtime event
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
    withReplActivity action =
        action reportReplActivity `finally` clearReplActivity
    reportReplActivity message =
        case fullscreen of
            Nothing -> renderEvent render (ActivityUpdated message)
            Just runtime ->
                emitUiEvent runtime
                    (UiSetNotice (Just (progressNotice message)))
    clearReplActivity =
        case fullscreen of
            Nothing -> clearThinking render
            Just runtime -> emitUiEvent runtime (UiSetNotice Nothing)
    chooseForkWorktree color = \case
        Just value -> pure (Just value)
        Nothing ->
            case fullscreen of
                Just runtime ->
                    requestFullscreenChoiceWithBody
                        runtime
                        "Fork session"
                        "Should the peer conversation use a fresh git worktree?"
                        0
                        [ ( "Use a new worktree"
                          , "Create an isolated branch and working directory"
                          )
                        , ( "Share current workspace"
                          , "Keep both conversations in the current checkout"
                          )
                        ]
                        >>= pure . \case
                            Just 0 -> Just True
                            Just 1 -> Just False
                            _ -> Nothing
                Nothing ->
                    readChoiceSelection
                        (\selected label ->
                            if selected
                                then rolePrompt color label
                                else roleMuted color label)
                        [ "Use a new worktree"
                        , "Share current workspace"
                        ]
                        >>= pure . \case
                            Just "Use a new worktree" -> Just True
                            Just "Share current workspace" -> Just False
                            _ -> Nothing
    chooseRewindPoint color choices =
        let newestFirst = reverse choices
            rows =
                zipWith
                    (\number (turn, _) ->
                        ( Text.pack (show number)
                            <> ". "
                            <> promptPreview 72 turn.turnUserText
                        , "Restore the conversation state before this prompt"
                        ))
                    [(1 :: Int) ..]
                    newestFirst
            labeledChoices = zip (map fst rows) newestFirst
        in case fullscreen of
            Just runtime ->
                requestFullscreenChoiceWithBody
                    runtime
                    "Rewind conversation"
                    ( "Choose a prompt to restore as a draft. "
                        <> "Conversation only; files stay unchanged."
                    )
                    0
                    rows
                    >>= pure . (>>= atIndex newestFirst)
            Nothing -> do
                Text.hPutStrLn stderr $
                    roleMuted color
                        "Choose a prompt to restore as a draft. Files stay unchanged."
                readChoiceSelectionAt
                    0
                    (\selected label ->
                        if selected
                            then rolePrompt color label
                            else roleMuted color label)
                    (map fst rows)
                    >>= pure . (>>= (`lookup` labeledChoices))
    confirmRewind color prompt =
        case fullscreen of
            Just runtime ->
                requestFullscreenChoiceWithBody
                    runtime
                    "Rewind conversation?"
                    ( "Restore conversation state before:\n\n"
                        <> promptPreview 240 prompt
                        <> "\n\nConversation only; files stay unchanged."
                    )
                    1
                    [ ( "Rewind conversation"
                      , "Remove later turns and restore this prompt as a draft"
                      )
                    , ( "Cancel"
                      , "Keep the current conversation"
                      )
                    ]
                    >>= pure . (== Just 0)
            Nothing -> do
                Text.hPutStrLn stderr $
                    roleMuted color
                        ( "Restore conversation state before “"
                            <> promptPreview 120 prompt
                            <> "”? Files stay unchanged."
                        )
                readChoiceSelectionAt
                    1
                    (\selected label ->
                        if selected
                            then rolePrompt color label
                            else roleMuted color label)
                    [ "Rewind conversation"
                    , "Cancel"
                    ]
                    >>= pure . (== Just "Rewind conversation")
    promptPreview limit prompt =
        let oneLine = Text.unwords (Text.words (Text.strip prompt))
        in if Text.length oneLine <= limit
            then oneLine
            else Text.take (max 0 (limit - 1)) oneLine <> "…"
    atIndex values index
        | index < 0 = Nothing
        | otherwise =
            case drop index values of
                value : _ -> Just value
                [] -> Nothing
    confirmDelete color sessionId =
        case fullscreen of
            Just runtime ->
                requestFullscreenChoiceWithBody
                    runtime
                    "Delete current session?"
                    ( "This permanently removes session "
                        <> sessionId
                        <> " and its local artifacts, then starts a fresh conversation."
                    )
                    1
                    [ ( "Delete session"
                      , "Permanently remove its transcript and artifacts"
                      )
                    , ( "Cancel"
                      , "Keep the current session"
                      )
                    ]
                    >>= pure . (== Just 0)
            Nothing ->
                readChoiceSelectionAt
                    1
                    (\selected label ->
                        if selected
                            then rolePrompt color label
                            else roleMuted color label)
                    [ "Delete session permanently"
                    , "Cancel"
                    ]
                    >>= pure . (== Just "Delete session permanently")
    cleanupForkWorktree source =
        mapM_ \path -> void (removeWorktree source path)
