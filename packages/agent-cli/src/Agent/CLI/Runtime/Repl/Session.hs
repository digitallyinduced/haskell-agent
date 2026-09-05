-- | Persisted-session, handoff, and worktree command handling.
module Agent.CLI.Runtime.Repl.Session
    ( handleSessionAction
    ) where

import Agent.CLI.Session.Request
    ( readSessionRequestParams
    )
import Agent.CLI.Afk
    ( AfkTarget(..), handoffLocal, handoffRemote, parseAfkTarget )
import Agent.CLI.Command
    ( currentEffort,
      currentModel,
      ForkRequest(..),
      SessionAction(ReplRenameAuto, ReplResume, ReplSearch, ReplHome, ReplRewind, ReplClear,
                 ReplNew, ReplDelete, ReplShowSession, ReplShowSessionInfo, ReplAfk,
                 ReplWorktree, ReplRename, ReplFork),
      ShellMode(ShellNone, ShellGhci, ShellBash, ShellBoth),
      SlashCatalog(slashCatalogToolNames) )
import Agent.CLI.Input ( readChoiceSelection, readChoiceSelectionAt )
import Agent.CLI.Models
    ( ModelTarget(targetModelId, ModelTarget, targetProvider,
                  targetConnectionId, targetWireModelId, targetDialect) )
import Agent.CLI.Render ( clearThinking, putTextLn, renderEvent )
import Agent.CLI.Runtime.HistorySource
    ( reloadFullscreenHistoryForHandle )
import Agent.CLI.Runtime.Types
    ( RunResult(RunDeleteSession, RunForkSession, RunSwitchWorktree, RunRestart,
                RunQuit) )
import Agent.CLI.Session
    ( TranscriptEffect(TranscriptReset),
      appendTurnKeepTitleIndexed,
      appendTurnWithPromptResetAndTaskPlanClearIndexed,
      createSession,
      forkSessionAt,
      loadCurrentTaskPlan,
      loadSession,
      rewindSession,
      removeSessionTemp,
      resetSessionTitleToAuto,
      sessionConversationText,
      sessionRewindChoices,
      sessionsRoot,
      setManualSessionTitle,
      Persistence(PersistenceEnabled, PersistenceDisabled),
      PersistenceState(PersistenceActive, PersistencePending),
      SessionCreate(createCwd, SessionCreate, createPool, createEffort,
                    createTarget, createGatewayIdentity, createTitleHint,
                    createTitleIsManual, createRoot),
      SessionHandle(sessionMeta, sessionPool,
                    sessionTempDir, sessionDir),
      SessionMeta(metaTitle, metaLastResponseId,
                  metaInputTokens, metaOutputTokens, metaCachedTokens, metaLastRecap,
                  metaLastTurnSummary, metaLastRecapMainTurns, metaTransportModel,
                  metaTitleUserTurns, metaId, metaCwd, metaGatewayIdentity),
      SessionTransfer(transferTurns, SessionTransfer, transferMeta,
                      transferTaskPlan),
      SessionTurn(turnUsage, SessionTurn, turnAt, turnUserText,
                  turnAssistantText, turnError, turnResponseId, turnEffect,
                  turnItems, turnDisplayItems, turnProviderTelemetry) )
import Agent.CLI.Session.Selection
    ( handleConversationSearch, handleResume )
import Agent.CLI.SessionEnv ( SessionEnv(..) )
import Agent.CLI.SessionTitle
    ( invalidateSessionTitles, requestSessionTitle )
import Agent.CLI.Status ( formatTokenUsageOrZero )
import Agent.CLI.Style
    ( cliWindowTitle, glyphOk, glyphSession, roleError, roleMuted, rolePrompt )
import Agent.CLI.TUI.App
    ( commitFullscreenHistoryTurn
    , emitUiEvent
    , requestFullscreenChoiceWithBody
    )
import Agent.CLI.TUI.SessionHistory ( sessionHistoryTurn )
import Agent.CLI.TUI.Types ( HistoryCommit(..) )
import Agent.CLI.Terminal ( resolveColor )
import Agent.CLI.Worktree
    ( createManagedWorktreeWithProgress
    , removeWorktree
    , worktreeProgressMessage
    )
import Agent.Dialect ( dialectId, dialectSlug )
import Agent.Loop ( LoopEvent(ActivityUpdated) )
import Agent.OpenAI.Compaction
    ( clearSessionUserText, newSessionUserText )
import Agent.OsPath ( toText )
import Agent.Provider ( providerSlug )
import Agent.ReasoningEffort (reasoningEffortText)
import Agent.TUI.Model
    ( progressNotice,
      UiEvent(UiConversationCleared, UiSystemMessage, UiErrorMessage,
              UiSetNotice) )
import Agent.Tools.PlanMode ( PlanModeEnv(planSessionDir) )
import Agent.Tools.TaskPlan
    ( CurrentTaskPlan(currentTaskPlanValue)
    , resetTaskPlanState
    )
import Control.Exception.Safe
    ( displayException, finally, mask, onException, tryAny )
import Control.Monad ( forM_, void )
import Data.IORef ( readIORef, writeIORef )
import Data.Maybe ( fromMaybe )
import Data.Text ( Text )
import Data.Time.Clock ( getCurrentTime )
import System.IO ( stdout, stderr )
import System.OsPath ( OsPath, takeDirectory )
import qualified Data.Set as Set ( toAscList )
import qualified Data.Text as Text
    ( intercalate, length, pack, strip, take, unlines, unwords, words )
import qualified Data.Text.IO as Text ( putStrLn, hPutStrLn )

handleSessionAction
    :: SessionEnv
    -> SlashCatalog
    -> IO RunResult
    -> SessionAction
    -> IO RunResult
handleSessionAction
        env
        slashCatalog
        continue =
    dispatchSessionAction SessionActionRuntime
        { actionEnv = env
        , actionSlashCatalog = slashCatalog
        , actionContinue = continue
        }

data SessionActionRuntime = SessionActionRuntime
    { actionEnv :: SessionEnv
    , actionSlashCatalog :: SlashCatalog
    , actionContinue :: IO RunResult
    }

dispatchSessionAction
    :: SessionActionRuntime
    -> SessionAction
    -> IO RunResult
dispatchSessionAction runtime = \case
    ReplResume maybeId -> handleResumeAction runtime maybeId
    ReplSearch query -> handleSearchAction runtime query
    ReplHome -> handleResumeAction runtime Nothing
    ReplRewind -> handleRewindAction runtime
    ReplClear -> handleClearAction runtime
    ReplNew -> handleNewAction runtime
    ReplDelete -> handleDeleteAction runtime
    ReplFork request -> handleForkAction runtime request
    ReplShowSession -> handleShowSessionAction runtime
    ReplShowSessionInfo -> handleShowSessionInfoAction runtime
    ReplAfk rawTarget -> handleAfkAction runtime rawTarget
    ReplWorktree -> handleWorktreeAction runtime
    ReplRename title -> handleRenameAction runtime title
    ReplRenameAuto -> handleRenameAutoAction runtime

runtimeFullscreenEvent :: SessionActionRuntime -> UiEvent -> IO ()
runtimeFullscreenEvent runtime event =
    case runtime.actionEnv.sessionFullscreen of
        Nothing -> pure ()
        Just fullscreen -> emitUiEvent fullscreen event

runtimeDisplayInfo :: SessionActionRuntime -> Text -> IO () -> IO ()
runtimeDisplayInfo runtime message minimalAction =
    case runtime.actionEnv.sessionFullscreen of
        Nothing -> minimalAction
        Just fullscreen ->
            emitUiEvent fullscreen (UiSystemMessage message)

runtimeDisplayError :: SessionActionRuntime -> Text -> IO () -> IO ()
runtimeDisplayError runtime message minimalAction =
    case runtime.actionEnv.sessionFullscreen of
        Nothing -> minimalAction
        Just fullscreen ->
            emitUiEvent fullscreen (UiErrorMessage message)

runtimeResetCurrentTaskPlan :: SessionActionRuntime -> IO ()
runtimeResetCurrentTaskPlan runtime =
    mapM_
        (\current -> resetTaskPlanState current Nothing)
        runtime.actionEnv.sessionTaskPlan

sessionShellModeText :: ShellMode -> Text
sessionShellModeText = \case
    ShellGhci -> "ghci"
    ShellBash -> "bash"
    ShellBoth -> "ghci + bash"
    ShellNone -> "none"

runtimeWithReplActivity
    :: SessionActionRuntime
    -> ((Text -> IO ()) -> IO a)
    -> IO a
runtimeWithReplActivity runtime action =
    action (runtimeReportReplActivity runtime)
        `finally` runtimeClearReplActivity runtime

runtimeReportReplActivity :: SessionActionRuntime -> Text -> IO ()
runtimeReportReplActivity runtime message =
    case runtime.actionEnv.sessionFullscreen of
        Nothing ->
            renderEvent
                runtime.actionEnv.sessionRender
                (ActivityUpdated message)
        Just fullscreen ->
            emitUiEvent fullscreen
                (UiSetNotice (Just (progressNotice message)))

runtimeClearReplActivity :: SessionActionRuntime -> IO ()
runtimeClearReplActivity runtime =
    case runtime.actionEnv.sessionFullscreen of
        Nothing -> clearThinking runtime.actionEnv.sessionRender
        Just fullscreen -> emitUiEvent fullscreen (UiSetNotice Nothing)

chooseForkWorktreeFor
    :: SessionActionRuntime
    -> Bool
    -> Maybe Bool
    -> IO (Maybe Bool)
chooseForkWorktreeFor runtime color = \case
    Just value -> pure (Just value)
    Nothing ->
        case runtime.actionEnv.sessionFullscreen of
            Just fullscreen ->
                requestFullscreenChoiceWithBody
                    fullscreen
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

chooseRewindPointFor
    :: SessionActionRuntime
    -> Bool
    -> [(SessionTurn, [SessionTurn])]
    -> IO (Maybe (SessionTurn, [SessionTurn]))
chooseRewindPointFor runtime color choices =
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
    in case runtime.actionEnv.sessionFullscreen of
        Just fullscreen ->
            requestFullscreenChoiceWithBody
                fullscreen
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

confirmRewindFor
    :: SessionActionRuntime
    -> Bool
    -> Text
    -> IO Bool
confirmRewindFor runtime color prompt =
    case runtime.actionEnv.sessionFullscreen of
        Just fullscreen ->
            requestFullscreenChoiceWithBody
                fullscreen
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

promptPreview :: Int -> Text -> Text
promptPreview limit prompt =
    let oneLine = Text.unwords (Text.words (Text.strip prompt))
    in if Text.length oneLine <= limit
        then oneLine
        else Text.take (max 0 (limit - 1)) oneLine <> "…"

atIndex :: [a] -> Int -> Maybe a
atIndex values index
    | index < 0 = Nothing
    | otherwise =
        case drop index values of
            value : _ -> Just value
            [] -> Nothing

confirmDeleteFor
    :: SessionActionRuntime
    -> Bool
    -> Text
    -> IO Bool
confirmDeleteFor runtime color sessionId =
    case runtime.actionEnv.sessionFullscreen of
        Just fullscreen ->
            requestFullscreenChoiceWithBody
                fullscreen
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

cleanupForkWorktreeFor :: OsPath -> Maybe OsPath -> IO ()
cleanupForkWorktreeFor source =
    mapM_ \path -> void (removeWorktree source path)

handleResumeAction
    :: SessionActionRuntime
    -> Maybe Text
    -> IO RunResult
handleResumeAction runtime maybeId =
    let env = runtime.actionEnv
    in handleResume
        env.sessionDatabasePool
        env.sessionFullscreen
        env.sessionGatewayIdentity
        maybeId
        env.sessionPersist >>= \case
            Nothing -> runtime.actionContinue
            Just result -> pure result

handleRewindAction :: SessionActionRuntime -> IO RunResult
handleRewindAction runtime = do
    color <- resolveColor stderr
    let env = runtime.actionEnv
        unavailable message = do
            runtimeDisplayError runtime message $
                putTextLn stderr (roleError color message)
            runtime.actionContinue
    case env.sessionPersist of
        PersistenceDisabled ->
            unavailable "/rewind requires a persisted interactive session"
        PersistenceEnabled slotRef ->
            readIORef slotRef >>= \case
                PersistencePending{} ->
                    unavailable
                        "/rewind is available after the first persisted turn"
                PersistenceActive source ->
                    runtimeWithReplActivity runtime
                        (\report -> do
                            report "Loading rewind points…"
                            loadSession
                                env.sessionDatabasePool
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
                                        chooseRewindPointFor
                                            runtime
                                            color
                                            choices >>= \case
                                                Nothing -> runtime.actionContinue
                                                Just (prompt, retained) ->
                                                    confirmRewindFor
                                                        runtime
                                                        color
                                                        prompt.turnUserText
                                                        >>= \case
                                                            False ->
                                                                runtime.actionContinue
                                                            True ->
                                                                mask \restore -> do
                                                                    result <-
                                                                        restore $
                                                                            runtimeWithReplActivity runtime \report -> do
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

handleClearAction :: SessionActionRuntime -> IO RunResult
handleClearAction runtime = do
    color <- resolveColor stderr
    let env = runtime.actionEnv
    clearResult <- mask \restore -> case env.sessionPersist of
        PersistenceDisabled -> do
            runtimeResetCurrentTaskPlan runtime
            pure (Right ("conversation cleared", Nothing))
        PersistenceEnabled slotRef -> do
            now <- getCurrentTime
            readIORef slotRef >>= \case
                PersistencePending{} -> do
                    runtimeResetCurrentTaskPlan runtime
                    pure (Right ("conversation cleared", Nothing))
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
                            , turnDisplayItems = []
                            , turnUsage = Nothing
                            , turnProviderTelemetry = []
                            }
                    tryAny
                        (restore
                            (appendTurnWithPromptResetAndTaskPlanClearIndexed
                                handle
                                turn
                                \meta ->
                                    meta
                                        { metaLastResponseId = Nothing
                                        , metaInputTokens = 0
                                        , metaOutputTokens = 0
                                        , metaCachedTokens = 0
                                        , metaLastRecap = Nothing
                                        , metaLastTurnSummary = Nothing
                                        , metaLastRecapMainTurns = 0
                                        })) >>= \case
                        Left err ->
                            pure $
                                Left
                                    ("could not clear conversation: "
                                        <> Text.pack (displayException err))
                        Right (handle', turnIndex) -> do
                            let meta = handle'.sessionMeta
                            writeIORef slotRef
                                (PersistenceActive handle')
                            runtimeResetCurrentTaskPlan runtime
                            pure $
                                Right
                                    ( "conversation cleared (session "
                                        <> meta.metaId
                                        <> ")"
                                    , Just (turnIndex, turn)
                                    )
    case clearResult of
        Left err -> do
            runtimeDisplayError runtime err $
                putTextLn stderr (roleError color err)
            runtime.actionContinue
        Right (message, committedTurn) -> do
            env.sessionReset
            runtimeFullscreenEvent runtime UiConversationCleared
            forM_ committedTurn \(turnIndex, turn) ->
                forM_ env.sessionFullscreen \fullscreen ->
                    commitFullscreenHistoryTurn
                        fullscreen
                        (sessionHistoryTurn turnIndex turn)
                        HistoryCommitReset
            runtimeDisplayInfo runtime message $
                Text.hPutStrLn stderr
                    (roleMuted color (glyphOk <> message))
            runtime.actionContinue

handleDeleteAction :: SessionActionRuntime -> IO RunResult
handleDeleteAction runtime = do
    color <- resolveColor stderr
    let env = runtime.actionEnv
        unavailable message = do
            runtimeDisplayError runtime message $
                putTextLn stderr (roleError color message)
            runtime.actionContinue
    case env.sessionPersist of
        PersistenceDisabled ->
            unavailable "/delete requires a persisted interactive session"
        PersistenceEnabled slotRef ->
            readIORef slotRef >>= \case
                PersistencePending{} ->
                    unavailable
                        "/delete is available after the first persisted turn"
                PersistenceActive handle ->
                    confirmDeleteFor
                        runtime
                        color
                        handle.sessionMeta.metaId >>= \case
                            False -> runtime.actionContinue
                            True -> do
                                let message =
                                        "deleting session "
                                            <> handle.sessionMeta.metaId
                                            <> " after shutdown…"
                                runtimeDisplayInfo runtime message $
                                    putTextLn stderr
                                        (roleMuted color message)
                                pure
                                    (RunDeleteSession
                                        handle.sessionMeta.metaId
                                        env.sessionCwd)

handleForkAction
    :: SessionActionRuntime
    -> ForkRequest
    -> IO RunResult
handleForkAction runtime request = do
    color <- resolveColor stderr
    let env = runtime.actionEnv
        failFork message = do
            runtimeDisplayError runtime message $
                putTextLn stderr (roleError color message)
            runtime.actionContinue
    case env.sessionPersist of
        PersistenceDisabled ->
            failFork "/fork requires a persisted interactive session"
        PersistenceEnabled slotRef ->
            readIORef slotRef >>= \case
                PersistencePending{} ->
                    failFork
                        "/fork is available after the first persisted turn"
                PersistenceActive source ->
                    chooseForkWorktreeFor
                        runtime
                        color
                        request.forkWorktree >>= \case
                            Nothing -> runtime.actionContinue
                            Just useWorktree -> mask \restore -> do
                                destination <-
                                    restore $
                                        if useWorktree
                                            then
                                                runtimeWithReplActivity runtime \report ->
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
                                                cleanupForkWorktreeFor
                                                    source.sessionMeta.metaCwd
                                                    worktreePath
                                        result <-
                                            restore
                                                (runtimeWithReplActivity runtime \report -> do
                                                    report "Forking session…"
                                                    loadSession
                                                        env.sessionDatabasePool
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
                                                runtimeDisplayInfo runtime message $
                                                    putTextLn stderr
                                                        (roleMuted color
                                                            (glyphOk <> message))
                                                pure
                                                    (RunForkSession
                                                        forked.sessionMeta.metaId
                                                        request.forkDirective)

handleShowSessionAction :: SessionActionRuntime -> IO RunResult
handleShowSessionAction runtime = do
    color <- resolveColor stdout
    case runtime.actionEnv.sessionPersist of
        PersistenceDisabled ->
            runtimeDisplayInfo runtime "session: (not persisted)" $
                Text.putStrLn
                    (roleMuted color "session: (not persisted)")
        PersistenceEnabled slotRef -> do
            slot <- readIORef slotRef
            case slot of
                PersistencePending _ _ _ ->
                    runtimeDisplayInfo
                        runtime
                        "session: (pending until first turn)" $
                        Text.putStrLn
                            (roleMuted color
                                "session: (pending until first turn)")
                PersistenceActive handle ->
                    let message =
                            "session: " <> handle.sessionMeta.metaId
                    in runtimeDisplayInfo runtime message $
                        Text.putStrLn
                            (roleMuted color (glyphSession <> message))
    runtime.actionContinue

handleShowSessionInfoAction :: SessionActionRuntime -> IO RunResult
handleShowSessionInfoAction runtime = do
    let env = runtime.actionEnv
    color <- resolveColor stdout
    params <- readSessionRequestParams env.sessionParams
    usage <- readIORef env.sessionUsage
    shellMode <- env.sessionShellMode
    (persistenceState, sessionId, sessionTitle) <-
        case env.sessionPersist of
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
                runtime.actionSlashCatalog.slashCatalogToolNames
        usageText = formatTokenUsageOrZero usage
        message = Text.unlines $
            [ "session: " <> fromMaybe "(not persisted)" sessionId
            , "state: " <> persistenceState
            ]
                <> maybe
                    []
                    (\title -> ["title: " <> title])
                    sessionTitle
                <> [ "provider: " <> providerSlug env.sessionProvider
                   , "connection: " <> env.sessionConnection
                   , "model: " <> currentModel params
                   , "dialect: "
                        <> dialectSlug (dialectId env.sessionDialect)
                   , "effort: "
                        <> reasoningEffortText (currentEffort params)
                   , "cwd: " <> toText env.sessionCwd
                   , "shell: " <> sessionShellModeText shellMode
                   , "tokens: " <> usageText
                   , "tools: "
                        <> if null toolNames
                            then "(none)"
                            else Text.intercalate ", " toolNames
                   ]
    runtimeDisplayInfo runtime message $
        Text.putStrLn (roleMuted color message)
    runtime.actionContinue

handleAfkAction
    :: SessionActionRuntime
    -> Maybe Text
    -> IO RunResult
handleAfkAction runtime rawTarget = do
    let env = runtime.actionEnv
        failAfk err = do
            color <- resolveColor stderr
            runtimeDisplayError runtime err $
                putTextLn stderr (roleError color err)
            runtime.actionContinue
        finishAfk message = do
            color <- resolveColor stderr
            runtimeDisplayInfo runtime message $
                putTextLn stderr
                    (roleMuted color (glyphOk <> message))
            pure RunQuit
    case parseAfkTarget rawTarget of
        Left err -> failAfk err
        Right target -> case env.sessionPersist of
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
                                    env.sessionCwd >>= \case
                                        Left err -> failAfk err
                                        Right message ->
                                            finishAfk message
                            AfkRemote host path ->
                                loadSession
                                    env.sessionDatabasePool
                                    (sessionsRoot env.sessionHome)
                                    handle.sessionMeta.metaId
                                    >>= \case
                                        Left err -> failAfk err
                                        Right (meta, turns) ->
                                            loadCurrentTaskPlan
                                                env.sessionPersist >>= \case
                                                    Left err -> failAfk err
                                                    Right currentPlan ->
                                                        handoffRemote
                                                            host
                                                            path
                                                            handle.sessionDir
                                                            SessionTransfer
                                                                { transferMeta =
                                                                    meta
                                                                , transferTaskPlan =
                                                                    (.currentTaskPlanValue)
                                                                        <$> currentPlan
                                                                , transferTurns =
                                                                    turns
                                                                }
                                                            >>= \case
                                                                Left err ->
                                                                    failAfk err
                                                                Right message ->
                                                                    finishAfk
                                                                        message

handleWorktreeAction :: SessionActionRuntime -> IO RunResult
handleWorktreeAction runtime = do
    let env = runtime.actionEnv
    result <- runtimeWithReplActivity runtime \report ->
        createManagedWorktreeWithProgress
            (report . worktreeProgressMessage)
            env.sessionHome
            env.sessionCwd
    case result of
        Left err -> do
            color <- resolveColor stderr
            runtimeDisplayError runtime err $
                putTextLn stderr (roleError color err)
            runtime.actionContinue
        Right path -> do
            color <- resolveColor stderr
            params <- readSessionRequestParams env.sessionParams
            let message = "worktree: " <> toText path
            runtimeDisplayInfo runtime message $
                putTextLn stderr
                    (roleMuted color (glyphSession <> message))
            pure
                (RunSwitchWorktree
                    path
                    env.sessionProvider
                    (currentModel params)
                    (currentEffort params))

handleRenameAction
    :: SessionActionRuntime
    -> Text
    -> IO RunResult
handleRenameAction runtime title = do
    let env = runtime.actionEnv
    color <- resolveColor stderr
    case env.sessionPersist of
        PersistenceDisabled ->
            runtimeDisplayError
                runtime
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
                    env.sessionSetWindowTitle
                        (cliWindowTitle pending.createCwd (Just title))
                    let message = "session title: " <> title
                    runtimeDisplayInfo runtime message $
                        putTextLn stderr
                            (roleMuted color (glyphOk <> message))
                PersistenceActive handle -> do
                    invalidateSessionTitles
                        env.sessionTitleManager
                        handle.sessionMeta.metaId
                    updated <- setManualSessionTitle title handle
                    writeIORef slotRef (PersistenceActive updated)
                    env.sessionSetWindowTitle
                        (cliWindowTitle
                            updated.sessionMeta.metaCwd
                            (Just updated.sessionMeta.metaTitle))
                    let message =
                            "session title: "
                                <> updated.sessionMeta.metaTitle
                    runtimeDisplayInfo runtime message $
                        putTextLn stderr
                            (roleMuted color (glyphOk <> message))
    runtime.actionContinue

handleRenameAutoAction :: SessionActionRuntime -> IO RunResult
handleRenameAutoAction runtime = do
    let env = runtime.actionEnv
        message = "automatic session titles enabled"
    color <- resolveColor stderr
    case env.sessionPersist of
        PersistenceDisabled ->
            runtimeDisplayError
                runtime
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
                    env.sessionSetWindowTitle
                        (cliWindowTitle pending.createCwd Nothing)
                    runtimeDisplayInfo runtime message $
                        putTextLn stderr
                            (roleMuted color (glyphOk <> message))
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
                                let source = sessionConversationText turns
                                requestSessionTitle
                                    env.sessionTitleManager
                                    updated.sessionMeta.metaId
                                    1
                                    source
                    runtimeDisplayInfo runtime message $
                        putTextLn stderr
                            (roleMuted color (glyphOk <> message))
    runtime.actionContinue

handleNewAction :: SessionActionRuntime -> IO RunResult
handleNewAction runtime = do
    let env = runtime.actionEnv
    env.sessionReset
    runtimeFullscreenEvent runtime UiConversationCleared
    color <- resolveColor stderr
    case env.sessionPersist of
        PersistenceDisabled -> do
            runtimeResetCurrentTaskPlan runtime
            runtimeDisplayInfo runtime "started a fresh conversation" $
                Text.hPutStrLn stderr
                    (roleMuted color
                        (glyphOk <> "started a fresh conversation"))
            runtime.actionContinue
        PersistenceEnabled slotRef -> do
            params <- readSessionRequestParams env.sessionParams
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
                                { targetProvider = env.sessionProvider
                                , targetConnectionId =
                                    env.sessionConnection
                                , targetModelId = model
                                , targetWireModelId =
                                    fromMaybe
                                        model
                                        handle.sessionMeta.metaTransportModel
                                , targetDialect =
                                    dialectId env.sessionDialect
                                }
                            , createGatewayIdentity =
                                handle.sessionMeta.metaGatewayIdentity
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
                    , turnDisplayItems = []
                    , turnUsage = Nothing
                    , turnProviderTelemetry = []
                    }
            (handle', _) <- appendTurnKeepTitleIndexed handle turn
            let meta = handle'.sessionMeta
            env.sessionOnPersisted handle'
            env.sessionSetTempDir handle'.sessionTempDir
            writeIORef slotRef (PersistenceActive handle')
            runtimeResetCurrentTaskPlan runtime
            writeIORef env.sessionTitleTurnCount 0
            writeIORef env.sessionPlanMode.planSessionDir
                (Just handle'.sessionDir)
            writeIORef env.sessionStoreRoot (Just handle'.sessionDir)
            forM_ env.sessionFullscreen \fullscreen ->
                reloadFullscreenHistoryForHandle fullscreen handle'
            env.sessionSetWindowTitle
                (cliWindowTitle meta.metaCwd (Just meta.metaTitle))
            let message = "new session: " <> meta.metaId
            runtimeDisplayInfo runtime message $
                Text.hPutStrLn stderr
                    (roleMuted color (glyphOk <> message))
            runtime.actionContinue

handleSearchAction
    :: SessionActionRuntime
    -> Text
    -> IO RunResult
handleSearchAction runtime query =
    let env = runtime.actionEnv
    in handleConversationSearch
        env.sessionDatabasePool
        env.sessionFullscreen
        env.sessionGatewayIdentity
        query
        env.sessionPersist >>= \case
            Nothing -> runtime.actionContinue
            Just result -> pure result
