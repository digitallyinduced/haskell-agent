-- | Execute one model turn and commit its observable session state.
module Agent.CLI.Turn
    ( IncompleteTurnCheckpoint(..)
    , applyPendingSessionTitles
    , checkpointIncompleteTurn
    , grokFirstTurnPrefix
    , grokFrameLastUserInput
    , grokUserQuery
    , restorePlanStateAfterIncomplete
    , runOneTurn
    ) where

import Agent.Cancel (resetCancel)
import Agent.CLI.CancelWatch (withEscCancel)
import Agent.CLI.Interrupt (withTurnCancel)
import Agent.CLI.Plan (extractProposedPlan, planDecisionFollowUp)
import Agent.CLI.ProviderFallback (isProviderUnavailable)
import Agent.CLI.ProviderTransition
    ( PendingTurn(..)
    , TurnResult(..)
    )
import Agent.CLI.TUI.App
    ( emitUiEvent
    )
import Agent.TUI.Model
    ( BlockState(..)
    , UiEvent(..)
    , successNotice
    )
import Agent.CLI.Render
    ( RenderConfig(..)
    , clearThinking
    , formatElapsed
    , formatLoopErrorAt
    , formatLoopErrorColored
    , formatLoopErrorColoredAt
    , formatLoopErrorPersistedAt
    , formatTurnStatus
    , putTextLn
    , renderAssistantText
    )
import Agent.CLI.Session
    ( SessionHandle(..)
    , SessionMeta(..)
    , SessionTurn(..)
    , Persistence(..)
    , PersistenceState(..)
    , appendTurnWithMetaUpdate
    , ensureSession
    , loadSession
    , sessionConversationText
    , sessionTitleFromPrompt
    , setGeneratedSessionTitle
    )
import Agent.CLI.SessionEnv (SessionEnv(..))
import Agent.CLI.SessionTitle
    ( SessionTitleResult(..)
    , requestSessionTitle
    , takeSessionTitleResults
    , titleRefreshIndex
    )
import Agent.CLI.Status (formatTokenUsage)
import Agent.CLI.Style
    ( cliWindowTitle
    , glyphSession
    , roleMuted
    )
import Agent.CLI.Terminal
    ( TerminalCapabilities(..)
    , emitTerminalSequence
    , notifyTerminal
    , osc133CommandFinished
    , osc133CommandStart
    , resolveColor
    )
import Agent.CLI.Timestamp (stampTurnInputs, stripBracketedTimestamps)
import Agent.Dialect (DialectId(..), dialectId)
import Agent.Loop
    ( LoopConfig(..)
    , LoopError(..)
    , LoopResult(..)
    , TurnInput(..)
    , addTokenUsage
    , runLoopInputs
    )
import Agent.Provider (Provider(..))
import Agent.Responses.LoopBackend (turnInputsToItems)
import Agent.Responses.Types (ResponseItem)
import Agent.Tools.PlanMode
    ( PlanDecision(..)
    , PlanCompletion(..)
    , PlanModeEnv(..)
    , PlanModeHooks(..)
    , PlanModeState(..)
    , activatePlanMode
    , deactivatePlanMode
    , isPlanModeActive
    , planFilePath
    , planModeReminder
    , writePlanMarkdown
    )
import Agent.OsPath (toText, unsafeToFilePath)
import Control.Monad (when)
import Control.Exception.Safe (onException, tryAny)
import Data.IORef
    ( atomicModifyIORef'
    , modifyIORef'
    , readIORef
    , writeIORef
    )
import Data.List (isPrefixOf)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime, localDay, zonedTimeToLocalTime)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.IO (stderr, stdout)
import System.Info (os)
import qualified System.OsPath
import System.Process
    ( CreateProcess(..)
    , proc
    , readCreateProcessWithExitCode
    )
import System.Timeout (timeout)

runOneTurn :: SessionEnv -> Text -> [TurnInput] -> IO TurnResult
runOneTurn env@SessionEnv
    { sessionLoop = config
    , sessionRender = render
    , sessionPrevious = previous
    , sessionPrinted = printed
    , sessionTranscript = transcriptRef
    , sessionPersist = persist
    , sessionPlanMode = planMode
    , sessionStartupContext = startupContext
    , sessionEscPaused = escPaused
    , sessionInterrupt = interrupt
    , sessionStoreRoot = storeRoot
    , sessionUsage = usageRef
    , sessionLastAssistant = lastAssistantRef
    , sessionTerminal = terminal
    , sessionFullscreen = fullscreen
    , sessionSetWindowTitle = setWindowTitle
    , sessionBeginSubagentTurn = beginSubagentTurn
    , sessionFinishSubagentTurn = finishSubagentTurn
    , sessionAbortSubagentTurn = abortSubagentTurn
    , sessionOnPersisted = onPersisted
    } promptText inputs = do
  -- Clear the prior turn before publishing this flag to Ctrl-C / Esc.
  -- Resetting inside runLoopInputs could erase the one-shot Esc signal.
  resetCancel config.loopCancel
  writeIORef env.sessionRestartEffort Nothing
  withTurnCancel interrupt config.loopCancel $
    (if isJust fullscreen
        then id
        else withEscCancel config.loopCancel escPaused) do
    applyPendingSessionTitles env
    initialPlanState <- readIORef planMode.planStateRef
    when (initialPlanState == PlanPending) (activatePlanMode planMode)
    -- Create the session directory before tools run so first-turn subagents
    -- can persist under agents/<id>/ as they complete.
    case persist of
        PersistenceEnabled slotRef -> do
            created <- isPendingPersistence <$> readIORef slotRef
            handle <- ensureSession slotRef
            onPersisted handle
            writeIORef planMode.planSessionDir (Just handle.sessionDir)
            writeIORef storeRoot (Just handle.sessionDir)
            when
                ( handle.sessionMeta.metaTitle == "untitled"
                    && not (Text.null (Text.strip promptText))
                )
                (setWindowTitle
                    (cliWindowTitle handle.sessionMeta.metaCwd
                        (Just (sessionTitleFromPrompt promptText))))
            when created do
                case fullscreen of
                    Just runtime ->
                        emitUiEvent runtime
                            (UiSystemMessage
                                ("session: " <> handle.sessionMeta.metaId))
                    Nothing -> do
                        color <- resolveColor stderr
                        putTextLn stderr
                            (roleMuted color
                                (glyphSession <> "session: "
                                    <> handle.sessionMeta.metaId))
            titleTurns <- readIORef env.sessionTitleTurnCount
            when
                ( titleTurns == 0
                    && not handle.sessionMeta.metaTitleIsManual
                    && handle.sessionMeta.metaTitleRefreshIndex == 0
                )
                (requestSessionTitle env.sessionTitleManager
                    handle.sessionMeta.metaId 1 promptText)
        PersistenceDisabled -> pure ()
    prev <- readIORef previous
    beforeItems <- readIORef transcriptRef
    pendingStartup <- atomicModifyIORef' startupContext \pendingCtx -> (Nothing, pendingCtx)
    planActive <- isPlanModeActive planMode
    planPath <- planFilePath planMode
    let planReminder =
            if planActive
                then Just $
                    planModeReminder
                        (case env.sessionProvider of
                            OpenAIProvider -> CompleteWithProposedPlan
                            _ -> CompleteWithExitTool)
                        planPath
                else Nothing
        baseInputs = case pendingStartup of
            Just context -> UserMessage context : inputs
            Nothing -> inputs
        restoreStartupContext = case pendingStartup of
            Nothing -> pure ()
            Just context ->
                modifyIORef' startupContext \current ->
                    Just $ case current of
                        Nothing -> context
                        Just newer -> context <> "\n\n" <> newer
        turnInputs0 = case planReminder of
            Just reminder -> UserMessage reminder : baseInputs
            Nothing -> baseInputs
    stampedInputs <- stampTurnInputs turnInputs0
    turnInputs <-
        if dialectId env.sessionDialect == GrokBuildDialect
            then do
                let framed = grokFrameLastUserInput stampedInputs
                    firstTurn = null beforeItems && prev == Nothing
                if firstTurn
                    then do
                        prefix <- loadGrokFirstTurnPrefix env.sessionCwd
                        pure (UserMessage prefix : framed)
                    else pure framed
            else pure stampedInputs
    startedAt <- readIORef render.renderStartedAt
    wallStarted <- getCurrentTime
    when (isNothing fullscreen && terminal.terminalSemanticPrompts) $
        emitTerminalSequence terminal stdout osc133CommandStart
    rootTurnId <- beginSubagentTurn
    result <- runLoopInputs config prev turnInputs
        `onException`
            ( restoreStartupContext
                >> restorePlanStateAfterIncomplete planMode initialPlanState
                >> abortSubagentTurn rootTurnId
            )
    clearThinking render
    finishedAt <- getCurrentTime
    restartEffort <-
        atomicModifyIORef' env.sessionRestartEffort \requested ->
            (Nothing, requested)
    let elapsedDetail extra = case startedAt of
            Nothing -> extra
            Just t0 -> extra <> " · " <> formatElapsed (realToFrac (diffUTCTime finishedAt t0))
        persistIncomplete retainedItems errorText = case persist of
            PersistenceDisabled -> pure ()
            PersistenceEnabled slotRef -> do
                now <- getCurrentTime
                handle <- ensureSession slotRef
                writeIORef planMode.planSessionDir (Just handle.sessionDir)
                writeIORef storeRoot (Just handle.sessionDir)
                let turn = SessionTurn
                        { turnAt = now
                        , turnUserText = promptText
                        , turnAssistantText = Nothing
                        , turnError = Just errorText
                        , turnResponseId = Nothing
                        , turnItems = retainedItems
                        , turnUsage = Nothing
                        }
                handle' <- appendTurnWithMetaUpdate handle turn \meta ->
                    meta { metaLastResponseId = Nothing }
                writeIORef slotRef (PersistenceActive handle')
    case (restartEffort, result) of
        (Just level, _) -> do
            restoreStartupContext
            abortSubagentTurn rootTurnId
            writeIORef transcriptRef beforeItems
            planState <- readIORef planMode.planStateRef
            case fullscreen of
                Just runtime ->
                    emitUiEvent runtime UiTurnRestarted
                Nothing -> pure ()
            pure $ TurnRestartRequested level PendingTurn
                { pendingPromptText = promptText
                , pendingInputs = inputs
                , pendingExitAfter = False
                , pendingPlanState = planState
                }
        (Nothing, Left cancelled@(LoopCancelled _)) -> do
            restorePlanStateAfterIncomplete planMode initialPlanState
            finishTerminal (isNothing fullscreen)
                terminal wallStarted finishedAt 130 "Agent cancelled"
            abortSubagentTurn rootTurnId
            -- turnInputs already contains any startup context consumed above.
            -- Checkpoint it instead of restoring it separately, which would
            -- duplicate the instructions on the next full-history request.
            let checkpoint = checkpointIncompleteTurn beforeItems turnInputs
            writeIORef transcriptRef checkpoint.checkpointTranscript
            writeIORef previous checkpoint.checkpointPreviousResponseId
            model <- readIORef render.renderModelRef
            case fullscreen of
                Just runtime -> do
                    emitUiEvent runtime
                        (UiTurnEnded BlockCancelled)
                    emitUiEvent runtime
                        (UiSystemMessage
                            ("cancelled · " <> elapsedDetail model))
                Nothing -> do
                    color <- resolveColor stderr
                    putTextLn stderr (formatLoopErrorColored color cancelled)
                    putTextLn stderr
                        (formatTurnStatus color "cancelled" (elapsedDetail model))
            persistIncomplete checkpoint.checkpointTurnItems "cancelled"
            pure TurnSucceeded
        (Nothing, Left err) -> do
            abortSubagentTurn rootTurnId
            afterItems <- readIORef transcriptRef
            case err of
                LoopTransport apiError
                    | length afterItems == length beforeItems
                    , isProviderUnavailable apiError -> do
                        restoreStartupContext
                        case fullscreen of
                            Nothing -> pure ()
                            Just runtime ->
                                emitUiEvent runtime
                                    (UiTurnEnded BlockFailed)
                        finishTerminal (isNothing fullscreen)
                            terminal wallStarted finishedAt 1
                            "Agent provider unavailable"
                        planState <- readIORef planMode.planStateRef
                        pure $ TurnProviderUnavailable apiError PendingTurn
                            { pendingPromptText = promptText
                            , pendingInputs = inputs
                            , pendingExitAfter = False
                            , pendingPlanState = planState
                            }
                _ -> do
                    restorePlanStateAfterIncomplete planMode initialPlanState
                    finishTerminal (isNothing fullscreen)
                        terminal wallStarted finishedAt 1 "Agent turn failed"
                    let checkpoint =
                            checkpointIncompleteTurn beforeItems turnInputs
                    writeIORef transcriptRef checkpoint.checkpointTranscript
                    writeIORef previous
                        checkpoint.checkpointPreviousResponseId
                    model <- readIORef render.renderModelRef
                    case fullscreen of
                        Just runtime ->
                            do
                                emitUiEvent runtime
                                    (UiTurnEnded BlockFailed)
                                emitUiEvent runtime
                                    (UiErrorMessage
                                        (formatLoopErrorAt finishedAt err
                                            <> "\n"
                                            <> elapsedDetail model))
                        Nothing -> do
                            color <- resolveColor stderr
                            putTextLn stderr
                                (formatLoopErrorColoredAt color finishedAt err)
                            putTextLn stderr
                                (formatTurnStatus color "error" (elapsedDetail model))
                    persistIncomplete checkpoint.checkpointTurnItems
                        (formatLoopErrorPersistedAt finishedAt err)
                    pure TurnFailed
        (Nothing, Right loopResult) -> do
            finishTerminal (isNothing fullscreen)
                terminal wallStarted finishedAt 0 "Agent finished"
            finishSubagentTurn rootTurnId
            writeIORef previous (Just loopResult.finalResponseId)
            modifyIORef' usageRef (`addTokenUsage` loopResult.tokenUsage)
            do
                model <- readIORef render.renderModelRef
                let turns = Text.pack (show loopResult.turnsUsed)
                    unit = if loopResult.turnsUsed == 1 then " turn" else " turns"
                    usageDetail = formatTokenUsage loopResult.tokenUsage
                    extra =
                        if Text.null usageDetail
                            then model <> " · " <> turns <> unit
                            else model <> " · " <> turns <> unit <> " · " <> usageDetail
                    detail = elapsedDetail extra
                case fullscreen of
                    Just runtime ->
                        emitUiEvent runtime
                            (UiSetNotice
                                (Just
                                    (successNotice
                                        ("Finished · " <> detail))))
                    Nothing -> do
                        color <- resolveColor stderr
                        putTextLn stderr
                            (formatTurnStatus color "ok" detail)
            followUp <- handleProposedPlan planMode loopResult.finalText
            printedText <- readIORef printed
            let assistantText =
                    fmap stripBracketedTimestamps loopResult.finalText
            writeIORef lastAssistantRef assistantText
            case (fullscreen, printedText, assistantText) of
                (Just _, _, _) -> pure ()
                (Nothing, False, Just text) | not (Text.null (Text.strip text)) -> do
                    useColor <- resolveColor stdout
                    putTextLn stdout (renderAssistantText useColor text)
                _ -> pure ()
            afterItems <- readIORef transcriptRef
            let newItems
                    | beforeItems `isPrefixOf` afterItems =
                        drop (length beforeItems) afterItems
                    | otherwise = afterItems
            case persist of
                PersistenceDisabled -> pure ()
                PersistenceEnabled slotRef -> do
                    now <- getCurrentTime
                    handle <- ensureSession slotRef
                    writeIORef planMode.planSessionDir (Just handle.sessionDir)
                    writeIORef storeRoot (Just handle.sessionDir)
                    let turn = SessionTurn
                            { turnAt = now
                            , turnUserText = promptText
                            , turnAssistantText = assistantText
                            , turnError = Nothing
                            , turnResponseId = Just loopResult.finalResponseId
                            , turnItems = newItems
                            , turnUsage = Just loopResult.tokenUsage
                            }
                    titleTurns <- (+ 1) <$> readIORef env.sessionTitleTurnCount
                    countedHandle <- appendTurnWithMetaUpdate handle turn \meta ->
                        meta { metaTitleUserTurns = titleTurns }
                    writeIORef env.sessionTitleTurnCount titleTurns
                    let countedMeta = countedHandle.sessionMeta
                    writeIORef slotRef (PersistenceActive countedHandle)
                    when
                        ( titleTurns `elem` [3, 6]
                            && not countedMeta.metaTitleIsManual
                            && countedMeta.metaTitleRefreshIndex
                                < titleRefreshIndex titleTurns
                        )
                        (requestConversationTitle env countedHandle titleTurns)
                    when (countedMeta.metaTitle /= handle.sessionMeta.metaTitle) do
                        setWindowTitle
                            (cliWindowTitle countedMeta.metaCwd
                                (Just countedMeta.metaTitle))
                    applyPendingSessionTitles env
            case followUp of
                Nothing -> pure TurnSucceeded
                Just notes -> do
                    writeIORef printed False
                    runOneTurn env notes [UserMessage notes]

-- | Wrap the last actual user payload in the Grok Build request envelope.
-- Synthetic startup, skill, plan, and reminder messages precede it and remain
-- separately framed.
grokFrameLastUserInput :: [TurnInput] -> [TurnInput]
grokFrameLastUserInput = reverse . go . reverse
  where
    go [] = []
    go (UserMessage text : rest) =
        UserMessage (grokUserQuery text) : rest
    go (input@UserMultimodal{userText} : rest) =
        input { userText = grokUserQuery userText } : rest
    go (input : rest) = input : go rest

grokUserQuery :: Text -> Text
grokUserQuery text =
    "<user_query>\n" <> text <> "\n</user_query>"

grokFirstTurnPrefix
    :: Text
    -> Text
    -> System.OsPath.OsPath
    -> Day
    -> Maybe Text
    -> Text
grokFirstTurnPrefix osName shell cwd today gitStatus =
    Text.intercalate "\n"
        [ "<user_info>"
        , "OS Version: " <> osName
        , "Shell: " <> shellBaseName shell
        , "Workspace Path: " <> toText cwd
        , "Today's date: "
            <> Text.pack
                (formatTime defaultTimeLocale "%A %b %-d, %Y" today)
        , "Note: Prefer using relative paths over absolute paths as tool call args when possible."
        , "</user_info>"
        ]
        <> maybe "" renderGitStatus gitStatus
  where
    shellBaseName path =
        case filter (not . Text.null)
                (Text.split (\char -> char == '/' || char == '\\') path) of
            [] -> path
            parts -> last parts
    renderGitStatus status =
        "\n\n<git_status>\n\
        \This is the git status at the start of the conversation. Note that this status is a snapshot in time, and will not update during the conversation.\n"
            <> status
            <> "\n</git_status>"

loadGrokFirstTurnPrefix :: System.OsPath.OsPath -> IO Text
loadGrokFirstTurnPrefix cwd = do
    shell <- maybe "/bin/sh" Text.pack <$> lookupEnv "SHELL"
    osVersion <- loadOperatingSystem
    today <- localDay . zonedTimeToLocalTime <$> getZonedTime
    status <- loadGitStatus cwd
    pure (grokFirstTurnPrefix osVersion shell cwd today status)

loadOperatingSystem :: IO Text
loadOperatingSystem = do
    result <- tryAny $
        timeout 1000000 $
            readCreateProcessWithExitCode (proc "uname" ["-sr"]) ""
    pure $ case result of
        Right (Just (ExitSuccess, output, _))
            | kernel : release <- Text.words (Text.strip (Text.pack output))
            , not (null release) ->
                Text.toLower kernel <> " " <> Text.unwords release
        _ -> Text.pack os

loadGitStatus :: System.OsPath.OsPath -> IO (Maybe Text)
loadGitStatus cwd = do
    result <- tryAny $
        timeout 5000000 $
            readCreateProcessWithExitCode
                (proc "git"
                    [ "status"
                    , "--short"
                    , "--branch"
                    , "--untracked-files=normal"
                    ])
                    { cwd = Just (unsafeToFilePath cwd) }
                ""
    pure $ case result of
        Right (Just (ExitSuccess, output, _)) ->
            normalizeGitStatus
                (collapseStatusSpaces (Text.pack output))
        _ -> Nothing

collapseStatusSpaces :: Text -> Text
collapseStatusSpaces = Text.pack . go False . Text.unpack
  where
    go _ [] = []
    go previousSpace (char : rest)
        | char == ' ' && previousSpace =
            go True rest
        | otherwise =
            char : go (char == ' ') rest

normalizeGitStatus :: Text -> Maybe Text
normalizeGitStatus raw
    | Text.null status = Nothing
    | Text.length status <= maxCharacters = Just status
    | otherwise =
        Just
            (snapToLastNewline (Text.take maxCharacters status)
                <> "\n\n... (git status truncated)")
  where
    status = Text.strip raw
    maxCharacters = 10000
    snapToLastNewline prefix =
        case Text.breakOnEnd "\n" prefix of
            ("", _) -> prefix
            (throughNewline, _) -> Text.dropEnd 1 throughNewline

isPendingPersistence :: PersistenceState -> Bool
isPendingPersistence = \case
    PersistencePending _ -> True
    PersistenceActive _ -> False

-- | Pure state transition for a cancelled or failed logical turn. It preserves
-- the exact model inputs while discarding partial assistant/tool state and
-- invalidates the provider response chain so the next request replays the
-- complete local transcript.
data IncompleteTurnCheckpoint = IncompleteTurnCheckpoint
    { checkpointTranscript :: ![ResponseItem]
    , checkpointTurnItems :: ![ResponseItem]
    , checkpointPreviousResponseId :: !(Maybe Text)
    } deriving (Eq, Show)

checkpointIncompleteTurn
    :: [ResponseItem]
    -> [TurnInput]
    -> IncompleteTurnCheckpoint
checkpointIncompleteTurn beforeItems turnInputs =
    let retainedItems = turnInputsToItems turnInputs
    in IncompleteTurnCheckpoint
        { checkpointTranscript = beforeItems <> retainedItems
        , checkpointTurnItems = retainedItems
        , checkpointPreviousResponseId = Nothing
        }

requestConversationTitle :: SessionEnv -> SessionHandle -> Int -> IO ()
requestConversationTitle env handle milestone =
    loadSession
        (System.OsPath.takeDirectory handle.sessionDir)
        handle.sessionMeta.metaId
        >>= \case
            Left _ -> pure ()
            Right (_, turns) ->
                requestSessionTitle env.sessionTitleManager
                    handle.sessionMeta.metaId
                    milestone
                    (sessionConversationText turns)

applyPendingSessionTitles :: SessionEnv -> IO ()
applyPendingSessionTitles env =
    takeSessionTitleResults env.sessionTitleManager >>= mapM_ applyOne
  where
    applyOne SessionTitleResult{..} =
        case env.sessionPersist of
            PersistenceDisabled -> pure ()
            PersistenceEnabled slotRef ->
                readIORef slotRef >>= \case
                    PersistencePending _ -> pure ()
                    PersistenceActive handle
                        | handle.sessionMeta.metaId /= resultSessionId -> pure ()
                        | handle.sessionMeta.metaTitleIsManual -> pure ()
                        | otherwise -> do
                            updated <- setGeneratedSessionTitle
                                (titleRefreshIndex resultMilestone)
                                resultTitle
                                handle
                            writeIORef slotRef (PersistenceActive updated)
                            env.sessionSetWindowTitle
                                (cliWindowTitle updated.sessionMeta.metaCwd
                                    (Just updated.sessionMeta.metaTitle))

-- | Roll a cancelled or failed turn back to the interaction mode it started
-- in. In particular, an agent-initiated enter_plan_mode must not trap the next
-- user prompt in Plan Mode after the turn is interrupted.
restorePlanStateAfterIncomplete
    :: PlanModeEnv
    -> PlanModeState
    -> IO ()
restorePlanStateAfterIncomplete planMode =
    writeIORef planMode.planStateRef

finishTerminal
    :: Bool
    -> TerminalCapabilities
    -> UTCTime
    -> UTCTime
    -> Int
    -> Text
    -> IO ()
finishTerminal semanticPrompts terminal started finished exitCode message = do
    when (semanticPrompts && terminal.terminalSemanticPrompts) $
        emitTerminalSequence terminal stdout
            (osc133CommandFinished (Just exitCode))
    let seconds = realToFrac (diffUTCTime finished started) :: Double
    when (exitCode /= 0 || seconds >= 10) $
        notifyTerminal terminal stdout message

handleProposedPlan
    :: PlanModeEnv
    -> Maybe Text
    -> IO (Maybe Text)
handleProposedPlan planMode = \case
    Nothing -> pure Nothing
    Just text -> do
        active <- isPlanModeActive planMode
        case (active, extractProposedPlan text) of
            (True, Just planBody) -> do
                _ <- writePlanMarkdown planMode planBody
                let PlanModeHooks{ planDecideExit = decideExit } = planMode.planHooks
                decision <- decideExit planBody
                case decision of
                    PlanApprove -> do
                        deactivatePlanMode planMode
                        pure (planDecisionFollowUp decision)
                    PlanCancel -> do
                        deactivatePlanMode planMode
                        pure Nothing
                    PlanRequestChanges _ ->
                        pure (planDecisionFollowUp decision)
            _ -> pure Nothing
