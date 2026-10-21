-- | A turn stream can reconnect independently of the frontend lease. Human
-- prompts have a separate owner so durable polling never interrupts stdin.
module Agent.CLI.Remote.Turn (runRemoteTurn, runRemoteQuestion) where

import Agent.Cancel
import Agent.CLI.CancelWatch (withEscCancel)
import Agent.CLI.Interrupt (withTurnCancel)
import Agent.CLI.Remote.Transport
import Agent.CLI.Remote.Ui
import Agent.CLI.Render (clearThinking, renderPrintedText, resetRenderPrintedText)
import Agent.CLI.TUI.App (emitUiEvent)
import Agent.CLI.TUI.Types (FullscreenRuntime(..), FullscreenInput(..))
import Agent.CLI.TUI.Composer.Buffer qualified as Composer
import Agent.CLI.Input (ReplLine(..))
import Agent.Loop (ImageAttachment, LoopEvent(..), TurnOutput(..))
import Agent.Runtime.Session.Settings
import Agent.Runtime.Session.Events (decodeDisplayEvent)
import Agent.Server.Client
import Agent.TUI.Model (UiEvent(..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (link, race, withAsync)
import Control.Concurrent.STM
import Control.Exception.Safe (bracket_, throwIO, onException)
import Control.Monad (forM_, forever, void, when)
import Data.Aeson (Value(..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Foldable (toList)
import Data.IORef
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import System.IO (stdout)
import System.Timeout (timeout)

runRemoteTurn :: RemoteUi -> AgentServerClient -> SessionOperation -> Text -> [ImageAttachment] -> IO ()
runRemoteTurn ui client operation prompt images = runRemoteTurnWithNotice ui client operation prompt images (remoteNotice ui)

runRemoteQuestion :: RemoteUi -> AgentServerClient -> SessionOperation -> Text -> IO Text
runRemoteQuestion ui client operation prompt = do
    answer <- newIORef ""
    runRemoteTurnWithNotice ui client operation prompt [] (writeIORef answer)
    readIORef answer

runRemoteTurnWithNotice :: RemoteUi -> AgentServerClient -> SessionOperation -> Text -> [ImageAttachment] -> (Text -> IO ()) -> IO ()
runRemoteTurnWithNotice ui client operation prompt images notice = do
    session <- readIORef ui.remoteCurrentSession
    settings <- readIORef ui.remoteSettings
    files <- if operation == ConversationTurn
        then atomicModifyIORef' ui.remoteFiles (\current -> ([], current))
        else pure []
    when (operation == ConversationTurn) (writeIORef ui.remoteLastFiles files)
    turn <- submitRemoteTurn client session settings operation prompt images files ui.remoteClock
    cancel <- newCancelFlag
    requests <- newTQueueIO
    seenRequests <- newTVarIO Set.empty
    cursor <- newIORef Nothing
    lastResponse <- newIORef Nothing
    lastAnswer <- newIORef Nothing
    textChunks <- newIORef []
    let enqueue request = atomically do
            seen <- readTVar seenRequests
            when (request.agentServerRequestId `Set.notMember` seen) do
                modifyTVar' seenRequests (Set.insert request.agentServerRequestId)
                writeTQueue requests request
        humanWorker = forever (atomically (readTQueue requests) >>= handleRemoteHumanRequest ui client)
        steeringWorker = case ui.remoteFullscreen of
            _ | operation /= ConversationTurn -> atomically retry
            Nothing -> atomically retry
            Just runtime -> forever do
                input <- atomically do
                    pending <- Composer.readFullscreenInputs runtime.runtimeInput
                    case toList pending of
                        first : _ | not first.fullscreenInputQueued
                            , ReplText text <- first.fullscreenInputLine
                            , not ("/" `Text.isPrefixOf` Text.strip text) ->
                                Composer.takeFullscreenInput runtime.runtimeInput
                        _ -> retry
                case input.fullscreenInputLine of
                    ReplText text -> do
                        identifier <- newAgentServerClientRequestId
                        steerAgentServerTurn client turn.agentServerTurnId
                            (AgentServerSteerTurnRequest identifier text) >>= \case
                                Right _ -> emitUiEvent runtime (UiUserSubmitted text)
                                Left _ -> void $ atomically $ Composer.appendFullscreenInput runtime.runtimeInput
                                    input{fullscreenInputQueued = True}
                    _ -> pure ()
        cancellation = do
            waitCancel cancel
            void (timeout 1_000_000 (cancelAgentServerTurn client turn.agentServerTurnId))
        answer text = do
            previous <- readIORef lastAnswer
            when (previous /= Just text) (notice text >> writeIORef lastAnswer (Just text))
        displayEvent display = do
            case display of
                TextDelta text -> modifyIORef' textChunks (text :)
                TurnStarted -> writeIORef textChunks []
                ResponseRestarted _ -> writeIORef textChunks []
                ResponseAttemptDiscarded -> writeIORef textChunks []
                TurnFinished output -> do
                    shown <- Text.concat . reverse <$> readIORef textChunks
                    forM_ output.assistantText \complete -> do
                        let missing = if shown `Text.isPrefixOf` complete
                                then Text.drop (Text.length shown) complete
                                else if Text.null shown then complete else "\n" <> complete
                        when (not (Text.null missing)) (emitRemoteLoop ui (TextDelta missing))
                    writeIORef lastResponse (Just output.responseId)
                _ -> pure ()
            emitRemoteLoop ui display
        localAnswer payload = case payload of
            Object fields | Just (String text) <- KeyMap.lookup "text" fields -> answer text >> pure (Right ())
            _ -> pure (Left (AgentServerProtocolError "invalid local answer"))
        onEvent event = do
            result <- processEvent event
            case result of
                Right () -> writeIORef cursor (Just event.agentServerEventId)
                Left _ -> pure ()
            pure result
        processEvent event = do
            case event.agentServerEventPayload of
                AgentServerRequestCreated request -> enqueue request
                _ -> pure ()
            case event.agentServerEventData of
                Object fields | event.agentServerEventType == "notice"
                    , Just (String message) <- KeyMap.lookup "message" fields -> notice message
                _ -> pure ()
            if event.agentServerEventType == "local_preferences"
            then remoteLocalPayload client event >>= \case
                Left err -> pure (Left err)
                Right Nothing -> pure (Left (AgentServerProtocolError "missing local session preferences"))
                Right (Just payload) -> updateRemotePreferences ui payload
            else if event.agentServerEventType == "local_catalog"
            then remoteLocalPayload client event >>= \case
                Left err -> pure (Left err)
                Right Nothing -> pure (Left (AgentServerProtocolError "missing local tool catalog"))
                Right (Just payload) -> updateRemoteCatalog ui payload
            else if event.agentServerEventType == "local_image"
            then remoteLocalPayload client event >>= \case
                Left err -> pure (Left err)
                Right Nothing -> pure (Left (AgentServerProtocolError "missing local image payload"))
                Right (Just payload) -> showRemoteImage ui payload
            else if event.agentServerEventType == "local_answer"
            then remoteLocalPayload client event >>= \case
                Left err -> pure (Left err)
                Right Nothing -> pure (Left (AgentServerProtocolError "missing local answer"))
                Right (Just payload) -> localAnswer payload
            else remoteDisplayEvent client event >>= \case
                Left err -> pure (Left err)
                Right value -> do
                    forM_ value displayEvent
                    pure (Right ())
        recover = do
            listAgentServerRequestsForTurn client turn.agentServerTurnId >>= requireRemote
                >>= mapM_ enqueue . (.agentServerRequests)
            current <- getAgentServerTurn client turn.agentServerTurnId >>= requireRemote
            case current.agentServerTurnStatus of
                AgentServerTurnCompleted -> do
                    getAgentServerLocalTurnResult client turn.agentServerTurnId >>= \case
                        Right payload@(Object fields) | Just (String "answer") <- KeyMap.lookup "kind" fields ->
                            localAnswer payload >>= requireRemote
                        Right (Object fields) | Just encoded <- KeyMap.lookup "event" fields ->
                            case decodeDisplayEvent encoded of
                                Right display@(TurnFinished output) -> do
                                    previous <- readIORef lastResponse
                                    when (previous /= Just output.responseId) (displayEvent display)
                                _ -> throwIO (RemoteFailure "Invalid local turn result.")
                        _ | operation == CompactContext -> pure ()
                          | otherwise -> do
                            result <- getAgentServerTurnResult client turn.agentServerTurnId >>= requireRemote
                            forM_ (result.agentServerResultOutput >>= (.agentServerOutputAssistantText)) notice
                    pure True
                AgentServerTurnFailed -> do
                    throwIO (RemoteFailure (maybe "The server turn failed." id current.agentServerTurnError))
                AgentServerTurnCancelled -> remoteNotice ui "Turn cancelled." >> pure True
                _ -> pure False
        stream failures = do
            lastSeen <- readIORef cursor
            streamAgentServerTurnWithKnownRequests client turn.agentServerTurnId lastSeen
                (Set.toList <$> readTVarIO seenRequests) onEvent >>= \case
                Right AgentServerStreamCompleted -> void recover
                Right AgentServerStreamCancelled -> remoteNotice ui "Turn cancelled."
                Right (AgentServerStreamFailed reason) -> throwIO (RemoteFailure (maybe "The server turn failed." id reason))
                Right (AgentServerStreamNeedsRefetch latest) -> do
                    modifyIORef' cursor (Just . max latest . maybe 0 id)
                    done <- recover
                    when (not done) (stream 0)
                Left _ | failures < 5 -> do
                    threadDelay (min 1_000_000 ((failures + 1) * 100_000))
                    done <- recover
                    when (not done) (stream (failures + 1))
                Left err -> throwIO (RemoteFailure (remoteErrorText err))
        updatesConversation = operation `elem` [ConversationTurn, CompactContext]
        busy value = when updatesConversation $
            forM_ ui.remoteFullscreen (\runtime -> emitUiEvent runtime (UiSetAwaitingInput (not value)))
        finish = do
            writeIORef ui.remoteCancel Nothing
            busy False
            case ui.remoteFullscreen of
                Just _ -> pure ()
                Nothing -> do
                    clearThinking ui.remoteRender
                    printed <- renderPrintedText ui.remoteRender
                    when printed (TextIO.hPutStrLn stdout "")
        action = withAsync cancellation \worker -> do
            link worker
            race humanWorker (race steeringWorker (stream (0 :: Int))) >> pure ()
        watched = case ui.remoteFullscreen of
            Just _ -> action
            Nothing -> withEscCancel cancel ui.remoteStdin action
    bracket_
        (writeIORef ui.remoteCancel (Just cancel) >> resetRenderPrintedText ui.remoteRender >> busy True)
        finish
        (withTurnCancel ui.remoteInterrupt cancel watched)
        `onException` void (timeout 1_000_000 (cancelAgentServerTurn client turn.agentServerTurnId))
    refreshed <- remoteSession client session.remoteId
    writeIORef ui.remoteCurrentSession refreshed
    when updatesConversation $ forM_ ui.remoteFullscreen (const (renderRemoteHistory ui client))
