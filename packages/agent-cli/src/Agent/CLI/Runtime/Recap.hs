module Agent.CLI.Runtime.Recap
    ( runSessionRecap
    , runSessionTurnSummary
    ) where

import Agent.CLI.CancelWatch (withEscCancel)
import Agent.CLI.Interrupt (withTurnCancel)
import Agent.CLI.Recap
    ( RecapKind(..)
    , RecapOutcome(..)
    , autoRecapIdleThreshold
    , formatRecapError
    , mainTurnCount
    , recapGate
    , recapPreview
    , recapUnavailableToast
    , runRecapWithCancel
    , runTurnSummaryWithCancel
    )
import Agent.CLI.Render (RenderConfig(..), putTextLn)
import Agent.CLI.Session
    ( Persistence(..)
    , PersistenceState(..)
    , SessionHandle(..)
    , SessionMeta(..)
    , SessionTurn(..)
    , loadSession
    , setSessionRecap
    , setSessionTurnSummary
    )
import Agent.CLI.Session.History (readLiveTranscript)
import Agent.CLI.SessionEnv (SessionEnv(..))
import Agent.CLI.Style (roleMuted)
import Agent.CLI.Terminal (resolveColor)
import Agent.CLI.TUI.App (emitUiEvent)
import Agent.TUI.Model (UiEvent(..))
import Control.Monad (forM_, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import System.OsPath (takeDirectory)

runSessionRecap :: Bool -> SessionEnv -> RecapKind -> IO ()
runSessionRecap registerCancel env kind = do
    let fullscreen = env.sessionFullscreen
        stdoutHandle = env.sessionRender.renderStdout
    transcriptRef <- newIORef =<< readLiveTranscript env.sessionConversation
    color <- resolveColor stdoutHandle
    transcript <- readIORef transcriptRef
    let mainTurns = mainTurnCount transcript
        hasMessages = mainTurns > 0
    lastCommittedTurns <- recapWatermark env
    lastCompletedAt <- recapLastCompletedAt env
    now <- getCurrentTime
    let idleOk =
            case lastCompletedAt of
                Nothing -> False
                Just finished ->
                    diffUTCTime now finished >= autoRecapIdleThreshold
    case recapGate mainTurns lastCommittedTurns kind idleOk of
        Left _ ->
            case kind of
                RecapManual -> recapUnavailable env hasMessages
                RecapAuto -> pure ()
        Right () -> do
            when (kind == RecapManual) $
                forM_ fullscreen \runtime ->
                    emitUiEvent runtime UiRecapStarted
            result <-
                runRecapWithCancel
                    (\cancel action ->
                        if registerCancel
                            then
                                withTurnCancel env.sessionInterrupt cancel $
                                    case fullscreen of
                                        Nothing
                                            | env.sessionBackground -> action
                                            | otherwise ->
                                            withEscCancel
                                                cancel
                                                env.sessionEscPaused
                                                action
                                        Just _ -> action
                            else action)
                    env.sessionBtwBackend
                    env.sessionParams
                    transcriptRef
                    kind
            case result of
                Left err ->
                    case kind of
                        RecapManual -> recapFailed env (formatRecapError err)
                        RecapAuto -> pure ()
                Right (RecapShown summary) -> do
                    persistSessionRecap env summary mainTurns
                    displayRecap env color summary
                Right (RecapSuppressed summary) ->
                    persistSessionRecap env summary mainTurns

recapWatermark :: SessionEnv -> IO Int
recapWatermark env =
    case env.sessionPersist of
        PersistenceDisabled -> pure 0
        PersistenceEnabled slotRef ->
            readIORef slotRef >>= \case
                PersistencePending _ _ _ -> pure 0
                PersistenceActive handle ->
                    pure handle.sessionMeta.metaLastRecapMainTurns

recapLastCompletedAt :: SessionEnv -> IO (Maybe UTCTime)
recapLastCompletedAt env =
    case env.sessionPersist of
        PersistenceDisabled -> pure Nothing
        PersistenceEnabled slotRef ->
            readIORef slotRef >>= \case
                PersistencePending _ _ _ -> pure Nothing
                PersistenceActive handle ->
                    loadSession
                        handle.sessionPool
                        (takeDirectory handle.sessionDir)
                        handle.sessionMeta.metaId
                        >>= \case
                            Left _ ->
                                pure (Just handle.sessionMeta.metaUpdatedAt)
                            Right (_, turns) ->
                                pure $
                                    case reverse turns of
                                        turn : _ -> Just turn.turnAt
                                        [] -> Just handle.sessionMeta.metaUpdatedAt

persistSessionRecap :: SessionEnv -> Text -> Int -> IO ()
persistSessionRecap env summary mainTurns =
    case env.sessionPersist of
        PersistenceDisabled -> pure ()
        PersistenceEnabled slotRef ->
            readIORef slotRef >>= \case
                PersistencePending _ _ _ -> pure ()
                PersistenceActive handle -> do
                    updated <-
                        setSessionRecap (recapPreview summary) mainTurns handle
                    writeIORef slotRef (PersistenceActive updated)

displayRecap :: SessionEnv -> Bool -> Text -> IO ()
displayRecap env color summary =
    let message = "Recap: " <> summary
    in case env.sessionFullscreen of
        Just runtime ->
            emitUiEvent runtime (UiRecapReady summary)
        Nothing ->
            putTextLn env.sessionRender.renderStdout (roleMuted color message)

recapUnavailable :: SessionEnv -> Bool -> IO ()
recapUnavailable env hasMessages =
    recapFailed env (recapUnavailableToast hasMessages)

recapFailed :: SessionEnv -> Text -> IO ()
recapFailed env message = do
    let stderrHandle = env.sessionRender.renderStderr
    color <- resolveColor stderrHandle
    case env.sessionFullscreen of
        Just runtime ->
            emitUiEvent runtime (UiRecapUnavailable message)
        Nothing ->
            putTextLn stderrHandle (roleMuted color message)

runSessionTurnSummary :: SessionEnv -> IO ()
runSessionTurnSummary env = do
    transcriptRef <- newIORef =<< readLiveTranscript env.sessionConversation
    result <-
        runTurnSummaryWithCancel
            (\_ action -> action)
            env.sessionBtwBackend
            env.sessionParams
            transcriptRef
    case result of
        Left _ -> pure ()
        Right summary ->
            case env.sessionPersist of
                PersistenceDisabled -> pure ()
                PersistenceEnabled slotRef ->
                    readIORef slotRef >>= \case
                        PersistencePending _ _ _ -> pure ()
                        PersistenceActive handle -> do
                            updated <- setSessionTurnSummary summary handle
                            writeIORef slotRef (PersistenceActive updated)
