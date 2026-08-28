-- | Prompt loading plus session and agent selection flows.
module Agent.CLI.Session.Selection
    ( currentSessionId
    , handleConversationSearch
    , handleResume
    , loadPrompt
    , pickAgentChoice
    , reservedSessionId
    ) where

import Agent.CLI.AgentViewport
    ( AgentEntry(..)
    , AgentTarget
    , pickAgentViewport
    )
import Agent.CLI.ManagedTurn
    ( ManagedTurnRequest
    , loadTextPrompt
    , loadManagedTurnRequest
    , managedTurnRequestFromText
    )
import Agent.CLI.Options (CliOptions(..))
import Agent.CLI.Resume
    ( ResumeEntry(..)
    , applyResumeSearchResults
    , initialResumeBrowser
    , loadResumeEntry
    , pickResumeEntries
    , pickResumeSession
    , resumeEntryFromMeta
    , resumeSearchEntries
    )
import Agent.CLI.Runtime.Types (RunResult(..))
import Agent.CLI.Session
    ( Persistence(..)
    , PersistenceState(..)
    , SessionHandle(..)
    , SessionMeta(..)
    , deleteSession
    , listSessions
    , loadSessionMeta
    , sessionsRoot
    )
import Agent.CLI.Style
    ( glyphSession
    , roleError
    , roleMuted
    )
import Agent.CLI.Terminal (resolveColor)
import Agent.CLI.TUI.App
    ( FullscreenRuntime
    , emitUiEvent
    , requestFullscreenChoice
    , requestFullscreenResume
    )
import Agent.Store.Postgres.Connection (StorePool)
import qualified Agent.Store.Postgres.Session as StoreSession
import Agent.Store.Types (renderStoreError)
import Agent.TUI.Model (UiEvent(..))
import Data.IORef (readIORef)
import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (getCurrentTime)
import System.Directory.OsPath (getHomeDirectory)
import System.Exit (die)
import System.OsPath (OsPath)
import System.IO (stderr)

loadPrompt :: CliOptions -> IO (Maybe ManagedTurnRequest)
loadPrompt options =
  case
        ( options.optPrompt
        , options.optPromptFile
        , options.optManagedTurnFile
        )
    of
    (Just text, _, _) ->
        pure (Just (managedTurnRequestFromText (Text.strip text)))
    (_, Just path, _) -> Just <$> loadTextPrompt path
    (_, _, Just path) ->
        loadManagedTurnRequest path >>= either (die . Text.unpack) (pure . Just)
    _ -> pure Nothing

handleResume
    :: StorePool
    -> Maybe FullscreenRuntime
    -> Maybe Text
    -> Persistence
    -> IO (Maybe RunResult)
handleResume databasePool fullscreen maybeId persist = do
    color <- resolveColor stderr
    home <- getHomeDirectory
    let reportInfo message =
            case fullscreen of
                Nothing ->
                    Text.hPutStrLn stderr
                        (roleMuted color (glyphSession <> message))
                Just runtime ->
                    emitUiEvent runtime (UiSystemMessage message)
        reportError message =
            case fullscreen of
                Nothing ->
                    Text.hPutStrLn stderr (roleError color message)
                Just runtime ->
                    emitUiEvent runtime (UiErrorMessage message)
        root = sessionsRoot home
        resume sessionId = do
            currentId <- currentSessionId persist
            if Just sessionId == currentId
                then do
                    reportInfo ("already on session " <> sessionId)
                    pure Nothing
                else
                    loadSessionMeta databasePool root sessionId >>= \case
                        Left err -> do
                            reportError err
                            pure Nothing
                        Right _ -> pure (Just (RunResumeSession sessionId))
    case maybeId of
        Just sessionId -> resume sessionId
        Nothing -> do
            (sessions, warnings) <- listSessions databasePool root
            mapM_ reportError warnings
            currentId <- currentSessionId persist
            pickResumeChoice
                databasePool fullscreen color root currentId sessions >>= \case
                Nothing -> pure Nothing
                Just sessionId -> resume sessionId

handleConversationSearch
    :: StorePool
    -> Maybe FullscreenRuntime
    -> Text
    -> Persistence
    -> IO (Maybe RunResult)
handleConversationSearch databasePool fullscreen query persist = do
    color <- resolveColor stderr
    home <- getHomeDirectory
    let root = sessionsRoot home
        reportError message =
            case fullscreen of
                Nothing ->
                    Text.hPutStrLn stderr (roleError color message)
                Just runtime ->
                    emitUiEvent runtime (UiErrorMessage message)
        reportInfo message =
            case fullscreen of
                Nothing ->
                    Text.hPutStrLn stderr
                        (roleMuted color (glyphSession <> message))
                Just runtime ->
                    emitUiEvent runtime (UiSystemMessage message)
    (sessions, warnings) <- listSessions databasePool root
    mapM_ reportError warnings
    searchResumeEntries databasePool sessions query >>= \case
        Left err -> do
            reportError err
            pure Nothing
        Right [] -> do
            reportInfo ("no conversations matched “" <> Text.strip query <> "”")
            pure Nothing
        Right entries -> do
            currentId <- currentSessionId persist
            pickSearchChoice
                databasePool
                fullscreen
                color
                root
                currentId
                sessions
                query
                entries
                >>= \case
                    Nothing -> pure Nothing
                    Just sessionId ->
                        handleResume
                            databasePool
                            fullscreen
                            (Just sessionId)
                            persist

searchResumeEntries
    :: StorePool
    -> [SessionMeta]
    -> Text
    -> IO (Either Text [ResumeEntry])
searchResumeEntries databasePool sessions query
    | Text.null (Text.strip query) =
        pure (Right (map resumeEntryFromMeta sessions))
    | otherwise =
        StoreSession.searchConversationTurns
            databasePool
            (Text.strip query)
            100
            >>= \case
                Left err -> pure (Left (renderStoreError err))
                Right results ->
                    pure (Right (resumeSearchEntries sessions results))

pickSearchChoice
    :: StorePool
    -> Maybe FullscreenRuntime
    -> Bool
    -> OsPath
    -> Maybe Text
    -> [SessionMeta]
    -> Text
    -> [ResumeEntry]
    -> IO (Maybe Text)
pickSearchChoice
    databasePool
    fullscreen
    color
    root
    currentId
    sessions
    query
    entries =
        case fullscreen of
            Nothing -> pickResumeEntries color entries
            Just runtime -> do
                now <- getCurrentTime
                let browser =
                        applyResumeSearchResults
                            query
                            entries
                            (initialResumeBrowser now entries)
                    deleteEntry sessionId
                        | currentId == Just sessionId =
                            pure (Left "cannot delete the current session")
                        | otherwise =
                            deleteSession databasePool root sessionId
                fmap (.resumeId)
                    <$> requestFullscreenResume
                        runtime
                        browser
                        (loadResumeEntry databasePool root)
                        deleteEntry
                        (searchResumeEntries databasePool sessions)

pickResumeChoice
    :: StorePool
    -> Maybe FullscreenRuntime
    -> Bool
    -> OsPath
    -> Maybe Text
    -> [SessionMeta]
    -> IO (Maybe Text)
pickResumeChoice databasePool fullscreen color root currentId sessions =
  case fullscreen of
    Nothing -> pickResumeSession databasePool color root sessions
    Just runtime -> do
        now <- getCurrentTime
        let browser =
                initialResumeBrowser now (map resumeEntryFromMeta sessions)
            deleteEntry sessionId
                | currentId == Just sessionId =
                    pure (Left "cannot delete the current session")
                | otherwise =
                    deleteSession databasePool root sessionId
        fmap (.resumeId)
            <$> requestFullscreenResume
                runtime
                browser
                (loadResumeEntry databasePool root)
                deleteEntry
                (searchResumeEntries databasePool sessions)

pickAgentChoice
    :: Maybe FullscreenRuntime
    -> Bool
    -> AgentTarget
    -> [AgentEntry]
    -> IO (Maybe AgentTarget)
pickAgentChoice fullscreen color selected entries = case fullscreen of
    Nothing -> pickAgentViewport color selected entries
    Just runtime ->
        requestFullscreenChoice
            runtime
            "Agents"
            (fromMaybe 0
                (findIndex ((== selected) . (.agentTarget)) entries))
            [ ( entry.agentPath
              , entry.agentStatus
                    <> case entry.agentTranscript of
                        first : _
                            | not (Text.null (Text.strip first)) ->
                                " · " <> Text.take 80 (Text.strip first)
                        _ -> ""
              )
            | entry <- entries
            ]
            >>= pure . (>>= (`atMay` map (.agentTarget) entries))

currentSessionId
    :: Persistence
    -> IO (Maybe Text)
currentSessionId = \case
    PersistenceDisabled -> pure Nothing
    PersistenceEnabled slotRef -> do
        slot <- readIORef slotRef
        pure $ case slot of
            PersistencePending _ _ _ -> Nothing
            PersistenceActive handle -> Just handle.sessionMeta.metaId

-- | Return the stable id already reserved for a pending session as well as
-- the id of an active one. Learned-skill evidence can safely record this id
-- before the successful turn creates the corresponding session row.
reservedSessionId
    :: Persistence
    -> IO (Maybe Text)
reservedSessionId = \case
    PersistenceDisabled -> pure Nothing
    PersistenceEnabled slotRef -> do
        slot <- readIORef slotRef
        pure $ case slot of
            PersistencePending _ sessionId _ -> Just sessionId
            PersistenceActive handle -> Just handle.sessionMeta.metaId

atMay :: Int -> [a] -> Maybe a
atMay index values
    | index < 0 = Nothing
    | otherwise = case drop index values of
        value : _ -> Just value
        [] -> Nothing
