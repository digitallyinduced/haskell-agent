-- | Load paged persisted turns into the fullscreen history window.
module Agent.CLI.Runtime.HistorySource
    ( sessionUiPageSize
    , emptyFullscreenHistoryPage
    , loadFullscreenHistoryPage
    , reloadFullscreenHistoryForHandle
    , restoreFullscreenPullRequest
    ) where

import Agent.CLI.Session
    ( SessionHandle(..)
    , SessionMeta(..)
    , SessionTurnPage(..)
    , loadSessionHistorySnapshot
    , loadSessionHistoryTurnsRangeBounded
    , loadRecentSessionTurns
    , loadSessionTurnsAfter
    , loadSessionTurnsBefore
    )
import Agent.CLI.TUI.App
    ( emitUiEvent
    , enqueueAppEvent
    , reloadFullscreenHistorySource
    )
import Agent.CLI.TUI.Types (AppEvent(..), FullscreenRuntime(..))
import Agent.CLI.TUI.History
    ( HistoryCursor(..)
    , HistoryDirection(..)
    , HistoryGeneration(..)
    , HistoryPage(..)
    , HistoryRequest(..)
    )
import Agent.CLI.TUI.SessionHistory (sessionHistoryPage, sessionTurnPullRequestURL)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.IORef (readIORef)
import Agent.Store.Postgres.Connection (StorePool)
import Agent.TUI.Model (UiEvent(..), warningNotice)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import System.OsPath (OsPath, takeDirectory)

sessionUiPageSize :: Int
sessionUiPageSize = 80

emptyFullscreenHistoryPage :: HistoryGeneration -> HistoryPage
emptyFullscreenHistoryPage generation =
    HistoryPage
        { historyPageGeneration = generation
        , historyPageDirection = HistoryNewer
        , historyPageTurns = Seq.empty
        , historyPageGenerationStart = HistoryCursor 0
        , historyPageTotalTurns = 0
        , historyPageHasOlder = False
        , historyPageHasNewer = False
        }

loadFullscreenHistoryPage
    :: StorePool
    -> OsPath
    -> Text
    -> HistoryRequest
    -> IO (Either Text HistoryPage)
loadFullscreenHistoryPage pool root sessionId request = do
    let loadPage =
            case ( request.historyRequestDirection
                 , request.historyRequestCursor
                 ) of
                (HistoryOlder, Just (HistoryCursor cursor)) ->
                    loadSessionTurnsBefore
                        pool root sessionId cursor sessionUiPageSize
                (HistoryNewer, Just (HistoryCursor cursor)) ->
                    loadSessionTurnsAfter
                        pool root sessionId cursor sessionUiPageSize
                _ ->
                    loadRecentSessionTurns
                        pool root sessionId sessionUiPageSize
    fmap
        (sessionHistoryPage
            request.historyRequestGeneration
            request.historyRequestDirection)
        <$> loadPage

-- | Associations belong to the conversation, including turns before
-- compaction or outside the bounded transcript window.
restoreFullscreenPullRequest
    :: FullscreenRuntime -> StorePool -> OsPath -> Text -> IO ()
restoreFullscreenPullRequest runtime pool root sessionId = do
    generation <- HistoryGeneration <$> readIORef runtime.runtimeHistoryGeneration
    result <- loadSessionHistorySnapshot pool root sessionId >>= \case
        Left err -> pure (Left err)
        Right (_, _, total) -> scan total
    case result of
        Left _ -> pure ()
        Right url ->
            enqueueAppEvent runtime (AppSetPullRequestURL generation url)
  where
    scan end
        | end <= 0 = pure (Right Nothing)
        | otherwise = do
            let start = max 0 (end - 32)
            loadSessionHistoryTurnsRangeBounded
                pool root sessionId start end 32 >>= \case
                    Left err -> pure (Left err)
                    Right page ->
                        case listToMaybe (mapMaybe
                            (sessionTurnPullRequestURL . snd)
                            (reverse page.pageTurns)) of
                            Just url -> pure (Right (Just url))
                            Nothing -> scan start

reloadFullscreenHistoryForHandle
    :: FullscreenRuntime
    -> SessionHandle
    -> IO ()
reloadFullscreenHistoryForHandle runtime handle = do
    let root = takeDirectory handle.sessionDir
        sessionId = handle.sessionMeta.metaId
        loader =
            loadFullscreenHistoryPage
                handle.sessionPool
                root
                sessionId
    loadRecentSessionTurns
        handle.sessionPool
        root
        sessionId
        sessionUiPageSize >>= \case
            Left err ->
                emitUiEvent runtime
                    (UiSetNotice
                        (Just (warningNotice
                            ("Could not refresh session history: " <> err))))
            Right page -> do
                reloadFullscreenHistorySource
                    runtime
                    sessionId
                    loader
                    (sessionHistoryPage
                        (HistoryGeneration 0)
                        HistoryNewer
                        page)
                restoreFullscreenPullRequest runtime handle.sessionPool root sessionId
