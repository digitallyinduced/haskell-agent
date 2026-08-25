-- | Load paged persisted turns into the fullscreen history window.
module Agent.CLI.Runtime.HistorySource
    ( sessionUiPageSize
    , emptyFullscreenHistoryPage
    , loadFullscreenHistoryPage
    , reloadFullscreenHistoryForHandle
    ) where

import Agent.CLI.Session
    ( SessionHandle(..)
    , SessionMeta(..)
    , loadRecentSessionTurns
    , loadSessionTurnsAfter
    , loadSessionTurnsBefore
    )
import Agent.CLI.TUI.App
    ( FullscreenRuntime
    , emitUiEvent
    , reloadFullscreenHistorySource
    )
import Agent.CLI.TUI.History
    ( HistoryCursor(..)
    , HistoryDirection(..)
    , HistoryGeneration(..)
    , HistoryPage(..)
    , HistoryRequest(..)
    )
import Agent.CLI.TUI.SessionHistory (sessionHistoryPage)
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
            Right page ->
                reloadFullscreenHistorySource
                    runtime
                    sessionId
                    loader
                    (sessionHistoryPage
                        (HistoryGeneration 0)
                        HistoryNewer
                        page)
