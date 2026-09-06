-- | PostgreSQL-backed session loading and metadata operations.
module Agent.CLI.Session.Storage
    ( sessionSchemaVersion
    , loadSession
    , loadActiveSession
    , loadSessionMeta
    , loadRecentSessionTurns
    , loadRecentSessionHistoryTurns
    , loadSessionTurnsBefore
    , loadSessionHistoryTurnsBefore
    , loadSessionTurnsAfter
    , loadSessionHistoryTurnsRange
    , loadSessionHistoryTurnsRangeBounded
    , loadSessionHistoryTurnsAround
    , loadSessionHistorySnapshot
    , loadSessionResumeStats
    , loadSessions
    , loadSessionHandle
    , deleteSession
    , renameSession
    , setSessionArchived
    , listSessions
    , listArchivedSessionIds
    , writeSessionMeta
    ) where

import Agent.CLI.Session.Codec
    ( decodeStoredSession
    , fromStoredMetadata
    , fromStoredTurn
    , importLegacySession
    , toStoredMetadata
    , validateSessionMeta
    )
import Agent.CLI.Session.TempWorkspace
    ( isValidSessionId
    , removeSessionTemp
    , sessionDirForId
    , sessionTempDirForId
    )
import Agent.CLI.Session.Types
    ( SessionHandle(..)
    , SessionMeta(..)
    , SessionResumeStats(..)
    , SessionTurn(..)
    , SessionTurnPage(..)
    )
import Agent.CLI.SessionLock
    ( acquireSessionLock
    , releaseSessionLock
    )
import Agent.Store.Postgres.Connection (StorePool)
import qualified Agent.Store.Postgres.Session as Store
import Agent.Store.Types (StoreError, renderStoreError)
import Control.Exception.Safe (displayException, finally, tryIO)
import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
    ( ExceptT
    , except
    , runExceptT
    , throwE
    )
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)
import qualified Data.Vector as Vector
import System.Directory.OsPath (doesDirectoryExist, removePathForcibly)
import System.OsPath (OsPath, unsafeEncodeUtf, (</>))

sessionSchemaVersion :: Int
sessionSchemaVersion = 1

loadSession
    :: StorePool
    -> OsPath
    -> Text
    -> IO (Either Text (SessionMeta, [SessionTurn]))
loadSession pool root sessionId = runExceptT do
    _ <- except (sessionDirForId root sessionId)
    stored <- loadWithLegacyImport root pool sessionId Store.loadSession
    storedPrompt <- loadStoredPromptEpoch pool sessionId
    decodeStoredSession
        sessionSchemaVersion
        isValidSessionId
        sessionId
        storedPrompt
        stored

loadActiveSession
    :: StorePool
    -> OsPath
    -> Text
    -> IO (Either Text (SessionMeta, [SessionTurn]))
loadActiveSession pool root sessionId = runExceptT do
    _ <- except (sessionDirForId root sessionId)
    stored <- loadWithLegacyImport root pool sessionId Store.loadActiveSession
    storedPrompt <- loadStoredPromptEpoch pool sessionId
    decodeStoredSession
        sessionSchemaVersion
        isValidSessionId
        sessionId
        storedPrompt
        stored

loadStoredPromptEpoch
    :: StorePool
    -> Text
    -> ExceptT Text IO (Maybe Store.SessionPromptEpoch)
loadStoredPromptEpoch pool sessionId =
    lift (Store.loadLatestSessionPromptEpoch pool sessionId)
        >>= either (throwE . renderStoreError) pure

loadSessionMeta
    :: StorePool
    -> OsPath
    -> Text
    -> IO (Either Text SessionMeta)
loadSessionMeta pool root sessionId = runExceptT do
    _ <- except (sessionDirForId root sessionId)
    stored <- loadWithLegacyImport root pool sessionId Store.loadSessionMetadata
    meta <- except (fromStoredMetadata stored)
    validateSessionMeta sessionSchemaVersion isValidSessionId sessionId meta
    pure meta

loadRecentSessionTurns
    :: StorePool
    -> OsPath
    -> Text
    -> Int
    -> IO (Either Text SessionTurnPage)
loadRecentSessionTurns pool root sessionId limit =
    loadSessionTurnPage root pool sessionId
        (\pool' key -> Store.loadRecentSessionTurns pool' key limit)

loadRecentSessionHistoryTurns
    :: StorePool
    -> OsPath
    -> Text
    -> Int
    -> IO (Either Text SessionTurnPage)
loadRecentSessionHistoryTurns pool root sessionId limit =
    loadSessionTurnPage root pool sessionId
        (\pool' key -> Store.loadRecentSessionHistoryTurns pool' key limit)

loadSessionTurnsBefore
    :: StorePool
    -> OsPath
    -> Text
    -> Int64
    -> Int
    -> IO (Either Text SessionTurnPage)
loadSessionTurnsBefore pool root sessionId cursor limit =
    loadSessionTurnPage root pool sessionId
        (\pool' key ->
            Store.loadSessionTurnsBefore
                pool' key cursor (boundedPageLimit limit))

loadSessionHistoryTurnsBefore
    :: StorePool
    -> OsPath
    -> Text
    -> Int64
    -> Int
    -> IO (Either Text SessionTurnPage)
loadSessionHistoryTurnsBefore pool root sessionId cursor limit =
    loadSessionTurnPage root pool sessionId
        (\pool' key ->
            Store.loadSessionHistoryTurnsBefore
                pool' key cursor (boundedPageLimit limit))

loadSessionTurnsAfter
    :: StorePool
    -> OsPath
    -> Text
    -> Int64
    -> Int
    -> IO (Either Text SessionTurnPage)
loadSessionTurnsAfter pool root sessionId cursor limit =
    loadSessionTurnPage root pool sessionId
        (\pool' key ->
            Store.loadSessionTurnsAfter
                pool' key cursor (boundedPageLimit limit))

loadSessionHistoryTurnsRange
    :: StorePool
    -> OsPath
    -> Text
    -> Int64
    -> Int
    -> IO (Either Text SessionTurnPage)
loadSessionHistoryTurnsRange pool root sessionId start limit =
    loadSessionTurnPage root pool sessionId
        (\pool' key ->
            Store.loadSessionHistoryTurnsRange
                pool' key start (boundedPageLimit limit))

loadSessionHistoryTurnsRangeBounded
    :: StorePool
    -> OsPath
    -> Text
    -> Int64
    -> Int64
    -> Int
    -> IO (Either Text SessionTurnPage)
loadSessionHistoryTurnsRangeBounded
        pool root sessionId start endExclusive limit =
    loadSessionTurnPage root pool sessionId
        (\pool' key ->
            Store.loadSessionHistoryTurnsRangeBounded
                pool' key start endExclusive (boundedPageLimit limit))

boundedPageLimit :: Int -> Int
boundedPageLimit = min 1000 . max 1

-- | Return a bounded full-history window centered on a durable turn index.
-- The requested turn is included when it exists. Near either edge the result
-- is intentionally not rebalanced; callers can use the page flags to continue
-- in either direction without an eager full-session load.
loadSessionHistoryTurnsAround
    :: StorePool
    -> OsPath
    -> Text
    -> Int64
    -> Int
    -> IO (Either Text SessionTurnPage)
loadSessionHistoryTurnsAround pool root sessionId center radius
    | center < 0 = pure (Left "turn index must be non-negative")
    | radius < 0 = pure (Left "turn radius must be non-negative")
    | otherwise =
        loadSessionHistoryTurnsRange
            pool
            root
            sessionId
            (max 0 (center - fromIntegral boundedRadius))
            (boundedRadius * 2 + 1)
  where
    boundedRadius = min 500 radius

loadSessionHistorySnapshot
    :: StorePool
    -> OsPath
    -> Text
    -> IO (Either Text (SessionMeta, Int64, Int64))
loadSessionHistorySnapshot pool root sessionId = runExceptT do
    _ <- except (sessionDirForId root sessionId)
    stored <- loadWithLegacyImport
        root pool sessionId Store.loadSessionHistorySnapshot
    meta <- except (fromStoredMetadata stored.sessionSnapshotMetadata)
    validateSessionMeta sessionSchemaVersion isValidSessionId sessionId meta
    pure
        ( meta
        , stored.sessionSnapshotStart
        , stored.sessionSnapshotTotal
        )

loadSessionResumeStats
    :: StorePool
    -> OsPath
    -> Text
    -> IO (Either Text SessionResumeStats)
loadSessionResumeStats pool root sessionId = runExceptT do
    _ <- except (sessionDirForId root sessionId)
    stored <- loadWithLegacyImport root pool sessionId Store.loadSessionResumeStats
    pure SessionResumeStats
        { resumeStatsTurnCount = fromIntegral stored.sessionResumeTurnCount
        , resumeStatsMessageCount = fromIntegral stored.sessionResumeMessageCount
        , resumeStatsToolCount = fromIntegral stored.sessionResumeToolCount
        , resumeStatsFirstPrompt =
            fmap Text.strip stored.sessionResumeFirstPrompt
        }

loadSessionTurnPage
    :: OsPath
    -> StorePool
    -> Text
    -> (StorePool -> Text
        -> IO (Either StoreError (Maybe Store.SessionTurnPage)))
    -> IO (Either Text SessionTurnPage)
loadSessionTurnPage root pool sessionId loader = runExceptT do
    _ <- except (sessionDirForId root sessionId)
    stored <- loadWithLegacyImport root pool sessionId loader
    turns <- except $ traverse
        (\storedTurn -> do
            turn <- fromStoredTurn storedTurn.storedTurn
            pure (storedTurn.storedTurnIndex, turn))
        (Vector.toList stored.sessionPageTurns)
    pure SessionTurnPage
        { pageTurns = turns
        , pageGenerationStart = stored.sessionPageGenerationStart
        , pageTotalTurns = stored.sessionPageTotal
        , pageHasOlder = stored.sessionPageHasOlder
        , pageHasNewer = stored.sessionPageHasNewer
        }

loadWithLegacyImport
    :: OsPath
    -> StorePool
    -> Text
    -> (StorePool -> Text -> IO (Either StoreError (Maybe a)))
    -> ExceptT Text IO a
loadWithLegacyImport root pool sessionId loader = do
    stored <- lift (loader pool sessionId)
        >>= either (throwE . renderStoreError) pure
    stored' <- case stored of
        Just value -> pure (Just value)
        Nothing -> do
            _ <- importLegacySession
                sessionSchemaVersion
                isValidSessionId
                (sessionDirForId root)
                pool
                sessionId
            -- Another process may win the import race and return False from
            -- its idempotent insert. Always reload the canonical row.
            lift (loader pool sessionId)
                >>= either (throwE . renderStoreError) pure
    maybe (throwE ("session not found: " <> sessionId)) pure stored'

-- | Load several sessions with one batched PostgreSQL read while preserving
-- request order. A missing database row still takes the legacy import path.
loadSessions
    :: StorePool
    -> OsPath
    -> [Text]
    -> IO [Either Text (SessionMeta, [SessionTurn])]
loadSessions pool root sessionIds = do
    let validated =
            [ sessionDirForId root sessionId
                >> Right sessionId
            | sessionId <- sessionIds
            ]
        validIds = [sessionId | Right sessionId <- validated]
    stored <- Store.loadSessions pool validIds
    restoreResults validated stored
  where
    restoreResults [] [] = pure []
    restoreResults (Left err : rest) results =
        (Left err :) <$> restoreResults rest results
    restoreResults (Right sessionId : rest) (result : results) = do
        loaded <- case result of
            Left err -> pure (Left (renderStoreError err))
            Right Nothing -> loadSession pool root sessionId
            Right (Just value) ->
                runExceptT
                    (decodeStoredSession
                        sessionSchemaVersion
                        isValidSessionId
                        sessionId
                        Nothing
                        value)
        (loaded :) <$> restoreResults rest results
    restoreResults _ _ =
        pure [Left "batched session load returned an unexpected result count"]

loadSessionHandle
    :: StorePool
    -> OsPath
    -> Text
    -> IO (Either Text (SessionHandle, [SessionTurn]))
loadSessionHandle pool root sessionId =
    loadActiveSession pool root sessionId >>= \case
        Left err -> pure (Left err)
        Right (meta, turns) ->
            pure do
                dir <- sessionDirForId root sessionId
                tempDir <- sessionTempDirForId root sessionId
                Right
                    ( SessionHandle
                        { sessionPool = pool
                        , sessionDir = dir
                        , sessionTempDir = tempDir
                        , sessionMetaPath = dir </> unsafeEncodeUtf "meta.json"
                        , sessionTranscriptPath =
                            dir </> unsafeEncodeUtf "transcript.jsonl"
                        , sessionMeta = meta
                        }
                    , turns
                    )

deleteSession :: StorePool -> OsPath -> Text -> IO (Either Text ())
deleteSession pool root sessionId = runExceptT do
    dir <- except (sessionDirForId root sessionId)
    exists <- lift (doesDirectoryExist dir)
    lock <- if exists
        then lift (acquireSessionLock dir sessionId) >>= \case
            Left _ -> throwE "cannot delete a running session"
            Right lock -> pure (Just lock)
        else pure Nothing
    now <- lift getCurrentTime
    deleted <- lift $
        Store.deleteSession pool sessionId now
            `finally` maybe (pure ()) releaseSessionLock lock
    case deleted of
        Left err -> throwE (renderStoreError err)
        Right False -> throwE ("session not found: " <> sessionId)
        Right True
            | not exists -> pure ()
            | otherwise ->
                lift (tryIO (removePathForcibly dir)) >>= \case
                    Left err ->
                        throwE
                            ("session deleted but artifacts could not be removed: "
                                <> Text.pack (displayException err))
                    Right () -> pure ()
    tempRemoved <- lift (removeSessionTemp root sessionId)
    except tempRemoved

renameSession
    :: StorePool
    -> OsPath
    -> Text
    -> Text
    -> IO (Either Text SessionMeta)
renameSession pool root sessionId rawTitle = runExceptT do
    let title = Text.unwords (Text.words (Text.strip rawTitle))
    when (Text.null title) (throwE "session title cannot be empty")
    _ <- except (sessionDirForId root sessionId)
    now <- lift getCurrentTime
    renamed <- lift $
        Store.setSessionTitle pool sessionId title True 2 now
    case renamed of
        Left err -> throwE (renderStoreError err)
        Right False -> throwE ("session not found: " <> sessionId)
        Right True -> lift (loadSessionMeta pool root sessionId) >>= except

setSessionArchived
    :: StorePool
    -> OsPath
    -> Text
    -> Bool
    -> IO (Either Text ())
setSessionArchived pool root sessionId archived = runExceptT do
    _ <- except (sessionDirForId root sessionId)
    now <- lift getCurrentTime
    changed <- lift $
        Store.setSessionArchived pool sessionId archived now
    case changed of
        Left err -> throwE (renderStoreError err)
        Right False -> throwE ("session not found: " <> sessionId)
        Right True -> pure ()

listSessions :: StorePool -> OsPath -> IO ([SessionMeta], [Text])
listSessions pool _root = do
    Store.listSessionMetadata pool >>= \case
        Left err ->
            fail
                ("could not list PostgreSQL sessions: "
                    <> Text.unpack (renderStoreError err))
        Right values ->
            let decoded = map decodeListedSessionMeta values
            in pure
                ( [meta | Right meta <- decoded]
                , [err | Left err <- decoded]
                )

-- | Decode one persisted session for listing. Corrupt or incompatible
-- metadata becomes an error string instead of disappearing from the picker.
decodeListedSessionMeta :: Store.SessionMetadata -> Either Text SessionMeta
decodeListedSessionMeta value = do
    meta <- fromStoredMetadata value
    unless (meta.metaVersion == sessionSchemaVersion) $
        Left $
            "unsupported session schema version "
                <> Text.pack (show meta.metaVersion)
                <> " for session "
                <> meta.metaId
                <> " (expected "
                <> Text.pack (show sessionSchemaVersion)
                <> ")"
    pure meta

listArchivedSessionIds :: StorePool -> IO (Either Text [Text])
listArchivedSessionIds pool =
    Store.listSessionArchiveKeys pool >>= \case
        Left err -> pure (Left (renderStoreError err))
        Right sessionIds -> pure (Right sessionIds)

writeSessionMeta :: StorePool -> OsPath -> SessionMeta -> IO ()
writeSessionMeta pool _path meta = do
    Store.replaceSessionMetadata
        pool
        "session.metadata_replaced"
        (toStoredMetadata meta) >>= \case
            Left err ->
                fail
                    ("could not update PostgreSQL session metadata: "
                        <> Text.unpack (renderStoreError err))
            Right False ->
                fail ("session not found: " <> Text.unpack meta.metaId)
            Right True -> pure ()
