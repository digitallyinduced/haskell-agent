-- | Full-fidelity session transfer import, export, and materialization.
module Agent.CLI.Session.Transfer
    ( SessionTransferEnvelope(..)
    , sessionTransferFormatVersion
    , validateSessionTransferEnvelope
    , importSessionTransfer
    , importSessionTransferRemapped
    , exportSessionTransfer
    , streamSessionTransfer
    , forkSessionAtTurn
    ) where

import Agent.CLI.Session.Codec
    ( contentFingerprint
    , toStoredMetadata
    , toStoredPromptSnapshot
    , toStoredTurn
    )
import Agent.CLI.Session.Storage
    ( loadSession
    , loadSessionHistorySnapshot
    , loadSessionHistoryTurnsRangeBounded
    )
import Agent.CLI.Session.TaskPlan
    ( fromStoredTaskPlan
    , taskPlanTransferJson
    , toStoredTaskPlanItem
    )
import Agent.CLI.Session.TempWorkspace
    ( allocateSessionTemp
    , ensurePrivateDir
    , ensureSessionTemp
    , isValidSessionId
    , removeSessionTemp
    , sessionDirForId
    )
import Agent.CLI.Session.Types
    ( SessionMeta(..)
    , SessionTransfer(..)
    , SessionTurn(..)
    , SessionTurnPage(..)
    , TranscriptEffect(..)
    )
import Agent.Loop (TokenUsage(..))
import Agent.OsPath (unsafeToFilePath)
import Agent.Store.Postgres (normalizePostgresTimestamp)
import Agent.Store.Postgres.Connection (StorePool)
import qualified Agent.Store.Postgres.Session as Store
import Agent.Store.Types (renderStoreError)
import Agent.Tools.TaskPlan (CurrentTaskPlan(..), TaskPlan(..))
import Control.Applicative ((<|>))
import Control.Exception.Safe (tryIO)
import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (except, runExceptT, throwE)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)
import System.Directory.OsPath
    ( createDirectory
    , doesDirectoryExist
    , removePathForcibly
    )
import System.OsPath (OsPath)
import System.Posix.Files (setFileMode)

-- | Stable interchange envelope for full-fidelity session transfers.
data SessionTransferEnvelope = SessionTransferEnvelope
    { transferFormat :: !Text
    , transferFormatVersion :: !Int
    , transferSession :: !SessionTransfer
    }
    deriving (Eq, Show)

sessionTransferFormatVersion :: Int
sessionTransferFormatVersion = 1

sessionTransferFormatName :: Text
sessionTransferFormatName = "haskell-agent.session-transfer"

instance Aeson.ToJSON SessionTransferEnvelope where
    toJSON envelope = Aeson.object
        [ "format" Aeson..= envelope.transferFormat
        , "version" Aeson..= envelope.transferFormatVersion
        , "session" Aeson..= envelope.transferSession
        ]

instance Aeson.FromJSON SessionTransferEnvelope where
    parseJSON = Aeson.withObject "SessionTransferEnvelope" \object -> do
        envelope <- SessionTransferEnvelope
            <$> object Aeson..: "format"
            <*> object Aeson..: "version"
            <*> object Aeson..: "session"
        either (fail . Text.unpack) pure
            (validateSessionTransferEnvelope envelope)

validateSessionTransferEnvelope
    :: SessionTransferEnvelope
    -> Either Text SessionTransferEnvelope
validateSessionTransferEnvelope envelope
    | envelope.transferFormat /= sessionTransferFormatName =
        Left "unsupported session transfer format"
    | envelope.transferFormatVersion /= sessionTransferFormatVersion =
        Left
            ("unsupported session transfer version: "
                <> Text.pack (show envelope.transferFormatVersion))
    | not (isValidSessionId meta.metaId) =
        Left "invalid transferred session id"
    | meta.metaVersion <= 0 =
        Left "invalid transferred session schema version"
    | length turns > maximumTransferTurns =
        Left "session transfer contains too many turns"
    | any invalidTurn turns =
        Left "session transfer contains an oversized turn"
    | otherwise = Right envelope
  where
    transfer = envelope.transferSession
    meta = transfer.transferMeta
    turns = transfer.transferTurns
    invalidTurn turn =
        Text.length turn.turnUserText > maximumTransferTextLength
            || maybe False
                ((> maximumTransferTextLength) . Text.length)
                turn.turnAssistantText
            || maybe False
                ((> maximumTransferTextLength) . Text.length)
                turn.turnError

maximumTransferTurns :: Int
maximumTransferTurns = 100000

maximumTransferTextLength :: Int
maximumTransferTextLength = 16 * 1024 * 1024

-- | Import a transferred session under its existing id and optional cwd.
importSessionTransfer
    :: StorePool
    -> OsPath
    -> Maybe OsPath
    -> SessionTransfer
    -> IO (Either Text Text)
importSessionTransfer pool root cwd transfer = runExceptT do
    let sessionId = transfer.transferMeta.metaId
    dir <- except (sessionDirForId root sessionId)
    exists <- lift (doesDirectoryExist dir)
    when exists (throwE ("session already exists: " <> sessionId))
    lift (ensurePrivateDir root)
    lift (createDirectory dir)
    lift (setFileMode (unsafeToFilePath dir) 0o700)
    _ <- lift (ensureSessionTemp root sessionId) >>= except
    let meta = transfer.transferMeta
            { metaCwd = fromMaybe transfer.transferMeta.metaCwd cwd }
        bytes = Aeson.encode
            (SessionTransfer meta transfer.transferTaskPlan transfer.transferTurns)
        legacy = Store.LegacySession
            { legacySourcePath = "afk:" <> transfer.transferMeta.metaId
            , legacyContentHash = contentFingerprint bytes
            , legacyMetadata = toStoredMetadata meta
            , legacyTurns = map toStoredTurn transfer.transferTurns
            , legacyPromptSnapshot =
                toStoredPromptSnapshot <$> meta.metaPromptSnapshot
            , legacyTaskPlan =
                fmap
                    (\plan ->
                        Store.SessionTaskPlanSnapshot
                            { Store.sessionTaskPlanSnapshotExplanation =
                                plan.taskPlanExplanation
                            , Store.sessionTaskPlanSnapshotItems =
                                map toStoredTaskPlanItem plan.taskPlanItems
                            })
                    transfer.transferTaskPlan
            }
    lift (Store.importLegacySession pool legacy) >>= \case
        Left err -> do
            lift (cleanupTransfer dir sessionId)
            throwE (renderStoreError err)
        Right False -> do
            lift (cleanupTransfer dir sessionId)
            throwE ("session already exists: " <> sessionId)
        Right True -> pure sessionId
  where
    cleanupTransfer dir sessionId = do
        _ <- tryIO (removePathForcibly dir)
        _ <- removeSessionTemp root sessionId
        pure ()

-- | Import a validated transfer as a new session. The source id is never
-- reused, which makes importing the same file safe and preserves the source
-- transcript as an immutable object.
importSessionTransferRemapped
    :: StorePool
    -> OsPath
    -> Maybe OsPath
    -> SessionTransferEnvelope
    -> IO (Either Text Text)
importSessionTransferRemapped pool root cwd rawEnvelope =
    case validateSessionTransferEnvelope rawEnvelope of
        Left err -> pure (Left err)
        Right envelope -> do
            (sessionId, _) <- allocateSessionTemp root
            now <- normalizePostgresTimestamp <$> getCurrentTime
            let transfer = envelope.transferSession
                sourceMeta = transfer.transferMeta
                meta = sourceMeta
                    { metaId = sessionId
                    , metaCreatedAt = now
                    , metaUpdatedAt = now
                    }
            importSessionTransfer pool root cwd
                transfer { transferMeta = meta }

exportSessionTransfer
    :: StorePool
    -> OsPath
    -> Text
    -> IO (Either Text SessionTransferEnvelope)
exportSessionTransfer pool root sessionId =
    loadSession pool root sessionId >>= \case
        Left err -> pure (Left err)
        Right (meta, turns) ->
            Store.loadSessionTaskPlan pool sessionId <&> \result -> do
                storedPlan <- either (Left . renderStoreError) Right result
                validateSessionTransferEnvelope SessionTransferEnvelope
                    { transferFormat = sessionTransferFormatName
                    , transferFormatVersion = sessionTransferFormatVersion
                    , transferSession =
                        SessionTransfer
                            meta
                            ((.currentTaskPlanValue) . fromStoredTaskPlan <$> storedPlan)
                            turns
                    }

-- | Stream a transfer document in bounded turn pages. Callback bytes are
-- valid only for the duration of the callback. The export captures the turn
-- count from its first page and ignores later appends, yielding an immutable
-- prefix without hydrating the full transcript.
streamSessionTransfer
    :: StorePool
    -> OsPath
    -> Text
    -> (BS.ByteString -> IO ())
    -> IO (Either Text ())
streamSessionTransfer pool root sessionId emit = runExceptT do
    (meta, snapshotStart, snapshotTotal) <-
        lift (loadSessionHistorySnapshot pool root sessionId) >>= except
    storedPlan <- lift (Store.loadSessionTaskPlan pool sessionId)
        >>= either (throwE . renderStoreError) pure
    let taskPlan = ((.currentTaskPlanValue) . fromStoredTaskPlan) <$> storedPlan
    let snapshotEnd = snapshotStart + max 0 snapshotTotal
    lift $ emitLazy
        ("{\"format\":\"haskell-agent.session-transfer\","
            <> "\"version\":1,\"session\":{\"meta\":"
            <> Aeson.encode meta
            <> ",\"currentTaskPlan\":"
            <> Aeson.encode (fmap taskPlanTransferJson taskPlan)
            <> ",\"turns\":[")
    firstPage <- lift
        (loadSessionHistoryTurnsRangeBounded
            pool root sessionId snapshotStart snapshotEnd 256)
        >>= except
    let total = max 0 snapshotTotal
    emitted <- lift (emitPage True total 0 firstPage)
    when (emitted < total) $
        streamRest snapshotEnd total emitted firstPage
    lift (emitLazy "]}}")
  where
    emitLazy = mapM_ emit . LBS.toChunks

    emitPage first total emitted page = do
        let remaining = max 0 (total - emitted)
            selected = take (fromIntegral remaining) page.pageTurns
        _ <- foldl'
            (\action (_, turn) -> do
                isFirst <- action
                unless isFirst (emit ",")
                emitLazy (Aeson.encode turn)
                pure False)
            (pure first)
            selected
        pure (emitted + fromIntegral (length selected))

    streamRest snapshotEnd total emitted previous =
        case reverse previous.pageTurns of
            [] -> throwE "session export paging made no progress"
            (lastIndex, _) : _ -> do
                page <- lift
                    (loadSessionHistoryTurnsRangeBounded
                        pool root sessionId (lastIndex + 1) snapshotEnd 256)
                    >>= except
                next <- lift (emitPage False total emitted page)
                when (next <= emitted) $
                    throwE "session export paging made no progress"
                when (next < total) (streamRest snapshotEnd total next page)

-- | Fork a source session through an inclusive durable turn index. The source
-- remains untouched. Transcript effects are copied verbatim, while derived
-- usage and continuation metadata are recomputed from the selected prefix.
forkSessionAtTurn
    :: StorePool
    -> OsPath
    -> Text
    -> Int64
    -> IO (Either Text Text)
forkSessionAtTurn pool root sourceId throughIndex = runExceptT do
    when (throughIndex < 0) (throwE "turn index must be non-negative")
    (sourceMeta, snapshotStart, snapshotTotal) <-
        lift (loadSessionHistorySnapshot pool root sourceId) >>= except
    let snapshotEnd = snapshotStart + max 0 snapshotTotal
    when
        (throughIndex < snapshotStart || throughIndex >= snapshotEnd) $
        throwE "turn index is outside the session transcript"
    turns <- loadPrefix snapshotStart (throughIndex + 1) id
    let
        usage = foldl' addUsage
            (TokenUsage
                { inputTokens = 0
                , outputTokens = 0
                , cachedTokens = 0
                })
            (mapMaybe (.turnUsage) turns)
        meta = sourceMeta
            { metaLastResponseId = continuationResponseId turns
            , metaInputTokens = usage.inputTokens
            , metaOutputTokens = usage.outputTokens
            , metaCachedTokens = usage.cachedTokens
            , metaLastRecap = Nothing
            , metaLastTurnSummary = Nothing
            , metaLastRecapMainTurns = 0
            , metaTitleUserTurns =
                length (filter (not . Text.null . Text.strip . (.turnUserText)) turns)
            }
        envelope = SessionTransferEnvelope
            { transferFormat = sessionTransferFormatName
            , transferFormatVersion = sessionTransferFormatVersion
            , transferSession = SessionTransfer meta Nothing turns
            }
    lift (importSessionTransferRemapped pool root Nothing envelope) >>= except
  where
    loadPrefix cursor endExclusive append
        | cursor >= endExclusive = pure (append [])
        | otherwise = do
            page <- lift
                (loadSessionHistoryTurnsRangeBounded
                    pool root sourceId cursor endExclusive 256)
                >>= except
            case page.pageTurns of
                [] -> throwE "session fork paging made no progress"
                values -> do
                    let lastIndex = fst (last values)
                        pageTurns = map snd values
                    loadPrefix
                        (lastIndex + 1)
                        endExclusive
                        (append . (pageTurns <>))

    addUsage left right = TokenUsage
        { inputTokens = left.inputTokens + right.inputTokens
        , outputTokens = left.outputTokens + right.outputTokens
        , cachedTokens = left.cachedTokens + right.cachedTokens
        }

    continuationResponseId = foldl' step Nothing
    step previous turn =
        let base = case turn.turnEffect of
                TranscriptAppend -> previous
                TranscriptReplace -> Nothing
                TranscriptReset -> Nothing
        in turn.turnResponseId <|> base
