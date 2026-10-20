-- | Durable provider-native history. Persistence is completed before a tool
-- event is acknowledged, including the final result preceding suspension.
module Agent.Subagents.DurableTranscript
    ( DurableTranscriptConfig (..)
    , newDurableTranscriptBackend
    , newDurableTranscriptBackendWithInput
    , newDurableTranscriptBackendWithInbox
    ) where

import Agent.Error (ApiError)
import qualified Agent.Loop as Agent
import Agent.Loop.Native (NativeCodec (..))
import Control.Concurrent.MVar
import Control.Exception.Safe (onException)
import Control.Monad (unless, void)
import Data.IORef
import Data.Text (Text)

data DurableTranscriptConfig item response = DurableTranscriptConfig
    { codec :: NativeCodec item response
    , continuationNamespace :: Text
    , loadTranscript :: IO [item]
    , saveTranscript :: [item] -> IO ()
    -- | Retain the provider's latest opaque compaction checkpoint.
    , retainHistory :: [item] -> [item]
    , prepareReplay :: [item] -> [item]
    -- | Only completed tool outputs have a deduplication identifier.
    , toolOutputIdentifier :: item -> Maybe Text
    , submit :: Maybe Text -> [item] -> IO (Either ApiError response)
    , recordResponse :: response -> IO ()
    }

newDurableTranscriptBackend
    :: DurableTranscriptConfig item response -> IO (Agent.Backend, Agent.LoopEvent -> IO ())
newDurableTranscriptBackend config = do
    (backend, recordEvent, _) <- newDurableTranscriptBackendWithInput config
    pure (backend, recordEvent)

-- | Additional native inputs are committed immediately and included exactly
-- once in the next continuation, or replayed from history after a pause.
newDurableTranscriptBackendWithInput
    :: DurableTranscriptConfig item response
    -> IO (Agent.Backend, Agent.LoopEvent -> IO (), [item] -> IO ())
newDurableTranscriptBackendWithInput config =
    newDurableTranscriptBackendWithInbox config \items ->
        [] <$ config.saveTranscript items

-- | The host atomically saves the supplied history, appends accepted mailbox
-- items and acknowledges those items in durable storage before a request.
-- Other history writes use saveTranscript without consuming the inbox.
-- Return just the mailbox suffix. A single lock protects pending continuation
-- input, including mail received while a provider request is in flight.
newDurableTranscriptBackendWithInbox
    :: DurableTranscriptConfig item response
    -> ([item] -> IO [item])
    -> IO (Agent.Backend, Agent.LoopEvent -> IO (), [item] -> IO ())
newDurableTranscriptBackendWithInbox config saveWithInbox = do
    initial <- config.loadTranscript
    state <- newMVar (config.retainHistory initial, [], Nothing)
    usable <- newIORef True
    let native = config.codec
        append items keepForContinuation readInbox = modifyMVarMasked state \(previous, pending, responseId) -> do
            valid <- readIORef usable
            unless valid (fail "Durable transcript must be reloaded after a failed commit")
            let candidate = config.retainHistory (foldl appendFresh previous items)
            -- COMMIT may have succeeded even if its acknowledgement failed.
            -- Never overwrite that committed inbox with stale local history.
            incoming <- (if readInbox then saveWithInbox candidate else [] <$ config.saveTranscript candidate)
                `onException` writeIORef usable False
            let updated = config.retainHistory (candidate <> incoming)
                remaining = pending <> incoming <> [item | keepForContinuation, item <- items]
            pure ((updated, remaining, responseId), (updated, remaining, responseId))
        appendFresh previous item = case config.toolOutputIdentifier item of
            Just identifier | any ((== Just identifier) . config.toolOutputIdentifier) previous -> previous
            _ -> previous <> [item]
        recordEvent = \case
            Agent.ToolFinished result -> void (append [native.completedToolItem result] False False)
            _ -> pure ()
        backend = Agent.backendWithCallbacks \snapshot previous inputs _ -> do
            let newItems =
                    [native.completedToolItem result | Agent.CompletedTool result <- inputs]
                        <> native.auxiliaryInputItems inputs
            (retained, additionalItems, responseId) <- append newItems False True
            -- A response may have been persisted just as steering cancelled
            -- its publication to the loop. Its old continuation cannot omit
            -- the durable response and the guidance that produced it.
            let continuation = case responseId of
                    Just known | previous /= Just known -> Nothing
                    _ -> previous
                requestItems = maybe (config.prepareReplay retained) (const (newItems <> additionalItems)) continuation
            config.submit continuation requestItems >>= \case
                Left err -> pure (Left err)
                Right response -> do
                    config.recordResponse response
                    void (append (native.responseItems response) False False)
                    modifyMVar_ state \(items, pending, _) ->
                        pure (items, drop (length additionalItems) pending, Just (native.responseIdentifier response))
                    pure $ Right Agent.BackendResult
                        { backendOutput = native.responseTurnOutput response
                        , backendState = Agent.advanceBackendSnapshot snapshot
                            (native.responseToolItems response)
                            (Just (Agent.BackendContinuation config.continuationNamespace
                                (native.responseIdentifier response)))
                        }
        appendAdditional items = void (append items True False)
    pure (backend, recordEvent, appendAdditional)
