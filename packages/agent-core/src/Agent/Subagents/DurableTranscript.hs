-- | Durable provider-native history. Persistence is completed before a tool
-- event is acknowledged, including the final result preceding suspension.
module Agent.Subagents.DurableTranscript
    ( DurableTranscriptConfig (..)
    , newDurableTranscriptBackend
    , newDurableTranscriptBackendWithInput
    ) where

import Agent.Error (ApiError)
import qualified Agent.Loop as Agent
import Agent.Loop.Native (NativeCodec (..))
import Control.Concurrent.MVar
import Control.Monad (void)
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
newDurableTranscriptBackendWithInput config = do
    history <- config.loadTranscript >>= newMVar . config.retainHistory
    additional <- newMVar []
    let native = config.codec
        append items = modifyMVar history \previous -> do
            let updated = config.retainHistory (foldl appendFresh previous items)
            config.saveTranscript updated
            pure (updated, updated)
        appendFresh previous item = case config.toolOutputIdentifier item of
            Just identifier | any ((== Just identifier) . config.toolOutputIdentifier) previous -> previous
            _ -> previous <> [item]
        recordEvent = \case
            Agent.ToolFinished result -> void (append [native.completedToolItem result])
            _ -> pure ()
        backend = Agent.backendWithCallbacks \snapshot previous inputs _ -> do
            additionalItems <- readMVar additional
            let newItems =
                    [native.completedToolItem result | Agent.CompletedTool result <- inputs]
                        <> native.auxiliaryInputItems inputs
            retained <- append newItems
            let requestItems = maybe (config.prepareReplay retained) (const (newItems <> additionalItems)) previous
            config.submit previous requestItems >>= \case
                Left err -> pure (Left err)
                Right response -> do
                    config.recordResponse response
                    void (append (native.responseItems response))
                    modifyMVar_ additional (pure . drop (length additionalItems))
                    pure $ Right Agent.BackendResult
                        { backendOutput = native.responseTurnOutput response
                        , backendState = Agent.advanceBackendSnapshot snapshot
                            (native.responseToolItems response)
                            (Just (Agent.BackendContinuation config.continuationNamespace
                                (native.responseIdentifier response)))
                        }
        appendAdditional items = modifyMVar_ additional \pending -> do
            void (append items)
            pure (pending <> items)
    pure (backend, recordEvent, appendAdditional)
