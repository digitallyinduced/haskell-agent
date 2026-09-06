-- | Provider-neutral loop adapters for Responses-compatible transports.
--
-- This facade preserves the provider-facing API. Wire input preparation,
-- completed-output conversion and attempt-local stream projection live in
-- separate private modules.
module Agent.Responses.LoopBackend
    ( statelessResponsesBackend
    , statelessResponsesBackendPreservingCheckpointHistory
    , statelessResponsesBackendPreservingHistory
    , statelessResponsesBackendWithRawReasoning
    , tokenProviderStatelessResponsesBackend
    , tokenProviderStatelessResponsesBackendPreservingCheckpointHistory
    , tokenProviderStatelessResponsesBackendPreservingHistory
    , turnInputsToItems
    , responseToTurnOutput
    , responseItemToToolCall
    , responseTokenUsage
    , streamEventToLoopEvent
    , streamEventToLoopEventWithRawReasoning
    , StreamProjectionState
    , emptyStreamProjectionState
    , streamEventToLoopEventsStep
    , newStreamEventToLoopEvents
    , toolArgumentActivityChunkChars
    , runawayToolArgumentWarningChars
    , streamOutputObserved
    , hasRecoverableIncompleteOutput
    , responseNeedsLoopContinuation
    , assistantTextFromResponse
    , toolResultToItem
    , withRequestInput
    , normalizeResponseInputItems
    , isServerCompactionCheckpoint
    ) where

import Agent.Error (ApiError)
import Agent.Loop
    ( Backend(..)
    , BackendCallbacks(..)
    , BackendResult(..)
    , BackendSnapshot(..)
    , advanceBackendSnapshot
    , backendWithCallbacks
    )
import Agent.Provider
    ( Credential
    , TokenProvider
    , runWithTokenProvider
    )
import Agent.Responses.LoopBackend.Input
import Agent.Responses.LoopBackend.Output
import Agent.Responses.LoopBackend.StreamProjection
import Agent.Responses.Request
    ( filterCompactionCheckpointsByOrigin
    , isServerCompactionCheckpoint
    )
import Agent.Responses.Types
import Data.Maybe (fromMaybe)

-- | Adapt a stateless Responses transport to the provider-neutral loop.
statelessResponsesBackend
    :: (ResponseCreateParams
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
statelessResponsesBackend send getParams =
    statelessResponsesBackendWithRawReasoning True send getParams

-- | Adapt a stateless transport whose opaque checkpoints are replayable, but
-- only by that same provider. Keep the complete pre-checkpoint history in host
-- state for later provider switches; the provider's wire projection must trim
-- that portable prefix when it replays its checkpoint.
statelessResponsesBackendPreservingCheckpointHistory
    :: (ResponseCreateParams
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
statelessResponsesBackendPreservingCheckpointHistory send getParams =
    statelessResponsesBackendWithMode
        PreservePreCheckpointHistoryAndCheckpoint
        True
        send
        getParams

-- | Adapt a stateless transport that cannot safely replay opaque server
-- checkpoints. Keep the complete request history even when a response
-- contains a checkpoint, and omit the unusable checkpoint itself from the
-- next snapshot so a later provider cannot mistake it for compatible state.
statelessResponsesBackendPreservingHistory
    :: (ResponseCreateParams
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
statelessResponsesBackendPreservingHistory send getParams =
    statelessResponsesBackendWithMode
        PreservePreCheckpointHistory
        True
        send
        getParams

-- | Adapt a stateless Responses transport while optionally exposing raw
-- reasoning text. Reasoning summaries remain visible in either mode.
statelessResponsesBackendWithRawReasoning
    :: Bool
    -> (ResponseCreateParams
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
statelessResponsesBackendWithRawReasoning showRawReasoning send getParams =
    statelessResponsesBackendWithMode
        ReplacePreCheckpointHistory
        showRawReasoning
        send
        getParams

data ServerCheckpointMode
    = ReplacePreCheckpointHistory
    | PreservePreCheckpointHistory
    | PreservePreCheckpointHistoryAndCheckpoint

statelessResponsesBackendWithMode
    :: ServerCheckpointMode
    -> Bool
    -> (ResponseCreateParams
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
statelessResponsesBackendWithMode
        checkpointMode
        showRawReasoning
        send
        getParams =
    backendWithCallbacks
        \snapshot _legacyPreviousResponseId inputs callbacks -> do
        baseParams <- getParams
        projectEvent <- newStreamEventToLoopEvents showRawReasoning
        let newItems = turnInputsToItems inputs
            requestItems = snapshot.backendItems <> newItems
            request = withRequestInput baseParams requestItems
        result <- send request \event -> do
            projectEvent event >>= mapM_ callbacks.onLoopEvent
            case completedAsyncToolCall event of
                Just call -> callbacks.onAsyncToolCall call
                Nothing -> pure ()
        case result of
            Left err -> pure (Left err)
            Right response ->
                let normalizedRequestItems =
                        normalizeResponseInputItems requestItems
                    completedItems =
                        case checkpointMode of
                            ReplacePreCheckpointHistory ->
                                fromMaybe
                                    (normalizedRequestItems <> response.output)
                                    (latestServerCheckpointSuffix response.output)
                            PreservePreCheckpointHistory ->
                                normalizedRequestItems
                                    <> filterCompactionCheckpointsByOrigin
                                        (const False)
                                        response.output
                            PreservePreCheckpointHistoryAndCheckpoint ->
                                normalizedRequestItems <> response.output
                in
                pure $ Right BackendResult
                    { backendOutput = responseToTurnOutput response
                    , backendState =
                        advanceBackendSnapshot snapshot
                            completedItems
                            Nothing
                    }

-- Server compaction checkpoints replace everything that preceded them. Keep
-- the checkpoint and later output in the stateless snapshot so subsequent
-- requests do not replay the obsolete pre-compaction transcript.
latestServerCheckpointSuffix
    :: [ResponseItem]
    -> Maybe [ResponseItem]
latestServerCheckpointSuffix = go [] . reverse
  where
    go _ [] = Nothing
    go after (item : before)
        | isServerCompactionCheckpoint item = Just (item : after)
        | otherwise = go (item : after) before

-- | Adapt a credentialed stateless Responses transport to the loop.
--
-- Credential acquisition and account failover are shared across providers;
-- the transport remains responsible only for one request with one credential.
tokenProviderStatelessResponsesBackend
    :: TokenProvider
    -> (Credential
        -> ResponseCreateParams
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
tokenProviderStatelessResponsesBackend provider send =
    statelessResponsesBackend \params onEvent ->
        runWithTokenProvider provider \credential ->
            send credential params onEvent

-- | Credentialed counterpart to
-- 'statelessResponsesBackendPreservingCheckpointHistory'.
tokenProviderStatelessResponsesBackendPreservingCheckpointHistory
    :: TokenProvider
    -> (Credential
        -> ResponseCreateParams
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
tokenProviderStatelessResponsesBackendPreservingCheckpointHistory
        provider
        send =
    statelessResponsesBackendPreservingCheckpointHistory \params onEvent ->
        runWithTokenProvider provider \credential ->
            send credential params onEvent

-- | Credentialed counterpart to
-- 'statelessResponsesBackendPreservingHistory'.
tokenProviderStatelessResponsesBackendPreservingHistory
    :: TokenProvider
    -> (Credential
        -> ResponseCreateParams
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
tokenProviderStatelessResponsesBackendPreservingHistory provider send =
    statelessResponsesBackendPreservingHistory \params onEvent ->
        runWithTokenProvider provider \credential ->
            send credential params onEvent
