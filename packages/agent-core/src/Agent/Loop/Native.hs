{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, OverloadedRecordDot, NamedFieldPuns, BlockArguments, LambdaCase #-}
module Agent.Loop.Native
    ( NativeLoopConfig(..)
    , NativeCodec(..)
    , NativeLoopOutcome(..)
    , NativeLoopLimit(..)
    , runNativeLoop
    , pollForNewSteering
    , noLiveSteering
    ) where

import Prelude
import qualified Agent.Loop as Agent
import Agent.Cancel (newCancelFlag, requestCancel)
import qualified Agent.ToolDispatch as AgentTool
import qualified Agent.Tools.Types as Tools
import qualified Agent.Responses.Types as Responses
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, readMVar, tryPutMVar)
import Control.Monad (forever, unless, when, void, forM_)
import qualified Control.Exception.Safe as Exception
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

-- | A projection, not a transcript conversion. Native input items (including
-- encrypted and provider-specific content) never round-trip through TurnInput.
-- Only scheduling information and tool identities enter the neutral loop.
data NativeCodec item response = NativeCodec
    { responseItems :: response -> [item]
    , responseIdentifier :: response -> Text
    , responseCompleted :: response -> Bool
    , responseContinues :: response -> Bool
    , responseTurnOutput :: response -> Agent.TurnOutput
    , responseToolItems :: response -> [Responses.ResponseItem]
    , itemCallIdentifier :: item -> Maybe Text
    , itemIsAssistantMessage :: item -> Bool
    , completedToolItem :: AgentTool.ToolCallResult -> item
    , finalizedToolItem :: AgentTool.ToolCall -> Text -> item
    , auxiliaryInputItems :: [Agent.TurnInput] -> [item]
    }

-- | Host integration for a provider-native transcript. Persistence and
-- authorization remain host responsibilities. The registry and approval
-- callback are mandatory: this adapter grants no ambient tool permissions.
data NativeLoopConfig item response = NativeLoopConfig
    { maxTurns :: !Int
    , continuationNamespace :: !Text
    , codec :: !(NativeCodec item response)
    , tools :: !Tools.ToolRegistry
    , approveTool :: !(AgentTool.ToolCall -> IO Agent.ToolApproval)
    , submit :: !(Int -> Maybe Text -> [item] -> IO response)
    , commitResponse :: !(response -> IO ())
    -- | Deliver the messages in this batch in native order. The supplied
    -- barrier must be awaited before delivering text after a tool call.
    , deliverOutput :: !((Text -> IO ()) -> [item] -> IO ())
    , persistToolOutput :: !(item -> IO ())
    , readSteering :: !(IO [item])
    , acknowledgeSteering :: !(Int -> IO ())
    -- | Returns once the host holds guidance that its last 'readSteering'
    -- did not return. The loop waits for it only while a model request is in
    -- flight and then abandons that request ('NativeLoopPreempted'). Hosts
    -- without live steering use 'noLiveSteering'.
    , awaitNewSteering :: !(IO ())
    -- | Tool effects and pending-upload recovery, read after the tool batch.
    , readAdditionalInput :: !(response -> IO [item])
    -- | Delegated child agents. The caller also advertises their tool
    -- definitions in the requests it submits.
    , subagents :: !(Maybe Agent.LoopSubagents)
    }

data NativeLoopOutcome item response
    = NativeLoopFinished !response
    | NativeLoopReachedLimit !(NativeLoopLimit item)
    -- | New guidance arrived while the model was answering, and the loop
    -- abandoned that request. The host must retire the interrupted transport
    -- when its provider cannot safely cancel or drain the response, and must
    -- respect the provider's continuation scope when reconnecting. It can
    -- resume from durable history containing everything committed so far.
    -- The new guidance is still unaccepted in its inbox.
    | NativeLoopPreempted

data NativeLoopLimit item = NativeLoopLimit
    { previousResponseId :: !(Maybe Text)
    , pendingItems :: ![item]
    -- | Only call after a successful wrap-up response accepted these inputs.
    , acknowledgePendingInput :: !(IO ())
    }

-- | The library's neutral TurnInput cannot represent arbitrary Responses
-- content (file references, encrypted and provider-specific items). Keep the
-- exact wire batch and expose one scheduling slot per item to agent-core.
-- Slots never reach the provider; submit uses the frozen native items below.
data InputBatch item = InputBatch
    { steeringItems :: ![item]
    , additionalItems :: ![item]
    , continueWithoutInput :: !Bool
    }

emptyInputBatch :: InputBatch item
emptyInputBatch = InputBatch [] [] False

batchItems :: InputBatch item -> [item]
batchItems batch = batch.additionalItems <> batch.steeringItems

batchSize :: InputBatch item -> Int
batchSize batch = max (length (batchItems batch)) (if batch.continueWithoutInput then 1 else 0)

runNativeLoop
    :: NativeLoopConfig item response
    -> Maybe Text
    -> [item]
    -> IO (NativeLoopOutcome item response)
runNativeLoop config initialResponseId initialItems = do
    cancel <- newCancelFlag
    preemptedRef <- newIORef False
    checkpointRef <- newIORef Agent.emptyBackendSnapshot
    responseRef <- newIORef Nothing
    batchRef <- newIORef emptyInputBatch
    iterationRef <- newIORef 0
    failureRef <- newIORef Nothing
    outputQueueRef <- newIORef []
    completedToolsRef <- newIORef Map.empty
    registry <- either (fail . Text.unpack) pure $ Tools.mkToolRegistry
        [ tool
            { Tools.appToolHandler = AgentTool.wrapToolHandler
                (\_ action -> rememberFailure failureRef (rethrowFailure failureRef >> action))
                tool.appToolHandler
            }
        | tool <- Tools.toolRegistryTools config.tools
        ]
    let native = config.codec
        completedToolItems inputs =
            [native.completedToolItem result | Agent.CompletedTool result <- inputs]
        requireResponse = readIORef responseRef >>= maybe
            (fail "NativeLoop: no committed response") pure
        toolCompletion callId = do
            completions <- readIORef completedToolsRef
            maybe (fail "NativeLoop: missing tool delivery barrier") pure
                (Map.lookup callId completions)
        deliverOutput items = do
            rethrowFailure failureRef
            config.deliverOutput
                (\callId -> do
                    rethrowFailure failureRef
                    toolCompletion callId >>= readMVar
                    rethrowFailure failureRef)
                items
        prepareTool call = rememberFailure failureRef do
            rethrowFailure failureRef
            queued <- readIORef outputQueueRef
            let (before, remaining) = break ((== Just call.callId) . native.itemCallIdentifier) queued
            -- agent-core prepares calls serially, but executes them in parallel.
            -- Only message boundaries wait for preceding tools. Keep those
            -- calls in the queue until their following text has been delivered.
            when (any native.itemIsAssistantMessage before) do
                deliverOutput before
                writeIORef outputQueueRef remaining
            approval <- config.approveTool call
            -- Rejected calls bypass tool dispatch, including its finalizer.
            -- Persist their synthetic output and release the same delivery
            -- barrier that an executed handler would release.
            case approval of
                Agent.ToolApprovalGranted -> pure ()
                Agent.ToolApprovalDenied reason -> void (finalizeOutput call reason)
                Agent.ToolApprovalRejected -> void (finalizeOutput call "Tool call rejected by user.")
            pure approval
        readBatch = rememberFailure failureRef do
            rethrowFailure failureRef
            queued <- readIORef outputQueueRef
            deliverOutput queued
            writeIORef outputQueueRef []
            previous <- readIORef responseRef
            additionalItems <- maybe (pure []) config.readAdditionalInput previous
            -- Tool effects can publish user-role inputs such as preview images.
            -- Include them in this read before deciding whether the turn ends.
            steeringItems <- config.readSteering
            let batch = InputBatch
                    { steeringItems
                    , additionalItems
                    , continueWithoutInput = maybe False native.responseContinues previous
                    }
            writeIORef batchRef batch
            pure (replicate (batchSize batch) (Agent.UserMessage ""))
        acknowledgeBatch count = rememberFailure failureRef do
            batch <- readIORef batchRef
            unless (count == batchSize batch) $
                fail "NativeLoop: steering acknowledgement does not match submitted batch"
            config.acknowledgeSteering (length batch.steeringItems)
            writeIORef batchRef emptyInputBatch
        backend = Agent.backendWithCallbacks \snapshot previous inputs callbacks ->
            rememberFailure failureRef do
                rethrowFailure failureRef
                callbacks.onCancellationMode Agent.CancelSubmission
                -- agent-core calls this once it preempts the request because
                -- 'awaitNewSteering' returned first. Resubmitting on this
                -- socket cannot work (see 'NativeLoopPreempted'), so the loop
                -- ends instead. False: there is no response to drain.
                callbacks.onSteeringInterrupt $ Just do
                    writeIORef preemptedRef True
                    requestCancel cancel
                    pure False
                iteration <- readIORef iterationRef
                batch <- readIORef batchRef
                let items = (if iteration == 0 then initialItems else [])
                        <> completedToolItems inputs <> batchItems batch
                        <> native.auxiliaryInputItems inputs
                response <- config.submit iteration previous items
                writeIORef responseRef (Just response)
                let callIds = mapMaybe native.itemCallIdentifier (native.responseItems response)
                forM_ callIds \callId -> do
                    completions <- readIORef completedToolsRef
                    unless (Map.member callId completions) do
                        completion <- newEmptyMVar
                        modifyIORef' completedToolsRef (Map.insert callId completion)
                -- Plain answers remain provisional until the final steering
                -- read. Only responses containing tools use ordered delivery.
                writeIORef outputQueueRef $
                    if native.responseCompleted response && not (null callIds)
                        then native.responseItems response
                        else []
                modifyIORef' iterationRef (+ 1)
                -- The opaque response id points to the provider's checkpoint;
                -- the host retains the native replay. Retain
                -- call identities for the core's pending-tool recovery without
                -- round-tripping the transcript through its narrower schema.
                pure $ Right Agent.BackendResult
                    { backendOutput = native.responseTurnOutput response
                    , backendState = Agent.advanceBackendSnapshot snapshot (native.responseToolItems response)
                        (Just (Agent.BackendContinuation config.continuationNamespace (native.responseIdentifier response)))
                    }
        stateStore = Agent.BackendStateStore
            { readBackendState = readIORef checkpointRef
            , commitBackendState = \candidate -> rememberFailure failureRef do
                response <- requireResponse
                config.commitResponse response
                writeIORef checkpointRef candidate
                pure candidate
            }
        finalizeOutput call output =
                (rememberFailure failureRef do
                    config.persistToolOutput (native.finalizedToolItem call output)
                    pure output)
                    `Exception.finally` do
                        completion <- toolCompletion call.callId
                        void (tryPutMVar completion ())
        dispatch = Agent.defaultLoopDispatch
            { AgentTool.toolDispatchFinalizeOutput = finalizeOutput
            }
    execution <- Agent.runLoopInputsDetailed Agent.LoopConfig
        { loopBackend = backend
        , loopBackendState = stateStore
        , loopTools = registry
        , loopReadTools = Nothing
        , loopDispatch = dispatch
        , loopMaxTurns = config.maxTurns
        , loopOnEvent = const (pure ())
        , loopApprove = prepareTool
        , loopReadSteering = readBatch
        -- agent-core races this against each model request (instant
        -- steering). The host's inbox knows which guidance it handed out, so
        -- the count of submitted inputs is not needed.
        , loopWaitSteering = const config.awaitNewSteering
        , loopCommitSteering = acknowledgeBatch
        -- Late inbox entries are picked up by the next activation.
        , loopCloseSteering = pure []
        , loopInterrupt = pure ()
        , loopCancel = cancel
        , loopSubagents = config.subagents
        } initialResponseId []
    -- agent-core returns synchronous exceptions as LoopUnexpected. Preserve
    -- the original typed provider/delivery exception for host recovery policy.
    rethrowFailure failureRef
    preempted <- readIORef preemptedRef
    case execution.executionResult of
        Right _ -> NativeLoopFinished <$> requireResponse
        Left (Agent.LoopCancelled _) | preempted -> pure NativeLoopPreempted
        Left (Agent.LoopMaxTurns _) -> do
            batch <- readIORef batchRef
            checkpoint <- readIORef checkpointRef
            pure $ NativeLoopReachedLimit NativeLoopLimit
                { previousResponseId = Agent.backendContinuationToken config.continuationNamespace checkpoint
                , pendingItems = completedToolItems execution.executionPendingInputs <> batchItems batch
                    <> native.auxiliaryInputItems execution.executionPendingInputs
                , acknowledgePendingInput = acknowledgeBatch (batchSize batch)
                }
        Left err -> fail ("NativeLoop: loop failed: " <> show err)

-- | 'awaitNewSteering' for an inbox kept in the database, which offers no
-- notification: check twice a second while a model request is in flight. A
-- failed check is reported and counts as no new input; it must not fail the
-- request it races.
pollForNewSteering :: (Exception.SomeException -> IO ()) -> IO Bool -> IO ()
pollForNewSteering reportFailure hasNewSteering = do
    threadDelay 500000
    ready <- Exception.catchAny hasNewSteering \exception -> do
        reportFailure exception
        pure False
    unless ready (pollForNewSteering reportFailure hasNewSteering)

-- | 'awaitNewSteering' for a host without live steering: never returns.
noLiveSteering :: IO ()
noLiveSteering = forever (threadDelay 3600000000)

rememberFailure :: IORef (Maybe Exception.SomeException) -> IO value -> IO value
rememberFailure ref action = Exception.catchAny action \exception -> do
    atomicModifyIORef' ref \previous ->
        let firstFailure = case previous of
                Nothing -> Just exception
                Just _ -> previous
        in (firstFailure, ())
    Exception.throwIO exception

rethrowFailure :: IORef (Maybe Exception.SomeException) -> IO ()
rethrowFailure ref = readIORef ref >>= mapM_ Exception.throwIO
