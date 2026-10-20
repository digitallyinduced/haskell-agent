-- | Session input delivery coordinated by STM. Provider I/O and durable delivery
-- remain host responsibilities; queue snapshots are acknowledged separately.
-- Steering queues retain their submitted prefix; pending queues lease a batch
-- until a submission succeeds, fails, or is cancelled.
module Agent.Loop.InputQueue
    ( InputQueue
    , PendingInputs
    , PendingNoticeKind(..)
    , newPendingInputs
    , clearPendingInputs
    , enqueuePendingInput
    , enqueuePendingNotice
    , withPendingInputBatch
    , pendingInputByteLimit
    , pendingInputCountLimit
    , awaitSteeringInput
    , awaitSteeringInputReady
    , awaitPendingInputReady
    , awaitUserSteering
    , awaitUserSteeringAfter
    , clearSteeringInputs
    , closeSteeringInputs
    , commitSteeringInputs
    , commitSteeringInputsSTM
    , dismissBackgroundCompletion
    , enqueueBackgroundCompletion
    , enqueueSteeringInputs
    , enqueueSteeringInputsSTM
    , hasSteeringInputWake
    , hasBackgroundCompletions
    , newInputQueue
    , prepareBackgroundCompletion
    , readSteeringInputs
    , readSteeringTurn
    , reserveSteeringInputs
    , steeringInputByteLimit
    , steeringInputCountLimit
    , suppressUserSteeringWake
    ) where

import Agent.Loop.Input (TurnInput(..))
import Agent.Loop.InputBudget
    ( logicalTurnInputBytes
    , saturatingAdd
    )
import Agent.Loop.InputQueue.Model
    ( InputQueueState(..), InputEntry(..), PendingNoticeKind(..)
    , pendingInputByteLimit, pendingInputCountLimit
    , steeringInputByteLimit, steeringInputCountLimit
    )
import qualified Agent.Loop.InputQueue.Model as Model
import Control.Exception (evaluate)
import Control.Exception.Safe (generalBracket)
import Control.Monad.Catch (ExitCase(..))
import Data.Foldable (toList)
import Control.Concurrent.STM
    ( STM
    , TVar
    , atomically
    , check
    , modifyTVar'
    , newTVarIO
    , readTVar
    , readTVarIO
    , writeTVar
    )
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text

newtype InputQueue = InputQueue (TVar InputQueueState)

-- | Pending submissions share the queue implementation, but own a different
-- acknowledgement boundary. Keep them distinct so steering cannot consume or
-- dismiss entries while a submission batch owns their retained budget.
newtype PendingInputs = PendingInputs InputQueue

-- | A session queue for active steering and background completion notices.
newInputQueue :: IO InputQueue
newInputQueue = InputQueue <$> newTVarIO Model.emptySteeringState

-- | A session queue for messages and coalesced notices prepended to submissions.
-- Its retained budget includes the batch currently owned by a backend call.
newPendingInputs :: IO PendingInputs
newPendingInputs = PendingInputs . InputQueue <$> newTVarIO Model.emptyPendingState

clearPendingInputs :: PendingInputs -> IO ()
clearPendingInputs (PendingInputs inputs) = clearSteeringInputs inputs

-- | Observe root messages queued after the current leased batch.
awaitPendingInputReady :: PendingInputs -> STM ()
awaitPendingInputReady (PendingInputs inputs) = awaitSteeringInputReady inputs

enqueuePendingInput :: PendingInputs -> TurnInput -> IO (Either Text ())
enqueuePendingInput (PendingInputs (InputQueue ref)) input =
    atomically $ applyQueueTransition ref (Model.enqueueInput input)

enqueuePendingNotice
    :: PendingInputs -> PendingNoticeKind -> TurnInput -> IO (Either Text ())
enqueuePendingNotice (PendingInputs (InputQueue ref)) kind input =
    atomically $ applyQueueTransition ref (Model.enqueueNotice kind input)

-- | Serialize one pending-batch lifecycle without holding a transaction across
-- host I/O. Bracket acquisition/release protects ownership against cancellation;
-- a reset invalidates the batch but never permits overlapping submissions.
withPendingInputBatch
    :: PendingInputs
    -> ([TurnInput] -> IO (Either error result))
    -> IO (Either error result)
withPendingInputBatch (PendingInputs (InputQueue ref)) submit =
    fst <$> generalBracket acquire release
        (\batch -> submit (Model.batchInputs batch) >>= evaluate)
  where
    acquire = atomically do
        state <- readTVar ref
        check (not state.queueBatchActive)
        let (next, batch) = Model.drain state
        writeTVar ref next { queueBatchActive = True }
        pure batch
    release batch outcome = atomically do
        state <- readTVar ref
        let finish = case outcome of
                ExitCaseSuccess (Right _) -> Model.commit batch
                _ -> Model.requeue batch
            (next, ()) = finish state
        writeTVar ref next { queueBatchActive = False }

applyQueueTransition
    :: TVar InputQueueState
    -> (InputQueueState -> (InputQueueState, result))
    -> STM result
applyQueueTransition ref transition = do
    state <- readTVar ref
    let (next, result) = transition state
    writeTVar ref next
    pure result

enqueueSteeringInputs
    :: InputQueue
    -> [TurnInput]
    -> IO (Either Text ())
enqueueSteeringInputs inputs = atomically . enqueueSteeringInputsSTM inputs

-- | Lets a host record its acceptance of guidance in the same transaction.
enqueueSteeringInputsSTM
    :: InputQueue
    -> [TurnInput]
    -> STM (Either Text ())
enqueueSteeringInputsSTM (InputQueue ref) inputs = do
    state <- readTVar ref
    let measured = Seq.fromList
            [ InputEntry input (logicalTurnInputBytes input) Nothing Nothing True
            | input <- inputs
            ]
    if state.queueClosed
        then pure $ Left
            "The turn has already answered; submit guidance as a new turn."
        else case Model.appendEntries measured state of
            Nothing -> pure $ Left
                "Steering queue is full; wait for the active turn to consume guidance."
            Just next -> writeTVar ref next >> pure (Right ())

-- | Queue one generated completion notice, suppressing duplicate publication
-- for the same managed task while the notice is still pending. Queue
-- saturation defers delivery rather than rejecting a completed task's result.
-- Individual notices must fit the byte budget; managed shell notices already
-- bound their output well below this limit.
enqueueBackgroundCompletion
    :: InputQueue
    -> Text
    -> TurnInput
    -> IO (Either Text Bool)
enqueueBackgroundCompletion = enqueueBackgroundCompletionForEpoch Nothing

-- | Capture the owning conversation, so a late child completion cannot wake
-- or inject input into a different conversation after a session reset.
prepareBackgroundCompletion
    :: InputQueue
    -> IO (Text -> TurnInput -> IO (Either Text Bool))
prepareBackgroundCompletion inputs@(InputQueue ref) = do
    state <- readTVarIO ref
    pure (enqueueBackgroundCompletionForEpoch (Just state.queueEpoch) inputs)

enqueueBackgroundCompletionForEpoch
    :: Maybe Word
    -> InputQueue
    -> Text
    -> TurnInput
    -> IO (Either Text Bool)
enqueueBackgroundCompletionForEpoch epoch (InputQueue ref) key input =
    atomically do
        state <- readTVar ref
        if maybe False (/= state.queueEpoch) epoch
                || any ((== Just key) . (.entryBackgroundKey))
                    (state.queueEntries Seq.>< state.queueDeferredCompletions)
            then pure (Right False)
            else do
                let bytes = logicalTurnInputBytes input
                if bytes > state.queueByteLimit
                    then
                        pure $ Left
                            "Background completion notice exceeds the steering input byte limit."
                    else do
                        writeTVar ref $ Model.promoteDeferredCompletions state
                            { queueDeferredCompletions =
                                state.queueDeferredCompletions
                                    Seq.|> InputEntry
                                        input
                                        bytes
                                        Nothing
                                        (Just key)
                                        True
                            }
                        pure (Right True)

readSteeringInputs :: InputQueue -> IO [TurnInput]
readSteeringInputs (InputQueue ref) = do
    state <- readTVarIO ref
    pure [entry.entryInput | entry <- toList state.queueEntries]

-- | Snapshot inputs for submission and protect their queue positions against
-- background-notice dismissal. Reading for observation does not reserve.
reserveSteeringInputs :: InputQueue -> IO [TurnInput]
reserveSteeringInputs (InputQueue ref) = atomically do
    state <- reserveInputQueueState ref
    pure [entry.entryInput | entry <- toList state.queueEntries]

reserveInputQueueState :: TVar InputQueueState -> STM InputQueueState
reserveInputQueueState ref = do
    state <- readTVar ref
    writeTVar ref state
        { queueReservedCount = Seq.length state.queueEntries }
    pure state

-- | The loop's 'Agent.Loop.loopCloseSteering' for a host that answers all
-- accepted guidance within one turn: hand back what arrived after the last
-- read, or refuse all further guidance when nothing is pending.
closeSteeringInputs :: InputQueue -> IO [TurnInput]
closeSteeringInputs (InputQueue ref) =
    atomically do
        state <- reserveInputQueueState ref
        if Seq.null state.queueEntries
            then do
                writeTVar ref state { queueClosed = True }
                pure []
            else
                pure [entry.entryInput | entry <- toList state.queueEntries]

-- | Snapshot an idle wake's display text and pending inputs together. Inputs
-- remain queued until the provider acknowledges them; the text is metadata
-- for the enclosing turn, not another provider input. Background notices
-- must not acquire the identity of a user submission.
readSteeringTurn :: InputQueue -> IO (Text, [TurnInput])
readSteeringTurn (InputQueue ref) = atomically do
    state <- reserveInputQueueState ref
    let entries = toList state.queueEntries
        userText :: InputEntry -> [Text]
        userText entry =
            case (entry.entryBackgroundKey, entry.entryInput) of
                (Nothing, UserMessage text) -> [text]
                (Nothing, UserMessageWithAttachments text _) -> [text]
                _ -> []
    pure
        ( Text.intercalate "\n\n" (concatMap userText entries)
        , map (.entryInput) entries
        )

hasBackgroundCompletions :: InputQueue -> IO Bool
hasBackgroundCompletions (InputQueue ref) = do
    state <- readTVarIO ref
    pure $
        not (Seq.null state.queueDeferredCompletions)
        || any (maybe False (const True) . (.entryBackgroundKey))
            state.queueEntries

hasSteeringInputWake :: InputQueue -> IO Bool
hasSteeringInputWake (InputQueue ref) = do
    state <- readTVarIO ref
    pure $ any (.entryWake)
        (state.queueEntries Seq.>< state.queueDeferredCompletions)

-- | Observe an idle wake without claiming it. Competing keyboard/inbox waits
-- may win after this transaction succeeds, so the winning owner must consume
-- the edge separately rather than losing it in a cancelled racing action.
awaitSteeringInputReady :: InputQueue -> STM ()
awaitSteeringInputReady (InputQueue ref) = do
    state <- readTVar ref
    check $ any (.entryWake)
        (state.queueEntries Seq.>< state.queueDeferredCompletions)

-- | Consume pending idle-wake edges without removing their inputs. Every
-- accepted input has an edge: guidance arriving after the active loop's final
-- read must start a follow-up turn rather than remain stranded at the prompt.
-- Keeping inputs queued preserves the loop's commit-on-provider-success semantics;
-- consuming the edge prevents a failed synthetic turn from hot-looping.
awaitSteeringInput :: InputQueue -> STM ()
awaitSteeringInput (InputQueue ref) = do
    state <- readTVar ref
    check $ any (.entryWake)
        (state.queueEntries Seq.>< state.queueDeferredCompletions)
    writeTVar ref state
        { queueEntries =
            fmap
                (\entry -> entry { entryWake = False })
                state.queueEntries
        , queueDeferredCompletions =
            fmap
                (\entry -> entry { entryWake = False })
                state.queueDeferredCompletions
        }

-- | Wake passive tool waits without consuming guidance or its idle-wake edge.
-- Every concurrent wait must observe the same pending input. Only provider
-- acknowledgement removes it; background completions are not user guidance.
awaitUserSteering :: InputQueue -> STM ()
awaitUserSteering inputs = awaitUserSteeringAfter inputs 0

-- | Observe new user guidance beyond the unacknowledged prefix already
-- submitted to the provider. Background completion notices never interrupt
-- generation, and observing guidance does not consume it or its idle wake.
awaitUserSteeringAfter :: InputQueue -> Int -> STM ()
awaitUserSteeringAfter (InputQueue ref) submittedCount = do
    state <- readTVar ref
    check $ any (\entry -> entry.entryBackgroundKey == Nothing)
        (Seq.drop (max 0 submittedCount) state.queueEntries)

-- | Remove notices not yet handed to a provider. Reserved notices must remain
-- in place: both acknowledgement counts and steering waits refer to that prefix.
dismissBackgroundCompletion :: InputQueue -> Text -> IO ()
dismissBackgroundCompletion (InputQueue ref) key =
    atomically $ modifyTVar' ref \state ->
        let (reserved, unreserved) =
                Seq.splitAt state.queueReservedCount state.queueEntries
            kept =
                reserved Seq.>< Seq.filter
                    ((/= Just key) . (.entryBackgroundKey))
                    unreserved
            keptBytes =
                foldr
                    (\entry total ->
                        entry.entryBytes `saturatingAdd` total)
                    0
                    kept
        in Model.promoteDeferredCompletions state
            { queueEntries = kept
            , queueRetainedCount = state.queueRetainedCount - (Seq.length state.queueEntries - Seq.length kept)
            , queueRetainedBytes = keptBytes
            , queueDeferredCompletions = Seq.filter
                ((/= Just key) . (.entryBackgroundKey))
                state.queueDeferredCompletions
            }

-- | Cancelling a turn must not immediately restart it with earlier guidance.
-- Retain that guidance for the next explicit turn, without changing managed
-- background completion wake behavior.
suppressUserSteeringWake :: InputQueue -> IO ()
suppressUserSteeringWake (InputQueue ref) =
    atomically $ modifyTVar' ref \state ->
        state
            { queueEntries = fmap suppress state.queueEntries }
  where
    suppress entry = case entry.entryBackgroundKey of
        Nothing -> entry { entryWake = False }
        Just _ -> entry

commitSteeringInputs :: InputQueue -> Int -> IO ()
commitSteeringInputs inputs = atomically . commitSteeringInputsSTM inputs

-- | Commit guidance together with its host's acceptance records.
commitSteeringInputsSTM :: InputQueue -> Int -> STM ()
commitSteeringInputsSTM (InputQueue ref) count =
    modifyTVar' ref \state ->
        let removed = Seq.take (max 0 count) state.queueEntries
            remaining = Seq.drop (max 0 count) state.queueEntries
            removedBytes = foldr
                (\entry total ->
                    entry.entryBytes `saturatingAdd` total)
                0
                removed
        in Model.promoteDeferredCompletions state
            { queueEntries = remaining
            , queueRetainedCount = max 0 (state.queueRetainedCount - Seq.length removed)
            , queueRetainedBytes = max 0 (state.queueRetainedBytes - removedBytes)
            , queueReservedCount =
                max 0 (state.queueReservedCount - max 0 count)
            }

clearSteeringInputs :: InputQueue -> IO ()
clearSteeringInputs (InputQueue ref) =
    atomically $ applyQueueTransition ref Model.clearPendingState
