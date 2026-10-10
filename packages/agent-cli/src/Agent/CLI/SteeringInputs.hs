-- | Bounded steering delivery with retained background completion overflow.
module Agent.CLI.SteeringInputs
    ( SteeringInputs
    , awaitSteeringInput
    , awaitSteeringInputReady
    , awaitUserSteering
    , awaitUserSteeringAfter
    , clearSteeringInputs
    , closeSteeringInputs
    , commitSteeringInputs
    , dismissBackgroundCompletion
    , enqueueBackgroundCompletion
    , enqueueSteeringInputs
    , enqueueSteeringInputsSTM
    , hasSteeringInputWake
    , hasBackgroundCompletions
    , newSteeringInputs
    , prepareBackgroundCompletion
    , readSteeringInputs
    , readSteeringTurn
    , reserveSteeringInputs
    , steeringInputByteLimit
    , steeringInputCountLimit
    , suppressUserSteeringWake
    ) where

import Agent.CLI.InputBudget
    ( logicalTurnInputBytes
    , saturatingAdd
    )
import Agent.Loop (TurnInput(..))
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

steeringInputCountLimit :: Int
steeringInputCountLimit = 128

steeringInputByteLimit :: Int
steeringInputByteLimit = 64 * 1024 * 1024

data SteeringEntry = SteeringEntry
    { steeringInput :: !TurnInput
    , steeringBytes :: !Int
    , steeringBackgroundKey :: !(Maybe Text)
    , steeringWake :: !Bool
    }

data SteeringState = SteeringState
    { steeringQueue :: !(Seq.Seq SteeringEntry)
    -- Completion callbacks cannot block while holding their delivery gate.
    -- Retain overflow separately until acknowledgement frees delivery space.
    , deferredCompletions :: !(Seq.Seq SteeringEntry)
    , steeringBytes :: !Int
    , steeringEpoch :: !Word
    -- | Set by 'closeSteeringInputs' once the loop has finished answering.
    , steeringClosed :: !Bool
    -- | A submitted snapshot's positions remain stable until acknowledgement.
    , steeringReservedCount :: !Int
    }

newtype SteeringInputs = SteeringInputs (TVar SteeringState)

newSteeringInputs :: IO SteeringInputs
newSteeringInputs =
    SteeringInputs <$> newTVarIO (SteeringState Seq.empty Seq.empty 0 0 False 0)

enqueueSteeringInputs
    :: SteeringInputs
    -> [TurnInput]
    -> IO (Either Text ())
enqueueSteeringInputs inputs = atomically . enqueueSteeringInputsSTM inputs

-- | Lets a host record its acceptance of guidance in the same transaction.
enqueueSteeringInputsSTM
    :: SteeringInputs
    -> [TurnInput]
    -> STM (Either Text ())
enqueueSteeringInputsSTM (SteeringInputs ref) inputs = do
    state <- readTVar ref
    let measured =
            [ SteeringEntry
                input
                (logicalTurnInputBytes input)
                Nothing
                True
            | input <- inputs
            ]
        addedCount = length measured
        addedBytes =
            foldr
                (\entry total ->
                    entry.steeringBytes `saturatingAdd` total)
                0
                measured
        nextCount = Seq.length state.steeringQueue + addedCount
        nextBytes = state.steeringBytes `saturatingAdd` addedBytes
    if state.steeringClosed
        then
            pure $ Left
                "The turn has already answered; submit guidance as a new turn."
        else if nextCount > steeringInputCountLimit
                || nextBytes > steeringInputByteLimit
            then
                pure $ Left
                    "Steering queue is full; wait for the active turn to consume guidance."
            else do
                writeTVar ref state
                    { steeringQueue =
                        state.steeringQueue Seq.>< Seq.fromList measured
                    , steeringBytes = nextBytes
                    }
                pure (Right ())

-- | Queue one generated completion notice, suppressing duplicate publication
-- for the same managed task while the notice is still pending. Queue
-- saturation defers delivery rather than rejecting a completed task's result.
-- Individual notices must fit the byte budget; managed shell notices already
-- bound their output well below this limit.
enqueueBackgroundCompletion
    :: SteeringInputs
    -> Text
    -> TurnInput
    -> IO (Either Text Bool)
enqueueBackgroundCompletion = enqueueBackgroundCompletionForEpoch Nothing

-- | Capture the owning conversation, so a late child completion cannot wake
-- or inject input into a different conversation after a session reset.
prepareBackgroundCompletion
    :: SteeringInputs
    -> IO (Text -> TurnInput -> IO (Either Text Bool))
prepareBackgroundCompletion inputs@(SteeringInputs ref) = do
    state <- readTVarIO ref
    pure (enqueueBackgroundCompletionForEpoch (Just state.steeringEpoch) inputs)

enqueueBackgroundCompletionForEpoch
    :: Maybe Word
    -> SteeringInputs
    -> Text
    -> TurnInput
    -> IO (Either Text Bool)
enqueueBackgroundCompletionForEpoch epoch (SteeringInputs ref) key input =
    atomically do
        state <- readTVar ref
        if maybe False (/= state.steeringEpoch) epoch
                || any ((== Just key) . (.steeringBackgroundKey))
                    (state.steeringQueue Seq.>< state.deferredCompletions)
            then pure (Right False)
            else do
                let bytes = logicalTurnInputBytes input
                if bytes > steeringInputByteLimit
                    then
                        pure $ Left
                            "Background completion notice exceeds the steering input byte limit."
                    else do
                        writeTVar ref $ promoteDeferredCompletions state
                            { deferredCompletions =
                                state.deferredCompletions
                                    Seq.|> SteeringEntry
                                        input
                                        bytes
                                        (Just key)
                                        True
                            }
                        pure (Right True)

-- | Fill the bounded provider-delivery queue without changing its existing
-- prefix: a provider acknowledgement still commits precisely its snapshot.
promoteDeferredCompletions :: SteeringState -> SteeringState
promoteDeferredCompletions state =
    case Seq.viewl state.deferredCompletions of
        Seq.EmptyL -> state
        entry Seq.:< remaining
            | Seq.length state.steeringQueue < steeringInputCountLimit
            , let nextBytes = state.steeringBytes `saturatingAdd` entry.steeringBytes
            , nextBytes <= steeringInputByteLimit ->
                promoteDeferredCompletions state
                    { steeringQueue = state.steeringQueue Seq.|> entry { steeringWake = True }
                    , deferredCompletions = remaining
                    , steeringBytes = nextBytes
                    }
            | otherwise -> state

readSteeringInputs :: SteeringInputs -> IO [TurnInput]
readSteeringInputs (SteeringInputs ref) = do
    state <- readTVarIO ref
    pure [entry.steeringInput | entry <- toList state.steeringQueue]

-- | Snapshot inputs for submission and protect their queue positions against
-- background-notice dismissal. Reading for observation does not reserve.
reserveSteeringInputs :: SteeringInputs -> IO [TurnInput]
reserveSteeringInputs (SteeringInputs ref) = atomically do
    state <- reserveSteeringState ref
    pure [entry.steeringInput | entry <- toList state.steeringQueue]

reserveSteeringState :: TVar SteeringState -> STM SteeringState
reserveSteeringState ref = do
    state <- readTVar ref
    writeTVar ref state
        { steeringReservedCount = Seq.length state.steeringQueue }
    pure state

-- | The loop's 'Agent.Loop.loopCloseSteering' for a host that answers all
-- accepted guidance within one turn: hand back what arrived after the last
-- read, or refuse all further guidance when nothing is pending.
closeSteeringInputs :: SteeringInputs -> IO [TurnInput]
closeSteeringInputs (SteeringInputs ref) =
    atomically do
        state <- reserveSteeringState ref
        if Seq.null state.steeringQueue
            then do
                writeTVar ref state { steeringClosed = True }
                pure []
            else
                pure [entry.steeringInput | entry <- toList state.steeringQueue]

-- | Snapshot an idle wake's display text and pending inputs together. Inputs
-- remain queued until the provider acknowledges them; the text is metadata
-- for the enclosing turn, not another provider input. Background notices
-- must not acquire the identity of a user submission.
readSteeringTurn :: SteeringInputs -> IO (Text, [TurnInput])
readSteeringTurn (SteeringInputs ref) = atomically do
    state <- reserveSteeringState ref
    let entries = toList state.steeringQueue
        userText entry =
            case (entry.steeringBackgroundKey, entry.steeringInput) of
                (Nothing, UserMessage text) -> [text]
                (Nothing, UserMessageWithAttachments text _) -> [text]
                _ -> []
    pure
        ( Text.intercalate "\n\n" (concatMap userText entries)
        , map (.steeringInput) entries
        )

hasBackgroundCompletions :: SteeringInputs -> IO Bool
hasBackgroundCompletions (SteeringInputs ref) = do
    state <- readTVarIO ref
    pure $
        not (Seq.null state.deferredCompletions)
        || any (maybe False (const True) . (.steeringBackgroundKey))
            state.steeringQueue

hasSteeringInputWake :: SteeringInputs -> IO Bool
hasSteeringInputWake (SteeringInputs ref) = do
    state <- readTVarIO ref
    pure $ any (.steeringWake)
        (state.steeringQueue Seq.>< state.deferredCompletions)

-- | Observe an idle wake without claiming it. Competing keyboard/inbox waits
-- may win after this transaction succeeds, so the winning owner must consume
-- the edge separately rather than losing it in a cancelled racing action.
awaitSteeringInputReady :: SteeringInputs -> STM ()
awaitSteeringInputReady (SteeringInputs ref) = do
    state <- readTVar ref
    check $ any (.steeringWake)
        (state.steeringQueue Seq.>< state.deferredCompletions)

-- | Consume pending idle-wake edges without removing their inputs. Every
-- accepted input has an edge: guidance arriving after the active loop's final
-- read must start a follow-up turn rather than remain stranded at the prompt.
-- Keeping inputs queued preserves the loop's commit-on-provider-success semantics;
-- consuming the edge prevents a failed synthetic turn from hot-looping.
awaitSteeringInput :: SteeringInputs -> STM ()
awaitSteeringInput (SteeringInputs ref) = do
    state <- readTVar ref
    check $ any (.steeringWake)
        (state.steeringQueue Seq.>< state.deferredCompletions)
    writeTVar ref state
        { steeringQueue =
            fmap
                (\entry -> entry { steeringWake = False })
                state.steeringQueue
        , deferredCompletions =
            fmap
                (\entry -> entry { steeringWake = False })
                state.deferredCompletions
        }

-- | Wake passive tool waits without consuming guidance or its idle-wake edge.
-- Every concurrent wait must observe the same pending input. Only provider
-- acknowledgement removes it; background completions are not user guidance.
awaitUserSteering :: SteeringInputs -> STM ()
awaitUserSteering inputs = awaitUserSteeringAfter inputs 0

-- | Observe new user guidance beyond the unacknowledged prefix already
-- submitted to the provider. Background completion notices never interrupt
-- generation, and observing guidance does not consume it or its idle wake.
awaitUserSteeringAfter :: SteeringInputs -> Int -> STM ()
awaitUserSteeringAfter (SteeringInputs ref) submittedCount = do
    state <- readTVar ref
    check $ any (\entry -> entry.steeringBackgroundKey == Nothing)
        (Seq.drop (max 0 submittedCount) state.steeringQueue)

-- | Remove notices not yet handed to a provider. Reserved notices must remain
-- in place: both acknowledgement counts and steering waits refer to that prefix.
dismissBackgroundCompletion :: SteeringInputs -> Text -> IO ()
dismissBackgroundCompletion (SteeringInputs ref) key =
    atomically $ modifyTVar' ref \state ->
        let (reserved, unreserved) =
                Seq.splitAt state.steeringReservedCount state.steeringQueue
            kept =
                reserved Seq.>< Seq.filter
                    ((/= Just key) . (.steeringBackgroundKey))
                    unreserved
            keptBytes =
                foldr
                    (\entry total ->
                        entry.steeringBytes `saturatingAdd` total)
                    0
                    kept
        in promoteDeferredCompletions state
            { steeringQueue = kept
            , steeringBytes = keptBytes
            , deferredCompletions = Seq.filter
                ((/= Just key) . (.steeringBackgroundKey))
                state.deferredCompletions
            }

-- | Cancelling a turn must not immediately restart it with earlier guidance.
-- Retain that guidance for the next explicit turn, without changing managed
-- background completion wake behavior.
suppressUserSteeringWake :: SteeringInputs -> IO ()
suppressUserSteeringWake (SteeringInputs ref) =
    atomically $ modifyTVar' ref \state ->
        state
            { steeringQueue = fmap suppress state.steeringQueue }
  where
    suppress entry = case entry.steeringBackgroundKey of
        Nothing -> entry { steeringWake = False }
        Just _ -> entry

commitSteeringInputs :: SteeringInputs -> Int -> IO ()
commitSteeringInputs (SteeringInputs ref) count =
    atomically $ modifyTVar' ref \state ->
        let removed = Seq.take (max 0 count) state.steeringQueue
            remaining = Seq.drop (max 0 count) state.steeringQueue
            removedBytes = foldr
                (\entry total ->
                    entry.steeringBytes `saturatingAdd` total)
                0
                removed
        in promoteDeferredCompletions state
            { steeringQueue = remaining
            , steeringBytes = max 0 (state.steeringBytes - removedBytes)
            , steeringReservedCount =
                max 0 (state.steeringReservedCount - max 0 count)
            }

clearSteeringInputs :: SteeringInputs -> IO ()
clearSteeringInputs (SteeringInputs ref) =
    atomically $ modifyTVar' ref \state ->
        SteeringState Seq.empty Seq.empty 0 (state.steeringEpoch + 1) False 0
