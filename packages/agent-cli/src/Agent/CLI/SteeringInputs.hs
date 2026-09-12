-- | A bounded queue of steering inputs retained while a turn is active.
module Agent.CLI.SteeringInputs
    ( SteeringInputs
    , awaitSteeringInput
    , clearSteeringInputs
    , commitSteeringInputs
    , dismissBackgroundCompletion
    , enqueueBackgroundCompletion
    , enqueueSteeringInputs
    , hasSteeringInputWake
    , hasBackgroundCompletions
    , newSteeringInputs
    , prepareBackgroundCompletion
    , readSteeringInputs
    , steeringInputByteLimit
    , steeringInputCountLimit
    , suppressUserSteeringWake
    ) where

import Agent.CLI.InputBudget
    ( logicalTurnInputBytes
    , saturatingAdd
    )
import Agent.Loop (TurnInput)
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
    , steeringBytes :: !Int
    , steeringEpoch :: !Word
    }

newtype SteeringInputs = SteeringInputs (TVar SteeringState)

newSteeringInputs :: IO SteeringInputs
newSteeringInputs =
    SteeringInputs <$> newTVarIO (SteeringState Seq.empty 0 0)

enqueueSteeringInputs
    :: SteeringInputs
    -> [TurnInput]
    -> IO (Either Text ())
enqueueSteeringInputs (SteeringInputs ref) inputs =
    atomically do
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
        if nextCount > steeringInputCountLimit
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
-- for the same managed task while the notice is still pending.
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
                    state.steeringQueue
            then pure (Right False)
            else do
                let bytes = logicalTurnInputBytes input
                    nextCount = Seq.length state.steeringQueue + 1
                    nextBytes = state.steeringBytes `saturatingAdd` bytes
                if nextCount > steeringInputCountLimit
                        || nextBytes > steeringInputByteLimit
                    then
                        pure $ Left
                            "Steering queue is full; a background completion notice could not be queued."
                    else do
                        writeTVar ref state
                            { steeringQueue =
                                state.steeringQueue
                                    Seq.|> SteeringEntry
                                        input
                                        bytes
                                        (Just key)
                                        True
                            , steeringBytes = nextBytes
                            }
                        pure (Right True)

readSteeringInputs :: SteeringInputs -> IO [TurnInput]
readSteeringInputs (SteeringInputs ref) = do
    state <- readTVarIO ref
    pure [entry.steeringInput | entry <- toList state.steeringQueue]

hasBackgroundCompletions :: SteeringInputs -> IO Bool
hasBackgroundCompletions (SteeringInputs ref) = do
    state <- readTVarIO ref
    pure $
        any (maybe False (const True) . (.steeringBackgroundKey))
            state.steeringQueue

hasSteeringInputWake :: SteeringInputs -> IO Bool
hasSteeringInputWake (SteeringInputs ref) = do
    state <- readTVarIO ref
    pure $ any (.steeringWake) state.steeringQueue

-- | Consume pending idle-wake edges without removing their inputs. Every
-- accepted input has an edge: guidance arriving after the active loop's final
-- read must start a follow-up turn rather than remain stranded at the prompt.
-- Keeping inputs queued preserves the loop's commit-on-provider-success semantics;
-- consuming the edge prevents a failed synthetic turn from hot-looping.
awaitSteeringInput :: SteeringInputs -> STM ()
awaitSteeringInput (SteeringInputs ref) = do
    state <- readTVar ref
    check $ any (.steeringWake) state.steeringQueue
    writeTVar ref state
        { steeringQueue =
            fmap
                (\entry -> entry { steeringWake = False })
                state.steeringQueue
        }

dismissBackgroundCompletion :: SteeringInputs -> Text -> IO ()
dismissBackgroundCompletion (SteeringInputs ref) key =
    atomically $ modifyTVar' ref \state ->
        let kept =
                Seq.filter
                    ((/= Just key) . (.steeringBackgroundKey))
                    state.steeringQueue
            keptBytes =
                foldr
                    (\entry total ->
                        entry.steeringBytes `saturatingAdd` total)
                    0
                    kept
        in state
            { steeringQueue = kept
            , steeringBytes = keptBytes
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
        in state
            { steeringQueue = remaining
            , steeringBytes = max 0 (state.steeringBytes - removedBytes)
            }

clearSteeringInputs :: SteeringInputs -> IO ()
clearSteeringInputs (SteeringInputs ref) =
    atomically $ modifyTVar' ref \state ->
        SteeringState Seq.empty 0 (state.steeringEpoch + 1)
