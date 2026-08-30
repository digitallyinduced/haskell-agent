-- | A bounded queue of steering inputs retained while a turn is active.
module Agent.CLI.SteeringInputs
    ( SteeringInputs
    , clearSteeringInputs
    , commitSteeringInputs
    , enqueueSteeringInputs
    , newSteeringInputs
    , readSteeringInputs
    , steeringInputByteLimit
    , steeringInputCountLimit
    ) where

import Agent.CLI.InputBudget
    ( logicalTurnInputBytes
    , saturatingAdd
    )
import Agent.Loop (TurnInput)
import Data.Foldable (toList)
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    )
import qualified Data.Sequence as Seq
import Data.Text (Text)

steeringInputCountLimit :: Int
steeringInputCountLimit = 128

steeringInputByteLimit :: Int
steeringInputByteLimit = 64 * 1024 * 1024

data SteeringState = SteeringState
    { steeringQueue :: !(Seq.Seq (TurnInput, Int))
    , steeringBytes :: !Int
    }

newtype SteeringInputs = SteeringInputs (IORef SteeringState)

newSteeringInputs :: IO SteeringInputs
newSteeringInputs =
    SteeringInputs <$> newIORef (SteeringState Seq.empty 0)

enqueueSteeringInputs
    :: SteeringInputs
    -> [TurnInput]
    -> IO (Either Text ())
enqueueSteeringInputs (SteeringInputs ref) inputs =
    atomicModifyIORef' ref \state ->
        let measured = fmap (\input -> (input, logicalTurnInputBytes input)) inputs
            addedCount = length measured
            addedBytes =
                foldr (\(_, bytes) total -> bytes `saturatingAdd` total) 0 measured
            nextCount = Seq.length state.steeringQueue + addedCount
            nextBytes = state.steeringBytes `saturatingAdd` addedBytes
        in if nextCount > steeringInputCountLimit
                || nextBytes > steeringInputByteLimit
            then
                ( state
                , Left
                    "Steering queue is full; wait for the active turn to consume guidance."
                )
            else
                ( SteeringState
                    { steeringQueue =
                        state.steeringQueue Seq.>< Seq.fromList measured
                    , steeringBytes = nextBytes
                    }
                , Right ()
                )

readSteeringInputs :: SteeringInputs -> IO [TurnInput]
readSteeringInputs (SteeringInputs ref) = do
    state <- readIORef ref
    pure [input | (input, _) <- toList state.steeringQueue]

commitSteeringInputs :: SteeringInputs -> Int -> IO ()
commitSteeringInputs (SteeringInputs ref) count =
    atomicModifyIORef' ref \state ->
        let removed = Seq.take (max 0 count) state.steeringQueue
            remaining = Seq.drop (max 0 count) state.steeringQueue
            removedBytes = foldr
                (\(_, bytes) total -> bytes `saturatingAdd` total)
                0
                removed
        in ( SteeringState
                { steeringQueue = remaining
                , steeringBytes = max 0 (state.steeringBytes - removedBytes)
                }
           , ()
           )

clearSteeringInputs :: SteeringInputs -> IO ()
clearSteeringInputs (SteeringInputs ref) =
    atomicModifyIORef' ref \_ ->
        (SteeringState Seq.empty 0, ())
