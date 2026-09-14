-- | Execute a prepared turn with the existing provider loop and preserve its
-- exceptional rollback boundary. This is not a session scheduler: the caller
-- still owns serialization, cancellation registration, and resource lifetimes.
module Agent.Runtime.TurnExecution
    ( PreparedExecution(..)
    , ExecutedTurn(..)
    , ExceptionalTurn(..)
    , executePreparedTurn
    ) where

import Agent.Loop (LoopConfig, LoopExecution, runLoopInputsDetailed)
import Agent.Runtime.Compaction (AutomaticCompactionBoundary)
import Agent.Runtime.TurnState
    ( ConversationOutcome(ConversationInterrupted)
    , ConversationPatch
    , PreparedTurn(..)
    , finishConversation
    , rebasePreparedTurn
    )
import Control.Exception.Safe (onException)
import Data.Text (Text)

data PreparedExecution = PreparedExecution
    { executionConfig :: !LoopConfig
    , executionPreviousResponseId :: !(Maybe Text)
    , executionPreparedTurn :: !PreparedTurn
    }

data ExecutedTurn = ExecutedTurn
    { executedLoop :: LoopExecution
    , executedCompaction :: Maybe AutomaticCompactionBoundary
    }

-- | State to commit when the loop throws rather than returning a LoopExecution.
-- The checkpoint also tells the host whether consumed auxiliary context (such
-- as a task-plan reminder) already belongs to a committed compaction boundary.
data ExceptionalTurn = ExceptionalTurn
    { exceptionalPatch :: ConversationPatch
    , exceptionalCompaction :: Maybe AutomaticCompactionBoundary
    }

-- | Run the real loop, without catching or converting asynchronous exceptions.
-- The exceptional-state sink runs under 'onException' before the exception
-- propagates. Returned provider failures use normal TurnEngine finalization,
-- not this sink. Loop events, including attempt-local display retractions,
-- are forwarded unchanged through the supplied LoopConfig.
--
-- Reading the successful checkpoint deliberately remains outside onException:
-- a later host/presentation failure must not retroactively roll back the loop.
executePreparedTurn
    :: PreparedExecution
    -> IO (Maybe AutomaticCompactionBoundary)
    -> (ExceptionalTurn -> IO ())
    -> IO ExecutedTurn
executePreparedTurn request readCompaction commitExceptional = do
    execution <-
        runLoopInputsDetailed
            request.executionConfig
            request.executionPreviousResponseId
            request.executionPreparedTurn.preparedTurnInputs
        `onException` do
            boundary <- readCompaction
            commitExceptional ExceptionalTurn
                { exceptionalPatch =
                    finishConversation
                        (rebasePreparedTurn boundary request.executionPreparedTurn)
                        ConversationInterrupted
                , exceptionalCompaction = boundary
                }
    boundary <- readCompaction
    pure ExecutedTurn
        { executedLoop = execution
        , executedCompaction = boundary
        }
