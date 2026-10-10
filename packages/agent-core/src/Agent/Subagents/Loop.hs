-- | Delegated child agents of a root loop.
--
-- A 'RootSubagents' owns one conversation's registry across its turns.
-- 'subagentLoop' turns it into the 'LoopSubagents' hooks of 'LoopConfig':
-- the loop then begins a root turn before its first request and ends it when
-- it returns, delivers what the children report with its steering, and
-- applies a 'SubagentTurnEnd' policy to children still running when the root
-- answers.
module Agent.Subagents.Loop
    ( RootSubagents
    , SubagentNotices(..)
    , SubagentTurnEnd(..)
    , newRootSubagents
    , rootSubagentsRegistry
    , currentRootTurn
    , abortLastRootTurn
    , sendToRootLoop
    , subagentLoop
    ) where

import Agent.InterAgentMessage (InterAgentMessage)
import Agent.Loop (LoopSubagents(..), TurnInput(..))
import Agent.Loop.SteeringInputs
    ( SteeringInputs
    , awaitSteeringInputReady
    , commitSteeringInputs
    , enqueueBackgroundCompletion
    , newSteeringInputs
    , readSteeringInputs
    )
import Agent.Subagents.Format (formatCompletionNotice)
import Agent.Subagents.Registry
    ( SubagentRegistry
    , abortRootTurn
    , activeSubagentsForTurnSTM
    , beginRootTurn
    , setSubagentOnComplete
    )
import Agent.Subagents.Types
    ( RootTurnId
    , SubagentId(..)
    , SubagentStatus(..)
    )
import Control.Concurrent.STM (atomically, check, orElse, retry)
import Control.Exception.Safe (finally)
import Control.Monad (forM_, void, when)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Timeout (timeout)

-- | Where completion notices and messages for the root go.
data SubagentNotices
    -- | The root loop delivers them with its steering. A notice that arrives
    -- between turns waits for the next turn's first request.
    = NoticesToLoop
    -- | The host routes them itself, for example to start a turn while the
    -- root is idle. The session then installs no completion callback.
    | NoticesToHost
    deriving (Eq, Show)

-- | What happens to children still running when the root answers.
data SubagentTurnEnd
    -- | They keep running; their completion notices arrive later.
    = KeepChildrenRunning
    -- | Before answering, the root waits up to this many milliseconds for
    -- them and continues with their notices. Children still running at the
    -- deadline are interrupted, and the root is told so.
    | AwaitChildren !Int
    -- | They are interrupted.
    | InterruptChildren
    deriving (Eq, Show)

data RootSubagents = RootSubagents
    { sessionRegistry :: !SubagentRegistry
    , sessionNotices :: !SubagentNotices
    , sessionInputs :: !SteeringInputs
    , sessionNextNotice :: !(IORef Int)
    , sessionTurn :: !(IORef (Maybe RootTurnId))
    , sessionLastTurn :: !(IORef (Maybe RootTurnId))
    }

-- | Wrap a host-owned registry. With 'NoticesToLoop' the session installs the
-- registry's completion callback.
newRootSubagents :: SubagentNotices -> SubagentRegistry -> IO RootSubagents
newRootSubagents notices registry = do
    inputs <- newSteeringInputs
    nextNotice <- newIORef 0
    turn <- newIORef Nothing
    lastTurn <- newIORef Nothing
    let session = RootSubagents
            { sessionRegistry = registry
            , sessionNotices = notices
            , sessionInputs = inputs
            , sessionNextNotice = nextNotice
            , sessionTurn = turn
            , sessionLastTurn = lastTurn
            }
    when (notices == NoticesToLoop) $
        setSubagentOnComplete registry \agentId status ->
            void (deliverToLoop session agentId.unSubagentId
                (UserMessage (formatCompletionNotice agentId status)))
    pure session

rootSubagentsRegistry :: RootSubagents -> SubagentRegistry
rootSubagentsRegistry session = session.sessionRegistry

-- | The root turn in progress, for admitting children spawned by the root.
currentRootTurn :: RootSubagents -> IO (Maybe RootTurnId)
currentRootTurn session = readIORef session.sessionTurn

-- | Interrupt the children of the most recent root turn, for a host that
-- discards a turn the loop finished (for example to restart it).
abortLastRootTurn :: RootSubagents -> IO ()
abortLastRootTurn session =
    readIORef session.sessionLastTurn
        >>= mapM_ (abortRootTurn session.sessionRegistry)

-- | Deliver a child's message to the root ('NoticesToLoop' only).
sendToRootLoop :: RootSubagents -> InterAgentMessage -> IO (Either Text Text)
sendToRootLoop session message =
    case session.sessionNotices of
        NoticesToHost -> pure (Left "root agent mailbox is unavailable")
        NoticesToLoop ->
            deliverToLoop session "message" (AgentMessage message) >>= \case
                Left err -> pure (Left err)
                Right _ -> pure (Right "queued")

subagentLoop :: SubagentTurnEnd -> RootSubagents -> LoopSubagents
subagentLoop turnEnd session = LoopSubagents
    { subagentsBeginTurn = do
        turn <- beginRootTurn session.sessionRegistry
        writeIORef session.sessionTurn (Just turn)
        writeIORef session.sessionLastTurn (Just turn)
    , subagentsReadInputs = readLoopInputs session
    , subagentsCommitInputs = \count ->
        when (session.sessionNotices == NoticesToLoop) $
            commitSteeringInputs session.sessionInputs count
    , subagentsBeforeAnswer = case turnEnd of
        AwaitChildren timeoutMs -> awaitChildren session timeoutMs
        KeepChildrenRunning -> pure []
        InterruptChildren -> pure []
    , subagentsEndTurn = \answered ->
        readIORef session.sessionTurn >>= mapM_ \turn ->
            when (not answered || turnEnd == InterruptChildren)
                (abortRootTurn session.sessionRegistry turn)
                `finally` writeIORef session.sessionTurn Nothing
    }

data AwaitOutcome = ChildReported | ChildrenSettled

-- | Wait for the root turn's children before the root answers. Reports that
-- are already queued are delivered at once.
awaitChildren :: RootSubagents -> Int -> IO [TurnInput]
awaitChildren session timeoutMs =
    readIORef session.sessionTurn >>= \case
        Nothing -> readLoopInputs session
        Just turn -> do
            outcome <- timeout (max 0 timeoutMs * 1000) $ atomically $
                (ChildReported <$ reported)
                    `orElse` do
                        active <- activeSubagentsForTurnSTM registry turn
                        check (null active)
                        pure ChildrenSettled
            case outcome of
                Just _ -> readLoopInputs session
                Nothing -> do
                    interrupted <- atomically (activeSubagentsForTurnSTM registry turn)
                    abortRootTurn registry turn
                    when (session.sessionNotices == NoticesToLoop) $
                        forM_ interrupted \agentId ->
                            deliverToLoop session agentId.unSubagentId
                                (UserMessage
                                    (formatCompletionNotice agentId Interrupted))
                    readLoopInputs session
  where
    registry = session.sessionRegistry
    reported = case session.sessionNotices of
        NoticesToLoop -> awaitSteeringInputReady session.sessionInputs
        NoticesToHost -> retry

readLoopInputs :: RootSubagents -> IO [TurnInput]
readLoopInputs session =
    case session.sessionNotices of
        NoticesToLoop -> readSteeringInputs session.sessionInputs
        NoticesToHost -> pure []

-- | Queue a report for the root. Completion notices are background
-- completions: a full queue defers them instead of rejecting them, and they
-- never count as user guidance.
deliverToLoop :: RootSubagents -> Text -> TurnInput -> IO (Either Text Bool)
deliverToLoop session source input = do
    sequenceNumber <- atomicModifyIORef' session.sessionNextNotice \next ->
        (next + 1, next)
    enqueueBackgroundCompletion
        session.sessionInputs
        ("subagent:" <> source <> ":" <> Text.pack (show sequenceNumber))
        input
