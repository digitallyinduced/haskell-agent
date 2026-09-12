-- | State shared by turn execution, interaction hooks, and supervision.
-- The supervisor remains the owner of worker creation, cancellation, and join.
module Agent.CLI.MacOS.TurnState
    ( NativeTurnOptions(..)
    , VoiceAudioCallback
    , defaultNativeTurnOptions
    , TurnControl(..)
    , TurnOutcome(..)
    , TaskResult(..)
    , PendingTurn(..)
    , RunningTurn(..)
    , TaskSupervisor(..)
    , defaultTaskLimit
    , discardStagedTurn
    , discardStagedTurnById
    , newTurnControl
    , cancelTurn
    ) where

import qualified Agent.Runtime.AgentSnapshot as Viewport
import Agent.CLI.MacOS.InteractionState
    ( InteractionRuntime(..), PendingInteraction(..), cancelledInteractionResolution )
import Agent.CLI.MacOS.NativeRequest (TurnStart, turnStartCleanupId)
import Agent.CLI.NativeRuntime (NativeInteractionMode(..), NativeShellMode(..))
import Agent.CLI.Permission (PermissionChoice(..))
import Agent.Loop (ImageAttachment, TokenUsage)
import Control.Concurrent.Async (Async)
import Control.Concurrent.STM
import Control.Monad (forM_, void)
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Word (Word8)
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types (CInt, CSize)

discardStagedTurn
    :: Text
    -> Aeson.Value
    -> TVar (Map Text a)
    -> TVar (Map Text b)
    -> STM ()
discardStagedTurn requestId params stagedImages stagedOptions = do
    discardStagedTurnById
        (turnStartCleanupId requestId params)
        stagedImages
        stagedOptions

discardStagedTurnById
    :: Text
    -> TVar (Map Text a)
    -> TVar (Map Text b)
    -> STM ()
discardStagedTurnById turnId stagedImages stagedOptions = do
    modifyTVar' stagedImages (Map.delete turnId)
    modifyTVar' stagedOptions (Map.delete turnId)

type VoiceAudioCallback = Ptr () -> CInt -> Ptr () -> Ptr Word8 -> CSize -> IO CInt

data NativeTurnOptions = NativeTurnOptions
    { nativeTurnInteractionMode :: !NativeInteractionMode
    , nativeTurnShellMode :: !NativeShellMode
    , nativeTurnVoice :: !(Maybe (FunPtr VoiceAudioCallback, Ptr ()))
    } deriving (Eq, Show)

defaultNativeTurnOptions :: NativeTurnOptions
defaultNativeTurnOptions = NativeTurnOptions
    { nativeTurnInteractionMode = NativeAsk
    , nativeTurnShellMode = NativeShellBash
    , nativeTurnVoice = Nothing
    }

data TurnControl = TurnControl
    { turnControlId :: !Text
    , turnControlGatewayIdentity :: !(Maybe Text)
    , turnControlSessionId :: !(TVar (Maybe Text))
    , turnControlCancelled :: !(TVar Bool)
    , turnControlCancel :: !(TVar (IO ()))
    , turnControlApprovals
        :: !(TVar (Map Text (Bool, TMVar PermissionChoice)))
    , turnControlApprovalCounter :: !(TVar Int)
    , turnControlInteractionCounter :: !(TVar Int)
    , turnControlAllowedTools :: !(TVar (Set.Set Text))
    , turnControlAgentSnapshot :: !(TVar (IO [Viewport.AgentSnapshot]))
    , turnControlInteractions :: !InteractionRuntime
    }

data TurnOutcome = TurnOutcome
    { turnOutcomeSessionId :: !(Maybe Text)
    , turnOutcomeError :: !(Maybe Text)
    , turnOutcomeUsage :: !TokenUsage
    , turnOutcomeProviderCostUSD :: !(Maybe Double)
    }

data TaskResult
    = TaskOutcome !TurnOutcome
    | TaskFailure !Text

data PendingTurn = PendingTurn
    { pendingTurnStart :: !TurnStart
    , pendingTurnGatewayIdentity :: !(Maybe Text)
    , pendingTurnImages :: ![ImageAttachment]
    , pendingTurnOptions :: !NativeTurnOptions
    }

data RunningTurn = RunningTurn
    { runningTurnControl :: !TurnControl
    , runningTurnWorker :: !(Async ())
    }

data TaskSupervisor = TaskSupervisor
    { supervisorLimit :: !Int
    , supervisorPending :: !(Seq PendingTurn)
    , supervisorRunning :: !(Map Text RunningTurn)
    , supervisorKnownTaskIds :: !(Set.Set Text)
    }

defaultTaskLimit :: Int
defaultTaskLimit = 3

newTurnControl
    :: Text
    -> Maybe Text
    -> Maybe Text
    -> InteractionRuntime
    -> IO TurnControl
newTurnControl turnId gatewayIdentity sessionId interactions = do
    sessionIdRef <- newTVarIO sessionId
    cancelled <- newTVarIO False
    cancelAction <- newTVarIO (pure ())
    approvals <- newTVarIO Map.empty
    approvalCounter <- newTVarIO 0
    interactionCounter <- newTVarIO 0
    allowedTools <- newTVarIO Set.empty
    agentSnapshot <- newTVarIO (pure [])
    pure TurnControl
        { turnControlId = turnId
        , turnControlGatewayIdentity = gatewayIdentity
        , turnControlSessionId = sessionIdRef
        , turnControlCancelled = cancelled
        , turnControlCancel = cancelAction
        , turnControlApprovals = approvals
        , turnControlApprovalCounter = approvalCounter
        , turnControlInteractionCounter = interactionCounter
        , turnControlAllowedTools = allowedTools
        , turnControlAgentSnapshot = agentSnapshot
        , turnControlInteractions = interactions
        }

cancelTurn :: TurnControl -> IO ()
cancelTurn control = do
    atomically $ writeTVar control.turnControlCancelled True
    cancelAction <- readTVarIO control.turnControlCancel
    cancelAction
    waiters <- atomically do
        current <- readTVar control.turnControlApprovals
        writeTVar control.turnControlApprovals Map.empty
        pure (Map.elems current)
    atomically $
        forM_ waiters \(_, waiter) ->
            void (tryPutTMVar waiter PermissionDeny)
    interactionWaiters <- atomically do
        current <- readTVar
            control.turnControlInteractions.interactionPending
        let (owned, remaining) =
                Map.partitionWithKey
                    (\(turnID, _) _ ->
                        turnID == control.turnControlId)
                    current
        writeTVar
            control.turnControlInteractions.interactionPending
            remaining
        pure (map (.pendingInteractionWaiter) (Map.elems owned))
    atomically $
        forM_ interactionWaiters \waiter ->
            void $ tryPutTMVar waiter cancelledInteractionResolution
