{-# LANGUAGE RankNTypes #-}
-- | Durable parent delivery independent of any running parent turn. The host
-- supplies storage and authorization; the runtime selects and coalesces events.
module Agent.Subagents.Runtime.Notifications
    ( ParentNotification (..)
    , ParentNotificationStore (..)
    , queueParentNotification
    , notifyParentOfSettlement
    , notifyParentOfAgentSettlement
    ) where

import Agent.InterAgentMessage (InterAgentMessage)
import Agent.Subagents.Types (SubagentId, SubagentStatus (..))
import Control.Monad (unless, void)
import Data.Text (Text)

data ParentNotification
    = ChildProgressAvailable
    | ChildMessageAvailable !InterAgentMessage
    | ChildResultAvailable !SubagentId !SubagentStatus
    deriving (Eq, Show)

-- | Every callback runs within 'withDeliveryTransaction'. It must serialize
-- delivery for this parent, commit the message and wake request atomically,
-- and roll back both on exceptions. Authorization must be checked under that
-- same scope. Pending progress means an unconsumed durable notification, so
-- coalescing remains correct across process restarts and multiple children.
-- Supply callbacks freshly bound to the transaction's connection; wrapping
-- already captured IO actions does not change their database connection.
data ParentNotificationStore = ParentNotificationStore
    { withDeliveryTransaction :: forall a. (ParentNotificationStore -> IO a) -> IO a
    , authorizeDelivery :: IO (Either Text ())
    , hasPendingProgress :: IO Bool
    , appendNotification :: ParentNotification -> IO ()
    , requestParentTurn :: IO ()
    }

queueParentNotification
    :: ParentNotificationStore -> ParentNotification -> IO (Either Text ())
queueParentNotification ParentNotificationStore { withDeliveryTransaction } notification =
    withDeliveryTransaction \store -> store.authorizeDelivery >>= \case
        Left failure -> pure (Left failure)
        Right () -> do
            pending <- case notification of
                ChildProgressAvailable -> store.hasPendingProgress
                ChildMessageAvailable _ -> pure False
                ChildResultAvailable _ _ -> pure False
            unless pending (store.appendNotification notification)
            -- A coalesced message still requires a durable wake request.
            store.requestParentTurn
            pure (Right ())

notifyParentOfSettlement :: ParentNotificationStore -> SubagentStatus -> IO ()
notifyParentOfSettlement store = \case
    Completed _ -> notify
    Errored _ -> notify
    Suspended _ _ -> notify
    _ -> pure ()
  where
    notify = void (queueParentNotification store ChildProgressAvailable)

-- | Deliver each terminal report with its author instead of coalescing it
-- into a generic progress notice. The host persists the complete report and
-- its wake request in the same transaction. Suspensions retain progress-only
-- delivery, and execution/cancellation bookkeeping does not wake the parent.
notifyParentOfAgentSettlement :: ParentNotificationStore -> SubagentId -> SubagentStatus -> IO ()
notifyParentOfAgentSettlement store agentId status = case status of
    Completed _ -> deliver
    Errored _ -> deliver
    _ -> notifyParentOfSettlement store status
  where
    deliver = void (queueParentNotification store (ChildResultAvailable agentId status))
