{-# LANGUAGE RankNTypes #-}
-- | Durable parent delivery independent of any running parent turn. The host
-- supplies storage and authorization; the runtime selects and coalesces events.
module Agent.Subagents.Runtime.Notifications
    ( ParentNotification (..)
    , ParentNotificationStore (..)
    , queueParentNotification
    , notifyParentOfSettlement
    ) where

import Agent.InterAgentMessage (InterAgentMessage)
import Agent.Subagents.Types (SubagentStatus (..))
import Control.Monad (unless, void)
import Data.Text (Text)

data ParentNotification
    = ChildProgressAvailable
    | ChildMessageAvailable !InterAgentMessage
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
