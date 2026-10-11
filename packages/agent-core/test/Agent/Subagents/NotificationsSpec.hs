{-# LANGUAGE RankNTypes #-}
module Agent.Subagents.NotificationsSpec (spec) where

import Agent.InterAgentMessage
import Agent.Subagents.Runtime.Notifications
import Agent.Subagents.Types (SubagentStatus (..))
import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent.MVar
import Control.Exception (onException)
import Data.IORef
import Test.Hspec

spec :: Spec
spec = describe "durable parent notifications" do
    it "coalesces concurrent child settlements and requests a parent turn" do
        (store, state) <- newStore
        mapConcurrently_ (notifyParentOfSettlement store) (replicate 20 (Completed Nothing))
        readIORef state `shouldReturn` ([ChildProgressAvailable], 20)

    it "uses durable pending state after rebuilding the delivery adapter" do
        (store, state) <- newStore
        notifyParentOfSettlement store (Completed Nothing)
        let reconstructed = store { requestParentTurn = modifyIORef' state (\(items, count) -> (items, count + 1)) }
        notifyParentOfSettlement reconstructed (Errored "failure")
        readIORef state `shouldReturn` ([ChildProgressAvailable], 2)

    it "does not coalesce explicit messages" do
        (store, state) <- newStore
        let message = ChildMessageAvailable (InterAgentMessage "worker" "root" QueuedMessage (PlainInterAgentContent "result"))
        queueParentNotification store message `shouldReturn` Right ()
        queueParentNotification store message `shouldReturn` Right ()
        readIORef state `shouldReturn` ([message, message], 2)

    it "rejects delivery without appending a message or scheduling a turn" do
        (store, state) <- newStore
        queueParentNotification (store { authorizeDelivery = pure (Left "disabled") }) ChildProgressAvailable
            `shouldReturn` Left "disabled"
        readIORef state `shouldReturn` ([], 0)

    it "propagates wake failures through the transaction so message and wake roll back together" do
        (store, state) <- newStore
        let broken = store { requestParentTurn = ioError (userError "wake write failed") }
        queueParentNotification broken ChildProgressAvailable `shouldThrow` anyIOException
        readIORef state `shouldReturn` ([], 0)
        queueParentNotification store ChildProgressAvailable `shouldReturn` Right ()
        readIORef state `shouldReturn` ([ChildProgressAvailable], 1)

    it "does not wake the parent for execution or cancellation bookkeeping" do
        (store, state) <- newStore
        mapM_ (notifyParentOfSettlement store) [Pending, Running, Interrupted, Closed, NotFound]
        readIORef state `shouldReturn` ([], 0)

newStore :: IO (ParentNotificationStore, IORef ([ParentNotification], Int))
newStore = do
    state <- newIORef ([], 0)
    lock <- newMVar ()
    let transaction action = withMVar lock \_ -> do
            previous <- readIORef state
            action `onException` writeIORef state previous
    pure (ParentNotificationStore
        { withDeliveryTransaction = transaction
        , authorizeDelivery = pure (Right ())
        , hasPendingProgress = elem ChildProgressAvailable . fst <$> readIORef state
        , appendNotification = \item -> modifyIORef' state (\(items, count) -> (items <> [item], count))
        , requestParentTurn = modifyIORef' state (\(items, count) -> (items, count + 1))
        }, state)
