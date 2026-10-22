{-# LANGUAGE RankNTypes #-}
module Agent.Subagents.NotificationsSpec (spec) where

import Agent.InterAgentMessage
import Agent.Subagents.Runtime.Notifications
import Agent.Subagents.Types (SubagentId (..), SubagentStatus (..))
import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent.MVar
import Control.Exception (onException)
import Data.IORef
import qualified Data.Text as Text
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
        let reconstructed = mapTransactionStore
                (\bound -> bound { requestParentTurn = modifyIORef' state (\(items, count) -> (items, count + 1)) }) store
        notifyParentOfSettlement reconstructed (Errored "failure")
        readIORef state `shouldReturn` ([ChildProgressAvailable], 2)

    it "does not coalesce explicit messages" do
        (store, state) <- newStore
        let message = ChildMessageAvailable (InterAgentMessage "worker" "root" QueuedMessage (PlainInterAgentContent "result") Nothing)
        queueParentNotification store message `shouldReturn` Right ()
        queueParentNotification store message `shouldReturn` Right ()
        readIORef state `shouldReturn` ([message, message], 2)

    it "delivers complete reports with their authors even while progress is pending" do
        (store, state) <- newStore
        queueParentNotification store ChildProgressAvailable `shouldReturn` Right ()
        let first = SubagentId "agent-first"
            second = SubagentId "agent-second"
            report = Completed (Just (Text.replicate 700 "x" <> "\nReport ending"))
        notifyParentOfAgentSettlement store first report
        notifyParentOfAgentSettlement store second (Completed (Just "Second report"))
        readIORef state `shouldReturn`
            ([ChildProgressAvailable, ChildResultAvailable first report,
                ChildResultAvailable second (Completed (Just "Second report"))], 3)

    it "preserves later reports and errors from the same agent" do
        (store, state) <- newStore
        let agentId = SubagentId "agent-review"
            initial = Completed Nothing
            revised = Completed (Just "Revised report")
            failure = Errored "Provider failure"
        mapM_ (notifyParentOfAgentSettlement store agentId) [initial, revised, failure]
        readIORef state `shouldReturn`
            (map (ChildResultAvailable agentId) [initial, revised, failure], 3)

    it "rolls back an attributed result when its wake request fails" do
        (store, state) <- newStore
        let broken = mapTransactionStore (\bound -> bound { requestParentTurn = ioError (userError "wake write failed") }) store
        notifyParentOfAgentSettlement broken (SubagentId "agent-review") (Completed (Just "Report"))
            `shouldThrow` anyIOException
        readIORef state `shouldReturn` ([], 0)

    it "rejects delivery without appending a message or scheduling a turn" do
        (store, state) <- newStore
        queueParentNotification (mapTransactionStore (\bound -> bound { authorizeDelivery = pure (Left "disabled") }) store) ChildProgressAvailable
            `shouldReturn` Left "disabled"
        readIORef state `shouldReturn` ([], 0)

    it "uses only the callbacks supplied by the delivery transaction" do
        (store, state) <- newStore
        let unbound = store
                { authorizeDelivery = fail "unbound authorization"
                , hasPendingProgress = fail "unbound pending read"
                , appendNotification = \_ -> fail "unbound notification write"
                , requestParentTurn = fail "unbound wake write"
                }
        queueParentNotification unbound ChildProgressAvailable `shouldReturn` Right ()
        readIORef state `shouldReturn` ([ChildProgressAvailable], 1)

    it "propagates wake failures through the transaction so message and wake roll back together" do
        (store, state) <- newStore
        let broken = mapTransactionStore (\bound -> bound { requestParentTurn = ioError (userError "wake write failed") }) store
        queueParentNotification broken ChildProgressAvailable `shouldThrow` anyIOException
        readIORef state `shouldReturn` ([], 0)
        queueParentNotification store ChildProgressAvailable `shouldReturn` Right ()
        readIORef state `shouldReturn` ([ChildProgressAvailable], 1)

    it "does not wake the parent for execution or cancellation bookkeeping" do
        (store, state) <- newStore
        mapM_ (notifyParentOfSettlement store) [Pending, Running, Interrupted, Closed, NotFound]
        readIORef state `shouldReturn` ([], 0)

mapTransactionStore :: (ParentNotificationStore -> ParentNotificationStore) -> ParentNotificationStore -> ParentNotificationStore
mapTransactionStore transform store@ParentNotificationStore { withDeliveryTransaction = transact } =
    store { withDeliveryTransaction = \action -> transact (action . transform) }

newStore :: IO (ParentNotificationStore, IORef ([ParentNotification], Int))
newStore = do
    state <- newIORef ([], 0)
    lock <- newMVar ()
    let transaction :: forall a. (ParentNotificationStore -> IO a) -> IO a
        transaction action = withMVar lock \_ -> do
            previous <- readIORef state
            action store `onException` writeIORef state previous
        store = ParentNotificationStore
            { withDeliveryTransaction = transaction
            , authorizeDelivery = pure (Right ())
            , hasPendingProgress = elem ChildProgressAvailable . fst <$> readIORef state
            , appendNotification = \item -> modifyIORef' state (\(items, count) -> (items <> [item], count))
            , requestParentTurn = modifyIORef' state (\(items, count) -> (items, count + 1))
            }
    pure (store, state)
