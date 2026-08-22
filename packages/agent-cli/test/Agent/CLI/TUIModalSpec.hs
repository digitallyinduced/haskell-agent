module Agent.CLI.TUIModalSpec (spec) where

import qualified Agent.CLI.TUI.Modal as Modal
import Control.Concurrent.Async
    ( cancel
    , concurrently
    , poll
    , wait
    , waitCatch
    , withAsync
    )
import Control.Concurrent.STM
    ( TMVar
    , atomically
    , newEmptyTMVarIO
    , putTMVar
    , readTMVar
    , tryPutTMVar
    )
import Control.Monad (void)
import Data.Text (Text)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "fullscreen modal coordinator" do
    it "services two concurrent requests in FIFO order and delivers both replies" do
        coordinator <- Modal.newModalCoordinator
        firstCommitted <- newEmptyTMVarIO
        firstReply <- newEmptyTMVarIO :: IO (TMVar Text)
        secondReply <- newEmptyTMVarIO :: IO (TMVar Text)

        (firstSubmission, secondSubmission) <- concurrently
            (atomically do
                submission <-
                    Modal.submitModal
                        coordinator
                        ("first" :: Text)
                        (void (tryPutTMVar firstReply "first cancelled"))
                putTMVar firstCommitted ()
                pure submission)
            (atomically do
                readTMVar firstCommitted
                Modal.submitModal
                    coordinator
                    "second"
                    (void (tryPutTMVar secondReply "second cancelled")))

        (firstId, firstTicket) <- expectPresented firstSubmission
        secondId <- expectQueued secondSubmission
        Modal.modalTicketRequest firstTicket `shouldBe` "first"

        withAsync (atomically (readTMVar firstReply)) \firstWaiter ->
            withAsync (atomically (readTMVar secondReply)) \secondWaiter -> do
                afterFirst <- atomically $
                    Modal.completeModal coordinator firstId \request ->
                        if request == "first"
                            then Just (putTMVar firstReply "first reply")
                            else Nothing
                secondTicket <- expectAdvanced afterFirst
                Modal.modalTicketId secondTicket `shouldBe` secondId
                Modal.modalTicketRequest secondTicket `shouldBe` "second"

                expectWithin "first modal reply was not delivered"
                    (wait firstWaiter)
                    `shouldReturn` "first reply"
                poll secondWaiter >>= \case
                    Nothing ->
                        pure ()
                    Just _ ->
                        expectationFailure
                            "second reply arrived before its modal was active"

                afterSecond <- atomically $
                    Modal.completeModal coordinator secondId \request ->
                        if request == "second"
                            then Just (putTMVar secondReply "second reply")
                            else Nothing
                expectFinished afterSecond
                expectWithin "second modal reply was not delivered"
                    (wait secondWaiter)
                    `shouldReturn` "second reply"

    it "removes and unblocks an async-cancelled active request" do
        coordinator <- Modal.newModalCoordinator
        reply <- newEmptyTMVarIO :: IO (TMVar (Maybe Text))
        presented <-
            newEmptyTMVarIO
                :: IO (TMVar (Modal.ModalTicket Text))
        advanced <-
            newEmptyTMVarIO
                :: IO
                    (TMVar
                        ( Modal.ModalId
                        , Maybe (Modal.ModalTicket Text)
                        ))
        let request =
                Modal.runModalRequest
                    coordinator
                    "active"
                    (void (tryPutTMVar reply Nothing))
                    (readTMVar reply)
                    (atomically . putTMVar presented)
                    (\modalId next ->
                        putTMVar advanced (modalId, next))

        withAsync request \worker -> do
            ticket <- expectWithin "active modal was not presented" $
                atomically (readTMVar presented)
            cancel worker
            _ <- waitCatch worker
            (cancelledId, next) <-
                expectWithin "active modal cancellation did not advance" $
                    atomically (readTMVar advanced)
            cancelledId `shouldBe` Modal.modalTicketId ticket
            case next of
                Nothing ->
                    pure ()
                Just _ ->
                    expectationFailure
                        "cancelled modal unexpectedly promoted a request"
            atomically (readTMVar reply) `shouldReturn` Nothing

    it "closes active and queued waiters and unblocks later callers" do
        coordinator <- Modal.newModalCoordinator
        activeCommitted <- newEmptyTMVarIO
        activeReply <- newEmptyTMVarIO :: IO (TMVar (Maybe Text))
        queuedReply <- newEmptyTMVarIO :: IO (TMVar (Maybe Text))

        (activeSubmission, queuedSubmission) <- concurrently
            (atomically do
                submission <-
                    Modal.submitModal
                        coordinator
                        ("active" :: Text)
                        (void (tryPutTMVar activeReply Nothing))
                putTMVar activeCommitted ()
                pure submission)
            (atomically do
                readTMVar activeCommitted
                Modal.submitModal
                    coordinator
                    "queued"
                    (void (tryPutTMVar queuedReply Nothing)))

        _ <- expectPresented activeSubmission
        _ <- expectQueued queuedSubmission

        withAsync (atomically (readTMVar activeReply)) \activeWaiter ->
            withAsync (atomically (readTMVar queuedReply)) \queuedWaiter -> do
                atomically (Modal.closeModalCoordinator coordinator)
                expectWithin "active modal was not unblocked by shutdown"
                    (wait activeWaiter)
                    `shouldReturn` Nothing
                expectWithin "queued modal was not unblocked by shutdown"
                    (wait queuedWaiter)
                    `shouldReturn` Nothing

        lateReply <- newEmptyTMVarIO :: IO (TMVar (Maybe Text))
        lateResult <- expectWithin "closed request remained blocked" $
            Modal.runModalRequest
                coordinator
                "late"
                (void (tryPutTMVar lateReply Nothing))
                (readTMVar lateReply)
                (\_ ->
                    expectationFailure
                        "closed coordinator presented a late request")
                (\_ _ -> pure ())
        lateResult `shouldBe` Nothing

expectPresented
    :: Modal.ModalSubmission request
    -> IO (Modal.ModalId, Modal.ModalTicket request)
expectPresented = \case
    Modal.ModalSubmitted modalId (Just ticket) ->
        pure (modalId, ticket)
    _ ->
        expectationFailure "expected an immediately presented modal"
            >> fail "missing presented modal"

expectQueued
    :: Modal.ModalSubmission request
    -> IO Modal.ModalId
expectQueued = \case
    Modal.ModalSubmitted modalId Nothing ->
        pure modalId
    _ ->
        expectationFailure "expected a queued modal"
            >> fail "missing queued modal"

expectAdvanced
    :: Modal.ModalTransition request
    -> IO (Modal.ModalTicket request)
expectAdvanced = \case
    Modal.ModalAdvanced (Just ticket) ->
        pure ticket
    _ ->
        expectationFailure "expected promotion of the next modal"
            >> fail "missing promoted modal"

expectFinished :: Modal.ModalTransition request -> IO ()
expectFinished = \case
    Modal.ModalAdvanced Nothing ->
        pure ()
    _ ->
        expectationFailure "expected the modal queue to become idle"

expectWithin :: String -> IO a -> IO a
expectWithin message action =
    timeout 2000000 action >>= \case
        Just result ->
            pure result
        Nothing ->
            expectationFailure message >> fail message
