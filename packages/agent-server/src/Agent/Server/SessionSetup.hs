-- | Background Git checkout after a session already exists. Turns wait on
-- the registry until clone and branch creation finish (or fail).
module Agent.Server.SessionSetup
    ( SessionEventSink(..)
    , SessionSetupRegistry
    , noopSessionEventSink
    , newSessionSetupRegistry
    , closeSessionSetupRegistry
    , startRepositorySetup
    , startRepositorySetupWith
    , awaitSessionSetup
    , cancelSessionSetup
    ) where

import Agent.Server.RepositoryCheckout
    ( CheckoutOperationStatus(..)
    , PreparedRepositoryLayout(..)
    , RepositoryCheckout(..)
    , RepositoryCheckoutOperation(..)
    , completeRepositoryCheckout
    )
import Agent.Server.Types
    ( AccessBoundary
    , RepositoryDescriptor(..)
    )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, race)
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , newEmptyMVar
    , newMVar
    , putMVar
    , readMVar
    , takeMVar
    , tryPutMVar
    )
import Control.Exception.Safe (tryAny)
import Control.Monad (void)
import Data.Aeson (Value, object, (.=))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

data SessionEventSink = SessionEventSink
    { emitSessionEvent :: AccessBoundary -> Text -> Text -> Value -> IO ()
    }

noopSessionEventSink :: SessionEventSink
noopSessionEventSink = SessionEventSink \_ _ _ _ -> pure ()

newtype SessionSetupRegistry = SessionSetupRegistry (MVar (Map Text SessionSetup))

data SessionSetup = SessionSetup
    { setupDone :: !(MVar (Either Text RepositoryCheckout))
    , setupWorker :: !(Async ())
    }

type CheckoutRunner
    = PreparedRepositoryLayout
    -> RepositoryDescriptor
    -> (RepositoryCheckoutOperation -> CheckoutOperationStatus -> IO ())
    -> IO (Either Text RepositoryCheckout)

newSessionSetupRegistry :: IO SessionSetupRegistry
newSessionSetupRegistry = SessionSetupRegistry <$> newMVar Map.empty

closeSessionSetupRegistry :: SessionSetupRegistry -> IO ()
closeSessionSetupRegistry (SessionSetupRegistry var) = do
    setups <- modifyMVar var \current ->
        pure (Map.empty, Map.elems current)
    mapM_ abandon setups

startRepositorySetup
    :: SessionSetupRegistry
    -> (Text -> Value -> IO ())
    -> Text
    -> PreparedRepositoryLayout
    -> RepositoryDescriptor
    -> IO ()
startRepositorySetup registry emit sessionId layout descriptor =
    startRepositorySetupWith
        registry
        emit
        sessionId
        layout
        descriptor
        completeRepositoryCheckout

startRepositorySetupWith
    :: SessionSetupRegistry
    -> (Text -> Value -> IO ())
    -> Text
    -> PreparedRepositoryLayout
    -> RepositoryDescriptor
    -> CheckoutRunner
    -> IO ()
startRepositorySetupWith
        (SessionSetupRegistry var)
        emit
        sessionId
        layout
        descriptor
        complete = do
    ready <- newEmptyMVar
    done <- newEmptyMVar
    worker <-
        async $ do
            takeMVar ready
            runCheckout emit done layout descriptor complete
    added <-
        modifyMVar var \current ->
            if Map.member sessionId current
                then pure (current, False)
                else
                    pure
                        ( Map.insert
                            sessionId
                            SessionSetup{setupDone = done, setupWorker = worker}
                            current
                        , True
                        )
    if added
        then putMVar ready ()
        else do
            cancel worker
            void $ tryPutMVar done (Left "repository checkout already in progress")

awaitSessionSetup :: SessionSetupRegistry -> Text -> IO (Either Text ())
awaitSessionSetup (SessionSetupRegistry var) sessionId = do
    current <- readMVar var
    case Map.lookup sessionId current of
        Nothing -> pure (Right ())
        Just setup ->
            race
                (threadDelay sessionSetupWaitMicroseconds)
                (readMVar setup.setupDone)
                >>= \case
                    Left () -> pure (Left "repository checkout timed out")
                    Right result -> pure (() <$ result)

cancelSessionSetup :: SessionSetupRegistry -> Text -> IO ()
cancelSessionSetup (SessionSetupRegistry var) sessionId = do
    setup <-
        modifyMVar var \current ->
            pure (Map.delete sessionId current, Map.lookup sessionId current)
    mapM_ abandon setup

abandon :: SessionSetup -> IO ()
abandon setup = do
    cancel setup.setupWorker
    void $ tryPutMVar setup.setupDone (Left "repository checkout cancelled")

runCheckout
    :: (Text -> Value -> IO ())
    -> MVar (Either Text RepositoryCheckout)
    -> PreparedRepositoryLayout
    -> RepositoryDescriptor
    -> CheckoutRunner
    -> IO ()
runCheckout emit done layout descriptor complete = do
    emit "session.setup.started" $
        object
            [ "repository" .= descriptor.repositoryFullName
            , "branch" .= descriptor.repositoryDefaultBranch
            ]
    let onStep operation status =
            emit "session.setup.step" $
                object
                    [ "id" .= operation.checkoutOperationId
                    , "command" .= operation.checkoutOperationCommand
                    , "status" .= checkoutOperationStatusText status
                    ]
    result <- tryAny (complete layout descriptor onStep)
    finished <- case result of
        Left _ -> failCheckout emit layout "could not prepare repository checkout"
        Right (Left message) -> failCheckout emit layout message
        Right (Right checkout) -> do
            emit "session.setup.completed" $
                object
                    [ "repository" .= descriptor.repositoryFullName
                    , "branch" .= checkout.checkoutBranch
                    ]
            pure (Right checkout)
    void $ tryPutMVar done finished

failCheckout
    :: (Text -> Value -> IO ())
    -> PreparedRepositoryLayout
    -> Text
    -> IO (Either Text RepositoryCheckout)
failCheckout emit layout message = do
    emit "session.setup.failed" $ object ["message" .= message]
    _ <- tryAny layout.layoutCleanup
    pure (Left message)

checkoutOperationStatusText :: CheckoutOperationStatus -> Text
checkoutOperationStatusText = \case
    CheckoutOperationRunning -> "running"
    CheckoutOperationCompleted -> "completed"
    CheckoutOperationFailed -> "failed"

sessionSetupWaitMicroseconds :: Int
sessionSetupWaitMicroseconds = 10 * 60 * 1_000_000
