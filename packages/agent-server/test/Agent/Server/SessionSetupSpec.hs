module Agent.Server.SessionSetupSpec (spec) where

import Agent.Server.RepositoryCheckout
    ( CheckoutOperationStatus(..)
    , PreparedRepositoryLayout(..)
    , RepositoryCheckout(..)
    , RepositoryCheckoutOperation(..)
    , prepareRepositoryLayout
    , repositoryCheckoutOperations
    )
import Agent.Server.SessionSetup
    ( awaitSessionSetup
    , closeSessionSetupRegistry
    , newSessionSetupRegistry
    , startRepositorySetupWith
    )
import Agent.Server.Types (RepositoryDescriptor(..))
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = do
    describe "repository checkout operations" do
        it "names the clone and working-branch Git commands" do
            withSystemTempDirectory "agent-server-checkout" \root -> do
                prepareRepositoryLayout root "01testcorrelation" validDescriptor
                    >>= \case
                        Left err -> fail (show err)
                        Right layout -> do
                            let commands =
                                    map
                                        (.checkoutOperationCommand)
                                        (repositoryCheckoutOperations layout validDescriptor)
                                cloneCommand :: Text
                                cloneCommand =
                                    "git clone --single-branch --branch main https://github.com/digitallyinduced/haskell-agent.git"
                                switchCommand :: Text
                                switchCommand = "git switch -c agent/01testcorrelation"
                            commands `shouldSatisfy` elem cloneCommand
                            commands `shouldSatisfy` elem switchCommand
                            doesDirectoryExist layout.layoutCheckoutPath
                                `shouldReturn` False
                            doesFileExist layout.layoutHelperPath
                                `shouldReturn` True
                            layout.layoutCleanup

    describe "session repository setup" do
        it "returns from start before Git operations finish" do
            withSystemTempDirectory "agent-server-setup" \root -> do
                layout <-
                    prepareRepositoryLayout root "01testcorrelation" validDescriptor
                        >>= either (fail . show) pure
                started <- newEmptyMVar
                release <- newEmptyMVar
                events <- newIORef []
                registry <- newSessionSetupRegistry
                let complete pendingLayout _descriptor onStep = do
                        putMVar started ()
                        takeMVar release
                        forM_
                            (repositoryCheckoutOperations pendingLayout validDescriptor)
                            \operation -> do
                                _ <- onStep operation CheckoutOperationRunning
                                onStep operation CheckoutOperationCompleted
                        pure $
                            Right
                                RepositoryCheckout
                                    { checkoutPath = pendingLayout.layoutCheckoutPath
                                    , checkoutBranch = pendingLayout.layoutBranch
                                    , cleanupCheckout = pendingLayout.layoutCleanup
                                    }
                    emit eventType payload =
                        atomicModifyIORef' events \current ->
                            (current <> [(eventType, payload)], ())
                startRepositorySetupWith
                    registry
                    emit
                    "session-1"
                    layout
                    validDescriptor
                    complete
                takeMVar started
                recordedBeforeFinish <- readIORef events
                recordedBeforeFinish
                    `shouldSatisfy`
                        (any (\(eventType, _) -> eventType == "session.setup.started"))
                putMVar release ()
                awaitSessionSetup registry "session-1" `shouldReturn` Right ()
                recorded <- readIORef events
                map fst recorded
                    `shouldSatisfy`
                        (\types ->
                            elem ("session.setup.started" :: Text) types
                                && elem ("session.setup.step" :: Text) types
                                && elem ("session.setup.completed" :: Text) types)
                closeSessionSetupRegistry registry

        it "fails the waiting turn when checkout fails" do
            withSystemTempDirectory "agent-server-setup-fail" \root -> do
                layout <-
                    prepareRepositoryLayout root "01testcorrelation" validDescriptor
                        >>= either (fail . show) pure
                registry <- newSessionSetupRegistry
                let complete _ _ _ =
                        pure (Left "git clone failed")
                startRepositorySetupWith
                    registry
                    (\_ _ -> pure ())
                    "session-1"
                    layout
                    validDescriptor
                    complete
                awaitSessionSetup registry "session-1"
                    `shouldReturn` Left "git clone failed"
                closeSessionSetupRegistry registry

validDescriptor :: RepositoryDescriptor
validDescriptor =
    RepositoryDescriptor
        { repositoryFullName = "digitallyinduced/haskell-agent"
        , repositoryCloneUrl = "https://github.com/digitallyinduced/haskell-agent.git"
        , repositoryDefaultBranch = "main"
        , repositoryCredentialBrokerUrl = "https://gateway.example/api/v1/github/repository-token"
        , repositoryCredentialLease = "opaque-lease"
        }
