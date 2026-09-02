module Agent.CLI.GatewayBoundarySpec (spec) where

import Agent.CLI.GatewayBoundary
import Agent.CLI.GatewayClient
    ( GatewayCredential(..)
    , saveGatewayCredentialAt
    )
import Agent.CLI.ModelConfig (organizationGatewayConnectionId)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
    ( poll
    , wait
    , withAsync
    )
import Control.Concurrent.MVar
    ( newEmptyMVar
    , putMVar
    , takeMVar
    )
import Control.Exception.Safe (bracket)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (isNothing)
import Data.Text qualified as Text
import System.Directory
    ( createDirectory
    , getTemporaryDirectory
    , removeFile
    , removePathForcibly
    )
import System.IO (hClose, openTempFile)
import System.OsPath
    ( OsPath
    , decodeUtf
    , unsafeEncodeUtf
    )
import Test.Hspec

spec :: Spec
spec = describe "GatewayBoundary" do
    it "treats direct, gateway, and replacement credentials as exact boundaries" do
        let first = testCredential "first-secret"
            replacement = testCredential "replacement-secret"
            direct = gatewayBoundaryFromCredential Nothing
            firstBoundary = gatewayBoundaryFromCredential (Just first)
            replacementBoundary =
                gatewayBoundaryFromCredential (Just replacement)
        direct `shouldNotBe` firstBoundary
        firstBoundary `shouldNotBe` replacementBoundary
        gatewayBoundariesMatch firstBoundary firstBoundary `shouldBe` True

    it "validates persisted sessions against the exact current route" do
        let credential = testCredential "secret"
            boundary = gatewayBoundaryFromCredential (Just credential)
            identity = boundary.gatewayBoundaryIdentity
        validateGatewaySessionBoundary
            boundary
            organizationGatewayConnectionId
            identity
            `shouldBe` Right ()
        validateGatewaySessionBoundary
            (gatewayBoundaryFromCredential Nothing)
            organizationGatewayConnectionId
            identity
            `shouldSatisfy` isSessionRejection

    it "rejects stale turn admission without executing the turn" $
        withTempHome \home -> do
            let initial = testCredential "initial-secret"
                replacement = testCredential "replacement-secret"
            saveGatewayCredentialAt home initial `shouldReturn` Right ()
            admitted <- loadGatewayBoundaryAt home >>= rightOrFail
            saveGatewayCredentialAt home replacement `shouldReturn` Right ()
            executed <- newIORef False
            withGatewayTurnBoundaryAt home admitted (writeIORef executed True)
                `shouldReturn` Left GatewayBoundaryChanged
            readIORef executed `shouldReturn` False

    it "holds one exact boundary for an entire short operation" $
        withTempHome \home -> do
            let initial = testCredential "initial-secret"
                replacement = testCredential "replacement-secret"
            saveGatewayCredentialAt home initial `shouldReturn` Right ()
            entered <- newEmptyMVar
            release <- newEmptyMVar
            writerStarted <- newEmptyMVar
            withAsync
                (withCurrentGatewayBoundaryAt home \boundary -> do
                    putMVar entered boundary
                    takeMVar release)
                \operation -> do
                    captured <- takeMVar entered
                    captured
                        `shouldBe` gatewayBoundaryFromCredential (Just initial)
                    withAsync
                        (putMVar writerStarted ()
                            >> saveGatewayCredentialAt home replacement)
                        \writer -> do
                            takeMVar writerStarted
                            threadDelay 100000
                            poll writer >>= (`shouldSatisfy` isNothing)
                            putMVar release ()
                            wait operation `shouldReturn` Right ()
                            wait writer `shouldReturn` Right ()
            loadGatewayBoundaryAt home
                `shouldReturn`
                    Right (gatewayBoundaryFromCredential (Just replacement))

    it "holds the exact boundary for the full lifetime of a turn" $
        withTempHome \home -> do
            let initial = testCredential "initial-secret"
                replacement = testCredential "replacement-secret"
            saveGatewayCredentialAt home initial `shouldReturn` Right ()
            admitted <- loadGatewayBoundaryAt home >>= rightOrFail
            entered <- newEmptyMVar
            release <- newEmptyMVar
            writerStarted <- newEmptyMVar
            withAsync
                (withGatewayTurnBoundaryAt home admitted do
                    putMVar entered ()
                    takeMVar release)
                \turn -> do
                    takeMVar entered
                    withAsync
                        (putMVar writerStarted ()
                            >> saveGatewayCredentialAt home replacement)
                        \writer -> do
                            takeMVar writerStarted
                            threadDelay 100000
                            poll writer >>= (`shouldSatisfy` isNothing)
                            putMVar release ()
                            wait turn `shouldReturn` Right ()
                            wait writer `shouldReturn` Right ()

    it "revalidates callbacks after a turn lease has been released" $
        withTempHome \home -> do
            let initial = testCredential "initial-secret"
                replacement = testCredential "replacement-secret"
            saveGatewayCredentialAt home initial `shouldReturn` Right ()
            admitted <- loadGatewayBoundaryAt home >>= rightOrFail
            withGatewayTurnBoundaryAt home admitted (pure ())
                `shouldReturn` Right ()
            saveGatewayCredentialAt home replacement `shouldReturn` Right ()
            emitted <- newIORef False
            withExpectedGatewayBoundaryAt
                home
                admitted
                (writeIORef emitted True)
                `shouldReturn` Left GatewayBoundaryChanged
            readIORef emitted `shouldReturn` False

isSessionRejection :: Either GatewayBoundaryError () -> Bool
isSessionRejection = \case
    Left (GatewayBoundarySessionRejected _) -> True
    _ -> False

testCredential :: String -> GatewayCredential
testCredential secret =
    GatewayCredential
        "https://gateway.example"
        "wss://gateway.example/v1/responses"
        (Text.pack secret)

rightOrFail :: Show err => Either err value -> IO value
rightOrFail = \case
    Left err -> do
        expectationFailure ("expected Right, got Left " <> show err)
        fail "unreachable after expectationFailure"
    Right value -> pure value

withTempHome :: (OsPath -> IO value) -> IO value
withTempHome =
    bracket create
        (removePathForcibly . either (error . show) id . decodeUtf)
  where
    create = do
        temporary <- getTemporaryDirectory
        (path, handle) <- openTempFile temporary "agent-gateway-boundary"
        hClose handle
        removeFile path
        createDirectory path
        pure (unsafeEncodeUtf path)
