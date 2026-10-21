module Agent.Server.ClientRegistrySpec (spec) where

import Agent.Server.ClientRegistry
import Agent.Server.Types
import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception.Safe (throwString)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = describe "local client ownership" do
    it "closes service admission atomically after outstanding mutations finish" do
        registry <- newClientRegistry (pure 0) 5
        entered <- newEmptyMVar
        finish <- newEmptyMVar
        withAsync (withServiceMutation registry (putMVar entered () >> takeMVar finish)) \worker -> do
            takeMVar entered
            beginServiceShutdown registry False `shouldReturn` False
            putMVar finish ()
            wait worker `shouldReturn` Right ()
        beginServiceShutdown registry False `shouldReturn` True
        registerClient registry "later" `shouldReturn` Left ClientRegistryClosed
        withServiceMutation registry (pure ()) `shouldReturn` Left ClientRegistryClosed

    it "releases an idle session when its client switches conversations" do
        registry <- newClientRegistry (pure 0) 5
        registerClient registry "first" `shouldReturn` Right ()
        registerClient registry "second" `shouldReturn` Right ()
        bindClientSession registry "first" (session "old") `shouldReturn` Right ()
        unbindClientSession registry "first" "old" `shouldReturn` Right ()
        bindClientSession registry "second" (session "old") `shouldReturn` Right ()

    it "requires force to stop a connected client" do
        registry <- newClientRegistry (pure 0) 5
        registerClient registry "client" `shouldReturn` Right ()
        beginServiceShutdown registry False `shouldReturn` False
        beginServiceShutdown registry True `shouldReturn` True
        registerClient registry "later" `shouldReturn` Left ClientRegistryClosed

    it "expires only the client whose heartbeat stopped" do
        clock <- newIORef 0
        registry <- newClientRegistry (readIORef clock) 5
        registerClient registry "first" `shouldReturn` Right ()
        registerClient registry "second" `shouldReturn` Right ()
        bindClientSession registry "first" (session "first-session") `shouldReturn` Right ()
        bindClientSession registry "second" (session "second-session") `shouldReturn` Right ()
        writeIORef clock 4
        renewClient registry "second" `shouldReturn` Right ()
        writeIORef clock 5
        cancelled <- newIORef []
        maintainClientRegistry registry
            (\owned -> modifyIORef' cancelled (<> [owned.clientSessionId]))
            (const (pure False))
        readIORef cancelled `shouldReturn` ["first-session"]
        clientRegistryCounts registry `shouldReturn` (1, 1)
        renewClient registry "second" `shouldReturn` Right ()
        renewClient registry "first" `shouldReturn` Left ClientNotRegistered

    it "rejects late heartbeats instead of reviving an expired lease" do
        clock <- newIORef 0
        registry <- newClientRegistry (readIORef clock) 5
        registerClient registry "client" `shouldReturn` Right ()
        writeIORef clock 5
        renewClient registry "client" `shouldReturn` Left ClientLeaseExpired
        withClientMutation registry "client" Nothing (pure ())
            `shouldReturn` Left ClientLeaseExpired

    it "keeps the writer claim until cancellation and cleanup finish" do
        registry <- newClientRegistry (pure 0) 5
        registerClient registry "first" `shouldReturn` Right ()
        registerClient registry "second" `shouldReturn` Right ()
        bindClientSession registry "first" (session "shared-session") `shouldReturn` Right ()
        bindClientSession registry "second" (session "shared-session")
            `shouldReturn` Left ClientSessionOwned
        releaseClient registry "first"
        releaseClient registry "first"
        renewClient registry "first" `shouldReturn` Left ClientLeaseExpired
        cancelled <- newIORef (0 :: Int)
        let cancelOwned _ = modifyIORef' cancelled (+ 1)
        maintainClientRegistry registry cancelOwned (const (pure True))
        bindClientSession registry "second" (session "shared-session")
            `shouldReturn` Left ClientSessionOwned
        maintainClientRegistry registry cancelOwned (const (pure False))
        readIORef cancelled `shouldReturn` 1
        bindClientSession registry "second" (session "shared-session") `shouldReturn` Right ()

    it "waits for an admitted submit before requesting cancellation" do
        registry <- newClientRegistry (pure 0) 5
        registerClient registry "client" `shouldReturn` Right ()
        bindClientSession registry "client" (session "session") `shouldReturn` Right ()
        entered <- newEmptyMVar
        finish <- newEmptyMVar
        cancelled <- newIORef False
        let submit = withClientMutation registry "client" (Just "session") do
                putMVar entered ()
                takeMVar finish
            cancelOwned _ = writeIORef cancelled True
        withAsync submit \worker -> do
            takeMVar entered
            releaseClient registry "client"
            maintainClientRegistry registry cancelOwned (const (pure False))
            readIORef cancelled `shouldReturn` False
            withClientMutation registry "client" (Just "session") (pure ())
                `shouldReturn` Left ClientLeaseExpired
            putMVar finish ()
            wait worker `shouldReturn` Right ()
        maintainClientRegistry registry cancelOwned (const (pure False))
        readIORef cancelled `shouldReturn` True

    it "releases admission after an exception and retries failed cancellation" do
        registry <- newClientRegistry (pure 0) 5
        registerClient registry "client" `shouldReturn` Right ()
        bindClientSession registry "client" (session "session") `shouldReturn` Right ()
        withClientMutation registry "client" (Just "session") (throwString "submit failed" :: IO ())
            `shouldThrow` anyException
        releaseClient registry "client"
        maintainClientRegistry registry (const (throwString "cancellation unavailable")) (const (pure False))
        clientRegistryCounts registry `shouldReturn` (1, 1)
        cancelled <- newIORef False
        maintainClientRegistry registry (\_ -> writeIORef cancelled True) (const (pure False))
        readIORef cancelled `shouldReturn` True
        clientRegistryCounts registry `shouldReturn` (0, 0)

    it "does not admit work after registry shutdown" do
        registry <- newClientRegistry (pure 0) 5
        registerClient registry "client" `shouldReturn` Right ()
        closeClientRegistry registry (const (pure ()))
        registerClient registry "later" `shouldReturn` Left ClientRegistryClosed
        withClientMutation registry "client" Nothing (pure ())
            `shouldReturn` Left ClientRegistryClosed

session :: Text -> ClientSession
session identifier = ClientSession
    (accessBoundary localPrincipal (GatewayBoundary Nothing)) identifier
