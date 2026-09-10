{-# LANGUAGE ForeignFunctionInterface #-}
module Agent.CLI.MacOS.MobileGatewaySpec (spec) where

import Agent.CLI.MacOS.MobileGateway (validPairingID)
import qualified Agent.CLI.MacOS.MobileGateway as Mobile
import Agent.CLI.GatewayClient (GatewayCredential(..), saveGatewayCredential, loadGatewayCredential)
import Agent.CLI.MacOS.MobileGatewayBridge
import Control.Concurrent.MVar
import Control.Exception.Safe (bracket)
import Foreign
import Foreign.C.Types
import System.Directory (getTemporaryDirectory, createDirectory, removeFile, removePathForcibly)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO (openTempFile, hClose)
import Test.Hspec

foreign import ccall "ha_mobile_validation_smoke" nativeSmoke :: IO CInt
foreign import ccall "wrapper" wrapCallback :: Callback -> IO (FunPtr Callback)

spec :: Spec
spec = describe "account-backed mobile gateway" do
    it "closes only the mobile session, preserving main credentials" $ withIsolatedHome do
        saveGatewayCredential fixtureCredential `shouldReturn` Right ()
        bracket Mobile.openSession Mobile.closeSession $ \session -> do
            Mobile.closeSession session
            Mobile.listPairings session `shouldThrow` isAccountUnavailable
            credential <- loadGatewayCredential
            case credential of
                Right (Just _) -> pure ()
                _ -> expectationFailure "Main account was removed"
    it "invalidates old sessions on account replacement before network I/O" $ withIsolatedHome do
        saveGatewayCredential fixtureCredential `shouldReturn` Right ()
        bracket Mobile.openSession Mobile.closeSession $ \session -> do
            saveGatewayCredential (fixtureCredential { gatewayAccessToken = "replacement-fixture" })
                `shouldReturn` Right ()
            Mobile.listPairings session `shouldThrow` isAccountUnavailable
    it "validates every native entry point without credentials" $
        nativeSmoke `shouldReturn` 0
    it "rejects path injection in pairing identifiers" do
        validPairingID "01234567-89ab-cdef-0123-456789abcdef" `shouldBe` True
        map validPairingID ["../runner", "", "01234567-89ab-cdef-0123-456789abcdef/relay"] `shouldBe` [False, False, False]
    it "completes invalid handles with a sanitized account error" do
        received <- newEmptyMVar
        bracket (wrapCallback $ \_ status handle _ count _ nameCount ->
            putMVar received (status, handle, count, nameCount)) freeHaskellFunPtr $ \callback -> do
                ha_mobile_pairings_list 0 callback nullPtr `shouldReturn` 0
                takeMVar received `shouldReturn` (2, 0, 0, 0)
    it "rejects oversized frames before dereferencing input" do
        bracket (wrapCallback $ \_ _ _ _ _ _ _ -> expectationFailure "Rejected call invoked callback")
            freeHaskellFunPtr $ \callback -> do
                ha_mobile_relay_send 0 nullPtr 1048577 callback nullPtr `shouldReturn` 1

fixtureCredential :: GatewayCredential
fixtureCredential = GatewayCredential "https://gateway.example" "wss://gateway.example/ws" "test-fixture"

isAccountUnavailable :: Mobile.MobileFailure -> Bool
isAccountUnavailable Mobile.AccountUnavailable = True
isAccountUnavailable _ = False

-- Sequential Hspec examples; never touch the developer's credential directory.
withIsolatedHome :: IO a -> IO a
withIsolatedHome action = bracket create removePathForcibly $ \home ->
    bracket (lookupEnv "HOME") (maybe (unsetEnv "HOME") (setEnv "HOME")) $ \_ ->
        setEnv "HOME" home >> action
  where
    create = do
        temporary <- getTemporaryDirectory
        (path, handle) <- openTempFile temporary "mobile-gateway-test"
        hClose handle
        removeFile path
        createDirectory path
        pure path
