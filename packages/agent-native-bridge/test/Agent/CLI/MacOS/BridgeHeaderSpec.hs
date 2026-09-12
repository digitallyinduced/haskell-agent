{-# LANGUAGE ForeignFunctionInterface #-}

module Agent.CLI.MacOS.BridgeHeaderSpec (spec) where

import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.Ptr (Ptr, FunPtr, nullFunPtr, nullPtr, freeHaskellFunPtr)
import Foreign.Marshal.Utils (fillBytes)
import Control.Exception.Safe (bracket)
import Control.Concurrent.Async (mapConcurrently)
import qualified Data.ByteString as BS
import Agent.CLI.MacOS.ConnectionBridge
    (withSnapshot, ConnectionSecretCallback, connectionSecretStore)
import Agent.Integration.Connection
import Test.Hspec (Spec, describe, it, shouldReturn)

foreign import ccall "ha_image_attachment_abi_smoke"
    imageAttachmentAbiSmoke :: IO CInt

foreign import ccall "ha_syntax_abi_smoke"
    syntaxAbiSmoke :: IO CInt

foreign import ccall "ha_voice_abi_smoke"
    voiceAbiSmoke :: IO CInt

foreign import ccall "ha_gateway_abi_smoke"
    gatewayAbiSmoke :: IO CInt

foreign import ccall "ha_mcp_admin_abi_smoke"
    mcpAdminAbiSmoke :: IO CInt

foreign import ccall "ha_session_continuity_abi_smoke"
    sessionContinuityAbiSmoke :: IO CInt

foreign import ccall "ha_session_observation_validation_smoke"
    sessionObservationValidationSmoke :: IO CInt

foreign import ccall "ha_learned_skill_admin_abi_smoke"
    learnedSkillAdminAbiSmoke :: IO CInt

foreign import ccall "ha_interaction_option_abi_smoke"
    interactionOptionAbiSmoke :: IO CInt

foreign import ccall "ha_data_browser_abi_smoke"
    dataBrowserAbiSmoke :: IO CInt

foreign import ccall "ha_integration_abi_smoke"
    integrationAbiSmoke :: IO CInt

foreign import ccall "ha_connection_abi_check_snapshot"
    connectionSnapshotAbiCheck :: Ptr () -> IO CInt

foreign import ccall "wrapper" makeSecretCallback
    :: ConnectionSecretCallback -> IO (FunPtr ConnectionSecretCallback)

spec :: Spec
spec = do
    describe "native syntax highlighting ABI" do
        it "preserves UTF-8 ranges, classifies Haskell, and validates fallback inputs" do
            syntaxAbiSmoke `shouldReturn` 0
        it "supports concurrent engine-independent requests" do
            mapConcurrently (const syntaxAbiSmoke) [1 .. 8 :: Int]
                `shouldReturn` replicate 8 0
    describe "native connection secure store" do
        it "fails closed when the native secure store is absent" do
            connectionSecretStore nullFunPtr nullPtr "scope"
                `shouldReturn` Left "A native secure store is required."
        it "copies exactly the host key before releasing callback storage" do
            bracket
                (makeSecretCallback \_ _ _ pointer capacity ->
                    if capacity /= 32 then pure 1
                    else fillBytes pointer 42 32 >> pure 0)
                freeHaskellFunPtr \callback ->
                    connectionSecretStore callback nullPtr "scope"
                        `shouldReturn` Right (BS.replicate 32 42)
        it "never returns partial key bytes after a secure-store failure" do
            bracket
                (makeSecretCallback \_ _ _ pointer _ ->
                    fillBytes pointer 42 16 >> pure 1)
                freeHaskellFunPtr \callback ->
                    connectionSecretStore callback nullPtr "scope"
                        `shouldReturn` Left "The native secure store is unavailable."
    describe "native bridge struct ABI" do
        it "validates voice handles and typed audio callbacks without opening devices" do
            voiceAbiSmoke `shouldReturn` 0
        it "marshals connection fields and items in the layout consumed by C" do
            let snapshot = ConnectionSnapshot ConnectionChallenge "session"
                    "Authorization" "Enter the code" "" 2500
                    [ConnectionField "tan" "Code" ConnectionSecretField True]
                    [ConnectionItem "account" "Bank" "••1234" True]
            withSnapshot snapshot connectionSnapshotAbiCheck `shouldReturn` 0
        it "preserves the documented image struct layout and ordered buffers" do
            imageAttachmentAbiSmoke `shouldReturn` 0
        it "preserves typed gateway callbacks and synchronous validation" do
            gatewayAbiSmoke `shouldReturn` 0
        it "preserves the typed MCP argument and environment layouts" do
            mcpAdminAbiSmoke `shouldReturn` 0
        it "compiles typed learned resource callbacks and stable enum values" do
            learnedSkillAdminAbiSmoke `shouldReturn` 0
        it "preserves the documented interaction option layout" do
            interactionOptionAbiSmoke `shouldReturn` 0
        it "preserves typed data-browser callbacks and synchronous validation" do
            dataBrowserAbiSmoke `shouldReturn` 0
        it "preserves generic integration callbacks and rejects invalid inputs" do
            integrationAbiSmoke `shouldReturn` 0
    describe "native session continuity ABI" do
        it "matches callback signatures and rejects invalid UTF-8 safely" do
            sessionContinuityAbiSmoke `shouldReturn` 0
        it "validates observation inputs and joins one terminal callback on cancellation" do
            sessionObservationValidationSmoke `shouldReturn` 0
