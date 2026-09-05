{-# LANGUAGE ForeignFunctionInterface #-}

-- | Gateway account endpoints. Credential-transition callbacks stay inside
-- the gateway client's lease, including exception containment after commit.
module Agent.CLI.MacOS.GatewayBridge (invokeGatewayCallbackOnce) where

import Agent.CLI.GatewayClient
    ( GatewayCredential(..)
    , GatewayDeviceAuthorization(..)
    , GatewayPollResult(..)
    , exchangeNativeGatewayAuthorizationCodeWith
    , loadGatewayCredential
    , pollNativeGatewayAuthorizationAndSaveWith
    , removeGatewayCredentialWith
    , startNativeGatewayAuthorization
    , withGatewayCredentialLease
    )
import Agent.CLI.MacOS.Marshalling (anyNonEmptyNull, decodeInput, withText)
import Control.Concurrent (forkIO)
import Control.Exception.Safe (tryAny)
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)
import Foreign (FunPtr, Ptr, nullFunPtr, nullPtr)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CSize(..))

type GatewayStatusCallback =
    Ptr () -> CInt
    -> CString -> CSize
    -> CString -> CSize
    -> IO ()

type GatewayConnectStartCallback =
    Ptr () -> CInt
    -> CString -> CSize
    -> CString -> CSize
    -> CString -> CSize
    -> CString -> CSize
    -> CInt -> CInt
    -> CString -> CSize
    -> IO ()

type GatewayPollCallback =
    Ptr () -> CInt -> CInt -> CString -> CSize -> IO ()

type GatewayResultCallback =
    Ptr () -> CInt -> CString -> CSize -> IO ()

foreign import ccall "dynamic"
    invokeGatewayStatusCallback
        :: FunPtr GatewayStatusCallback -> GatewayStatusCallback

foreign import ccall "dynamic"
    invokeGatewayConnectStartCallback
        :: FunPtr GatewayConnectStartCallback -> GatewayConnectStartCallback

foreign import ccall "dynamic"
    invokeGatewayPollCallback
        :: FunPtr GatewayPollCallback -> GatewayPollCallback

foreign import ccall "dynamic"
    invokeGatewayResultCallback
        :: FunPtr GatewayResultCallback -> GatewayResultCallback

foreign export ccall ha_gateway_status
    :: FunPtr GatewayStatusCallback -> Ptr () -> IO CInt

foreign export ccall ha_gateway_connect_start
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr GatewayConnectStartCallback -> Ptr () -> IO CInt

foreign export ccall ha_gateway_connect_poll
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr GatewayPollCallback -> Ptr () -> IO CInt

foreign export ccall ha_gateway_connect_exchange
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize
    -> FunPtr GatewayResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_gateway_disconnect
    :: FunPtr GatewayResultCallback -> Ptr () -> IO CInt

ha_gateway_status
    :: FunPtr GatewayStatusCallback -> Ptr () -> IO CInt
ha_gateway_status callback context
    | callback == nullFunPtr = pure 1
    | otherwise = do
        _ <- forkIO do
            tryAny
                (withGatewayCredentialLease $
                    loadGatewayCredential >>= \case
                        Left err ->
                            invokeGatewayStatusError callback context err
                        Right Nothing ->
                            invokeGatewayStatusCallback callback context 1
                                nullPtr 0 nullPtr 0
                        Right (Just credential) ->
                            withText credential.gatewayBaseUrl $
                                \baseUrl baseUrlLength ->
                                    invokeGatewayStatusCallback callback context 0
                                        baseUrl baseUrlLength nullPtr 0)
                >>= \case
                Left exception ->
                    invokeGatewayStatusError callback context
                        (Text.pack (show exception))
                Right () -> pure ()
        pure 0

ha_gateway_connect_start
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr GatewayConnectStartCallback -> Ptr () -> IO CInt
ha_gateway_connect_start
    baseUrlBytes (CSize baseUrlLength)
    clientNameBytes (CSize clientNameLength)
    callback context
    | callback == nullFunPtr = pure 1
    | anyNonEmptyNull
        [ (baseUrlBytes, baseUrlLength)
        , (clientNameBytes, clientNameLength)
        ] = pure 2
    | otherwise = do
        baseUrl <- decodeInput baseUrlBytes baseUrlLength
        clientName <- decodeInput clientNameBytes clientNameLength
        _ <- forkIO do
            tryAny
                (startNativeGatewayAuthorization baseUrl clientName)
                >>= \case
                Left exception ->
                    invokeGatewayConnectStartError callback context
                        (Text.pack (show exception))
                Right (Left err) ->
                    invokeGatewayConnectStartError callback context err
                Right (Right device) ->
                    withGatewayDeviceStrings device $
                        invokeGatewayConnectStartCallback callback context 0
        pure 0

ha_gateway_connect_poll
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr GatewayPollCallback -> Ptr () -> IO CInt
ha_gateway_connect_poll
    baseUrlBytes (CSize baseUrlLength)
    deviceCodeBytes (CSize deviceCodeLength)
    callback context
    | callback == nullFunPtr = pure 1
    | anyNonEmptyNull
        [ (baseUrlBytes, baseUrlLength)
        , (deviceCodeBytes, deviceCodeLength)
        ] = pure 2
    | otherwise = do
        baseUrl <- decodeInput baseUrlBytes baseUrlLength
        deviceCode <- decodeInput deviceCodeBytes deviceCodeLength
        _ <- forkIO do
            tryAny
                (pollNativeGatewayAuthorizationAndSaveWith
                    baseUrl
                    deviceCode
                    (invokeGatewayCallbackOnce
                        . invokeGatewayPollResult callback context))
                >>= \case
                    Left exception ->
                        invokeGatewayPollError callback context
                            (Text.pack (show exception))
                    Right (Left err) ->
                        invokeGatewayPollError callback context err
                    Right (Right (GatewayAuthorized _ _)) -> pure ()
                    Right (Right result) ->
                        invokeGatewayPollResult callback context result
        pure 0

ha_gateway_connect_exchange
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize
    -> FunPtr GatewayResultCallback -> Ptr () -> IO CInt
ha_gateway_connect_exchange
    baseUrlBytes (CSize baseUrlLength)
    clientIdBytes (CSize clientIdLength)
    codeBytes (CSize codeLength)
    verifierBytes (CSize verifierLength)
    redirectUriBytes (CSize redirectUriLength)
    callback context
    | callback == nullFunPtr = pure 1
    | anyNonEmptyNull
        [ (baseUrlBytes, baseUrlLength)
        , (clientIdBytes, clientIdLength)
        , (codeBytes, codeLength)
        , (verifierBytes, verifierLength)
        , (redirectUriBytes, redirectUriLength)
        ] = pure 2
    | otherwise = do
        baseUrl <- decodeInput baseUrlBytes baseUrlLength
        clientId <- decodeInput clientIdBytes clientIdLength
        code <- decodeInput codeBytes codeLength
        verifier <- decodeInput verifierBytes verifierLength
        redirectUri <- decodeInput redirectUriBytes redirectUriLength
        _ <- forkIO do
            tryAny
                (exchangeNativeGatewayAuthorizationCodeWith
                    baseUrl
                    clientId
                    code
                    verifier
                    redirectUri
                    (invokeGatewayCallbackOnce $
                        invokeGatewayResultCallback callback context
                            0 nullPtr 0))
                >>= \case
                    Left exception ->
                        invokeGatewayResultError callback context
                            (Text.pack (show exception))
                    Right (Left err) ->
                        invokeGatewayResultError callback context err
                    Right (Right ()) -> pure ()
        pure 0

ha_gateway_disconnect
    :: FunPtr GatewayResultCallback -> Ptr () -> IO CInt
ha_gateway_disconnect callback context
    | callback == nullFunPtr = pure 1
    | otherwise = do
        _ <- forkIO do
            tryAny
                (removeGatewayCredentialWith
                    (invokeGatewayCallbackOnce $
                        invokeGatewayResultCallback callback context
                            0 nullPtr 0)) >>= \case
                Left exception ->
                    invokeGatewayResultError callback context
                        (Text.pack (show exception))
                Right (Left err) ->
                    invokeGatewayResultError callback context err
                Right (Right ()) -> pure ()
        pure 0

invokeGatewayStatusError
    :: FunPtr GatewayStatusCallback -> Ptr () -> Text -> IO ()
invokeGatewayStatusError callback context err =
    withText err $ \errorPtr errorLength ->
        invokeGatewayStatusCallback callback context (-1)
            nullPtr 0 errorPtr errorLength

invokeGatewayConnectStartError
    :: FunPtr GatewayConnectStartCallback -> Ptr () -> Text -> IO ()
invokeGatewayConnectStartError callback context err =
    withText err $ \errorPtr errorLength ->
        invokeGatewayConnectStartCallback callback context (-1)
            nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
            0 0 errorPtr errorLength

withGatewayDeviceStrings
    :: GatewayDeviceAuthorization
    -> (CString -> CSize -> CString -> CSize
        -> CString -> CSize -> CString -> CSize
        -> CInt -> CInt -> CString -> CSize -> IO a)
    -> IO a
withGatewayDeviceStrings device action =
    withText device.userCode $ \userCode userCodeLength ->
    withText device.verificationUri $ \verificationUri verificationUriLength ->
    withText device.verificationUriComplete $
        \verificationComplete verificationCompleteLength ->
    withText device.deviceCode $ \deviceCode deviceCodeLength ->
        action
            userCode userCodeLength
            verificationUri verificationUriLength
            verificationComplete verificationCompleteLength
            deviceCode deviceCodeLength
            (fromIntegral device.pollIntervalSeconds)
            (fromIntegral device.expiresInSeconds)
            nullPtr 0

invokeGatewayPollResult
    :: FunPtr GatewayPollCallback -> Ptr () -> GatewayPollResult -> IO ()
invokeGatewayPollResult callback context = \case
    GatewayAuthorized _ _ ->
        invokeGatewayPollCallback callback context 0 0 nullPtr 0
    GatewayAuthorizationPending retryInterval ->
        invokeGatewayPollCallback callback context 1
            (fromIntegral (fromMaybe 0 retryInterval)) nullPtr 0
    GatewaySlowDown retryInterval ->
        invokeGatewayPollCallback callback context 2
            (fromIntegral (fromMaybe 0 retryInterval)) nullPtr 0
    GatewayAccessDenied ->
        invokeGatewayPollError callback context
            "Gateway authorization was denied."
    GatewayExpired ->
        invokeGatewayPollError callback context
            "Gateway authorization expired."
    GatewayPollFailed code ->
        invokeGatewayPollError callback context
            ("Gateway authorization failed: " <> code)

invokeGatewayPollError
    :: FunPtr GatewayPollCallback -> Ptr () -> Text -> IO ()
invokeGatewayPollError callback context err =
    withText err $ \errorPtr errorLength ->
        invokeGatewayPollCallback callback context (-1) 0 errorPtr errorLength

invokeGatewayResultError
    :: FunPtr GatewayResultCallback -> Ptr () -> Text -> IO ()
invokeGatewayResultError callback context err =
    withText err $ \errorPtr errorLength ->
        invokeGatewayResultCallback callback context (-1) errorPtr errorLength

-- Credential transition callbacks are terminal. Contain a host exception
-- after the durable mutation so it cannot be misreported as a failed
-- transition or retried with a second terminal callback.
invokeGatewayCallbackOnce :: IO () -> IO ()
invokeGatewayCallbackOnce callback =
    void (tryAny callback)
