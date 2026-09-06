-- | Public gateway API and terminal commands.
--
-- Credential ownership, OAuth, and gateway-bound services are implemented in
-- private modules; this facade preserves the API used by CLI and native hosts.
module Agent.CLI.GatewayClient
    ( GatewayCredential(..)
    , GatewayModel(..)
    , GatewayModelCatalogResponse(..)
    , GatewayModelProtocol(..)
    , GatewayModelAccess
    , GatewayAuthorization(..)
    , GatewayAuthorizationCodeResponse(..)
    , GatewayDeviceAuthorization(..)
    , GatewayPollResult(..)
    , gatewayCredentialIdentity
    , connectGatewayBrowser
    , connectGatewayBrowserWithCancel
    , defaultGatewayBaseUrl
    , gatewayAuthorizationCodeDecoder
    , gatewayAuthorizationUrl
    , gatewayBrowserClientId
    , gatewayBrowserRedirectPath
    , gatewayPkceChallenge
    , validateGatewayAuthorizationCallback
    , validateGatewayAuthorizationCodeResponse
    , validateGatewayDeviceAuthorization
    , startGatewayAuthorization
    , startNativeGatewayAuthorization
    , pollGatewayAuthorization
    , pollNativeGatewayAuthorizationAndSave
    , pollNativeGatewayAuthorizationAndSaveWith
    , exchangeNativeGatewayAuthorizationCode
    , exchangeNativeGatewayAuthorizationCodeWith
    , saveGatewayCredential
    , removeGatewayCredential
    , removeGatewayCredentialWith
    , openGatewayAuthorizationPage
    , connectGateway
    , disconnectGateway
    , gatewayCredentialPath
    , withGatewayCredentialLock
    , withGatewayCredentialLockAt
    , withGatewayCredentialLease
    , withGatewayCredentialLeaseAt
    , withGatewayCredentialTurnLease
    , withGatewayCredentialTurnLeaseAt
    , gatewayDeviceDecoder
    , gatewayPollDecoder
    , loadGatewayCredential
    , loadGatewayCredentialAt
    , fetchGatewayModels
    , fetchGatewayUsage
    , newGatewayModelAccess
    , newGatewayModelAccessWith
    , newGatewayModelAccessWithDictation
    , newGatewayModelAccessWithUsage
    , refreshGatewayModels
    , cachedGatewayModels
    , gatewayModelIds
    , transcribeGatewayPcm
    , runGatewayCommand
    , saveGatewayCredentialAt
    , showGatewayStatus
    , validateBaseUrl
    , validateGatewayCredential
    ) where

import Agent.CLI.Gateway.Catalog
import Agent.CLI.Gateway.Credentials
import Agent.CLI.Gateway.OAuth
import Agent.CLI.Gateway.OAuth.Protocol
import Agent.CLI.Gateway.Origin (validateBaseUrl)
import Agent.CLI.Runtime.Options (GatewayCommand(..))
import Agent.Server.Client.GatewayIdentity
    ( GatewayCredential(..)
    , gatewayCredentialIdentity
    )
import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as Text
import Network.HTTP.Client.TLS (newTlsManager)

connectGateway :: Text -> IO ()
connectGateway rawBaseUrl = do
    authorization <-
        startGatewayAuthorization rawBaseUrl >>= either failText pure
    manager <- newTlsManager
    let device = authorization.authorizationDevice
    putStrLn ("Enter code " <> Text.unpack device.userCode <> " at:")
    putStrLn (Text.unpack device.verificationUri)
    opened <- openGatewayAuthorizationPage authorization
    when (not opened) $
        putStrLn "Could not open a browser automatically."
    credential <- pollUntilAuthorized manager authorization
    saveGatewayCredential credential >>= either failText pure
    putStrLn "Gateway connection saved."

showGatewayStatus :: IO ()
showGatewayStatus =
    loadGatewayCredential >>= \case
        Left err -> failText err
        Right Nothing -> putStrLn "Not connected to a gateway."
        Right (Just credential) -> do
            putStrLn ("Connected to " <> Text.unpack credential.gatewayBaseUrl)
            putStrLn ("Responses WebSocket: " <> Text.unpack credential.gatewayWebSocketUrl)

disconnectGateway :: IO ()
disconnectGateway = do
    removeGatewayCredential >>= either failText pure
    putStrLn "Gateway connection removed."

runGatewayCommand :: GatewayCommand -> IO ()
runGatewayCommand = \case
    GatewayConnect url -> connectGateway url
    GatewayStatus -> showGatewayStatus
    GatewayDisconnect -> disconnectGateway

failText :: Text -> IO a
failText = ioError . userError . Text.unpack
