module Agent.CLI.MacOS.AccountConnectionSpec (spec) where

import Agent.CLI.MacOS.AccountConnection
import Agent.CLI.MacOS.NativeSupervisor (runIntegrationAdmin)
import Agent.CLI.NativeRuntime
import Agent.Runtime.GatewayClient (GatewayCredential(..))
import Agent.CLI.IntegrationGateway (gatewayIntegrationMcpConfig)
import Agent.Integration.API
import Agent.Json (rawJsonFromEncoding)
import qualified Data.Aeson as Aeson
import Data.IORef
import Control.Exception.Safe (bracket)
import System.Directory (getTemporaryDirectory)
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec

-- Invalid requests are rejected before provider I/O or credential mutation.
spec :: Spec
spec = describe "native account connection validation" do
    it "dispatches email administration locally without retiring organization connections" do
        root <- getTemporaryDirectory
        bankCloses <- newIORef (0 :: Int)
        let payload = rawJsonFromEncoding (Aeson.toEncoding ([] :: [Aeson.Value]))
            bankPayload = rawJsonFromEncoding (Aeson.toEncoding ("bank" :: String))
            provider response close _ = pure (Right IntegrationRuntime
                { integrationRuntimeEndpoint = NoIntegrationEndpoint
                , integrationRuntimeAdminDefinitions = response
                , callIntegrationRuntimeAdmin = \_ _ -> pure (Right response)
                , integrationRuntimeConnections = Nothing
                , closeIntegrationRuntime = close
                })
            config = gatewayIntegrationMcpConfig GatewayCredential
                { gatewayBaseUrl = "https://gateway.example"
                , gatewayWebSocketUrl = "wss://gateway.example/ws"
                , gatewayAccessToken = "fixture-token"
                }
        bracket
            (newNativeProcessRuntimeWithOrganizationIntegrations
                (provider payload (pure ()))
                (Just (\_ -> provider bankPayload (modifyIORef' bankCloses (+ 1))))
                (unsafeEncodeUtf root))
            closeNativeProcessRuntime \process -> do
                bank <- acquireIntegrationRuntime
                    (nativeProcessIntegrationSupervisor process)
                    (OrganizationIntegrationAuthority config)
                case bank of
                    Left err -> expectationFailure (show err)
                    Right _ -> pure ()
                runIntegrationAdmin process
                    (\runtime -> pure (Right (integrationRuntimeAdminDefinitions runtime)))
                    `shouldReturn` Right payload
                readIORef bankCloses `shouldReturn` 0
        readIORef bankCloses `shouldReturn` 1

    it "rejects unsupported OAuth providers" do
        startAccountOAuth (AccountProviderRequest "unsupported")
            `shouldReturn`
                Left "OAuth account connection is not supported for this provider"

    it "rejects incomplete OpenAI and xAI challenges" do
        let missingChallenge provider = AccountOAuthPollRequest
                { oauthPollProvider = provider
                , oauthPollVerificationUrl = Nothing
                , oauthPollUserCode = Nothing
                , oauthPollDeviceAuthId = Nothing
                , oauthPollDeviceCode = Nothing
                , oauthPollIntervalSeconds = Nothing
                , oauthPollExpiresInSeconds = Nothing
                }
        pollAccountOAuth (missingChallenge "openai")
            `shouldReturn` Left "OAuth challenge is missing required fields"
        pollAccountOAuth (missingChallenge "xai")
            `shouldReturn` Left "OAuth challenge is missing required fields"

    it "rejects unsupported API-key providers and empty keys" do
        connectAccountAPIKey (AccountAPIKeyRequest "openai" "unused")
            `shouldReturn` Left "API-key connections are supported for OpenRouter"
        connectAccountAPIKey (AccountAPIKeyRequest "openrouter" "  ")
            `shouldReturn` Left "API-key connections are supported for OpenRouter"
