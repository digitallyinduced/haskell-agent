module Agent.CLI.MacOS.AccountConnectionSpec (spec) where

import Agent.CLI.MacOS.AccountConnection
import Test.Hspec

-- Invalid requests are rejected before provider I/O or credential mutation.
spec :: Spec
spec = describe "native account connection validation" do
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
