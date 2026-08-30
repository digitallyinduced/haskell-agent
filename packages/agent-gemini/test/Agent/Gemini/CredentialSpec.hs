module Agent.Gemini.CredentialSpec (spec) where

import Agent.Gemini.Credential
import Agent.Provider (Provider(..))
import Agent.Provider (Credential(..))
import Test.Hspec

spec :: Spec
spec = describe "Gemini credentials" do
    it "creates a static API-key credential" do
        let credential = credentialFromApiKey "secret"
        let Credential{accessToken, accountId, provider} = credential
        provider `shouldBe` GeminiProvider
        accessToken `shouldBe` "secret"
        accountId `shouldBe` "gemini"
