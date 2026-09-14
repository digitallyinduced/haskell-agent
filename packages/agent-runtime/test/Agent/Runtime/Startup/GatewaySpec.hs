module Agent.Runtime.Startup.GatewaySpec (spec) where

import Agent.Provider (Provider(..))
import Agent.Runtime.Models (ModelOption(..), ModelTarget(..), rawModelOption)
import Agent.Runtime.Startup.Gateway
import Test.Hspec

spec :: Spec
spec = describe "startup gateway selection" do
    it "rejects a requested alias outside the admitted catalog" do
        selectGatewayModelOption offered (Just "private") Nothing []
            `shouldBe` Left "Model private is not offered by the organization gateway."
    it "rejects an explicit alias routed through another provider" do
        selectGatewayModelOption offered (Just "grok") (Just OpenAIProvider) []
            `shouldBe` Left "The selected gateway model does not use the requested provider."
    it "uses explicit selection before saved hints" do
        selectGatewayModelOption offered (Just "openai") Nothing [grok.modelTarget]
            `shouldBe` Right openai
    it "treats a legacy saved provider as a hint, not routing authority" do
        let legacy = grok.modelTarget{targetProvider = OpenAIProvider}
        selectGatewayModelOption offered Nothing (Just XAIProvider) [legacy]
            `shouldBe` Right grok
    it "skips unavailable and provider-incompatible saved hints" do
        selectGatewayModelOption offered Nothing (Just OpenAIProvider)
            [(rawModelOption OpenAIProvider "removed").modelTarget, grok.modelTarget]
            `shouldBe` Right openai
    it "preserves hint ordering among available aliases" do
        selectGatewayModelOption offered Nothing Nothing [grok.modelTarget, openai.modelTarget]
            `shouldBe` Right grok
    it "does not fall back to an unrequested provider" do
        selectGatewayModelOption offered Nothing (Just GeminiProvider) []
            `shouldBe` Left "The organization gateway does not offer any models for the requested provider."
    it "fails closed on an empty gateway catalog" do
        selectGatewayModelOption [] Nothing Nothing []
            `shouldBe` Left "The organization gateway does not offer any models."
  where
    openai = rawModelOption OpenAIProvider "openai"
    grok = rawModelOption XAIProvider "grok"
    offered = [openai, grok]
