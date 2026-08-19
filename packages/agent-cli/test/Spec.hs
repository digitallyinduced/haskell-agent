module Main (main) where

import Agent.OpenAI.Client (defaultCodexBaseUrl)
import Test.Hspec

main :: IO ()
main = hspec do
    describe "agent-openai integration" do
        it "exposes the default OpenAI Responses endpoint" do
            defaultCodexBaseUrl `shouldBe` "https://chatgpt.com/backend-api/codex"
