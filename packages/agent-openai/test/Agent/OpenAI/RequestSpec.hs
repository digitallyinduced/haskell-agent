module Agent.OpenAI.RequestSpec (spec) where

import Agent.OpenAI.Compaction (compactionTriggerItem, userTextItem)
import Agent.OpenAI.Request (sanitizeCodexRequest)
import Agent.Responses.Types
import Test.Hspec

spec :: Spec
spec = describe "sanitizeCodexRequest" do
    it "keeps compaction_trigger items for Codex-hosted models" do
        let history = [userTextItem "hello", compactionTriggerItem]
            request = sanitizeCodexRequest defaultResponseCreateParams
                { model = Just "gpt-5.6-sol"
                , input = Just (ResponseInputItems history)
                }
        requestItems request `shouldBe` history

    it "drops compaction_trigger items for Grok models on the OpenAI transport" do
        let history = [userTextItem "hello", compactionTriggerItem]
            request = sanitizeCodexRequest defaultResponseCreateParams
                { model = Just "grok-4.6"
                , input = Just (ResponseInputItems history)
                }
        requestItems request `shouldBe` [userTextItem "hello"]
  where
    requestItems request = case request.input of
        Just (ResponseInputItems items) -> items
        _ -> []
