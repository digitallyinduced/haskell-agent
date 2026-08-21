module Agent.CLI.RequestSpec (spec) where

import Agent.CLI.Request (requestParams)
import Agent.CLI.Tools (webSearchTool)
import Agent.Responses.Types
import qualified Data.Aeson.KeyMap as KeyMap
import Test.Hspec

spec :: Spec
spec = describe "requestParams" do
    it "constructs a non-storing request with model, instructions, tools, and effort" do
        let params =
                requestParams
                    "test-model"
                    "test instructions"
                    [webSearchTool]
                    "high"
        params.model `shouldBe` Just "test-model"
        params.instructions `shouldBe` Just "test instructions"
        params.tools `shouldBe` Just [webSearchTool]
        params.store `shouldBe` Just False
        case params.reasoning of
            Nothing -> expectationFailure "expected reasoning configuration"
            Just reasoning -> do
                reasoning.effort `shouldBe` Just "high"
                reasoning.context `shouldBe` Nothing
                reasoning.generateSummary `shouldBe` Nothing
                reasoning.reasoningMode `shouldBe` Nothing
                reasoning.summary `shouldBe` Nothing
                reasoning.extraFields `shouldBe` KeyMap.empty
