module Agent.ComputerUse.SemanticSpec (spec) where

import Agent.ComputerUse.Accessibility
import Agent.ComputerUse.Protocol
import Agent.ComputerUse.Semantic
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import Data.Either (isLeft, isRight)
import Data.List.NonEmpty (NonEmpty(..))
import Test.Hspec

spec :: Spec
spec = describe "transport-independent semantic responses" do
    it "accepts a target list without image or accessibility data" do
        result <- validate ListComputerTargets emptyResponse
        result `shouldSatisfy` isRight
    it "rejects non-object result metadata" do
        result <- validate ListComputerTargets emptyResponse
            { semanticResultBytes = "[]" }
        result `shouldSatisfy` isLeft
    it "rejects embedded screenshot data in nested metadata" do
        result <- validate ListComputerTargets emptyResponse
            { semanticResultBytes = "{\"nested\":[\"data:image/png;base64,AAAA\"]}" }
        result `shouldSatisfy` isLeft
    it "rejects accessibility data attached to a target list" do
        result <- validate ListComputerTargets emptyResponse
            { semanticAccessibilityBytes = "{}" }
        result `shouldSatisfy` isLeft
    it "rejects an unrequested screenshot" do
        result <- validate (ObserveComputerTarget False) emptyResponse
            { semanticImageBytes = "invalid", semanticImageFormat = 1 }
        result `shouldSatisfy` isLeft
    it "rejects malformed requested images" do
        result <- validate (ObserveComputerTarget True) emptyResponse
            { semanticImageBytes = "invalid", semanticImageFormat = 1 }
        result `shouldSatisfy` isLeft
    it "rejects oversized channels before JSON decoding" do
        result <- validate ListComputerTargets emptyResponse
            { semanticResultBytes = BS.replicate (1024 * 1024 + 1) 32 }
        result `shouldSatisfy` isLeft
    it "marks delivered input as unverified without fresh evidence" do
        result <- validate actionRequest emptyResponse
        case result of
            Right (response, _) -> case response.semanticResultValue of
                Aeson.Object object -> KeyMap.lookup "verdict" object
                    `shouldBe` Just (Aeson.toJSON (unverifiedComputerUseVerdict False))
                _ -> expectationFailure "Expected object result"
            Left err -> expectationFailure (show err)
    it "rejects claims of fresh evidence when all evidence is absent" do
        result <- validate actionRequest emptyResponse
            { semanticResultBytes =
                "{\"verdict\":{\"effect\":\"unverifiable\",\"decision\":\"inspect_fresh_state\",\"fresh_observation\":true,\"hint\":\"fresh\"}}" }
        result `shouldSatisfy` isLeft
  where
    validate request = validateSemanticComputerResponse request initialAccessibilityDeltaState
    emptyResponse = SemanticComputerResponse "{}" BS.empty BS.empty 0
    actionRequest = ActOnComputerTarget
        (PerformComputerAction "element" "AXPress" :| []) False
