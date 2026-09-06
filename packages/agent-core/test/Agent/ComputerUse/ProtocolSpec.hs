module Agent.ComputerUse.ProtocolSpec (spec) where

import Agent.ComputerUse.Protocol
import Control.Monad (forM_)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text.Encoding as TextEncoding
import Paths_agent_core (getDataFileName)
import Test.Hspec

spec :: Spec
spec = describe "semantic computer protocol" do
    it "decodes every operation and scalar into typed requests" do
        decode requestList `shouldBe` Right ListComputerTargets
        decode requestBind `shouldBe`
            Right (BindComputerTarget "target-1" True)
        decode requestObserve `shouldBe`
            Right (ObserveComputerTarget False)
        decode requestAct `shouldBe`
            Right fixtureActRequest

    it "round-trips every request through the versioned native wire" do
        forM_ fixtureRequests \request ->
            decodeSemanticComputerWireRequest
                (encodeSemanticComputerRequest request)
                `shouldBe` Right request

    it "matches the shared protocol-v1 golden fixture" do
        fixturePath <-
            getDataFileName "data/computer-use/protocol-v1.json"
        fixtureBytes <- BS.readFile fixturePath
        case (Aeson.eitherDecodeStrict' fixtureBytes
                :: Either String Aeson.Value) of
            Left err -> expectationFailure err
            Right fixtureValue ->
                fixtureValue `shouldBe`
                    Aeson.toJSON
                        (map semanticComputerRequestWireValue fixtureRequests)

    it "derives ABI metadata from the typed request" do
        semanticComputerRequestOperation ListComputerTargets
            `shouldBe` ListComputerTargetsOperation
        semanticComputerRequestOperation (BindComputerTarget "target-1" True)
            `shouldBe` BindComputerTargetOperation
        semanticComputerRequestOperation (ObserveComputerTarget False)
            `shouldBe` ObserveOrActOnComputerTargetOperation
        semanticComputerRequestWantsScreenshot
            (ObserveComputerTarget True) `shouldBe` True

    it "rejects shape drift before approval or execution" do
        decode
            "{\"operation\":\"observe\",\"target_id\":null,\"actions\":null}"
            `shouldSatisfy` isLeft
        decode
            ( "{\"operation\":\"observe\",\"target_id\":null,\"actions\":null,"
            <> "\"include_screenshot\":false,\"x\":1}"
            )
            `shouldSatisfy` isLeft
        decode
            ( "{\"operation\":\"list_targets\",\"target_id\":null,"
            <> "\"actions\":null,\"include_screenshot\":true}"
            )
            `shouldSatisfy` isLeft
        decode
            ( "{\"operation\":\"act\",\"target_id\":null,\"actions\":[],"
            <> "\"include_screenshot\":false}"
            )
            `shouldSatisfy` isLeft
        decode
            ( "{\"operation\":\"act\",\"target_id\":null,\"actions\":["
            <> "{\"type\":\"set_value\",\"element_id\":\"field\","
            <> "\"action\":null,\"value\":null,\"text\":null}],"
            <> "\"include_screenshot\":false}"
            )
            `shouldSatisfy` isLeft
        decode
            ( "{\"operation\":\"bind\",\"target_id\":\"\",\"actions\":null,"
            <> "\"include_screenshot\":false}"
            )
            `shouldSatisfy` isLeft
        decode
            ( "{\"operation\":\"act\",\"target_id\":null,\"actions\":["
            <> "{\"type\":\"set_value\",\"element_id\":\"field\","
            <> "\"action\":null,\"value\":1e400,\"text\":null}],"
            <> "\"include_screenshot\":false}"
            )
            `shouldSatisfy` isLeft
        decode
            ( "{\"operation\":\"act\",\"target_id\":null,\"actions\":["
            <> "{\"type\":\"set_value\",\"element_id\":\"field\","
            <> "\"action\":null,\"value\":9007199254740993,\"text\":null}],"
            <> "\"include_screenshot\":false}"
            )
            `shouldSatisfy` isLeft

    it "rejects oversized, missing, duplicate, and unsupported wire input" do
        decodeSemanticComputerWireRequest
            (BS.replicate (1024 * 1024 + 1) 0x20)
            `shouldBe`
                Left "computer request exceeds the native protocol capacity"
        wireDecode
            ( "{\"operation\":\"observe\",\"target_id\":null,"
            <> "\"actions\":null,\"include_screenshot\":false}"
            )
            `shouldSatisfy` isLeft
        wireDecode
            ( "{\"protocol_version\":2,\"operation\":\"observe\","
            <> "\"target_id\":null,\"actions\":null,"
            <> "\"include_screenshot\":false}"
            )
            `shouldSatisfy` isLeft
        wireDecode
            ( "{\"protocol_version\":1,\"protocol_version\":1,"
            <> "\"operation\":\"observe\",\"target_id\":null,"
            <> "\"actions\":null,\"include_screenshot\":false}"
            )
            `shouldSatisfy` isLeft
  where
    decode :: Text -> Either Text SemanticComputerRequest
    decode = decodeSemanticComputerRequest
    wireDecode :: Text -> Either Text SemanticComputerRequest
    wireDecode =
        decodeSemanticComputerWireRequest . TextEncoding.encodeUtf8

requestList :: Text
requestList =
    "{\"operation\":\"list_targets\",\"target_id\":null,\"actions\":null,\"include_screenshot\":false}"

requestBind :: Text
requestBind =
    "{\"operation\":\"bind\",\"target_id\":\"target-1\",\"actions\":null,\"include_screenshot\":true}"

requestObserve :: Text
requestObserve =
    "{\"operation\":\"observe\",\"target_id\":null,\"actions\":null,\"include_screenshot\":false}"

requestAct :: Text
requestAct =
    "{\"operation\":\"act\",\"target_id\":null,\"actions\":[\
    \{\"type\":\"perform\",\"element_id\":\"button-1\",\"action\":\"AXPress\",\"value\":null,\"text\":null},\
    \{\"type\":\"set_value\",\"element_id\":\"field-1\",\"action\":null,\"value\":\"hello\",\"text\":null},\
    \{\"type\":\"set_value\",\"element_id\":\"slider-1\",\"action\":null,\"value\":42,\"text\":null},\
    \{\"type\":\"set_value\",\"element_id\":\"check-1\",\"action\":null,\"value\":true,\"text\":null},\
    \{\"type\":\"replace_selected_text\",\"element_id\":\"field-2\",\"action\":null,\"value\":null,\"text\":\"world\"}],\
    \\"include_screenshot\":true}"

fixtureRequests :: [SemanticComputerRequest]
fixtureRequests =
    [ ListComputerTargets
    , BindComputerTarget "target-1" True
    , ObserveComputerTarget False
    , fixtureActRequest
    ]

fixtureActRequest :: SemanticComputerRequest
fixtureActRequest =
    ActOnComputerTarget
        ( PerformComputerAction "button-1" "AXPress"
            NonEmpty.:| [ SetComputerValue "field-1" (ComputerText "hello")
                       , SetComputerValue "slider-1" (ComputerNumber 42)
                       , SetComputerValue "check-1" (ComputerBool True)
                       , ReplaceComputerSelectedText "field-2" "world"
                       ]
        )
        True

isLeft :: Either a b -> Bool
isLeft = \case
    Left _ -> True
    Right _ -> False
