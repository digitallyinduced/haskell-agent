module Agent.OpenAI.CompactionNativeSpec (spec) where

import Agent.OpenAI.Compaction.Native
import Agent.OpenAI.Compaction.Transport
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Vector as Vector
import Data.Either (isLeft)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "Native remote compaction" do
    it "preserves unknown nested provider payloads in the compaction input" do
        let opaque = Aeson.object
                [ "type" .= ("future_provider_state" :: Text.Text)
                , "payload" .= Aeson.object ["encrypted" .= ("opaque" :: Text.Text)]
                ]
        case buildCompactionRequest adapter request [opaque] of
            Left err -> expectationFailure (Text.unpack err)
            Right prepared -> take 1 (adapter.requestInput prepared) `shouldBe` [opaque]
    it "rejects oversized opaque history rather than silently discarding fields" do
        let opaque = Aeson.object
                [ "type" .= ("future_provider_state" :: Text.Text)
                , "payload" .= Text.replicate 8000000 "x"
                ]
        buildCompactionRequest adapter request [opaque] `shouldSatisfy` isLeft
    it "normalizes only transport-owned fields" do
        let item = Aeson.object
                [ "type" .= ("future_provider_state" :: Text.Text)
                , "status" .= ("completed" :: Text.Text)
                , "payload" .= Aeson.object ["status" .= ("preserved" :: Text.Text)]
                ]
            prepared = nativeCompactionRequestBody "gpt-5.4"
                (adapter.withRequestInput request [item])
            expected = Aeson.object
                [ "type" .= ("future_provider_state" :: Text.Text)
                , "payload" .= Aeson.object ["status" .= ("preserved" :: Text.Text)]
                ]
        adapter.requestInput prepared `shouldBe` [expected]
    it "reads measured occupancy without relying on application response types" do
        responseOccupancy (Aeson.object
            ["usage" .= Aeson.object ["input_tokens" .= (120 :: Int), "output_tokens" .= (30 :: Int)]])
            `shouldBe` Just 150

request :: Aeson.Value
request = Aeson.object ["model" .= ("gpt-5.4" :: Text.Text), "input" .= ([] :: [Aeson.Value])]

-- Generic JSON fixtures intentionally preserve arbitrary provider fields.
adapter :: NativeRequestAdapter Aeson.Value Aeson.Value
adapter = NativeRequestAdapter
    { requestModel = const "gpt-5.4"
    , requestInput = \case
        Aeson.Object object -> case KeyMap.lookup "input" object of
            Just (Aeson.Array items) -> Vector.toList items
            _ -> []
        _ -> []
    , withRequestInput = \value items -> case value of
        Aeson.Object object -> Aeson.Object (KeyMap.insert "input" (Aeson.toJSON items) object)
        other -> other
    }
