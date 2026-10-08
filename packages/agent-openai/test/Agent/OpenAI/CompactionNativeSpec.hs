module Agent.OpenAI.CompactionNativeSpec (spec) where

import Agent.OpenAI.Compaction.Native
import Agent.OpenAI.Compaction.Manager
import Agent.OpenAI.Compaction.Transport
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Vector as Vector
import Data.IORef
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
    it "preserves oversized opaque history for provider validation" do
        let opaque = Aeson.object
                [ "type" .= ("future_provider_state" :: Text.Text)
                , "payload" .= Text.replicate 8000000 "x"
                ]
        case buildCompactionRequest adapter request [opaque] of
            Left err -> expectationFailure (Text.unpack err)
            Right prepared -> take 1 (adapter.requestInput prepared) `shouldBe` [opaque]
    it "submits and installs large encrypted checkpoints without modifying them" do
        let checkpoint = Aeson.object
                [ "type" .= ("compaction" :: Text.Text)
                , "encrypted_content" .= Text.replicate 11800000 "x"
                ]
            newerMessage = Aeson.object
                [ "type" .= ("message" :: Text.Text)
                , "role" .= ("user" :: Text.Text)
                , "content" .= [Aeson.object ["type" .= ("input_text" :: Text.Text), "text" .= ("Remember the new facts" :: Text.Text)]]
                ]
            history = [checkpoint, newerMessage]
            response = Aeson.object
                [ "id" .= ("response-compacted" :: Text.Text)
                , "object" .= ("response" :: Text.Text)
                , "created_at" .= (0 :: Int)
                , "status" .= ("completed" :: Text.Text)
                , "model" .= ("gpt-5.4" :: Text.Text)
                , "output" .= [checkpoint]
                ]
        installed <- newIORef []
        sent <- newIORef (0 :: Int)
        (prepared, previous) <- prepareCompaction (openAIRemoteCompactionStrategy adapter)
            100000 Nothing Nothing (adapter.withRequestInput request history) []
            (pure (CompactionSource history (writeIORef installed)))
            (\compactionRequest -> do
                take 2 (adapter.requestInput compactionRequest) `shouldBe` history
                modifyIORef' sent (+ 1)
                pure response)
        adapter.requestInput prepared `shouldBe` [checkpoint]
        previous `shouldBe` Nothing
        readIORef installed `shouldReturn` [checkpoint]
        readIORef sent `shouldReturn` 1
        -- After a normal response reports low occupancy, replaying the large
        -- checkpoint must not trigger another compaction based on wire size.
        let measured = Aeson.object
                ["usage" .= Aeson.object ["input_tokens" .= (120 :: Int), "output_tokens" .= (30 :: Int)]]
        prepareCompaction (openAIRemoteCompactionStrategy adapter)
            100000 (responseOccupancy measured) Nothing prepared []
            (pure (CompactionSource [checkpoint] (const (fail "unexpected install"))))
            (\_ -> fail "unexpected repeated compaction" :: IO Aeson.Value)
            `shouldReturn` (prepared, Nothing)
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
