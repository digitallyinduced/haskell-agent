module Agent.OpenAI.CompactionEnvelopeSpec (spec) where

import Agent.OpenAI.Compaction.Envelope (nativeCompactionRequestBody)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = describe "native compaction envelope" do
    it "removes unsupported Lite tools and hosted search calls while retaining findings and client searches" do
        let function = typed "function"
            custom = typed "custom"
            clientSearch = Aeson.object ["type" .= ("tool_search" :: Text), "execution" .= ("client" :: Text)]
            serverSearch = Aeson.object ["type" .= ("tool_search" :: Text), "execution" .= ("server" :: Text)]
            finding = Aeson.object ["type" .= ("message" :: Text), "text" .= ("search findings" :: Text)]
            searchCall = typed "tool_search_call"
            request = Aeson.object
                [ "tools" .= [function, custom, clientSearch, serverSearch, typed "web_search", typed "computer"]
                , "input" .= [typed "web_search_call", typed "raw:web_search_call", finding, searchCall]
                ]
            lite = nativeCompactionRequestBody "gpt-6.1-sol" request
            standard = nativeCompactionRequestBody "gpt-5.3-codex" request
        field "tools" lite `shouldBe` Just (Aeson.toJSON [function, custom, clientSearch])
        field "input" lite `shouldBe` Just (Aeson.toJSON [finding, searchCall])
        field "tools" standard `shouldBe` field "tools" request
        field "input" standard `shouldBe` field "input" request

    it "retains opaque nested fields while stripping replay lifecycle metadata" do
        let payload = Aeson.object ["status" .= ("domain-value" :: Text), "ciphertext" .= ("opaque" :: Text)]
            item = Aeson.object
                [ "type" .= ("future_item" :: Text), "status" .= ("completed" :: Text)
                , "payload" .= payload, "content_item_kinds" .= ["local" :: Text]
                , "haskell_agent_message_metadata" .= Aeson.object ["message_id" .= ("amsg_local" :: Text)]
                , "passthrough" .= Aeson.object ["content_item_kinds" .= ["local" :: Text], "turn_id" .= ("turn" :: Text)]
                ]
            expected = Aeson.object
                [ "type" .= ("future_item" :: Text), "payload" .= payload
                , "passthrough" .= Aeson.object ["turn_id" .= ("turn" :: Text)]
                ]
            result = nativeCompactionRequestBody "gpt-6.1-sol" (Aeson.object ["input" .= [item]])
        field "input" result `shouldBe` Just (Aeson.toJSON [expected])

    it "creates a fresh streamed unstored request with dialect-specific parallelism" do
        let request = Aeson.object ["previous_response_id" .= ("old" :: Text), "prompt_cache_retention" .= ("24h" :: Text)]
            lite = nativeCompactionRequestBody "gpt-6.1-sol" request
        field "previous_response_id" lite `shouldBe` Nothing
        field "prompt_cache_retention" lite `shouldBe` Nothing
        field "stream" lite `shouldBe` Just (Aeson.Bool True)
        field "store" lite `shouldBe` Just (Aeson.Bool False)
        field "parallel_tool_calls" lite `shouldBe` Just (Aeson.Bool False)
        field "parallel_tool_calls" (nativeCompactionRequestBody "gpt-5.3-codex" request)
            `shouldBe` Just (Aeson.Bool True)

typed :: Text -> Aeson.Value
typed value = Aeson.object ["type" .= value]

field :: Aeson.Key -> Aeson.Value -> Maybe Aeson.Value
field key (Aeson.Object fields) = KeyMap.lookup key fields
field _ _ = Nothing
