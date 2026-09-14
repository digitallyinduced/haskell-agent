module Agent.OpenAI.RequestIdentitySpec (spec) where

import Agent.OpenAI.RequestIdentity
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text.Encoding as Text
import Test.Hspec

spec :: Spec
spec = describe "Agent.OpenAI.RequestIdentity" do
    describe "codexAttributionHeaders" do
        it "sends the canonical Codex identity headers" do
            let headers = codexAttributionHeaders sampleIdentity
            lookup "originator" headers `shouldBe` Just "haskell-agent"
            lookup "session-id" headers `shouldBe` Just "2026-09-14-67ed6793"
            lookup "thread-id" headers `shouldBe` Just "2026-09-14-67ed6793"
            lookup "x-client-request-id" headers
                `shouldBe` Just "2026-09-14-67ed6793"
            lookup "x-codex-window-id" headers
                `shouldBe` Just "2026-09-14-67ed6793:2"
            map fst headers `shouldBe`
                [ "originator"
                , "session-id"
                , "thread-id"
                , "x-client-request-id"
                , "x-codex-window-id"
                , "x-codex-turn-metadata"
                ]

        it "repeats the turn metadata record verbatim in the header" do
            lookup "x-codex-turn-metadata" (codexAttributionHeaders sampleIdentity)
                `shouldBe` Just (Text.encodeUtf8 (codexTurnMetadataJson sampleIdentity))

    describe "codexTurnMetadataJson" do
        it "encodes every attribution field" do
            Aeson.decodeStrict (Text.encodeUtf8 (codexTurnMetadataJson sampleIdentity))
                `shouldBe` Just (Aeson.object
                    [ "session_id" .= ("2026-09-14-67ed6793" :: String)
                    , "thread_id" .= ("2026-09-14-67ed6793" :: String)
                    , "turn_id" .= ("0199-turn" :: String)
                    , "window_id" .= ("2026-09-14-67ed6793:2" :: String)
                    , "window_number" .= (2 :: Int)
                    , "context_window_id" .= ("0199-window" :: String)
                    , "request_kind" .= ("turn" :: String)
                    , "turn_started_at_unix_ms" .= (1757851200123 :: Int)
                    ])

        it "labels compaction requests" do
            let identity = sampleIdentity { requestKind = CodexCompactionRequest }
            (Aeson.decodeStrict (Text.encodeUtf8 (codexTurnMetadataJson identity))
                >>= lookupField "request_kind")
                `shouldBe` Just (Aeson.String "compaction")

    describe "codexClientMetadata" do
        it "mirrors the identity as client metadata fields" do
            let metadata = codexClientMetadata sampleIdentity
            KeyMap.lookup "session_id" metadata
                `shouldBe` Just (Aeson.String "2026-09-14-67ed6793")
            KeyMap.lookup "thread_id" metadata
                `shouldBe` Just (Aeson.String "2026-09-14-67ed6793")
            KeyMap.lookup "turn_id" metadata
                `shouldBe` Just (Aeson.String "0199-turn")
            KeyMap.lookup "x-codex-window-id" metadata
                `shouldBe` Just (Aeson.String "2026-09-14-67ed6793:2")
            KeyMap.lookup "x-codex-turn-metadata" metadata
                `shouldBe` Just (Aeson.String (codexTurnMetadataJson sampleIdentity))

    describe "asciiJsonText" do
        it "keeps printable ASCII as is" do
            asciiJsonText (Aeson.object ["k" .= ("plain ~ text" :: String)])
                `shouldBe` "{\"k\":\"plain ~ text\"}"

        it "escapes characters above printable ASCII" do
            asciiJsonText (Aeson.object ["k" .= ("caf\233 \127 \x1F600" :: String)])
                `shouldBe` "{\"k\":\"caf\\u00e9 \\u007f \\ud83d\\ude00\"}"

    describe "isHeaderSafeIdentifier" do
        it "accepts visible ASCII identifiers only" do
            isHeaderSafeIdentifier "2026-09-14-67ed6793" `shouldBe` True
            isHeaderSafeIdentifier "" `shouldBe` False
            isHeaderSafeIdentifier "has space" `shouldBe` False
            isHeaderSafeIdentifier "caf\233" `shouldBe` False

lookupField :: KeyMap.Key -> Aeson.Value -> Maybe Aeson.Value
lookupField name = \case
    Aeson.Object object -> KeyMap.lookup name object
    _ -> Nothing

sampleIdentity :: CodexRequestIdentity
sampleIdentity = CodexRequestIdentity
    { sessionId = "2026-09-14-67ed6793"
    , threadId = "2026-09-14-67ed6793"
    , turnId = "0199-turn"
    , windowNumber = 2
    , contextWindowId = "0199-window"
    , requestKind = CodexTurnRequest
    , turnStartedAtUnixMs = 1757851200123
    }
