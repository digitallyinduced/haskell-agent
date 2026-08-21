module Agent.OpenAI.CompactionSpec (spec) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.OpenAI.CompactClient
import Agent.OpenAI.Compaction
import qualified Data.Aeson as Aeson
import Agent.Provider
import Agent.Responses.Types
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = do
    describe "buildLocalCompactedHistory" do
        it "keeps recent user texts and an assistant summary" do
            let history =
                    [ user "one"
                    , assistant "a1"
                    , user "two"
                    , assistant "a2"
                    , user "three"
                    ]
                compacted = buildLocalCompactedHistory 2 history "did stuff"
            length compacted `shouldBe` 3
            compacted `shouldSatisfy` any isSummary
            -- newest users retained
            let texts = [userOnly m | MessageItem m <- compacted, m.role == RoleUser]
            texts `shouldBe` ["two", "three"]

        it "skips prior /compact user markers" do
            let history = [user "/compact", user "keep me", assistant "x"]
                compacted = buildLocalCompactedHistory 3 history "summary"
            let texts = [userOnly m | MessageItem m <- compacted, m.role == RoleUser]
            texts `shouldBe` ["keep me"]

    describe "compactTranscriptAtLastCheckpoint" do
        it "preserves history without a checkpoint" do
            let history = [user "one", assistant "two"]
            compactTranscriptAtLastCheckpoint history `shouldBe` history

        it "keeps the latest checkpoint and following items" do
            let first = checkpoint "first"
                latest = checkpoint "latest"
                history = [user "old", first, assistant "middle", latest, user "recent"]
            compactTranscriptAtLastCheckpoint history
                `shouldBe` [latest, user "recent"]

        it "does not treat a compaction trigger as a checkpoint" do
            let trigger = KnownResponseItem ItemCompactionTrigger TaggedObject
                    { tag = "compaction_trigger"
                    , fields = KeyMap.empty
                    }
                history = [user "old", trigger, user "recent"]
            compactTranscriptAtLastCheckpoint history `shouldBe` history

    describe "isCompactSessionTurn" do
        it "recognizes compact markers" do
            isCompactSessionTurn "/compact" `shouldBe` True
            isCompactSessionTurn "/compact focus auth" `shouldBe` True
            isCompactSessionTurn "hello" `shouldBe` False

    describe "clear/new session markers" do
        it "recognizes /clear and /new as transcript resets" do
            isClearSessionTurn "/clear" `shouldBe` True
            isNewSessionTurn "/new" `shouldBe` True
            isTranscriptResetTurn "/clear" `shouldBe` True
            isTranscriptResetTurn "/new" `shouldBe` True
            isTranscriptResetTurn "/compact" `shouldBe` True
            isTranscriptResetTurn "hello" `shouldBe` False

    describe "compactConversationAt" do
        it "rejects non-OpenAI credentials before making a request" do
            let provider = TokenProvider \_ -> pure $ Right Credential
                    { accessToken = "xai-secret"
                    , accountId = "account"
                    , leaseId = Nothing
                    , provider = XAIProvider
                    }
                request = CompactRequest
                    { compactModel = "gpt-test"
                    , compactInput = []
                    , compactInstructions = Nothing
                    , compactTools = Nothing
                    , compactParallelToolCalls = False
                    , compactReasoning = Nothing
                    }
            compactConversationAt "http://127.0.0.1:1" provider request
                `shouldReturn` Left (ProviderError ApiErrorType
                    "Codex compaction requires an OpenAI credential" Nothing)

  where
    user text = MessageItem ResponseMessage
        { messageId = Nothing
        , content = MessageContentParts [InputTextPart text Nothing KeyMap.empty]
        , role = RoleUser
        , status = Nothing
        , phase = Nothing
        , extraFields = KeyMap.empty
        }
    assistant text = MessageItem ResponseMessage
        { messageId = Nothing
        , content = MessageContentParts [InputTextPart text Nothing KeyMap.empty]
        , role = RoleAssistant
        , status = Nothing
        , phase = Nothing
        , extraFields = KeyMap.empty
        }
    isSummary (MessageItem m) =
        m.role == RoleAssistant
            && case m.content of
                MessageContentParts (InputTextPart text _ _ : _) ->
                    Text.isPrefixOf summaryPrefix text
                _ -> False
    isSummary _ = False
    userOnly m = case m.content of
        MessageContentParts (InputTextPart text _ _ : _) -> text
        MessageContentText text -> text
        _ -> ""
    checkpoint name = KnownResponseItem ItemCompaction TaggedObject
        { tag = "compaction"
        , fields = KeyMap.fromList [("name", Aeson.String name)]
        }
