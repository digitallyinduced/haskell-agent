module Agent.TelegramSpec (spec) where

import Agent.Telegram
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Set as Set
import Agent.Provider (Provider(..))
import Test.Hspec

spec :: Spec
spec = describe "Agent.Telegram" do
    describe "parseTelegramArgs" do
        it "runs the configured gateway by default" do
            parseTelegramArgs [] `shouldBe` Right TelegramRun
            parseTelegramArgs ["run"] `shouldBe` Right TelegramRun

        it "parses non-secret setup options" do
            parseTelegramArgs
                [ "setup"
                , "--provider", "xai"
                , "--model", "grok-4.6"
                , "--cwd", "/tmp/project"
                , "--allowed-user", "123"
                , "--yolo"
                , "--start"
                ]
                `shouldBe` Right
                    (TelegramSetup defaultTelegramSetupOptions
                        { setupProvider = Just XAIProvider
                        , setupModel = Just "grok-4.6"
                        , setupCwd = Just "/tmp/project"
                        , setupYolo = True
                        , setupAllowedUser = Just 123
                        , setupStart = True
                        })

        it "rejects tokens and malformed setup values as arguments" do
            parseTelegramArgs ["setup", "--token", "secret"]
                `shouldSatisfy` isLeft
            parseTelegramArgs ["setup", "--allowed-user", "nope"]
                `shouldSatisfy` isLeft

    describe "parseAllowedUsers" do
        it "parses and deduplicates numeric IDs" do
            parseAllowedUsers "123, 456,123"
                `shouldBe` Right (Set.fromList [123, 456])

        it "rejects an empty allowlist and malformed IDs" do
            parseAllowedUsers "" `shouldSatisfy` isLeft
            parseAllowedUsers "123,nope" `shouldSatisfy` isLeft
            parseAllowedUsers "-1" `shouldSatisfy` isLeft

    describe "splitTelegramText" do
        it "keeps messages within the requested limit" do
            splitTelegramText 4 "abcdefghij"
                `shouldBe` ["abcd", "efgh", "ij"]

        it "does not emit an empty message" do
            splitTelegramText 4 "" `shouldBe` []

    describe "durable queue state" do
        it "loads state written before pending turns were introduced" do
            let decoded = eitherDecode
                    (LBS.pack
                        "{\"nextUpdateId\":12,\"bindings\":[],\"pendingReplies\":[]}")
                    :: Either String TelegramState
            decoded `shouldSatisfy` \case
                Right state ->
                    state.nextUpdateId == Just 12
                        && null state.pendingTurns
                Left _ -> False

        it "persists inbound work and advances the polling offset" do
            let key = TelegramChatKey 123 Nothing
                state = storeUpdateAction
                    10
                    (QueueTurn 77 key "hello")
                    emptyTelegramState
            state.nextUpdateId `shouldBe` Just 11
            state.pendingTurns `shouldBe`
                [TelegramPendingTurn 10 77 key "hello"]

        it "does not enqueue a delivered update twice" do
            let key = TelegramChatKey 123 Nothing
                once = storeUpdateAction
                    10
                    (QueueTurn 77 key "hello")
                    emptyTelegramState
                twice = storeUpdateAction
                    10
                    (QueueTurn 77 key "hello")
                    once
            twice.pendingTurns `shouldBe` once.pendingTurns

        it "selects work in update order within a conversation" do
            let key = TelegramChatKey 123 Nothing
                newerTurn = TelegramPendingTurn 12 79 key "later"
                olderTurn = TelegramPendingTurn 10 77 key "first"
                middleReply = TelegramPendingReply 11 key "reply"
                state = emptyTelegramState
                    { pendingTurns = [newerTurn, olderTurn]
                    , pendingReplies = [middleReply]
                    }
            nextPendingAction key state
                `shouldBe` Just (RunPendingTurn olderTurn)

        it "keeps delivery ahead of later inbound work" do
            let key = TelegramChatKey 123 Nothing
                turn = TelegramPendingTurn 12 79 key "later"
                reply = TelegramPendingReply 11 key "reply"
                state = emptyTelegramState
                    { pendingTurns = [turn]
                    , pendingReplies = [reply]
                    }
            nextPendingAction key state
                `shouldBe` Just (DeliverReply reply)

isLeft :: Either a b -> Bool
isLeft = \case
    Left _ -> True
    Right _ -> False
