module Agent.TelegramSpec (spec) where

import Agent.Telegram
import Control.Concurrent
    ( newEmptyMVar
    , putMVar
    , takeMVar
    , threadDelay
    )
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Set as Set
import Agent.Provider (Provider(..))
import qualified System.Timeout as Timeout
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

    describe "markdownToTelegramHtml" do
        it "renders common agent Markdown and preserves line breaks" do
            markdownToTelegramHtml
                "**Bold** and *italic*\n`code` ~~gone~~ [site](https://example.com)"
                `shouldBe`
                    "<b>Bold</b> and <i>italic</i><br>\
                    \<code>code</code> <s>gone</s> \
                    \<a href=\"https://example.com\">site</a>"

        it "escapes literal HTML and link attributes" do
            markdownToTelegramHtml
                "<unsafe> & [link](https://example.com/?a=1&b=\"2\")"
                `shouldBe`
                    "&lt;unsafe&gt; &amp; \
                    \<a href=\"https://example.com/?a=1&amp;b=&quot;2&quot;\">link</a>"

        it "renders fenced code without interpreting its Markdown" do
            markdownToTelegramHtml "before\n```haskell\nx < y && **raw**\n```\nafter"
                `shouldBe`
                    "before<br><pre>x &lt; y &amp;&amp; **raw**\n</pre><br>after"

        it "leaves unmatched delimiters literal" do
            markdownToTelegramHtml "unfinished **bold and `code"
                `shouldBe` "unfinished **bold and `code"

    describe "Telegram progress lifecycle" do
        it "cancels and joins an in-flight indicator before returning" do
            started <- newEmptyMVar
            events <- newIORef ([] :: [String])
            let record event = modifyIORef' events (<> [event])
            withTelegramProgressUsing
                (do
                    record "typing-start"
                    putMVar started ()
                    threadDelay 10_000_000
                    record "typing-finish")
                (pure ())
                (takeMVar started >> record "action-finish")

            readIORef events `shouldReturn`
                ["typing-start", "action-finish"]

        it "does not swallow asynchronous cancellation" do
            result <- Timeout.timeout 50_000 $
                withTelegramProgressUsing
                    (threadDelay 10_000_000)
                    (pure ())
                    (threadDelay 10_000_000)
            result `shouldBe` Nothing

    describe "Telegram reactions and voice" do
        it "turns an inbound reaction into a durable agent message" do
            let decoded = eitherDecode
                    (LBS.pack
                        "{\"update_id\":20,\"message_reaction\":{\
                        \\"chat\":{\"id\":123,\"type\":\"private\"},\
                        \\"message_id\":77,\"user\":{\"id\":456},\
                        \\"old_reaction\":[],\
                        \\"new_reaction\":[{\"type\":\"emoji\",\
                        \\"emoji\":\"\\u2764\"}]}}")
                    :: Either String TelegramUpdate
            decoded `shouldSatisfy` \case
                Right update -> case update.updateMessageReaction of
                    Just reaction ->
                        reactionMessageText reaction
                            == "[Telegram reaction on message 77]: ❤"
                    Nothing -> False
                Left _ -> False

        it "recognizes emoji-only assistant replies as reactions" do
            telegramReactionEmoji " 👍 " `shouldBe` Just "👍"
            telegramReactionEmoji "❤️" `shouldBe` Just "❤"
            telegramReactionEmoji "Looks good 👍" `shouldBe` Nothing

        it "decodes Telegram voice metadata" do
            let decoded = eitherDecode
                    (LBS.pack
                        "{\"update_id\":21,\"message\":{\
                        \\"message_id\":78,\"from\":{\"id\":456},\
                        \\"chat\":{\"id\":123,\"type\":\"private\"},\
                        \\"voice\":{\"file_id\":\"voice-file\",\"duration\":4,\
                        \\"mime_type\":\"audio/ogg\",\"file_size\":1024}}}")
                    :: Either String TelegramUpdate
            decoded `shouldSatisfy` \case
                Right update -> case update.updateMessage >>= (.messageVoice) of
                    Just voice ->
                        voice.voiceFileId == "voice-file"
                            && voice.voiceDuration == 4
                            && voice.voiceMimeType == Just "audio/ogg"
                            && voice.voiceFileSize == Just 1024
                    Nothing -> False
                Left _ -> False

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
                    (QueueTurn 77 key "hello" Nothing)
                    emptyTelegramState
            state.nextUpdateId `shouldBe` Just 11
            state.pendingTurns `shouldBe`
                [TelegramPendingTurn 10 77 key "hello" Nothing]

        it "does not enqueue a delivered update twice" do
            let key = TelegramChatKey 123 Nothing
                once = storeUpdateAction
                    10
                    (QueueTurn 77 key "hello" Nothing)
                    emptyTelegramState
                twice = storeUpdateAction
                    10
                    (QueueTurn 77 key "hello" Nothing)
                    once
            twice.pendingTurns `shouldBe` once.pendingTurns

        it "checkpoints a voice transcript before running the agent" do
            let key = TelegramChatKey 123 Nothing
                voice = TelegramVoice "voice-file" 4 (Just "audio/ogg")
                    (Just 1024)
                pending = TelegramPendingTurn
                    10 77 key "[Voice message]" (Just voice)
                state = checkpointPendingVoiceTranscript
                    10
                    "[Voice message transcript]: hello"
                    emptyTelegramState { pendingTurns = [pending] }
            state.pendingTurns `shouldBe`
                [ TelegramPendingTurn
                    10
                    77
                    key
                    "[Voice message transcript]: hello"
                    Nothing
                ]

        it "selects work in update order within a conversation" do
            let key = TelegramChatKey 123 Nothing
                newerTurn = TelegramPendingTurn 12 79 key "later" Nothing
                olderTurn = TelegramPendingTurn 10 77 key "first" Nothing
                middleReply = TelegramPendingReply 11 key (Just 77) "reply"
                state = emptyTelegramState
                    { pendingTurns = [newerTurn, olderTurn]
                    , pendingReplies = [middleReply]
                    }
            nextPendingAction key state
                `shouldBe` Just (RunPendingTurn olderTurn)

        it "keeps delivery ahead of later inbound work" do
            let key = TelegramChatKey 123 Nothing
                turn = TelegramPendingTurn 12 79 key "later" Nothing
                reply = TelegramPendingReply 11 key (Just 77) "reply"
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
