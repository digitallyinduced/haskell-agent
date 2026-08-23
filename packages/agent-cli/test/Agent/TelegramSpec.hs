module Agent.TelegramSpec (spec) where

import Agent.Telegram
import Control.Concurrent
    ( newEmptyMVar
    , putMVar
    , takeMVar
    , threadDelay
    )
import Data.Aeson (Value, eitherDecode, encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
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
                        && Map.null state.pendingQueues
                Left _ -> False

        it "loads legacy state into keyed bindings and per-chat queues" do
            let firstKey = TelegramChatKey 123 Nothing
                secondKey = TelegramChatKey 456 (Just 7)
                firstTurn =
                    TelegramPendingTurn 10 77 firstKey "first" Nothing
                laterTurn =
                    TelegramPendingTurn 12 79 firstKey "later" Nothing
                secondTurn =
                    TelegramPendingTurn 9 76 secondKey "other" Nothing
                firstReply =
                    TelegramPendingReply 11 firstKey (Just 77) "reply"
                secondReply =
                    TelegramPendingReply 8 secondKey (Just 75) "other reply"
                decoded = eitherDecode
                    (LBS.pack
                        "{\"nextUpdateId\":12,\"bindings\":[{\
                        \\"chat\":{\"chatId\":123},\"sessionId\":\"session-1\"}],\
                        \\"pendingTurns\":[{\
                        \\"updateId\":12,\"messageId\":79,\
                        \\"chat\":{\"chatId\":123},\"text\":\"later\"},{\
                        \\"updateId\":9,\"messageId\":76,\
                        \\"chat\":{\"chatId\":456,\"messageThreadId\":7},\
                        \\"text\":\"other\"},{\
                        \\"updateId\":10,\"messageId\":77,\
                        \\"chat\":{\"chatId\":123},\"text\":\"first\"}],\
                        \\"pendingReplies\":[{\
                        \\"updateId\":11,\"chat\":{\"chatId\":123},\
                        \\"replyToMessageId\":77,\"text\":\"reply\"},{\
                        \\"updateId\":8,\
                        \\"chat\":{\"chatId\":456,\"messageThreadId\":7},\
                        \\"replyToMessageId\":75,\"text\":\"other reply\"}]}")
                    :: Either String TelegramState
            state <- decoded `shouldReturnRight`
                "legacy Telegram state should decode"
            Map.lookup firstKey state.bindings `shouldBe` Just "session-1"
            Map.keysSet state.pendingQueues
                `shouldBe` Set.fromList [firstKey, secondKey]
            nextPendingAction firstKey state
                `shouldBe` Just (RunPendingTurn firstTurn)
            nextPendingAction secondKey state
                `shouldBe` Just (DeliverReply secondReply)
            Map.lookup firstKey state.pendingQueues `shouldBe`
                Just
                    (Map.fromList
                        [ (10, RunPendingTurn firstTurn)
                        , (11, DeliverReply firstReply)
                        , (12, RunPendingTurn laterTurn)
                        ])
            Map.lookup secondKey state.pendingQueues `shouldBe`
                Just
                    (Map.fromList
                        [ (8, DeliverReply secondReply)
                        , (9, RunPendingTurn secondTurn)
                        ])

        it "preserves first-match semantics for duplicate legacy bindings" do
            let key = TelegramChatKey 123 Nothing
                decoded = eitherDecode
                    (LBS.pack
                        "{\"bindings\":[{\
                        \\"chat\":{\"chatId\":123},\"sessionId\":\"current\"},{\
                        \\"chat\":{\"chatId\":123},\"sessionId\":\"stale\"}]}")
                    :: Either String TelegramState
            state <- decoded `shouldReturnRight`
                "legacy Telegram bindings should decode"
            Map.lookup key state.bindings `shouldBe` Just "current"

        it "writes keyed state using the legacy JSON array fields" do
            let key = TelegramChatKey 123 Nothing
                turn = TelegramPendingTurn 10 77 key "hello" Nothing
                reply = TelegramPendingReply 11 key (Just 77) "reply"
                state = emptyTelegramState
                    { nextUpdateId = Just 12
                    , bindings = Map.singleton key "session-1"
                    , pendingQueues =
                        Map.singleton key $ Map.fromList
                            [ (10, RunPendingTurn turn)
                            , (11, DeliverReply reply)
                            ]
                    }
                expected = object
                    [ "nextUpdateId" .= (Just 12 :: Maybe Integer)
                    , "bindings" .=
                        [ object
                            [ "chat" .= key
                            , "sessionId" .= ("session-1" :: String)
                            ]
                        ]
                    , "pendingTurns" .= [turn]
                    , "pendingReplies" .= [reply]
                    ]
            (eitherDecode (encode state) :: Either String Value)
                `shouldBe` Right expected

        it "persists inbound work and advances the polling offset" do
            let key = TelegramChatKey 123 Nothing
                state = storeUpdateAction
                    10
                    (QueueTurn 77 key "hello" Nothing)
                    emptyTelegramState
            state.nextUpdateId `shouldBe` Just 11
            nextPendingAction key state `shouldBe`
                Just
                    (RunPendingTurn
                        (TelegramPendingTurn 10 77 key "hello" Nothing))

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
            twice.pendingQueues `shouldBe` once.pendingQueues

        it "checkpoints a voice transcript before running the agent" do
            let key = TelegramChatKey 123 Nothing
                voice = TelegramVoice "voice-file" 4 (Just "audio/ogg")
                    (Just 1024)
                pending = TelegramPendingTurn
                    10 77 key "[Voice message]" (Just voice)
                state = checkpointPendingVoiceTranscript
                    10
                    "[Voice message transcript]: hello"
                    emptyTelegramState
                        { pendingQueues =
                            Map.singleton key
                                (Map.singleton 10 (RunPendingTurn pending))
                        }
            nextPendingAction key state `shouldBe`
                Just
                    (RunPendingTurn
                        (TelegramPendingTurn
                            10
                            77
                            key
                            "[Voice message transcript]: hello"
                            Nothing))

        it "selects work in update order within a conversation" do
            let key = TelegramChatKey 123 Nothing
                newerTurn = TelegramPendingTurn 12 79 key "later" Nothing
                olderTurn = TelegramPendingTurn 10 77 key "first" Nothing
                middleReply = TelegramPendingReply 11 key (Just 77) "reply"
                state = emptyTelegramState
                    { pendingQueues =
                        Map.singleton key $ Map.fromList
                            [ (12, RunPendingTurn newerTurn)
                            , (10, RunPendingTurn olderTurn)
                            , (11, DeliverReply middleReply)
                            ]
                    }
            nextPendingAction key state
                `shouldBe` Just (RunPendingTurn olderTurn)

        it "keeps delivery ahead of later inbound work" do
            let key = TelegramChatKey 123 Nothing
                turn = TelegramPendingTurn 12 79 key "later" Nothing
                reply = TelegramPendingReply 11 key (Just 77) "reply"
                state = emptyTelegramState
                    { pendingQueues =
                        Map.singleton key $ Map.fromList
                            [ (12, RunPendingTurn turn)
                            , (11, DeliverReply reply)
                            ]
                    }
            nextPendingAction key state
                `shouldBe` Just (DeliverReply reply)

isLeft :: Either a b -> Bool
isLeft = \case
    Left _ -> True
    Right _ -> False

shouldReturnRight :: Either String a -> String -> IO a
shouldReturnRight result message = case result of
    Left err -> expectationFailure (message <> ": " <> err) >> fail message
    Right value -> pure value
