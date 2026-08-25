module Agent.TelegramSpec (spec) where

import Agent.Telegram
import Agent.OsPath (unsafeToFilePath)
import qualified Agent.Telegram.Bridge as Bridge
import Agent.Telegram.Types
    ( TelegramApprovalMode(..)
    , defaultTelegramWorkerCount
    , TelegramFileMedia(..)
    , TelegramMedia(..)
    , TelegramMediaKind(..)
    , TelegramCallbackBinding(..)
    , TelegramDeadLetter(..)
    , TelegramPendingCallback(..)
    , TelegramPendingMediaTurn(..)
    , TelegramRetryMetadata(..)
    )
import Data.List (sort)
import Control.Concurrent
    ( newEmptyMVar
    , putMVar
    , takeMVar
    , threadDelay
    )
import Control.Exception.Safe (finally)
import Data.Aeson (Value, eitherDecode, encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.IORef
    ( atomicModifyIORef'
    , modifyIORef'
    , newIORef
    , readIORef
    )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Agent.Provider (Provider(..))
import System.Directory (listDirectory)
import System.IO.Temp (withSystemTempDirectory)
import System.OsPath (unsafeEncodeUtf)
import qualified System.Timeout as Timeout
import Test.Hspec

spec :: Spec
spec = describe "Agent.Telegram" do
    describe "telegramAgentPrompt" do
        it "injects Telegram streaming and brevity guidance" do
            let prompt = telegramAgentPrompt "Inspect the failing tests"
            prompt `shouldSatisfy`
                Text.isInfixOf "shown to the user as a live Telegram draft"
            prompt `shouldSatisfy`
                Text.isInfixOf "Keep messages concise and conversational"
            prompt `shouldSatisfy`
                Text.isPrefixOf "Inspect the failing tests"

    describe "telegramActivityDraftHtml" do
        it "shows escaped reasoning summaries and streamed answer text" do
            Bridge.telegramActivityDraftHtml
                "Writing reply…"
                "Checking <files>"
                "Found & fixed\nDone"
                `shouldBe`
                "<tg-thinking>Checking &lt;files&gt;</tg-thinking>\
                \<p>Found &amp; fixed<br>Done</p>"

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
                , "--all-group-messages"
                , "--workers", "12"
                , "--yolo"
                , "--start"
                ]
                `shouldBe` Right
                    (TelegramSetup defaultTelegramSetupOptions
                        { setupProvider = Just XAIProvider
                        , setupModel = Just "grok-4.6"
                        , setupCwd = Just "/tmp/project"
                        , setupApprovalMode = TelegramApprovalYolo
                        , setupAllowedUsers = [123]
                        , setupRespondToAllGroupMessages = True
                        , setupWorkerCount = 12
                        , setupStart = True
                        })

        it "rejects tokens and malformed setup values as arguments" do
            parseTelegramArgs ["setup", "--token", "secret"]
                `shouldSatisfy` isLeft
            parseTelegramArgs ["setup", "--allowed-user", "nope"]
                `shouldSatisfy` isLeft
            parseTelegramArgs ["setup", "--workers", "0"]
                `shouldSatisfy` isLeft
            parseTelegramArgs ["setup", "--workers", "65"]
                `shouldSatisfy` isLeft

    describe "parseAllowedUsers" do
        it "parses and deduplicates numeric IDs" do
            parseAllowedUsers "123, 456,123"
                `shouldBe` Right (Set.fromList [123, 456])

        it "rejects an empty allowlist and malformed IDs" do
            parseAllowedUsers "" `shouldSatisfy` isLeft
            parseAllowedUsers "123,nope" `shouldSatisfy` isLeft
            parseAllowedUsers "-1" `shouldSatisfy` isLeft

    describe "Telegram config migration" do
        it "maps legacy yolo booleans onto explicit approval modes" do
            let decode yolo = eitherDecode
                    (encode (object
                        [ "provider" .= ("xai" :: String)
                        , "cwd" .= ("/tmp" :: String)
                        , "allowedUsers" .= ([123] :: [Integer])
                        , "yolo" .= yolo
                        ]))
                    :: Either String TelegramConfig
            fmap (.telegramApprovalMode) (decode False)
                `shouldBe` Right TelegramApprovalPrompt
            fmap (.telegramApprovalMode) (decode True)
                `shouldBe` Right TelegramApprovalYolo

        it "defaults old configs to eight workers and validates explicit values" do
            let base workers = object $
                    [ "provider" .= ("xai" :: String)
                    , "cwd" .= ("/tmp" :: String)
                    , "allowedUsers" .= ([123] :: [Integer])
                    ] <> maybe [] (\value -> ["workers" .= value]) workers
                decode :: Maybe Int -> Either String TelegramConfig
                decode workers =
                    eitherDecode (encode (base workers))
            fmap (.telegramWorkerCount) (decode Nothing)
                `shouldBe` Right defaultTelegramWorkerCount
            fmap (.telegramWorkerCount) (decode (Just (16 :: Int)))
                `shouldBe` Right 16
            decode (Just (0 :: Int)) `shouldSatisfy` isLeft
            decode (Just (65 :: Int)) `shouldSatisfy` isLeft

    describe "Telegram media downloads" do
        it "downloads concurrently with a bound and preserves attachment order" $
            withSystemTempDirectory "telegram-media-" \directory -> do
                active <- newIORef (0 :: Int)
                maximumActive <- newIORef (0 :: Int)
                let attachments =
                        zipWith testMedia
                            [ TelegramMediaPhoto
                            , TelegramMediaDocument
                            , TelegramMediaVideo
                            , TelegramMediaAudio
                            , TelegramMediaAnimation
                            , TelegramMediaSticker
                            ]
                            [1 :: Int ..]
                    download remote local = do
                        current <- atomicModifyIORef' active \count ->
                            let next = count + 1 in (next, next)
                        atomicModifyIORef' maximumActive \seen ->
                            (max seen current, ())
                        let finish =
                                atomicModifyIORef' active
                                    (\count -> (count - 1, ()))
                        (do
                            threadDelay
                                ((7 - read (dropWhile (not . (`elem` ['0'..'9'])) remote))
                                    * 10_000)
                            writeFile (unsafeToFilePath local) remote
                            pure local)
                            `finally` finish
                results <- downloadTelegramMediaAttachmentsWith
                    (pure . Text.unpack)
                    download
                    (unsafeEncodeUtf directory)
                    42
                    attachments
                map fst results `shouldBe` map (.telegramMediaKind) attachments
                observed <- readIORef maximumActive
                observed `shouldSatisfy` (\value -> value > 1 && value <= 4)

        it "removes completed and partial targets when one download fails" $
            withSystemTempDirectory "telegram-media-failure-" \directory -> do
                let attachments =
                        zipWith testMedia
                            [ TelegramMediaPhoto
                            , TelegramMediaDocument
                            , TelegramMediaVideo
                            ]
                            [1 :: Int ..]
                    download remote local = do
                        writeFile (unsafeToFilePath local) remote
                        if remote == "file-2"
                            then fail "download failed"
                            else threadDelay 50_000 >> pure local
                downloadTelegramMediaAttachmentsWith
                    (pure . Text.unpack)
                    download
                    (unsafeEncodeUtf directory)
                    43
                    attachments
                    `shouldThrow` anyException
                listDirectory directory `shouldReturn` []

    describe "Telegram bridge request batches" do
        it "admits JSON requests once and dispatches them with a bound" do
            dispatched <- newIORef []
            active <- newIORef (0 :: Int)
            maximumActive <- newIORef (0 :: Int)
            let files =
                    [ "request-6.json"
                    , "ignored.tmp"
                    , "request-2.json"
                    , "request-5.json"
                    , "request-1.json"
                    , "request-4.json"
                    , "request-3.json"
                    ]
                decode name = pure (Just name)
                dispatch name = do
                    current <- atomicModifyIORef' active \count ->
                        let next = count + 1 in (next, next)
                    atomicModifyIORef' maximumActive \value ->
                        (max value current, ())
                    (threadDelay 20_000
                        >> modifyIORef' dispatched (name :))
                        `finally`
                            atomicModifyIORef' active
                                (\count -> (count - 1, ()))
            seen <-
                Bridge.processBridgeRequestBatch Set.empty files decode dispatch
            _ <- Bridge.processBridgeRequestBatch seen files decode dispatch
            completed <- readIORef dispatched
            sort completed `shouldBe`
                sort (filter (Text.isSuffixOf ".json" . Text.pack) files)
            observed <- readIORef maximumActive
            observed `shouldSatisfy` (\value -> value > 1 && value <= 4)

        it "does not retry JSON files that fail to decode" do
            decoded <- newIORef []
            let decode name = do
                    modifyIORef' decoded (name :)
                    pure Nothing
                dispatch _ =
                    fail "undecodable requests must not be dispatched"
            seen <-
                Bridge.processBridgeRequestBatch
                    Set.empty
                    ["bad.json", "ignored.tmp"]
                    decode
                    dispatch
            seen `shouldBe` Set.fromList ["bad.json"]
            _ <-
                Bridge.processBridgeRequestBatch
                    seen
                    ["bad.json"]
                    decode
                    dispatch
            readIORef decoded `shouldReturn` ["bad.json"]

    describe "splitTelegramText" do
        it "keeps messages within the requested limit" do
            splitTelegramText 4 "abcdefghij"
                `shouldBe` ["abcd", "efgh", "ij"]

        it "does not split a message at the rendered limit" do
            splitTelegramText 4096 (Text.replicate 4096 "a")
                `shouldBe` [Text.replicate 4096 "a"]

        it "does not count HTML escaping toward the rendered limit" do
            splitTelegramText 4096 (Text.replicate 4096 "<")
                `shouldBe` [Text.replicate 4096 "<"]

        it "splits only when escaped text exceeds the rendered limit" do
            splitTelegramText 4096 (Text.replicate 4097 "<")
                `shouldBe`
                    [ Text.replicate 4096 "<"
                    , "<"
                    ]

        it "does not count link markup toward the rendered limit" do
            let link = "[site](https://example.com/"
                    <> Text.replicate 4096 "a"
                    <> ")"
            splitTelegramText 4 link `shouldBe` [link]

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

        it "renders ATX headings with native bold entities" do
            markdownToTelegramHtml
                "# Summary\n#### Details with *emphasis*\nnot# a heading"
                `shouldBe`
                    "<b>Summary</b><br>\
                    \<b>Details with <i>emphasis</i></b><br>\
                    \not# a heading"

        it "renders Markdown tables as aligned preformatted text" do
            markdownToTelegramHtml
                "| Metric | Before | Current | Change |\n\
                \|:---|---:|---:|:---:|\n\
                \| Clicks | 2,026 | 3,487 | **+72%** |\n\
                \| Views & visits | 77 | 169 | +119% |"
                `shouldBe`
                    "<pre>Metric         | Before | Current | Change\n\
                    \---------------+--------+---------+-------\n\
                    \Clicks         |  2,026 |   3,487 |  +72% \n\
                    \Views &amp; visits |     77 |     169 | +119% </pre>"

        it "does not mistake ordinary pipe-separated text for a table" do
            markdownToTelegramHtml "one | two\nstill | text"
                `shouldBe` "one | two<br>still | text"

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

        it "routes allowed callback queries onto the durable callback queue" do
            let bot = TelegramUser
                    { userId = 999
                    , userIsBot = True
                    , userFirstName = Just "Harness"
                    , userLastName = Nothing
                    , userUsername = Just "HarnessBot"
                    }
                decoded = eitherDecode
                    (LBS.pack
                        "{\"update_id\":24,\"callback_query\":{\
                        \\"id\":\"callback-1\",\"from\":{\"id\":456},\
                        \\"data\":\"ha:request:0\",\
                        \\"message\":{\"message_id\":88,\
                        \\"chat\":{\"id\":123,\"type\":\"private\"}}}}")
                    :: Either String TelegramUpdate
            update <- decoded `shouldReturnRight`
                "Telegram callback update should decode"
            classifyTelegramUpdate bot (Set.singleton 456) update
                `shouldBe`
                    QueueCallback TelegramPendingCallback
                        { pendingCallbackUpdateId = 24
                        , pendingCallbackQueryId = "callback-1"
                        , pendingCallbackUserId = 456
                        , pendingCallbackChat =
                            Just (TelegramChatKey 123 Nothing)
                        , pendingCallbackMessageId = Just 88
                        , pendingCallbackData = "ha:request:0"
                        }

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

    describe "Telegram media messages" do
        let bot = TelegramUser
                { userId = 999
                , userIsBot = True
                , userFirstName = Just "Harness"
                , userLastName = Nothing
                , userUsername = Just "HarnessBot"
                }
            allowedUsers = Set.singleton 456
            classify bytes = do
                update <- (eitherDecode (LBS.pack bytes)
                    :: Either String TelegramUpdate)
                    `shouldReturnRight` "Telegram update should decode"
                pure (classifyTelegramUpdate bot allowedUsers update)

        it "queues private photo captions as managed media turns" do
            action <- classify
                "{\"update_id\":31,\"message\":{\
                \\"message_id\":90,\"from\":{\"id\":456},\
                \\"chat\":{\"id\":123,\"type\":\"private\"},\
                \\"caption\":\"summarize this\",\
                \\"photo\":[{\
                \\"file_id\":\"small\",\"width\":10,\"height\":10},{\
                \\"file_id\":\"large\",\"width\":20,\"height\":20,\
                \\"file_size\":1024}]}}"
            action `shouldBe`
                QueueMediaTurn
                    TelegramPendingMediaTurn
                        { pendingMediaUpdateId = 31
                        , pendingMediaMessageId = 90
                        , pendingMediaChat = TelegramChatKey 123 Nothing
                        , pendingMediaUserId = 456
                        , pendingMediaText = "summarize this"
                        , pendingMediaAttachments =
                            [ TelegramMedia
                                { telegramMediaKind = TelegramMediaPhoto
                                , telegramMediaFile =
                                    Just TelegramFileMedia
                                        { fileMediaFileId = "large"
                                        , fileMediaName = Nothing
                                        , fileMediaMimeType = Just "image/jpeg"
                                        , fileMediaFileSize = Just 1024
                                        , fileMediaDuration = Nothing
                                        }
                                , telegramMediaDescription = "[Photo]"
                                }
                            ]
                        , pendingMediaEdited = False
                        , pendingMediaGroupId = Nothing
                        }

        it "routes edited text through replaceable managed-turn work" do
            action <- classify
                "{\"update_id\":33,\"edited_message\":{\
                \\"message_id\":90,\"from\":{\"id\":456},\
                \\"chat\":{\"id\":123,\"type\":\"private\"},\
                \\"text\":\"corrected text\"}}"
            action `shouldBe`
                QueueMediaTurn TelegramPendingMediaTurn
                    { pendingMediaUpdateId = 33
                    , pendingMediaMessageId = 90
                    , pendingMediaChat = TelegramChatKey 123 Nothing
                    , pendingMediaUserId = 456
                    , pendingMediaText = "corrected text"
                    , pendingMediaAttachments = []
                    , pendingMediaEdited = True
                    , pendingMediaGroupId = Nothing
                    }

        it "queues group media replies as attributed managed media turns" do
            action <- classify
                "{\"update_id\":32,\"message\":{\
                \\"message_id\":91,\"from\":{\"id\":456,\"first_name\":\"Marc\"},\
                \\"chat\":{\"id\":-1001,\"type\":\"group\"},\
                \\"document\":{\"file_id\":\"doc-file\",\"file_name\":\"report.pdf\",\
                \\"mime_type\":\"application/pdf\",\"file_size\":2048},\
                \\"reply_to_message\":{\"message_id\":77,\
                \\"from\":{\"id\":999,\"is_bot\":true},\
                \\"chat\":{\"id\":-1001,\"type\":\"group\"}}}}"
            action `shouldBe`
                QueueMediaTurn
                    TelegramPendingMediaTurn
                        { pendingMediaUpdateId = 32
                        , pendingMediaMessageId = 91
                        , pendingMediaChat = TelegramChatKey (-1001) Nothing
                        , pendingMediaUserId = 456
                        , pendingMediaText =
                            "[Telegram group message from Marc, user 456]\n\
                            \[Document: report.pdf]"
                        , pendingMediaAttachments =
                            [ TelegramMedia
                                { telegramMediaKind = TelegramMediaDocument
                                , telegramMediaFile =
                                    Just TelegramFileMedia
                                        { fileMediaFileId = "doc-file"
                                        , fileMediaName = Just "report.pdf"
                                        , fileMediaMimeType = Just "application/pdf"
                                        , fileMediaFileSize = Just 2048
                                        , fileMediaDuration = Nothing
                                        }
                                , telegramMediaDescription = "[Document: report.pdf]"
                                }
                            ]
                        , pendingMediaEdited = False
                        , pendingMediaGroupId = Nothing
                        }

    describe "Telegram group chats" do
        let bot = TelegramUser
                { userId = 999
                , userIsBot = True
                , userFirstName = Just "Harness"
                , userLastName = Nothing
                , userUsername = Just "HarnessBot"
                }
            allowedUsers = Set.singleton 456
            classify bytes = do
                update <- (eitherDecode (LBS.pack bytes)
                    :: Either String TelegramUpdate)
                    `shouldReturnRight` "Telegram update should decode"
                pure (classifyTelegramUpdate bot allowedUsers update)
            classifyAll bytes = do
                update <- (eitherDecode (LBS.pack bytes)
                    :: Either String TelegramUpdate)
                    `shouldReturnRight` "Telegram update should decode"
                pure
                    (classifyTelegramUpdateWithMode
                        bot allowedUsers True update)

        it "routes an allowed mention into the shared group session" do
            action <- classify
                "{\"update_id\":22,\"message\":{\
                \\"message_id\":80,\
                \\"from\":{\"id\":456,\"first_name\":\"Marc\",\
                \\"username\":\"marc\"},\
                \\"chat\":{\"id\":-1001,\"type\":\"group\"},\
                \\"text\":\"@HarnessBot please summarize\"}}"
            action `shouldBe`
                QueueTurn
                    80
                    (TelegramChatKey (-1001) Nothing)
                    "[Telegram group message from Marc, @marc, user 456]\n\
                    \please summarize"
                    Nothing

        it "keeps forum topics as separate conversations" do
            action <- classify
                "{\"update_id\":23,\"message\":{\
                \\"message_id\":81,\"message_thread_id\":7,\
                \\"from\":{\"id\":456,\"first_name\":\"Marc\"},\
                \\"chat\":{\"id\":-1002,\"type\":\"supergroup\"},\
                \\"text\":\"@harnessbot check this\"}}"
            action `shouldBe`
                QueueTurn
                    81
                    (TelegramChatKey (-1002) (Just 7))
                    "[Telegram group message from Marc, user 456]\ncheck this"
                    Nothing

        it "accepts commands addressed to this bot without changing them" do
            action <- classify
                "{\"update_id\":24,\"message\":{\
                \\"message_id\":82,\"from\":{\"id\":456},\
                \\"chat\":{\"id\":-1001,\"type\":\"group\"},\
                \\"text\":\"/new@HarnessBot\"}}"
            action `shouldBe`
                QueueTurn
                    82
                    (TelegramChatKey (-1001) Nothing)
                    "/new@HarnessBot"
                    Nothing

        it "accepts text and voice replies to the bot" do
            textAction <- classify
                "{\"update_id\":25,\"message\":{\
                \\"message_id\":83,\
                \\"from\":{\"id\":456,\"first_name\":\"Marc\"},\
                \\"chat\":{\"id\":-1001,\"type\":\"group\"},\
                \\"text\":\"continue\",\
                \\"reply_to_message\":{\"message_id\":70,\
                \\"from\":{\"id\":999,\"is_bot\":true,\
                \\"username\":\"HarnessBot\"},\
                \\"chat\":{\"id\":-1001,\"type\":\"group\"}}}}"
            textAction `shouldBe`
                QueueTurn
                    83
                    (TelegramChatKey (-1001) Nothing)
                    "[Telegram group message from Marc, user 456]\ncontinue"
                    Nothing

            commandAction <- classify
                "{\"update_id\":26,\"message\":{\
                \\"message_id\":84,\"from\":{\"id\":456},\
                \\"chat\":{\"id\":-1001,\"type\":\"group\"},\
                \\"text\":\"/new\",\
                \\"reply_to_message\":{\"message_id\":70,\
                \\"from\":{\"id\":999,\"is_bot\":true},\
                \\"chat\":{\"id\":-1001,\"type\":\"group\"}}}}"
            commandAction `shouldBe`
                QueueTurn
                    84
                    (TelegramChatKey (-1001) Nothing)
                    "/new"
                    Nothing

            voiceAction <- classify
                "{\"update_id\":27,\"message\":{\
                \\"message_id\":85,\"message_thread_id\":7,\
                \\"from\":{\"id\":456,\"first_name\":\"Marc\"},\
                \\"chat\":{\"id\":-1002,\"type\":\"supergroup\"},\
                \\"voice\":{\"file_id\":\"voice-file\",\"duration\":4},\
                \\"reply_to_message\":{\"message_id\":71,\
                \\"from\":{\"id\":999,\"is_bot\":true,\
                \\"username\":\"HarnessBot\"},\
                \\"chat\":{\"id\":-1002,\"type\":\"supergroup\"}}}}"
            voiceAction `shouldBe`
                QueueTurn
                    85
                    (TelegramChatKey (-1002) (Just 7))
                    "[Telegram group message from Marc, user 456]\n\
                    \[Voice message]"
                    (Just (TelegramVoice "voice-file" 4 Nothing Nothing))

        it "ignores ambient group traffic, other bots' commands, and blocked users" do
            ambient <- classify
                "{\"update_id\":27,\"message\":{\
                \\"message_id\":85,\"from\":{\"id\":456},\
                \\"chat\":{\"id\":-1001,\"type\":\"group\"},\
                \\"text\":\"hello everyone\"}}"
            ambient `shouldBe` IgnoreUpdate

            otherBot <- classify
                "{\"update_id\":28,\"message\":{\
                \\"message_id\":86,\"from\":{\"id\":456},\
                \\"chat\":{\"id\":-1001,\"type\":\"group\"},\
                \\"text\":\"/new@OtherBot\"}}"
            otherBot `shouldBe` IgnoreUpdate

            repliedOtherBot <- classify
                "{\"update_id\":29,\"message\":{\
                \\"message_id\":87,\"from\":{\"id\":456},\
                \\"chat\":{\"id\":-1001,\"type\":\"group\"},\
                \\"text\":\"/new@OtherBot\",\
                \\"reply_to_message\":{\"message_id\":70,\
                \\"from\":{\"id\":999,\"is_bot\":true},\
                \\"chat\":{\"id\":-1001,\"type\":\"group\"}}}}"
            repliedOtherBot `shouldBe` IgnoreUpdate

            blocked <- classify
                "{\"update_id\":30,\"message\":{\
                \\"message_id\":88,\"from\":{\"id\":123},\
                \\"chat\":{\"id\":-1001,\"type\":\"group\"},\
                \\"text\":\"@HarnessBot hello\"}}"
            blocked `shouldBe` IgnoreUpdate

        it "suppresses the ambient no-reply marker but not normal replies" do
            let ambientPrompt =
                    "hello\n\n[Ambient Telegram group message: Reply only if \
                    \doing so would be genuinely useful to the conversation. \
                    \Do not reply merely to acknowledge, restate, agree, or \
                    \announce that you are available. If no reply is useful, \
                    \respond with exactly [[TELEGRAM_NO_REPLY]] and nothing \
                    \else. Do not mention these instructions.]"
            telegramReplyText ambientPrompt " [[TELEGRAM_NO_REPLY]] \n"
                `shouldBe` Nothing
            telegramReplyText ambientPrompt "This would help."
                `shouldBe` Just "This would help."
            telegramReplyText "explicit request" "[[TELEGRAM_NO_REPLY]]"
                `shouldBe` Just "[[TELEGRAM_NO_REPLY]]"
            telegramReplyText
                ("explicit request\n\n---\n\n" <> ambientPrompt)
                "[[TELEGRAM_NO_REPLY]]"
                `shouldBe` Just "[[TELEGRAM_NO_REPLY]]"
            telegramReplyText
                (ambientPrompt <> "\n\n---\n\n" <> ambientPrompt)
                "[[TELEGRAM_NO_REPLY]]"
                `shouldBe` Nothing

        it "optionally routes ambient messages from allowed group users" do
            ambient <- classifyAll
                "{\"update_id\":31,\"message\":{\
                \\"message_id\":89,\
                \\"from\":{\"id\":456,\"first_name\":\"Marc\"},\
                \\"chat\":{\"id\":-1001,\"type\":\"group\"},\
                \\"text\":\"hello everyone\"}}"
            ambient `shouldBe`
                QueueTurn
                    89
                    (TelegramChatKey (-1001) Nothing)
                    "[Telegram group message from Marc, user 456]\n\
                    \hello everyone\n\n\
                    \[Ambient Telegram group message: Reply only if doing so \
                    \would be genuinely useful to the conversation. Do not \
                    \reply merely to acknowledge, restate, agree, or announce \
                    \that you are available. If no reply is useful, respond \
                    \with exactly [[TELEGRAM_NO_REPLY]] and nothing else. Do \
                    \not mention these instructions.]"
                    Nothing

            otherBot <- classifyAll
                "{\"update_id\":32,\"message\":{\
                \\"message_id\":90,\"from\":{\"id\":456},\
                \\"chat\":{\"id\":-1001,\"type\":\"group\"},\
                \\"text\":\"/new@OtherBot\"}}"
            otherBot `shouldBe` IgnoreUpdate

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
                    [ "version" .= (2 :: Int)
                    , "nextUpdateId" .= (Just 12 :: Maybe Integer)
                    , "bindings" .=
                        [ object
                            [ "chat" .= key
                            , "sessionId" .= ("session-1" :: String)
                            ]
                        ]
                    , "pendingTurns" .= [turn]
                    , "pendingReplies" .= [reply]
                    , "pendingMediaTurns" .= ([] :: [TelegramPendingMediaTurn])
                    , "pendingCallbacks" .= ([] :: [TelegramPendingCallback])
                    , "callbackBindings" .= ([] :: [TelegramCallbackBinding])
                    , "retryMetadata" .=
                        ([] :: [(Text.Text, TelegramRetryMetadata)])
                    , "deliveryCheckpoints" .=
                        ([] :: [(Text.Text, Int)])
                    , "deadLetters" .= ([] :: [TelegramDeadLetter])
                    , "outboundMessages" .= ([] :: [Value])
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

        it "coalesces adjacent text bursts into one managed turn" do
            let key = TelegramChatKey 123 Nothing
                first = storeUpdateAction
                    10
                    (QueueTurn 77 key "first" Nothing)
                    emptyTelegramState
                second = storeUpdateAction
                    11
                    (QueueTurn 78 key "second" Nothing)
                    first
            nextPendingAction key second `shouldBe`
                Just
                    (RunPendingMediaTurn TelegramPendingMediaTurn
                        { pendingMediaUpdateId = 11
                        , pendingMediaMessageId = 78
                        , pendingMediaChat = key
                        , pendingMediaUserId = 0
                        , pendingMediaText = "first\n\n---\n\nsecond"
                        , pendingMediaAttachments = []
                        , pendingMediaEdited = False
                        , pendingMediaGroupId = Nothing
                        })

        it "persists callback, retry, and chunk-delivery state" do
            let key = TelegramChatKey 123 Nothing
                callback = TelegramPendingCallback
                    20 "callback-1" 456 (Just key) (Just 80) "ha:request:0"
                retry = TelegramRetryMetadata 2 Nothing (Just "temporary")
                state = emptyTelegramState
                    { pendingCallbacks = Map.singleton 20 callback
                    , retryMetadata = Map.singleton "turn" retry
                    , deliveryCheckpoints = Map.singleton "reply" 2
                    }
            (eitherDecode (encode state) :: Either String TelegramState)
                `shouldBe` Right state

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

testMedia :: TelegramMediaKind -> Int -> TelegramMedia
testMedia kind index = TelegramMedia
    { telegramMediaKind = kind
    , telegramMediaFile =
        Just TelegramFileMedia
            { fileMediaFileId = "file-" <> Text.pack (show index)
            , fileMediaName = Just ("attachment-" <> Text.pack (show index))
            , fileMediaMimeType = Just "application/octet-stream"
            , fileMediaFileSize = Just 10
            , fileMediaDuration = Nothing
            }
    , telegramMediaDescription = ""
    }
