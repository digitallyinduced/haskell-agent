module Main where

import Agent.Telegram.Client
import Agent.Telegram.Markdown
import Agent.Telegram.Progress
import Agent.Telegram.Presentation
import Agent.Telegram.Types.State (TelegramChatKey (..))
import Agent.Telegram.Reaction
import Agent.Telegram.Internal.Text (splitTelegramText)
import Agent.Telegram.VoicePreparation
import Agent.Telegram.Types.Wire
import Control.Concurrent.MVar
import Control.Exception.Safe (finally)
import Data.Aeson (object, (.=))
import Data.IORef
import qualified Data.Text as Text
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.Internal as HTTPInternal
import Network.HTTP.Types (status200, http11)
import Test.Hspec

main :: IO ()
main = hspec do
    describe "Shared conversation style" do
        it "retains the standalone conversational guidance" do
            telegramConversationStyle `shouldBe`
                "Keep messages concise and conversational; avoid terminal-style verbosity unless the user asks for detail. Follow the language and style of the conversation."
        it "does not assume application delivery capabilities or coding tools" do
            mapM_ (\instruction -> telegramConversationStyle `shouldSatisfy` (not . Text.isInfixOf instruction))
                ["live Telegram draft", "commentary", "create_agent_session", "reaction emoji"]
    describe "Pure response preparation" do
        it "preserves topic and reply addressing independently of presentation" do
            object (messageAddressFields (TelegramChatKey 42 (Just 7)) (Just 8))
                `shouldBe` object
                    [ "chat_id" .= (42 :: Integer)
                    , "message_thread_id" .= (7 :: Integer)
                    , "reply_parameters" .= object
                        [ "message_id" .= (8 :: Integer)
                        , "allow_sending_without_reply" .= True
                        ]
                    ]
            object (messageAddressFields (TelegramChatKey 42 Nothing) Nothing)
                `shouldBe` object ["chat_id" .= (42 :: Integer)]
        it "preserves plain, HTML, and rich message wire fields" do
            object (textPresentationFields PlainText "**Hello** <input>")
                `shouldBe` object ["text" .= ("**Hello** <input>" :: Text.Text)]
            object (textPresentationFields HtmlText "**Hello** <input>")
                `shouldBe` object
                    [ "text" .= ("<b>Hello</b> &lt;input&gt;" :: Text.Text)
                    , "parse_mode" .= ("HTML" :: Text.Text)
                    ]
            object (textPresentationFields RichText "**Hello**")
                `shouldBe` object ["rich_message" .= object ["html" .= ("<b>Hello</b>" :: Text.Text)]]
        it "preserves keyboard rows and explicit keyboard removal" do
            inlineKeyboardMarkup [[("Approve", "approve:1")], [("Reject", "reject:1")]]
                `shouldBe` object ["inline_keyboard" .=
                    [[object ["text" .= ("Approve" :: Text.Text), "callback_data" .= ("approve:1" :: Text.Text)]]
                    ,[object ["text" .= ("Reject" :: Text.Text), "callback_data" .= ("reject:1" :: Text.Text)]]]]
            inlineKeyboardMarkup [] `shouldBe` object ["inline_keyboard" .= ([] :: [[Int]])]
        it "selects plain text before sending formatting-expanded content" do
            let content = "| " <> Text.replicate 100 "x" <> " | B |\n| --- | --- |\n"
                    <> Text.replicate 50 "| a | b |\n"
            telegramRenderedLength content `shouldSatisfy` (> 2000)
            boundedTextPresentationFields 2000 content `shouldBe` textPresentationFields PlainText content
        it "uses scalar budgets without discarding supplementary characters" do
            let content = Text.replicate 2000 "😀"
            boundedTextPresentationFields 2000 content `shouldBe` textPresentationFields HtmlText content
            boundedTextPresentationFields 1999 content `shouldBe` textPresentationFields PlainText content
    describe "Single-attempt Telegram transport" do
        it "rejects download traversal and encoded separators" do
            map validTelegramFilePath ["../file", "/file", "a/%2fsecret", "https://other/file"]
                `shouldBe` replicate 4 False
            validTelegramFilePath "voice/file_1.oga" `shouldBe` True
        it "does not repeat uncertain sends and redacts credentials" do
            manager <- HTTP.newManager HTTP.defaultManagerSettings
            attempts <- newIORef (0 :: Int)
            redirects <- newIORef (-1 :: Int)
            let client = TelegramClient "123:private-test-token" manager
                send request = do
                    writeIORef redirects request.redirectCount
                    modifyIORef' attempts (+ 1)
                    fail "failed URL /bot123:private-test-token/sendMessage"
            result <- telegramRequestOnceWith send client "sendMessage" (object []) 1
            readIORef attempts `shouldReturn` 1
            readIORef redirects `shouldReturn` 0
            case result of
                Left failure -> do
                    failure.telegramErrorCode `shouldBe` Nothing
                    failure.telegramErrorMessage `shouldSatisfy`
                        (not . Text.isInfixOf "private-test-token")
                Right _ -> expectationFailure "Expected an uncertain failure"
        it "classifies malformed and incomplete success envelopes as uncertain without retrying" do
            manager <- HTTP.newManager HTTP.defaultManagerSettings
            let client = TelegramClient "123:private-test-token" manager
            mapM_ (\body -> do
                attempts <- newIORef (0 :: Int)
                let send request = do
                        modifyIORef' attempts (+ 1)
                        pure (HTTPInternal.Response status200 http11 [] body
                            (HTTP.createCookieJar []) (HTTPInternal.ResponseClose (pure ())) request [])
                result <- telegramRequestOnceWith send client "sendMessage" (object []) 1
                readIORef attempts `shouldReturn` 1
                case result of
                    Left failure -> do
                        failure.telegramErrorCode `shouldBe` Nothing
                        failure.telegramErrorRetryable `shouldBe` False
                    Right _ -> expectationFailure "Expected an uncertain response")
                ["not json", "{}", "{\"ok\":true}", "{\"ok\":true,\"result\":null}"]
    describe "Reusable presentation" do
        it "retains inbound reaction and removal context" do
            let reaction = TelegramMessageReaction (TelegramChat 1 "private") 42 Nothing []
                    [TelegramReactionType "emoji" (Just "👍") Nothing]
            reactionMessageText reaction `shouldBe` "[Telegram reaction on message 42]: 👍"
            reactionMessageText reaction { messageReactionNew = [] }
                `shouldBe` "[Telegram reaction removed from message 42]"
        it "segments text without discarding supplementary Unicode characters" do
            let text = Text.replicate 5000 "😀"
            Text.concat (splitTelegramText 4096 text) `shouldBe` text
        it "escapes unsafe HTML while preserving Markdown emphasis" do
            markdownToTelegramHtml "**Hello** <script>" `shouldBe` "<b>Hello</b> &lt;script&gt;"
        it "joins progress children when the action completes" do
            entered <- newEmptyMVar
            released <- newEmptyMVar
            blocked <- newEmptyMVar
            let typing = (putMVar entered () >> takeMVar blocked)
                    `finally` putMVar released ()
            withTelegramProgressUsing typing (pure ()) (takeMVar entered)
            takeMVar released `shouldReturn` ()
    describe "Injected voice transcription" do
        it "rejects oversized metadata before acquisition" do
            let voice = TelegramVoice "file" 601 Nothing Nothing
            withTelegramVoiceTranscript voice (expectationFailure "acquired")
                (const (pure ())) (const (pure "text"))
                `shouldThrow` anyIOException
        it "releases acquired content on transcription failure" do
            released <- newIORef False
            let voice = TelegramVoice "file" 1 Nothing (Just 10)
            withTelegramVoiceTranscript voice (pure ())
                (\() -> writeIORef released True) (\() -> fail "unavailable")
                `shouldThrow` anyIOException
            readIORef released `shouldReturn` True
        it "rejects empty transcription and trims valid transcripts" do
            let voice = TelegramVoice "file" 1 Nothing Nothing
                run text = withTelegramVoiceTranscript voice (pure ())
                    (const (pure ())) (const (pure text))
            run "  " `shouldThrow` anyIOException
            run " Hallo \n" `shouldReturn` "Hallo"
