module Main where

import Agent.Telegram.Client
import Agent.Telegram.Markdown
import Agent.Telegram.Progress
import Agent.Telegram.Reaction
import Agent.Telegram.Internal.Text (splitTelegramText)
import Agent.Telegram.VoicePreparation
import Agent.Telegram.Types.Wire
import Control.Concurrent.MVar
import Control.Exception.Safe (finally)
import Data.Aeson (object)
import Data.IORef
import qualified Data.Text as Text
import qualified Network.HTTP.Client as HTTP
import Test.Hspec

main :: IO ()
main = hspec do
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
