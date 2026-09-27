module InputSpec (spec) where

import Agent.Telegram.Connector
import Agent.Telegram.Connector.Input
import Agent.Telegram.Types.Wire
import qualified Agent.Json.Decode as Json
import Data.Aeson (object, (.=), encode)
import qualified Data.ByteString.Lazy as ByteString
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "Conversation input orchestration" do
    it "validates and publishes text without invoking media capabilities" do
        (caps, events) <- fixture
        processConversationInput caps (ConversationMessage message)
        readIORef events `shouldReturn` ["publish:hello"]
    it "rejects NUL, empty and oversized prepared content" do
        (caps, events) <- fixture
        mapM_ (\text -> processConversationInput caps (ConversationMessage message { messageText = Just text }))
            [" ", "\NUL", Text.replicate 16001 "x"]
        readIORef events `shouldReturn` replicate 3 "notice:invalid"
    it "does not prepare or publish duplicate voice messages" do
        (caps, events) <- fixture
        processConversationInput caps { inputAlreadyRecorded = const (pure True) }
            (ConversationMessage message { messageVoice = Just voice })
        readIORef events `shouldReturn` []
    it "transcribes before publishing, and surfaces expected preparation failures" do
        (caps, events) <- fixture
        processConversationInput caps (ConversationMessage message { messageVoice = Just voice })
        processConversationInput caps { inputTranscribe = const (pure (Left "unavailable")) }
            (ConversationMessage message { messageVoice = Just voice })
        readIORef events `shouldReturn` ["transcribe", "publish:transcript", "notice:unavailable"]
    it "prepares photos through the media capability before publication" do
        (caps, events) <- fixture
        processConversationInput caps (ConversationMessage message
            { messagePhoto = [TelegramPhotoSize "photo" 20 20 Nothing] })
        readIORef events `shouldReturn` ["media", "publish:stored"]
    it "routes cancellation and edits without submitting new turns" do
        (caps, events) <- fixture
        processConversationInput caps (ConversationMessage message { messageText = Just "/cancel" })
        processConversationInput caps (ConversationEdit message)
        readIORef events `shouldReturn` ["cancel", "edit"]

fixture :: IO (InputCapabilities, IORef [Text])
fixture = do
    events <- newIORef []
    let record value = modifyIORef' events (<> [value])
    pure (InputCapabilities
        { inputAlreadyRecorded = const (pure False)
        , inputTranscribe = \_ -> record "transcribe" >> pure (Right "transcript")
        , inputPrepareMedia = \_ _ -> record "media" >> pure (Right "stored")
        , inputPublish = \_ content -> record ("publish:" <> content)
        , inputNotice = \_ content -> record ("notice:" <> content)
        , inputInvalidText = "invalid", inputUnsupported = "unsupported"
        , inputStart = const (record "start"), inputCancel = const (record "cancel")
        , inputEdit = const (record "edit"), inputReaction = const (record "reaction")
        , inputCallback = const (record "callback")
        }, events)

voice :: TelegramVoice
voice = TelegramVoice "voice" 1 Nothing Nothing

message :: TelegramMessage
message = either (error . show) id $ Json.decodeEither telegramMessageDecoder $
    ByteString.toStrict $ encode $ object
        [ "message_id" .= (1 :: Int), "text" .= (" hello " :: Text)
        , "chat" .= object ["id" .= (42 :: Int), "type" .= ("private" :: Text)]
        , "from" .= object ["id" .= (42 :: Int), "is_bot" .= False]
        ]
