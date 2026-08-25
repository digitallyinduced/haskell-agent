-- | Telegram Bot API transport with bounded, status-aware retries.
module Agent.Telegram.Client
    ( TelegramRequestError(..)
    , telegramRequest
    , telegramRequestWith
    , decodeTelegramResponse
    , getUpdates
    , getTelegramBot
    , getTelegramFilePath
    , downloadTelegramFile
    , sendTypingAction
    , sendThinkingDraft
    , sendStreamingDraft
    , setMessageReaction
    , sendRichMessage
    , sendMessageWithKeyboard
    , editMessageText
    , answerCallbackQuery
    , sendTelegramDocument
    , sendTelegramPhoto
    , sendTelegramVoice
    , redactToken
    ) where

import Agent.Telegram.Markdown (markdownToTelegramHtml)
import Agent.Telegram.Types
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (displayException, tryAny)
import Control.Monad (when)
import Control.Retry
    ( fullJitterBackoff
    , limitRetries
    , retrying
    )
import Data.Aeson
    ( FromJSON(..)
    , Value
    , eitherDecode
    , encode
    , object
    , withObject
    , (.:?)
    , (.=)
    )
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Http
import Network.HTTP.Types.Status (statusCode)
import System.Directory (doesFileExist)
import System.FilePath (takeFileName)
import System.OsPath (OsPath)
import Agent.OsPath (unsafeToFilePath)
import System.Posix.Files (setFileMode)

data TelegramRequestError = TelegramRequestError
    { telegramErrorMessage :: !Text
    , telegramErrorCode :: !(Maybe Int)
    , telegramRetryAfter :: !(Maybe Int)
    , telegramErrorRetryable :: !Bool
    } deriving (Eq, Show)

data TelegramFile = TelegramFile
    { telegramFilePath :: !(Maybe Text)
    }

instance FromJSON TelegramFile where
    parseJSON = withObject "TelegramFile" \o ->
        TelegramFile <$> o .:? "file_path"

telegramRequest
    :: TelegramClient
    -> String
    -> Value
    -> Int
    -> IO (Either Text LBS.ByteString)
telegramRequest client =
    telegramRequestWith (Http.httpLbs `flip` client.clientManager) client

telegramRequestWith
    :: (Http.Request -> IO (Http.Response LBS.ByteString))
    -> TelegramClient
    -> String
    -> Value
    -> Int
    -> IO (Either Text LBS.ByteString)
telegramRequestWith send client method body timeoutSeconds =
    runRetrying client do
        base <- Http.parseRequest $
            "https://api.telegram.org/bot"
                <> Text.unpack client.clientToken
                <> "/"
                <> method
        let configured = base
                { Http.method = "POST"
                , Http.requestHeaders =
                    [("Content-Type", "application/json")]
                , Http.requestBody = Http.RequestBodyLBS (encode body)
                , Http.responseTimeout =
                    Http.responseTimeoutMicro
                        (timeoutSeconds * 1_000_000)
                }
        send configured

runRetrying
    :: TelegramClient
    -> IO (Http.Response LBS.ByteString)
    -> IO (Either Text LBS.ByteString)
runRetrying client request = do
    result <- retrying
        (fullJitterBackoff 250_000 <> limitRetries 4)
        shouldRetry
        (const attempt)
    pure $ case result of
        Left err -> Left (renderRequestError client err)
        Right body -> Right body
  where
    attempt =
        tryAny request >>= \case
            Left err ->
                pure $ Left TelegramRequestError
                    { telegramErrorMessage =
                        "Telegram request failed: "
                            <> Text.pack (displayException err)
                    , telegramErrorCode = Nothing
                    , telegramRetryAfter = Nothing
                    , telegramErrorRetryable = True
                    }
            Right response -> do
                let code = statusCode (Http.responseStatus response)
                    body = Http.responseBody response
                pure $
                    if code >= 200 && code < 300
                        then case responseFailure body of
                            Nothing -> Right body
                            Just err -> Left err
                        else Left $
                            fromMaybe
                                TelegramRequestError
                                    { telegramErrorMessage =
                                        "Telegram HTTP error "
                                            <> Text.pack (show code)
                                    , telegramErrorCode = Just code
                                    , telegramRetryAfter = Nothing
                                    , telegramErrorRetryable =
                                        code == 429 || code >= 500
                                    }
                                (responseFailure body)

    shouldRetry _ = \case
        Right _ -> pure False
        Left err
            | not err.telegramErrorRetryable -> pure False
            | otherwise -> do
                maybe (pure ()) (\seconds ->
                    threadDelay (max 1 seconds * 1_000_000))
                    err.telegramRetryAfter
                pure True

responseFailure :: LBS.ByteString -> Maybe TelegramRequestError
responseFailure bytes =
    case eitherDecode bytes :: Either String (TelegramResponse Value) of
        Left _ -> Nothing
        Right envelope
            | envelope.responseOk -> Nothing
            | otherwise ->
                Just TelegramRequestError
                    { telegramErrorMessage =
                        "Telegram API error: "
                            <> fromMaybe
                                "unknown error"
                                envelope.responseDescription
                    , telegramErrorCode = envelope.responseErrorCode
                    , telegramRetryAfter =
                        envelope.responseParameters >>= (.responseRetryAfter)
                    , telegramErrorRetryable =
                        envelope.responseErrorCode == Just 429
                            || maybe False (>= 500) envelope.responseErrorCode
                    }

renderRequestError :: TelegramClient -> TelegramRequestError -> Text
renderRequestError client err =
    redactToken client.clientToken err.telegramErrorMessage

decodeTelegramResponse
    :: forall a. FromJSON a
    => LBS.ByteString
    -> Either Text a
decodeTelegramResponse bytes = do
    envelope <- case
            eitherDecode bytes
                :: Either String (TelegramResponse a)
        of
        Left err -> Left ("Telegram returned invalid JSON: " <> Text.pack err)
        Right value -> Right value
    if envelope.responseOk
        then maybe
            (Left "Telegram response did not contain a result")
            Right
            envelope.responseResult
        else Left $
            "Telegram API error: "
                <> fromMaybe "unknown error" envelope.responseDescription

getUpdates
    :: TelegramClient
    -> Maybe Integer
    -> IO (Either Text [TelegramUpdate])
getUpdates client offset =
    telegramRequest client "getUpdates" body 45 >>= \case
        Left err -> pure (Left err)
        Right response -> pure (decodeTelegramResponse response)
  where
    body = object $
        [ "timeout" .= (30 :: Int)
        , "allowed_updates" .=
            ( [ "message"
              , "edited_message"
              , "message_reaction"
              , "callback_query"
              ] :: [Text]
            )
        ]
            <> maybe [] (\value -> ["offset" .= value]) offset

getTelegramBot :: TelegramClient -> IO TelegramUser
getTelegramBot client =
    telegramRequest client "getMe" (object []) 15 >>= \case
        Left err -> fail (Text.unpack err)
        Right response ->
            either (fail . Text.unpack) pure (decodeTelegramResponse response)

getTelegramFilePath :: TelegramClient -> Text -> IO FilePath
getTelegramFilePath client fileId =
    telegramRequest client "getFile" (object ["file_id" .= fileId]) 30 >>= \case
        Left err -> fail (Text.unpack err)
        Right response ->
            case decodeTelegramResponse response :: Either Text TelegramFile of
                Left err -> fail (Text.unpack err)
                Right TelegramFile { telegramFilePath = Nothing } ->
                    fail "Telegram getFile response did not contain file_path"
                Right TelegramFile { telegramFilePath = Just path } ->
                    pure (Text.unpack path)

downloadTelegramFile
    :: TelegramClient
    -> Integer
    -> FilePath
    -> OsPath
    -> IO OsPath
downloadTelegramFile client maxBytes remotePath destination = do
    response <- runRetrying client do
        request <- Http.parseRequest $
            "https://api.telegram.org/file/bot"
                <> Text.unpack client.clientToken
                <> "/"
                <> remotePath
        Http.httpLbs
            request
                { Http.responseTimeout =
                    Http.responseTimeoutMicro 60_000_000
                }
            client.clientManager
    body <- either (fail . Text.unpack) pure response
    when (LBS.length body > fromIntegral maxBytes) $
        fail "Telegram file download exceeds the configured limit"
    LBS.writeFile (unsafeToFilePath destination) body
    setFileMode (unsafeToFilePath destination) 0o600
    pure destination

sendTypingAction :: TelegramClient -> TelegramChatKey -> IO ()
sendTypingAction client key =
    expectBool client "sendChatAction" $
        object $
            [ "chat_id" .= key.chatId
            , "action" .= ("typing" :: Text)
            ]
                <> threadParameters key

sendStreamingDraft
    :: TelegramClient
    -> TelegramChatKey
    -> Text
    -> IO ()
sendStreamingDraft client key html =
    expectBool client "sendRichMessageDraft" $
        object $
            [ "chat_id" .= key.chatId
            , "draft_id" .= (1 :: Int)
            , "rich_message" .= object ["html" .= html]
            ]
                <> threadParameters key

sendThinkingDraft :: TelegramClient -> TelegramChatKey -> Text -> IO ()
sendThinkingDraft client key status =
    expectBool client "sendRichMessageDraft" $
        object $
            [ "chat_id" .= key.chatId
            , "draft_id" .= (1 :: Int)
            , "rich_message" .= object
                [ "html" .=
                    ("<tg-thinking>"
                        <> escapeHtml status
                        <> "</tg-thinking>")
                ]
            ]
                <> threadParameters key

setMessageReaction
    :: TelegramClient
    -> TelegramChatKey
    -> Integer
    -> Text
    -> IO (Either Text ())
setMessageReaction client key messageId emoji =
    requestUnit client "setMessageReaction" $ object
        [ "chat_id" .= key.chatId
        , "message_id" .= messageId
        , "reaction" .=
            [ object
                [ "type" .= ("emoji" :: Text)
                , "emoji" .= emoji
                ]
            ]
        ]

sendRichMessage
    :: TelegramClient
    -> TelegramChatKey
    -> Maybe Integer
    -> Text
    -> IO (Either Text (Maybe Integer))
sendRichMessage client key replyToMessageId text =
    telegramRequest client "sendRichMessage" richBody 30 >>= \case
        Right response
            | Right messageId <- decodeSentMessageId response ->
                pure (Right messageId)
        _ -> sendHtmlMessage client key replyToMessageId text
  where
    richBody = object $
        [ "chat_id" .= key.chatId
        , "rich_message" .= object
            [ "html" .= markdownToTelegramHtml text
            ]
        ]
            <> threadParameters key
            <> replyParameters replyToMessageId

sendHtmlMessage
    :: TelegramClient
    -> TelegramChatKey
    -> Maybe Integer
    -> Text
    -> IO (Either Text (Maybe Integer))
sendHtmlMessage client key replyToMessageId text =
    telegramRequest client "sendMessage" htmlBody 30 >>= \case
        Right response
            | Right messageId <- decodeSentMessageId response ->
                pure (Right messageId)
        _ -> sendPlainMessage client key replyToMessageId text
  where
    htmlBody = object $
        [ "chat_id" .= key.chatId
        , "text" .= markdownToTelegramHtml text
        , "parse_mode" .= ("HTML" :: Text)
        ]
            <> threadParameters key
            <> replyParameters replyToMessageId

sendPlainMessage
    :: TelegramClient
    -> TelegramChatKey
    -> Maybe Integer
    -> Text
    -> IO (Either Text (Maybe Integer))
sendPlainMessage client key replyToMessageId text =
    telegramRequest client "sendMessage" body 30 >>= \case
        Left err -> pure (Left err)
        Right response -> pure (decodeSentMessageId response)
  where
    body = object $
        [ "chat_id" .= key.chatId
        , "text" .= text
        ]
            <> threadParameters key
            <> replyParameters replyToMessageId

sendMessageWithKeyboard
    :: TelegramClient
    -> TelegramChatKey
    -> Maybe Integer
    -> Text
    -> [[(Text, Text)]]
    -> IO (Either Text (Maybe Integer))
sendMessageWithKeyboard client key replyToMessageId text rows =
    telegramRequest client "sendMessage" body 30 >>= \case
        Left err -> pure (Left err)
        Right response -> pure (decodeSentMessageId response)
  where
    body = object $
        [ "chat_id" .= key.chatId
        , "text" .= text
        , "reply_markup" .= object
            [ "inline_keyboard" .=
                [ [ object
                        [ "text" .= label
                        , "callback_data" .= callbackData
                        ]
                  | (label, callbackData) <- row
                  ]
                | row <- rows
                ]
            ]
        ]
            <> threadParameters key
            <> replyParameters replyToMessageId

editMessageText
    :: TelegramClient
    -> TelegramChatKey
    -> Integer
    -> Text
    -> IO (Either Text ())
editMessageText client key messageId text =
    requestUnit client "editMessageText" $ object
        [ "chat_id" .= key.chatId
        , "message_id" .= messageId
        , "text" .= text
        , "reply_markup" .= object
            [ "inline_keyboard" .= ([] :: [[Value]])
            ]
        ]

answerCallbackQuery
    :: TelegramClient
    -> Text
    -> Maybe Text
    -> IO (Either Text ())
answerCallbackQuery client queryId answerText =
    requestUnit client "answerCallbackQuery" $
        object $
            ["callback_query_id" .= queryId]
                <> maybe [] (\text -> ["text" .= text]) answerText

sendTelegramDocument
    :: TelegramClient
    -> TelegramChatKey
    -> FilePath
    -> Maybe Text
    -> Maybe Text
    -> IO (Either Text (Maybe Integer))
sendTelegramDocument client key path caption filename =
    sendMultipartFile client key "sendDocument" "document" path caption filename

sendTelegramPhoto
    :: TelegramClient
    -> TelegramChatKey
    -> FilePath
    -> Maybe Text
    -> Maybe Text
    -> IO (Either Text (Maybe Integer))
sendTelegramPhoto client key path caption filename =
    sendMultipartFile client key "sendPhoto" "photo" path caption filename

sendTelegramVoice
    :: TelegramClient
    -> TelegramChatKey
    -> FilePath
    -> Maybe Text
    -> Maybe Text
    -> IO (Either Text (Maybe Integer))
sendTelegramVoice client key path caption filename =
    sendMultipartFile client key "sendVoice" "voice" path caption filename

sendMultipartFile
    :: TelegramClient
    -> TelegramChatKey
    -> String
    -> Text
    -> FilePath
    -> Maybe Text
    -> Maybe Text
    -> IO (Either Text (Maybe Integer))
sendMultipartFile client key method fieldName path caption requestedName = do
    exists <- doesFileExist path
    if not exists
        then pure (Left ("file does not exist: " <> Text.pack path))
        else do
            bytes <- BS.readFile path
            let boundary = "haskell-agent-telegram-boundary"
                filename = fromMaybe (Text.pack (takeFileName path)) requestedName
                fields =
                    [("chat_id", Text.pack (show key.chatId))]
                        <> maybe []
                            (\threadId ->
                                [("message_thread_id", Text.pack (show threadId))])
                            key.messageThreadId
                        <> maybe [] (\text -> [("caption", text)]) caption
                body = multipartBody boundary fields fieldName filename bytes
            result <- runRetrying client do
                base <- Http.parseRequest $
                    "https://api.telegram.org/bot"
                        <> Text.unpack client.clientToken
                        <> "/"
                        <> method
                Http.httpLbs
                    base
                        { Http.method = "POST"
                        , Http.requestHeaders =
                            [ ( "Content-Type"
                              , "multipart/form-data; boundary=" <> boundary
                              )
                            ]
                        , Http.requestBody = Http.RequestBodyLBS body
                        , Http.responseTimeout =
                            Http.responseTimeoutMicro 60_000_000
                        }
                    client.clientManager
            case result of
                Left err -> pure (Left err)
                Right response -> pure (decodeSentMessageId response)

multipartBody
    :: BS.ByteString
    -> [(Text, Text)]
    -> Text
    -> Text
    -> BS.ByteString
    -> LBS.ByteString
multipartBody boundary fields fileField filename fileBytes =
    mconcat
        (map textPart fields)
        <> filePart
        <> LBS8.pack ("--" <> BS8.unpack boundary <> "--\r\n")
  where
    delimiter = "--" <> BS8.unpack boundary <> "\r\n"
    textPart (name, value) = LBS8.pack $
        delimiter
            <> "Content-Disposition: form-data; name=\""
            <> Text.unpack name
            <> "\"\r\n\r\n"
            <> Text.unpack value
            <> "\r\n"
    filePart =
        LBS8.pack
            ( delimiter
                <> "Content-Disposition: form-data; name=\""
                <> Text.unpack fileField
                <> "\"; filename=\""
                <> Text.unpack (sanitizeFilename filename)
                <> "\"\r\n"
                <> "Content-Type: application/octet-stream\r\n\r\n"
            )
            <> LBS.fromStrict fileBytes
            <> "\r\n"

sanitizeFilename :: Text -> Text
sanitizeFilename =
    Text.map \char ->
        if char `elem` ['\r', '\n', '"'] then '_' else char

decodeSentMessageId :: LBS.ByteString -> Either Text (Maybe Integer)
decodeSentMessageId response =
    case decodeTelegramResponse response :: Either Text TelegramMessage of
        Right message -> Right (Just message.messageId)
        Left _ ->
            (() <$ (decodeTelegramResponse response :: Either Text Bool))
                >> Right Nothing

expectBool :: TelegramClient -> String -> Value -> IO ()
expectBool client method body =
    telegramRequest client method body 30 >>= \case
        Left err -> fail (Text.unpack err)
        Right response ->
            case decodeTelegramResponse response :: Either Text Bool of
                Left err -> fail (Text.unpack err)
                Right _ -> pure ()

requestUnit :: TelegramClient -> String -> Value -> IO (Either Text ())
requestUnit client method body =
    telegramRequest client method body 30 >>= \case
        Left err -> pure (Left err)
        Right response ->
            pure (() <$ (decodeTelegramResponse response :: Either Text Value))

threadParameters :: TelegramChatKey -> [(Key.Key, Value)]
threadParameters key =
    maybe []
        (\threadId -> ["message_thread_id" .= threadId])
        key.messageThreadId

replyParameters :: Maybe Integer -> [(Key.Key, Value)]
replyParameters =
    maybe [] \messageId ->
        [ "reply_parameters" .= object
            [ "message_id" .= messageId
            , "allow_sending_without_reply" .= True
            ]
        ]

escapeHtml :: Text -> Text
escapeHtml =
    Text.replace ">" "&gt;"
        . Text.replace "<" "&lt;"
        . Text.replace "&" "&amp;"

redactToken :: Text -> Text -> Text
redactToken token = Text.replace token "<redacted>"
