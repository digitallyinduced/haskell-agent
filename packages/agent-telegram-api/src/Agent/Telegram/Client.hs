-- | Telegram Bot API transport with bounded, status-aware retries.
module Agent.Telegram.Client
    ( TelegramRequestError(..)
    , telegramRequest
    , telegramRequestWith
    , telegramRequestOnce
    , telegramRequestOnceWith
    , decodeTelegramResponse
    , getUpdates
    , getTelegramBot
    , getTelegramFilePath
    , downloadTelegramFile
    , validTelegramFilePath
    , sendTypingAction
    , sendThinkingDraft
    , sendStreamingDraft
    , clearStreamingDraft
    , setMessageReaction
    , deleteMessage
    , sendRichMessage
    , sendMessageWithKeyboard
    , editMessageText
    , editRichMessageText
    , answerCallbackQuery
    , sendTelegramDocument
    , sendTelegramPhoto
    , sendTelegramVoice
    , getChatAdministrators
    , leaveChat
    , redactToken
    ) where

import Agent.Telegram.Presentation
import Agent.Telegram.Types.Wire
import Agent.Telegram.Types.State (TelegramChatKey(..))
import Agent.Json (rawJsonDecoder)
import qualified Agent.Json.Decode as Hermes
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (displayException, tryAny)
import Control.Monad (when)
import Control.Retry
    ( fullJitterBackoff
    , limitRetries
    , retrying
    )
import Data.Aeson
    ( Value
    , encode
    , object
    , (.=)
    )
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Char (isAscii, isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.MultipartFormData as Multipart
import Network.HTTP.Types.Status (statusCode)
import System.Directory (doesFileExist)
import System.FilePath (takeFileName)
import System.OsPath (OsPath, decodeFS)
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

telegramFileDecoder :: Hermes.Decoder TelegramFile
telegramFileDecoder = Hermes.object $
    TelegramFile <$> Hermes.optionalKey "file_path" Hermes.text

telegramRequest
    :: TelegramClient
    -> String
    -> Value
    -> Int
    -> IO (Either Text LBS.ByteString)
telegramRequest client =
    telegramRequestWith (\request -> httpLimited client 2097152 request) client

telegramRequestWith
    :: (Http.Request -> IO (Http.Response LBS.ByteString))
    -> TelegramClient
    -> String
    -> Value
    -> Int
    -> IO (Either Text LBS.ByteString)
telegramRequestWith send client method body timeoutSeconds =
    runRetrying client (performTelegramRequest send client method body timeoutSeconds)

-- | A single attempt for a caller-owned durable outbox. A missing error code
-- means delivery is uncertain; callers must not retry a non-idempotent send.
telegramRequestOnce
    :: TelegramClient -> String -> Value -> Int
    -> IO (Either TelegramRequestError LBS.ByteString)
telegramRequestOnce client =
    telegramRequestOnceWith (\request -> httpLimited client 2097152 request) client

httpLimited :: TelegramClient -> Integer -> Http.Request -> IO (Http.Response LBS.ByteString)
httpLimited client maximumBytes request =
    Http.withResponse request client.clientManager \response -> do
        body <- readChunks maximumBytes [] response.responseBody
        pure response { Http.responseBody = body }
  where
    readChunks remaining chunks reader = do
        chunk <- Http.brRead reader
        if BS.null chunk
            then pure (LBS.fromChunks (reverse chunks))
            else if toInteger (BS.length chunk) > remaining
                then fail "Telegram response exceeds the configured limit"
                else readChunks (remaining - toInteger (BS.length chunk)) (chunk : chunks) reader

telegramRequestOnceWith
    :: (Http.Request -> IO (Http.Response LBS.ByteString))
    -> TelegramClient -> String -> Value -> Int
    -> IO (Either TelegramRequestError LBS.ByteString)
telegramRequestOnceWith send client method body timeoutSeconds = do
    result <- requestAttempt (performTelegramRequest send client method body timeoutSeconds)
    pure $ case result of
        Left err -> Left err { telegramErrorMessage = renderRequestError client err }
        Right bytes ->
            case Hermes.decodeEither (telegramResponseDecoder rawJsonDecoder) (LBS.toStrict bytes) of
                Right envelope | envelope.responseOk, Just _ <- envelope.responseResult -> Right bytes
                _ -> Left TelegramRequestError
                    { telegramErrorMessage = "Telegram response is malformed or incomplete; delivery outcome is uncertain"
                    , telegramErrorCode = Nothing
                    , telegramRetryAfter = Nothing
                    , telegramErrorRetryable = False
                    }

performTelegramRequest
    :: (Http.Request -> IO (Http.Response LBS.ByteString))
    -> TelegramClient -> String -> Value -> Int
    -> IO (Http.Response LBS.ByteString)
performTelegramRequest send client method body timeoutSeconds = do
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
                , Http.redirectCount = 0
                , Http.checkResponse = \_ _ -> pure ()
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
        (const (requestAttempt request))
    pure $ case result of
        Left err -> Left (renderRequestError client err)
        Right body -> Right body
  where
    shouldRetry _ = \case
        Right _ -> pure False
        Left err
            | not err.telegramErrorRetryable -> pure False
            | otherwise -> do
                maybe (pure ()) (\seconds ->
                    threadDelay (max 1 (min 60 seconds) * 1_000_000))
                    err.telegramRetryAfter
                pure True

requestAttempt
    :: IO (Http.Response LBS.ByteString)
    -> IO (Either TelegramRequestError LBS.ByteString)
requestAttempt request =
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

responseFailure :: LBS.ByteString -> Maybe TelegramRequestError
responseFailure bytes =
    case Hermes.decodeEither
            (telegramResponseDecoder rawJsonDecoder)
            (LBS.toStrict bytes)
        of
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
    :: Hermes.Decoder a
    -> LBS.ByteString
    -> Either Text a
decodeTelegramResponse decoder bytes = do
    envelope <- case Hermes.decodeEither
            (telegramResponseDecoder decoder)
            (LBS.toStrict bytes) of
        Left err -> Left
            ("Telegram returned invalid JSON: " <> Hermes.jsonErrorMessage err)
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
        Right response ->
            pure (decodeTelegramResponse (Hermes.list telegramUpdateDecoder) response)
  where
    body = object $
        [ "timeout" .= (30 :: Int)
        , "allowed_updates" .=
            ( [ "message"
              , "edited_message"
              , "message_reaction"
              , "callback_query"
              , "my_chat_member"
              ] :: [Text]
            )
        ]
            <> maybe [] (\value -> ["offset" .= value]) offset

getChatAdministrators
    :: TelegramClient
    -> Integer
    -> IO (Either Text [TelegramChatMember])
getChatAdministrators client chatId =
    telegramRequest client "getChatAdministrators"
        (object ["chat_id" .= chatId])
        15 >>= \case
        Left err -> pure (Left err)
        Right response ->
            pure (decodeTelegramResponse
                (Hermes.list telegramChatMemberDecoder)
                response)

leaveChat
    :: TelegramClient
    -> Integer
    -> IO (Either Text ())
leaveChat client chatId =
    requestUnit client "leaveChat" (object ["chat_id" .= chatId])

getTelegramBot :: TelegramClient -> IO TelegramUser
getTelegramBot client =
    telegramRequest client "getMe" (object []) 15 >>= \case
        Left err -> fail (Text.unpack err)
        Right response ->
            either (fail . Text.unpack) pure
                (decodeTelegramResponse telegramUserDecoder response)

getTelegramFilePath :: TelegramClient -> Text -> IO FilePath
getTelegramFilePath client fileId =
    telegramRequest client "getFile" (object ["file_id" .= fileId]) 30 >>= \case
        Left err -> fail (Text.unpack err)
        Right response ->
            case decodeTelegramResponse telegramFileDecoder response of
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
    when (maxBytes <= 0 || not (validTelegramFilePath (Text.pack remotePath))) $
        fail "Invalid Telegram download path or size limit"
    response <- runRetrying client do
        request <- Http.parseRequest $
            "https://api.telegram.org/file/bot"
                <> Text.unpack client.clientToken
                <> "/"
                <> remotePath
        httpLimited client maxBytes
            request
                { Http.responseTimeout =
                    Http.responseTimeoutMicro 60_000_000
                , Http.redirectCount = 0
                , Http.checkResponse = \_ _ -> pure ()
                }
    body <- either (fail . Text.unpack) pure response
    when (LBS.length body > fromIntegral maxBytes) $
        fail "Telegram file download exceeds the configured limit"
    destinationPath <- decodeFS destination
    LBS.writeFile destinationPath body
    setFileMode destinationPath 0o600
    pure destination

validTelegramFilePath :: Text -> Bool
validTelegramFilePath path =
    not (Text.null path) && all validSegment (Text.splitOn "/" path)
  where
    validSegment segment =
        not (Text.null segment) && segment /= "." && segment /= ".."
            && Text.all (\character -> isAscii character
                && (isAlphaNum character || character `elem` ("._-" :: [Char]))) segment

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

clearStreamingDraft :: TelegramClient -> TelegramChatKey -> IO ()
clearStreamingDraft client key =
    expectBool client "sendRichMessageDraft" $
        object $
            [ "chat_id" .= key.chatId
            , "draft_id" .= (1 :: Int)
            , "rich_message" .= object ["html" .= ("" :: Text)]
            ]
                <> threadParameters key

deleteMessage
    :: TelegramClient
    -> TelegramChatKey
    -> Integer
    -> IO (Either Text ())
deleteMessage client key messageId =
    requestUnit client "deleteMessage" $ object
        [ "chat_id" .= key.chatId
        , "message_id" .= messageId
        ]

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
        messageAddressFields key replyToMessageId <> textPresentationFields RichText text

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
        messageAddressFields key replyToMessageId <> textPresentationFields HtmlText text

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
        messageAddressFields key replyToMessageId <> textPresentationFields PlainText text

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
        messageAddressFields key replyToMessageId <> textPresentationFields PlainText text
            <> ["reply_markup" .= inlineKeyboardMarkup rows]

editMessageText
    :: TelegramClient
    -> TelegramChatKey
    -> Integer
    -> Text
    -> IO (Either Text ())
editMessageText client key messageId text =
    requestUnit client "editMessageText" $ object $
        [ "chat_id" .= key.chatId
        , "message_id" .= messageId
        , "reply_markup" .= inlineKeyboardMarkup []
        ] <> textPresentationFields PlainText text

editRichMessageText
    :: TelegramClient
    -> TelegramChatKey
    -> Integer
    -> Text
    -> IO (Either Text ())
editRichMessageText client key messageId text =
    requestUnit client "editMessageText" richBody >>= \case
        Left _ -> editMessageText client key messageId text
        Right () -> pure (Right ())
  where
    richBody = object $
        [ "chat_id" .= key.chatId
        , "message_id" .= messageId
        ] <> textPresentationFields HtmlText text

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
            let filename = fromMaybe (Text.pack (takeFileName path)) requestedName
                fields =
                    [("chat_id", Text.pack (show key.chatId))]
                        <> maybe []
                            (\threadId ->
                                [("message_thread_id", Text.pack (show threadId))])
                            key.messageThreadId
                        <> maybe [] (\text -> [("caption", text)]) caption
                textPart (name, value) =
                    Multipart.partBS name (TextEncoding.encodeUtf8 value)
                filePart =
                    (Multipart.partFileSource fieldName path)
                        { Multipart.partFilename =
                            Just (Text.unpack (sanitizeFilename filename))
                        , Multipart.partContentType =
                            Just "application/octet-stream"
                        }
            result <- runRetrying client do
                base <- Http.parseRequest $
                    "https://api.telegram.org/bot"
                        <> Text.unpack client.clientToken
                        <> "/"
                        <> method
                request <- Multipart.formDataBody
                    (map textPart fields <> [filePart])
                    base
                        { Http.method = "POST"
                        , Http.redirectCount = 0
                        , Http.checkResponse = \_ _ -> pure ()
                        , Http.responseTimeout =
                            Http.responseTimeoutMicro 60_000_000
                        }
                httpLimited client 2097152 request
            case result of
                Left err -> pure (Left err)
                Right response -> pure (decodeSentMessageId response)

sanitizeFilename :: Text -> Text
sanitizeFilename =
    Text.map \char ->
        if char `elem` ['\r', '\n', '"'] then '_' else char

decodeSentMessageId :: LBS.ByteString -> Either Text (Maybe Integer)
decodeSentMessageId response =
    case decodeTelegramResponse telegramMessageDecoder response of
        Right message -> Right (Just message.messageId)
        Left _ ->
            (() <$ decodeTelegramResponse Hermes.bool response)
                >> Right Nothing

expectBool :: TelegramClient -> String -> Value -> IO ()
expectBool client method body =
    telegramRequest client method body 30 >>= \case
        Left err -> fail (Text.unpack err)
        Right response ->
            case decodeTelegramResponse Hermes.bool response of
                Left err -> fail (Text.unpack err)
                Right _ -> pure ()

requestUnit :: TelegramClient -> String -> Value -> IO (Either Text ())
requestUnit client method body =
    telegramRequest client method body 30 >>= \case
        Left err -> pure (Left err)
        Right response ->
            pure (() <$ decodeTelegramResponse rawJsonDecoder response)


threadParameters :: TelegramChatKey -> [(Key.Key, Value)]
threadParameters key =
    maybe []
        (\threadId -> ["message_thread_id" .= threadId])
        key.messageThreadId

escapeHtml :: Text -> Text
escapeHtml =
    Text.replace ">" "&gt;"
        . Text.replace "<" "&lt;"
        . Text.replace "&" "&amp;"

redactToken :: Text -> Text -> Text
redactToken token = Text.replace token "<redacted>"
