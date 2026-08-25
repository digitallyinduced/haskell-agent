-- | Parent-side Telegram implementation of the private managed-turn bridge.
module Agent.Telegram.Bridge
    ( TelegramBridgeEnv(..)
    , withTelegramBridge
    , processTelegramCallbacks
    , processBridgeRequestBatch
    , telegramActivityDraftHtml
    ) where

import Agent.CLI.GatewayBridge
    ( ManagedActivity(..)
    , ManagedBridgeRequest(..)
    , ManagedBridgeResponse(..)
    , managedBridgeActivityPath
    , managedBridgeRequestsDirectory
    , writeManagedBridgeResponse
    , writeManagedBridgeResponseAt
    )
import Agent.CLI.ManagedTurn (ManagedTurnRequest(..))
import Agent.FileRetry (retryOnFileBusy)
import Agent.Concurrent (mapConcurrentlyBounded)
import Agent.OsPath (unsafeToFilePath)
import qualified Agent.Telegram.Client as Client
import Agent.Telegram.Types
import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Exception.Safe (SomeException, displayException, try)
import Control.Monad (forM_, void, when)
import Data.Aeson
    ( FromJSON(..)
    , Result(..)
    , Value(..)
    , eitherDecode
    , fromJSON
    , withObject
    , (.:)
    , (.:?)
    )
import qualified Data.ByteString.Lazy as LBS
import Data.List (isPrefixOf, sort)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (addUTCTime, getCurrentTime)
import System.Directory
    ( canonicalizePath
    , doesFileExist
    , listDirectory
    )
import System.FilePath
    ( addTrailingPathSeparator
    , takeExtension
    , (</>)
    )

data TelegramBridgeEnv = TelegramBridgeEnv
    { telegramBridgeClient :: !TelegramClient
    , telegramBridgeRequest :: !ManagedTurnRequest
    , telegramBridgeChat :: !TelegramChatKey
    , telegramBridgeUserId :: !Integer
    , telegramBridgeReplyTo :: !(Maybe Integer)
    , telegramBridgeAllowedRoot :: !FilePath
    , telegramBridgeModifyState ::
        !((TelegramState -> TelegramState) -> IO ())
    }

data PathRequest = PathRequest
    { pathRequestPath :: !FilePath
    , pathRequestCaption :: !(Maybe Text)
    , pathRequestFilename :: !(Maybe Text)
    }

instance FromJSON PathRequest where
    parseJSON = withObject "PathRequest" \o ->
        PathRequest
            <$> (Text.unpack <$> o .: "path")
            <*> o .:? "caption"
            <*> o .:? "filename"

data ReactionRequest = ReactionRequest
    { reactionRequestEmoji :: !Text
    , reactionRequestMessageId :: !(Maybe Integer)
    }

instance FromJSON ReactionRequest where
    parseJSON = withObject "ReactionRequest" \o ->
        ReactionRequest <$> o .: "emoji" <*> o .:? "message_id"

data ChoiceRequest = ChoiceRequest
    { choiceRequestQuestion :: !Text
    , choiceRequestOptions :: ![Text]
    }

instance FromJSON ChoiceRequest where
    parseJSON = withObject "ChoiceRequest" \o ->
        ChoiceRequest <$> o .: "question" <*> o .: "options"

data ApprovalRequest = ApprovalRequest
    { approvalToolName :: !Text
    , approvalArguments :: !Text
    }

instance FromJSON ApprovalRequest where
    parseJSON = withObject "ApprovalRequest" \o ->
        ApprovalRequest <$> o .: "tool_name" <*> o .: "arguments"

withTelegramBridge :: TelegramBridgeEnv -> IO a -> IO a
withTelegramBridge env action =
    withAsync (bridgeLoop env) (const action)

bridgeLoop :: TelegramBridgeEnv -> IO ()
bridgeLoop env = go Set.empty Nothing (0 :: Int)
  where
    go seen lastDraft tick = do
        seen' <- processBridgeRequests env seen
        lastDraft' <-
            if tick `mod` 10 == 0
                then publishActivity env lastDraft (tick `mod` 200 == 0)
                else pure lastDraft
        threadDelay 100_000
        go seen' lastDraft' (tick + 1)

processBridgeRequests
    :: TelegramBridgeEnv
    -> Set.Set FilePath
    -> IO (Set.Set FilePath)
processBridgeRequests env seen = do
    let directory =
            unsafeToFilePath
                (managedBridgeRequestsDirectory env.telegramBridgeRequest)
    files <- try @_ @SomeException (listDirectory directory) >>= \case
        Left _ -> pure []
        Right values -> pure values
    processBridgeRequestBatch
        seen
        files
        (decodeBridgeRequest . (directory </>))
        (processBridgeRequest env)

-- | Decode and dispatch one deterministic bridge polling batch. Successfully
-- decoded names are admitted exactly once before concurrent dispatch begins.
processBridgeRequestBatch
    :: Set.Set FilePath
    -> [FilePath]
    -> (FilePath -> IO (Maybe request))
    -> (request -> IO ())
    -> IO (Set.Set FilePath)
processBridgeRequestBatch seen files decode dispatch = do
    let published =
            sort $
                filter
                    (\name ->
                        takeExtension name == ".json"
                            && name `Set.notMember` seen)
                    files
    decoded <-
        mapConcurrentlyBounded telegramBridgeConcurrency
            (\name -> fmap ((,) name) <$> decode name)
            published
    let admitted = catMaybes decoded
    void $
        mapConcurrentlyBounded telegramBridgeConcurrency
            (dispatch . snd)
            admitted
    pure (foldr (Set.insert . fst) seen admitted)

telegramBridgeConcurrency :: Int
telegramBridgeConcurrency = 4

decodeBridgeRequest :: FilePath -> IO (Maybe ManagedBridgeRequest)
decodeBridgeRequest path =
    try @_ @SomeException
        (retryOnFileBusy (LBS.readFile path)) >>= \case
            Left _ -> pure Nothing
            Right bytes ->
                pure (either (const Nothing) Just (eitherDecode bytes))

processBridgeRequest :: TelegramBridgeEnv -> ManagedBridgeRequest -> IO ()
processBridgeRequest env request =
    try @_ @SomeException (dispatch request) >>= \case
        Left err ->
            respondError env request
                (Text.pack (displayException err))
        Right (Left err) -> respondError env request err
        Right (Right Nothing) -> pure ()
        Right (Right (Just result)) -> respondOk env request result
  where
    dispatch current =
        case current.bridgeRequestKind of
            "send_document" ->
                withPathPayload current \(payload :: PathRequest) ->
                    sendPath Client.sendTelegramDocument payload
            "send_photo" ->
                withPathPayload current \(payload :: PathRequest) ->
                    sendPath Client.sendTelegramPhoto payload
            "send_voice" ->
                withPathPayload current \(payload :: PathRequest) ->
                    sendPath Client.sendTelegramVoice payload
            "react" ->
                withPayload current \(payload :: ReactionRequest) -> do
                    let messageId =
                            fromMaybe
                                (fromMaybe 0 env.telegramBridgeReplyTo)
                                payload.reactionRequestMessageId
                    if messageId <= 0
                        then pure (Left
                            "no Telegram message is available for a reaction")
                        else
                            Client.setMessageReaction
                                env.telegramBridgeClient
                                env.telegramBridgeChat
                                messageId
                                payload.reactionRequestEmoji
                                >>= pure . fmap (const (Just (String "reaction sent")))
            "ask_choice" ->
                withPayload current \(payload :: ChoiceRequest) ->
                    registerChoice env current
                        payload.choiceRequestQuestion
                        [ (label, label)
                        | label <- payload.choiceRequestOptions
                        ]
            "approval" ->
                withPayload current \(payload :: ApprovalRequest) ->
                    registerChoice env current
                        (approvalText payload)
                        [ ("Allow once", "allow_once")
                        , ("Always this tool", "allow_tool")
                        , ("Allow all", "allow_all")
                        , ("Deny", "deny")
                        ]
            other ->
                pure (Left ("unknown Telegram bridge request: " <> other))

    withPathPayload current use =
        withPayload current \payload -> do
            allowed <- pathIsAllowed
                env.telegramBridgeAllowedRoot
                payload.pathRequestPath
            if allowed
                then use payload
                else pure (Left
                    "Telegram file path is outside the private session directory")

    sendPath send payload =
        send
            env.telegramBridgeClient
            env.telegramBridgeChat
            payload.pathRequestPath
            payload.pathRequestCaption
            payload.pathRequestFilename
            >>= pure . fmap
                (Just . String . maybe "sent" (\messageId ->
                    "sent Telegram message " <> Text.pack (show messageId)))

withPayload
    :: FromJSON a
    => ManagedBridgeRequest
    -> (a -> IO (Either Text (Maybe Value)))
    -> IO (Either Text (Maybe Value))
withPayload request use =
    case fromJSON request.bridgeRequestPayload of
        Error err -> pure (Left (Text.pack err))
        Success payload -> use payload

registerChoice
    :: TelegramBridgeEnv
    -> ManagedBridgeRequest
    -> Text
    -> [(Text, Text)]
    -> IO (Either Text (Maybe Value))
registerChoice env request question rawOptions = do
    let options = take 8
            [ (Text.take 48 (Text.strip label), value)
            | (label, value) <- rawOptions
            , not (Text.null (Text.strip label))
            ]
    if null options
        then pure (Left "Telegram choice requires at least one option")
        else do
            now <- getCurrentTime
            let expiresAt = addUTCTime (30 * 60) now
                bindings =
                    [ TelegramCallbackBinding
                        { callbackBindingData =
                            callbackToken request.bridgeRequestId index
                        , callbackBindingRequestId = request.bridgeRequestId
                        , callbackBindingChat = env.telegramBridgeChat
                        , callbackBindingUserId = env.telegramBridgeUserId
                        , callbackBindingMessageId = Nothing
                        , callbackBindingBridgeDirectory =
                            fromMaybe
                                (error "Telegram bridge directory is missing")
                                env.telegramBridgeRequest.managedTurnBridgeDirectory
                        , callbackBindingValue = value
                        , callbackBindingExpiresAt = expiresAt
                        , callbackBindingConsumed = False
                        }
                    | (index, (_, value)) <- zip [0 :: Int ..] options
                    ]
                rows = chunksOf 2
                    [ (label, binding.callbackBindingData)
                    | ((label, _), binding) <- zip options bindings
                    ]
            env.telegramBridgeModifyState \state ->
                state
                    { callbackBindings =
                        foldr
                            (\binding ->
                                Map.insert
                                    binding.callbackBindingData
                                    binding)
                            state.callbackBindings
                            bindings
                    }
            Client.sendMessageWithKeyboard
                env.telegramBridgeClient
                env.telegramBridgeChat
                env.telegramBridgeReplyTo
                question
                rows >>= \case
                    Left err -> do
                        removeRequestBindings env request.bridgeRequestId
                        pure (Left err)
                    Right messageId -> do
                        env.telegramBridgeModifyState \state ->
                            state
                                { callbackBindings =
                                    fmap
                                        (\binding ->
                                            if binding.callbackBindingRequestId
                                                == request.bridgeRequestId
                                                then binding
                                                    { callbackBindingMessageId =
                                                        messageId
                                                    }
                                                else binding)
                                        state.callbackBindings
                                }
                        pure (Right Nothing)

removeRequestBindings :: TelegramBridgeEnv -> Text -> IO ()
removeRequestBindings env requestId =
    env.telegramBridgeModifyState \state ->
        state
            { callbackBindings =
                Map.filter
                    ((/= requestId) . (.callbackBindingRequestId))
                    state.callbackBindings
            }

processTelegramCallbacks
    :: TelegramClient
    -> Set.Set Integer
    -> ((TelegramState -> TelegramState) -> IO ())
    -> IO TelegramState
    -> IO ()
processTelegramCallbacks client allowedUsers modifyState readState =
    readState >>= \state ->
        case Map.lookupMin state.pendingCallbacks of
            Nothing -> pure ()
            Just (updateId, callback) -> do
                now <- getCurrentTime
                case Map.lookup callback.pendingCallbackData state.callbackBindings of
                    Nothing ->
                        finishInvalid updateId callback
                            "This button has expired."
                    Just binding
                        | binding.callbackBindingConsumed ->
                            finishInvalid updateId callback
                                "This button was already used."
                        | binding.callbackBindingExpiresAt < now ->
                            finishInvalid updateId callback
                                "This button has expired."
                        | callback.pendingCallbackUserId
                            `Set.notMember` allowedUsers ->
                            finishInvalid updateId callback
                                "You are not allowed to use this button."
                        | binding.callbackBindingUserId /= 0
                        , binding.callbackBindingUserId
                            /= callback.pendingCallbackUserId ->
                            finishInvalid updateId callback
                                "This button belongs to another user."
                        | not (callbackChatMatches binding callback) ->
                            finishInvalid updateId callback
                                "This button belongs to another conversation."
                        | otherwise -> do
                            writeManagedBridgeResponseAt
                                binding.callbackBindingBridgeDirectory
                                ManagedBridgeResponse
                                    { bridgeResponseVersion = 1
                                    , bridgeResponseId =
                                        binding.callbackBindingRequestId
                                    , bridgeResponseOk = True
                                    , bridgeResponseResult =
                                        Just (String binding.callbackBindingValue)
                                    , bridgeResponseError = Nothing
                                    }
                            void $ Client.answerCallbackQuery
                                client
                                callback.pendingCallbackQueryId
                                (Just "Selected")
                            forM_ binding.callbackBindingMessageId \messageId ->
                                void $ Client.editMessageText
                                    client
                                    binding.callbackBindingChat
                                    messageId
                                    ("Selected: " <> binding.callbackBindingValue)
                            modifyState \current ->
                                current
                                    { pendingCallbacks =
                                        Map.delete updateId current.pendingCallbacks
                                    , callbackBindings =
                                        Map.filter
                                            ((/= binding.callbackBindingRequestId)
                                                . (.callbackBindingRequestId))
                                            current.callbackBindings
                                    }
                processTelegramCallbacks
                    client
                    allowedUsers
                    modifyState
                    readState
  where
    finishInvalid updateId callback message = do
        void $ Client.answerCallbackQuery
            client
            callback.pendingCallbackQueryId
            (Just message)
        modifyState \state ->
            state
                { pendingCallbacks =
                    Map.delete updateId state.pendingCallbacks
                }

callbackChatMatches
    :: TelegramCallbackBinding
    -> TelegramPendingCallback
    -> Bool
callbackChatMatches binding callback =
    case callback.pendingCallbackChat of
        Nothing -> False
        Just key -> key == binding.callbackBindingChat

publishActivity
    :: TelegramBridgeEnv
    -> Maybe Text
    -> Bool
    -> IO (Maybe Text)
publishActivity env previous forceRefresh = do
    let path = unsafeToFilePath
            (managedBridgeActivityPath env.telegramBridgeRequest)
    exists <- doesFileExist path
    activity <-
        if not exists
            then pure Nothing
            else
                try @_ @SomeException
                    (retryOnFileBusy (LBS.readFile path)) >>= \case
                        Left _ -> pure Nothing
                        Right bytes ->
                            pure case
                                    eitherDecode bytes
                                        :: Either String ManagedActivity
                                of
                                Right value -> Just value
                                Left _ -> Nothing
    let next = activity <&> \value ->
            telegramActivityDraftHtml
                value.managedActivityMessage
                value.managedActivityReasoning
                value.managedActivityResponse
    forM_ next \html ->
        when (forceRefresh || Just html /= previous) $
            void $ try @_ @SomeException $
                Client.sendStreamingDraft
                    env.telegramBridgeClient
                    env.telegramBridgeChat
                    html
    pure (next <|> previous)

telegramActivityDraftHtml :: Text -> Text -> Text -> Text
telegramActivityDraftHtml status reasoningText responseText =
    "<tg-thinking>"
        <> escapeDraftText thinkingText
        <> "</tg-thinking>"
        <> if Text.null response
            then ""
            else "<p>" <> escapeDraftText response <> "</p>"
  where
    reasoning = Text.strip reasoningText
    thinkingText =
        Text.takeEnd 1200 $
            if Text.null reasoning
                then status
                else
                    if status `elem`
                        ["Thinking…", "Writing reply…", "Finishing…"]
                        then reasoning
                        else status <> "\n" <> reasoning
    response = Text.takeEnd 2600 responseText

escapeDraftText :: Text -> Text
escapeDraftText =
    Text.replace "\n" "<br>" . Text.concatMap \case
        '<' -> "&lt;"
        '>' -> "&gt;"
        '&' -> "&amp;"
        '"' -> "&quot;"
        '\'' -> "&#39;"
        character -> Text.singleton character

respondOk :: TelegramBridgeEnv -> ManagedBridgeRequest -> Value -> IO ()
respondOk env request result =
    writeManagedBridgeResponse env.telegramBridgeRequest ManagedBridgeResponse
        { bridgeResponseVersion = 1
        , bridgeResponseId = request.bridgeRequestId
        , bridgeResponseOk = True
        , bridgeResponseResult = Just result
        , bridgeResponseError = Nothing
        }

respondError :: TelegramBridgeEnv -> ManagedBridgeRequest -> Text -> IO ()
respondError env request err =
    writeManagedBridgeResponse env.telegramBridgeRequest ManagedBridgeResponse
        { bridgeResponseVersion = 1
        , bridgeResponseId = request.bridgeRequestId
        , bridgeResponseOk = False
        , bridgeResponseResult = Nothing
        , bridgeResponseError = Just err
        }

approvalText :: ApprovalRequest -> Text
approvalText request =
    "Allow the agent to run "
        <> request.approvalToolName
        <> "?\n\n"
        <> Text.take 1200 request.approvalArguments

callbackToken :: Text -> Int -> Text
callbackToken requestId index =
    "ha:"
        <> Text.takeEnd 48
            (Text.map
                (\char ->
                    if isTokenChar char then char else '-')
                requestId)
        <> ":"
        <> Text.pack (show index)
  where
    isTokenChar char =
        ('a' <= char && char <= 'z')
            || ('A' <= char && char <= 'Z')
            || ('0' <= char && char <= '9')
            || char `elem` ['-', '_']

pathIsAllowed :: FilePath -> FilePath -> IO Bool
pathIsAllowed root path = do
    canonicalRootPath <- canonicalizePath root
    canonicalPath <- canonicalizePath path
    let canonicalRoot = addTrailingPathSeparator canonicalRootPath
    pure $
        canonicalPath == canonicalRootPath
            || canonicalRoot `isPrefixOf` canonicalPath

chunksOf :: Int -> [a] -> [[a]]
chunksOf size values
    | size <= 0 = []
    | null values = []
    | otherwise =
        let (chunk, rest) = splitAt size values
        in chunk : chunksOf size rest
