-- | Private request/response bridge for gateway-owned agent turns.
module Agent.CLI.GatewayBridge
    ( ManagedBridgeRequest(..)
    , ManagedBridgeResponse(..)
    , ManagedActivity(..)
    , managedGatewayTools
    , publishManagedLoopEvent
    , requestManagedApproval
    , managedBridgeRequestsDirectory
    , managedBridgeResponsesDirectory
    , managedBridgeActivityPath
    , writeManagedBridgeResponse
    , writeManagedBridgeResponseAt
    ) where

import Agent.CLI.ManagedTurn (ManagedTurnRequest(..))
import Agent.CLI.Permission (PermissionChoice(..))
import Agent.FileRetry (retryOnFileBusy, writeLazyFileAtomically)
import Agent.Loop (LoopEvent(..), ToolSchedulingSnapshot(..))
import Agent.OsPath (unsafeToFilePath)
import Agent.ToolArgs (objectArgs, optInt, optText, reqText)
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch
    ( ToolCall(..)
    , typedToolWithCall
    )
import Agent.Tools.Types
    ( AppTool
    , ToolExecutionPolicy(..)
    , jsonTool
    )
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (SomeException, finally, try)
import Control.Monad (void)
import Data.Aeson
    ( FromJSON(..)
    , ToJSON(..)
    , Value(..)
    , eitherDecode
    , encode
    , object
    , withObject
    , (.:)
    , (.:?)
    , (.!=)
    , (.=)
    )
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.Directory
    ( createDirectoryIfMissing
    , doesFileExist
    , removeFile
    )
import System.OsPath (OsPath, unsafeEncodeUtf, (</>))
import System.Posix.Files (setFileMode)
import System.Posix.Process (getProcessID)

bridgeSchemaVersion :: Int
bridgeSchemaVersion = 1

data ManagedBridgeRequest = ManagedBridgeRequest
    { bridgeRequestVersion :: !Int
    , bridgeRequestId :: !Text
    , bridgeRequestKind :: !Text
    , bridgeRequestPayload :: !Value
    } deriving (Eq, Show)

instance ToJSON ManagedBridgeRequest where
    toJSON request = object
        [ "version" .= request.bridgeRequestVersion
        , "id" .= request.bridgeRequestId
        , "kind" .= request.bridgeRequestKind
        , "payload" .= request.bridgeRequestPayload
        ]

instance FromJSON ManagedBridgeRequest where
    parseJSON = withObject "ManagedBridgeRequest" \o ->
        ManagedBridgeRequest
            <$> (o .:? "version" .!= bridgeSchemaVersion)
            <*> o .: "id"
            <*> o .: "kind"
            <*> (o .:? "payload" .!= Object KeyMap.empty)

data ManagedBridgeResponse = ManagedBridgeResponse
    { bridgeResponseVersion :: !Int
    , bridgeResponseId :: !Text
    , bridgeResponseOk :: !Bool
    , bridgeResponseResult :: !(Maybe Value)
    , bridgeResponseError :: !(Maybe Text)
    } deriving (Eq, Show)

instance ToJSON ManagedBridgeResponse where
    toJSON response = object
        [ "version" .= response.bridgeResponseVersion
        , "id" .= response.bridgeResponseId
        , "ok" .= response.bridgeResponseOk
        , "result" .= response.bridgeResponseResult
        , "error" .= response.bridgeResponseError
        ]

instance FromJSON ManagedBridgeResponse where
    parseJSON = withObject "ManagedBridgeResponse" \o ->
        ManagedBridgeResponse
            <$> (o .:? "version" .!= bridgeSchemaVersion)
            <*> o .: "id"
            <*> o .: "ok"
            <*> o .:? "result"
            <*> o .:? "error"

data ManagedActivity = ManagedActivity
    { managedActivityVersion :: !Int
    , managedActivityKind :: !Text
    , managedActivityMessage :: !Text
    , managedActivityUpdatedAt :: !UTCTime
    } deriving (Eq, Show)

instance ToJSON ManagedActivity where
    toJSON activity = object
        [ "version" .= activity.managedActivityVersion
        , "kind" .= activity.managedActivityKind
        , "message" .= activity.managedActivityMessage
        , "updated_at" .= activity.managedActivityUpdatedAt
        ]

instance FromJSON ManagedActivity where
    parseJSON = withObject "ManagedActivity" \o ->
        ManagedActivity
            <$> (o .:? "version" .!= bridgeSchemaVersion)
            <*> o .: "kind"
            <*> o .: "message"
            <*> o .: "updated_at"

managedGatewayTools :: ManagedTurnRequest -> [AppTool]
managedGatewayTools request =
    case request.managedTurnBridgeDirectory of
        Nothing -> []
        Just _ ->
            [ sendPathTool
                "send_telegram_document"
                "Send a file from the private session temp directory to the current Telegram conversation."
                "send_document"
            , sendPathTool
                "send_telegram_photo"
                "Send an image from the private session temp directory to the current Telegram conversation."
                "send_photo"
            , sendPathTool
                "send_telegram_voice"
                "Send an audio file as a Telegram voice reply to the current conversation."
                "send_voice"
            , reactTool
            , choiceTool
            ]
  where
    sendPathTool name description kind =
        jsonTool
            name
            description
            [ PropertySchema "path" PropertyString True
                (Just "Absolute path to a file under the session temp directory.")
            , PropertySchema "caption" PropertyString False
                (Just "Optional Telegram caption.")
            , PropertySchema "filename" PropertyString False
                (Just "Optional download filename.")
            ]
            True
            TurnSequential
            (typedToolWithCall name \call args ->
                bridgeTool request call kind (toPathPayload args))

    reactTool =
        jsonTool
            "react_to_telegram_message"
            "React to the triggering Telegram message, or to an explicit message id."
            [ PropertySchema "emoji" PropertyString True
                (Just "One standard Telegram reaction emoji.")
            , PropertySchema "message_id" PropertyInteger False
                (Just "Optional Telegram message id; defaults to the triggering message.")
            ]
            True
            TurnSequential
            (typedToolWithCall "react_to_telegram_message" \call args ->
                bridgeTool request call "react" (toReactionPayload args))

    choiceTool =
        jsonTool
            "ask_telegram_choice"
            "Ask the Telegram user to choose one option using inline buttons and wait for the answer."
            [ PropertySchema "question" PropertyString True
                (Just "Question displayed above the inline buttons.")
            , PropertySchema "options" (PropertyArray PropertyString) True
                (Just "Between 1 and 8 short button labels.")
            ]
            True
            TurnSequential
            (typedToolWithCall "ask_telegram_choice" \call args ->
                bridgeTool request call "ask_choice" (toChoicePayload args))

data SendPathArgs = SendPathArgs
    { sendPath :: !Text
    , sendCaption :: !(Maybe Text)
    , sendFilename :: !(Maybe Text)
    }

instance FromJSON SendPathArgs where
    parseJSON = objectArgs \input ->
        SendPathArgs
            <$> reqText input "path"
            <*> optText input "caption"
            <*> optText input "filename"

toPathPayload :: SendPathArgs -> Value
toPathPayload args = object
    [ "path" .= args.sendPath
    , "caption" .= args.sendCaption
    , "filename" .= args.sendFilename
    ]

data ReactionArgs = ReactionArgs
    { reactionEmoji :: !Text
    , reactionMessageId :: !(Maybe Int)
    }

instance FromJSON ReactionArgs where
    parseJSON = objectArgs \input ->
        ReactionArgs
            <$> reqText input "emoji"
            <*> optInt input "message_id"

toReactionPayload :: ReactionArgs -> Value
toReactionPayload args = object
    [ "emoji" .= args.reactionEmoji
    , "message_id" .= args.reactionMessageId
    ]

data ChoiceArgs = ChoiceArgs
    { choiceQuestion :: !Text
    , choiceOptions :: ![Text]
    }

instance FromJSON ChoiceArgs where
    parseJSON = withObject "ChoiceArgs" \o ->
        ChoiceArgs <$> o .: "question" <*> o .: "options"

toChoicePayload :: ChoiceArgs -> Value
toChoicePayload args = object
    [ "question" .= args.choiceQuestion
    , "options" .= args.choiceOptions
    ]

bridgeTool
    :: ManagedTurnRequest
    -> ToolCall
    -> Text
    -> Value
    -> IO (Either Text Text)
bridgeTool request call kind payload =
    performBridgeRequest request call.callId kind payload (30 * 60 * 1_000_000)
        >>= \case
            Left err -> pure (Left err)
            Right (String text) -> pure (Right text)
            Right value ->
                pure (Right
                    (TextEncoding.decodeUtf8 (LBS.toStrict (encode value))))

requestManagedApproval
    :: ManagedTurnRequest
    -> ToolCall
    -> IO (Maybe PermissionChoice)
requestManagedApproval request call =
    performBridgeRequest
        request
        call.callId
        "approval"
        (object
            [ "tool_name" .= call.name
            , "arguments" .= call.arguments
            ])
        (30 * 60 * 1_000_000) >>= \case
            Right (String "allow_once") -> pure (Just PermissionAllowOnce)
            Right (String "allow_tool") -> pure (Just PermissionAllowTool)
            Right (String "allow_all") -> pure (Just PermissionAllowAll)
            Right (String "deny") -> pure (Just PermissionDeny)
            _ -> pure Nothing

publishManagedLoopEvent :: ManagedTurnRequest -> LoopEvent -> IO ()
publishManagedLoopEvent request event =
    case request.managedTurnBridgeDirectory of
        Nothing -> pure ()
        Just _ -> do
            now <- getCurrentTime
            let (kind, message) = activityFor event
            void $ try @_ @SomeException $
                writeLazyFileAtomically
                    (managedBridgeActivityPath request)
                    0o600
                    (encode ManagedActivity
                        { managedActivityVersion = bridgeSchemaVersion
                        , managedActivityKind = kind
                        , managedActivityMessage = message
                        , managedActivityUpdatedAt = now
                        })
  where
    activityFor = \case
        TurnStarted -> ("thinking", "Thinking…")
        ReasoningDelta _ -> ("thinking", "Thinking…")
        TextDelta _ -> ("writing", "Writing reply…")
        ActivityUpdated message -> ("activity", nonEmpty "Working…" message)
        WarningRaised message -> ("warning", nonEmpty "Warning" message)
        ResponseRestarted _ -> ("retrying", "Retrying response…")
        ToolStarted call -> ("tool", "Running " <> call.name <> "…")
        ToolSchedulingUpdated snapshot ->
            ("tool", schedulingActivity snapshot)
        ToolOutputUpdated name _ -> ("tool", "Running " <> name <> "…")
        ToolFinished _ -> ("thinking", "Thinking…")
        TurnFinished _ -> ("finished", "Finishing…")

    nonEmpty fallback value
        | Text.null (Text.strip value) = fallback
        | otherwise = Text.strip value

    schedulingActivity :: ToolSchedulingSnapshot -> Text
    schedulingActivity snapshot =
        let running = length snapshot.schedulingReadyCallIds
            queued = length snapshot.schedulingBlockedCallIds
        in "Running " <> Text.pack (show running) <> " tool"
            <> if running == 1 then "" else "s"
            <> if queued == 0
                then ""
                else "; " <> Text.pack (show queued) <> " queued"

writeManagedBridgeResponse
    :: ManagedTurnRequest
    -> ManagedBridgeResponse
    -> IO ()
writeManagedBridgeResponse request response = do
    ensureBridgeDirectories request
    writeManagedBridgeResponseAt
        (fromMaybe
            (error "managed gateway bridge directory is unavailable")
            request.managedTurnBridgeDirectory)
        response

writeManagedBridgeResponseAt
    :: FilePath
    -> ManagedBridgeResponse
    -> IO ()
writeManagedBridgeResponseAt bridgeDirectory response = do
    let root = unsafeEncodeUtf bridgeDirectory
        responses = root </> unsafeEncodeUtf "responses"
    createDirectoryIfMissing True (unsafeToFilePath responses)
    setFileMode (unsafeToFilePath root) 0o700
    setFileMode (unsafeToFilePath responses) 0o700
    writeLazyFileAtomically
        (responses
            </> unsafeEncodeUtf
                (Text.unpack response.bridgeResponseId <> ".json"))
        0o600
        (encode response)

performBridgeRequest
    :: ManagedTurnRequest
    -> Text
    -> Text
    -> Value
    -> Int
    -> IO (Either Text Value)
performBridgeRequest request callId kind payload timeoutMicros =
    case request.managedTurnBridgeDirectory of
        Nothing -> pure (Left "managed gateway bridge is unavailable")
        Just _ -> do
            ensureBridgeDirectories request
            requestId <- uniqueRequestId callId
            let requestPath =
                    managedBridgeRequestsDirectory request
                        </> unsafeEncodeUtf (Text.unpack requestId <> ".json")
                responsePath =
                    managedBridgeResponsesDirectory request
                        </> unsafeEncodeUtf (Text.unpack requestId <> ".json")
                bridgeRequest = ManagedBridgeRequest
                    { bridgeRequestVersion = bridgeSchemaVersion
                    , bridgeRequestId = requestId
                    , bridgeRequestKind = kind
                    , bridgeRequestPayload = payload
                    }
            writeLazyFileAtomically requestPath 0o600 (encode bridgeRequest)
            waitForBridgeResponse responsePath timeoutMicros
                `finally` do
                    removePrivateFile requestPath
                    removePrivateFile responsePath

waitForBridgeResponse :: OsPath -> Int -> IO (Either Text Value)
waitForBridgeResponse path timeoutMicros = go timeoutMicros
  where
    step = 100_000
    go :: Int -> IO (Either Text Value)
    go remaining
        | remaining <= 0 =
            pure (Left "timed out waiting for the Telegram gateway")
        | otherwise = do
            exists <- doesFileExist (unsafeToFilePath path)
            if not exists
                then threadDelay step >> go (remaining - step)
                else do
                    decoded <- try @_ @SomeException do
                        bytes <- retryOnFileBusy
                            (LBS.readFile (unsafeToFilePath path))
                        pure
                            (eitherDecode bytes
                                :: Either String ManagedBridgeResponse)
                    case decoded of
                        Left _ -> threadDelay step >> go (remaining - step)
                        Right (Left _) -> threadDelay step >> go (remaining - step)
                        Right (Right response)
                            | response.bridgeResponseOk ->
                                pure (Right
                                    (fromMaybe Null response.bridgeResponseResult))
                            | otherwise ->
                                pure (Left
                                    (fromMaybe
                                        "Telegram gateway request failed"
                                        response.bridgeResponseError))

ensureBridgeDirectories :: ManagedTurnRequest -> IO ()
ensureBridgeDirectories request = do
    let paths =
            [ bridgeRoot request
            , managedBridgeRequestsDirectory request
            , managedBridgeResponsesDirectory request
            ]
    mapM_ (\path -> do
        createDirectoryIfMissing True (unsafeToFilePath path)
        setFileMode (unsafeToFilePath path) 0o700) paths

managedBridgeRequestsDirectory :: ManagedTurnRequest -> OsPath
managedBridgeRequestsDirectory request =
    bridgeRoot request </> unsafeEncodeUtf "requests"

managedBridgeResponsesDirectory :: ManagedTurnRequest -> OsPath
managedBridgeResponsesDirectory request =
    bridgeRoot request </> unsafeEncodeUtf "responses"

managedBridgeActivityPath :: ManagedTurnRequest -> OsPath
managedBridgeActivityPath request =
    bridgeRoot request </> unsafeEncodeUtf "activity.json"

bridgeRoot :: ManagedTurnRequest -> OsPath
bridgeRoot request =
    unsafeEncodeUtf $
        fromMaybe
            (error "managed gateway bridge directory is unavailable")
            request.managedTurnBridgeDirectory

uniqueRequestId :: Text -> IO Text
uniqueRequestId callId = do
    now <- getCurrentTime
    pid <- getProcessID
    let micros =
            floor (utcTimeToPOSIXSeconds now * 1_000_000) :: Integer
        safeCall = Text.take 40 (Text.map sanitize callId)
    pure $
        (if Text.null safeCall then "request" else safeCall)
            <> "-"
            <> Text.pack (show pid)
            <> "-"
            <> Text.pack (show micros)
  where
    sanitize char
        | isAlphaNum char = char
        | otherwise = '-'

removePrivateFile :: OsPath -> IO ()
removePrivateFile path = do
    _ <- try @_ @SomeException (removeFile (unsafeToFilePath path))
    pure ()
