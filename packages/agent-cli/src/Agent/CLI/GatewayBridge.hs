-- | Private request/response bridge for gateway-owned agent turns.
module Agent.CLI.GatewayBridge
    ( ManagedBridgeRequest(..)
    , managedBridgeRequestDecoder
    , ManagedBridgeResponse(..)
    , managedBridgeResponseDecoder
    , ManagedActivity(..)
    , managedActivityDecoder
    , managedGatewayTools
    , newManagedLoopEventPublisher
    , requestManagedApproval
    , requestManagedRootAccess
    , managedBridgeRequestsDirectory
    , managedBridgeResponsesDirectory
    , managedBridgeActivityPath
    , writeManagedBridgeResponse
    , writeManagedBridgeResponseAt
    ) where

import Agent.CLI.ManagedTurn (ManagedTurnRequest(..))
import Agent.CLI.Permission (PermissionChoice(..))
import Agent.FileRetry (retryOnFileBusy, writeLazyFileAtomically)
import Agent.Loop (LoopEvent(..))
import Agent.OsPath (toText, unsafeToFilePath)
import Agent.TextBuffer
    ( TextBuffer
    , appendTextBuffer
    , emptyTextBuffer
    , textBufferToText
    )
import Agent.Json.Decode (optionalKey)
import Agent.Json.Decode qualified as Hermes
import Agent.Json
    ( RawJson
    , rawJsonBytes
    , rawJsonDecoder
    , rawJsonFromEncoding
    , rawJsonEncoding
    )
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
    ( ToJSON(..)
    , Value(..)
    , encode
    , object
    , (.=)
    )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum)
import Data.Maybe (fromMaybe)
import Data.IORef (newIORef, readIORef, writeIORef)
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
    , bridgeRequestPayload :: !RawJson
    } deriving (Eq, Show)

instance ToJSON ManagedBridgeRequest where
    toJSON _ =
        error "ManagedBridgeRequest supports Aeson encoding only"
    toEncoding request = Aeson.pairs
        ( "version" .= request.bridgeRequestVersion
        <> "id" .= request.bridgeRequestId
        <> "kind" .= request.bridgeRequestKind
        <> "payload" .= EncodedRawJson request.bridgeRequestPayload
        )

newtype EncodedRawJson = EncodedRawJson RawJson

instance ToJSON EncodedRawJson where
    toJSON _ = error "EncodedRawJson supports Aeson encoding only"
    toEncoding (EncodedRawJson raw) = rawJsonEncoding raw

managedBridgeRequestDecoder :: Hermes.Decoder ManagedBridgeRequest
managedBridgeRequestDecoder = Hermes.object $
    ManagedBridgeRequest
        <$> Hermes.defaultKey bridgeSchemaVersion "version" Hermes.int
        <*> Hermes.atKey "id" Hermes.text
        <*> Hermes.atKey "kind" Hermes.text
        <*> Hermes.atKey "payload" rawJsonDecoder

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

data DecodedBridgeResponse = DecodedBridgeResponse
    { decodedResponseVersion :: !Int
    , decodedResponseId :: !Text
    , decodedResponseOk :: !Bool
    , decodedResponseResult :: !(Maybe RawJson)
    , decodedResponseError :: !(Maybe Text)
    }

managedBridgeResponseDecoder :: Hermes.Decoder DecodedBridgeResponse
managedBridgeResponseDecoder = Hermes.object $
    DecodedBridgeResponse
        <$> Hermes.defaultKey bridgeSchemaVersion "version" Hermes.int
        <*> Hermes.atKey "id" Hermes.text
        <*> Hermes.atKey "ok" Hermes.bool
        <*> optionalKey "result" rawJsonDecoder
        <*> optionalKey "error" Hermes.text

data ManagedActivity = ManagedActivity
    { managedActivityVersion :: !Int
    , managedActivityKind :: !Text
    , managedActivityMessage :: !Text
    , managedActivityReasoning :: !Text
    , managedActivityResponse :: !Text
    , managedActivityUpdatedAt :: !UTCTime
    } deriving (Eq, Show)

instance ToJSON ManagedActivity where
    toJSON activity = object
        [ "version" .= activity.managedActivityVersion
        , "kind" .= activity.managedActivityKind
        , "message" .= activity.managedActivityMessage
        , "reasoning" .= activity.managedActivityReasoning
        , "response" .= activity.managedActivityResponse
        , "updated_at" .= activity.managedActivityUpdatedAt
        ]

managedActivityDecoder :: Hermes.Decoder ManagedActivity
managedActivityDecoder = Hermes.object $
        ManagedActivity
            <$> Hermes.defaultKey bridgeSchemaVersion "version" Hermes.int
            <*> Hermes.atKey "kind" Hermes.text
            <*> Hermes.atKey "message" Hermes.text
            <*> Hermes.defaultKey "" "reasoning" Hermes.text
            <*> Hermes.defaultKey "" "response" Hermes.text
            <*> Hermes.atKey "updated_at" Hermes.utcTime

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
            , allowUserTool
            , denyUserTool
            , listUsersTool
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
            (typedToolWithCall name sendPathArgsDecoder \call args ->
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
            (typedToolWithCall "react_to_telegram_message" reactionArgsDecoder
                \call args ->
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
            (typedToolWithCall "ask_telegram_choice" choiceArgsDecoder \call args ->
                bridgeTool request call "ask_choice" (toChoicePayload args))

    allowUserTool =
        jsonTool
            "allow_telegram_user"
            "Allow another Telegram user to talk to this bot. Use this when an already-allowed user asks to accept messages from someone in the current group. Identify them by name, @username, or numeric user id. Omit query to allow the user this message replies to."
            [ PropertySchema "query" PropertyString False
                (Just "Name, @username, or numeric Telegram user id.")
            , PropertySchema "user_id" PropertyInteger False
                (Just "Numeric Telegram user id when already known.")
            ]
            True
            TurnSequential
            (typedToolWithCall "allow_telegram_user" allowlistArgsDecoder \call args ->
                bridgeTool request call "allow_user" (toAllowlistPayload args))

    denyUserTool =
        jsonTool
            "deny_telegram_user"
            "Remove a Telegram user from the allowlist so this bot stops accepting their messages."
            [ PropertySchema "query" PropertyString False
                (Just "Name, @username, or numeric Telegram user id.")
            , PropertySchema "user_id" PropertyInteger False
                (Just "Numeric Telegram user id when already known.")
            ]
            True
            TurnSequential
            (typedToolWithCall "deny_telegram_user" allowlistArgsDecoder \call args ->
                bridgeTool request call "deny_user" (toAllowlistPayload args))

    listUsersTool =
        jsonTool
            "list_telegram_users"
            "List Telegram users currently allowed to talk to this bot, plus people recently seen in this chat so you can allow someone by name without knowing their numeric id."
            []
            True
            TurnSequential
            (typedToolWithCall "list_telegram_users" emptyArgsDecoder \call () ->
                bridgeTool request call "list_users" (object []))

data SendPathArgs = SendPathArgs
    { sendPath :: !Text
    , sendCaption :: !(Maybe Text)
    , sendFilename :: !(Maybe Text)
    }

sendPathArgsDecoder :: Hermes.Decoder SendPathArgs
sendPathArgsDecoder = Hermes.object $
        SendPathArgs
            <$> Hermes.atKey "path" Hermes.text
            <*> optionalKey "caption" Hermes.text
            <*> optionalKey "filename" Hermes.text

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

reactionArgsDecoder :: Hermes.Decoder ReactionArgs
reactionArgsDecoder = Hermes.object $
        ReactionArgs
            <$> Hermes.atKey "emoji" Hermes.text
            <*> optionalKey "message_id" Hermes.int

toReactionPayload :: ReactionArgs -> Value
toReactionPayload args = object
    [ "emoji" .= args.reactionEmoji
    , "message_id" .= args.reactionMessageId
    ]

data ChoiceArgs = ChoiceArgs
    { choiceQuestion :: !Text
    , choiceOptions :: ![Text]
    }

choiceArgsDecoder :: Hermes.Decoder ChoiceArgs
choiceArgsDecoder = Hermes.object $
    ChoiceArgs
        <$> Hermes.atKey "question" Hermes.text
        <*> Hermes.atKey "options" (Hermes.list Hermes.text)

toChoicePayload :: ChoiceArgs -> Value
toChoicePayload args = object
    [ "question" .= args.choiceQuestion
    , "options" .= args.choiceOptions
    ]

data AllowlistArgs = AllowlistArgs
    { allowlistQuery :: !(Maybe Text)
    , allowlistUserId :: !(Maybe Int)
    }

allowlistArgsDecoder :: Hermes.Decoder AllowlistArgs
allowlistArgsDecoder = Hermes.object $
        AllowlistArgs
            <$> optionalKey "query" Hermes.text
            <*> optionalKey "user_id" Hermes.int

emptyArgsDecoder :: Hermes.Decoder ()
emptyArgsDecoder = Hermes.object (pure ())

toAllowlistPayload :: AllowlistArgs -> Value
toAllowlistPayload args = object
    [ "query" .= args.allowlistQuery
    , "user_id" .= args.allowlistUserId
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
            Right raw ->
                pure $ Right $ case Hermes.decodeEither Hermes.text (rawJsonBytes raw) of
                    Right text -> text
                    Left _ -> TextEncoding.decodeUtf8 (rawJsonBytes raw)

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
            Right raw -> case Hermes.decodeEither Hermes.text (rawJsonBytes raw) of
                Right "allow_once" -> pure (Just PermissionAllowOnce)
                Right "allow_tool" -> pure (Just PermissionAllowTool)
                Right "allow_all" -> pure (Just PermissionAllowAll)
                Right "deny" -> pure (Just PermissionDeny)
                _ -> pure Nothing
            _ -> pure Nothing

requestManagedRootAccess :: ManagedTurnRequest -> OsPath -> IO Bool
requestManagedRootAccess request root =
    performBridgeRequest
        request
        "filesystem-access"
        "filesystem_access"
        (object ["path" .= toText root])
        (30 * 60 * 1_000_000) >>= \case
            Right raw ->
                case Hermes.decodeEither Hermes.text (rawJsonBytes raw) of
                    Right "allow" -> pure True
                    _ -> pure False
            Left _ -> pure False

data ManagedActivityAccumulator = ManagedActivityAccumulator
    { accumulatorKind :: !Text
    , accumulatorMessage :: !Text
    , accumulatorReasoning :: !TextBuffer
    , accumulatorResponse :: !TextBuffer
    }

newManagedLoopEventPublisher
    :: ManagedTurnRequest
    -> IO (LoopEvent -> IO ())
newManagedLoopEventPublisher request =
    case request.managedTurnBridgeDirectory of
        Nothing -> pure (const (pure ()))
        Just _ -> do
            stateRef <- newIORef emptyManagedActivityAccumulator
            pure \event -> do
                state <- updateManagedActivity event <$> readIORef stateRef
                writeIORef stateRef state
                now <- getCurrentTime
                void $ try @_ @SomeException $
                    writeLazyFileAtomically
                        (managedBridgeActivityPath request)
                        0o600
                        (encode ManagedActivity
                            { managedActivityVersion = bridgeSchemaVersion
                            , managedActivityKind = state.accumulatorKind
                            , managedActivityMessage = state.accumulatorMessage
                            , managedActivityReasoning =
                                textBufferToText state.accumulatorReasoning
                            , managedActivityResponse =
                                textBufferToText state.accumulatorResponse
                            , managedActivityUpdatedAt = now
                            })

emptyManagedActivityAccumulator :: ManagedActivityAccumulator
emptyManagedActivityAccumulator = ManagedActivityAccumulator
    { accumulatorKind = "thinking"
    , accumulatorMessage = "Thinking…"
    , accumulatorReasoning = emptyTextBuffer
    , accumulatorResponse = emptyTextBuffer
    }

updateManagedActivity
    :: LoopEvent
    -> ManagedActivityAccumulator
    -> ManagedActivityAccumulator
updateManagedActivity event state =
    case event of
        ToolArgumentEvent _ -> emptyManagedActivityAccumulator
        TurnStarted -> emptyManagedActivityAccumulator
        ReasoningDelta delta ->
            state
                { accumulatorKind = "thinking"
                , accumulatorMessage = "Thinking…"
                , accumulatorReasoning =
                    appendTextBuffer delta state.accumulatorReasoning
                }
        TextDelta delta ->
            state
                { accumulatorKind = "writing"
                , accumulatorMessage = "Writing reply…"
                , accumulatorResponse =
                    appendTextBuffer delta state.accumulatorResponse
                }
        ActivityUpdated message ->
            state
                { accumulatorKind = "activity"
                , accumulatorMessage = nonEmpty "Working…" message
                }
        ProviderLimitUpdated{} ->
            state
        WarningRaised message ->
            state
                { accumulatorKind = "warning"
                , accumulatorMessage = nonEmpty "Warning" message
                }
        ResponseRestarted _ ->
            emptyManagedActivityAccumulator
                { accumulatorKind = "retrying"
                , accumulatorMessage = "Retrying response…"
                }
        ToolStarted call ->
            state
                { accumulatorKind = "tool"
                , accumulatorMessage = "Running " <> call.name <> "…"
                }
        ToolOutputUpdated name _ ->
            state
                { accumulatorKind = "tool"
                , accumulatorMessage = "Running " <> name <> "…"
                }
        ToolArgumentEvent _ ->
            state
        ToolFinished _ ->
            state
                { accumulatorKind = "thinking"
                , accumulatorMessage = "Thinking…"
                }
        ToolUpdated _ ->
            state
        ToolArgumentsUpdated _ ->
            state
        ToolRetracted _ ->
            state
        ResponseAttemptDiscarded ->
            emptyManagedActivityAccumulator
        NativeAgentStarted{} ->
            state
        NativeAgentOutput{} ->
            state
        NativeAgentFinished{} ->
            state
        TurnFinished _ ->
            state
                { accumulatorKind = "finished"
                , accumulatorMessage = "Finishing…"
                }
  where
    nonEmpty fallback value
        | Text.null (Text.strip value) = fallback
        | otherwise = Text.strip value

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
    -> IO (Either Text RawJson)
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
                    , bridgeRequestPayload =
                        rawJsonFromEncoding (Aeson.toEncoding payload)
                    }
            writeLazyFileAtomically requestPath 0o600 (encode bridgeRequest)
            waitForBridgeResponse responsePath timeoutMicros
                `finally` do
                    removePrivateFile requestPath
                    removePrivateFile responsePath

waitForBridgeResponse :: OsPath -> Int -> IO (Either Text RawJson)
waitForBridgeResponse path timeoutMicros = go timeoutMicros
  where
    step = 100_000
    go :: Int -> IO (Either Text RawJson)
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
                        pure (Hermes.decodeEither managedBridgeResponseDecoder
                            (LBS.toStrict bytes))
                    case decoded of
                        Left _ -> threadDelay step >> go (remaining - step)
                        Right (Left _) -> threadDelay step >> go (remaining - step)
                        Right (Right response)
                            | response.decodedResponseOk ->
                                pure (Right
                                    (fromMaybe
                                        (rawJsonFromEncoding (Aeson.toEncoding Aeson.Null))
                                        response.decodedResponseResult))
                            | otherwise ->
                                pure (Left
                                    (fromMaybe
                                        "Telegram gateway request failed"
                                        response.decodedResponseError))

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
