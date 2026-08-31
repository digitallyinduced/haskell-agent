module Agent.Telegram.Internal.Turn where


import Agent.CLI.AgentSessions.Process (launchManagedTurnBounded)
import Agent.CLI.ManagedTurn
    ( ManagedTurnMedia(..)
    , ManagedTurnContext(..)
    , ManagedTurnRequest(..)
    , managedTurnRequestFromText
    , managedTurnRequestWithGateway
    )
import Agent.CLI.Session
    ( SessionHandle(..)
    , SessionMeta(metaId)
    , loadSessionHandle
    )
import Agent.Telegram.Types
import Agent.Telegram.Classify
    ( checkpointPendingVoiceTranscript
    , isAmbientGroupPrompt
    , nextPendingAction
    )
import Agent.Telegram.Bridge (withTelegramBridge)
import qualified Agent.Telegram.Client as TelegramClient
import Agent.Telegram.Voice (transcribeWithXAI)
import Agent.Concurrent (mapConcurrentlyBounded)
import Agent.OsPath (unsafeToFilePath)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
    ( modifyMVar
    , modifyMVar_
    , newMVar
    , readMVar
    )
import Control.Exception.Safe
    ( bracket
    , finally
    , onException
    , tryAny
    )
import Control.Monad (void, when)
import Data.IORef (newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock
    ( addUTCTime
    , diffUTCTime
    , getCurrentTime
    )
import qualified System.Directory as Directory
import System.Directory.OsPath
    ( createDirectoryIfMissing
    , removeFile
    )
import System.FilePath (takeExtension)
import System.OsPath (OsPath, unsafeEncodeUtf, (</>))
import System.Posix.Files (setFileMode)
import Agent.Telegram.Internal.Runtime.Types
import Agent.Telegram.Internal.Allowlist
import Agent.Telegram.Internal.Support
data TelegramTurnResponse = TelegramTurnResponse
    { telegramTurnText :: !Text
    , telegramTurnProgressMessageId :: !(Maybe Integer)
    }

runQueuedMediaTurn
    :: TelegramRuntime
    -> TelegramPendingMediaTurn
    -> IO TelegramTurnResponse
runQueuedMediaTurn runtime pending = do
    progressMessageId <- newIORef Nothing
    handle <- sessionForPrompt runtime pending.pendingMediaChat pending.pendingMediaText
    let agentPrompt = telegramAgentPrompt pending.pendingMediaText
    attachments <- downloadTelegramMediaAttachments runtime handle pending
    let imageAttachments =
            [ media
            | (TelegramMediaPhoto, media) <- attachments
            ]
        fileAttachments =
            [ media
            | (kind, media) <- attachments
            , kind /= TelegramMediaPhoto
            ]
        request = ManagedTurnRequest
            { managedTurnVersion = 1
            , managedTurnText = agentPrompt
            , managedTurnImages = imageAttachments
            , managedTurnFiles = fileAttachments
            , managedTurnBridgeDirectory = Nothing
            , managedTurnContext = Nothing
            }
    let bridgeDir =
            handle.sessionTempDir
                </> unsafeEncodeUtf
                    ("telegram-bridge-"
                        <> maybe "turn" show (Just pending.pendingMediaMessageId))
        bridgePath = unsafeToFilePath bridgeDir
        gatewayRequest =
            managedTurnRequestWithGateway
                bridgePath
                ManagedTurnContext
                    { managedGateway = "telegram"
                    , managedChatId = pending.pendingMediaChat.chatId
                    , managedMessageThreadId =
                        pending.pendingMediaChat.messageThreadId
                    , managedReplyToMessageId =
                        Just pending.pendingMediaMessageId
                    , managedUserId = pending.pendingMediaUserId
                    }
                request
        bridgeEnv =
            telegramBridgeEnv
                runtime
                gatewayRequest
                pending.pendingMediaChat
                pending.pendingMediaUserId
                (Just pending.pendingMediaMessageId)
                progressMessageId
                (not (isAmbientGroupPrompt pending.pendingMediaText))
                (unsafeToFilePath handle.sessionTempDir)
    createDirectoryIfMissing True bridgeDir
    setFileMode bridgePath 0o700
    priorTurnIndex <-
        latestPersistedTurnIndex runtime handle.sessionMeta.metaId
    result <-
        (withTelegramBridge bridgeEnv $
            launchManagedTurnBounded
                runtime.runtimeProcessManager
                False
                runtime.runtimePolicy
                True
                False
                (Just (20 * 60 * 1_000_000))
                handle
                gatewayRequest)
            `finally` cleanupTelegramBridge runtime bridgePath bridgeDir
    response <- (case result of
        Left err -> fail (Text.unpack err)
        Right _ ->
            loadSessionHandle
                runtime.runtimePool
                runtime.runtimeSessionsRoot
                handle.sessionMeta.metaId >>= \case
                    Left err -> fail (Text.unpack err)
                    Right (_, turns) ->
                        latestPersistedTurnIndex
                            runtime
                            handle.sessionMeta.metaId >>= \case
                                Just turnIndex
                                    | maybe True (< turnIndex) priorTurnIndex
                                    , latestTurnMatches
                                        agentPrompt
                                        turns ->
                                        pure (renderLatestTurn turns)
                                _ ->
                                    fail
                                        "agent completed without recording \
                                        \the Telegram turn")
        `finally` cleanupManagedTurnMedia request
    TelegramTurnResponse response <$> readIORef progressMessageId

checkpointVoiceTranscript
    :: TelegramRuntime
    -> TelegramPendingTurn
    -> Text
    -> IO ()
checkpointVoiceTranscript runtime pending transcript =
    modifyState runtime
        (checkpointPendingVoiceTranscript
            pending.pendingTurnUpdateId
            transcript)

transcribeTelegramVoice
    :: TelegramRuntime
    -> TelegramPendingTurn
    -> TelegramVoice
    -> IO Text
transcribeTelegramVoice runtime pending voice = do
    when (voice.voiceDuration > 600) $
        fail "Telegram voice message exceeds the 10-minute limit"
    when (maybe False (> 20 * 1024 * 1024) voice.voiceFileSize) $
        fail "Telegram voice message exceeds the 20 MB limit"
    filePath <-
        TelegramClient.getTelegramFilePath
            runtime.runtimeClient
            voice.voiceFileId
    let extension = case Text.toLower (Text.pack (takeExtension filePath)) of
            ".wav" -> ".wav"
            ".mp3" -> ".mp3"
            ".m4a" -> ".m4a"
            ".webm" -> ".webm"
            _ -> ".ogg"
        localPath =
            runtime.runtimeGatewayDirectory
                </> unsafeEncodeUtf
                    ("voice-"
                        <> show pending.pendingTurnUpdateId
                        <> Text.unpack extension)
    bracket
        (TelegramClient.downloadTelegramFile
            runtime.runtimeClient
            (20 * 1024 * 1024)
            filePath
            localPath)
        (\path -> void (tryAny (removeFile path)))
        \path -> do
            transcriptionCwd <- Directory.getTemporaryDirectory
            transcript <- transcribeWithXAI
                transcriptionCwd
                (unsafeToFilePath path)
            let clean = Text.strip transcript
            when (Text.null clean) $
                fail "xAI returned an empty voice transcription"
            pure $
                let rendered = "[Voice message transcript]: " <> clean
                in if pending.pendingTurnText == "[Voice message]"
                    then rendered
                    else pending.pendingTurnText <> "\n" <> rendered

downloadTelegramMediaAttachments
    :: TelegramRuntime
    -> SessionHandle
    -> TelegramPendingMediaTurn
    -> IO [(TelegramMediaKind, ManagedTurnMedia)]
downloadTelegramMediaAttachments runtime handle pending =
    downloadTelegramMediaAttachmentsWith
        (TelegramClient.getTelegramFilePath runtime.runtimeClient)
        (TelegramClient.downloadTelegramFile
            runtime.runtimeClient
            (20 * 1024 * 1024))
        handle.sessionTempDir
        pending.pendingMediaUpdateId
        pending.pendingMediaAttachments

-- | Download one Telegram media batch with bounded concurrency. The injected
-- operations make ordering, cleanup, and cancellation behavior testable
-- without a live Telegram API.
downloadTelegramMediaAttachmentsWith
    :: (Text -> IO FilePath)
    -> (FilePath -> OsPath -> IO OsPath)
    -> OsPath
    -> Integer
    -> [TelegramMedia]
    -> IO [(TelegramMediaKind, ManagedTurnMedia)]
downloadTelegramMediaAttachmentsWith getFilePath downloadFile tempDir updateId media =
    do
        cleanupPaths <- newMVar []
        let cleanup =
                readMVar cleanupPaths >>= mapM_ \path ->
                    void (tryAny (removeFile path))
            downloadOne (index, attachment) =
                case attachment.telegramMediaFile of
                    Nothing -> pure []
                    Just file -> do
                        filePath <- getFilePath file.fileMediaFileId
                        let extension =
                                fromMaybe
                                    (kindExtension attachment.telegramMediaKind)
                                    (fileMediaExtension file)
                            localPath =
                                tempDir
                                    </> unsafeEncodeUtf
                                        ("media-"
                                            <> show updateId
                                            <> "-"
                                            <> show index
                                            <> extension)
                        modifyMVar_ cleanupPaths (pure . (localPath :))
                        path <- downloadFile filePath localPath
                        pure
                            [ ( attachment.telegramMediaKind
                              , ManagedTurnMedia
                                    { managedTurnMediaPath =
                                        unsafeToFilePath path
                                    , managedTurnMediaMime =
                                        fromMaybe "application/octet-stream"
                                            file.fileMediaMimeType
                                    , managedTurnMediaName =
                                        file.fileMediaName
                                    }
                              )
                            ]
        concat
            <$> mapConcurrentlyBounded
                telegramMediaDownloadConcurrency
                downloadOne
                (zip [0 :: Int ..] media)
            `onException` cleanup
  where
    telegramMediaDownloadConcurrency = 4

    fileMediaExtension file =
        case file.fileMediaName of
            Just name | not (Text.null name) ->
                let ext = takeExtension (Text.unpack name)
                in if null ext then Nothing else Just ext
            _ -> Nothing

    kindExtension = \case
        TelegramMediaPhoto -> ".jpg"
        TelegramMediaDocument -> ".bin"
        TelegramMediaVideo -> ".mp4"
        TelegramMediaVideoNote -> ".mp4"
        TelegramMediaAudio -> ".ogg"
        TelegramMediaAnimation -> ".mp4"
        TelegramMediaSticker -> ".webp"
        TelegramMediaLocation -> ".txt"
        TelegramMediaContact -> ".txt"
        TelegramMediaVenue -> ".txt"
        TelegramMediaPoll -> ".txt"
        TelegramMediaDice -> ".txt"

nextChatAction
    :: TelegramRuntime
    -> TelegramChatKey
    -> IO (Maybe PendingChatAction)
nextChatAction runtime key = do
    state <- readMVar runtime.runtimeStateVar
    pure (nextPendingAction key state)

completePendingAction
    :: PendingChatAction
    -> TelegramState
    -> TelegramState
completePendingAction action state =
    (deletePendingAction action state)
        { retryMetadata =
            Map.delete (pendingRetryKey action) state.retryMetadata
        , deliveryCheckpoints =
            Map.delete (pendingRetryKey action) state.deliveryCheckpoints
        }

waitForActionRetry :: TelegramRuntime -> PendingChatAction -> IO ()
waitForActionRetry runtime action = do
    state <- readMVar runtime.runtimeStateVar
    case Map.lookup (pendingRetryKey action) state.retryMetadata
        >>= (.retryNextAt) of
        Nothing -> pure ()
        Just retryAt -> do
            now <- getCurrentTime
            let micros =
                    floor
                        (max 0 (realToFrac (diffUTCTime retryAt now) * 1_000_000)
                            :: Double)
            when (micros > 0) (threadDelay micros)

recordPendingFailure
    :: TelegramRuntime
    -> PendingChatAction
    -> Text
    -> IO (Maybe Int)
recordPendingFailure runtime action err =
    modifyMVar runtime.runtimeStateVar \state -> do
        now <- getCurrentTime
        let key = pendingRetryKey action
            previous =
                fromMaybe
                    (TelegramRetryMetadata 0 Nothing Nothing)
                    (Map.lookup key state.retryMetadata)
            attempts = previous.retryAttempts + 1
        if attempts >= 5 && not (isLeaveAction action)
            then do
                let withoutAction =
                        (deletePendingAction action state)
                            { retryMetadata =
                                Map.delete key state.retryMetadata
                            , deadLetters =
                                state.deadLetters
                                    <> [ TelegramDeadLetter
                                            { deadLetterUpdateId =
                                                pendingActionUpdateIdLocal action
                                            , deadLetterChat =
                                                Just (pendingActionChatLocal action)
                                            , deadLetterError = err
                                            , deadLetterFailedAt = now
                                            , deadLetterAction = Just action
                                            }
                                       ]
                            }
                    next = case failureReply action of
                        Nothing -> withoutAction
                        Just pending ->
                            enqueuePendingAction
                                (DeliverReply pending)
                                withoutAction
                saveTelegramState runtime.runtimeStatePath next
                pure (next, Nothing)
            else do
                let seconds = min 60 (2 ^ min 6 attempts)
                    retryAt = addUTCTime (fromIntegral seconds) now
                    next = state
                        { retryMetadata =
                            Map.insert
                                key
                                TelegramRetryMetadata
                                    { retryAttempts = attempts
                                    , retryNextAt = Just retryAt
                                    , retryLastError = Just err
                                    }
                                state.retryMetadata
                        }
                saveTelegramState runtime.runtimeStatePath next
                pure (next, Just (seconds * 1_000_000))

failureReply :: PendingChatAction -> Maybe TelegramPendingReply
failureReply = \case
    DeliverReply _ -> Nothing
    LeaveUnauthorizedChat _ -> Nothing
    RunPendingTurn pending ->
        Just TelegramPendingReply
            { pendingUpdateId = pending.pendingTurnUpdateId
            , pendingChat = pending.pendingTurnChat
            , pendingReplyToMessageId = Just pending.pendingTurnMessageId
            , pendingEditMessageId = Nothing
            , pendingText =
                "This turn failed after 5 attempts. Send /retry to try it again."
            }
    RunPendingMediaTurn pending ->
        Just TelegramPendingReply
            { pendingUpdateId = pending.pendingMediaUpdateId
            , pendingChat = pending.pendingMediaChat
            , pendingReplyToMessageId = Just pending.pendingMediaMessageId
            , pendingEditMessageId = Nothing
            , pendingText =
                "This media turn failed after 5 attempts. Send /retry to try it again."
            }

pendingRetryKey :: PendingChatAction -> Text
pendingRetryKey action =
    Text.intercalate ":"
        [ Text.pack (show (pendingActionChatLocal action).chatId)
        , maybe
            "-"
            (Text.pack . show)
            (pendingActionChatLocal action).messageThreadId
        , Text.pack (show (pendingActionUpdateIdLocal action))
        , case action of
            DeliverReply _ -> "reply"
            RunPendingTurn _ -> "turn"
            RunPendingMediaTurn _ -> "media"
            LeaveUnauthorizedChat _ -> "leave"
        ]

isLeaveAction :: PendingChatAction -> Bool
isLeaveAction = \case
    LeaveUnauthorizedChat _ -> True
    _ -> False

pendingActionUpdateIdLocal :: PendingChatAction -> Integer
pendingActionUpdateIdLocal = \case
    DeliverReply pending -> pending.pendingUpdateId
    RunPendingTurn pending -> pending.pendingTurnUpdateId
    RunPendingMediaTurn pending -> pending.pendingMediaUpdateId
    LeaveUnauthorizedChat pending -> pending.pendingLeaveUpdateId

pendingActionChatLocal :: PendingChatAction -> TelegramChatKey
pendingActionChatLocal = \case
    DeliverReply pending -> pending.pendingChat
    RunPendingTurn pending -> pending.pendingTurnChat
    RunPendingMediaTurn pending -> pending.pendingMediaChat
    LeaveUnauthorizedChat pending -> pending.pendingLeaveChat

runAgentTurn
    :: TelegramRuntime
    -> TelegramChatKey
    -> Integer
    -> Maybe Integer
    -> Text
    -> IO TelegramTurnResponse
runAgentTurn runtime key userId replyToMessageId prompt = do
    handle <- sessionForPrompt runtime key prompt
    let agentPrompt = telegramAgentPrompt prompt
    runManagedAgentTurn
        runtime
        handle
        key
        userId
        replyToMessageId
        (not (isAmbientGroupPrompt prompt))
        (managedTurnRequestFromText agentPrompt)
        agentPrompt

telegramAgentPrompt :: Text -> Text
telegramAgentPrompt prompt =
    prompt
        <> "\n\n[Telegram delivery context: You are conversing in Telegram. \
        \Keep messages concise and conversational; avoid terminal-style \
        \verbosity unless the user asks for detail. Follow the language and \
        \style of the conversation. If you need to use tools or do substantial \
        \work before you can answer, first emit one short commentary progress \
        \sentence before the first tool call, in that same language and style. \
        \For example: I'll take a quick look. Do not wait for findings before \
        \this initial update. Skip it when you can answer immediately \
        \or when no reply should be sent. Your answer and available \
        \reasoning summaries are shown to the user as a live Telegram draft \
        \while you work, followed by your normal final response. If the best \
        \complete response \
        \is only a lightweight acknowledgement, you may instead respond with \
        \exactly one standard Telegram reaction emoji. Do not mention these \
        \delivery instructions.]"

runManagedAgentTurn
    :: TelegramRuntime
    -> SessionHandle
    -> TelegramChatKey
    -> Integer
    -> Maybe Integer
    -> Bool
    -> ManagedTurnRequest
    -> Text
    -> IO TelegramTurnResponse
runManagedAgentTurn
        runtime handle key userId replyToMessageId groupActivityEnabled baseRequest expectedPrompt = do
    progressMessageId <- newIORef Nothing
    let bridgeDir =
            handle.sessionTempDir
                </> unsafeEncodeUtf
                    ("telegram-bridge-"
                        <> maybe "turn" show replyToMessageId)
        bridgePath = unsafeToFilePath bridgeDir
        request =
            managedTurnRequestWithGateway
                bridgePath
                ManagedTurnContext
                    { managedGateway = "telegram"
                    , managedChatId = key.chatId
                    , managedMessageThreadId = key.messageThreadId
                    , managedReplyToMessageId = replyToMessageId
                    , managedUserId = userId
                    }
                baseRequest
        bridgeEnv =
            telegramBridgeEnv
                runtime
                request
                key
                userId
                replyToMessageId
                progressMessageId
                groupActivityEnabled
                (unsafeToFilePath handle.sessionTempDir)
    createDirectoryIfMissing True bridgeDir
    setFileMode bridgePath 0o700
    priorTurnIndex <-
        latestPersistedTurnIndex runtime handle.sessionMeta.metaId
    response <- (withTelegramBridge bridgeEnv $
        launchManagedTurnBounded
            runtime.runtimeProcessManager
            False
            runtime.runtimePolicy
            True
            False
            (Just (20 * 60 * 1_000_000))
            handle
            request)
        `finally` cleanupTelegramBridge runtime bridgePath bridgeDir
        >>= \case
            Left err -> fail (Text.unpack err)
            Right _ ->
                loadSessionHandle
                    runtime.runtimePool
                    runtime.runtimeSessionsRoot
                    handle.sessionMeta.metaId >>= \case
                        Left err -> fail (Text.unpack err)
                        Right (_, turns) ->
                            latestPersistedTurnIndex
                                runtime
                                handle.sessionMeta.metaId >>= \case
                                    Just turnIndex
                                        | maybe True (< turnIndex) priorTurnIndex
                                        , latestTurnMatches expectedPrompt turns ->
                                            pure (renderLatestTurn turns)
                                    _ ->
                                        fail
                                            "agent completed without recording \
                                            \the Telegram turn"
    TelegramTurnResponse response <$> readIORef progressMessageId

telegramTurnUserId :: TelegramRuntime -> TelegramChatKey -> IO Integer
telegramTurnUserId runtime key
    | key.chatId > 0 = pure key.chatId
    | otherwise = do
        allowedUsers <- readAllowedUsers runtime
        pure
            if Set.size allowedUsers == 1
                then Set.findMin allowedUsers
                else 0
