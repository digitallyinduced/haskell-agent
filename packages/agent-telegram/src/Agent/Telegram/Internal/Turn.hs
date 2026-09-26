module Agent.Telegram.Internal.Turn where


import Agent.Runtime.AgentSessions.Process (launchManagedTurnCancellable)
import Agent.Cancel (CancelFlag, isCancelled, newCancelFlag, requestCancel)
import Agent.Runtime.ManagedTurn
    ( ManagedTurnMedia(..)
    , ManagedTurnContext(..)
    , ManagedTurnRequest(..)
    , managedTurnRequestFromText
    , managedTurnRequestWithGateway
    )
import Agent.Runtime.Session
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
    ( MVar
    , modifyMVar
    , modifyMVar_
    , newMVar
    , readMVar
    )
import Control.Exception.Safe
    ( bracket
    , bracket_
    , onException
    , tryAny
    , throwIO
    )
import Control.Monad (void, when)
import Data.Char (isAscii, isAlphaNum)
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
import Agent.Telegram.Internal.Model
    ( retargetTelegramSession
    , selectedTargetForChat
    )
import Agent.Telegram.Internal.Support

-- Allow substantial development tasks while still bounding each turn.
telegramTurnTimeoutMicros :: Int
telegramTurnTimeoutMicros = 12 * 60 * 60 * 1_000_000

data TelegramTurnResponse = TelegramTurnResponse
    { telegramTurnText :: !Text
    , telegramTurnProgressMessageId :: !(Maybe Integer)
    }

-- The polling thread signals this latch directly, never queues cancellation
-- behind the turn it needs to interrupt. A fresh latch prevents a late stop
-- from affecting the next turn in the conversation.
withTelegramTurnCancellation
    :: TelegramRuntime
    -> TelegramChatKey
    -> (CancelFlag -> IO TelegramTurnResponse)
    -> IO TelegramTurnResponse
withTelegramTurnCancellation runtime =
    withTelegramTurnCancellationUsing runtime.runtimeActiveTurns

withTelegramTurnCancellationUsing
    :: MVar (Map.Map TelegramChatKey CancelFlag)
    -> TelegramChatKey
    -> (CancelFlag -> IO TelegramTurnResponse)
    -> IO TelegramTurnResponse
withTelegramTurnCancellationUsing activeTurns key action =
    bracket
        (do
            cancellation <- newCancelFlag
            modifyMVar_ activeTurns
                (pure . Map.insert key cancellation)
            pure cancellation)
        (const (modifyMVar_ activeTurns (pure . Map.delete key)))
        \cancellation -> do
            result <- tryAny (action cancellation)
            cancelled <- isCancelled cancellation
            case result of
                Right response -> pure response
                Left exception
                    | cancelled -> pure (TelegramTurnResponse "Stopped." Nothing)
                    | otherwise -> throwIO exception

interruptTelegramTurn :: TelegramRuntime -> TelegramChatKey -> IO Bool
interruptTelegramTurn runtime =
    interruptTelegramTurnUsing runtime.runtimeActiveTurns

interruptTelegramTurnUsing
    :: MVar (Map.Map TelegramChatKey CancelFlag)
    -> TelegramChatKey
    -> IO Bool
interruptTelegramTurnUsing activeTurns key =
    modifyMVar activeTurns \active ->
        case Map.lookup key active of
            Nothing -> pure (active, False)
            Just cancellation -> do
                requestCancel cancellation
                pure (active, True)

-- Preparation (including voice download/transcription) belongs to the same
-- turn as model execution. Let its resource cleanup finish, then honor any
-- stop received during preparation before submitting work to the model.
prepareTelegramTurn
    :: CancelFlag
    -> IO a
    -> (a -> IO TelegramTurnResponse)
    -> IO TelegramTurnResponse
prepareTelegramTurn cancellation prepare continue = do
    prepared <- prepare
    cancelled <- isCancelled cancellation
    if cancelled
        then pure (TelegramTurnResponse "Stopped." Nothing)
        else continue prepared

runQueuedMediaTurn
    :: TelegramRuntime
    -> TelegramPendingMediaTurn
    -> IO TelegramTurnResponse
runQueuedMediaTurn runtime pending =
  withTelegramTurnCancellation runtime pending.pendingMediaChat \cancellation -> do
    progressMessageId <- newIORef Nothing
    handle <- sessionForSelectedPrompt
        runtime pending.pendingMediaChat pending.pendingMediaText
    let agentPrompt = telegramAgentPrompt pending.pendingMediaText
    bracket
        (downloadTelegramMediaAttachments runtime handle pending)
        cleanupTelegramMediaAttachments
        (runWithAttachments cancellation progressMessageId handle agentPrompt)
  where
   runWithAttachments cancellation progressMessageId handle agentPrompt attachments = do
    let request = telegramMediaTurnRequest agentPrompt attachments
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
    (priorTurnIndex, result) <-
        withTurnBridgeDirectory runtime bridgeDir do
          priorTurnIndex <-
              latestPersistedTurnIndex runtime handle.sessionMeta.metaId
          result <- withTelegramBridge bridgeEnv $
            launchManagedTurnCancellable cancellation
                runtime.runtimeProcessManager
                runtime.runtimePolicy
                True
                False
                (Just telegramTurnTimeoutMicros)
                handle
                gatewayRequest
          pure (priorTurnIndex, result)
    response <- case result of
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
                                        request.managedTurnText
                                        turns ->
                                        pure (renderLatestTurn turns)
                                _ ->
                                    fail
                                        "agent completed without recording \
                                        \the Telegram turn"
    TelegramTurnResponse response <$> readIORef progressMessageId

-- Install cleanup before chmod, persisted-turn lookup, or bridge startup.
withTurnBridgeDirectory :: TelegramRuntime -> OsPath -> IO a -> IO a
withTurnBridgeDirectory runtime bridgeDir action =
    bracket_
        (createDirectoryIfMissing True bridgeDir)
        (cleanupTelegramBridge runtime bridgePath bridgeDir)
        (setFileMode bridgePath 0o700 >> action)
  where
    bridgePath = unsafeToFilePath bridgeDir

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

-- Videos are local tool inputs, not generic provider file attachments. Keep
-- the same classification for request construction and resource cleanup.
telegramMediaUsesLocalPath :: TelegramMediaKind -> ManagedTurnMedia -> Bool
telegramMediaUsesLocalPath kind media =
    kind `elem` [TelegramMediaVideo, TelegramMediaVideoNote, TelegramMediaAnimation]
        || (kind == TelegramMediaDocument
            && ("video/" `Text.isPrefixOf` Text.toLower media.managedTurnMediaMime
                || extension `elem` [".mp4", ".mov", ".m4v", ".webm", ".mkv", ".avi", ".mpeg", ".mpg", ".3gp", ".3g2", ".ogv", ".wmv", ".flv"]))
  where
    extension = Text.toLower $ Text.pack $ takeExtension $
        maybe media.managedTurnMediaPath Text.unpack media.managedTurnMediaName

telegramMediaTurnRequest
    :: Text
    -> [(TelegramMediaKind, ManagedTurnMedia)]
    -> ManagedTurnRequest
telegramMediaTurnRequest prompt attachments =
    (managedTurnRequestFromText (prompt <> localReferences))
        { managedTurnImages =
            [media | (TelegramMediaPhoto, media) <- attachments]
        , managedTurnFiles =
            [ media
            | (kind, media) <- attachments
            , kind /= TelegramMediaPhoto
            , not (telegramMediaUsesLocalPath kind media)
            ]
        }
  where
    localReferences = Text.concat
        [ "\n\n[Video available as a local file]\n"
            <> renderLocalPath media.managedTurnMediaPath
            <> "\nUse local tools to inspect this file or extract frames/audio as needed."
        | (kind, media) <- attachments
        , telegramMediaUsesLocalPath kind media
        ]
    renderLocalPath path =
        let rawPath = Text.pack path
            fence = Text.replicate (1 + maximum (2 : map Text.length (Text.split (/= '`') rawPath))) "`"
        in fence <> "\n" <> rawPath <> "\n" <> fence

-- Retained videos remain in the session workspace for subsequent tool calls,
-- subject to its normal retention policy. Inline inputs can be removed after
-- import into the conversation.
cleanupTelegramMediaAttachments :: [(TelegramMediaKind, ManagedTurnMedia)] -> IO ()
cleanupTelegramMediaAttachments attachments =
    cleanupManagedTurnMedia
        [ media
        | (kind, media) <- attachments
        , not (telegramMediaUsesLocalPath kind media)
        ]

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
                            mediaInput = ManagedTurnMedia
                                    { managedTurnMediaPath =
                                        unsafeToFilePath localPath
                                    , managedTurnMediaMime =
                                        fromMaybe "application/octet-stream"
                                            file.fileMediaMimeType
                                    , managedTurnMediaName =
                                        file.fileMediaName
                                    }
                            retain = telegramMediaUsesLocalPath attachment.telegramMediaKind mediaInput
                            stagingPath = localPath <> unsafeEncodeUtf ".download"
                        retained <- if retain
                            then Directory.doesFileExist (unsafeToFilePath localPath)
                            else pure False
                        when (not retained) do
                            filePath <- getFilePath file.fileMediaFileId
                            modifyMVar_ cleanupPaths
                                (pure . ((if retain then [stagingPath, localPath] else [localPath]) <>))
                            if retain
                                then do
                                    path <- downloadFile filePath stagingPath
                                    Directory.renameFile (unsafeToFilePath path) (unsafeToFilePath localPath)
                                else void (downloadFile filePath localPath)
                        pure [(attachment.telegramMediaKind, mediaInput)]
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
                in if length ext > 1 && length ext <= 16
                        && all (\character -> isAscii character && isAlphaNum character) (drop 1 ext)
                    then Just ext
                    else Nothing
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
        if (attempts >= 5 || isTerminalTurnFailure err)
                && not (isLeaveAction action)
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
                    next = case failureReply action err of
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

failureReply :: PendingChatAction -> Text -> Maybe TelegramPendingReply
failureReply action err = case action of
    DeliverReply _ -> Nothing
    LeaveUnauthorizedChat _ -> Nothing
    RunPendingTurn pending ->
        Just TelegramPendingReply
            { pendingUpdateId = pending.pendingTurnUpdateId
            , pendingChat = pending.pendingTurnChat
            , pendingReplyToMessageId = Just pending.pendingTurnMessageId
            , pendingEditMessageId = Nothing
            , pendingText =
                failureMessage "I couldn't process this turn." err
            }
    RunPendingMediaTurn pending ->
        Just TelegramPendingReply
            { pendingUpdateId = pending.pendingMediaUpdateId
            , pendingChat = pending.pendingMediaChat
            , pendingReplyToMessageId = Just pending.pendingMediaMessageId
            , pendingEditMessageId = Nothing
            , pendingText =
                failureMessage "I couldn't process this media turn." err
            }

failureMessage :: Text -> Text -> Text
failureMessage prefix err
    | isTerminalTurnFailure err =
        prefix <> "\n\nReason: " <> err
            <> "\n\nUse /model to switch models, then send /retry."
    | otherwise =
        prefix <> " after 5 attempts. Send /retry to try it again."

isTerminalTurnFailure :: Text -> Bool
isTerminalTurnFailure err =
    "no fallback account is available" `Text.isInfixOf` Text.toLower err

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
    :: CancelFlag
    -> TelegramRuntime
    -> TelegramChatKey
    -> Integer
    -> Maybe Integer
    -> Text
    -> IO TelegramTurnResponse
runAgentTurn cancellation runtime key userId replyToMessageId prompt =
  prepareTelegramTurn cancellation (sessionForSelectedPrompt runtime key prompt) \handle -> do
    let agentPrompt = telegramAgentPrompt prompt
    runManagedAgentTurn
        cancellation
        runtime
        handle
        key
        userId
        replyToMessageId
        (not (isAmbientGroupPrompt prompt))
        (managedTurnRequestFromText agentPrompt)
        agentPrompt

sessionForSelectedPrompt
    :: TelegramRuntime -> TelegramChatKey -> Text -> IO SessionHandle
sessionForSelectedPrompt runtime key prompt = do
    selectedTargetForChat runtime key >>= \case
        Left err -> fail (Text.unpack err)
        Right selected -> do
            handle <- sessionForPrompt runtime key prompt
            case selected of
                Nothing -> pure handle
                Just (target, gatewayIdentity) ->
                    retargetTelegramSession handle target gatewayIdentity

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
        \while you work, followed by your normal final response. When the user \
        \asks you to implement a non-trivial software feature and the \
        \create_agent_session and wait_agent_session tools are available, act \
        \as the coordinator: delegate implementation and testing to a new \
        \persisted session, wait for its current turn with wait_agent_session \
        \(waiting again after a timeout when needed), inspect its result, and \
        \retain responsibility for verification and the final answer. Do not \
        \use this indirection for small edits or questions, and do not modify \
        \the same files concurrently with the delegated session. If the best \
        \complete response \
        \is only a lightweight acknowledgement, you may instead respond with \
        \exactly one standard Telegram reaction emoji. Do not mention these \
        \delivery instructions.]"

runManagedAgentTurn
    :: CancelFlag
    -> TelegramRuntime
    -> SessionHandle
    -> TelegramChatKey
    -> Integer
    -> Maybe Integer
    -> Bool
    -> ManagedTurnRequest
    -> Text
    -> IO TelegramTurnResponse
runManagedAgentTurn
        cancellation runtime handle key userId replyToMessageId groupActivityEnabled baseRequest expectedPrompt = do
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
    (priorTurnIndex, result) <- withTurnBridgeDirectory runtime bridgeDir do
      priorTurnIndex <-
          latestPersistedTurnIndex runtime handle.sessionMeta.metaId
      result <- withTelegramBridge bridgeEnv $
        launchManagedTurnCancellable cancellation
            runtime.runtimeProcessManager
            runtime.runtimePolicy
            True
            False
            (Just telegramTurnTimeoutMicros)
            handle
            request
      pure (priorTurnIndex, result)
    response <- case result of
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
