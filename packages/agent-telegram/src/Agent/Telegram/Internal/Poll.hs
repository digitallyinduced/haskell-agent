module Agent.Telegram.Internal.Poll
    ( pollForever, dispatchForever, scheduleTelegramWorkForever ) where


import Agent.CLI.AgentSessions
    ( SessionProcessLifetime(..)
    , SessionProcessManager
    , closeSessionProcessManager
    , launchManagedTurnBounded
    , newSessionProcessManagerWithLifetime
    )
import Agent.CLI.ManagedTurn
    ( ManagedTurnMedia(..)
    , ManagedTurnContext(..)
    , ManagedTurnRequest(..)
    , managedTurnRequestFromText
    , managedTurnRequestWithGateway
    )
import Agent.CLI.Options
    ( ApprovalPolicy(..)
    , defaultEffortFor
    )
import Agent.CLI.Models
    ( ModelOption(..)
    , ModelTarget(..)
    , defaultModelOptionFor
    , rawModelOption
    , resolveConfiguredModel
    , resolveModelOptionDialect
    )
import Agent.CLI.ModelConfig (loadModelCatalog)
import Agent.CLI.Session
    ( SessionCreate(..)
    , SessionHandle(..)
    , SessionMeta(..)
    , SessionTurn(..)
    , SessionTurnPage(..)
    , createSession
    , loadSessionHandle
    , loadRecentSessionTurns
    , sessionTitleFromPrompt
    , sessionsRoot
    )
import Agent.Telegram.Types
import Agent.Telegram.Classify
    ( TelegramUpdateAction(..)
    , ambientGroupPrompt
    , checkpointPendingVoiceTranscript
    , classifyTelegramUpdate
    , classifyTelegramUpdateWithMode
    , grantableTelegramUser
    , groupJoinAuthorized
    , isAnonymousAdmin
    , telegramAnonymousAdminUserId
    , isAmbientGroupPrompt
    , nextPendingAction
    , reactionMessageText
    , recordSeenTelegramUsers
    , resolveTelegramUser
    , storeUpdateAction
    , telegramCommand
    , telegramCommandArguments
    , telegramReactionEmoji
    , telegramReplyText
    , telegramReplyUserIdFromPrompt
    , telegramUserLabel
    , TelegramUserResolution(..)
    )
import Agent.Telegram.Bridge
    ( TelegramBridgeEnv(..)
    , processTelegramCallbacks
    , withTelegramBridge
    )
import qualified Agent.Telegram.Client as TelegramClient
import Agent.Telegram.Log (logTelegramEvent)
import Agent.Telegram.Markdown
    ( markdownToTelegramHtml
    , telegramRenderedLength
    )
import Agent.Telegram.Voice (transcribeWithXAI)
import Agent.FileRetry (writeLazyFileAtomically)
import Agent.Concurrent (mapConcurrentlyBounded)
import Agent.OsPath (unsafeToFilePath)
import Agent.Provider (Provider, parseProvider)
import Agent.ReasoningEffort (reasoningEffortText)
import Agent.Store.Postgres
    ( Store
    , managedPostgresConfigFromEnv
    , trustedPool
    , withStore
    )
import Agent.Store.Postgres.Connection (StorePool)
import Agent.Store.Types (renderStoreError)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
    ( replicateConcurrently_
    , race_
    , withAsync
    )
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , modifyMVar_
    , newMVar
    , readMVar
    )
import Control.Exception.Safe
    ( SomeException
    , bracket
    , displayException
    , finally
    , onException
    , try
    , tryAny
    )
import Control.Monad (forM_, unless, void, when)
import Data.Aeson
    ( Value(..)
    , eitherDecode
    , encode
    , object
    , (.=)
    )
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Map.Strict as Map
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock
    ( addUTCTime
    , diffUTCTime
    , getCurrentTime
    )
import qualified Network.HTTP.Client.TLS as HttpTls
import qualified System.Directory as Directory
import System.Directory.OsPath
    ( createDirectoryIfMissing
    , doesFileExist
    , getCurrentDirectory
    , getHomeDirectory
    , makeAbsolute
    , removeFile
    )
import System.Environment (getArgs, getExecutablePath, lookupEnv)
import System.Exit (die)
import System.FilePath (takeExtension)
import System.IO
    ( IOMode(AppendMode)
    , hFlush
    , hGetEcho
    , hIsTerminalDevice
    , hSetEcho
    , withFile
    , stderr
    , stdin
    )
import System.OsPath (OsPath, unsafeEncodeUtf, (</>))
import System.Posix.Files (setFileMode)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (nullSignal, sigTERM, signalProcess)
import System.Posix.Types (ProcessID)
import System.Process
    ( CreateProcess(..)
    , ProcessHandle
    , StdStream(..)
    , createProcess
    , getProcessExitCode
    , proc
    )
import qualified System.FileLock as FileLock
import Text.Read (readMaybe)
import Agent.Telegram.Internal.Runtime.Types
import Agent.Telegram.Internal.Turn
import Agent.Telegram.Internal.Allowlist
import Agent.Telegram.Internal.Support
pollForever :: TelegramRuntime -> IO ()
pollForever runtime = do
    state <- readMVar runtime.runtimeStateVar
    TelegramClient.getUpdates runtime.runtimeClient state.nextUpdateId >>= \case
        Left err -> do
            logTelegramEvent "poll_failed"
                [ "error" .=
                    redactToken runtime.runtimeClient.clientToken err
                ]
            threadDelay 2_000_000
        Right updates ->
            -- Process updates in ascending update_id order. storeUpdateAction
            -- advances the poll offset (nextUpdateId) monotonically and
            -- updateAlreadyStored drops anything at or below it, so handling a
            -- higher-id update before a lower-id one in the same batch would
            -- advance the offset past the lower-id update and silently drop it
            -- — a queued message or approval callback lost, and getUpdates
            -- never returns it again. Telegram already returns updates sorted;
            -- sort defensively to keep the offset invariant robust.
            forM_ (sortOn (.updateId) updates) (processUpdate runtime)
    pollForever runtime

processUpdate :: TelegramRuntime -> TelegramUpdate -> IO ()
processUpdate runtime update = do
    handled <- tryAny do
        modifyState runtime (recordSeenTelegramUsers update)
        action <- classifyUpdate runtime update
        modifyState runtime (storeUpdateAction update.updateId action)
    case handled of
        Left err ->
            logTelegramEvent "update_persist_failed"
                [ "update_id" .= update.updateId
                , "error" .=
                    redactToken runtime.runtimeClient.clientToken
                        (Text.pack (displayException err))
                ]
        Right () -> pure ()

classifyUpdate
    :: TelegramRuntime
    -> TelegramUpdate
    -> IO TelegramUpdateAction
classifyUpdate runtime update = do
    state <- readMVar runtime.runtimeStateVar
    classified <-
        case classifyTelegramUpdateWithMode
                runtime.runtimeBot
                state.allowedUserIds
                state.authorizedGroupChatIds
                runtime.runtimeRespondToAllGroupMessages
                update of
            ReviewGroupJoin chatId actor ->
                resolveGroupJoin runtime chatId actor
            action ->
                pure action
    pure $ case (classified, update.updateMessageReaction) of
        (LeaveUnauthorizedGroup _, _) ->
            classified
        (_, Just reaction)
            | reaction.messageReactionChat.telegramChatType /= "private"
            , not (reactionBelongsToBot state reaction) ->
                IgnoreUpdate
        _ ->
            classified

reactionBelongsToBot :: TelegramState -> TelegramMessageReaction -> Bool
reactionBelongsToBot state reaction =
    let key = TelegramChatKey
            { chatId = reaction.messageReactionChat.telegramChatId
            , messageThreadId = Nothing
            }
    in maybe False
        (Set.member reaction.messageReactionMessageId)
        (Map.lookup key state.outboundMessageIds)

resolveGroupJoin
    :: TelegramRuntime
    -> Integer
    -> TelegramUser
    -> IO TelegramUpdateAction
resolveGroupJoin runtime chatId actor = do
    allowedUsers <- readAllowedUsers runtime
    resolveGroupJoinWith allowedUsers runtime chatId actor

resolveGroupJoinWith
    :: Set Integer
    -> TelegramRuntime
    -> Integer
    -> TelegramUser
    -> IO TelegramUpdateAction
resolveGroupJoinWith allowedUsers runtime chatId actor
    | actor.userId `Set.notMember` allowedUsers
    , not (isAnonymousAdmin actor) = do
        logRejectedJoin chatId actor "adder is not an allowed user"
        pure (LeaveUnauthorizedGroup chatId)
    | otherwise =
        TelegramClient.getChatAdministrators runtime.runtimeClient chatId >>= \case
            Left err -> do
                logTelegramEvent "group_admin_lookup_failed"
                    [ "chat_id" .= chatId
                    , "user_id" .= actor.userId
                    , "error" .=
                        redactToken runtime.runtimeClient.clientToken err
                    ]
                logRejectedJoin chatId actor
                    "could not verify group administrators"
                pure (LeaveUnauthorizedGroup chatId)
            Right admins
                | groupJoinAuthorized allowedUsers actor admins -> do
                    logTelegramEvent "group_join_authorized"
                        [ "chat_id" .= chatId
                        , "user_id" .= actor.userId
                        ]
                    pure (AuthorizeGroupChat chatId)
                | otherwise -> do
                    logRejectedJoin chatId actor
                        "adder is not an allowed group administrator"
                    pure (LeaveUnauthorizedGroup chatId)

logRejectedJoin
    :: Integer
    -> TelegramUser
    -> Text
    -> IO ()
logRejectedJoin chatId actor reason =
    logTelegramEvent "group_join_rejected"
        [ "chat_id" .= chatId
        , "user_id" .= actor.userId
        , "reason" .= reason
        ]

dispatchForever :: TelegramRuntime -> IO ()
dispatchForever runtime =
    race_
        (scheduleTelegramWorkForever runtime)
        (replicateConcurrently_
            runtime.runtimeWorkerCount
            (telegramWorkerLoop runtime))

scheduleTelegramWorkForever :: TelegramRuntime -> IO ()
scheduleTelegramWorkForever runtime = do
    processTelegramCallbacks
        runtime.runtimeClient
        (readAllowedUsers runtime)
        (modifyState runtime)
        (readMVar runtime.runtimeStateVar)
    state <- readMVar runtime.runtimeStateVar
    scheduled <- readMVar runtime.runtimeScheduled
    let inactive =
            Map.keysSet state.pendingQueues
                `Set.difference`
                    scheduled
    forM_ (Set.toList inactive) \key -> do
        inserted <- modifyMVar runtime.runtimeScheduled \current ->
            if Set.member key current
                then pure (current, False)
                else pure (Set.insert key current, True)
        when inserted (writeChan runtime.runtimeWorkQueue key)
    threadDelay 250_000
    scheduleTelegramWorkForever runtime

telegramWorkerLoop :: TelegramRuntime -> IO ()
telegramWorkerLoop runtime = do
    key <- readChan runtime.runtimeWorkQueue
    processChatQueue runtime key `finally`
        modifyMVar_ runtime.runtimeScheduled
            (pure . Set.delete key)
    telegramWorkerLoop runtime

unauthorizedGroupLeaveNotice :: Text
unauthorizedGroupLeaveNotice =
    "I only join group chats when an authorized admin adds me. Leaving this group."

leaveUnauthorizedGroup
    :: TelegramRuntime
    -> TelegramPendingLeave
    -> IO ()
leaveUnauthorizedGroup runtime pending = do
    let key = pending.pendingLeaveChat
        chatId = key.chatId
    logTelegramEvent "leaving_unauthorized_group"
        [ "chat_id" .= chatId
        , "update_id" .= pending.pendingLeaveUpdateId
        ]
    TelegramClient.sendRichMessage
        runtime.runtimeClient
        key
        Nothing
        unauthorizedGroupLeaveNotice >>= \case
            Left err ->
                logTelegramEvent "unauthorized_group_notice_failed"
                    [ "chat_id" .= chatId
                    , "error" .=
                        redactToken runtime.runtimeClient.clientToken err
                    ]
            Right _ -> pure ()
    TelegramClient.leaveChat runtime.runtimeClient chatId >>= \case
        Left err
            | isBenignLeaveError err ->
                logTelegramEvent "unauthorized_group_already_left"
                    [ "chat_id" .= chatId
                    , "error" .=
                        redactToken runtime.runtimeClient.clientToken err
                    ]
            | otherwise ->
                fail (Text.unpack err)
        Right () ->
            logTelegramEvent "left_unauthorized_group"
                [ "chat_id" .= chatId
                ]

isBenignLeaveError :: Text -> Bool
isBenignLeaveError err =
    any (`Text.isInfixOf` Text.toLower err)
        [ "chat not found"
        , "bot is not a member"
        , "chat_id is empty"
        , "peer_id_invalid"
        , "bot was kicked"
        , "bot was blocked"
        ]

processChatQueue :: TelegramRuntime -> TelegramChatKey -> IO ()
processChatQueue runtime key =
    nextChatAction runtime key >>= \case
        Nothing -> pure ()
        Just action -> do
            waitForActionRetry runtime action
            result <- tryAny case action of
                DeliverReply pending -> do
                    reply runtime pending
                    modifyState runtime (completePendingAction action)
                RunPendingTurn pending -> do
                    response <- case telegramCommand pending.pendingTurnText of
                        Nothing ->
                            withTelegramProgress
                                runtime.runtimeClient
                                pending.pendingTurnChat
                                (runQueuedTurn runtime pending)
                        Just _ -> runQueuedTurn runtime pending
                    modifyState runtime \state ->
                        let completed = completePendingAction action state
                        in case telegramReplyText pending.pendingTurnText response of
                            Nothing -> completed
                            Just replyText ->
                                enqueuePendingAction
                                    (DeliverReply
                                        (TelegramPendingReply
                                            pending.pendingTurnUpdateId
                                            pending.pendingTurnChat
                                            (Just pending.pendingTurnMessageId)
                                            replyText))
                                    completed
                RunPendingMediaTurn pending -> do
                    response <- case telegramCommand pending.pendingMediaText of
                        Nothing ->
                            withTelegramProgress
                                runtime.runtimeClient
                                pending.pendingMediaChat
                                (runQueuedMediaTurn runtime pending)
                        Just _ -> runQueuedMediaTurn runtime pending
                    modifyState runtime \state ->
                        let completed = completePendingAction action state
                        in case telegramReplyText pending.pendingMediaText response of
                            Nothing -> completed
                            Just replyText ->
                                enqueuePendingAction
                                    (DeliverReply
                                        (TelegramPendingReply
                                            pending.pendingMediaUpdateId
                                            pending.pendingMediaChat
                                            (Just pending.pendingMediaMessageId)
                                            replyText))
                                    completed
                LeaveUnauthorizedChat pending -> do
                    leaveUnauthorizedGroup runtime pending
                    modifyState runtime (completePendingAction action)
            case result of
                Left err -> do
                    delay <- recordPendingFailure
                        runtime
                        action
                        (redactToken
                            runtime.runtimeClient.clientToken
                            (Text.pack (displayException err)))
                    logTelegramEvent "action_failed"
                        [ "chat_id" .= (pendingActionChatLocal action).chatId
                        , "thread_id" .=
                            (pendingActionChatLocal action).messageThreadId
                        , "update_id" .= pendingActionUpdateIdLocal action
                        , "error" .=
                            redactToken runtime.runtimeClient.clientToken
                                (Text.pack (displayException err))
                        ]
                    maybe (pure ()) threadDelay delay
                    processChatQueue runtime key
                Right () -> processChatQueue runtime key

runQueuedTurn :: TelegramRuntime -> TelegramPendingTurn -> IO Text
runQueuedTurn runtime pending =
    case telegramCommand pending.pendingTurnText of
        Just "start" -> pure
            "Send a message to start or continue an agent session. \
            \Use /new for a fresh session, /session for its ID, and /allow \
            \in a group to accept another member by name or by replying to them."
        Just "new" -> do
            modifyState runtime \state ->
                state
                    { bindings = Map.delete
                        pending.pendingTurnChat
                        state.bindings
                    }
            pure "Started a new conversation. Send your next prompt."
        Just "session" -> do
            state <- readMVar runtime.runtimeStateVar
            pure case lookupBinding pending.pendingTurnChat state of
                Nothing -> "No session yet. Send a prompt to create one."
                Just sessionId -> "Session: " <> sessionId
        Just "status" -> telegramConversationStatus runtime pending.pendingTurnChat
        Just "retry" ->
            retryLastDeadLetter
                runtime
                pending.pendingTurnChat
                (pending.pendingTurnUpdateId + 1)
        Just "allow" ->
            applyAllowlistChange
                runtime
                pending.pendingTurnChat.chatId
                AllowlistGrant
                Nothing
                (telegramCommandArguments pending.pendingTurnText)
        Just "deny" ->
            applyAllowlistChange
                runtime
                pending.pendingTurnChat.chatId
                AllowlistRevoke
                Nothing
                (telegramCommandArguments pending.pendingTurnText)
        Just "users" ->
            describeTelegramAllowlist runtime pending.pendingTurnChat.chatId
        Just command -> pure ("Unknown command: /" <> command)
        Nothing -> do
            userId <- telegramTurnUserId runtime pending.pendingTurnChat
            case pending.pendingTurnVoice of
                Nothing ->
                    runAgentTurn
                        runtime
                        pending.pendingTurnChat
                        userId
                        (Just pending.pendingTurnMessageId)
                        pending.pendingTurnText
                Just voice ->
                    tryAny (transcribeTelegramVoice runtime pending voice) >>= \case
                        Left err -> do
                            logTelegramEvent "voice_transcription_failed"
                                [ "update_id" .= pending.pendingTurnUpdateId
                                , "chat_id" .= pending.pendingTurnChat.chatId
                                , "error" .=
                                    redactToken
                                        runtime.runtimeClient.clientToken
                                        (Text.pack (displayException err))
                                ]
                            pure
                                "I could not transcribe that voice message. \
                                \Check that Codex is installed and logged in, and \
                                \that the subscription has usage available."
                        Right prompt -> do
                            let deliveredPrompt
                                    | isAmbientGroupPrompt pending.pendingTurnText =
                                        ambientGroupPrompt prompt
                                    | otherwise = prompt
                            checkpointVoiceTranscript runtime pending deliveredPrompt
                            runAgentTurn
                                runtime
                                pending.pendingTurnChat
                                userId
                                (Just pending.pendingTurnMessageId)
                                deliveredPrompt

telegramConversationStatus :: TelegramRuntime -> TelegramChatKey -> IO Text
telegramConversationStatus runtime key = do
    state <- readMVar runtime.runtimeStateVar
    let queued = maybe 0 Map.size (Map.lookup key state.pendingQueues)
        retries =
            length
                [ ()
                | action <- maybe [] Map.elems
                    (Map.lookup key state.pendingQueues)
                , Map.member (pendingRetryKey action) state.retryMetadata
                ]
        failed =
            length
                [ ()
                | dead <- state.deadLetters
                , dead.deadLetterChat == Just key
                ]
        session = fromMaybe "none" (lookupBinding key state)
    pure $ Text.unlines
        [ "Session: " <> session
        , "Queued actions: " <> Text.pack (show queued)
        , "Retrying: " <> Text.pack (show retries)
        , "Failed turns available for /retry: "
            <> Text.pack (show failed)
        ]

retryLastDeadLetter
    :: TelegramRuntime
    -> TelegramChatKey
    -> Integer
    -> IO Text
retryLastDeadLetter runtime key nextUpdateId =
    modifyMVar runtime.runtimeStateVar \state ->
        case takeLastDeadLetter key state.deadLetters of
            Nothing ->
                pure (state, "There is no failed turn to retry.")
            Just (dead, remaining) ->
                case dead.deadLetterAction of
                    Nothing ->
                        pure (state { deadLetters = remaining }
                            , "The last failure cannot be retried.")
                    Just action -> do
                        let retried = rekeyPendingAction nextUpdateId action
                            next =
                                enqueuePendingAction retried state
                                    { deadLetters = remaining
                                    }
                        saveTelegramState runtime.runtimeStatePath next
                        pure (next, "Queued the last failed turn for retry.")

takeLastDeadLetter
    :: TelegramChatKey
    -> [TelegramDeadLetter]
    -> Maybe (TelegramDeadLetter, [TelegramDeadLetter])
takeLastDeadLetter key = go [] . reverse
  where
    go _ [] = Nothing
    go skipped (dead : rest)
        | dead.deadLetterChat == Just key =
            Just (dead, reverse rest <> reverse skipped)
        | otherwise = go (dead : skipped) rest

rekeyPendingAction :: Integer -> PendingChatAction -> PendingChatAction
rekeyPendingAction updateId = \case
    DeliverReply pending ->
        DeliverReply pending { pendingUpdateId = updateId }
    RunPendingTurn pending ->
        RunPendingTurn pending { pendingTurnUpdateId = updateId }
    RunPendingMediaTurn pending ->
        RunPendingMediaTurn pending { pendingMediaUpdateId = updateId }
    LeaveUnauthorizedChat pending ->
        LeaveUnauthorizedChat pending { pendingLeaveUpdateId = updateId }

