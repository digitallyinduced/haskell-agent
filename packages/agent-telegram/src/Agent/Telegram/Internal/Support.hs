module Agent.Telegram.Internal.Support
    ( cleanupManagedTurnMedia, cleanupTelegramBridge, latestTurnMatches
    , latestPersistedTurnIndex, sessionForPrompt
    , renderLatestTurn, lookupBinding, modifyState, reply
    , withTelegramProgress, withTelegramProgressUsing
    , loadTelegramState, saveTelegramState
    , redactToken
    ) where


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
import qualified Agent.Json.Decode as Hermes
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
import Agent.Telegram.Internal.Text (splitTelegramText)
cleanupManagedTurnMedia :: ManagedTurnRequest -> IO ()
cleanupManagedTurnMedia request =
    forM_
        (request.managedTurnImages <> request.managedTurnFiles)
        \media ->
            void $ tryAny $
                removeFile (unsafeEncodeUtf media.managedTurnMediaPath)

cleanupTelegramBridge
    :: TelegramRuntime
    -> FilePath
    -> OsPath
    -> IO ()
cleanupTelegramBridge runtime bridgePath bridgeDir = do
    modifyState runtime \state ->
        state
            { callbackBindings =
                Map.filter
                    ((/= bridgePath) . (.callbackBindingBridgeDirectory))
                    state.callbackBindings
            }
    void $ tryAny $
        Directory.removePathForcibly (unsafeToFilePath bridgeDir)

latestTurnMatches :: Text -> [SessionTurn] -> Bool
latestTurnMatches prompt turns = case reverse turns of
    turn : _ -> turn.turnUserText == prompt
    [] -> False

latestPersistedTurnIndex
    :: TelegramRuntime
    -> Text
    -> IO (Maybe Int64)
latestPersistedTurnIndex runtime sessionId =
    loadRecentSessionTurns
        runtime.runtimePool
        runtime.runtimeSessionsRoot
        sessionId
        1 >>= \case
            Left err -> fail (Text.unpack err)
            Right page ->
                pure $ case reverse page.pageTurns of
                    (turnIndex, _) : _ -> Just turnIndex
                    [] -> Nothing

sessionForPrompt
    :: TelegramRuntime
    -> TelegramChatKey
    -> Text
    -> IO SessionHandle
sessionForPrompt runtime key prompt = do
    state <- readMVar runtime.runtimeStateVar
    case lookupBinding key state of
        Just sessionId ->
            loadSessionHandle
                runtime.runtimePool
                runtime.runtimeSessionsRoot
                sessionId >>= \case
                Left err ->
                    fail . Text.unpack $
                        "could not load Telegram session "
                            <> sessionId
                            <> ": "
                            <> err
                Right (handle, _) ->
                    pure handle
        Nothing -> do
            handle <- createSession SessionCreate
                { createPool = runtime.runtimePool
                , createRoot = runtime.runtimeSessionsRoot
                , createTarget = runtime.runtimeTarget
                , createCwd = runtime.runtimeCwd
                , createEffort = runtime.runtimeEffort
                , createTitleHint = Just (sessionTitleFromPrompt prompt)
                , createTitleIsManual = False
                }
            modifyState runtime \current ->
                current
                    { bindings =
                        Map.insert key handle.sessionMeta.metaId current.bindings
                    }
            pure handle

renderLatestTurn :: [SessionTurn] -> Text
renderLatestTurn turns =
    case reverse turns of
        [] -> "The agent completed without recording a response."
        turn : _ -> fromMaybe
            (fromMaybe "The agent completed without a text response." turn.turnError)
            turn.turnAssistantText

lookupBinding :: TelegramChatKey -> TelegramState -> Maybe Text
lookupBinding key state =
    Map.lookup key state.bindings

modifyState :: TelegramRuntime -> (TelegramState -> TelegramState) -> IO ()
modifyState runtime update =
    modifyMVar runtime.runtimeStateVar \current -> do
        let next = update current
        saveTelegramState runtime.runtimeStatePath next
        pure (next, ())

reply :: TelegramRuntime -> TelegramPendingReply -> IO ()
reply runtime pending
    | Nothing <- pending.pendingEditMessageId
    , Just emoji <- telegramReactionEmoji pending.pendingText
    , Just messageId <- pending.pendingReplyToMessageId = do
        TelegramClient.setMessageReaction
            runtime.runtimeClient
            pending.pendingChat
            messageId
            emoji >>= \case
                Right () -> pure ()
                Left _ -> sendTextReply
    | otherwise = do
        sendTextReply
  where
    sendTextReply = do
        -- Telegram accepts messages up to 4096 rendered characters.
        let chunks = case splitTelegramText 4096 pending.pendingText of
                [] -> ["(empty response)"]
                values -> values
            checkpointKey = replyCheckpointKey pending
        state <- readMVar runtime.runtimeStateVar
        let startIndex =
                fromMaybe 0
                    (Map.lookup checkpointKey state.deliveryCheckpoints)
        forM_ (drop startIndex (zip [0 :: Int ..] chunks)) \(index, chunk) ->
            case (index, pending.pendingEditMessageId) of
                (0, Just messageId) ->
                    TelegramClient.editRichMessageText
                        runtime.runtimeClient
                        pending.pendingChat
                        messageId
                        chunk >>= \case
                            Left err -> fail (Text.unpack err)
                            Right () -> checkpoint (index + 1) Nothing
                _ ->
                    TelegramClient.sendRichMessage
                        runtime.runtimeClient
                        pending.pendingChat
                        (if index == 0
                            then pending.pendingReplyToMessageId
                            else Nothing)
                        chunk >>= \case
                            Left err -> fail (Text.unpack err)
                            Right messageId -> checkpoint (index + 1) messageId
      where
        checkpoint nextIndex messageId =
            modifyState runtime \current ->
                current
                    { deliveryCheckpoints =
                        Map.insert
                            (replyCheckpointKey pending)
                            nextIndex
                            current.deliveryCheckpoints
                    , outboundMessageIds =
                        maybe
                            current.outboundMessageIds
                            (\sentId ->
                                Map.insertWith
                                    Set.union
                                    pending.pendingChat
                                    (Set.singleton sentId)
                                    current.outboundMessageIds)
                            messageId
                    }

replyCheckpointKey :: TelegramPendingReply -> Text
replyCheckpointKey pending =
    Text.intercalate ":"
        [ Text.pack (show pending.pendingChat.chatId)
        , maybe
            "-"
            (Text.pack . show)
            pending.pendingChat.messageThreadId
        , Text.pack (show pending.pendingUpdateId)
        , "reply"
        ]

withTelegramProgress
    :: TelegramClient
    -> TelegramChatKey
    -> IO a
    -> IO a
withTelegramProgress client key action =
    withTelegramProgressUsing
        (TelegramClient.sendTypingAction client key)
        (TelegramClient.sendThinkingDraft client key "Thinking…")
        action

withTelegramProgressUsing :: IO () -> IO () -> IO a -> IO a
withTelegramProgressUsing sendTyping sendDraft action = do
    -- Seed the native draft once. The managed-turn bridge takes ownership of
    -- refreshing it with live reasoning, response, and tool activity.
    void (tryAny sendDraft)
    withAsync progressLoop (const action)
  where
    progressLoop = loop
    loop = do
        -- Chat actions expire after roughly five seconds. Keep typing as a
        -- fallback for clients that do not support native rich drafts.
        void (tryAny sendTyping)
        threadDelay 4_000_000
        loop

loadTelegramState :: OsPath -> IO TelegramState
loadTelegramState path = do
    exists <- doesFileExist path
    if not exists
        then pure emptyTelegramState
        else Hermes.decodeEither telegramStateDecoder . LBS.toStrict
            <$> LBS.readFile (unsafeToFilePath path) >>= \case
            Left err -> fail ("could not decode Telegram state: "
                <> Text.unpack (Hermes.jsonErrorMessage err))
            Right state -> pure state

saveTelegramState :: OsPath -> TelegramState -> IO ()
saveTelegramState path state =
    writeLazyFileAtomically path 0o600 (encode state)

redactToken :: Text -> Text -> Text
redactToken token = Text.replace token "<redacted>"
