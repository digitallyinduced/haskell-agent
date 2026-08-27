module Agent.Telegram.Internal.Allowlist where


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
import Agent.Telegram.Internal.Support

readAllowedUsers :: TelegramRuntime -> IO (Set Integer)
readAllowedUsers runtime =
    (.allowedUserIds) <$> readMVar runtime.runtimeStateVar

data AllowlistChange
    = AllowlistGrant
    | AllowlistRevoke

telegramBridgeEnv
    :: TelegramRuntime
    -> ManagedTurnRequest
    -> TelegramChatKey
    -> Integer
    -> Maybe Integer
    -> FilePath
    -> TelegramBridgeEnv
telegramBridgeEnv runtime request key userId replyTo allowedRoot =
    TelegramBridgeEnv
        { telegramBridgeClient = runtime.runtimeClient
        , telegramBridgeRequest = request
        , telegramBridgeChat = key
        , telegramBridgeUserId = userId
        , telegramBridgeReplyTo = replyTo
        , telegramBridgeAllowedRoot = allowedRoot
        , telegramBridgeModifyState = modifyState runtime
        , telegramBridgeGrantUser =
            applyAllowlistChange
                runtime
                key.chatId
                AllowlistGrant
                (telegramReplyUserIdFromPrompt request.managedTurnText)
        , telegramBridgeRevokeUser =
            applyAllowlistChange
                runtime
                key.chatId
                AllowlistRevoke
                (telegramReplyUserIdFromPrompt request.managedTurnText)
        , telegramBridgeListUsers =
            describeTelegramAllowlist runtime key.chatId
        }

applyAllowlistChange
    :: TelegramRuntime
    -> Integer
    -> AllowlistChange
    -> Maybe Integer
    -> Text
    -> IO Text
applyAllowlistChange runtime chatId change fallbackUserId query = do
    state <- readMVar runtime.runtimeStateVar
    case resolveTelegramUser state chatId fallbackUserId query of
        UnresolvedTelegramUser err ->
            pure err
        AmbiguousTelegramUsers users ->
            pure $
                "Multiple people match that request:\n"
                    <> Text.unlines
                        [ "- " <> telegramUserLabel user
                        | user <- users
                        ]
                    <> "Pass a more specific @username or reply to one of \
                       \their messages."
        ResolvedTelegramUser user ->
            case change of
                AllowlistGrant -> grantAllowedUser runtime chatId user
                AllowlistRevoke -> revokeAllowedUser runtime user

grantAllowedUser
    :: TelegramRuntime
    -> Integer
    -> TelegramUser
    -> IO Text
grantAllowedUser runtime chatId user = do
    alreadyAllowed <- modifyMVar runtime.runtimeStateVar \state -> do
        let already = Set.member user.userId state.allowedUserIds
            next = state
                { allowedUserIds = Set.insert user.userId state.allowedUserIds
                , seenTelegramUsers =
                    Map.insert user.userId user state.seenTelegramUsers
                , seenUsersByChat =
                    Map.insertWith
                        Set.union
                        chatId
                        (Set.singleton user.userId)
                        state.seenUsersByChat
                }
        saveTelegramState runtime.runtimeStatePath next
        pure (next, already)
    persistAllowedUserIdsToConfig runtime
    logTelegramEvent "allowed_user_granted"
        [ "chat_id" .= chatId
        , "user_id" .= user.userId
        ]
    pure
        if alreadyAllowed
            then telegramUserLabel user
                <> " is already allowed to talk to this bot."
            else
                "Now accepting messages from "
                    <> telegramUserLabel user
                    <> "."

revokeAllowedUser :: TelegramRuntime -> TelegramUser -> IO Text
revokeAllowedUser runtime user = do
    result <- modifyMVar runtime.runtimeStateVar \state ->
        let remaining = Set.delete user.userId state.allowedUserIds
        in if Set.null remaining
            then pure
                (state, Left "Refusing to remove the last allowed Telegram user.")
            else if user.userId `Set.notMember` state.allowedUserIds
                then pure
                    ( state
                    , Left
                        (telegramUserLabel user <> " is not on the allowlist.")
                    )
                else do
                    let next = state { allowedUserIds = remaining }
                    saveTelegramState runtime.runtimeStatePath next
                    pure (next, Right remaining)
    case result of
        Left message -> pure message
        Right remaining -> do
            persistAllowedUserIdsToConfig runtime
            logTelegramEvent "allowed_user_revoked"
                [ "user_id" .= user.userId
                , "remaining" .= Set.size remaining
                ]
            pure
                ("No longer accepting messages from "
                    <> telegramUserLabel user
                    <> ".")

describeTelegramAllowlist :: TelegramRuntime -> Integer -> IO Text
describeTelegramAllowlist runtime chatId = do
    state <- readMVar runtime.runtimeStateVar
    let allowed =
            [ maybe
                ("user " <> Text.pack (show userId))
                telegramUserLabel
                (Map.lookup userId state.seenTelegramUsers)
            | userId <- Set.toAscList state.allowedUserIds
            ]
        seen =
            [ telegramUserLabel user
            | userId <-
                Set.toAscList
                    (fromMaybe Set.empty (Map.lookup chatId state.seenUsersByChat))
            , Just user <- [Map.lookup userId state.seenTelegramUsers]
            , grantableTelegramUser user
            ]
    pure $
        "Allowed users:\n"
            <> formatLines allowed
            <> if chatId < 0 && not (null seen)
                then "\nSeen in this chat:\n" <> formatLines seen
                else ""
  where
    formatLines values =
        Text.unlines [ "- " <> value | value <- values ]

persistAllowedUserIdsToConfig :: TelegramRuntime -> IO ()
persistAllowedUserIdsToConfig runtime = do
    users <- readAllowedUsers runtime
    let path =
            runtime.runtimeGatewayDirectory
                </> unsafeEncodeUtf "config.json"
    result <- tryAny do
        exists <- doesFileExist path
        when exists do
            bytes <- LBS.readFile (unsafeToFilePath path)
            case eitherDecode bytes of
                Left err ->
                    fail ("could not decode Telegram config: " <> err)
                Right config ->
                    writeLazyFileAtomically
                        path
                        0o600
                        (encode config { telegramAllowedUsers = users })
    case result of
        Left err ->
            logTelegramEvent "allowlist_config_persist_failed"
                [ "error" .=
                    redactToken
                        runtime.runtimeClient.clientToken
                        (Text.pack (displayException err))
                ]
        Right () -> pure ()

