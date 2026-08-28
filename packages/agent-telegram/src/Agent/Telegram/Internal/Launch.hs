module Agent.Telegram.Internal.Launch (runTelegram, runTelegramWithStore) where


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
import Agent.Telegram.Internal.Poll (pollForever, dispatchForever, scheduleTelegramWorkForever)
import Agent.Telegram.Internal.Support (loadTelegramState, saveTelegramState)

gatewayDirectory :: OsPath -> OsPath
gatewayDirectory home =
    home
        </> unsafeEncodeUtf ".haskell-agent"
        </> unsafeEncodeUtf "gateways"
        </> unsafeEncodeUtf "telegram"

statePathFor :: OsPath -> OsPath
statePathFor home = gatewayDirectory home </> unsafeEncodeUtf "state.json"

runTelegram :: TelegramConfig -> Text -> IO ()
runTelegram config token = do
    home <- getHomeDirectory
    databaseConfig <- managedPostgresConfigFromEnv
        (unsafeToFilePath (home </> unsafeEncodeUtf ".haskell-agent"))
    withStore databaseConfig
        (\store -> runTelegramWithStore store home config token)
        >>= either
            (die . Text.unpack . renderStoreError)
            pure

runTelegramWithStore :: Store -> OsPath -> TelegramConfig -> Text -> IO ()
runTelegramWithStore store home config token = do
    cwd <- makeAbsolute (unsafeEncodeUtf config.telegramCwd)
    catalog <- loadModelCatalog home >>= either
        (die . Text.unpack)
        pure
    let provider = config.telegramProvider
        effort =
            fromMaybe
                (reasoningEffortText (defaultEffortFor provider))
                config.telegramEffort
        root = sessionsRoot home
        gatewayDir = gatewayDirectory home
        statePath = statePathFor home
        policy = case config.telegramApprovalMode of
            TelegramApprovalPrompt -> PromptMutating
            TelegramApprovalDeny -> DenyMutating
            TelegramApprovalYolo -> ApproveAll
        configuredOption = config.telegramModel >>= \model ->
            case resolveConfiguredModel catalog model of
                Just option
                    | option.modelTarget.targetProvider == provider ->
                        Just option
                _ -> Just (rawModelOption provider model)
    selectedOption <- case configuredOption of
        Just option -> pure option
        Nothing -> maybe
            (die "configured provider has no default model")
            pure
            (defaultModelOptionFor catalog provider)
    resolvedOption <- resolveModelOptionDialect selectedOption
    let target = resolvedOption.modelTarget
    createDirectoryIfMissing True gatewayDir
    setFileMode (unsafeToFilePath gatewayDir) 0o700
    loadedState <- loadTelegramState statePath
    let state = loadedState
            { allowedUserIds =
                loadedState.allowedUserIds <> config.telegramAllowedUsers
            }
    when (state /= loadedState) (saveTelegramState statePath state)
    stateVar <- newMVar state
    workQueue <- newChan
    scheduled <- newMVar Set.empty
    manager <- HttpTls.newTlsManager
    let client = TelegramClient token manager
    bot <- TelegramClient.getTelegramBot client
    processManager <-
        newSessionProcessManagerWithLifetime ScopedSessionProcesses root
    let runtime = TelegramRuntime
            { runtimeClient = client
            , runtimeBot = bot
            , runtimeRespondToAllGroupMessages =
                config.telegramRespondToAllGroupMessages
            , runtimeWorkerCount = config.telegramWorkerCount
            , runtimeGatewayDirectory = gatewayDir
            , runtimePool = trustedPool store
            , runtimeSessionsRoot = root
            , runtimeStatePath = statePath
            , runtimeStateVar = stateVar
            , runtimeWorkQueue = workQueue
            , runtimeScheduled = scheduled
            , runtimeProcessManager = processManager
            , runtimeTarget = target
            , runtimeCwd = cwd
            , runtimeEffort = effort
            , runtimePolicy = policy
            }
    Text.putStrLn "Telegram gateway started (private and group chats)."
    race_ (pollForever runtime) (dispatchForever runtime)
        `finally` do
            closeSessionProcessManager processManager
