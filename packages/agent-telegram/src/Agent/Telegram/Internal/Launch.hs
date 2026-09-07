module Agent.Telegram.Internal.Launch (runTelegram, runTelegramWithStore) where


import Agent.CLI.AgentSessions.Process
    ( SessionProcessLifetime(ScopedSessionProcesses)
    , closeSessionProcessManager
    , newSessionProcessManagerWithLifetime
    )
import Agent.CLI.ManagedTurn ()
import Agent.CLI.Runtime.Options
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
import Agent.CLI.Session (sessionsRoot)
import Agent.Telegram.Types
import Agent.Telegram.Classify ()
import Agent.Telegram.Bridge ()
import qualified Agent.Telegram.Client as TelegramClient
import Agent.Telegram.Log ()
import Agent.Telegram.Markdown ()
import Agent.Telegram.Voice ()
import Agent.FileRetry ()
import Agent.Concurrent ()
import Agent.OsPath (unsafeToFilePath)
import Agent.Provider ()
import Agent.ReasoningEffort (reasoningEffortText)
import Agent.Store.Postgres
    ( Store
    , managedPostgresConfigFromEnv
    , trustedPool
    , withStore
    )
import Agent.Store.Postgres.Connection ()
import Agent.Store.Types (renderStoreError)
import Control.Concurrent ()
import Control.Concurrent.Async (race_)
import Control.Concurrent.Chan (newChan)
import Control.Concurrent.MVar (newMVar)
import Control.Exception.Safe (finally)
import Control.Monad (when)
import Data.Aeson ()
import qualified Data.ByteString.Lazy as LBS ()
import Data.Int ()
import qualified Data.Text.Encoding as TextEncoding ()
import qualified Data.Map.Strict as Map ()
import Data.List ()
import Data.Maybe (fromMaybe)
import Data.Set ()
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock ()
import qualified Network.HTTP.Client.TLS as HttpTls
import qualified System.Directory as Directory ()
import System.Directory.OsPath
    ( createDirectoryIfMissing
    , getHomeDirectory
    , makeAbsolute
    )
import System.Environment ()
import System.Exit (die)
import System.FilePath ()
import System.IO ()
import System.OsPath (OsPath, unsafeEncodeUtf, (</>))
import System.Posix.Files (setFileMode)
import System.Posix.Process ()
import System.Posix.Signals ()
import System.Posix.Types ()
import System.Process ()
import qualified System.FileLock as FileLock ()
import Text.Read ()
import Agent.Telegram.Internal.Runtime.Types
import Agent.Telegram.Internal.Poll (pollForever, dispatchForever)
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
    let selectedOption =
            fromMaybe (defaultModelOptionFor catalog provider) configuredOption
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
