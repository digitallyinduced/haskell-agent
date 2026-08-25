-- | Dedicated Telegram gateway backed by persisted agent sessions.
module Agent.Telegram
    ( telegramMain
    , parseAllowedUsers
    , splitTelegramText
    , markdownToTelegramHtml
    , withTelegramProgressUsing
    , TelegramConfig(..)
    , TelegramCommand(..)
    , TelegramSetupOptions(..)
    , defaultTelegramSetupOptions
    , parseTelegramArgs
    , TelegramUsersCommand(..)
    , TelegramChatKey(..)
    , TelegramState(..)
    , TelegramPendingTurn(..)
    , TelegramPendingReply(..)
    , TelegramVoice(..)
    , TelegramUser(..)
    , TelegramMessage(..)
    , TelegramUpdate(..)
    , TelegramUpdateAction(..)
    , PendingChatAction(..)
    , emptyTelegramState
    , classifyTelegramUpdate
    , classifyTelegramUpdateWithMode
    , storeUpdateAction
    , nextPendingAction
    , checkpointPendingVoiceTranscript
    , reactionMessageText
    , telegramReactionEmoji
    , telegramReplyText
    , transcribeWithCodex
    , downloadTelegramMediaAttachmentsWith
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
    , createSession
    , loadSessionHandle
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
    , isAmbientGroupPrompt
    , nextPendingAction
    , reactionMessageText
    , storeUpdateAction
    , telegramCommand
    , telegramReactionEmoji
    , telegramReplyText
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
import Agent.Telegram.Voice (transcribeWithCodex)
import Agent.FileRetry (writeLazyFileAtomically)
import Agent.Concurrent (mapConcurrentlyBounded)
import Agent.OsPath (unsafeToFilePath)
import Agent.Provider (Provider, parseProvider)
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
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Map.Strict as Map
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

parseAllowedUsers :: Text -> Either Text (Set Integer)
parseAllowedUsers raw = do
    let values = filter (not . Text.null) $
            Text.strip <$> Text.splitOn "," raw
    users <- traverse parseUser values
    if null users
        then Left "TELEGRAM_ALLOWED_USERS must contain at least one numeric user ID"
        else Right (Set.fromList users)
  where
    parseUser value = case readMaybe (Text.unpack value) of
        Just userId | userId > 0 -> Right userId
        _ -> Left ("invalid Telegram user ID: " <> value)

splitTelegramText :: Int -> Text -> [Text]
splitTelegramText limit text
    | limit < 1 = []
    | Text.null text = []
    | telegramRenderedLength text <= limit = [text]
    | otherwise =
        let fitting = largestFittingPrefix text
            prefix = Text.take fitting text
            suffix = Text.drop fitting text
            splitAtBoundary
                | Text.null suffix = Text.length prefix
                | otherwise =
                    fromMaybe (Text.length prefix)
                        (preferredBoundary prefix)
            (chunk, rest) = Text.splitAt splitAtBoundary text
        in chunk : splitTelegramText limit rest
  where
    largestFittingPrefix value = search 1 (Text.length value)
      where
        search low high
            | low >= high = low
            | renderedLength midpoint <= limit = search midpoint high
            | otherwise = search low (midpoint - 1)
          where
            midpoint = low + (high - low + 1) `div` 2

        renderedLength =
            telegramRenderedLength . (`Text.take` value)

    preferredBoundary prefix =
        let reverseIndex character =
                (\index -> Text.length prefix - index - 1)
                    <$> Text.findIndex (== character) (Text.reverse prefix)
            newline = reverseIndex '\n'
            space = reverseIndex ' '
            boundary = max newline space
            minimumUseful = max 1 (limit `div` 2)
        in case boundary of
            Just index | index + 1 >= minimumUseful -> Just (index + 1)
            _ -> Nothing

telegramMain :: IO ()
telegramMain =
    getArgs >>= either die executeTelegramCommand . parseTelegramArgs

parseTelegramArgs :: [String] -> Either String TelegramCommand
parseTelegramArgs = \case
    [] -> Right TelegramRun
    ["run"] -> Right TelegramRun
    ["start"] -> Right TelegramStart
    ["stop"] -> Right TelegramStop
    ["status"] -> Right TelegramStatus
    ["users", "list"] -> Right (TelegramUsers TelegramUsersList)
    ["users", "add", value] ->
        TelegramUsers . TelegramUsersAdd <$> parseUserId value
    ["users", "remove", value] ->
        TelegramUsers . TelegramUsersRemove <$> parseUserId value
    ["--help"] -> Right TelegramHelp
    ["-h"] -> Right TelegramHelp
    ["--version"] -> Right TelegramVersion
    "setup" : rest -> TelegramSetup <$> parseSetupOptions rest
    _ -> Left telegramUsage
  where
    parseUserId value =
        case readMaybe value of
            Just userId | userId > 0 -> Right userId
            _ -> Left ("invalid Telegram user ID: " <> value)

parseSetupOptions :: [String] -> Either String TelegramSetupOptions
parseSetupOptions = go defaultTelegramSetupOptions
  where
    go options = \case
        [] -> Right options
        "--provider" : value : rest ->
            case parseProvider (Text.pack value) of
                Nothing -> Left ("unknown provider: " <> value)
                Just provider -> go options { setupProvider = Just provider } rest
        "--model" : value : rest ->
            go options { setupModel = Just (Text.pack value) } rest
        "--cwd" : value : rest ->
            go options { setupCwd = Just value } rest
        "--effort" : value : rest ->
            go options { setupEffort = Just (Text.pack value) } rest
        "--allowed-user" : value : rest ->
            case readMaybe value of
                Just userId | userId > 0 ->
                    go options
                        { setupAllowedUsers =
                            options.setupAllowedUsers <> [userId]
                        }
                        rest
                _ -> Left ("invalid Telegram user ID: " <> value)
        "--yolo" : rest ->
            go options { setupApprovalMode = TelegramApprovalYolo } rest
        "--deny-mutations" : rest ->
            go options { setupApprovalMode = TelegramApprovalDeny } rest
        "--all-group-messages" : rest ->
            go options { setupRespondToAllGroupMessages = True } rest
        "--workers" : value : rest ->
            case readMaybe value of
                Just workers
                    | workers >= 1
                    , workers <= maximumTelegramWorkerCount ->
                        go options { setupWorkerCount = workers } rest
                _ -> Left
                    ("workers must be between 1 and "
                        <> show maximumTelegramWorkerCount)
        "--start" : rest -> go options { setupStart = True } rest
        flag : _ -> Left ("unknown setup option: " <> flag <> "\n\n" <> telegramUsage)

executeTelegramCommand :: TelegramCommand -> IO ()
executeTelegramCommand = \case
    TelegramSetup options -> setupTelegram options
    TelegramRun -> runConfiguredTelegram
    TelegramStart -> startTelegram
    TelegramStop -> stopTelegram
    TelegramStatus -> telegramStatus
    TelegramUsers command -> manageTelegramUsers command
    TelegramHelp -> putStrLn telegramUsage
    TelegramVersion -> putStrLn "agent-telegram 0.1.0.0"

telegramUsage :: String
telegramUsage = unlines
    [ "Usage: agent-telegram setup [OPTIONS]"
    , "       agent-telegram run"
    , "       agent-telegram start"
    , "       agent-telegram stop"
    , "       agent-telegram status"
    , "       agent-telegram users list|add ID|remove ID"
    , ""
    , "Setup options:"
    , "  --provider NAME       openai, xai, or openrouter"
    , "  --model NAME          optional model override"
    , "  --cwd PATH            agent working directory"
    , "  --effort LEVEL        optional reasoning effort"
    , "  --allowed-user ID     numeric Telegram user ID"
    , "                          may be repeated"
    , "  --yolo                auto-approve mutating agent tools"
    , "  --deny-mutations       deny mutating agent tools"
    , "                          default: ask with Telegram buttons"
    , "  --all-group-messages  consider every allowed-user group message"
    , "                          and reply only when useful"
    , "  --workers N           concurrent chat workers (1-64, default: 8)"
    , "  --start               start the gateway after setup"
    ]

manageTelegramUsers :: TelegramUsersCommand -> IO ()
manageTelegramUsers command = do
    home <- getHomeDirectory
    configured <- doesFileExist (configPath home)
    unless configured (die "Telegram is not configured. Run `agent-telegram setup` first.")
    config <- loadTelegramConfig home
    let users = config.telegramAllowedUsers
        updated = case command of
            TelegramUsersList -> config
            TelegramUsersAdd userId ->
                config { telegramAllowedUsers = Set.insert userId users }
            TelegramUsersRemove userId ->
                config { telegramAllowedUsers = Set.delete userId users }
    case command of
        TelegramUsersList ->
            forM_ (Set.toAscList users) (Text.putStrLn . Text.pack . show)
        _ -> do
            when (Set.null updated.telegramAllowedUsers) $
                die "refusing to remove the last allowed Telegram user"
            writeLazyFileAtomically
                (configPath home)
                0o600
                (encode updated)
            Text.putStrLn "Telegram allowlist updated. Restart the gateway to apply it."

telegramStatus :: IO ()
telegramStatus = do
    home <- getHomeDirectory
    running <- telegramIsRunning
    stateExists <- doesFileExist (statePathFor home)
    state <-
        if stateExists
            then loadTelegramState (statePathFor home)
            else pure emptyTelegramState
    let queued = sum (map Map.size (Map.elems state.pendingQueues))
        callbacks = Map.size state.pendingCallbacks
        retries = Map.size state.retryMetadata
        failed = length state.deadLetters
    Text.putStrLn $
        (if running then "agent-telegram is running" else "agent-telegram is stopped")
            <> " · queued "
            <> Text.pack (show queued)
            <> " · callbacks "
            <> Text.pack (show callbacks)
            <> " · retries "
            <> Text.pack (show retries)
            <> " · failed "
            <> Text.pack (show failed)

gatewayDirectory :: OsPath -> OsPath
gatewayDirectory home =
    home
        </> unsafeEncodeUtf ".haskell-agent"
        </> unsafeEncodeUtf "gateways"
        </> unsafeEncodeUtf "telegram"

configPath, tokenPath, statePathFor, pidPath, logPath, lockPath :: OsPath -> OsPath
configPath home = gatewayDirectory home </> unsafeEncodeUtf "config.json"
tokenPath home = gatewayDirectory home </> unsafeEncodeUtf "token"
statePathFor home = gatewayDirectory home </> unsafeEncodeUtf "state.json"
pidPath home = gatewayDirectory home </> unsafeEncodeUtf "agent-telegram.pid"
logPath home = gatewayDirectory home </> unsafeEncodeUtf "agent-telegram.log"
lockPath home = gatewayDirectory home </> unsafeEncodeUtf "agent-telegram.lock"

setupTelegram :: TelegramSetupOptions -> IO ()
setupTelegram options = do
    home <- getHomeDirectory
    provider <- maybe promptProvider pure options.setupProvider
    cwd <- case options.setupCwd of
        Nothing -> unsafeToFilePath <$> getCurrentDirectory
        Just value -> unsafeToFilePath <$> makeAbsolute (unsafeEncodeUtf value)
    allowedUsers <- case options.setupAllowedUsers of
        [] -> Set.singleton <$> promptAllowedUser
        values -> pure (Set.fromList values)
    token <- readSecretLine "BotFather token: " >>= maybe
        (die "a Telegram bot token is required")
        pure
    manager <- HttpTls.newTlsManager
    let client = TelegramClient token manager
    TelegramClient.telegramRequest client "getMe" (object []) 15 >>= \case
        Left err -> die (Text.unpack err)
        Right response -> case
                TelegramClient.decodeTelegramResponse response
                    :: Either Text Value
            of
            Left err -> die (Text.unpack err)
            Right _ -> pure ()
    let directory = gatewayDirectory home
        config = TelegramConfig
            { telegramProvider = provider
            , telegramModel = options.setupModel
            , telegramCwd = cwd
            , telegramEffort = options.setupEffort
            , telegramApprovalMode = options.setupApprovalMode
            , telegramAllowedUsers = allowedUsers
            , telegramRespondToAllGroupMessages =
                options.setupRespondToAllGroupMessages
            , telegramWorkerCount = options.setupWorkerCount
            }
    createDirectoryIfMissing True directory
    setFileMode (unsafeToFilePath directory) 0o700
    writeLazyFileAtomically (configPath home) 0o600 (encode config)
    writeLazyFileAtomically (tokenPath home) 0o600
        (LBS.fromStrict (TextEncoding.encodeUtf8 token))
    Text.putStrLn "Telegram gateway configured."
    when options.setupStart startTelegram

promptProvider :: IO Provider
promptProvider = do
    Text.hPutStr stderr "Provider [openai]: "
    hFlush stderr
    value <- Text.strip <$> Text.getLine
    let selected = if Text.null value then "openai" else value
    maybe (die ("unknown provider: " <> Text.unpack selected)) pure
        (parseProvider selected)

promptAllowedUser :: IO Integer
promptAllowedUser = do
    Text.hPutStr stderr "Allowed Telegram user ID: "
    hFlush stderr
    Text.getLine >>= \value -> case readMaybe (Text.unpack (Text.strip value)) of
        Just userId | userId > 0 -> pure userId
        _ -> die "Telegram user ID must be a positive integer"

readSecretLine :: Text -> IO (Maybe Text)
readSecretLine prompt = do
    tty <- hIsTerminalDevice stdin
    unless tty $
        die "Telegram setup requires an interactive terminal for secret entry."
    Text.hPutStr stderr prompt
    hFlush stderr
    oldEcho <- hGetEcho stdin
    value <- bracket
        (hSetEcho stdin False)
        (const (hSetEcho stdin oldEcho))
        (const Text.getLine)
        <* Text.hPutStrLn stderr ""
    pure case Text.strip value of
        "" -> Nothing
        token -> Just token

loadTelegramConfig :: OsPath -> IO TelegramConfig
loadTelegramConfig home =
    eitherDecode <$> LBS.readFile (unsafeToFilePath (configPath home)) >>= \case
        Left err -> die ("could not decode Telegram config: " <> err)
        Right config -> pure config

loadTelegramToken :: OsPath -> IO Text
loadTelegramToken home =
    lookupEnv "TELEGRAM_BOT_TOKEN" >>= \case
        Just value | not (null value) -> pure (Text.pack value)
        _ -> Text.strip . TextEncoding.decodeUtf8 . LBS.toStrict
            <$> LBS.readFile (unsafeToFilePath (tokenPath home))

runConfiguredTelegram :: IO ()
runConfiguredTelegram = do
    home <- getHomeDirectory
    configExists <- doesFileExist (configPath home)
    unless configExists $
        die "Telegram is not configured. Run `agent-telegram setup` first."
    config <- loadTelegramConfig home
    token <- loadTelegramToken home
    FileLock.tryLockFile
        (unsafeToFilePath (lockPath home))
        FileLock.Exclusive >>= \case
            Nothing -> die "agent-telegram is already running"
            Just lock -> bracket
                (pure lock)
                FileLock.unlockFile
                \_ -> do
                    pid <- getProcessID
                    writeLazyFileAtomically (pidPath home) 0o600
                        (LBS.fromStrict
                            (TextEncoding.encodeUtf8 (Text.pack (show pid))))
                    runTelegram config token `finally` removePidFile home pid

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
        effort = fromMaybe (defaultEffortFor provider) config.telegramEffort
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
    state <- loadTelegramState statePath
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
            , runtimeAllowedUsers = config.telegramAllowedUsers
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

removePidFile :: OsPath -> ProcessID -> IO ()
removePidFile home expected = do
    current <- loadPid home
    when (current == Just expected) do
        _ <- try @_ @SomeException (removeFile (pidPath home))
        pure ()

startTelegram :: IO ()
startTelegram = do
    running <- telegramIsRunning
    when running (die "agent-telegram is already running")
    home <- getHomeDirectory
    configured <- doesFileExist (configPath home)
    unless configured (die "Telegram is not configured. Run `agent-telegram setup` first.")
    createDirectoryIfMissing True (gatewayDirectory home)
    executable <- getExecutablePath
    withFile (unsafeToFilePath (logPath home)) AppendMode \logHandle -> do
        (_, _, _, process) <- createProcess (proc executable ["run"])
            { std_in = NoStream
            , std_out = UseHandle logHandle
            , std_err = UseHandle logHandle
            , create_group = True
            }
        waitForGatewayStart home process 100
    Text.putStrLn "agent-telegram started"

waitForGatewayStart :: OsPath -> ProcessHandle -> Int -> IO ()
waitForGatewayStart _ process 0 = do
    exitCode <- getProcessExitCode process
    die ("agent-telegram did not start" <> maybe "" ((": " <>) . show) exitCode)
waitForGatewayStart home process attempts = do
    running <- telegramIsRunning
    if running
        then pure ()
        else getProcessExitCode process >>= \case
            Just exitCode -> die ("agent-telegram exited during startup: " <> show exitCode)
            Nothing -> threadDelay 50_000 >> waitForGatewayStart home process (attempts - 1)

stopTelegram :: IO ()
stopTelegram = do
    home <- getHomeDirectory
    loadPid home >>= \case
        Nothing -> Text.putStrLn "agent-telegram is not running"
        Just pid -> do
            alive <- processIsAlive pid
            if alive
                then signalProcess sigTERM pid >> Text.putStrLn "agent-telegram stopped"
                else Text.putStrLn "agent-telegram is not running"
            _ <- try @_ @SomeException (removeFile (pidPath home))
            pure ()

telegramIsRunning :: IO Bool
telegramIsRunning = do
    home <- getHomeDirectory
    createDirectoryIfMissing True (gatewayDirectory home)
    FileLock.tryLockFile
        (unsafeToFilePath (lockPath home))
        FileLock.Exclusive >>= \case
            Nothing -> pure True
            Just lock -> FileLock.unlockFile lock >> pure False

loadPid :: OsPath -> IO (Maybe ProcessID)
loadPid home = do
    exists <- doesFileExist (pidPath home)
    if not exists
        then pure Nothing
        else readMaybe . Text.unpack . Text.strip . TextEncoding.decodeUtf8 . LBS.toStrict
            <$> LBS.readFile (unsafeToFilePath (pidPath home))

processIsAlive :: ProcessID -> IO Bool
processIsAlive pid =
    try @_ @SomeException (signalProcess nullSignal pid) >>= \case
        Left _ -> pure False
        Right () -> pure True

data TelegramRuntime = TelegramRuntime
    { runtimeClient :: !TelegramClient
    , runtimeBot :: !TelegramUser
    , runtimeAllowedUsers :: !(Set Integer)
    , runtimeRespondToAllGroupMessages :: !Bool
    , runtimeWorkerCount :: !Int
    , runtimeGatewayDirectory :: !OsPath
    , runtimePool :: !StorePool
    , runtimeSessionsRoot :: !OsPath
    , runtimeStatePath :: !OsPath
    , runtimeStateVar :: !(MVar TelegramState)
    , runtimeWorkQueue :: !(Chan TelegramChatKey)
    , runtimeScheduled :: !(MVar (Set TelegramChatKey))
    , runtimeProcessManager :: !SessionProcessManager
    , runtimeTarget :: !ModelTarget
    , runtimeCwd :: !OsPath
    , runtimeEffort :: !Text
    , runtimePolicy :: !ApprovalPolicy
    }

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
            forM_ updates (processUpdate runtime)
    pollForever runtime

processUpdate :: TelegramRuntime -> TelegramUpdate -> IO ()
processUpdate runtime update = do
    handled <- tryAny do
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
classifyUpdate runtime update =
    case update.updateMessageReaction of
        Just reaction
            | reaction.messageReactionChat.telegramChatType /= "private" -> do
                state <- readMVar runtime.runtimeStateVar
                let key = TelegramChatKey
                        { chatId =
                            reaction.messageReactionChat.telegramChatId
                        , messageThreadId = Nothing
                        }
                    belongsToBot =
                        maybe False
                            (Set.member reaction.messageReactionMessageId)
                            (Map.lookup key state.outboundMessageIds)
                pure $
                    if belongsToBot
                        then classifyTelegramUpdateWithMode
                            runtime.runtimeBot
                            runtime.runtimeAllowedUsers
                            runtime.runtimeRespondToAllGroupMessages
                            update
                        else IgnoreUpdate
        _ ->
            pure (classifyTelegramUpdateWithMode
                runtime.runtimeBot
                runtime.runtimeAllowedUsers
                runtime.runtimeRespondToAllGroupMessages
                update)

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
        runtime.runtimeAllowedUsers
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
            \Use /new for a fresh session and /session for its ID."
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
        Just command -> pure ("Unknown command: /" <> command)
        Nothing -> case pending.pendingTurnVoice of
            Nothing ->
                runAgentTurn
                    runtime
                    pending.pendingTurnChat
                    (telegramTurnUserId runtime pending.pendingTurnChat)
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
                            (telegramTurnUserId runtime pending.pendingTurnChat)
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

runQueuedMediaTurn :: TelegramRuntime -> TelegramPendingMediaTurn -> IO Text
runQueuedMediaTurn runtime pending = do
    handle <- sessionForPrompt runtime pending.pendingMediaChat pending.pendingMediaText
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
            , managedTurnText = pending.pendingMediaText
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
        bridgeEnv = TelegramBridgeEnv
            { telegramBridgeClient = runtime.runtimeClient
            , telegramBridgeRequest = gatewayRequest
            , telegramBridgeChat = pending.pendingMediaChat
            , telegramBridgeUserId = pending.pendingMediaUserId
            , telegramBridgeReplyTo = Just pending.pendingMediaMessageId
            , telegramBridgeAllowedRoot =
                unsafeToFilePath handle.sessionTempDir
            , telegramBridgeModifyState = modifyState runtime
            }
    createDirectoryIfMissing True bridgeDir
    setFileMode bridgePath 0o700
    priorTurnCount <- loadSessionHandle
        runtime.runtimePool
        runtime.runtimeSessionsRoot
        handle.sessionMeta.metaId >>= \case
            Left err -> fail (Text.unpack err)
            Right (_, turns) -> pure (length turns)
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
    (case result of
        Left err -> fail (Text.unpack err)
        Right _ ->
            loadSessionHandle
                runtime.runtimePool
                runtime.runtimeSessionsRoot
                handle.sessionMeta.metaId >>= \case
                    Left err -> fail (Text.unpack err)
                    Right (_, turns)
                        | length turns > priorTurnCount
                        , latestTurnMatches pending.pendingMediaText turns ->
                            pure (renderLatestTurn turns)
                        | otherwise ->
                            fail
                                "agent completed without recording \
                                \the Telegram turn")
        `finally` cleanupManagedTurnMedia request

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
            transcript <- transcribeWithCodex
                transcriptionCwd
                (unsafeToFilePath path)
            let clean = Text.strip transcript
            when (Text.null clean) $
                fail "Codex returned an empty voice transcription"
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
        if attempts >= 5
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
                let seconds = min 60 (2 ^ attempts)
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
    RunPendingTurn pending ->
        Just TelegramPendingReply
            { pendingUpdateId = pending.pendingTurnUpdateId
            , pendingChat = pending.pendingTurnChat
            , pendingReplyToMessageId = Just pending.pendingTurnMessageId
            , pendingText =
                "This turn failed after 5 attempts. Send /retry to try it again."
            }
    RunPendingMediaTurn pending ->
        Just TelegramPendingReply
            { pendingUpdateId = pending.pendingMediaUpdateId
            , pendingChat = pending.pendingMediaChat
            , pendingReplyToMessageId = Just pending.pendingMediaMessageId
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
        ]

pendingActionUpdateIdLocal :: PendingChatAction -> Integer
pendingActionUpdateIdLocal = \case
    DeliverReply pending -> pending.pendingUpdateId
    RunPendingTurn pending -> pending.pendingTurnUpdateId
    RunPendingMediaTurn pending -> pending.pendingMediaUpdateId

pendingActionChatLocal :: PendingChatAction -> TelegramChatKey
pendingActionChatLocal = \case
    DeliverReply pending -> pending.pendingChat
    RunPendingTurn pending -> pending.pendingTurnChat
    RunPendingMediaTurn pending -> pending.pendingMediaChat

runAgentTurn
    :: TelegramRuntime
    -> TelegramChatKey
    -> Integer
    -> Maybe Integer
    -> Text
    -> IO Text
runAgentTurn runtime key userId replyToMessageId prompt = do
    handle <- sessionForPrompt runtime key prompt
    let agentPrompt =
            prompt
                <> "\n\n[Telegram delivery context: If the best response is \
                \only a lightweight acknowledgement, you may respond with \
                \exactly one standard Telegram reaction emoji. Otherwise \
                \respond normally. Do not mention this delivery context.]"
    runManagedAgentTurn
        runtime
        handle
        key
        userId
        replyToMessageId
        (managedTurnRequestFromText agentPrompt)
        agentPrompt

runManagedAgentTurn
    :: TelegramRuntime
    -> SessionHandle
    -> TelegramChatKey
    -> Integer
    -> Maybe Integer
    -> ManagedTurnRequest
    -> Text
    -> IO Text
runManagedAgentTurn
        runtime handle key userId replyToMessageId baseRequest expectedPrompt = do
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
        bridgeEnv = TelegramBridgeEnv
            { telegramBridgeClient = runtime.runtimeClient
            , telegramBridgeRequest = request
            , telegramBridgeChat = key
            , telegramBridgeUserId = userId
            , telegramBridgeReplyTo = replyToMessageId
            , telegramBridgeAllowedRoot =
                unsafeToFilePath handle.sessionTempDir
            , telegramBridgeModifyState = modifyState runtime
            }
    createDirectoryIfMissing True bridgeDir
    setFileMode bridgePath 0o700
    priorTurnCount <- loadSessionHandle
        runtime.runtimePool
        runtime.runtimeSessionsRoot
        handle.sessionMeta.metaId >>= \case
            Left err -> fail (Text.unpack err)
            Right (_, turns) -> pure (length turns)
    (withTelegramBridge bridgeEnv $
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
                        Right (_, turns)
                            | length turns > priorTurnCount
                            , latestTurnMatches expectedPrompt turns ->
                                pure (renderLatestTurn turns)
                            | otherwise ->
                                fail
                                    "agent completed without recording \
                                    \the Telegram turn"

telegramTurnUserId :: TelegramRuntime -> TelegramChatKey -> Integer
telegramTurnUserId runtime key
    | key.chatId > 0 = key.chatId
    | Set.size runtime.runtimeAllowedUsers == 1 =
        Set.findMin runtime.runtimeAllowedUsers
    | otherwise = 0

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

sessionForPrompt
    :: TelegramRuntime
    -> TelegramChatKey
    -> Text
    -> IO SessionHandle
sessionForPrompt runtime key prompt = do
    state <- readMVar runtime.runtimeStateVar
    existing <- case lookupBinding key state of
        Nothing -> pure Nothing
        Just sessionId ->
            loadSessionHandle
                runtime.runtimePool
                runtime.runtimeSessionsRoot
                sessionId >>= \case
                Left _ -> pure Nothing
                Right (handle, _) -> pure (Just handle)
    case existing of
        Just handle -> pure handle
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
    | Just emoji <- telegramReactionEmoji pending.pendingText
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
            checkpointKey =
                pendingRetryKey (DeliverReply pending)
        state <- readMVar runtime.runtimeStateVar
        let startIndex =
                fromMaybe 0
                    (Map.lookup checkpointKey state.deliveryCheckpoints)
        forM_ (drop startIndex (zip [0 :: Int ..] chunks)) \(index, chunk) ->
            TelegramClient.sendRichMessage
                runtime.runtimeClient
                pending.pendingChat
                (if index == 0
                    then pending.pendingReplyToMessageId
                    else Nothing)
                chunk >>= \case
                    Left err -> fail (Text.unpack err)
                    Right messageId ->
                        modifyState runtime \current ->
                            current
                                { deliveryCheckpoints =
                                    Map.insert
                                        checkpointKey
                                        (index + 1)
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
withTelegramProgressUsing sendTyping sendDraft action =
    withAsync progressLoop (const action)
  where
    progressLoop = loop (0 :: Int)
    loop tick = do
        -- Chat actions expire after roughly five seconds. Rich drafts last
        -- longer, so refresh typing every four seconds and the draft every
        -- fifth tick. Both are best-effort: the final reply remains durable
        -- even when a client or Bot API version does not support rich drafts.
        void (tryAny sendTyping)
        when (tick `mod` 5 == 0) $
            void (tryAny sendDraft)
        threadDelay 4_000_000
        loop (tick + 1)

loadTelegramState :: OsPath -> IO TelegramState
loadTelegramState path = do
    exists <- doesFileExist path
    if not exists
        then pure emptyTelegramState
        else eitherDecode <$> LBS.readFile (unsafeToFilePath path) >>= \case
            Left err -> fail ("could not decode Telegram state: " <> err)
            Right state -> pure state

saveTelegramState :: OsPath -> TelegramState -> IO ()
saveTelegramState path state =
    writeLazyFileAtomically path 0o600 (encode state)

redactToken :: Text -> Text -> Text
redactToken token = Text.replace token "<redacted>"
