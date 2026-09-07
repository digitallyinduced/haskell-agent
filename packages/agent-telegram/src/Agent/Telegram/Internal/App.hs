module Agent.Telegram.Internal.App where


import Agent.CLI.AgentSessions.Process ()
import Agent.CLI.ManagedTurn ()
import Agent.CLI.Runtime.Options ()
import Agent.CLI.Models ()
import Agent.CLI.ModelConfig ()
import Agent.CLI.Session ()
import Agent.Telegram.Types
import Agent.Telegram.Classify
    ( telegramUserLabel )
import Agent.Telegram.Bridge ()
import qualified Agent.Telegram.Client as TelegramClient
import Agent.Telegram.Log ()
import Agent.Telegram.Markdown ()
import Agent.Telegram.Voice ()
import Agent.Json (rawJsonDecoder)
import qualified Agent.Json.Decode as Hermes
import Agent.FileRetry (writeLazyFileAtomically)
import Agent.Concurrent ()
import Agent.OsPath (unsafeToFilePath)
import Agent.Provider (Provider, parseProvider)
import Agent.ReasoningEffort ()
import Agent.Store.Postgres ()
import Agent.Store.Postgres.Connection ()
import Agent.Store.Types ()
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async ()
import Control.Concurrent.Chan ()
import Control.Concurrent.MVar ()
import Control.Applicative (many, (<|>))
import Control.Exception.Safe
    ( SomeException
    , bracket
    , finally
    , try
    )
import Control.Monad (forM_, unless, when)
import Data.Aeson
    ( encode
    , object
    )
import qualified Data.ByteString.Lazy as LBS
import Data.Int ()
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Map.Strict as Map
import Data.Maybe ()
import Data.Set ()
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock ()
import qualified Network.HTTP.Client.TLS as HttpTls
import Options.Applicative
    ( Parser
    , ParserInfo
    , ParserResult(..)
    , ReadM
    , argument
    , command
    , defaultPrefs
    , eitherReader
    , execParserPure
    , flag'
    , fullDesc
    , header
    , help
    , helper
    , hsubparser
    , info
    , long
    , metavar
    , option
    , progDesc
    , renderFailure
    , short
    , strOption
    , (<**>)
    )
import qualified System.Directory as Directory ()
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
import System.FilePath ()
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
import Agent.Telegram.Internal.Launch (runTelegram)
import Agent.Telegram.Internal.Support (loadTelegramState)
telegramMain :: IO ()
telegramMain =
    getArgs >>= either die executeTelegramCommand . parseTelegramArgs

parseTelegramArgs :: [String] -> Either String TelegramCommand
parseTelegramArgs args =
    case execParserPure defaultPrefs telegramParserInfo args of
        Success commandValue -> Right commandValue
        Failure failure -> Left (fst (renderFailure failure "agent-telegram"))
        CompletionInvoked _ -> Left telegramUsage

telegramParserInfo :: ParserInfo TelegramCommand
telegramParserInfo = info telegramCommandParser
    (fullDesc
        <> progDesc "Run and configure the Haskell Agent Telegram gateway"
        <> header "agent-telegram")

telegramCommandParser :: Parser TelegramCommand
telegramCommandParser =
    flag' TelegramHelp
        (long "help" <> short 'h' <> help "Show this help text")
        <|> telegramOperationalParser

telegramOperationalParser :: Parser TelegramCommand
telegramOperationalParser =
    flag' TelegramVersion (long "version" <> help "Show the version")
        <|> telegramSubcommandParser
        <|> pure TelegramRun

telegramSubcommandParser :: Parser TelegramCommand
telegramSubcommandParser = hsubparser
    ( command "setup"
        (info (TelegramSetup <$> telegramSetupOptionsParser)
            (progDesc "Configure the Telegram gateway"))
        <> command "run"
            (info (pure TelegramRun) (progDesc "Run the configured gateway"))
        <> command "start"
            (info (pure TelegramStart) (progDesc "Start the background gateway"))
        <> command "stop"
            (info (pure TelegramStop) (progDesc "Stop the background gateway"))
        <> command
            "status"
            (info (pure TelegramStatus) (progDesc "Show gateway status"))
        <> command "users"
            (info telegramUsersParser (progDesc "Manage allowed Telegram users"))
    )

telegramUsersParser :: Parser TelegramCommand
telegramUsersParser = TelegramUsers <$> hsubparser
    ( command "list"
        (info (pure TelegramUsersList) (progDesc "List allowed users"))
        <> command "add"
            (info
                (TelegramUsersAdd <$> argument telegramUserIdReader (metavar "ID"))
                (progDesc "Allow a Telegram user"))
        <> command "remove"
            (info
                (TelegramUsersRemove <$> argument telegramUserIdReader (metavar "ID"))
                (progDesc "Remove a Telegram user"))
    )

telegramSetupOptionsParser :: Parser TelegramSetupOptions
telegramSetupOptionsParser =
    foldl' (\options update -> update options) defaultTelegramSetupOptions
        <$> many setupOption
  where
    setupOption =
        (\provider options -> options { setupProvider = Just provider })
            <$> option providerReader
                (long "provider" <> metavar "NAME" <> help "Provider name")
        <|> (\model options -> options { setupModel = Just (Text.pack model) })
            <$> strOption
                (long "model" <> metavar "NAME"
                    <> help "Optional model override")
        <|> (\cwd options -> options { setupCwd = Just cwd })
            <$> strOption
                (long "cwd" <> metavar "PATH" <> help "Agent working directory")
        <|> (\effort options -> options { setupEffort = Just (Text.pack effort) })
            <$> strOption
                (long "effort" <> metavar "LEVEL"
                    <> help "Optional reasoning effort")
        <|> (\userId options ->
                options
                    { setupAllowedUsers = options.setupAllowedUsers <> [userId] })
            <$> option telegramUserIdReader
                (long "allowed-user" <> metavar "ID"
                    <> help "Allowed Telegram user ID; may be repeated")
        <|> flag' (\options ->
                options { setupApprovalMode = TelegramApprovalYolo })
            (long "yolo" <> help "Auto-approve mutating agent tools")
        <|> flag' (\options ->
                options { setupApprovalMode = TelegramApprovalDeny })
            (long "deny-mutations" <> help "Deny mutating agent tools")
        <|> flag' (\options ->
                options { setupRespondToAllGroupMessages = True })
            (long "all-group-messages"
                <> help "Consider every allowed-user group message")
        <|> (\workers options -> options { setupWorkerCount = workers })
            <$> option workerCountReader
                (long "workers" <> metavar "N"
                    <> help ("Concurrent chat workers (1-"
                        <> show maximumTelegramWorkerCount <> ")"))
        <|> flag' (\options -> options { setupStart = True })
            (long "start" <> help "Start the gateway after setup")

providerReader :: ReadM Provider
providerReader = eitherReader \value ->
    maybe (Left ("unknown provider: " <> value)) Right
        (parseProvider (Text.pack value))

telegramUserIdReader :: ReadM Integer
telegramUserIdReader = eitherReader \value ->
    case readMaybe value of
        Just userId | userId > 0 -> Right userId
        _ -> Left ("invalid Telegram user ID: " <> value)

workerCountReader :: ReadM Int
workerCountReader = eitherReader \value ->
    case readMaybe value of
        Just workers
            | workers >= 1
            , workers <= maximumTelegramWorkerCount ->
                Right workers
        _ -> Left
            ("workers must be between 1 and "
                <> show maximumTelegramWorkerCount)

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
telegramUsage =
    case execParserPure defaultPrefs
        (info (telegramOperationalParser <**> helper)
            (fullDesc
                <> progDesc
                    "Run and configure the Haskell Agent Telegram gateway"
                <> header "agent-telegram"))
        ["--help"] of
        Failure failure -> fst (renderFailure failure "agent-telegram")
        _ -> "Usage: agent-telegram COMMAND"

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
        TelegramUsersList -> do
            stateExists <- doesFileExist (statePathFor home)
            state <-
                if stateExists
                    then loadTelegramState (statePathFor home)
                    else pure emptyTelegramState
            let ids = users <> state.allowedUserIds
            forM_ (Set.toAscList ids) \userId ->
                Text.putStrLn $
                    case Map.lookup userId state.seenTelegramUsers of
                        Just user -> telegramUserLabel user
                        Nothing -> Text.pack (show userId)
        _ -> do
            when (Set.null updated.telegramAllowedUsers) $
                die "refusing to remove the last allowed Telegram user"
            writeLazyFileAtomically
                (configPath home)
                0o600
                (encode updated)
            Text.putStrLn
                "Telegram allowlist updated. Restart the gateway to apply a CLI \
                \change. In a group, /allow applies immediately without a restart."

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
                TelegramClient.decodeTelegramResponse rawJsonDecoder response
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
    Hermes.decodeEither telegramConfigDecoder . LBS.toStrict
        <$> LBS.readFile (unsafeToFilePath (configPath home)) >>= \case
        Left err -> die ("could not decode Telegram config: "
            <> Text.unpack (Hermes.jsonErrorMessage err))
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
