-- | Dedicated Telegram gateway backed by persisted agent sessions.
module Agent.Telegram
    ( telegramMain
    , parseAllowedUsers
    , splitTelegramText
    , TelegramConfig(..)
    , TelegramCommand(..)
    , TelegramSetupOptions(..)
    , defaultTelegramSetupOptions
    , parseTelegramArgs
    , TelegramChatKey(..)
    , TelegramState(..)
    , TelegramPendingTurn(..)
    , TelegramPendingReply(..)
    , TelegramUpdateAction(..)
    , PendingChatAction(..)
    , emptyTelegramState
    , storeUpdateAction
    , nextPendingAction
    ) where

import Agent.CLI.AgentSessions
    ( SessionProcessManager
    , closeSessionProcessManager
    , launchSessionTurn
    , newSessionProcessManager
    )
import Agent.CLI.Options
    ( ApprovalPolicy(..)
    , defaultEffortFor
    )
import Agent.CLI.Models
    ( ModelOption(..)
    , resolveModelOptionDialect
    )
import Agent.CLI.Prompt (defaultModelFor)
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
import Agent.FileRetry (writeLazyFileAtomically)
import Agent.Dialect (DialectId, dialectIdForModel)
import Agent.OsPath (unsafeToFilePath)
import Agent.Provider (Provider, parseProvider, providerSlug)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, race_)
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , newMVar
    , readMVar
    )
import Control.Concurrent.STM
    ( TVar
    , atomically
    , modifyTVar'
    , newTVarIO
    , readTVar
    )
import Control.Exception.Safe
    ( SomeException
    , bracket
    , displayException
    , finally
    , try
    , tryAny
    )
import Control.Monad (forM_, unless, void, when)
import Data.Aeson
    ( FromJSON(..)
    , ToJSON(..)
    , Value
    , eitherDecode
    , encode
    , object
    , withObject
    , (.:)
    , (.:?)
    , (.!=)
    , (.=)
    )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TextEncoding
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as HttpTls
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

data TelegramConfig = TelegramConfig
    { telegramProvider :: !Provider
    , telegramModel :: !(Maybe Text)
    , telegramCwd :: !FilePath
    , telegramEffort :: !(Maybe Text)
    , telegramYolo :: !Bool
    , telegramAllowedUsers :: !(Set Integer)
    } deriving (Eq, Show)

instance ToJSON TelegramConfig where
    toJSON config = object
        [ "provider" .= providerSlug config.telegramProvider
        , "model" .= config.telegramModel
        , "cwd" .= config.telegramCwd
        , "effort" .= config.telegramEffort
        , "yolo" .= config.telegramYolo
        , "allowedUsers" .= Set.toList config.telegramAllowedUsers
        ]

instance FromJSON TelegramConfig where
    parseJSON = withObject "TelegramConfig" \o -> do
        providerText <- o .: "provider"
        telegramProvider <- maybe
            (fail ("unknown provider: " <> Text.unpack providerText))
            pure
            (parseProvider providerText)
        TelegramConfig
            <$> pure telegramProvider
            <*> o .:? "model"
            <*> o .: "cwd"
            <*> o .:? "effort"
            <*> (o .:? "yolo" .!= False)
            <*> (Set.fromList <$> (o .: "allowedUsers"))

data TelegramSetupOptions = TelegramSetupOptions
    { setupProvider :: !(Maybe Provider)
    , setupModel :: !(Maybe Text)
    , setupCwd :: !(Maybe FilePath)
    , setupEffort :: !(Maybe Text)
    , setupYolo :: !Bool
    , setupAllowedUser :: !(Maybe Integer)
    , setupStart :: !Bool
    } deriving (Eq, Show)

defaultTelegramSetupOptions :: TelegramSetupOptions
defaultTelegramSetupOptions = TelegramSetupOptions
    { setupProvider = Nothing
    , setupModel = Nothing
    , setupCwd = Nothing
    , setupEffort = Nothing
    , setupYolo = False
    , setupAllowedUser = Nothing
    , setupStart = False
    }

data TelegramCommand
    = TelegramSetup !TelegramSetupOptions
    | TelegramRun
    | TelegramStart
    | TelegramStop
    | TelegramStatus
    | TelegramHelp
    | TelegramVersion
    deriving (Eq, Show)

data TelegramChatKey = TelegramChatKey
    { chatId :: !Integer
    , messageThreadId :: !(Maybe Integer)
    } deriving (Eq, Ord, Show)

instance ToJSON TelegramChatKey where
    toJSON key = object
        [ "chatId" .= key.chatId
        , "messageThreadId" .= key.messageThreadId
        ]

instance FromJSON TelegramChatKey where
    parseJSON = withObject "TelegramChatKey" \o ->
        TelegramChatKey
            <$> o .: "chatId"
            <*> o .:? "messageThreadId"

data TelegramBinding = TelegramBinding
    { bindingChat :: !TelegramChatKey
    , bindingSessionId :: !Text
    } deriving (Eq, Show)

instance ToJSON TelegramBinding where
    toJSON binding = object
        [ "chat" .= binding.bindingChat
        , "sessionId" .= binding.bindingSessionId
        ]

instance FromJSON TelegramBinding where
    parseJSON = withObject "TelegramBinding" \o ->
        TelegramBinding
            <$> o .: "chat"
            <*> o .: "sessionId"

data TelegramState = TelegramState
    { nextUpdateId :: !(Maybe Integer)
    , bindings :: ![TelegramBinding]
    , pendingTurns :: ![TelegramPendingTurn]
    , pendingReplies :: ![TelegramPendingReply]
    } deriving (Eq, Show)

instance ToJSON TelegramState where
    toJSON state = object
        [ "nextUpdateId" .= state.nextUpdateId
        , "bindings" .= state.bindings
        , "pendingTurns" .= state.pendingTurns
        , "pendingReplies" .= state.pendingReplies
        ]

instance FromJSON TelegramState where
    parseJSON = withObject "TelegramState" \o ->
        TelegramState
            <$> o .:? "nextUpdateId"
            <*> (o .:? "bindings" .!= [])
            <*> (o .:? "pendingTurns" .!= [])
            <*> (o .:? "pendingReplies" .!= [])

data TelegramPendingTurn = TelegramPendingTurn
    { pendingTurnUpdateId :: !Integer
    , pendingTurnMessageId :: !Integer
    , pendingTurnChat :: !TelegramChatKey
    , pendingTurnText :: !Text
    } deriving (Eq, Show)

instance ToJSON TelegramPendingTurn where
    toJSON pending = object
        [ "updateId" .= pending.pendingTurnUpdateId
        , "messageId" .= pending.pendingTurnMessageId
        , "chat" .= pending.pendingTurnChat
        , "text" .= pending.pendingTurnText
        ]

instance FromJSON TelegramPendingTurn where
    parseJSON = withObject "TelegramPendingTurn" \o ->
        TelegramPendingTurn
            <$> o .: "updateId"
            <*> o .:? "messageId" .!= 0
            <*> o .: "chat"
            <*> o .: "text"

data TelegramPendingReply = TelegramPendingReply
    { pendingUpdateId :: !Integer
    , pendingChat :: !TelegramChatKey
    , pendingText :: !Text
    } deriving (Eq, Show)

instance ToJSON TelegramPendingReply where
    toJSON pending = object
        [ "updateId" .= pending.pendingUpdateId
        , "chat" .= pending.pendingChat
        , "text" .= pending.pendingText
        ]

instance FromJSON TelegramPendingReply where
    parseJSON = withObject "TelegramPendingReply" \o ->
        TelegramPendingReply
            <$> o .: "updateId"
            <*> o .: "chat"
            <*> o .: "text"

data TelegramUser = TelegramUser
    { userId :: !Integer
    } deriving (Eq, Show)

instance FromJSON TelegramUser where
    parseJSON = withObject "TelegramUser" \o ->
        TelegramUser <$> o .: "id"

data TelegramChat = TelegramChat
    { telegramChatId :: !Integer
    , telegramChatType :: !Text
    } deriving (Eq, Show)

instance FromJSON TelegramChat where
    parseJSON = withObject "TelegramChat" \o ->
        TelegramChat
            <$> o .: "id"
            <*> o .: "type"

data TelegramMessage = TelegramMessage
    { messageId :: !Integer
    , messageFrom :: !(Maybe TelegramUser)
    , messageChat :: !TelegramChat
    , messageThread :: !(Maybe Integer)
    , messageText :: !(Maybe Text)
    } deriving (Eq, Show)

instance FromJSON TelegramMessage where
    parseJSON = withObject "TelegramMessage" \o ->
        TelegramMessage
            <$> o .: "message_id"
            <*> o .:? "from"
            <*> o .: "chat"
            <*> o .:? "message_thread_id"
            <*> o .:? "text"

data TelegramUpdate = TelegramUpdate
    { updateId :: !Integer
    , updateMessage :: !(Maybe TelegramMessage)
    } deriving (Eq, Show)

instance FromJSON TelegramUpdate where
    parseJSON = withObject "TelegramUpdate" \o ->
        TelegramUpdate
            <$> o .: "update_id"
            <*> o .:? "message"

data TelegramResponse a = TelegramResponse
    { responseOk :: !Bool
    , responseResult :: !(Maybe a)
    , responseDescription :: !(Maybe Text)
    }

instance FromJSON a => FromJSON (TelegramResponse a) where
    parseJSON = withObject "TelegramResponse" \o ->
        TelegramResponse
            <$> o .: "ok"
            <*> o .:? "result"
            <*> o .:? "description"

data TelegramClient = TelegramClient
    { clientToken :: !Text
    , clientManager :: !Http.Manager
    }

emptyTelegramState :: TelegramState
emptyTelegramState = TelegramState
    { nextUpdateId = Nothing
    , bindings = []
    , pendingTurns = []
    , pendingReplies = []
    }

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
    | otherwise =
        let (chunk, rest) = Text.splitAt limit text
        in chunk : splitTelegramText limit rest

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
    ["--help"] -> Right TelegramHelp
    ["-h"] -> Right TelegramHelp
    ["--version"] -> Right TelegramVersion
    "setup" : rest -> TelegramSetup <$> parseSetupOptions rest
    _ -> Left telegramUsage

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
                    go options { setupAllowedUser = Just userId } rest
                _ -> Left ("invalid Telegram user ID: " <> value)
        "--yolo" : rest -> go options { setupYolo = True } rest
        "--start" : rest -> go options { setupStart = True } rest
        flag : _ -> Left ("unknown setup option: " <> flag <> "\n\n" <> telegramUsage)

executeTelegramCommand :: TelegramCommand -> IO ()
executeTelegramCommand = \case
    TelegramSetup options -> setupTelegram options
    TelegramRun -> runConfiguredTelegram
    TelegramStart -> startTelegram
    TelegramStop -> stopTelegram
    TelegramStatus -> do
        running <- telegramIsRunning
        Text.putStrLn (if running then "agent-telegram is running" else "agent-telegram is stopped")
    TelegramHelp -> putStrLn telegramUsage
    TelegramVersion -> putStrLn "agent-telegram 0.1.0.0"

telegramUsage :: String
telegramUsage = unlines
    [ "Usage: agent-telegram setup [OPTIONS]"
    , "       agent-telegram run"
    , "       agent-telegram start"
    , "       agent-telegram stop"
    , "       agent-telegram status"
    , ""
    , "Setup options:"
    , "  --provider NAME       openai, xai, or openrouter"
    , "  --model NAME          optional model override"
    , "  --cwd PATH            agent working directory"
    , "  --effort LEVEL        optional reasoning effort"
    , "  --allowed-user ID     numeric Telegram user ID"
    , "  --yolo                allow mutating agent tools"
    , "  --start               start the gateway after setup"
    ]

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
    allowedUser <- maybe promptAllowedUser pure options.setupAllowedUser
    token <- readSecretLine "BotFather token: " >>= maybe
        (die "a Telegram bot token is required")
        pure
    manager <- HttpTls.newTlsManager
    let client = TelegramClient token manager
    telegramRequest client "getMe" (object []) 15 >>= \case
        Left err -> die (Text.unpack err)
        Right response -> case decodeTelegramResponse response :: Either Text Value of
            Left err -> die (Text.unpack err)
            Right _ -> pure ()
    let directory = gatewayDirectory home
        config = TelegramConfig
            { telegramProvider = provider
            , telegramModel = options.setupModel
            , telegramCwd = cwd
            , telegramEffort = options.setupEffort
            , telegramYolo = options.setupYolo
            , telegramAllowedUsers = Set.singleton allowedUser
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
    cwd <- makeAbsolute (unsafeEncodeUtf config.telegramCwd)
    let provider = config.telegramProvider
        model = fromMaybe (defaultModelFor provider) config.telegramModel
        effort = fromMaybe (defaultEffortFor provider) config.telegramEffort
        root = sessionsRoot home
        gatewayDir = gatewayDirectory home
        statePath = statePathFor home
        policy
            | config.telegramYolo = ApproveAll
            | otherwise = DenyMutating
    target <- resolveModelOptionDialect ModelOption
        { modelProvider = provider
        , modelId = model
        , modelTransportId = model
        , modelDialect = dialectIdForModel provider model
        , modelLabel = Nothing
        }
    createDirectoryIfMissing True gatewayDir
    setFileMode (unsafeToFilePath gatewayDir) 0o700
    state <- loadTelegramState statePath
    stateVar <- newMVar state
    activeChats <- newTVarIO Set.empty
    manager <- HttpTls.newTlsManager
    processManager <- newSessionProcessManager root
    let client = TelegramClient token manager
        runtime = TelegramRuntime
            { runtimeClient = client
            , runtimeAllowedUsers = config.telegramAllowedUsers
            , runtimeSessionsRoot = root
            , runtimeStatePath = statePath
            , runtimeStateVar = stateVar
            , runtimeActiveChats = activeChats
            , runtimeProcessManager = processManager
            , runtimeProvider = provider
            , runtimeModel = model
            , runtimeTransportModel = target.modelTransportId
            , runtimeDialect = target.modelDialect
            , runtimeCwd = cwd
            , runtimeEffort = effort
            , runtimePolicy = policy
            }
    Text.putStrLn "Telegram gateway started (private chats only)."
    race_ (pollForever runtime) (dispatchForever runtime)
        `finally` closeSessionProcessManager processManager

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
    , runtimeAllowedUsers :: !(Set Integer)
    , runtimeSessionsRoot :: !OsPath
    , runtimeStatePath :: !OsPath
    , runtimeStateVar :: !(MVar TelegramState)
    , runtimeActiveChats :: !(TVar (Set TelegramChatKey))
    , runtimeProcessManager :: !SessionProcessManager
    , runtimeProvider :: !Provider
    , runtimeModel :: !Text
    , runtimeTransportModel :: !Text
    , runtimeDialect :: !DialectId
    , runtimeCwd :: !OsPath
    , runtimeEffort :: !Text
    , runtimePolicy :: !ApprovalPolicy
    }

pollForever :: TelegramRuntime -> IO ()
pollForever runtime = do
    state <- readMVar runtime.runtimeStateVar
    getUpdates runtime.runtimeClient state.nextUpdateId >>= \case
        Left err -> do
            Text.hPutStrLn stderr err
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
            Text.hPutStrLn stderr
                ("Telegram update could not be persisted and will be retried: "
                    <> redactToken runtime.runtimeClient.clientToken
                        (Text.pack (displayException err)))
        Right () -> pure ()

data TelegramUpdateAction
    = IgnoreUpdate
    | QueueTurn !Integer !TelegramChatKey !Text
    deriving (Eq, Show)

storeUpdateAction
    :: Integer
    -> TelegramUpdateAction
    -> TelegramState
    -> TelegramState
storeUpdateAction updateId action current
    | updateAlreadyStored updateId current =
        advanceOffset current
    | otherwise =
        advanceOffset case action of
            IgnoreUpdate -> current
            QueueTurn messageId key text ->
                current
                    { pendingTurns =
                        current.pendingTurns
                            <> [TelegramPendingTurn updateId messageId key text]
                    }
  where
    advanceOffset state =
        state
            { nextUpdateId =
                Just (max (updateId + 1)
                    (fromMaybe 0 state.nextUpdateId))
            }

classifyUpdate
    :: TelegramRuntime
    -> TelegramUpdate
    -> IO TelegramUpdateAction
classifyUpdate runtime update =
    case update.updateMessage of
        Just message
            | message.messageChat.telegramChatType == "private"
            , Just sender <- message.messageFrom
            , sender.userId `Set.member` runtime.runtimeAllowedUsers
            , Just rawText <- message.messageText
            , not (Text.null (Text.strip rawText)) ->
                pure (QueueTurn
                    message.messageId
                    key
                    (Text.strip rawText))
          where
            key = TelegramChatKey
                { chatId = message.messageChat.telegramChatId
                , messageThreadId = message.messageThread
                }
        _ -> pure IgnoreUpdate

updateAlreadyStored :: Integer -> TelegramState -> Bool
updateAlreadyStored updateId state =
    any ((== updateId) . (.pendingTurnUpdateId)) state.pendingTurns
        || any ((== updateId) . (.pendingUpdateId)) state.pendingReplies
        || maybe False (> updateId) state.nextUpdateId

data PendingChatAction
    = DeliverReply !TelegramPendingReply
    | RunPendingTurn !TelegramPendingTurn
    deriving (Eq, Show)

dispatchForever :: TelegramRuntime -> IO ()
dispatchForever runtime = do
    state <- readMVar runtime.runtimeStateVar
    active <- atomically (readTVar runtime.runtimeActiveChats)
    let pendingChats = Set.fromList
            (map (.pendingTurnChat) state.pendingTurns
                <> map (.pendingChat) state.pendingReplies)
    forM_ (Set.toList (pendingChats `Set.difference` active)) \key -> do
        claimed <- atomically do
            current <- readTVar runtime.runtimeActiveChats
            if key `Set.member` current
                then pure False
                else do
                    modifyTVar' runtime.runtimeActiveChats (Set.insert key)
                    pure True
        if claimed
            then void $ async $
                processChatQueue runtime key `finally`
                    atomically
                        (modifyTVar' runtime.runtimeActiveChats (Set.delete key))
            else pure ()
    threadDelay 250_000
    dispatchForever runtime

processChatQueue :: TelegramRuntime -> TelegramChatKey -> IO ()
processChatQueue runtime key =
    nextChatAction runtime key >>= \case
        Nothing -> pure ()
        Just action -> do
            result <- tryAny case action of
                DeliverReply pending -> do
                    reply runtime pending.pendingChat pending.pendingText
                    modifyState runtime \state ->
                        state
                            { pendingReplies =
                                filter
                                    ((/= pending.pendingUpdateId)
                                        . (.pendingUpdateId))
                                    state.pendingReplies
                            }
                RunPendingTurn pending -> do
                    response <- runQueuedTurn runtime pending
                    modifyState runtime \state ->
                        state
                            { pendingTurns =
                                filter
                                    ((/= pending.pendingTurnUpdateId)
                                        . (.pendingTurnUpdateId))
                                    state.pendingTurns
                            , pendingReplies =
                                state.pendingReplies
                                    <> [ TelegramPendingReply
                                            pending.pendingTurnUpdateId
                                            pending.pendingTurnChat
                                            response
                                       ]
                            }
            case result of
                Left err -> do
                    Text.hPutStrLn stderr
                        ("Telegram conversation worker failed and will retry: "
                            <> redactToken runtime.runtimeClient.clientToken
                                (Text.pack (displayException err)))
                    threadDelay 2_000_000
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
                    { bindings =
                        filter
                            ((/= pending.pendingTurnChat) . (.bindingChat))
                            state.bindings
                    }
            pure "Started a new conversation. Send your next prompt."
        Just "session" -> do
            state <- readMVar runtime.runtimeStateVar
            pure case lookupBinding pending.pendingTurnChat state of
                Nothing -> "No session yet. Send a prompt to create one."
                Just sessionId -> "Session: " <> sessionId
        Just command -> pure ("Unknown command: /" <> command)
        Nothing -> runAgentTurn
            runtime
            pending.pendingTurnChat
            pending.pendingTurnText

nextChatAction
    :: TelegramRuntime
    -> TelegramChatKey
    -> IO (Maybe PendingChatAction)
nextChatAction runtime key = do
    state <- readMVar runtime.runtimeStateVar
    pure (nextPendingAction key state)

nextPendingAction
    :: TelegramChatKey
    -> TelegramState
    -> Maybe PendingChatAction
nextPendingAction key state =
    case
        ( oldestBy (.pendingTurnUpdateId)
            (filter ((== key) . (.pendingTurnChat)) state.pendingTurns)
        , oldestBy (.pendingUpdateId)
            (filter ((== key) . (.pendingChat)) state.pendingReplies)
        ) of
        (Nothing, Nothing) -> Nothing
        (Just turn, Nothing) -> Just (RunPendingTurn turn)
        (Nothing, Just pending) -> Just (DeliverReply pending)
        (Just turn, Just pending)
            | pending.pendingUpdateId <= turn.pendingTurnUpdateId ->
                Just (DeliverReply pending)
            | otherwise -> Just (RunPendingTurn turn)

oldestBy :: Ord b => (a -> b) -> [a] -> Maybe a
oldestBy _ [] = Nothing
oldestBy getKey (value : values) =
    Just (foldl choose value values)
  where
    choose current candidate
        | getKey candidate < getKey current = candidate
        | otherwise = current

telegramCommand :: Text -> Maybe Text
telegramCommand text = do
    firstWord <- case Text.words text of
        [] -> Nothing
        value : _ -> Just value
    withoutSlash <- Text.stripPrefix "/" firstWord
    pure (Text.toLower (Text.takeWhile (/= '@') withoutSlash))

runAgentTurn :: TelegramRuntime -> TelegramChatKey -> Text -> IO Text
runAgentTurn runtime key prompt = do
    handle <- sessionForPrompt runtime key prompt
    priorTurnCount <- loadSessionHandle
        runtime.runtimeSessionsRoot
        handle.sessionMeta.metaId >>= \case
            Left err -> fail (Text.unpack err)
            Right (_, turns) -> pure (length turns)
    launchSessionTurn
        runtime.runtimeProcessManager
        False
        runtime.runtimePolicy
        handle
        prompt >>= \case
            Left err -> fail (Text.unpack err)
            Right _ ->
                loadSessionHandle
                    runtime.runtimeSessionsRoot
                    handle.sessionMeta.metaId >>= \case
                        Left err -> fail (Text.unpack err)
                        Right (_, turns)
                            | length turns > priorTurnCount
                            , latestTurnMatches prompt turns ->
                                pure (renderLatestTurn turns)
                            | otherwise ->
                                fail
                                    "agent completed without recording \
                                    \the Telegram turn"

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
            loadSessionHandle runtime.runtimeSessionsRoot sessionId >>= \case
                Left _ -> pure Nothing
                Right (handle, _) -> pure (Just handle)
    case existing of
        Just handle -> pure handle
        Nothing -> do
            handle <- createSession SessionCreate
                { createRoot = runtime.runtimeSessionsRoot
                , createProvider = runtime.runtimeProvider
                , createModel = runtime.runtimeModel
                , createTransportModel = runtime.runtimeTransportModel
                , createDialect = runtime.runtimeDialect
                , createCwd = runtime.runtimeCwd
                , createEffort = runtime.runtimeEffort
                , createTitleHint = Just (sessionTitleFromPrompt prompt)
                , createTitleIsManual = False
                }
            modifyState runtime \current ->
                current
                    { bindings =
                        TelegramBinding key handle.sessionMeta.metaId
                            : filter ((/= key) . (.bindingChat)) current.bindings
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
    (.bindingSessionId) <$> find ((== key) . (.bindingChat)) state.bindings

modifyState :: TelegramRuntime -> (TelegramState -> TelegramState) -> IO ()
modifyState runtime update =
    modifyMVar runtime.runtimeStateVar \current -> do
        let next = update current
        saveTelegramState runtime.runtimeStatePath next
        pure (next, ())

reply :: TelegramRuntime -> TelegramChatKey -> Text -> IO ()
reply runtime key text = do
    -- Telegram counts astral Unicode characters as two UTF-16 code units.
    -- A 2000-code-point chunk therefore remains below sendMessage's limit.
    let chunks = case splitTelegramText 2000 text of
            [] -> ["(empty response)"]
            values -> values
    forM_ chunks \chunk ->
        sendMessage runtime.runtimeClient key chunk >>= \case
            Left err -> fail (Text.unpack err)
            Right () -> pure ()

getUpdates
    :: TelegramClient
    -> Maybe Integer
    -> IO (Either Text [TelegramUpdate])
getUpdates client offset =
    telegramRequest client "getUpdates" body 45 >>= \case
        Left err -> pure (Left err)
        Right response -> pure (decodeTelegramResponse response)
  where
    body = object $
        [ "timeout" .= (30 :: Int)
        , "allowed_updates" .= (["message"] :: [Text])
        ]
            <> maybe [] (\value -> ["offset" .= value]) offset

sendMessage
    :: TelegramClient
    -> TelegramChatKey
    -> Text
    -> IO (Either Text ())
sendMessage client key text =
    telegramRequest client "sendMessage" body 30 >>= \case
        Left err -> pure (Left err)
        Right response ->
            pure (() <$ (decodeTelegramResponse response :: Either Text Value))
  where
    body = object $
        [ "chat_id" .= key.chatId
        , "text" .= text
        ]
            <> maybe []
                (\threadId -> ["message_thread_id" .= threadId])
                key.messageThreadId

telegramRequest
    :: TelegramClient
    -> String
    -> Value
    -> Int
    -> IO (Either Text LBS.ByteString)
telegramRequest client method body timeoutSeconds =
    tryAny request >>= \case
        Left err ->
            pure $ Left $
                "Telegram request failed: "
                    <> redactToken client.clientToken
                        (Text.pack (displayException err))
        Right response -> pure (Right (Http.responseBody response))
  where
    request = do
        base <- Http.parseRequest $
            "https://api.telegram.org/bot"
                <> Text.unpack client.clientToken
                <> "/"
                <> method
        let configured = base
                { Http.method = "POST"
                , Http.requestHeaders =
                    [("Content-Type", "application/json")]
                , Http.requestBody = Http.RequestBodyLBS (encode body)
                , Http.responseTimeout =
                    Http.responseTimeoutMicro
                        (timeoutSeconds * 1_000_000)
                }
        Http.httpLbs configured client.clientManager

decodeTelegramResponse
    :: forall a. FromJSON a
    => LBS.ByteString
    -> Either Text a
decodeTelegramResponse bytes = do
    envelope <- case
            eitherDecode bytes
                :: Either String (TelegramResponse a)
        of
        Left err -> Left ("Telegram returned invalid JSON: " <> Text.pack err)
        Right value -> Right value
    if envelope.responseOk
        then maybe
            (Left "Telegram response did not contain a result")
            Right
            envelope.responseResult
        else Left $
            "Telegram API error: "
                <> fromMaybe "unknown error" envelope.responseDescription

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
