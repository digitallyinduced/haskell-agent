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
    , TelegramChatKey(..)
    , TelegramState(..)
    , TelegramPendingTurn(..)
    , TelegramPendingReply(..)
    , TelegramVoice(..)
    , TelegramMessage(..)
    , TelegramUpdate(..)
    , TelegramUpdateAction(..)
    , PendingChatAction(..)
    , emptyTelegramState
    , storeUpdateAction
    , nextPendingAction
    , checkpointPendingVoiceTranscript
    , reactionMessageText
    , telegramReactionEmoji
    , transcribeWithCodex
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
import Agent.FileRetry (writeLazyFileAtomically)
import Agent.OsPath (unsafeToFilePath)
import Agent.Provider (Provider, parseProvider, providerSlug)
import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
    ( Async
    , async
    , cancel
    , race_
    , waitCatch
    , withAsync
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , modifyMVar_
    , newEmptyMVar
    , newMVar
    , putMVar
    , readMVar
    , takeMVar
    )
import Control.Exception.Safe
    ( SomeException
    , bracket
    , displayException
    , finally
    , mask
    , try
    , tryAny
    )
import Control.Monad (forM_, unless, void, when)
import Data.Aeson
    ( FromJSON(..)
    , Result(..)
    , ToJSON(..)
    , Value(..)
    , eitherDecode
    , eitherDecodeStrict'
    , encode
    , fromJSON
    , object
    , withObject
    , (.:)
    , (.:?)
    , (.!=)
    , (.=)
    )
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text.Encoding as TextEncoding
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Vector as Vector
import qualified Network.HTTP.Client as Http
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
    ( Handle
    , IOMode(AppendMode)
    , hClose
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
    , terminateProcess
    , waitForProcess
    )
import qualified System.FileLock as FileLock
import qualified System.Timeout as Timeout
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
    , pendingTurnVoice :: !(Maybe TelegramVoice)
    } deriving (Eq, Show)

instance ToJSON TelegramPendingTurn where
    toJSON pending = object
        [ "updateId" .= pending.pendingTurnUpdateId
        , "messageId" .= pending.pendingTurnMessageId
        , "chat" .= pending.pendingTurnChat
        , "text" .= pending.pendingTurnText
        , "voice" .= pending.pendingTurnVoice
        ]

instance FromJSON TelegramPendingTurn where
    parseJSON = withObject "TelegramPendingTurn" \o ->
        TelegramPendingTurn
            <$> o .: "updateId"
            <*> o .:? "messageId" .!= 0
            <*> o .: "chat"
            <*> o .: "text"
            <*> o .:? "voice"

data TelegramPendingReply = TelegramPendingReply
    { pendingUpdateId :: !Integer
    , pendingChat :: !TelegramChatKey
    , pendingReplyToMessageId :: !(Maybe Integer)
    , pendingText :: !Text
    } deriving (Eq, Show)

instance ToJSON TelegramPendingReply where
    toJSON pending = object
        [ "updateId" .= pending.pendingUpdateId
        , "chat" .= pending.pendingChat
        , "replyToMessageId" .= pending.pendingReplyToMessageId
        , "text" .= pending.pendingText
        ]

instance FromJSON TelegramPendingReply where
    parseJSON = withObject "TelegramPendingReply" \o ->
        TelegramPendingReply
            <$> o .: "updateId"
            <*> o .: "chat"
            <*> o .:? "replyToMessageId"
            <*> o .: "text"

data TelegramVoice = TelegramVoice
    { voiceFileId :: !Text
    , voiceDuration :: !Int
    , voiceMimeType :: !(Maybe Text)
    , voiceFileSize :: !(Maybe Integer)
    } deriving (Eq, Show)

instance ToJSON TelegramVoice where
    toJSON voice = object
        [ "fileId" .= voice.voiceFileId
        , "duration" .= voice.voiceDuration
        , "mimeType" .= voice.voiceMimeType
        , "fileSize" .= voice.voiceFileSize
        ]

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
    , messageVoice :: !(Maybe TelegramVoice)
    } deriving (Eq, Show)

instance FromJSON TelegramMessage where
    parseJSON = withObject "TelegramMessage" \o ->
        TelegramMessage
            <$> o .: "message_id"
            <*> o .:? "from"
            <*> o .: "chat"
            <*> o .:? "message_thread_id"
            <*> o .:? "text"
            <*> o .:? "voice"

instance FromJSON TelegramVoice where
    parseJSON = withObject "TelegramVoice" \o ->
        TelegramVoice
            <$> (o .:? "file_id" >>= maybe (o .: "fileId") pure)
            <*> (o .:? "duration" .!= 0)
            <*> (o .:? "mime_type" >>= maybe (o .:? "mimeType") (pure . Just))
            <*> (o .:? "file_size" >>= maybe (o .:? "fileSize") (pure . Just))

data TelegramReactionType = TelegramReactionType
    { reactionType :: !Text
    , reactionEmoji :: !(Maybe Text)
    , reactionCustomEmojiId :: !(Maybe Text)
    } deriving (Eq, Show)

instance FromJSON TelegramReactionType where
    parseJSON = withObject "TelegramReactionType" \o ->
        TelegramReactionType
            <$> o .: "type"
            <*> o .:? "emoji"
            <*> o .:? "custom_emoji_id"

data TelegramMessageReaction = TelegramMessageReaction
    { messageReactionChat :: !TelegramChat
    , messageReactionMessageId :: !Integer
    , messageReactionUser :: !(Maybe TelegramUser)
    , messageReactionOld :: ![TelegramReactionType]
    , messageReactionNew :: ![TelegramReactionType]
    } deriving (Eq, Show)

instance FromJSON TelegramMessageReaction where
    parseJSON = withObject "TelegramMessageReaction" \o ->
        TelegramMessageReaction
            <$> o .: "chat"
            <*> o .: "message_id"
            <*> o .:? "user"
            <*> (o .:? "old_reaction" .!= [])
            <*> (o .:? "new_reaction" .!= [])

data TelegramUpdate = TelegramUpdate
    { updateId :: !Integer
    , updateMessage :: !(Maybe TelegramMessage)
    , updateMessageReaction :: !(Maybe TelegramMessageReaction)
    } deriving (Eq, Show)

instance FromJSON TelegramUpdate where
    parseJSON = withObject "TelegramUpdate" \o ->
        TelegramUpdate
            <$> o .: "update_id"
            <*> o .:? "message"
            <*> o .:? "message_reaction"

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
    catalog <- loadModelCatalog home >>= either
        (die . Text.unpack)
        pure
    let provider = config.telegramProvider
        effort = fromMaybe (defaultEffortFor provider) config.telegramEffort
        root = sessionsRoot home
        gatewayDir = gatewayDirectory home
        statePath = statePathFor home
        policy
            | config.telegramYolo = ApproveAll
            | otherwise = DenyMutating
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
    workers <- newMVar Map.empty
    manager <- HttpTls.newTlsManager
    processManager <- newSessionProcessManager root
    let client = TelegramClient token manager
        runtime = TelegramRuntime
            { runtimeClient = client
            , runtimeAllowedUsers = config.telegramAllowedUsers
            , runtimeGatewayDirectory = gatewayDir
            , runtimeSessionsRoot = root
            , runtimeStatePath = statePath
            , runtimeStateVar = stateVar
            , runtimeWorkers = workers
            , runtimeProcessManager = processManager
            , runtimeTarget = target
            , runtimeCwd = cwd
            , runtimeEffort = effort
            , runtimePolicy = policy
            }
    Text.putStrLn "Telegram gateway started (private chats only)."
    race_ (pollForever runtime) (dispatchForever runtime)
        `finally` do
            closeTelegramWorkers runtime
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
    , runtimeAllowedUsers :: !(Set Integer)
    , runtimeGatewayDirectory :: !OsPath
    , runtimeSessionsRoot :: !OsPath
    , runtimeStatePath :: !OsPath
    , runtimeStateVar :: !(MVar TelegramState)
    , runtimeWorkers :: !(MVar (Map TelegramChatKey (Async ())))
    , runtimeProcessManager :: !SessionProcessManager
    , runtimeTarget :: !ModelTarget
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
    | QueueTurn !Integer !TelegramChatKey !Text !(Maybe TelegramVoice)
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
            QueueTurn messageId key text voice ->
                current
                    { pendingTurns =
                        current.pendingTurns
                            <> [TelegramPendingTurn
                                    updateId messageId key text voice]
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
    pure case update.updateMessage of
        Just message
            | message.messageChat.telegramChatType == "private"
            , Just sender <- message.messageFrom
            , sender.userId `Set.member` runtime.runtimeAllowedUsers ->
                let key = TelegramChatKey
                        { chatId = message.messageChat.telegramChatId
                        , messageThreadId = message.messageThread
                        }
                in case message.messageVoice of
                    Just voice ->
                        QueueTurn message.messageId key "[Voice message]" (Just voice)
                    Nothing
                        | Just rawText <- message.messageText
                        , not (Text.null (Text.strip rawText)) ->
                            QueueTurn message.messageId key
                                (Text.strip rawText) Nothing
                    _ -> IgnoreUpdate
        _ -> case update.updateMessageReaction of
            Just reaction
                | reaction.messageReactionChat.telegramChatType == "private"
                , Just sender <- reaction.messageReactionUser
                , sender.userId `Set.member` runtime.runtimeAllowedUsers ->
                    QueueTurn
                        reaction.messageReactionMessageId
                        TelegramChatKey
                            { chatId =
                                reaction.messageReactionChat.telegramChatId
                            , messageThreadId = Nothing
                            }
                        (reactionMessageText reaction)
                        Nothing
            _ -> IgnoreUpdate

reactionMessageText :: TelegramMessageReaction -> Text
reactionMessageText reaction
    | null reaction.messageReactionNew =
        "[Telegram reaction removed from message "
            <> Text.pack (show reaction.messageReactionMessageId)
            <> "]"
    | otherwise =
        "[Telegram reaction on message "
            <> Text.pack (show reaction.messageReactionMessageId)
            <> "]: "
            <> Text.intercalate " "
                (map renderReaction reaction.messageReactionNew)
  where
    renderReaction value = case
        (value.reactionEmoji, value.reactionCustomEmojiId) of
            (Just emoji, _) -> emoji
            (_, Just customId) -> "custom-emoji:" <> customId
            _ -> value.reactionType

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
    workers <- readMVar runtime.runtimeWorkers
    let pendingChats = Set.fromList
            (map (.pendingTurnChat) state.pendingTurns
                <> map (.pendingChat) state.pendingReplies)
        inactive =
            pendingChats `Set.difference` Map.keysSet workers
    forM_ (Set.toList inactive) (startTelegramWorker runtime)
    threadDelay 250_000
    dispatchForever runtime

startTelegramWorker :: TelegramRuntime -> TelegramChatKey -> IO ()
startTelegramWorker runtime key =
    mask \restore -> do
        startGate <- newEmptyMVar
        worker <- async do
            takeMVar startGate
            restore (processChatQueue runtime key) `finally`
                modifyMVar_ runtime.runtimeWorkers
                    (pure . Map.delete key)
        modifyMVar_ runtime.runtimeWorkers \workers ->
            pure (Map.insert key worker workers)
        putMVar startGate ()

closeTelegramWorkers :: TelegramRuntime -> IO ()
closeTelegramWorkers runtime = do
    workers <- modifyMVar runtime.runtimeWorkers \current ->
        pure (Map.empty, Map.elems current)
    forM_ workers cancel
    forM_ workers (void . waitCatch)

processChatQueue :: TelegramRuntime -> TelegramChatKey -> IO ()
processChatQueue runtime key =
    nextChatAction runtime key >>= \case
        Nothing -> pure ()
        Just action -> do
            result <- tryAny case action of
                DeliverReply pending -> do
                    reply runtime pending
                    modifyState runtime \state ->
                        state
                            { pendingReplies =
                                filter
                                    ((/= pending.pendingUpdateId)
                                        . (.pendingUpdateId))
                                    state.pendingReplies
                            }
                RunPendingTurn pending -> do
                    response <- case telegramCommand pending.pendingTurnText of
                        Nothing ->
                            withTelegramProgress
                                runtime.runtimeClient
                                pending.pendingTurnChat
                                (runQueuedTurn runtime pending)
                        Just _ -> runQueuedTurn runtime pending
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
                                            (Just pending.pendingTurnMessageId)
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
        Nothing -> case pending.pendingTurnVoice of
            Nothing ->
                runAgentTurn
                    runtime
                    pending.pendingTurnChat
                    pending.pendingTurnText
            Just voice ->
                tryAny (transcribeTelegramVoice runtime pending voice) >>= \case
                    Left err -> do
                        Text.hPutStrLn stderr $
                            "Telegram voice transcription failed: "
                                <> redactToken
                                    runtime.runtimeClient.clientToken
                                    (Text.pack (displayException err))
                        pure
                            "I could not transcribe that voice message. \
                            \Check that Codex is installed and logged in, and \
                            \that the subscription has usage available."
                    Right prompt -> do
                        checkpointVoiceTranscript runtime pending prompt
                        runAgentTurn runtime pending.pendingTurnChat prompt

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

checkpointPendingVoiceTranscript
    :: Integer
    -> Text
    -> TelegramState
    -> TelegramState
checkpointPendingVoiceTranscript updateId transcript state =
    state
        { pendingTurns =
            map checkpoint state.pendingTurns
        }
  where
    checkpoint current
        | current.pendingTurnUpdateId == updateId =
            current
                { pendingTurnText = transcript
                , pendingTurnVoice = Nothing
                }
        | otherwise = current

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
    filePath <- getTelegramFilePath runtime.runtimeClient voice.voiceFileId
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
        (downloadTelegramFile runtime.runtimeClient filePath localPath)
        (\path -> void (tryAny (removeFile path)))
        \path -> do
            transcriptionCwd <- Directory.getTemporaryDirectory
            transcript <- transcribeWithCodex
                transcriptionCwd
                (unsafeToFilePath path)
            let clean = Text.strip transcript
            when (Text.null clean) $
                fail "Codex returned an empty voice transcription"
            pure ("[Voice message transcript]: " <> clean)

data TelegramFile = TelegramFile
    { telegramFilePath :: !(Maybe Text)
    }

instance FromJSON TelegramFile where
    parseJSON = withObject "TelegramFile" \o ->
        TelegramFile <$> o .:? "file_path"

getTelegramFilePath :: TelegramClient -> Text -> IO FilePath
getTelegramFilePath client fileId =
    telegramRequest client "getFile" (object ["file_id" .= fileId]) 30 >>= \case
        Left err -> fail (Text.unpack err)
        Right response ->
            case decodeTelegramResponse response :: Either Text TelegramFile of
                Left err -> fail (Text.unpack err)
                Right TelegramFile { telegramFilePath = Nothing } ->
                    fail "Telegram getFile response did not contain file_path"
                Right TelegramFile { telegramFilePath = Just path } ->
                    pure (Text.unpack path)

downloadTelegramFile
    :: TelegramClient
    -> FilePath
    -> OsPath
    -> IO OsPath
downloadTelegramFile client remotePath destination = do
    response <- tryAny do
        request <- Http.parseRequest $
            "https://api.telegram.org/file/bot"
                <> Text.unpack client.clientToken
                <> "/"
                <> remotePath
        Http.httpLbs
            request
                { Http.responseTimeout =
                    Http.responseTimeoutMicro 60_000_000
                }
            client.clientManager
    body <- case response of
        Left err ->
            fail . Text.unpack $
                redactToken client.clientToken
                    (Text.pack (displayException err))
        Right value -> pure (Http.responseBody value)
    when (LBS.length body > 20 * 1024 * 1024) $
        fail "Telegram voice download exceeds the 20 MB limit"
    LBS.writeFile (unsafeToFilePath destination) body
    setFileMode (unsafeToFilePath destination) 0o600
    pure destination

transcribeWithCodex :: FilePath -> FilePath -> IO Text
transcribeWithCodex cwd audioPath = do
    result <- Timeout.timeout (5 * 60 * 1_000_000) $
        bracket start stop \(input, output, _) -> do
            send input $ object
                [ "method" .= ("initialize" :: Text)
                , "id" .= (1 :: Int)
                , "params" .= object
                    [ "clientInfo" .= object
                        [ "name" .= ("haskell_agent_telegram" :: Text)
                        , "title" .= ("Haskell Agent Telegram" :: Text)
                        , "version" .= ("0.1.0" :: Text)
                        ]
                    ]
                ]
            _ <- awaitResult output 1
            send input $ object
                [ "method" .= ("initialized" :: Text)
                , "params" .= object []
                ]
            send input $ object
                [ "method" .= ("thread/start" :: Text)
                , "id" .= (2 :: Int)
                , "params" .= object
                    [ "ephemeral" .= True
                    , "cwd" .= cwd
                    , "approvalPolicy" .= ("never" :: Text)
                    , "sandbox" .= ("read-only" :: Text)
                    ]
                ]
            threadResponse <- awaitResult output 2
            threadId <- maybe
                (fail "Codex thread/start response did not contain a thread ID")
                pure
                (lookupText ["result", "thread", "id"] threadResponse)
            send input $ object
                [ "method" .= ("turn/start" :: Text)
                , "id" .= (3 :: Int)
                , "params" .= object
                    [ "threadId" .= threadId
                    , "input" .=
                        [ object
                            [ "type" .= ("text" :: Text)
                            , "text" .=
                                ("Transcribe the attached voice message exactly. \
                                \Return only the transcription, without commentary."
                                    :: Text)
                            ]
                        , object
                            [ "type" .= ("localAudio" :: Text)
                            , "path" .= audioPath
                            ]
                        ]
                    ]
                ]
            _ <- awaitResult output 3
            awaitCodexTranscript output Nothing
    maybe (fail "Codex voice transcription timed out") pure result
  where
    start = do
        (Just input, Just output, _, process) <-
            createProcess (proc "codex" ["app-server", "--stdio"])
                { std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = Inherit
                }
        pure (input, output, process)
    stop (input, output, process) = do
        void (tryAny (hClose input))
        void (tryAny (hClose output))
        void (tryAny (terminateProcess process))
        void (tryAny (waitForProcess process))
    send handle value = do
        LBS8.hPutStrLn handle (encode value)
        hFlush handle

awaitResult :: Handle -> Int -> IO Value
awaitResult output expectedId = do
    value <- readCodexValue output
    case lookupInteger ["id"] value of
        Just actualId
            | actualId == fromIntegral expectedId ->
                case lookupText ["error", "message"] value of
                    Just message -> fail (Text.unpack message)
                    Nothing -> pure value
        _ -> awaitResult output expectedId

awaitCodexTranscript :: Handle -> Maybe Text -> IO Text
awaitCodexTranscript output latest = do
    value <- readCodexValue output
    let latest' = case
            ( lookupText ["method"] value
            , lookupText ["params", "item", "type"] value
            , lookupText ["params", "item", "text"] value
            ) of
                (Just "item/completed", Just "agentMessage", Just text) ->
                    Just text
                _ -> latest
    case lookupText ["method"] value of
        Just "turn/completed" -> case
            lookupText ["params", "turn", "error", "message"] value of
                Just message -> fail (Text.unpack message)
                Nothing ->
                    maybe
                        (fail "Codex completed without a transcription")
                        pure
                        ( latest'
                            <|> lookupText
                                ["params", "turn", "items", "0", "text"]
                                value
                        )
        _ -> awaitCodexTranscript output latest'

readCodexValue :: Handle -> IO Value
readCodexValue output = do
    line <- BS8.hGetLine output
    case eitherDecodeStrict' line of
        Left _ -> readCodexValue output
        Right value -> pure value

lookupText :: [Text] -> Value -> Maybe Text
lookupText path value = case lookupValue path value of
    Just (String text) -> Just text
    _ -> Nothing

lookupInteger :: [Text] -> Value -> Maybe Integer
lookupInteger path value = case lookupValue path value of
    Just number -> case fromJSON number of
        Success integer -> Just integer
        Error _ -> Nothing
    _ -> Nothing

lookupValue :: [Text] -> Value -> Maybe Value
lookupValue [] value = Just value
lookupValue (field : fields) (Object values) =
    KeyMap.lookup (Key.fromText field) values >>= lookupValue fields
lookupValue (index : fields) (Array values) = do
    position <- readMaybe (Text.unpack index)
    value <- values Vector.!? position
    lookupValue fields value
lookupValue _ _ = Nothing

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
    let agentPrompt =
            prompt
                <> "\n\n[Telegram delivery context: If the best response is \
                \only a lightweight acknowledgement, you may respond with \
                \exactly one standard Telegram reaction emoji. Otherwise \
                \respond normally. Do not mention this delivery context.]"
    priorTurnCount <- loadSessionHandle
        runtime.runtimeSessionsRoot
        handle.sessionMeta.metaId >>= \case
            Left err -> fail (Text.unpack err)
            Right (_, turns) -> pure (length turns)
    launchSessionTurn
        runtime.runtimeProcessManager
        False
        runtime.runtimePolicy
        True
        False
        handle
        agentPrompt >>= \case
            Left err -> fail (Text.unpack err)
            Right _ ->
                loadSessionHandle
                    runtime.runtimeSessionsRoot
                    handle.sessionMeta.metaId >>= \case
                        Left err -> fail (Text.unpack err)
                        Right (_, turns)
                            | length turns > priorTurnCount
                            , latestTurnMatches agentPrompt turns ->
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
                , createTarget = runtime.runtimeTarget
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

reply :: TelegramRuntime -> TelegramPendingReply -> IO ()
reply runtime pending
    | Just emoji <- telegramReactionEmoji pending.pendingText
    , Just messageId <- pending.pendingReplyToMessageId = do
        setMessageReaction
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
        -- Rich HTML entities can expand one source character to six bytes.
        -- Keep source chunks conservative so the rendered message stays below
        -- Telegram's 4096-character limit without splitting generated tags.
        let chunks = case splitTelegramText 600 pending.pendingText of
                [] -> ["(empty response)"]
                values -> values
        forM_ chunks \chunk ->
            sendRichMessage
                runtime.runtimeClient
                pending.pendingChat
                chunk >>= \case
                    Left err -> fail (Text.unpack err)
                    Right () -> pure ()

telegramReactionEmoji :: Text -> Maybe Text
telegramReactionEmoji text =
    let candidate = Text.filter (/= '\xFE0F') (Text.strip text)
        normalized = if candidate == "♥" then "❤" else candidate
    in if normalized `Set.member` supportedTelegramReactions
        then Just normalized
        else Nothing

supportedTelegramReactions :: Set Text
supportedTelegramReactions = Set.fromList
    [ "👍", "👎", "❤", "🔥", "🥰", "👏", "😁", "🤔", "🤯", "😱"
    , "🤬", "😢", "🎉", "🤩", "🤮", "💩", "🙏", "👌", "🕊", "🤡"
    , "🥱", "🥴", "😍", "🐳", "🌚", "🌭", "💯", "🤣", "⚡"
    , "🍌", "🏆", "💔", "🤨", "😐", "🍓", "🍾", "💋", "🖕", "😈"
    , "😴", "😭", "🤓", "👻", "👀", "🎃", "🙈", "😇", "😨"
    , "🤝", "✍", "🤗", "🫡", "🎅", "🎄", "☃", "💅", "🤪", "🗿"
    , "🆒", "💘", "🙉", "🦄", "😘", "💊", "🙊", "😎", "👾", "🤷"
    , "😡"
    ]

withTelegramProgress
    :: TelegramClient
    -> TelegramChatKey
    -> IO a
    -> IO a
withTelegramProgress client key action =
    withTelegramProgressUsing
        (sendTypingAction client key)
        (sendThinkingDraft client key)
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

sendTypingAction :: TelegramClient -> TelegramChatKey -> IO ()
sendTypingAction client key =
    telegramRequest client "sendChatAction" body 30 >>= \case
        Left err -> fail (Text.unpack err)
        Right response ->
            case decodeTelegramResponse response :: Either Text Bool of
                Left err -> fail (Text.unpack err)
                Right _ -> pure ()
  where
    body = object $
        [ "chat_id" .= key.chatId
        , "action" .= ("typing" :: Text)
        ]
            <> maybe []
                (\threadId -> ["message_thread_id" .= threadId])
                key.messageThreadId

-- | Convert the Markdown subset commonly emitted by agents to Telegram's
-- supported HTML. Literal text and link attributes are always escaped.
markdownToTelegramHtml :: Text -> Text
markdownToTelegramHtml input =
    Text.concat (zipWith renderSegment [0 :: Int ..] (Text.splitOn "```" input))
  where
    renderSegment index segment
        | odd index =
            "<pre>" <> escapeTelegramHtml (stripLanguage segment) <> "</pre>"
        | otherwise =
            Text.replace "\n" "<br>" (renderInline segment)

    stripLanguage segment =
        case Text.breakOn "\n" segment of
            (language, rest)
                | not (Text.null language)
                , Text.all isLanguageCharacter language ->
                    Text.drop 1 rest
            _ -> segment

    isLanguageCharacter char =
        ('a' <= char && char <= 'z')
            || ('A' <= char && char <= 'Z')
            || ('0' <= char && char <= '9')
            || char `elem` ("-+#_" :: String)

renderInline :: Text -> Text
renderInline text
    | Text.null text = ""
    | Just rest <- Text.stripPrefix "`" text =
        renderDelimited "`" "<code>" "</code>" rest
    | Just rest <- Text.stripPrefix "**" text =
        renderDelimited "**" "<b>" "</b>" rest
    | Just rest <- Text.stripPrefix "~~" text =
        renderDelimited "~~" "<s>" "</s>" rest
    | Just rest <- Text.stripPrefix "*" text =
        renderDelimited "*" "<i>" "</i>" rest
    | Just rest <- Text.stripPrefix "[" text =
        case parseMarkdownLink rest of
            Just (label, url, remaining) ->
                "<a href=\"" <> escapeTelegramHtml url <> "\">"
                    <> escapeTelegramHtml label
                    <> "</a>"
                    <> renderInline remaining
            Nothing -> "&#91;" <> renderInline rest
    | otherwise =
        let (plain, rest) = Text.break isMarkdownStarter text
        in if Text.null plain
            then escapeTelegramHtml (Text.take 1 text)
                <> renderInline (Text.drop 1 text)
            else escapeTelegramHtml plain <> renderInline rest
  where
    renderDelimited delimiter opening closing rest =
        case Text.breakOn delimiter rest of
            (_, after) | Text.null after ->
                escapeTelegramHtml delimiter <> renderInline rest
            (inside, after) ->
                opening
                    <> escapeTelegramHtml inside
                    <> closing
                    <> renderInline (Text.drop (Text.length delimiter) after)

    isMarkdownStarter char =
        char == '`' || char == '*' || char == '~' || char == '['

parseMarkdownLink :: Text -> Maybe (Text, Text, Text)
parseMarkdownLink text = do
    let (label, afterLabel) = Text.breakOn "](" text
    unlessMaybe (not (Text.null afterLabel))
    let afterOpen = Text.drop 2 afterLabel
        (url, afterUrl) = Text.breakOn ")" afterOpen
    unlessMaybe (not (Text.null afterUrl))
    pure (label, url, Text.drop 1 afterUrl)

unlessMaybe :: Bool -> Maybe ()
unlessMaybe True = Just ()
unlessMaybe False = Nothing

escapeTelegramHtml :: Text -> Text
escapeTelegramHtml = Text.concatMap \case
    '<' -> "&lt;"
    '>' -> "&gt;"
    '&' -> "&amp;"
    '"' -> "&quot;"
    '\'' -> "&#39;"
    char -> Text.singleton char

sendThinkingDraft :: TelegramClient -> TelegramChatKey -> IO ()
sendThinkingDraft client key =
    telegramRequest client "sendRichMessageDraft" body 30 >>= \case
        Left err -> fail (Text.unpack err)
        Right response ->
            case decodeTelegramResponse response :: Either Text Bool of
                Left err -> fail (Text.unpack err)
                Right _ -> pure ()
  where
    body = object $
        [ "chat_id" .= key.chatId
        , "draft_id" .= (1 :: Int)
        , "rich_message" .= object
            [ "html" .=
                ("<tg-thinking>✨ Thinking…</tg-thinking>" :: Text)
            ]
        ]
            <> maybe []
                (\threadId -> ["message_thread_id" .= threadId])
                key.messageThreadId

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
        , "allowed_updates" .=
            (["message", "message_reaction"] :: [Text])
        ]
            <> maybe [] (\value -> ["offset" .= value]) offset

setMessageReaction
    :: TelegramClient
    -> TelegramChatKey
    -> Integer
    -> Text
    -> IO (Either Text ())
setMessageReaction client key messageId emoji =
    telegramRequest client "setMessageReaction" body 30 >>= \case
        Left err -> pure (Left err)
        Right response ->
            pure (() <$ (decodeTelegramResponse response :: Either Text Bool))
  where
    body = object
        [ "chat_id" .= key.chatId
        , "message_id" .= messageId
        , "reaction" .=
            [ object
                [ "type" .= ("emoji" :: Text)
                , "emoji" .= emoji
                ]
            ]
        ]

sendRichMessage
    :: TelegramClient
    -> TelegramChatKey
    -> Text
    -> IO (Either Text ())
sendRichMessage client key text =
    telegramRequest client "sendRichMessage" richBody 30 >>= \case
        Right response
            | Right (_ :: Value) <- decodeTelegramResponse response ->
                pure (Right ())
        _ -> sendHtmlMessage client key text
  where
    richBody = object $
        [ "chat_id" .= key.chatId
        , "rich_message" .= object
            [ "html" .= markdownToTelegramHtml text
            ]
        ]
            <> maybe []
                (\threadId -> ["message_thread_id" .= threadId])
                key.messageThreadId

sendHtmlMessage
    :: TelegramClient
    -> TelegramChatKey
    -> Text
    -> IO (Either Text ())
sendHtmlMessage client key text =
    telegramRequest client "sendMessage" htmlBody 30 >>= \case
        Right response
            | Right (_ :: Value) <- decodeTelegramResponse response ->
                pure (Right ())
        _ -> sendPlainMessage client key text
  where
    htmlBody = object $
        [ "chat_id" .= key.chatId
        , "text" .= markdownToTelegramHtml text
        , "parse_mode" .= ("HTML" :: Text)
        ]
            <> maybe []
                (\threadId -> ["message_thread_id" .= threadId])
                key.messageThreadId

sendPlainMessage
    :: TelegramClient
    -> TelegramChatKey
    -> Text
    -> IO (Either Text ())
sendPlainMessage client key text =
    telegramRequest client "sendMessage" (messageBody key text) 30 >>= \case
        Left err -> pure (Left err)
        Right response ->
            pure (() <$ (decodeTelegramResponse response :: Either Text Value))

messageBody :: TelegramChatKey -> Text -> Value
messageBody key text =
    object $
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
