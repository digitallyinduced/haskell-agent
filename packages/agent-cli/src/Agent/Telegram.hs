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
import Agent.Telegram.Types
import Agent.Telegram.Markdown (markdownToTelegramHtml)
import Agent.Telegram.Voice (transcribeWithCodex)
import Agent.FileRetry (writeLazyFileAtomically)
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
    , Value(..)
    , eitherDecode
    , encode
    , object
    , withObject
    , (.:?)
    , (.=)
    )
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
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
        "--all-group-messages" : rest ->
            go options { setupRespondToAllGroupMessages = True } rest
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
    , "  --all-group-messages  respond to every allowed-user group message"
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
            , telegramRespondToAllGroupMessages =
                options.setupRespondToAllGroupMessages
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
    let client = TelegramClient token manager
    bot <- getTelegramBot client
    processManager <- newSessionProcessManager root
    let runtime = TelegramRuntime
            { runtimeClient = client
            , runtimeBot = bot
            , runtimeAllowedUsers = config.telegramAllowedUsers
            , runtimeRespondToAllGroupMessages =
                config.telegramRespondToAllGroupMessages
            , runtimeGatewayDirectory = gatewayDir
            , runtimePool = trustedPool store
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
    Text.putStrLn "Telegram gateway started (private and group chats)."
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
    , runtimeBot :: !TelegramUser
    , runtimeAllowedUsers :: !(Set Integer)
    , runtimeRespondToAllGroupMessages :: !Bool
    , runtimeGatewayDirectory :: !OsPath
    , runtimePool :: !StorePool
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
                enqueuePendingAction
                    (RunPendingTurn
                        (TelegramPendingTurn
                            updateId messageId key text voice))
                    current
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
    pure (classifyTelegramUpdateWithMode
        runtime.runtimeBot
        runtime.runtimeAllowedUsers
        runtime.runtimeRespondToAllGroupMessages
        update)

classifyTelegramUpdate
    :: TelegramUser
    -> Set Integer
    -> TelegramUpdate
    -> TelegramUpdateAction
classifyTelegramUpdate bot allowedUsers =
    classifyTelegramUpdateWithMode bot allowedUsers False

classifyTelegramUpdateWithMode
    :: TelegramUser
    -> Set Integer
    -> Bool
    -> TelegramUpdate
    -> TelegramUpdateAction
classifyTelegramUpdateWithMode bot allowedUsers respondToAllGroupMessages update =
    case update.updateMessage of
        Just message
            | Just sender <- message.messageFrom
            , sender.userId `Set.member` allowedUsers ->
                classifyMessage
                    bot
                    sender
                    respondToAllGroupMessages
                    message
        _ -> case update.updateMessageReaction of
            Just reaction
                | reaction.messageReactionChat.telegramChatType == "private"
                , Just sender <- reaction.messageReactionUser
                , sender.userId `Set.member` allowedUsers ->
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

classifyMessage
    :: TelegramUser
    -> TelegramUser
    -> Bool
    -> TelegramMessage
    -> TelegramUpdateAction
classifyMessage bot sender respondToAllGroupMessages message =
    case message.messageChat.telegramChatType of
        "private" -> queueMessage id
        "group" -> classifyGroupMessage
        "supergroup" -> classifyGroupMessage
        _ -> IgnoreUpdate
  where
    key = TelegramChatKey
        { chatId = message.messageChat.telegramChatId
        , messageThreadId = message.messageThread
        }

    classifyGroupMessage
        | Just rawText <- message.messageText
        , Just target <- explicitCommandTarget (Text.strip rawText)
        , not (botUsernameMatches bot target) =
            IgnoreUpdate
        | messageRepliesToBot bot message =
            queueGroupReply
        | Just rawText <- message.messageText
        , Just targetedText <- groupTextForBot bot rawText =
            queueText (attributeGroupText sender targetedText)
        | respondToAllGroupMessages =
            queueGroupReply
        | otherwise = IgnoreUpdate

    queueGroupReply =
        case message.messageVoice of
            Just voice ->
                QueueTurn message.messageId key
                    (attributeGroupMessage sender "[Voice message]")
                    (Just voice)
            Nothing
                | Just rawText <- message.messageText ->
                    queueText (attributeGroupText sender rawText)
            _ -> IgnoreUpdate

    queueMessage transform =
        case message.messageVoice of
            Just voice ->
                QueueTurn message.messageId key
                    (transform "[Voice message]")
                    (Just voice)
            Nothing
                | Just rawText <- message.messageText ->
                    queueText (transform rawText)
            _ -> IgnoreUpdate

    queueText rawText
        | Text.null clean = IgnoreUpdate
        | otherwise =
            QueueTurn message.messageId key clean Nothing
      where
        clean = Text.strip rawText

messageRepliesToBot :: TelegramUser -> TelegramMessage -> Bool
messageRepliesToBot bot message =
    case message.messageReplyTo >>= (.messageFrom) of
        Just repliedTo -> repliedTo.userId == bot.userId
        Nothing -> False

groupTextForBot :: TelegramUser -> Text -> Maybe Text
groupTextForBot bot rawText =
    case explicitCommandTarget clean of
        Just target
            | usernameMatches target -> Just clean
            | otherwise -> Nothing
        Nothing -> stripBotMention bot clean
  where
    clean = Text.strip rawText
    usernameMatches = botUsernameMatches bot

botUsernameMatches :: TelegramUser -> Text -> Bool
botUsernameMatches bot target =
    maybe False
        ((== Text.toCaseFold target) . Text.toCaseFold)
        bot.userUsername

explicitCommandTarget :: Text -> Maybe Text
explicitCommandTarget text = do
    firstWord <- case Text.words text of
        [] -> Nothing
        value : _ -> Just value
    command <- Text.stripPrefix "/" firstWord
    let (_, targetWithAt) = Text.breakOn "@" command
    Text.stripPrefix "@" targetWithAt

stripBotMention :: TelegramUser -> Text -> Maybe Text
stripBotMention bot text = do
    username <- bot.userUsername
    let needle = "@" <> Text.map asciiLower username
        folded = Text.map asciiLower text
        (beforeFolded, matchAndAfter) = Text.breakOn needle folded
    if Text.null matchAndAfter
        then Nothing
        else do
            let mentionOffset = Text.length beforeFolded
                (before, mentionAndAfter) = Text.splitAt mentionOffset text
                after = Text.drop (Text.length needle) mentionAndAfter
            if mentionBoundaryBefore before && mentionBoundaryAfter after
                then Just (Text.strip (before <> after))
                else Nothing

asciiLower :: Char -> Char
asciiLower char
    | 'A' <= char && char <= 'Z' =
        toEnum (fromEnum char + fromEnum 'a' - fromEnum 'A')
    | otherwise = char

mentionBoundaryBefore :: Text -> Bool
mentionBoundaryBefore text =
    maybe True (not . isTelegramUsernameCharacter) (lastTextCharacter text)

mentionBoundaryAfter :: Text -> Bool
mentionBoundaryAfter text =
    maybe True (not . isTelegramUsernameCharacter) (firstTextCharacter text)

isTelegramUsernameCharacter :: Char -> Bool
isTelegramUsernameCharacter char =
    ('a' <= char && char <= 'z')
        || ('A' <= char && char <= 'Z')
        || ('0' <= char && char <= '9')
        || char == '_'

firstTextCharacter :: Text -> Maybe Char
firstTextCharacter = fmap fst . Text.uncons

lastTextCharacter :: Text -> Maybe Char
lastTextCharacter = fmap snd . Text.unsnoc

attributeGroupText :: TelegramUser -> Text -> Text
attributeGroupText sender text
    | telegramCommand text /= Nothing = text
    | otherwise = attributeGroupMessage sender text

attributeGroupMessage :: TelegramUser -> Text -> Text
attributeGroupMessage sender text =
    "[Telegram group message from "
        <> telegramUserLabel sender
        <> "]\n"
        <> text

telegramUserLabel :: TelegramUser -> Text
telegramUserLabel user =
    let name = Text.unwords
            [ value
            | Just value <- [user.userFirstName, user.userLastName]
            , not (Text.null (Text.strip value))
            ]
        username = ("@" <>) <$> user.userUsername
        identityParts =
            filter (not . Text.null)
                [ name
                , fromMaybe "" username
                , "user " <> Text.pack (show user.userId)
                ]
    in Text.intercalate ", " identityParts

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
    maybe False (> updateId) state.nextUpdateId
        || any (Map.member updateId) state.pendingQueues

dispatchForever :: TelegramRuntime -> IO ()
dispatchForever runtime = do
    state <- readMVar runtime.runtimeStateVar
    workers <- readMVar runtime.runtimeWorkers
    let inactive =
            Map.keysSet state.pendingQueues
                `Set.difference`
                    Map.keysSet workers
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
                    modifyState runtime
                        (deletePendingAction (DeliverReply pending))
                RunPendingTurn pending -> do
                    response <- case telegramCommand pending.pendingTurnText of
                        Nothing ->
                            withTelegramProgress
                                runtime.runtimeClient
                                pending.pendingTurnChat
                                (runQueuedTurn runtime pending)
                        Just _ -> runQueuedTurn runtime pending
                    modifyState runtime \state ->
                        enqueuePendingAction
                            (DeliverReply
                                (TelegramPendingReply
                                    pending.pendingTurnUpdateId
                                    pending.pendingTurnChat
                                    (Just pending.pendingTurnMessageId)
                                    response))
                            (deletePendingAction (RunPendingTurn pending) state)
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
        { pendingQueues =
            fmap (fmap checkpoint) state.pendingQueues
        }
  where
    checkpoint = \case
        RunPendingTurn current
            | current.pendingTurnUpdateId == updateId ->
                RunPendingTurn current
                    { pendingTurnText = transcript
                    , pendingTurnVoice = Nothing
                    }
        current -> current

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
            pure $
                let rendered = "[Voice message transcript]: " <> clean
                in if pending.pendingTurnText == "[Voice message]"
                    then rendered
                    else pending.pendingTurnText <> "\n" <> rendered

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
    snd <$> (Map.lookupMin =<< Map.lookup key state.pendingQueues)

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
        runtime.runtimePool
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
                    runtime.runtimePool
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
        forM_ (zip [0 :: Int ..] chunks) \(index, chunk) ->
            sendRichMessage
                runtime.runtimeClient
                pending.pendingChat
                (if index == 0
                    then pending.pendingReplyToMessageId
                    else Nothing)
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

telegramReplyParameters :: Maybe Integer -> [(Key.Key, Value)]
telegramReplyParameters =
    maybe [] \messageId ->
        [ "reply_parameters" .= object
            [ "message_id" .= messageId
            , "allow_sending_without_reply" .= True
            ]
        ]

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

getTelegramBot :: TelegramClient -> IO TelegramUser
getTelegramBot client =
    telegramRequest client "getMe" (object []) 15 >>= \case
        Left err -> fail (Text.unpack err)
        Right response ->
            case decodeTelegramResponse response of
                Left err -> fail (Text.unpack err)
                Right bot -> pure bot

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
    -> Maybe Integer
    -> Text
    -> IO (Either Text ())
sendRichMessage client key replyToMessageId text =
    telegramRequest client "sendRichMessage" richBody 30 >>= \case
        Right response
            | Right (_ :: Value) <- decodeTelegramResponse response ->
                pure (Right ())
        _ -> sendHtmlMessage client key replyToMessageId text
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
            <> telegramReplyParameters replyToMessageId

sendHtmlMessage
    :: TelegramClient
    -> TelegramChatKey
    -> Maybe Integer
    -> Text
    -> IO (Either Text ())
sendHtmlMessage client key replyToMessageId text =
    telegramRequest client "sendMessage" htmlBody 30 >>= \case
        Right response
            | Right (_ :: Value) <- decodeTelegramResponse response ->
                pure (Right ())
        _ -> sendPlainMessage client key replyToMessageId text
  where
    htmlBody = object $
        [ "chat_id" .= key.chatId
        , "text" .= markdownToTelegramHtml text
        , "parse_mode" .= ("HTML" :: Text)
        ]
            <> maybe []
                (\threadId -> ["message_thread_id" .= threadId])
                key.messageThreadId
            <> telegramReplyParameters replyToMessageId

sendPlainMessage
    :: TelegramClient
    -> TelegramChatKey
    -> Maybe Integer
    -> Text
    -> IO (Either Text ())
sendPlainMessage client key replyToMessageId text =
    telegramRequest client "sendMessage"
        (messageBody key replyToMessageId text)
        30 >>= \case
        Left err -> pure (Left err)
        Right response ->
            pure (() <$ (decodeTelegramResponse response :: Either Text Value))

messageBody :: TelegramChatKey -> Maybe Integer -> Text -> Value
messageBody key replyToMessageId text =
    object $
        [ "chat_id" .= key.chatId
        , "text" .= text
        ]
            <> maybe []
                (\threadId -> ["message_thread_id" .= threadId])
                key.messageThreadId
            <> telegramReplyParameters replyToMessageId

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
