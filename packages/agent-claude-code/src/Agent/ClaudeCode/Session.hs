-- | Lifecycle management for an interactive, subscription-authenticated
-- Claude Code process.
--
-- Claude Code deliberately keeps subscription use inside its own interactive
-- harness.  This module therefore gives it a real pseudo-terminal and keeps
-- one process alive across turns.  Callers consume the durable JSONL
-- transcript rather than attempting to parse terminal rendering.
module Agent.ClaudeCode.Session
    ( ClaudeCodePermission(..)
    , ClaudeCodeOptions(..)
    , defaultClaudeCodeOptions
    , ClaudeCodeSession
    , ClaudeCodeTurn
    , withClaudeCodeSession
    , withClaudeCodeSessionWithoutTools
    , withClaudeCodeTurn
    , sendClaudeCodePrompt
    , claudeCodeTurnSessionId
    , claudeCodeTurnTranscriptPath
    , claudeCodeTurnTranscriptOffset
    , claudeCodeTurnIsNewSession
    , claudeCodeTurnProcessExit
    , claudeCodeTurnDiagnostic
    ) where

import Agent.ClaudeCode.Internal.Environment
    ( getSanitizedClaudeEnvironment )
import Agent.Error (ApiError(..))
import Agent.Tools.IO (terminateProcessGroup)
import Control.Applicative ((<|>))
import Control.Concurrent.Async
    ( Async
    , asyncWithUnmask
    , cancel
    , waitCatch
    )
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
    ( MVar
    , newMVar
    , withMVar
    )
import Control.Exception.Safe
    ( SomeException
    , bracket
    , catchAny
    , mask
    , onException
    , tryAny
    )
import Control.Monad (filterM, void)
import Data.Bits ((.&.), (.|.))
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Char (isAlphaNum, isAscii, isControl)
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.UUID.Types as UUID
import System.Directory
    ( canonicalizePath
    , doesDirectoryExist
    , doesFileExist
    , getFileSize
    , getHomeDirectory
    , listDirectory
    )
import System.Entropy (getEntropy)
import System.Environment (lookupEnv)
import System.Exit (ExitCode)
import System.FilePath
    ( (</>)
    , isRelative
    , takeFileName
    )
import System.IO
    ( BufferMode(NoBuffering)
    , Handle
    , hClose
    , hFlush
    , hSetBinaryMode
    , hSetBuffering
    )
import System.Posix.IO
    ( closeFd
    , dup
    , fdToHandle
    )
import System.Posix.Terminal (openPseudoTerminal)
import System.Posix.Types (ProcessGroupID)
import System.Process
    ( CreateProcess(..)
    , ProcessHandle
    , StdStream(UseHandle)
    , createProcess
    , getPid
    , getProcessExitCode
    , proc
    , waitForProcess
    )

-- | Claude's non-interactive permission policies that cannot pause to ask the
-- host terminal user for confirmation.
data ClaudeCodePermission
    = ClaudeCodeDontAsk
    | ClaudeCodeBypass
    deriving (Eq, Show)

data ClaudeCodeToolMode
    = ClaudeCodeDefaultTools
    | ClaudeCodeNoTools
    deriving (Eq, Show)

-- | Options that are stable for the lifetime of a backend.
data ClaudeCodeOptions = ClaudeCodeOptions
    { executable :: !FilePath
    , cwd :: !FilePath
    , permission :: !ClaudeCodePermission
    , safeMode :: !Bool
    } deriving (Eq, Show)

defaultClaudeCodeOptions :: FilePath -> FilePath -> ClaudeCodeOptions
defaultClaudeCodeOptions executable cwd = ClaudeCodeOptions
    { executable
    , cwd
    , permission = ClaudeCodeDontAsk
    , safeMode = True
    }

-- | A serialized owner for at most one interactive Claude process.
data ClaudeCodeSession = ClaudeCodeSession
    { sessionOptions :: !ClaudeCodeOptions
    , sessionToolMode :: !ClaudeCodeToolMode
    , sessionInitialPrevious :: !(Maybe Text)
    , sessionInitialPreviousConsumed :: !(IORef Bool)
    , sessionState :: !(IORef SessionState)
    , sessionTurnLock :: !(MVar ())
    }

data SessionState = SessionState
    { stateRunning :: !(Maybe RunningClaude)
    -- True when the current session UUID already has a completed turn and may
    -- therefore be resumed after a process restart.
    , stateHadCompletedTurn :: !Bool
    }

data RunningClaude = RunningClaude
    { runningSessionId :: !Text
    , runningModel :: !(Maybe Text)
    , runningEffort :: !(Maybe Text)
    , runningTranscriptPath :: !FilePath
    , runningInput :: !Handle
    , runningOutput :: !Handle
    , runningProcess :: !ProcessHandle
    , runningGroupId :: !(Maybe ProcessGroupID)
    , runningDiagnosticBytes :: !(IORef ByteString.ByteString)
    , runningReadyPrompts :: !(IORef Int)
    , runningConsumedPrompts :: !(IORef Int)
    , runningDrain :: !(Async ())
    }

-- | Immutable view of the process and transcript boundary for one submitted
-- turn.  The owning 'ClaudeCodeSession' keeps the underlying process alive.
data ClaudeCodeTurn = ClaudeCodeTurn
    { turnRunning :: !RunningClaude
    , turnTranscriptOffset :: !Integer
    , turnIsNewSession :: !Bool
    }

-- | Allocate a serialized session manager and clean up its process group when
-- the callback exits, including on asynchronous cancellation.
withClaudeCodeSession
    :: ClaudeCodeOptions
    -> Maybe Text
    -> (ClaudeCodeSession -> IO a)
    -> IO a
withClaudeCodeSession =
    withClaudeCodeSessionMode ClaudeCodeDefaultTools

-- | Allocate a session whose Claude process cannot invoke built-in tools.
-- Auxiliary requests such as title generation and /btw must remain pure model
-- calls because their host loop intentionally does not expose tool execution.
withClaudeCodeSessionWithoutTools
    :: ClaudeCodeOptions
    -> Maybe Text
    -> (ClaudeCodeSession -> IO a)
    -> IO a
withClaudeCodeSessionWithoutTools =
    withClaudeCodeSessionMode ClaudeCodeNoTools

withClaudeCodeSessionMode
    :: ClaudeCodeToolMode
    -> ClaudeCodeOptions
    -> Maybe Text
    -> (ClaudeCodeSession -> IO a)
    -> IO a
withClaudeCodeSessionMode toolMode options initialPrevious =
    bracket acquire release
  where
    acquire :: IO ClaudeCodeSession
    acquire = ClaudeCodeSession options toolMode initialPrevious
        <$> newIORef False
        <*> newIORef emptySessionState
        <*> newMVar ()
    release :: ClaudeCodeSession -> IO ()
    release session = do
        state <- readIORef session.sessionState
        mapM_ stopRunningClaude state.stateRunning
        writeIORef session.sessionState emptySessionState

-- | Run one transcript-consuming action against the appropriate persistent
-- Claude process.
--
-- A valid UUID in the host's previous-response field resumes that Claude
-- session.  Model or effort changes restart the process while preserving the
-- UUID.  Passing no previous response after a completed turn is treated as an
-- explicit new conversation and receives a fresh UUID.
withClaudeCodeTurn
    :: ClaudeCodeSession
    -> Maybe Text
    -> Maybe Text
    -> Maybe Text
    -> (ClaudeCodeTurn -> IO (Either ApiError a))
    -> IO (Either ApiError a)
withClaudeCodeTurn session previous model effort callback =
    withMVar session.sessionTurnLock \_ ->
        mask \restore -> do
            selectedPrevious <- selectPrevious session previous
            prepared <- tryAny $ prepareTurn session
                selectedPrevious
                (nonEmptyText model)
                (nonEmptyText effort)
            case prepared of
                Left exception ->
                    pure (Left (connectionError "Failed to start Claude Code" exception))
                Right turn -> do
                    result <- tryAny $
                        restore (callback turn)
                            `onException` abortClaudeCodeTurn turn
                    case result of
                        Left exception ->
                            pure (Left (connectionError "Claude Code turn failed" exception))
                        Right answer -> do
                            case answer of
                                Left _ ->
                                    abortClaudeCodeTurn turn
                                Right _ ->
                                    markTurnCompleted session turn
                            pure answer

-- | Submit one prompt through bracketed paste.  Escape and NUL characters are
-- removed so prompt content cannot break out of the paste protocol and inject
-- terminal control sequences.
sendClaudeCodePrompt :: ClaudeCodeTurn -> Text -> IO ()
sendClaudeCodePrompt turn prompt = do
    awaitReadyPrompt turn.turnRunning
    let safeCharacter character =
            not (isControl character)
                || character == '\n'
                || character == '\t'
        clean = Text.filter safeCharacter prompt
        bytes =
            "\ESC[200~"
                <> TextEncoding.encodeUtf8 clean
                <> "\ESC[201~\r"
    ByteString.hPut turn.turnRunning.runningInput bytes
    hFlush turn.turnRunning.runningInput

claudeCodeTurnSessionId :: ClaudeCodeTurn -> Text
claudeCodeTurnSessionId turn =
    turn.turnRunning.runningSessionId

claudeCodeTurnTranscriptPath :: ClaudeCodeTurn -> FilePath
claudeCodeTurnTranscriptPath turn =
    turn.turnRunning.runningTranscriptPath

claudeCodeTurnTranscriptOffset :: ClaudeCodeTurn -> Integer
claudeCodeTurnTranscriptOffset = (.turnTranscriptOffset)

claudeCodeTurnIsNewSession :: ClaudeCodeTurn -> Bool
claudeCodeTurnIsNewSession = (.turnIsNewSession)

claudeCodeTurnProcessExit :: ClaudeCodeTurn -> IO (Maybe ExitCode)
claudeCodeTurnProcessExit turn =
    getProcessExitCode turn.turnRunning.runningProcess

-- | A bounded tail of the PTY rendering, intended only for connection-error
-- diagnostics.  User-visible model output comes from the JSONL transcript.
claudeCodeTurnDiagnostic :: ClaudeCodeTurn -> IO Text
claudeCodeTurnDiagnostic turn =
    TextEncoding.decodeUtf8With lenientDecode
        <$> readIORef turn.turnRunning.runningDiagnosticBytes

emptySessionState :: SessionState
emptySessionState = SessionState
    { stateRunning = Nothing
    , stateHadCompletedTurn = False
    }

selectPrevious :: ClaudeCodeSession -> Maybe Text -> IO (Maybe Text)
selectPrevious session explicit = do
    useInitial <- atomicModifyIORef'
        session.sessionInitialPreviousConsumed
        \consumed -> (True, not consumed)
    pure $
        explicit
            <|> if useInitial
                then session.sessionInitialPrevious
                else Nothing

prepareTurn
    :: ClaudeCodeSession
    -> Maybe Text
    -> Maybe Text
    -> Maybe Text
    -> IO ClaudeCodeTurn
prepareTurn session previous model effort = do
    oldState <- readIORef session.sessionState
    processExited <- case oldState.stateRunning of
        Nothing -> pure False
        Just running -> isJust <$> getProcessExitCode running.runningProcess
    decision <- decideProcess oldState processExited previous model effort
    (running, newState, isNewSession) <- case decision of
        Reuse running ->
            pure (running, oldState, False)
        Restart mode completed -> do
            mapM_ stopRunningClaude oldState.stateRunning
            -- Clear ownership before launch.  If launch fails there must be no
            -- stale handle for outer cleanup to touch.
            writeIORef session.sessionState emptySessionState
            running <- startRunningClaude
                session.sessionOptions
                session.sessionToolMode
                model
                effort
                mode
            let state = SessionState
                    { stateRunning = Just running
                    , stateHadCompletedTurn = completed
                    }
            -- Store the newly acquired process before exposing it to a caller.
            writeIORef session.sessionState state
            pure (running, state, isNewStart mode)
    offset <- transcriptSize running.runningTranscriptPath
    -- Keep the state write explicit in the reuse branch as documentation that
    -- the process remains owned for the complete callback lifetime.
    writeIORef session.sessionState newState
    pure ClaudeCodeTurn
        { turnRunning = running
        , turnTranscriptOffset = offset
        , turnIsNewSession = isNewSession
        }

data ProcessDecision
    = Reuse !RunningClaude
    | Restart !StartMode !Bool

data StartMode
    = StartNew !Text
    | StartResume !Text

decideProcess
    :: SessionState
    -> Bool
    -> Maybe Text
    -> Maybe Text
    -> Maybe Text
    -> IO ProcessDecision
decideProcess state processExited previous model effort =
    case state.stateRunning of
        Nothing ->
            case previous >>= validSessionId of
                Just sessionId -> pure (Restart (StartResume sessionId) True)
                Nothing -> freshStart
        Just running
            | Just rawPrevious <- previous
            , Nothing <- validSessionId rawPrevious ->
                -- A response ID from another provider cannot identify a Claude
                -- transcript.  Start clean; LoopBackend can import host history.
                freshStart
            | Just sessionId <- previous >>= validSessionId
            , sessionId /= running.runningSessionId ->
                pure (Restart (StartResume sessionId) True)
            | previous == Nothing && state.stateHadCompletedTurn ->
                freshStart
            | processExited ->
                -- A cancelled or failed callback stops the old process before
                -- releasing the turn lock. Reuse its UUID so late JSONL writes
                -- from that process cannot cross the next baseline, while
                -- Claude still retains any durable session context.
                restartStopped running
            | modeChanged running model effort ->
                pure $
                    Restart
                        (if state.stateHadCompletedTurn
                            then StartResume running.runningSessionId
                            else StartNew running.runningSessionId)
                        state.stateHadCompletedTurn
            | otherwise ->
                pure (Reuse running)
  where
    freshStart = do
        sessionId <- newSessionId
        pure (Restart (StartNew sessionId) False)
    restartStopped :: RunningClaude -> IO ProcessDecision
    restartStopped running = do
        transcriptExists <- doesFileExist running.runningTranscriptPath
        pure $
            Restart
                (if state.stateHadCompletedTurn || transcriptExists
                    then StartResume running.runningSessionId
                    else StartNew running.runningSessionId)
                state.stateHadCompletedTurn

modeChanged :: RunningClaude -> Maybe Text -> Maybe Text -> Bool
modeChanged running model effort =
    running.runningModel /= model
        || running.runningEffort /= effort

markTurnCompleted :: ClaudeCodeSession -> ClaudeCodeTurn -> IO ()
markTurnCompleted session turn =
    atomicModifyIORef' session.sessionState \state ->
        let sameProcess =
                case state.stateRunning of
                    Just running ->
                        running.runningSessionId
                            == turn.turnRunning.runningSessionId
                    Nothing -> False
        in
            ( if sameProcess
                then state { stateHadCompletedTurn = True }
                else state
            , ()
            )

startRunningClaude
    :: ClaudeCodeOptions
    -> ClaudeCodeToolMode
    -> Maybe Text
    -> Maybe Text
    -> StartMode
    -> IO RunningClaude
startRunningClaude options toolMode model effort mode =
    mask \restore -> do
        workingDirectory <- canonicalizePath options.cwd
        transcriptPath <- transcriptPathFor workingDirectory (startSessionId mode)
        environment <- prepareInteractiveEnvironment
            <$> getSanitizedClaudeEnvironment
        (parentInput, parentOutput, childSlave) <- acquirePtyHandles
        mapM_ (`hSetBinaryMode` True) [parentInput, parentOutput, childSlave]
        mapM_ (`hSetBuffering` NoBuffering) [parentInput, parentOutput, childSlave]
        let arguments =
                startArguments mode
                    -- Claude's fullscreen renderer waits for terminal geometry
                    -- and focus events that an internal, drained PTY cannot
                    -- provide reliably. The flat accessibility renderer keeps
                    -- the interactive subscription session scriptable while
                    -- model output still comes from the JSONL transcript.
                    <> ["--ax-screen-reader"]
                    <> permissionArguments options.permission
                    <> toolArguments toolMode
                    <> ["--safe-mode" | options.safeMode]
                    <> optionalArgument "--model" model
                    <> optionalEffortArgument effort
            processSpec = (proc options.executable arguments)
                { cwd = Just workingDirectory
                , env = Just environment
                , std_in = UseHandle childSlave
                , std_out = UseHandle childSlave
                , std_err = UseHandle childSlave
                , close_fds = True
                , create_group = True
                , new_session = True
                }
            closePty =
                mapM_ closeHandleQuietly [parentInput, parentOutput, childSlave]
        created <- createProcess processSpec
            `onException` closePty
        let (_, _, _, processHandle) = created
            stopCreated groupId = do
                terminateProcessGroup groupId processHandle
                closePty
                void (tryAny (waitForProcess processHandle))
        groupId <- getPid processHandle
            `onException` stopCreated Nothing
        hClose childSlave
            `onException` stopCreated groupId
        diagnosticRef <- newIORef ByteString.empty
        readyPromptsRef <- newIORef 0
        consumedPromptsRef <- newIORef 0
        drain <-
            (asyncWithUnmask \unmask ->
                unmask (drainPty parentInput diagnosticRef readyPromptsRef)
                    `catchAny` \_ -> pure ())
            `onException` stopCreated groupId
        let running = RunningClaude
                { runningSessionId = startSessionId mode
                , runningModel = model
                , runningEffort = effort
                , runningTranscriptPath = transcriptPath
                , runningInput = parentOutput
                , runningOutput = parentInput
                , runningProcess = processHandle
                , runningGroupId = groupId
                , runningDiagnosticBytes = diagnosticRef
                , runningReadyPrompts = readyPromptsRef
                , runningConsumedPrompts = consumedPromptsRef
                , runningDrain = drain
                }
        restore (pure running) `onException` stopRunningClaude running

-- | Allocate distinct read/write Handles for one PTY master and a Handle for
-- its slave.  Distinct master Handles avoid blocking Handle-level locks.
acquirePtyHandles :: IO (Handle, Handle, Handle)
acquirePtyHandles =
    mask \_ -> do
        (masterFd, slaveFd) <- openPseudoTerminal
        writeFd <- dup masterFd `onException` do
            closeFd masterFd
            closeFd slaveFd
        readHandle <- fdToHandle masterFd `onException` do
            closeFd masterFd
            closeFd writeFd
            closeFd slaveFd
        writeHandle <- fdToHandle writeFd `onException` do
            hClose readHandle
            closeFd writeFd
            closeFd slaveFd
        slaveHandle <- fdToHandle slaveFd `onException` do
            hClose readHandle
            hClose writeHandle
            closeFd slaveFd
        pure (readHandle, writeHandle, slaveHandle)

stopRunningClaude :: RunningClaude -> IO ()
stopRunningClaude running = do
    terminateProcessGroup running.runningGroupId running.runningProcess
    closeHandleQuietly running.runningInput
    closeHandleQuietly running.runningOutput
    cancel running.runningDrain
    void (waitCatch running.runningDrain)
    void (tryAny (waitForProcess running.runningProcess))

interruptRunningClaude :: RunningClaude -> IO ()
interruptRunningClaude running =
    tryAny
        (ByteString.hPut running.runningInput "\ETX" >> hFlush running.runningInput)
        >>= \_ -> pure ()

abortClaudeCodeTurn :: ClaudeCodeTurn -> IO ()
abortClaudeCodeTurn turn = do
    interruptRunningClaude turn.turnRunning
    stopRunningClaude turn.turnRunning

drainPty
    :: Handle
    -> IORef ByteString.ByteString
    -> IORef Int
    -> IO ()
drainPty handle diagnosticRef readyPromptsRef =
    go ByteString.empty
  where
    go scanTail = do
        chunk <- ByteString.hGetSome handle 8192
        if ByteString.null chunk
            then pure ()
            else do
                atomicModifyIORef' diagnosticRef \bytes ->
                    (appendDiagnostic bytes chunk, ())
                let scanBytes = scanTail <> chunk
                    promptCount =
                        countOccurrences readyPromptMarker scanBytes
                if promptCount == 0
                    then pure ()
                    else atomicModifyIORef' readyPromptsRef
                        \count -> (count + promptCount, ())
                go (readinessScanTail scanBytes)

-- Screen-reader mode renders an idle interactive prompt as @$@ followed by a
-- cursor-to-column-two escape. Matching the complete sequence avoids treating
-- ordinary tool or model output whose line starts with @$@ as readiness.
-- Waiting for that prompt prevents startup input from being discarded while
-- Claude initializes its terminal UI. Counting prompts also synchronizes
-- immediately consecutive turns without relying on arbitrary sleeps.
awaitReadyPrompt :: RunningClaude -> IO ()
awaitReadyPrompt running =
    go readyPromptPollLimit
  where
    go remaining = do
        ready <- readIORef running.runningReadyPrompts
        claimed <- atomicModifyIORef' running.runningConsumedPrompts
            \consumed ->
                if consumed < ready
                    then (consumed + 1, True)
                    else (consumed, False)
        if claimed
            then pure ()
            else do
                processExit <- getProcessExitCode running.runningProcess
                case processExit of
                    Just exitCode ->
                        failReady
                            ( "Claude Code exited before its interactive prompt was ready ("
                                <> Text.pack (show exitCode)
                                <> ")."
                            )
                    Nothing
                        | remaining <= 0 ->
                            failReady
                                "Timed out waiting for Claude Code's interactive prompt."
                        | otherwise -> do
                            threadDelay readyPromptPollMicros
                            go (remaining - 1)
    failReady prefix = do
        diagnostic <-
            TextEncoding.decodeUtf8With lenientDecode
                <$> readIORef running.runningDiagnosticBytes
        fail $
            Text.unpack $
                prefix
                    <> if Text.null (Text.strip diagnostic)
                        then ""
                        else "\nTerminal output:\n" <> Text.takeEnd 2_000 diagnostic

readyPromptMarker :: ByteString.ByteString
readyPromptMarker = "\n$\ESC[2G"

readyPromptPollMicros :: Int
readyPromptPollMicros = 50_000

readyPromptPollLimit :: Int
readyPromptPollLimit = 600

countOccurrences
    :: ByteString.ByteString
    -> ByteString.ByteString
    -> Int
countOccurrences needle haystack
    | ByteString.null needle = 0
    | otherwise =
        case ByteString.breakSubstring needle haystack of
            (_, suffix)
                | ByteString.null suffix -> 0
                | otherwise ->
                    1
                        + countOccurrences
                            needle
                            (ByteString.drop (ByteString.length needle) suffix)

readinessScanTail :: ByteString.ByteString -> ByteString.ByteString
readinessScanTail bytes =
    ByteString.drop
        (max 0 (ByteString.length bytes - ByteString.length readyPromptMarker + 1))
        bytes

appendDiagnostic
    :: ByteString.ByteString
    -> ByteString.ByteString
    -> ByteString.ByteString
appendDiagnostic previous chunk =
    let combined = previous <> chunk
        excess = ByteString.length combined - diagnosticByteLimit
    in if excess > 0
        then ByteString.drop excess combined
        else combined

diagnosticByteLimit :: Int
diagnosticByteLimit = 65_536

closeHandleQuietly :: Handle -> IO ()
closeHandleQuietly handle =
    void (tryAny (hClose handle))

connectionError :: Text -> SomeException -> ApiError
connectionError prefix exception =
    ConnectionError
        (prefix <> ": " <> Text.pack (show exception))

startArguments :: StartMode -> [String]
startArguments = \case
    StartNew sessionId ->
        ["--session-id", Text.unpack sessionId]
    StartResume sessionId ->
        ["--resume", Text.unpack sessionId]

startSessionId :: StartMode -> Text
startSessionId = \case
    StartNew sessionId -> sessionId
    StartResume sessionId -> sessionId

isNewStart :: StartMode -> Bool
isNewStart = \case
    StartNew _ -> True
    StartResume _ -> False

permissionArguments :: ClaudeCodePermission -> [String]
permissionArguments = \case
    ClaudeCodeDontAsk ->
        ["--permission-mode", "dontAsk"]
    ClaudeCodeBypass ->
        [ "--dangerously-skip-permissions"
        , "--permission-mode"
        , "bypassPermissions"
        ]

toolArguments :: ClaudeCodeToolMode -> [String]
toolArguments = \case
    ClaudeCodeDefaultTools ->
        -- Claude's interactive AskUserQuestion card is hidden behind this
        -- bridge's drained PTY. Force clarification to happen as ordinary
        -- assistant text until the harness has an explicit relay.
        ["--disallowedTools", "AskUserQuestion"]
    ClaudeCodeNoTools ->
        [ "--tools"
        , ""
        , "--disallowedTools"
        , "AskUserQuestion"
        ]

optionalArgument :: String -> Maybe Text -> [String]
optionalArgument name = \case
    Just value -> [name, Text.unpack value]
    Nothing -> []

optionalEffortArgument :: Maybe Text -> [String]
optionalEffortArgument = \case
    Just value
        | Text.toLower value /= "none" ->
            ["--effort", Text.unpack value]
    _ -> []

nonEmptyText :: Maybe Text -> Maybe Text
nonEmptyText =
    (>>= \value ->
        let stripped = Text.strip value
        in if Text.null stripped then Nothing else Just stripped)

validSessionId :: Text -> Maybe Text
validSessionId value =
    UUID.toText <$> UUID.fromText (Text.strip value)

newSessionId :: IO Text
newSessionId = do
    randomBytes <- getEntropy 16
    let bytes = ByteString.unpack randomBytes
        versioned = replaceAt 6 setVersion (replaceAt 8 setVariant bytes)
        setVersion byte = (byte .&. 0x0f) .|. 0x40
        setVariant byte = (byte .&. 0x3f) .|. 0x80
    case UUID.fromByteString (LazyByteString.fromStrict (ByteString.pack versioned)) of
        Just uuid -> pure (UUID.toText uuid)
        Nothing -> fail "Failed to construct a random Claude session UUID"

replaceAt :: Int -> (a -> a) -> [a] -> [a]
replaceAt index update values =
    case splitAt index values of
        (before, value : after) -> before <> (update value : after)
        _ -> values

transcriptPathFor :: FilePath -> Text -> IO FilePath
transcriptPathFor workingDirectory sessionId = do
    configDirectory <- claudeConfigDirectory workingDirectory
    let projectsDirectory = configDirectory </> "projects"
        expected =
            projectsDirectory
                </> cwdSlug workingDirectory
                </> Text.unpack sessionId <> ".jsonl"
    expectedExists <- doesFileExist expected
    if expectedExists
        then pure expected
        else do
            matches <- findTranscript projectsDirectory (Text.unpack sessionId <> ".jsonl")
            pure $ case matches of
                first : _ -> first
                [] -> expected

claudeConfigDirectory :: FilePath -> IO FilePath
claudeConfigDirectory workingDirectory = do
    configured <- lookupEnv "CLAUDE_CONFIG_DIR"
    case configured of
        Just path
            | not (null path) -> do
                if isRelative path
                    then canonicalizePath (workingDirectory </> path)
                    else pure path
        _ -> (</> ".claude") <$> getHomeDirectory

cwdSlug :: FilePath -> FilePath
cwdSlug =
    map \character ->
        if isAscii character && isAlphaNum character
            then character
            else '-'

findTranscript :: FilePath -> FilePath -> IO [FilePath]
findTranscript root target = do
    rootExists <- doesDirectoryExist root
    if not rootExists
        then pure []
        else go root
  where
    go directory = do
        entriesResult <- tryAny (listDirectory directory)
        case entriesResult of
            Left _ -> pure []
            Right entries -> do
                let paths = map (directory </>) entries
                    matching = filter ((== target) . takeFileName) paths
                files <- filterM doesFileExist matching
                directories <- filterM doesDirectoryExist paths
                nested <- concat <$> mapM go directories
                pure (files <> nested)

transcriptSize :: FilePath -> IO Integer
transcriptSize path = do
    exists <- doesFileExist path
    if exists
        then getFileSize path
        else pure 0

prepareInteractiveEnvironment :: [(String, String)] -> [(String, String)]
prepareInteractiveEnvironment environment =
    setEnvironmentVariable
        "CLAUDE_CODE_DISABLE_TERMINAL_TITLE"
        "1"
        (ensureEnvironmentVariable "TERM" "xterm-256color" environment)

ensureEnvironmentVariable
    :: String
    -> String
    -> [(String, String)]
    -> [(String, String)]
ensureEnvironmentVariable name value environment
    | any ((== name) . fst) environment = environment
    | otherwise = (name, value) : environment

setEnvironmentVariable
    :: String
    -> String
    -> [(String, String)]
    -> [(String, String)]
setEnvironmentVariable name value environment =
    (name, value) : filter ((/= name) . fst) environment
