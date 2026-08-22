-- | Lifecycle management for a subscription-authenticated Claude Code
-- process using the same structured JSONL transport as the Agent SDK.
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
    , readClaudeCodeOutputLine
    , resolveClaudeCodeTurnUsage
    , claudeCodeTurnSessionId
    , claudeCodeTurnIsNewSession
    , claudeCodeTurnProcessExit
    , claudeCodeTurnDiagnostic
    ) where

import Agent.ClaudeCode.Internal.Environment
    ( getSanitizedClaudeEnvironment )
import Agent.Error (ApiError(..))
import Agent.Loop
    ( TokenUsage(..)
    , addTokenUsage
    , emptyTokenUsage
    )
import Agent.Tools.IO (terminateProcessGroup)
import Control.Applicative ((<|>))
import Control.Concurrent.Async
    ( Async
    , asyncWithUnmask
    , cancel
    , waitCatch
    )
import Control.Concurrent.MVar
    ( MVar
    , newMVar
    , withMVar
    )
import Control.Exception (IOException)
import Control.Exception.Safe
    ( SomeException
    , bracket
    , catchAny
    , mask
    , onException
    , throwIO
    , try
    , tryAny
    )
import Control.Monad (unless, void)
import qualified Data.Aeson as Aeson
import Data.Bits ((.&.), (.|.))
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString8
import qualified Data.ByteString.Lazy as LazyByteString
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
import System.Directory (canonicalizePath)
import System.Entropy (getEntropy)
import System.Exit (ExitCode)
import System.IO
    ( BufferMode(NoBuffering)
    , Handle
    , hClose
    , hFlush
    , hSetBinaryMode
    , hSetBuffering
    )
import System.IO.Error (isEOFError)
import System.Posix.Types (ProcessGroupID)
import System.Process
    ( CreateProcess(..)
    , ProcessHandle
    , StdStream(CreatePipe)
    , createProcess
    , getPid
    , getProcessExitCode
    , proc
    , waitForProcess
    )
import System.Timeout (timeout)

-- | Claude's non-interactive permission policies. Neither policy can pause
-- the hidden subprocess to ask the terminal user for confirmation.
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
    , promptWriteTimeoutMicros :: !Int
    } deriving (Eq, Show)

defaultClaudeCodeOptions :: FilePath -> FilePath -> ClaudeCodeOptions
defaultClaudeCodeOptions executable cwd = ClaudeCodeOptions
    { executable
    , cwd
    , permission = ClaudeCodeDontAsk
    , safeMode = True
    , promptWriteTimeoutMicros = 60 * 1_000_000
    }

-- | A serialized owner for at most one structured Claude process.
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
    -- A failed or cancelled turn may already have persisted uncommitted
    -- context in Claude's session. Force the next turn onto a fresh UUID so
    -- the host's committed history remains authoritative.
    , stateNeedsFreshSession :: !Bool
    }

data RunningClaude = RunningClaude
    { runningSessionId :: !Text
    , runningModel :: !(Maybe Text)
    , runningEffort :: !(Maybe Text)
    , runningInput :: !Handle
    , runningOutput :: !Handle
    , runningError :: !Handle
    , runningProcess :: !ProcessHandle
    , runningGroupId :: !(Maybe ProcessGroupID)
    , runningDiagnosticBytes :: !(IORef ByteString.ByteString)
    , runningUsageAccounting :: !(IORef UsageAccounting)
    , runningPromptWriteTimeoutMicros :: !Int
    , runningErrorDrain :: !(Async ())
    }

data UsageAccounting = UsageAccounting
    { usageCumulativeBaseline :: !(Maybe TokenUsage)
    , usagePendingFallback :: !TokenUsage
    }

data UsageComponents = UsageComponents
    { usageUncachedInput :: !Int
    , usageCachedInput :: !Int
    , usageOutput :: !Int
    }

-- | Immutable view of the process selected for one submitted turn. The owning
-- 'ClaudeCodeSession' keeps the structured process alive across turns.
data ClaudeCodeTurn = ClaudeCodeTurn
    { turnRunning :: !RunningClaude
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

-- | Run one structured-stream action against the appropriate persistent
-- Claude process. The host-state check must confirm that the previously
-- committed turn is still present. A failed check invalidates the current
-- continuation before process selection, preventing rolled-back host history
-- from reusing hidden Claude state. A successful callback returns a commit
-- action; that action and the Claude completion marker run together while
-- asynchronous exceptions are masked.
--
-- A valid UUID in the host's previous-response field resumes that Claude
-- session. Model or effort changes restart the process while preserving the
-- UUID. Passing no previous response after a completed turn is treated as an
-- explicit new conversation and receives a fresh UUID.
withClaudeCodeTurn
    :: ClaudeCodeSession
    -> IO Bool
    -> Maybe Text
    -> Maybe Text
    -> Maybe Text
    -> (ClaudeCodeTurn -> IO (Either ApiError (a, IO ())))
    -> IO (Either ApiError a)
withClaudeCodeTurn
    session
    hostTranscriptMatches
    previous
    model
    effort
    callback =
    withMVar session.sessionTurnLock \_ ->
        mask \restore -> do
            selectedPrevious <- selectPrevious session previous
            prepared <- tryAny $ do
                matches <- hostTranscriptMatches
                unless matches (invalidateClaudeCodeContinuation session)
                prepareTurn session
                    selectedPrevious
                    (nonEmptyText model)
                    (nonEmptyText effort)
            case prepared of
                Left exception ->
                    pure (Left (connectionError "Failed to start Claude Code" exception))
                Right turn -> do
                    result <- tryAny $
                        restore (callback turn)
                            `onException`
                                abortAndInvalidateClaudeCodeTurn session turn
                    case result of
                        Left exception ->
                            pure (Left (connectionError "Claude Code turn failed" exception))
                        Right (Left err) -> do
                            abortAndInvalidateClaudeCodeTurn session turn
                            pure (Left err)
                        Right (Right (value, commit)) -> do
                            committed <- tryAny commit
                            case committed of
                                Left exception -> do
                                    abortAndInvalidateClaudeCodeTurn
                                        session
                                        turn
                                    pure $
                                        Left
                                            (connectionError
                                                "Failed to commit Claude Code turn"
                                                exception)
                                Right () -> do
                                    markTurnCompleted session turn
                                    pure (Right value)

-- | Submit one SDK-compatible user message to the persistent process. The
-- write is bounded so a child that stops reading stdin cannot hang forever.
sendClaudeCodePrompt :: ClaudeCodeTurn -> Text -> IO Bool
sendClaudeCodePrompt turn prompt = do
    result <-
        timeout
            (max 1 turn.turnRunning.runningPromptWriteTimeoutMicros)
            do
                LazyByteString.hPut
                    turn.turnRunning.runningInput
                    ( Aeson.encode
                        (Aeson.object
                            [ "type" Aeson..= ("user" :: Text)
                            , "message" Aeson..= Aeson.object
                                [ "role" Aeson..= ("user" :: Text)
                                , "content" Aeson..=
                                    [ Aeson.object
                                        [ "type" Aeson..= ("text" :: Text)
                                        , "text" Aeson..= prompt
                                        ]
                                    ]
                                ]
                            , "parent_tool_use_id" Aeson..= Aeson.Null
                            ]
                        )
                        <> "\n"
                    )
                hFlush turn.turnRunning.runningInput
    pure (isJust result)

-- | Read the next complete JSONL record from Claude's stdout. 'Nothing'
-- denotes EOF; other I/O errors are rethrown for the turn wrapper to report.
readClaudeCodeOutputLine
    :: ClaudeCodeTurn
    -> IO (Maybe ByteString.ByteString)
readClaudeCodeOutputLine turn = do
    result <-
        try (ByteString8.hGetLine turn.turnRunning.runningOutput)
            :: IO (Either IOException ByteString.ByteString)
    case result of
        Right line -> pure (Just line)
        Left exception
            | isEOFError exception -> pure Nothing
            | otherwise -> throwIO exception

-- | Convert Claude's process-cumulative @modelUsage@ snapshot to the per-turn
-- delta expected by 'Agent.Loop'. If Claude omits or malforms that snapshot,
-- remember the emitted per-result fallback as debt. Later cumulative deltas
-- are reduced by that debt so the same usage is not reported twice.
resolveClaudeCodeTurnUsage
    :: ClaudeCodeTurn
    -> TokenUsage
    -> Maybe TokenUsage
    -> IO TokenUsage
resolveClaudeCodeTurnUsage turn fallback cumulative =
    case cumulative of
        Nothing ->
            atomicModifyIORef'
                turn.turnRunning.runningUsageAccounting
                \accounting ->
                    ( accounting
                        { usagePendingFallback =
                            addTokenUsage
                                accounting.usagePendingFallback
                                fallback
                        }
                    , fallback
                    )
        Just current ->
            atomicModifyIORef'
                turn.turnRunning.runningUsageAccounting
                \accounting ->
                    let (reported, pending) =
                            reconcileCumulativeUsage accounting current
                    in
                        ( UsageAccounting
                            { usageCumulativeBaseline = Just current
                            , usagePendingFallback = pending
                            }
                        , reported
                        )

claudeCodeTurnSessionId :: ClaudeCodeTurn -> Text
claudeCodeTurnSessionId turn =
    turn.turnRunning.runningSessionId

claudeCodeTurnIsNewSession :: ClaudeCodeTurn -> Bool
claudeCodeTurnIsNewSession = (.turnIsNewSession)

claudeCodeTurnProcessExit :: ClaudeCodeTurn -> IO (Maybe ExitCode)
claudeCodeTurnProcessExit turn =
    getProcessExitCode turn.turnRunning.runningProcess

-- | A bounded tail of Claude's stderr, intended only for connection-error
-- diagnostics. User-visible model output comes from stdout stream-json.
claudeCodeTurnDiagnostic :: ClaudeCodeTurn -> IO Text
claudeCodeTurnDiagnostic turn =
    TextEncoding.decodeUtf8With lenientDecode
        <$> readIORef turn.turnRunning.runningDiagnosticBytes

emptySessionState :: SessionState
emptySessionState = SessionState
    { stateRunning = Nothing
    , stateHadCompletedTurn = False
    , stateNeedsFreshSession = False
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
            -- Clear ownership before launch. If launch fails there must be no
            -- stale handle for outer cleanup to touch. Preserve dirty-session
            -- invalidation so a failed replacement launch cannot resume it.
            writeIORef session.sessionState
                emptySessionState
                    { stateNeedsFreshSession =
                        oldState.stateNeedsFreshSession
                    }
            running <- startRunningClaude
                session.sessionOptions
                session.sessionToolMode
                model
                effort
                mode
            let state = SessionState
                    { stateRunning = Just running
                    , stateHadCompletedTurn = completed
                    , stateNeedsFreshSession = False
                    }
            writeIORef session.sessionState state
            pure (running, state, isNewStart mode)
    -- Keep the state write explicit in the reuse branch as documentation that
    -- the process remains owned for the complete callback lifetime.
    writeIORef session.sessionState newState
    pure ClaudeCodeTurn
        { turnRunning = running
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
    if state.stateNeedsFreshSession
        then freshStart
        else
            case state.stateRunning of
                Nothing ->
                    case previous >>= validSessionId of
                        Just sessionId ->
                            pure (Restart (StartResume sessionId) True)
                        Nothing ->
                            freshStart
                Just running
                    | Just rawPrevious <- previous
                    , Nothing <- validSessionId rawPrevious ->
                        -- A response ID from another provider cannot identify a
                        -- Claude session. Start clean; LoopBackend can import
                        -- host history.
                        freshStart
                    | Just sessionId <- previous >>= validSessionId
                    , sessionId /= running.runningSessionId ->
                        pure (Restart (StartResume sessionId) True)
                    | previous == Nothing && state.stateHadCompletedTurn ->
                        freshStart
                    | processExited ->
                        if state.stateHadCompletedTurn
                            then pure
                                (Restart
                                    (StartResume running.runningSessionId)
                                    True)
                            else freshStart
                    | modeChanged running model effort ->
                        pure $
                            Restart
                                (if state.stateHadCompletedTurn
                                    then
                                        StartResume running.runningSessionId
                                    else
                                        StartNew running.runningSessionId)
                                state.stateHadCompletedTurn
                    | otherwise ->
                        pure (Reuse running)
  where
    freshStart = do
        sessionId <- newSessionId
        pure (Restart (StartNew sessionId) False)

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

invalidateClaudeCodeContinuation :: ClaudeCodeSession -> IO ()
invalidateClaudeCodeContinuation session =
    atomicModifyIORef' session.sessionState \state ->
        ( state
            { stateHadCompletedTurn = False
            , stateNeedsFreshSession = True
            }
        , ()
        )

waitForProcessQuietly :: ProcessHandle -> IO ()
waitForProcessQuietly processHandle =
    void $
        timeout processWaitTimeoutMicros
            (tryAny (waitForProcess processHandle))

processWaitTimeoutMicros :: Int
processWaitTimeoutMicros = 2 * 1_000_000

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
        environment <- prepareStructuredEnvironment
            <$> getSanitizedClaudeEnvironment
        let arguments =
                [ "-p"
                , "--input-format"
                , "stream-json"
                , "--output-format"
                , "stream-json"
                , "--verbose"
                ]
                    <> startArguments mode
                    <> permissionArguments options.permission
                    <> toolArguments toolMode
                    <> safeModeArguments options.safeMode
                    <> optionalArgument "--model" model
                    <> optionalEffortArgument effort
            processSpec = (proc options.executable arguments)
                { cwd = Just workingDirectory
                , env = Just environment
                , std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = CreatePipe
                , close_fds = True
                , create_group = True
                , new_session = True
                }
        created <- createProcess processSpec
        let (maybeInput, maybeOutput, maybeError, processHandle) = created
            closeCreatedHandles =
                mapM_ closeHandleQuietly
                    [ handle
                    | Just handle <-
                        [maybeInput, maybeOutput, maybeError]
                    ]
            stopCreated groupId = do
                terminateProcessGroup groupId processHandle
                closeCreatedHandles
                waitForProcessQuietly processHandle
        groupId <- getPid processHandle
            `onException` stopCreated Nothing
        (inputHandle, outputHandle, errorHandle) <-
            case (maybeInput, maybeOutput, maybeError) of
                (Just inputHandle, Just outputHandle, Just errorHandle) ->
                    pure (inputHandle, outputHandle, errorHandle)
                _ -> do
                    stopCreated groupId
                    fail "Claude Code did not provide all requested stdio pipes"
        (do
            mapM_ (`hSetBinaryMode` True)
                [inputHandle, outputHandle, errorHandle]
            mapM_ (`hSetBuffering` NoBuffering)
                [inputHandle, outputHandle, errorHandle])
            `onException` stopCreated groupId
        diagnosticRef <- newIORef ByteString.empty
        usageAccountingRef <- newIORef UsageAccounting
            { usageCumulativeBaseline = Nothing
            , usagePendingFallback = emptyTokenUsage
            }
        errorDrain <-
            (asyncWithUnmask \unmask ->
                unmask (drainDiagnostic errorHandle diagnosticRef)
                    `catchAny` \_ -> pure ())
            `onException` stopCreated groupId
        let running = RunningClaude
                { runningSessionId = startSessionId mode
                , runningModel = model
                , runningEffort = effort
                , runningInput = inputHandle
                , runningOutput = outputHandle
                , runningError = errorHandle
                , runningProcess = processHandle
                , runningGroupId = groupId
                , runningDiagnosticBytes = diagnosticRef
                , runningUsageAccounting = usageAccountingRef
                , runningPromptWriteTimeoutMicros =
                    options.promptWriteTimeoutMicros
                , runningErrorDrain = errorDrain
                }
        restore (pure running) `onException` stopRunningClaude running

stopRunningClaude :: RunningClaude -> IO ()
stopRunningClaude running = do
    terminateProcessGroup running.runningGroupId running.runningProcess
    closeHandleQuietly running.runningInput
    closeHandleQuietly running.runningOutput
    closeHandleQuietly running.runningError
    cancel running.runningErrorDrain
    void (waitCatch running.runningErrorDrain)
    waitForProcessQuietly running.runningProcess

abortClaudeCodeTurn :: ClaudeCodeTurn -> IO ()
abortClaudeCodeTurn turn =
    stopRunningClaude turn.turnRunning

abortAndInvalidateClaudeCodeTurn
    :: ClaudeCodeSession
    -> ClaudeCodeTurn
    -> IO ()
abortAndInvalidateClaudeCodeTurn session turn = do
    atomicModifyIORef' session.sessionState \state ->
        let ownsTurn =
                case state.stateRunning of
                    Just running ->
                        running.runningSessionId
                            == turn.turnRunning.runningSessionId
                    Nothing -> False
        in
            ( if ownsTurn
                then state
                    { stateHadCompletedTurn = False
                    , stateNeedsFreshSession = True
                    }
                else state
            , ()
            )
    abortClaudeCodeTurn turn
    atomicModifyIORef' session.sessionState \state ->
        let ownsTurn =
                case state.stateRunning of
                    Just running ->
                        running.runningSessionId
                            == turn.turnRunning.runningSessionId
                    Nothing -> False
        in
            ( if ownsTurn
                then state { stateRunning = Nothing }
                else state
            , ()
            )

drainDiagnostic
    :: Handle
    -> IORef ByteString.ByteString
    -> IO ()
drainDiagnostic handle diagnosticRef =
    go
  where
    go = do
        chunk <- ByteString.hGetSome handle 8192
        if ByteString.null chunk
            then pure ()
            else do
                atomicModifyIORef' diagnosticRef \bytes ->
                    (appendDiagnostic bytes chunk, ())
                go

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
        [ "--allow-dangerously-skip-permissions"
        , "--permission-mode"
        , "bypassPermissions"
        ]

toolArguments :: ClaudeCodeToolMode -> [String]
toolArguments = \case
    ClaudeCodeDefaultTools ->
        -- AskUserQuestion has no host UI relay in this backend. Force
        -- clarification to happen as ordinary assistant text.
        ["--disallowedTools", "AskUserQuestion"]
    ClaudeCodeNoTools ->
        [ "--tools"
        , ""
        , "--disallowedTools"
        , "AskUserQuestion"
        ]

safeModeArguments :: Bool -> [String]
safeModeArguments enabled =
    [ "--setting-sources"
    , ""
    , "--strict-mcp-config"
    , "--mcp-config"
    , "{\"mcpServers\":{}}"
    , "--no-chrome"
    ]
        <> if enabled
            then
                [ "--safe-mode"
                , "--disable-slash-commands"
                ]
            else
                []

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

usageDelta :: TokenUsage -> TokenUsage -> TokenUsage
usageDelta previous current
    | usageIsMonotonic previous current =
        fromUsageComponents $
            subtractUsageComponents
                (toUsageComponents previous)
                (toUsageComponents current)
    | otherwise =
        -- Claude resets the cumulative ledger on a new process epoch (for
        -- example after resume or an internal conversation reset). Do not mix
        -- components from different epochs.
        current

reconcileCumulativeUsage
    :: UsageAccounting
    -> TokenUsage
    -> (TokenUsage, TokenUsage)
reconcileCumulativeUsage accounting current =
    case accounting.usageCumulativeBaseline of
        Just previous
            | not (usageIsMonotonic previous current) ->
                (current, emptyTokenUsage)
        previous ->
            subtractReportedFallback
                accounting.usagePendingFallback
                (maybe current (`usageDelta` current) previous)

usageIsMonotonic :: TokenUsage -> TokenUsage -> Bool
usageIsMonotonic previous current =
    let previousComponents = toUsageComponents previous
        currentComponents = toUsageComponents current
    in
        currentComponents.usageUncachedInput
            >= previousComponents.usageUncachedInput
            && currentComponents.usageCachedInput
                >= previousComponents.usageCachedInput
            && currentComponents.usageOutput
                >= previousComponents.usageOutput

subtractReportedFallback
    :: TokenUsage
    -> TokenUsage
    -> (TokenUsage, TokenUsage)
subtractReportedFallback pending gross =
    ( fromUsageComponents $
        subtractUsageComponents
            (toUsageComponents pending)
            (toUsageComponents gross)
    , fromUsageComponents $
        subtractUsageComponents
            (toUsageComponents gross)
            (toUsageComponents pending)
    )

toUsageComponents :: TokenUsage -> UsageComponents
toUsageComponents usage =
    let cached = max 0 (min usage.inputTokens usage.cachedTokens)
    in UsageComponents
        { usageUncachedInput = max 0 (usage.inputTokens - cached)
        , usageCachedInput = cached
        , usageOutput = max 0 usage.outputTokens
        }

fromUsageComponents :: UsageComponents -> TokenUsage
fromUsageComponents components =
    TokenUsage
        { inputTokens =
            components.usageUncachedInput
                + components.usageCachedInput
        , outputTokens = components.usageOutput
        , cachedTokens = components.usageCachedInput
        }

subtractUsageComponents
    :: UsageComponents
    -> UsageComponents
    -> UsageComponents
subtractUsageComponents subtrahend minuend =
    UsageComponents
        { usageUncachedInput =
            max 0
                ( minuend.usageUncachedInput
                    - subtrahend.usageUncachedInput
                )
        , usageCachedInput =
            max 0
                ( minuend.usageCachedInput
                    - subtrahend.usageCachedInput
                )
        , usageOutput =
            max 0 (minuend.usageOutput - subtrahend.usageOutput)
        }

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

prepareStructuredEnvironment :: [(String, String)] -> [(String, String)]
prepareStructuredEnvironment environment =
    setEnvironmentVariable
        "CLAUDE_CODE_ENTRYPOINT"
        "sdk-cli"
        (setEnvironmentVariable
            "CLAUDE_AGENT_SDK_CLIENT_APP"
            "haskell-agent"
            (setEnvironmentVariable
                "ENABLE_CLAUDEAI_MCP_SERVERS"
                "0"
                environment))

setEnvironmentVariable
    :: String
    -> String
    -> [(String, String)]
    -> [(String, String)]
setEnvironmentVariable name value environment =
    (name, value) : filter ((/= name) . fst) environment
