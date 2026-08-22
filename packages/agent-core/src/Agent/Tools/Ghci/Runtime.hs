-- | Persistent GHCi session shared by coding-tool providers.
--
-- Replies are framed with unique markers written to both stdout and stderr.
-- Waiting for both markers prevents diagnostics on stderr from arriving after
-- a stdout-only completion marker.
module Agent.Tools.Ghci.Runtime
    ( GhciSession
    , GhciOutcome(..)
    , GhciResult(..)
    , newGhciSession
    , closeGhciSession
    , evalGhci
    , classifyGhci
    ) where

import Agent.Cancel (CancelFlag, isCancelled, waitCancel)
import Agent.OsPath (unsafeToFilePath)
import Agent.Tools.Ghci.Classify
    ( GhciClass(..)
    , classifyGhciInput
    , defaultGhciExtensions
    , typeLooksEffectful
    )
import Agent.Tools.IO (terminateProcessGroup)
import Agent.Tools.Types (ToolEnv(..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
    ( Async
    , asyncWithUnmask
    , cancel
    , race
    , waitCatch
    )
import Control.Concurrent.MVar
import Control.Concurrent.STM
    ( STM
    , TBQueue
    , TMVar
    , atomically
    , isFullTBQueue
    , newEmptyTMVarIO
    , newTBQueueIO
    , orElse
    , readTBQueue
    , readTMVar
    , retry
    , tryReadTBQueue
    , tryPutTMVar
    , writeTBQueue
    )
import Control.Exception.Safe
    ( SomeException
    , finally
    , mask
    , onException
    , try
    )
import Control.Monad (void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.IO
    ( BufferMode(..)
    , Handle
    , hClose
    , hFlush
    , hSetBinaryMode
    , hSetBuffering
    )
import System.Process
    ( CreateProcess(..)
    , ProcessHandle
    , StdStream(..)
    , createProcess
    , getPid
    , getProcessExitCode
    , interruptProcessGroupOf
    , proc
    )
import System.Posix.Signals (sigINT, signalProcessGroup)
import System.Posix.Types (ProcessGroupID)

data GhciOutcome
    = GhciCompleted
    | GhciTimedOut
    | GhciCancelled
    | GhciProcessFailed
    deriving (Eq, Show)

data GhciResult = GhciResult
    { ghciOutcome :: !GhciOutcome
    , ghciOk :: !Bool
    , ghciTimedOut :: !Bool
    , ghciCancelled :: !Bool
    , ghciClass :: !GhciClass
    , ghciOutput :: !Text
    , ghciStdout :: !Text
    , ghciStderr :: !Text
    , ghciTruncated :: !Bool
    , ghciRestarted :: !Bool
    } deriving (Eq, Show)

data GhciStream
    = GhciStdout
    | GhciStderr
    deriving (Eq, Show)

data GhciEvent
    = GhciChunk !GhciStream !ByteString
    | GhciStreamClosed !GhciStream

data GhciProcess = GhciProcess
    { ghciStdin :: !Handle
    , ghciStdoutHandle :: !Handle
    , ghciStderrHandle :: !Handle
    , ghciHandle :: !ProcessHandle
    , ghciGroupId :: !(Maybe ProcessGroupID)
    , ghciEvents :: !(TBQueue GhciEvent)
    , ghciStdoutClosed :: !(TMVar ())
    , ghciStderrClosed :: !(TMVar ())
    , ghciStdoutDrain :: !(Async ())
    , ghciStderrDrain :: !(Async ())
    }

data GhciSessionState
    = GhciNotStarted
    | GhciRunning !GhciProcess
    | GhciTainted !GhciProcess
    | GhciClosed

data GhciSession = GhciSession
    { ghciEnv :: !ToolEnv
    , ghciLock :: !(MVar ())
    , ghciState :: !(IORef GhciSessionState)
    , ghciNextMarker :: !(IORef Int)
    , ghciMarkerSeed :: !Text
    , ghciClassificationCache :: !(IORef (Maybe (Text, GhciClass)))
    }

newGhciSession :: ToolEnv -> IO GhciSession
newGhciSession env = do
    lock <- newMVar ()
    state <- newIORef GhciNotStarted
    nextMarkerRef <- newIORef 0
    seed <- Text.pack . show <$> getMonotonicTimeNSec
    classificationCache <- newIORef Nothing
    pure GhciSession
        { ghciEnv = env
        , ghciLock = lock
        , ghciState = state
        , ghciNextMarker = nextMarkerRef
        , ghciMarkerSeed = seed
        , ghciClassificationCache = classificationCache
        }

closeGhciSession :: GhciSession -> IO ()
closeGhciSession session =
    withGhciLock session do
        writeIORef session.ghciClassificationCache Nothing
        readIORef session.ghciState >>= \case
            GhciRunning process -> closeProcess process
            GhciTainted process -> closeProcess process
            GhciNotStarted ->
                writeIORef session.ghciState GhciClosed
            GhciClosed -> pure ()
  where
    closeProcess process = do
        -- Retain ownership if shutdown is interrupted so a later close can
        -- retry instead of forgetting a live process.
        writeIORef session.ghciState (GhciTainted process)
        shutdownProcess process
        writeIORef session.ghciState GhciClosed

-- | Evaluate @expression@ in the persistent GHCi, classifying side effects first.
-- The timeout is an overall budget for classification and execution.
evalGhci :: GhciSession -> Text -> Int -> IO GhciResult
evalGhci session expression requestedTimeout =
    withGhciLock session do
        let timeoutMs = normalizeTimeout requestedTimeout
        started <- getMonotonicTimeNSec
        cached <- readIORef session.ghciClassificationCache
        classification <- case cached of
            Just (cachedExpression, cls)
                | cachedExpression == expression -> pure cls
            _ -> classifyGhciLocked session expression (min 15000 timeoutMs)
        writeIORef session.ghciClassificationCache Nothing
        remaining <- remainingMillis started timeoutMs
        if remaining <= 0
            then pure $ emptyResult GhciTimedOut classification
                "Timed out while classifying the GHCi input."
            else do
                result <- evalRawGhci session expression remaining
                pure result { ghciClass = classification }

-- | Classify without evaluating. Fail closed on ambiguity.
classifyGhci :: GhciSession -> Text -> IO GhciClass
classifyGhci session expression =
    withGhciLock session do
        classification <- classifyGhciLocked session expression 15000
        writeIORef session.ghciClassificationCache
            (Just (expression, classification))
        pure classification

withGhciLock :: GhciSession -> IO a -> IO a
withGhciLock session action =
    withMVar session.ghciLock (const action)

classifyGhciLocked :: GhciSession -> Text -> Int -> IO GhciClass
classifyGhciLocked session expression timeoutMs =
    case classifyGhciInput expression of
        Just cls -> pure cls
        Nothing -> do
            typeResult <- evalRawGhci session (":type " <> expression) timeoutMs
            if typeResult.ghciOutcome /= GhciCompleted || not typeResult.ghciOk
                then pure GhciEffectful
                else
                    if typeLooksEffectful typeResult.ghciOutput
                        then pure GhciEffectful
                        else pure GhciPure

evalRawGhci :: GhciSession -> Text -> Int -> IO GhciResult
evalRawGhci session expression requestedTimeout =
    run `onException` taintCurrentProcess session
  where
    run = do
        let timeoutMs = normalizeTimeout requestedTimeout
        cancelled <- isCancelled session.ghciEnv.toolCancel
        if cancelled
            then pure $ emptyResult GhciCancelled GhciEffectful
                "GHCi evaluation cancelled."
            else prepareProcess session >>= \case
                Left err ->
                    pure $ emptyResult GhciProcessFailed GhciEffectful err
                Right (process, restartedBefore) -> do
                    marker <- nextMarker session
                    sent <- try @_ @SomeException do
                        sendGhciInput process expression
                        sendMarker process marker
                    case sent of
                        Left err -> do
                            restarted <- restartProcess session
                            pure $ (emptyResult GhciProcessFailed GhciEffectful
                                ("Failed to send input to GHCi: "
                                    <> Text.pack (show err)))
                                        { ghciRestarted =
                                            restartedBefore || restarted
                                        }
                        Right () -> do
                            awaited <- awaitMarker
                                (Just session.ghciEnv.toolCancel)
                                session.ghciEnv.toolStdoutCap
                                process
                                marker
                                timeoutMs
                            finishAwaited
                                session process restartedBefore awaited

-- | Never reuse a process after a request exits exceptionally. The exception
-- may have arrived after bytes were written but before marker recovery, so
-- flushing currently queued output is not enough: delayed output would be
-- attributed to the next request. Keep a failed cleanup as 'GhciTainted' so a
-- later call still owns and retries shutdown rather than orphaning the process.
taintCurrentProcess :: GhciSession -> IO ()
taintCurrentProcess session = do
    writeIORef session.ghciClassificationCache Nothing
    process <- atomicModifyIORef' session.ghciState \case
        GhciRunning current ->
            (GhciTainted current, Just current)
        GhciTainted current ->
            (GhciTainted current, Just current)
        GhciNotStarted ->
            (GhciNotStarted, Nothing)
        GhciClosed ->
            (GhciClosed, Nothing)
    case process of
        Nothing -> pure ()
        Just current ->
            try @_ @SomeException (shutdownProcess current) >>= \case
                Left _ ->
                    pure ()
                Right () ->
                    writeIORef session.ghciState GhciNotStarted

prepareProcess :: GhciSession -> IO (Either Text (GhciProcess, Bool))
prepareProcess session = do
    ensured <- ensureProcess session
    case ensured of
        Left err -> pure (Left err)
        Right process -> do
            flushed <- flushEvents process
            if flushed
                then pure (Right (process, False))
                else do
                    restarted <- restartProcess session
                    if not restarted
                        then pure (Left staleOutputError)
                        else do
                            ensuredAgain <- ensureProcess session
                            pure (markRestarted <$> ensuredAgain)
  where
    markRestarted process = (process, True)
    staleOutputError =
        "GHCi produced excessive stale output and could not be restarted."

finishAwaited
    :: GhciSession
    -> GhciProcess
    -> Bool
    -> Awaited
    -> IO GhciResult
finishAwaited session process restartedBefore awaited =
    case awaited.awaitReason of
        AwaitMarkers ->
            pure $ capturedResult
                GhciCompleted awaited.awaitOutput restartedBefore
        AwaitTimeout -> recover GhciTimedOut
        AwaitCancellation -> recover GhciCancelled
        AwaitProcessExit -> do
            restarted <- restartProcess session
            pure $ (capturedResult GhciProcessFailed awaited.awaitOutput
                (restartedBefore || restarted))
                { ghciOk = False }
  where
    recover outcome = do
        interruptGhciProcess process
        recovered <-
            if awaited.awaitOutput.capturedTruncated
                then pure False
                else recoverAfterInterrupt session process
        restarted <- if recovered
            then pure False
            else restartProcess session
        pure $ (capturedResult outcome awaited.awaitOutput
            (restartedBefore || restarted))
            { ghciOk = False }

recoverAfterInterrupt :: GhciSession -> GhciProcess -> IO Bool
recoverAfterInterrupt session process = do
    flushed <- flushEvents process
    if not flushed
        then pure False
        else do
            marker <- nextMarker session
            sent <- try @_ @SomeException do
                sendLine process ":type ()"
                sendMarker process marker
            case sent of
                Left _ -> pure False
                Right () -> do
                    awaited <- awaitMarker Nothing
                        session.ghciEnv.toolStdoutCap process marker 5000
                    pure (awaited.awaitReason == AwaitMarkers)

interruptGhciProcess :: GhciProcess -> IO ()
interruptGhciProcess process =
    case process.ghciGroupId of
        Just groupId ->
            void $ try @_ @SomeException
                (signalProcessGroup sigINT groupId)
        Nothing ->
            void $ try @_ @SomeException
                (interruptProcessGroupOf process.ghciHandle)

capturedResult :: GhciOutcome -> CapturedOutput -> Bool -> GhciResult
capturedResult outcome captured restarted =
    let stdout = Text.strip captured.capturedStdout
        stderr = Text.strip captured.capturedStderr
        output = combineOutput stdout stderr
        ok = outcome == GhciCompleted && not (isGhciError stderr)
    in GhciResult
        { ghciOutcome = outcome
        , ghciOk = ok
        , ghciTimedOut = outcome == GhciTimedOut
        , ghciCancelled = outcome == GhciCancelled
        , ghciClass = GhciPure
        , ghciOutput = output
        , ghciStdout = stdout
        , ghciStderr = stderr
        , ghciTruncated = captured.capturedTruncated
        , ghciRestarted = restarted
        }

emptyResult :: GhciOutcome -> GhciClass -> Text -> GhciResult
emptyResult outcome classification message = GhciResult
    { ghciOutcome = outcome
    , ghciOk = False
    , ghciTimedOut = outcome == GhciTimedOut
    , ghciCancelled = outcome == GhciCancelled
    , ghciClass = classification
    , ghciOutput = message
    , ghciStdout = ""
    , ghciStderr = message
    , ghciTruncated = False
    , ghciRestarted = False
    }

combineOutput :: Text -> Text -> Text
combineOutput stdout stderr =
    Text.intercalate "\n" (filter (not . Text.null) [stdout, stderr])

isGhciError :: Text -> Bool
isGhciError stderr =
    any isErrorLine (Text.lines stderr)
  where
    isErrorLine line =
        let stripped = Text.strip line
        in "*** Exception:" `Text.isPrefixOf` stripped
            || (": error:" `Text.isInfixOf` stripped)

nextMarker :: GhciSession -> IO Text
nextMarker session = do
    n <- atomicModifyIORef' session.ghciNextMarker \i -> (i + 1, i + 1)
    pure $
        "{- AGENT_GHCI_DONE:"
            <> session.ghciMarkerSeed
            <> ":"
            <> Text.pack (show n)
            <> " -}"

ensureProcess :: GhciSession -> IO (Either Text GhciProcess)
ensureProcess session =
    readIORef session.ghciState >>= \case
        GhciClosed -> pure (Left "GHCi session is closed.")
        GhciNotStarted -> startProcess session
        GhciTainted process ->
            restartTaintedProcess session process
        GhciRunning process -> do
            exited <- getProcessExitCode process.ghciHandle
            case exited of
                Nothing -> pure (Right process)
                Just _ -> do
                    writeIORef session.ghciState (GhciTainted process)
                    restartTaintedProcess session process

restartTaintedProcess
    :: GhciSession
    -> GhciProcess
    -> IO (Either Text GhciProcess)
restartTaintedProcess session process = do
    shutdownProcess process
    writeIORef session.ghciState GhciNotStarted
    startProcess session

startProcess :: GhciSession -> IO (Either Text GhciProcess)
startProcess session = mask \restore ->
    spawnProcess session.ghciEnv >>= \case
        Left err -> pure (Left err)
        Right process ->
            restore (initialize process)
                `onException` shutdownProcess process
  where
    initialize process = do
        flushed <- flushEvents process
        if not flushed
            then do
                shutdownProcess process
                pure $ Left
                    "GHCi produced excessive output during startup."
            else do
                marker <- nextMarker session
                sent <- try @_ @SomeException (sendMarker process marker)
                case sent of
                    Left err -> do
                        shutdownProcess process
                        pure $ Left
                            ("Failed to initialize GHCi: "
                                <> Text.pack (show err))
                    Right () -> do
                        awaited <- awaitMarker Nothing
                            session.ghciEnv.toolStdoutCap process marker 5000
                        if awaited.awaitReason == AwaitMarkers
                            && not
                                (isGhciError
                                    awaited.awaitOutput.capturedStderr)
                            then do
                                writeIORef session.ghciState
                                    (GhciRunning process)
                                pure (Right process)
                            else do
                                shutdownProcess process
                                pure $ Left $
                                    "GHCi failed its startup handshake.\n"
                                        <> combineOutput
                                            awaited.awaitOutput.capturedStdout
                                            awaited.awaitOutput.capturedStderr

restartProcess :: GhciSession -> IO Bool
restartProcess session =
    readIORef session.ghciState >>= \case
        GhciClosed -> pure False
        GhciNotStarted -> start
        GhciRunning process -> do
            writeIORef session.ghciState (GhciTainted process)
            restart process
        GhciTainted process ->
            restart process
  where
    start = do
        writeIORef session.ghciClassificationCache Nothing
        either (const False) (const True) <$> startProcess session

    restart process = do
        writeIORef session.ghciClassificationCache Nothing
        shutdownProcess process
        writeIORef session.ghciState GhciNotStarted
        either (const False) (const True) <$> startProcess session

ghciArgs :: [String]
ghciArgs =
    [ "-ignore-dot-ghci"
    , "-v0"
    , "-XGHC2021"
    ]
        ++ map ("-X" <>) defaultGhciExtensions

spawnProcess :: ToolEnv -> IO (Either Text GhciProcess)
spawnProcess env = do
    let spec = (proc "ghci" ghciArgs)
            { cwd = Just (unsafeToFilePath env.toolCwd)
            , std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            , create_group = True
            }
    spawned <- try @_ @SomeException $ mask \restore -> do
        created@(_, _, _, handle) <- createProcess spec
        groupId <- getPid handle
            `onException` cleanupSpawn Nothing created []
        case created of
            (Just hin, Just hout, Just herr, _) -> do
                let cleanup drains =
                        cleanupSpawn groupId created drains
                mapM_ prepareHandle [hin, hout, herr]
                    `onException` cleanup []
                (events, stdoutClosed, stderrClosed) <-
                    (do
                        queue <- newTBQueueIO
                            (fromIntegral ghciEventQueueCapacity)
                        outClosed <- newEmptyTMVarIO
                        errClosed <- newEmptyTMVarIO
                        pure (queue, outClosed, errClosed))
                        `onException` cleanup []
                stdoutDrain <-
                    asyncWithUnmask
                        (\unmask ->
                            unmask (drainHandle
                                GhciStdout hout events stdoutClosed))
                        `onException` cleanup []
                stderrDrain <-
                    asyncWithUnmask
                        (\unmask ->
                            unmask (drainHandle
                                GhciStderr herr events stderrClosed))
                        `onException` cleanup [stdoutDrain]
                let process = GhciProcess
                        { ghciStdin = hin
                        , ghciStdoutHandle = hout
                        , ghciStderrHandle = herr
                        , ghciHandle = handle
                        , ghciGroupId = groupId
                        , ghciEvents = events
                        , ghciStdoutClosed = stdoutClosed
                        , ghciStderrClosed = stderrClosed
                        , ghciStdoutDrain = stdoutDrain
                        , ghciStderrDrain = stderrDrain
                        }
                restore (pure (Just process))
                    `onException` shutdownProcess process
            _ -> do
                cleanupSpawn groupId created []
                pure Nothing
    case spawned of
        Left err ->
            pure $ Left ("Failed to start GHCi: " <> Text.pack (show err))
        Right Nothing -> pure (Left "Failed to create GHCi pipes.")
        Right (Just process) -> pure (Right process)

cleanupSpawn
    :: Maybe ProcessGroupID
    -> (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
    -> [Async ()]
    -> IO ()
cleanupSpawn groupId (hin, hout, herr, handle) drains = do
    terminateProcessGroup groupId handle
    mapM_ (mapM_ (void . try @_ @SomeException . hClose))
        [hin, hout, herr]
    stopDrains drains

prepareHandle :: Handle -> IO ()
prepareHandle handle = do
    hSetBinaryMode handle True
    hSetBuffering handle NoBuffering

shutdownProcess :: GhciProcess -> IO ()
shutdownProcess process = do
    void $ try @_ @SomeException (sendLine process ":quit")
    void $ waitForExit process.ghciHandle 200
    terminateProcessGroup process.ghciGroupId process.ghciHandle
    mapM_ (void . try @_ @SomeException . hClose)
        [ process.ghciStdin
        , process.ghciStdoutHandle
        , process.ghciStderrHandle
        ]
    stopDrains [process.ghciStdoutDrain, process.ghciStderrDrain]

stopDrains :: [Async ()] -> IO ()
stopDrains drains = do
    mapM_ (void . try @_ @SomeException . cancel) drains
    mapM_ (void . waitCatch) drains

waitForExit :: ProcessHandle -> Int -> IO Bool
waitForExit handle timeoutMs = go (max 1 timeoutMs)
  where
    go remaining = do
        getProcessExitCode handle >>= \case
            Just _ -> pure True
            Nothing
                | remaining <= 0 -> pure False
                | otherwise -> do
                    let delayMs = min 10 remaining
                    threadDelay (delayMs * 1000)
                    go (remaining - delayMs)

sendGhciInput :: GhciProcess -> Text -> IO ()
sendGhciInput process expression =
    sendLine process (frameGhciInput expression)

frameGhciInput :: Text -> Text
frameGhciInput expression
    | not ("\n" `Text.isInfixOf` expression) = expression
    | ":" `Text.isPrefixOf` Text.stripStart expression = expression
    | Just binding <- multilineLetBinding expression =
        ":{\n" <> binding <> "\n:}"
    | otherwise = ":{\n" <> expression <> "\n:}"

multilineLetBinding :: Text -> Maybe Text
multilineLetBinding expression = do
    let stripped = Text.stripStart expression
    binding <- Text.stripPrefix "let " stripped
    if "\nin " `Text.isInfixOf` expression
        then Nothing
        else Just binding

sendMarker :: GhciProcess -> Text -> IO ()
sendMarker process marker = do
    let quoted = Text.pack (show (Text.unpack marker))
    -- An explicit qualified Prelude import makes GHCi drop its implicit
    -- unqualified Prelude import. Restore the ordinary interactive context
    -- before installing the private aliases used by the marker action.
    sendLine process "import Prelude"
    sendLine process
        "import qualified Prelude as AgentGhciPreludeInternal"
    sendLine process
        "import qualified System.IO as AgentGhciIOInternal"
    sendLine process $
        "AgentGhciPreludeInternal.putStrLn " <> quoted
            <> " AgentGhciPreludeInternal.>> "
            <> "AgentGhciIOInternal.hPutStrLn "
            <> "AgentGhciIOInternal.stderr "
            <> quoted
            <> " AgentGhciPreludeInternal.>> "
            <> "AgentGhciIOInternal.hFlush AgentGhciIOInternal.stdout "
            <> "AgentGhciPreludeInternal.>> "
            <> "AgentGhciIOInternal.hFlush AgentGhciIOInternal.stderr"

sendLine :: GhciProcess -> Text -> IO ()
sendLine process text = do
    BS.hPut process.ghciStdin (encodeUtf8 (text <> "\n"))
    hFlush process.ghciStdin

-- | Drop one bounded queue snapshot. If the queue was full, a drain thread
-- may already be blocked with more stale output, so the caller must restart.
flushEvents :: GhciProcess -> IO Bool
flushEvents process =
    atomically do
        wasFull <- isFullTBQueue process.ghciEvents
        let go =
                tryReadTBQueue process.ghciEvents >>= \case
                    Nothing -> pure ()
                    Just _ -> go
        go
        pure (not wasFull)

data AwaitReason
    = AwaitMarkers
    | AwaitTimeout
    | AwaitCancellation
    | AwaitProcessExit
    deriving (Eq, Show)

data Awaited = Awaited
    { awaitReason :: !AwaitReason
    , awaitOutput :: !CapturedOutput
    }

data CapturedOutput = CapturedOutput
    { capturedStdout :: !Text
    , capturedStderr :: !Text
    , capturedTruncated :: !Bool
    }

data StreamCapture = StreamCapture
    { streamPending :: !ByteString
    , streamOutput :: !ByteString
    , streamDropped :: !Int
    , streamDone :: !Bool
    , streamClosed :: !Bool
    }

data CaptureState = CaptureState
    { stdoutCapture :: !StreamCapture
    , stderrCapture :: !StreamCapture
    }

emptyStreamCapture :: StreamCapture
emptyStreamCapture = StreamCapture
    { streamPending = BS.empty
    , streamOutput = BS.empty
    , streamDropped = 0
    , streamDone = False
    , streamClosed = False
    }

emptyCaptureState :: CaptureState
emptyCaptureState = CaptureState
    { stdoutCapture = emptyStreamCapture
    , stderrCapture = emptyStreamCapture
    }

awaitMarker
    :: Maybe CancelFlag
    -> Int
    -> GhciProcess
    -> Text
    -> Int
    -> IO Awaited
awaitMarker cancelFlag cap process marker requestedTimeout = do
    stateRef <- newIORef emptyCaptureState
    alreadyCancelled <- maybe (pure False) isCancelled cancelFlag
    reason <-
        if alreadyCancelled
            then pure AwaitCancellation
            else do
                let timeoutMs = normalizeTimeout requestedTimeout
                    collect = collectEvents
                        stateRef cap (encodeUtf8 marker) process
                    timed = race
                        (threadDelay (timeoutMs * 1000))
                        collect
                case cancelFlag of
                    Nothing ->
                        timed >>= \case
                            Left () -> pure AwaitTimeout
                            Right completed -> pure completed
                    Just flag ->
                        race (waitCancel flag) timed >>= \case
                            Left () -> pure AwaitCancellation
                            Right (Left ()) -> pure AwaitTimeout
                            Right (Right completed) -> pure completed
    captured <- renderCapture cap <$> readIORef stateRef
    pure Awaited
        { awaitReason = reason
        , awaitOutput = captured
        }

collectEvents
    :: IORef CaptureState
    -> Int
    -> ByteString
    -> GhciProcess
    -> IO AwaitReason
collectEvents stateRef cap marker process = go
  where
    go = do
        current <- readIORef stateRef
        event <- atomically (readGhciEvent process current)
        state <- atomicModifyIORef' stateRef \current ->
            let updated = applyEvent cap marker event current
            in (updated, updated)
        if state.stdoutCapture.streamDone
            && state.stderrCapture.streamDone
            then pure AwaitMarkers
            else if state.stdoutCapture.streamClosed
                && state.stderrCapture.streamClosed
                then pure AwaitProcessExit
                else go

readGhciEvent :: GhciProcess -> CaptureState -> STM GhciEvent
readGhciEvent process state =
    readTBQueue process.ghciEvents
        `orElse` closedEvent
            GhciStdout
            state.stdoutCapture.streamClosed
            process.ghciStdoutClosed
        `orElse` closedEvent
            GhciStderr
            state.stderrCapture.streamClosed
            process.ghciStderrClosed
  where
    closedEvent stream alreadyClosed closed
        | alreadyClosed = retry
        | otherwise = do
            readTMVar closed
            pure (GhciStreamClosed stream)

applyEvent :: Int -> ByteString -> GhciEvent -> CaptureState -> CaptureState
applyEvent cap marker event state =
    case event of
        GhciChunk GhciStdout bytes ->
            state
                { stdoutCapture =
                    consumeChunk cap marker bytes state.stdoutCapture
                }
        GhciChunk GhciStderr bytes ->
            state
                { stderrCapture =
                    consumeChunk cap marker bytes state.stderrCapture
                }
        GhciStreamClosed GhciStdout ->
            state
                { stdoutCapture = state.stdoutCapture { streamClosed = True } }
        GhciStreamClosed GhciStderr ->
            state
                { stderrCapture = state.stderrCapture { streamClosed = True } }

consumeChunk
    :: Int
    -> ByteString
    -> ByteString
    -> StreamCapture
    -> StreamCapture
consumeChunk cap marker chunk capture
    | capture.streamDone = capture
    | otherwise =
        let combined = capture.streamPending <> chunk
            (before, after) = BS.breakSubstring marker combined
        in if not (BS.null after)
            then (appendBounded cap before capture)
                { streamPending = BS.empty
                , streamDone = True
                }
            else
                let keep = min
                        (max 0 (BS.length marker - 1))
                        (BS.length combined)
                    emitLength = BS.length combined - keep
                    emitted = BS.take emitLength combined
                    pending = BS.drop emitLength combined
                in (appendBounded cap emitted capture)
                    { streamPending = pending }

appendBounded :: Int -> ByteString -> StreamCapture -> StreamCapture
appendBounded cap bytes capture
    | BS.null bytes = capture
    | cap <= 0 =
        capture { streamOutput = capture.streamOutput <> bytes }
    | otherwise =
        let remaining = max 0 (cap - BS.length capture.streamOutput)
            kept = BS.take remaining bytes
            dropped = BS.length bytes - BS.length kept
        in capture
            { streamOutput = capture.streamOutput <> kept
            , streamDropped = capture.streamDropped + dropped
            }

renderCapture :: Int -> CaptureState -> CapturedOutput
renderCapture cap state =
    let (stdout, stdoutDropped) = renderStream cap state.stdoutCapture
        (stderr, stderrDropped) = renderStream cap state.stderrCapture
    in CapturedOutput
        { capturedStdout = stdout
        , capturedStderr = stderr
        , capturedTruncated = stdoutDropped > 0 || stderrDropped > 0
        }

renderStream :: Int -> StreamCapture -> (Text, Int)
renderStream cap capture =
    let finalized =
            if capture.streamDone
                then capture
                else appendBounded cap capture.streamPending capture
        decoded = decodeUtf8With lenientDecode finalized.streamOutput
        dropped = finalized.streamDropped
        suffix
            | dropped <= 0 = ""
            | otherwise =
                "\n...[truncated "
                    <> Text.pack (show dropped)
                    <> " bytes]"
    in (decoded <> suffix, dropped)

ghciEventQueueCapacity :: Int
ghciEventQueueCapacity = 64

drainHandle
    :: GhciStream
    -> Handle
    -> TBQueue GhciEvent
    -> TMVar ()
    -> IO ()
drainHandle stream handle events closed =
    go `finally` atomically (void (tryPutTMVar closed ()))
  where
    go = do
        chunk <- BS.hGetSome handle 4096
        if BS.null chunk
            then pure ()
            else do
                atomically (writeTBQueue events (GhciChunk stream chunk))
                go

normalizeTimeout :: Int -> Int
normalizeTimeout = min 300000 . max 1

remainingMillis :: Word64 -> Int -> IO Int
remainingMillis started budget = do
    now <- getMonotonicTimeNSec
    let elapsed = fromIntegral ((now - started) `div` 1000000)
    pure (max 0 (budget - elapsed))
