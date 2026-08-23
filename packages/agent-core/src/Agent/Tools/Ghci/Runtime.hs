-- | Persistent GHCi session shared by coding-tool providers.
--
-- Replies are framed with unique markers written to both stdout and stderr.
-- Waiting for both markers prevents diagnostics on stderr from arriving after
-- a stdout-only completion marker.
module Agent.Tools.Ghci.Runtime
    ( GhciSession
    , GhciOutcome(..)
    , GhciResult(..)
    , GhciProgramRequest(..)
    , GhciProgramResponse(..)
    , newGhciSession
    , newGhciProgramSession
    , closeGhciSession
    , evalGhci
    , evalGhciProgram
    , classifyGhci
    ) where

import Agent.Cancel (CancelFlag, isCancelled, waitCancel)
import Agent.OsPath (unsafeToFilePath)
import Agent.Responses.Types
    ( Response
    , ResponseCreateParams
    )
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
    , withAsync
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
    , catchAny
    , finally
    , mask
    , onException
    , try
    )
import Control.Monad (forever, void)
import Data.Aeson (FromJSON(..), ToJSON(..), Value, (.:), (.:?), (.!=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (withObject)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (sort)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory
    ( createDirectory
    , doesFileExist
    , getTemporaryDirectory
    , listDirectory
    , renameFile
    , removeDirectoryRecursive
    , removeFile
    )
import System.FilePath
    ( dropExtension
    , takeDirectory
    , takeExtension
    , (</>)
    )
import System.Environment (getExecutablePath, lookupEnv)
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
import System.Posix.Files (setFileMode)
import System.Posix.Process (getProcessID)
import System.Posix.Types (ProcessGroupID)
import Paths_agent_core (getDataFileName)

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

data GhciProgramRequest
    = GhciToolRequest !Text !Value
    | GhciLlmRequest !ResponseCreateParams
    deriving (Eq, Show)

data GhciProgramResponse
    = GhciToolResponse !Text
    | GhciLlmResponse !(Either Text Response)
    | GhciProgramError !Text
    deriving (Eq, Show)

instance ToJSON GhciProgramResponse where
    toJSON = \case
        GhciToolResponse output -> toJSON (Right output :: Either Text Text)
        GhciLlmResponse response -> toJSON response
        GhciProgramError err -> toJSON (Left err :: Either Text Text)

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
    | GhciClosed

data GhciSession = GhciSession
    { ghciEnv :: !ToolEnv
    , ghciLock :: !(MVar ())
    , ghciState :: !(IORef GhciSessionState)
    , ghciNextMarker :: !(IORef Int)
    , ghciMarkerSeed :: !Text
    , ghciClassificationCache :: !(IORef (Maybe (Text, GhciClass)))
    , ghciRpcDir :: !(Maybe FilePath)
    , ghciResponsesTypesSource :: !(Maybe FilePath)
    }

newGhciSession :: ToolEnv -> IO GhciSession
newGhciSession env = newGhciSessionWithRpc env False

-- | Dedicated GHCi runtime with the file-based nested-tool bridge installed.
-- Each program evaluation gets a fresh process so prior source cannot poison
-- helper bindings or leave background Haskell threads running.
newGhciProgramSession :: ToolEnv -> IO GhciSession
newGhciProgramSession env = newGhciSessionWithRpc env True

newGhciSessionWithRpc :: ToolEnv -> Bool -> IO GhciSession
newGhciSessionWithRpc env enableRpc = do
    lock <- newMVar ()
    state <- newIORef GhciNotStarted
    nextMarkerRef <- newIORef 0
    seed <- Text.pack . show <$> getMonotonicTimeNSec
    classificationCache <- newIORef Nothing
    rpcDir <-
        if enableRpc
            then Just <$> createRpcDirectory seed
            else pure Nothing
    responsesTypesSource <-
        if enableRpc
            then Just <$> locateResponsesTypesSource
            else pure Nothing
    pure GhciSession
        { ghciEnv = env
        , ghciLock = lock
        , ghciState = state
        , ghciNextMarker = nextMarkerRef
        , ghciMarkerSeed = seed
        , ghciClassificationCache = classificationCache
        , ghciRpcDir = rpcDir
        , ghciResponsesTypesSource = responsesTypesSource
        }

locateResponsesTypesSource :: IO FilePath
locateResponsesTypesSource = do
    configured <- lookupEnv "AGENT_RESPONSES_TYPES_SOURCE"
    dataFile <- getDataFileName "src/Agent/Responses/Types.hs"
    executable <- getExecutablePath
    firstExisting
        ( maybe [] pure configured
            <> [dataFile]
        )
        >>= \case
            Just path -> pure path
            Nothing ->
                findFromAncestor (takeDirectory executable) >>= \case
                    Just path -> pure path
                    Nothing -> pure dataFile
  where
    firstExisting [] = pure Nothing
    firstExisting (path : rest) = do
        exists <- doesFileExist path
        if exists then pure (Just path) else firstExisting rest

    findFromAncestor directory = do
        found <- firstExisting
            [ directory </> "packages/agent-core/src/Agent/Responses/Types.hs"
            , directory </> "src/Agent/Responses/Types.hs"
            ]
        case found of
            Just path -> pure (Just path)
            Nothing ->
                let parent = takeDirectory directory
                in if parent == directory
                    then pure Nothing
                    else findFromAncestor parent

closeGhciSession :: GhciSession -> IO ()
closeGhciSession session =
    withGhciLock session do
        current <- atomicModifyIORef' session.ghciState \state ->
            (GhciClosed, state)
        writeIORef session.ghciClassificationCache Nothing
        case current of
            GhciRunning process -> shutdownProcess process
            GhciNotStarted -> pure ()
            GhciClosed -> pure ()
        mapM_ removeRpcDirectory session.ghciRpcDir

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

-- | Evaluate an approval-gated Haskell program with a local nested-tool
-- callback. Requests and responses travel through a private temporary
-- directory, so intermediate tool results never enter GHCi stdout or the
-- model transcript unless the program explicitly emits them.
evalGhciProgram
    :: GhciSession
    -> Text
    -> Int
    -> ([GhciProgramRequest] -> IO [GhciProgramResponse])
    -> IO GhciResult
evalGhciProgram session expression requestedTimeout invokeProgramRequests =
    withGhciLock session do
        case session.ghciRpcDir of
            Nothing ->
                pure $ emptyResult GhciProcessFailed GhciEffectful
                    "This GHCi session does not have the nested-tool bridge."
            Just rpcDir -> do
                clearRpcDirectory rpcDir
                stopGhciProcess session
                let timeoutMs = normalizeTimeout requestedTimeout
                started <- getMonotonicTimeNSec
                token <- programRpcToken session
                inFlight <- newIORef (0 :: Int)
                (do
                    result <-
                        withAsync
                            (serveRpcRequests
                                rpcDir token inFlight invokeProgramRequests)
                            \_worker -> do
                                bound <- evalRawGhci
                                    session
                                    (bindProgramHelpers token)
                                    (min 5000 timeoutMs)
                                if bound.ghciOutcome /= GhciCompleted
                                    || not bound.ghciOk
                                    then pure bound
                                    else do
                                        remaining <-
                                            remainingMillis started timeoutMs
                                        if remaining <= 0
                                            then pure $ emptyResult
                                                GhciTimedOut
                                                GhciEffectful
                                                "Timed out while preparing the Haskell program."
                                            else do
                                                evaluated <- evalRawGhci
                                                    session expression remaining
                                                -- The submitted expression has
                                                -- returned. Stop its process
                                                -- before draining accepted RPCs
                                                -- so delayed forked threads
                                                -- cannot start new tool calls.
                                                stopGhciProcess session
                                                finishProgramRequests
                                                    rpcDir
                                                    inFlight
                                                    started
                                                    timeoutMs
                                                    evaluated
                    pure result { ghciClass = GhciEffectful })
                    `finally` do
                        stopGhciProcess session
                        clearRpcDirectory rpcDir

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
evalRawGhci session expression requestedTimeout = do
    let timeoutMs = normalizeTimeout requestedTimeout
    cancelled <- isCancelled session.ghciEnv.toolCancel
    if cancelled
        then pure $ emptyResult GhciCancelled GhciEffectful "GHCi evaluation cancelled."
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
                            ("Failed to send input to GHCi: " <> Text.pack (show err)))
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
        GhciRunning process -> do
            exited <- getProcessExitCode process.ghciHandle
            case exited of
                Nothing -> pure (Right process)
                Just _ -> do
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
                sent <- try @_ @SomeException do
                    case ( session.ghciRpcDir
                         , session.ghciResponsesTypesSource
                         ) of
                        (Just rpcDir, Just responsesTypesSource) ->
                            installRpcHelpers
                                process rpcDir responsesTypesSource
                        _ -> pure ()
                    sendMarker process marker
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
restartProcess session = do
    current <- atomicModifyIORef' session.ghciState \state ->
        (case state of
            GhciClosed -> GhciClosed
            _ -> GhciNotStarted, state)
    case current of
        GhciRunning process -> shutdownProcess process
        GhciNotStarted -> pure ()
        GhciClosed -> pure ()
    writeIORef session.ghciClassificationCache Nothing
    case current of
        GhciClosed -> pure False
        _ -> either (const False) (const True) <$> startProcess session

-- | Stop the current process without starting a replacement. Programmatic
-- evaluations use this at both boundaries to isolate helper bindings and
-- terminate any Haskell threads forked by the submitted source.
stopGhciProcess :: GhciSession -> IO ()
stopGhciProcess session = do
    current <- atomicModifyIORef' session.ghciState \state ->
        (case state of
            GhciClosed -> GhciClosed
            _ -> GhciNotStarted, state)
    case current of
        GhciRunning process -> shutdownProcess process
        GhciNotStarted -> pure ()
        GhciClosed -> pure ()
    writeIORef session.ghciClassificationCache Nothing

ghciArgs :: [String]
ghciArgs =
    [ "-ignore-dot-ghci"
    , "-v0"
    , "-XGHC2021"
    ]
        ++ map ("-X" <>) defaultGhciExtensions

spawnProcess :: ToolEnv -> IO (Either Text GhciProcess)
spawnProcess env = do
    ghciExecutable <- maybe "ghci" id <$> lookupEnv "AGENT_GHCI"
    let spec = (proc ghciExecutable ghciArgs)
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

installRpcHelpers :: GhciProcess -> FilePath -> FilePath -> IO ()
installRpcHelpers process rpcDir responsesTypesSource = do
    sendLine process
        (":load " <> Text.pack (show responsesTypesSource))
    mapM_ (sendLine process)
        [ "import Prelude"
        , "import qualified Prelude as AgentGhciPreludeInternal"
        , "import Agent.Responses.Types"
        , "import qualified Agent.Responses.Types as Responses"
        , "import Control.Concurrent.Async (Concurrently(..), runConcurrently)"
        , "import Data.Aeson (FromJSON, ToJSON, Value, object, (.=))"
        , "import qualified Data.Aeson as AgentGhciAesonInternal"
        , "import qualified Data.ByteString.Lazy as AgentGhciLazyBytesInternal"
        , "import Data.List (group, nub, sort, sortOn)"
        , "import Data.Maybe (catMaybes, fromMaybe, mapMaybe)"
        , "import Data.Text (Text)"
        , "import qualified Data.Text as Text"
        , "import qualified Data.Text as T"
        , "import qualified Data.Text as AgentGhciTextInternal"
        , "import qualified Data.Text.Encoding as AgentGhciTextEncodingInternal"
        , "import qualified Data.Text.IO as AgentGhciTextIOInternal"
        , "import qualified Data.Unique as AgentGhciUniqueInternal"
        , "import qualified Control.Concurrent as AgentGhciConcurrentInternal"
        , "import qualified System.Directory as AgentGhciDirectoryInternal"
        , "import qualified System.FilePath as AgentGhciFilePathInternal"
        , "import qualified System.IO as AgentGhciIOInternal"
        ]
    sendLine process ":{"
    mapM_ (sendLine process)
        [ "let agentGhciAwaitResponseInternal responsePath = do"
        , "      exists <- AgentGhciDirectoryInternal.doesFileExist responsePath"
        , "      if exists"
        , "        then AgentGhciLazyBytesInternal.readFile responsePath"
        , "        else AgentGhciConcurrentInternal.threadDelay 10000 >> agentGhciAwaitResponseInternal responsePath"
        , "    agentGhciRpcInternal requestPrefix payload = do"
        , "      unique <- AgentGhciUniqueInternal.newUnique"
        , "      let requestId = requestPrefix AgentGhciPreludeInternal.++ \"-\" AgentGhciPreludeInternal.++ AgentGhciPreludeInternal.show (AgentGhciUniqueInternal.hashUnique unique)"
        , "          requestPath = AgentGhciFilePathInternal.combine "
            <> quotedRpcDir <> " (requestId AgentGhciPreludeInternal.++ \".request\")"
        , "          requestTmp = requestPath AgentGhciPreludeInternal.++ \".tmp\""
        , "          responsePath = AgentGhciFilePathInternal.combine "
            <> quotedRpcDir <> " (requestId AgentGhciPreludeInternal.++ \".response\")"
        , "      AgentGhciLazyBytesInternal.writeFile requestTmp"
        , "        (AgentGhciAesonInternal.encode payload)"
        , "      AgentGhciDirectoryInternal.renameFile requestTmp requestPath"
        , "      responseBytes <- agentGhciAwaitResponseInternal responsePath"
        , "      AgentGhciDirectoryInternal.removeFile responsePath"
        , "      AgentGhciPreludeInternal.pure responseBytes"
        , "    agentGhciCallToolInternal :: AgentGhciPreludeInternal.String -> AgentGhciTextInternal.Text -> Value -> IO AgentGhciTextInternal.Text"
        , "    agentGhciCallToolInternal requestPrefix name arguments = do"
        , "      responseBytes <- agentGhciRpcInternal requestPrefix"
        , "        (object [\"kind\" .= (\"tool\" :: Text), \"name\" .= name, \"arguments\" .= arguments])"
        , "      case AgentGhciAesonInternal.eitherDecode responseBytes of"
        , "        AgentGhciPreludeInternal.Left err ->"
        , "          AgentGhciPreludeInternal.ioError"
        , "            (AgentGhciPreludeInternal.userError (\"Invalid tool response: \" AgentGhciPreludeInternal.++ err))"
        , "        AgentGhciPreludeInternal.Right (AgentGhciPreludeInternal.Left err) ->"
        , "          AgentGhciPreludeInternal.pure err"
        , "        AgentGhciPreludeInternal.Right (AgentGhciPreludeInternal.Right output) ->"
        , "          AgentGhciPreludeInternal.pure output"
        , "    agentGhciCallLLMInternal :: AgentGhciPreludeInternal.String -> ResponseCreateParams -> IO Response"
        , "    agentGhciCallLLMInternal requestPrefix request = do"
        , "      responseBytes <- agentGhciRpcInternal requestPrefix"
        , "        (object [\"kind\" .= (\"llm\" :: Text), \"request\" .= request])"
        , "      case AgentGhciAesonInternal.eitherDecode responseBytes of"
        , "        AgentGhciPreludeInternal.Left err ->"
        , "          AgentGhciPreludeInternal.ioError"
        , "            (AgentGhciPreludeInternal.userError (\"Invalid LLM response: \" AgentGhciPreludeInternal.++ err))"
        , "        AgentGhciPreludeInternal.Right (AgentGhciPreludeInternal.Left err) ->"
        , "          AgentGhciPreludeInternal.ioError"
        , "            (AgentGhciPreludeInternal.userError (AgentGhciTextInternal.unpack err))"
        , "        AgentGhciPreludeInternal.Right (AgentGhciPreludeInternal.Right response) ->"
        , "          AgentGhciPreludeInternal.pure response"
        , "    agentGhciCallLLMTextInternal :: AgentGhciPreludeInternal.String -> ResponseCreateParams -> IO AgentGhciTextInternal.Text"
        , "    agentGhciCallLLMTextInternal requestPrefix request = do"
        , "      response <- agentGhciCallLLMInternal requestPrefix request"
        , "      case responseOutputText response of"
        , "        AgentGhciPreludeInternal.Left err ->"
        , "          AgentGhciPreludeInternal.ioError"
        , "            (AgentGhciPreludeInternal.userError"
        , "              (AgentGhciTextInternal.unpack err))"
        , "        AgentGhciPreludeInternal.Right outputText ->"
        , "          AgentGhciPreludeInternal.pure outputText"
        , "    decodeJsonText :: FromJSON value => AgentGhciTextInternal.Text -> AgentGhciPreludeInternal.Either AgentGhciTextInternal.Text value"
        , "    decodeJsonText value = case AgentGhciAesonInternal.eitherDecodeStrict'"
        , "      (AgentGhciTextEncodingInternal.encodeUtf8 value) of"
        , "        AgentGhciPreludeInternal.Left err ->"
        , "          AgentGhciPreludeInternal.Left (AgentGhciTextInternal.pack err)"
        , "        AgentGhciPreludeInternal.Right decoded ->"
        , "          AgentGhciPreludeInternal.Right decoded"
        , "    encodeJsonText :: ToJSON value => value -> AgentGhciTextInternal.Text"
        , "    encodeJsonText = AgentGhciTextEncodingInternal.decodeUtf8"
        , "      AgentGhciPreludeInternal.. AgentGhciLazyBytesInternal.toStrict"
        , "      AgentGhciPreludeInternal.. AgentGhciAesonInternal.encode"
        , "    agentGhciCallLLMJsonInternal :: FromJSON value => AgentGhciPreludeInternal.String -> ResponseCreateParams -> IO value"
        , "    agentGhciCallLLMJsonInternal requestPrefix request = do"
        , "      outputText <- agentGhciCallLLMTextInternal requestPrefix request"
        , "      case decodeJsonText outputText of"
        , "        AgentGhciPreludeInternal.Left err ->"
        , "          AgentGhciPreludeInternal.ioError"
        , "            (AgentGhciPreludeInternal.userError"
        , "              (\"Invalid JSON assistant output: \""
        , "                AgentGhciPreludeInternal.++ AgentGhciTextInternal.unpack err))"
        , "        AgentGhciPreludeInternal.Right decoded ->"
        , "          AgentGhciPreludeInternal.pure decoded"
        , "    callTool = agentGhciCallToolInternal \"inactive\""
        , "    callLLM = agentGhciCallLLMInternal \"inactive\""
        , "    callLLMText = agentGhciCallLLMTextInternal \"inactive\""
        , "    callLLMJson = agentGhciCallLLMJsonInternal \"inactive\""
        , "    emitText :: AgentGhciTextInternal.Text -> IO ()"
        , "    emitText text = do"
        , "      AgentGhciTextIOInternal.putStrLn text"
        , "      AgentGhciIOInternal.hFlush AgentGhciIOInternal.stdout"
        ]
    sendLine process ":}"
  where
    quotedRpcDir = Text.pack (show rpcDir)

instance FromJSON GhciProgramRequest where
    parseJSON = withObject "GHCi program request" \object -> do
        kind <- object .:? "kind" .!= ("tool" :: Text)
        case kind of
            "tool" ->
                GhciToolRequest
                    <$> object .: "name"
                    <*> object .: "arguments"
            "llm" -> GhciLlmRequest <$> object .: "request"
            other -> fail
                ("Unknown GHCi program request kind: " <> Text.unpack other)

createRpcDirectory :: Text -> IO FilePath
createRpcDirectory seed = do
    root <- getTemporaryDirectory
    pid <- getProcessID
    let path =
            root
                </> ( "haskell-agent-ghci-rpc-"
                        <> show pid
                        <> "-"
                        <> Text.unpack seed
                    )
    createDirectory path
    setFileMode path 0o700
    pure path

removeRpcDirectory :: FilePath -> IO ()
removeRpcDirectory path =
    removeDirectoryRecursive path `catchAny` \_ -> pure ()

clearRpcDirectory :: FilePath -> IO ()
clearRpcDirectory path =
    (listDirectory path >>= mapM_ removeEntry)
        `catchAny` \_ -> pure ()
  where
    removeEntry entry =
        removeFile (path </> entry) `catchAny` \_ -> pure ()

serveRpcRequests
    :: FilePath
    -> Text
    -> IORef Int
    -> ([GhciProgramRequest] -> IO [GhciProgramResponse])
    -> IO ()
serveRpcRequests rpcDir token inFlight invokeTools = forever do
    entries <- sort <$> listDirectory rpcDir
    let requests = requestEntries entries
    if null requests
        then threadDelay 10000
        else do
            -- Concurrently-authored callTool actions publish their request
            -- files close together. Wait for a short quiet period, claim the
            -- complete batch, then route it through the loop's normal
            -- execution-policy scheduler.
            batch <- collectBatch requests
            claimed <- claimBatch batch
            serveBatch claimed
  where
    tokenPrefix = token <> "-"

    requestEntries entries =
        [ entry
        | entry <- entries
        , takeExtension entry == ".request"
        ]

    collectBatch initial = go initial (0 :: Int) (0 :: Int)
      where
        pollMicros = 2000
        quietTarget = 2
        maxPolls = 10

        go previous quiet polls
            | quiet >= quietTarget || polls >= maxPolls = pure previous
            | otherwise = do
                threadDelay pollMicros
                current <- requestEntries . sort <$> listDirectory rpcDir
                go current
                    (if current == previous then quiet + 1 else 0)
                    (polls + 1)

    claimBatch entries =
        fmap concat $ traverse claim entries
      where
        claim entry =
            let requestPath = rpcDir </> entry
                processingEntry = dropExtension entry <> ".processing"
                processingPath = rpcDir </> processingEntry
            in (renameFile requestPath processingPath
                    >> pure [processingEntry])
                `catchAny` \_ -> pure []

    serveBatch [] = pure ()
    serveBatch entries = do
        let claimedCount = length entries
        atomicModifyIORef' inFlight \count ->
            (count + claimedCount, ())
        (do
            decoded <- traverse readRequest entries
            let accepted =
                    [ request
                    | (_, Right request) <- decoded
                    ]
            outputs <-
                if null accepted
                    then pure []
                    else
                        (normalizeOutputs accepted
                            <$> invokeTools accepted)
                            `catchAny` \exception ->
                                pure
                                    [ failureResponse request
                                        ("Nested program bridge failed: "
                                            <> Text.pack (show exception))
                                    | request <- accepted
                                    ]
            writeDecoded decoded outputs)
            `finally` atomicModifyIORef' inFlight \count ->
                (max 0 (count - claimedCount), ())

    readRequest entry =
        ((\request -> (entry, request)) <$> readOne entry)
            `catchAny` \exception ->
                (removeFile (rpcDir </> entry) `catchAny` \_ -> pure ())
                    >> pure
                    ( entry
                    , Left
                        ("Nested tool bridge failed: "
                            <> Text.pack (show exception))
                    )

    readOne entry = do
        let processingPath = rpcDir </> entry
        bytes <- LBS.readFile processingPath
            `finally`
                (removeFile processingPath `catchAny` \_ -> pure ())
        if tokenPrefix `Text.isPrefixOf` Text.pack entry
            then case Aeson.eitherDecode bytes
                    :: Either String GhciProgramRequest of
                Left err ->
                    pure (Left
                        ("Invalid nested program request: " <> Text.pack err))
                Right request ->
                    pure (Right request)
            else pure (Left
                "Error: this callTool capability belongs to an earlier \
                \run_haskell_program invocation")

    normalizeOutputs requests outputs
        | length outputs == length requests = outputs
        | otherwise =
            zipWith
                (\request output ->
                    maybe
                        (failureResponse request
                            "Nested program bridge failed: result count mismatch")
                        id
                        output)
                requests
                (map Just outputs <> repeat Nothing)

    failureResponse request message = case request of
        GhciToolRequest{} -> GhciToolResponse message
        GhciLlmRequest{} -> GhciLlmResponse (Left message)

    writeDecoded
        :: [(FilePath, Either Text GhciProgramRequest)]
        -> [GhciProgramResponse]
        -> IO ()
    writeDecoded [] _ = pure ()
    writeDecoded ((entry, Left output) : rest) outputs =
        writeResponse entry (GhciProgramError output)
            >> writeDecoded rest outputs
    writeDecoded ((entry, Right _) : rest) (output : outputs) =
        writeResponse entry output >> writeDecoded rest outputs
    writeDecoded ((entry, Right request) : rest) [] =
        writeResponse entry
            (failureResponse request
                "Nested program bridge failed: missing result")
            >> writeDecoded rest []

    writeResponse :: FilePath -> GhciProgramResponse -> IO ()
    writeResponse entry output = do
        let base = dropExtension entry
            responsePath = rpcDir </> (base <> ".response")
            responseTmp = responsePath <> ".tmp"
        LBS.writeFile responseTmp (Aeson.encode output)
        renameFile responseTmp responsePath

programRpcToken :: GhciSession -> IO Text
programRpcToken session = do
    now <- getMonotonicTimeNSec
    pure (session.ghciMarkerSeed <> "-" <> Text.pack (show now))

bindProgramHelpers :: Text -> Text
bindProgramHelpers token =
    "let callTool = agentGhciCallToolInternal "
        <> quoted
        <> "; callLLM = agentGhciCallLLMInternal "
        <> quoted
        <> "; callLLMText = agentGhciCallLLMTextInternal "
        <> quoted
        <> "; callLLMJson = agentGhciCallLLMJsonInternal "
        <> quoted
  where
    quoted = Text.pack (show (Text.unpack token))

finishProgramRequests
    :: FilePath
    -> IORef Int
    -> Word64
    -> Int
    -> GhciResult
    -> IO GhciResult
finishProgramRequests rpcDir inFlight started timeoutMs result
    | result.ghciOutcome /= GhciCompleted = pure result
    | otherwise = do
        remaining <- remainingMillis started timeoutMs
        drained <- waitForRpcDrain rpcDir inFlight remaining
        if drained
            then pure result
            else pure $ emptyResult
                GhciTimedOut
                GhciEffectful
                "Timed out waiting for nested tool calls to finish."

waitForRpcDrain :: FilePath -> IORef Int -> Int -> IO Bool
waitForRpcDrain rpcDir inFlight = go (0 :: Int)
  where
    go quiet remaining
        | remaining <= 0 = drainedNow
        | otherwise = do
            drained <- drainedNow
            if drained && quiet >= 1
                then pure True
                else do
                    let delayMs = min 10 remaining
                    threadDelay (delayMs * 1000)
                    go (if drained then quiet + 1 else 0) (remaining - delayMs)

    drainedNow = do
        active <- readIORef inFlight
        entries <- listDirectory rpcDir `catchAny` \_ -> pure []
        let bridgeFiles =
                [ entry
                | entry <- entries
                , takeExtension entry `elem`
                    [".request", ".processing", ".tmp"]
                ]
        pure (active == 0 && null bridgeFiles)

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
