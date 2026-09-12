-- | A fail-closed host for short-lived JavaScript code-mode cells.
--
-- Every cell gets a fresh VM context. Healthy Bun processes may be retained
-- between cells, but malformed or unexpected protocol messages retire the
-- worker. Effects remain restricted to the typed 'CodeModeToolHandler'.
module Agent.Tools.CodeMode.Host
    ( CodeModeConfig(..)
    , CodeModeBackend(..)
    , CodeModeError(..)
    , CodeModeHost
    , CodeModeResult(..)
    , CodeModeToolHandler
    , ImageDetailVisibility(..)
    , bundledCodeModeWorkerPath
    , codeModeWorkerPath
    , checkCodeModeAvailability
    , closeCodeModeHost
    , defaultCodeModeConfig
    , execCodeCell
    , execCodeCellWithTools
    , newCodeModeHost
    , readRunningCodeCells
    , terminateCodeCell
    , waitCodeCell
    , withCodeModeHost
    ) where

import Agent.Tools.CodeMode.Protocol
    ( CodeModeToolMetadata(..)
    , ProtocolMessage(..)
    , ToolInvocation(..)
    , decodeProtocolMessage
    , encodeExecRequestWithStateAndImageDetail
    , encodeToolFailure
    , encodeToolSuccess
    )
import Agent.Json (RawJson)
import Agent.Process (terminateProcessGroup)
import Agent.Tools.CodeMode.Host.Types
    ( Cell(..)
    , CellObservation(..)
    , CellOutcome(..)
    , CodeModeConfig(..)
    , CodeModeBackend(..)
    , CodeModeError(..)
    , CodeModeHost(..)
    , CodeModeResult(..)
    , CodeModeToolHandler
    , ImageDetailVisibility(..)
    , IdleWorker(..)
    , WorkerPool(..)
    , WorkerProcessHandle(..)
    , defaultCodeModeConfig
    )
import Agent.Tools.CodeMode.Host.Availability
    ( checkCodeModeAvailability
    , resolveWorkerCommand
    )
import Agent.Tools.CodeMode.Host.Worker
    ( bundledCodeModeWorkerPath
    , codeModeWorkerPath
    )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
    ( Async
    , asyncWithUnmask
    , cancel
    , mapConcurrently_
    , race
    , waitCatch
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , modifyMVar_
    , modifyMVarMasked_
    , newEmptyMVar
    , newMVar
    , putMVar
    , readMVar
    , withMVar
    )
import Control.Concurrent.STM
    ( STM
    , TMVar
    , TQueue
    , atomically
    , newEmptyTMVarIO
    , newTQueueIO
    , orElse
    , readTMVar
    , readTQueue
    , tryReadTQueue
    , tryReadTMVar
    , writeTQueue
    , tryPutTMVar
    )
import Control.Exception.Safe
    ( SomeException
    , bracket
    , bracketOnError
    , displayException
    , mask_
    , onException
    , try
    )
import Control.Monad (filterM, void)
import Data.Aeson (ToJSON(toJSON), Value(..), object, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector
import Data.Maybe (isNothing)
import Data.Time.Clock (UTCTime, getCurrentTime)
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
    , StdStream(..)
    , createProcess
    , getPid
    , proc
    , waitForProcess
    )
newCodeModeHost :: CodeModeConfig -> IO CodeModeHost
newCodeModeHost config = do
    -- Snapshot executable/backend selection once. Pool replenishment must not
    -- switch runtimes if the application's environment subsequently changes.
    command <- resolveWorkerCommand config
    bracketOnError
        (CodeModeHost config command
            <$> newMVar Map.empty
            <*> newIORef 0
            <*> newMVar Map.empty
            <*> newMVar (WorkerPool [] Nothing False))
        closeCodeModeHost
        \host -> do
            if config.workerPoolSize > 0
                -- Complete the ownership handoff before leaving the worker's
                -- rollback scope, so cancellation cannot close it both here
                -- and through the host's newly populated pool.
                then mask_ $ bracketOnError
                    (spawnIdleWorker config command)
                    (either (const (pure ())) stopIdleWorker)
                    \case
                        Right worker -> modifyMVar_ host.hostWorkerPool \pool ->
                            pure pool { poolIdle = [worker] }
                        Left _ -> pure ()
                else pure ()
            pure host

-- | Keep a host, its cells, and its retained workers within an action's
-- lifetime. Use 'newCodeModeHost' only when transferring ownership to a
-- longer-lived resource manager.
withCodeModeHost :: CodeModeConfig -> (CodeModeHost -> IO a) -> IO a
withCodeModeHost config = bracket (newCodeModeHost config) closeCodeModeHost

closeCodeModeHost :: CodeModeHost -> IO ()
closeCodeModeHost host = do
    cells <- modifyMVar host.hostCells \current ->
        pure (Map.empty, Map.elems current)
    mapM_ (\cell -> markCellClosed cell >> stopCell cell) cells
    (idle, filler) <- modifyMVar host.hostWorkerPool \pool ->
        pure
            ( pool { poolIdle = [], poolFiller = Nothing, poolClosed = True }
            , (pool.poolIdle, pool.poolFiller)
            )
    mapM_ cancel filler
    mapM_ (void . waitCatch) filler
    mapM_ stopIdleWorker idle

execCodeCell
    :: CodeModeHost
    -> Text
    -> [Text]
    -> Int
    -> IO (Either CodeModeError CodeModeResult)
execCodeCell host source toolNames yieldMs = do
    execCodeCellWithTools
        host
        source
        [ CodeModeToolMetadata
            { toolMetadataName = name
            , toolMetadataDescription = ""
            }
        | name <- toolNames
        ]
        yieldMs

execCodeCellWithTools
    :: CodeModeHost
    -> Text
    -> [CodeModeToolMetadata]
    -> Int
    -> IO (Either CodeModeError CodeModeResult)
execCodeCellWithTools host source tools yieldMs = do
    if BS.length (Text.encodeUtf8 source) > max 0 host.hostConfig.maxSourceBytes
        then pure $ Left $ CodeModeResourceError $
            "JavaScript source exceeds the configured "
                <> Text.pack (show host.hostConfig.maxSourceBytes)
                <> "-byte limit"
        else do
            atCapacity <- withMVar host.hostCells $
                pure . (>= activeCellLimit host) . Map.size
            if atCapacity
                then pure (Left (activeCellLimitError host))
                else do
                    identifier <- nextCellId host
                    storedValues <- withMVar host.hostStoredValues pure
                    startCell host identifier tools >>= \case
                        Left err -> pure (Left err)
                        Right cell -> do
                            inserted <- modifyMVar host.hostCells \current ->
                                if Map.size current >= activeCellLimit host
                                    then pure (current, False)
                                    else pure
                                        ( Map.insert identifier cell current
                                        , True
                                        )
                            if not inserted
                                then do
                                    stopCell cell
                                    pure (Left (activeCellLimitError host))
                                else
                                    (do
                                        sent <- try @_ @SomeException $ sendLine cell $
                                            encodeExecRequestWithStateAndImageDetail
                                                "exec"
                                                source
                                                tools
                                                storedValues
                                                (host.hostConfig.imageDetailVisibility
                                                    == ImageDetailVisible)
                                        case sent of
                                            Left err -> do
                                                void $ takeCell host identifier
                                                stopCell cell
                                                pure $ Left $ CodeModeProtocolError $
                                                    "failed to send execution request: "
                                                        <> Text.pack
                                                            (displayException err)
                                            Right () ->
                                                observeCell host cell yieldMs)
                                        `onException` abortCell host cell

abortCell :: CodeModeHost -> Cell -> IO ()
abortCell host cell =
    takeCell host cell.cellIdentifier >>= \case
        Nothing -> pure ()
        Just ownedCell -> do
            markCellClosed ownedCell
            stopCell ownedCell

activeCellLimit :: CodeModeHost -> Int
activeCellLimit host = max 1 host.hostConfig.maxActiveCells

activeCellLimitError :: CodeModeHost -> CodeModeError
activeCellLimitError host = CodeModeResourceError $
    "too many active code-mode cells (limit "
        <> Text.pack (show (activeCellLimit host))
        <> ")"

-- | Read activity without consuming output or claiming a cell's observer.
-- Completed cells remain retained until an explicit wait, but are not running.
readRunningCodeCells :: CodeModeHost -> IO [(Text, UTCTime)]
readRunningCodeCells host = withMVar host.hostCells \cells -> do
    running <- atomically $ filterM
        (fmap isNothing . tryReadTMVar . (.cellResult))
        (Map.elems cells)
    pure [(cell.cellIdentifier, cell.cellStartedAt) | cell <- running]

waitCodeCell
    :: CodeModeHost
    -> Text
    -> Int
    -> IO (Either CodeModeError CodeModeResult)
waitCodeCell host identifier yieldMs =
    lookupCell host identifier >>= \case
        Nothing -> pure $ Left $ CodeModeUnknownCell identifier
        Just cell -> observeCell host cell yieldMs

terminateCodeCell
    :: CodeModeHost
    -> Text
    -> IO (Either CodeModeError CodeModeResult)
terminateCodeCell host identifier =
    lookupCell host identifier >>= \case
        Nothing -> pure $ Left $ CodeModeUnknownCell identifier
        Just cell ->
            bracket (beginTermination cell) (releaseTermination cell) \case
                Left err -> pure (Left err)
                Right () -> do
                    observed <- atomically $ tryReadTMVar cell.cellResult
                    case observed of
                        Just result -> do
                            releaseCell host cell
                            pure $ fmap (cellOutcomeResult identifier) result
                        Nothing -> do
                            beforeStop <- atomically $
                                (,) <$> drainTQueue cell.cellYields
                                    <*> drainTQueue cell.cellContent
                            stopCell cell
                            afterStop <- atomically $
                                (,) <$> drainTQueue cell.cellYields
                                    <*> drainTQueue cell.cellContent
                            pure $ Right CodeModeTerminated
                                { cellId = identifier
                                , cellValue = combineCellOutput
                                    (fst beforeStop <> fst afterStop)
                                    (snd beforeStop <> snd afterStop)
                                }
  where
    releaseTermination _ (Left _) = pure ()
    releaseTermination cell (Right ()) = do
        markCellClosed cell
        void $ takeCell host identifier

startCell
    :: CodeModeHost
    -> Text
    -> [CodeModeToolMetadata]
    -> IO (Either CodeModeError Cell)
startCell host identifier tools = do
    idle <- modifyMVar host.hostWorkerPool \pool ->
        if pool.poolClosed
            then pure (pool, Left ())
            else case pool.poolIdle of
                [] -> pure (pool, Right Nothing)
                worker : rest -> pure
                    (pool { poolIdle = rest }, Right (Just worker))
    case idle of
        Left () ->
            pure $ Left $ CodeModeResourceError "code-mode host is closed"
        Right (Just worker) -> do
            replenishPool host
            startCellFromIdleWorker host identifier tools worker
        Right Nothing -> startFreshCell host identifier tools

startFreshCell
    :: CodeModeHost
    -> Text
    -> [CodeModeToolMetadata]
    -> IO (Either CodeModeError Cell)
startFreshCell host identifier tools =
    spawnIdleWorker host.hostConfig host.hostWorkerCommand >>= \case
        Left err -> pure (Left err)
        Right worker -> startCellFromIdleWorker host identifier tools worker

startCellFromIdleWorker
    :: CodeModeHost
    -> Text
    -> [CodeModeToolMetadata]
    -> IdleWorker
    -> IO (Either CodeModeError Cell)
startCellFromIdleWorker host identifier tools
        (IdleWorker input output stderr process writer stderrReader) =
    startCellFromProcess host identifier tools True
        (input, output, stderr, process, Just writer, Just stderrReader)

-- Keep one standby worker once the eagerly-created worker is checked out.
-- The tracked filler is cancelled and joined when the host closes.
replenishPool :: CodeModeHost -> IO ()
replenishPool host = do
    gate <- newEmptyMVar
    filler <- asyncWithUnmask \unmask -> do
        readMVar gate
        result <- unmask (spawnIdleWorker host.hostConfig host.hostWorkerCommand)
        discarded <- modifyMVar host.hostWorkerPool \pool -> do
            let poolWithoutFiller = pool { poolFiller = Nothing }
            case result of
                Right worker
                    | not pool.poolClosed
                    , length pool.poolIdle < max 0 host.hostConfig.workerPoolSize ->
                        pure
                            ( poolWithoutFiller { poolIdle = [worker] }
                            , Nothing
                            )
                Right worker -> pure (poolWithoutFiller, Just worker)
                Left _ -> pure (poolWithoutFiller, Nothing)
        mapM_ stopIdleWorker discarded
    registered <- modifyMVar host.hostWorkerPool \pool ->
        if pool.poolClosed || not (null pool.poolIdle)
                || maybe False (const True) pool.poolFiller
            then pure (pool, False)
            else pure (pool { poolFiller = Just filler }, True)
    if registered
        then putMVar gate ()
        else do
            cancel filler
            void $ waitCatch filler

spawnIdleWorker :: CodeModeConfig -> Either Text (FilePath, [String]) -> IO (Either CodeModeError IdleWorker)
spawnIdleWorker config command =
    case command of
        Left err -> pure (Left (CodeModeStartupError err))
        Right (executable, arguments) -> do
            started <- try @_ @SomeException $ createWorkerProcess $
                (proc executable arguments)
                    { std_in = CreatePipe
                    , std_out = CreatePipe
                    , std_err = CreatePipe
                    , env = Just []
                    , create_group = True
                    }
            case started of
                Left err ->
                    pure $ Left $ CodeModeStartupError $
                        Text.pack (displayException err)
                Right
                        ( Just input
                        , Just output
                        , Just stderr
                        , processHandle
                        ) -> do
                    mapM_ configurePipe [input, output, stderr]
                        `onException` stopIncompleteProcess
                            input output stderr processHandle
                    writer <- newMVar ()
                    stderrReader <- asyncWithUnmask
                        (\unmask -> unmask (readAll stderr))
                        `onException` stopIncompleteProcess
                            input output stderr processHandle
                    let idleWorker = IdleWorker input output stderr
                            processHandle writer stderrReader
                    (do
                        startup <- race
                            (threadDelay
                                (max 1 config.startupTimeoutMs * 1000))
                            (try @_ @SomeException (BS8.hGetLine output))
                        case startup of
                            Right (Right line) ->
                                case decodeProtocolMessage line of
                                    Right WorkerReady -> pure (Right idleWorker)
                                    _ -> stopIdleWorker idleWorker >> pure
                                        (Left (CodeModeProtocolError
                                            "worker did not send ready"))
                            Right (Left err) -> stopIdleWorker idleWorker >> pure
                                (Left (CodeModeStartupError
                                    (Text.pack (displayException err))))
                            Left () -> stopIdleWorker idleWorker >> pure
                                (Left (CodeModeStartupError
                                    "code-mode worker did not become ready")))
                        `onException` stopIdleWorker idleWorker
                Right (input, output, stderr, processHandle) -> do
                    stopIncompleteProcessMaybe input output stderr processHandle
                    pure $ Left $ CodeModeStartupError
                        "failed to create all code-mode worker pipes"

createWorkerProcess
    :: CreateProcess
    -> IO (Maybe Handle, Maybe Handle, Maybe Handle, WorkerProcessHandle)
createWorkerProcess specification =
    bracketOnError
        (createProcess specification)
        (\(input, output, stderr, processHandle) -> do
            groupId <- getPid processHandle
            stopIncompleteProcessMaybe input output stderr
                (WorkerProcessHandle processHandle groupId))
        \(input, output, stderr, processHandle) -> do
            groupId <- getPid processHandle
            pure (input, output, stderr, WorkerProcessHandle processHandle groupId)

startCellFromProcess
    :: CodeModeHost
    -> Text
    -> [CodeModeToolMetadata]
    -> Bool
    -> (Handle, Handle, Handle, WorkerProcessHandle, Maybe (MVar ()), Maybe (Async Text))
    -> IO (Either CodeModeError Cell)
startCellFromProcess host identifier tools alreadyReady
        (input, output, stderr, processHandle, existingWriter, existingStderr) = do
                            ( ready
                                , result
                                , yields
                                , content
                                , writerLock
                                , callbacks
                                ) <-
                                (do
                                    if alreadyReady
                                        then pure ()
                                        else mapM_ configurePipe [input, output, stderr]
                                    (,,,,,)
                                        <$> newEmptyTMVarIO
                                        <*> newEmptyTMVarIO
                                        <*> newTQueueIO
                                        <*> newTQueueIO
                                        <*> maybe (newMVar ()) pure existingWriter
                                        <*> newMVar [])
                                `onException`
                                    stopIncompleteProcess
                                        input output stderr processHandle
                            observation <- newMVar CellIdle
                            startedAt <- getCurrentTime
                            stderrReader <-
                                case (alreadyReady, existingStderr) of
                                    (True, Just reader) -> pure reader
                                    _ ->
                                        asyncWithUnmask
                                            (\unmask -> unmask (readAll stderr))
                                        `onException`
                                            stopIncompleteProcess
                                                input output stderr processHandle
                            monitor <-
                                asyncWithUnmask
                                    (\unmask -> unmask $
                                        monitorWorker
                                            host.hostConfig.toolHandler
                                            host.hostConfig.notifyHandler
                                            host.hostStoredValues
                                            (Set.fromList
                                                (map
                                                    (.toolMetadataName)
                                                    tools))
                                            input
                                            writerLock
                                            callbacks
                                            output
                                            ready
                                            result
                                            yields
                                            content
                                            alreadyReady)
                                `onException`
                                    stopIncompleteReader
                                        input output stderr
                                        processHandle stderrReader
                            startup <- if alreadyReady
                                then pure (Right (Right ()))
                                else race
                                    (threadDelay
                                        (max 1
                                            host.hostConfig.startupTimeoutMs
                                            * 1000))
                                    (atomically $ readTMVar ready)
                                `onException`
                                    stopIncomplete
                                        input output stderr
                                        processHandle monitor stderrReader
                            case startup of
                                Right (Right ()) ->
                                    pure $ Right Cell
                                        { cellIdentifier = identifier
                                        , cellStartedAt = startedAt
                                        , cellInput = input
                                        , cellOutput = output
                                        , cellErrorOutput = stderr
                                        , cellProcess = processHandle
                                        , cellWriterLock = writerLock
                                        , cellResult = result
                                        , cellYields = yields
                                        , cellContent = content
                                        , cellMonitor = monitor
                                        , cellStderr = stderrReader
                                        , cellCallbacks = callbacks
                                        , cellObservation = observation
                                        }
                                Right (Left err) -> do
                                    stopIncomplete
                                        input output stderr
                                        processHandle monitor stderrReader
                                    pure $ Left err
                                Left () -> do
                                    stopIncomplete
                                        input output stderr
                                        processHandle monitor stderrReader
                                    pure $ Left $ CodeModeStartupError
                                        "code-mode worker did not become ready"

monitorWorker
    :: CodeModeToolHandler
    -> (Text -> IO ())
    -> MVar (Map Text Value)
    -> Set Text
    -> Handle
    -> MVar ()
    -> MVar [Async ()]
    -> Handle
    -> TMVar (Either CodeModeError ())
    -> TMVar (Either CodeModeError CellOutcome)
    -> TQueue Value
    -> TQueue Value
    -> Bool
    -> IO ()
monitorWorker
        handler notify storedValues allowedTools
        input writerLock callbacks output ready result yields content initialReady =
    try @_ @SomeException (loop initialReady) >>= \case
        Right () -> pure ()
        Left err -> atomically do
            let failure = Left $ CodeModeProtocolError $
                    "worker monitor failed: "
                        <> Text.pack (displayException err)
            void $ tryPutTMVar ready failure
            void $ tryPutTMVar result failure
  where
    loop hasStarted = do
        received <- try @_ @SomeException $ BS8.hGetLine output
        case received of
            Left err ->
                failClosed hasStarted $
                    "worker output closed: "
                        <> Text.pack (displayException err)
            Right line ->
                case decodeProtocolMessage line of
                    Left err ->
                        failClosed hasStarted $
                            "invalid worker message: " <> err
                    Right WorkerReady
                        | not hasStarted -> do
                            atomically $ void $ tryPutTMVar ready (Right ())
                            loop True
                        | otherwise ->
                            failClosed True "duplicate worker ready message"
                    Right (WorkerToolInvocation invocation)
                        | hasStarted -> do
                            launchInvocation invocation
                            loop True
                        | otherwise ->
                            failClosed False "tool call received before ready"
                    Right WorkerYielded{..}
                        | hasStarted -> do
                            atomically do
                                values <- drainTQueue content
                                writeTQueue yields $
                                    preferStreamedContent values
                                        (protocolValue responseValue)
                            loop True
                        | otherwise ->
                            failClosed False "yield received before ready"
                    Right WorkerNotification{..}
                        | hasStarted -> do
                            notify notificationText
                            loop True
                        | otherwise ->
                            failClosed False "notification received before ready"
                    Right WorkerContent{..}
                        | hasStarted -> do
                            atomically $
                                writeTQueue content (protocolValue contentValue)
                            loop True
                        | otherwise ->
                            failClosed False "content received before ready"
                    Right WorkerExecSucceeded{..}
                        | hasStarted && responseId == "exec" ->
                            modifyMVarMasked_ storedValues \current -> do
                                let updated =
                                        Map.union
                                            (Map.map protocolValue
                                                responseStoredValueWrites)
                                            current
                                -- Keep stored writes and terminal publication
                                -- in one state-transition boundary. A later
                                -- cell that sees these writes must also see
                                -- this completed cell.
                                atomically do
                                    values <- drainTQueue content
                                    void $ tryPutTMVar result $
                                        Right (CellSucceeded
                                            (preferStreamedContent
                                                values
                                                (protocolValue responseValue)))
                                pure updated
                        | otherwise ->
                            failClosed hasStarted
                                "unexpected execution response id"
                    Right WorkerExecFailed{..}
                        | hasStarted && responseId == "exec" ->
                            modifyMVarMasked_ storedValues \current -> do
                                let updated =
                                        Map.union
                                            (Map.map protocolValue
                                                responseStoredValueWrites)
                                            current
                                -- See the corresponding success branch.
                                atomically do
                                    values <- drainTQueue content
                                    void $ tryPutTMVar result $
                                        Right (CellFailed
                                            (preferStreamedContent
                                                values
                                                (protocolValue responseValue))
                                            responseError)
                                pure updated
                        | otherwise ->
                            failClosed hasStarted
                                "unexpected execution error id"

    handleInvocation :: ToolInvocation -> IO ()
    handleInvocation invocation = do
        if invocation.invocationName `Set.notMember` allowedTools
            then send $ encodeToolFailure invocation.invocationId $
                "tool is not available in this cell: "
                    <> invocation.invocationName
            else do
                handled <- try @_ @SomeException $
                    handler
                        invocation.invocationName
                        (protocolValue invocation.invocationArguments)
                case handled of
                    Left err ->
                        send $ encodeToolFailure invocation.invocationId $
                            Text.pack (displayException err)
                    Right (Left err) ->
                        send $ encodeToolFailure invocation.invocationId err
                    Right (Right value) ->
                        send $ encodeToolSuccess invocation.invocationId value

    -- The worker can issue independent nested calls before awaiting them
    -- (for example through Promise.all). Keep every callback scoped to the
    -- cell so termination and shutdown cancel and join outstanding effects.
    launchInvocation :: ToolInvocation -> IO ()
    launchInvocation invocation = do
        startGate <- newEmptyMVar
        callback <- asyncWithUnmask \unmask -> do
            readMVar startGate
            unmask (handleInvocation invocation)
        (do
            modifyMVar_ callbacks (pure . (callback :))
            putMVar startGate ())
            `onException` do
                cancel callback
                void $ waitCatch callback

    send bytes =
        withMVar writerLock \() -> do
            BS.hPut input bytes
            BS8.hPutStrLn input ""
            hFlush input

    failClosed hasStarted message =
        atomically $
            if hasStarted
                then void $ tryPutTMVar result failure
                else void $ tryPutTMVar ready failure
      where
        failure = Left $ CodeModeProtocolError message

waitForCell
    :: CodeModeHost
    -> Cell
    -> Int
    -> IO (Either CodeModeError CodeModeResult)
waitForCell host cell yieldMs = do
    waited <- race
        (threadDelay (max 1 yieldMs * 1000))
        (atomically $
            (Left <$> readTQueue cell.cellYields)
                `orElse`
                    (Right <$> readTMVar cell.cellResult))
    case waited of
        Left () ->
            do
                output <- atomically $
                    contentResult <$> drainTQueue cell.cellContent
                pure $ Right CodeModeRunning
                    { cellId = cell.cellIdentifier
                    , cellOutput = output
                    }
        Right (Left output) ->
            pure $ Right CodeModeRunning
                { cellId = cell.cellIdentifier
                , cellOutput = output
                }
        Right (Right result) -> do
            markCellClosed cell
            void $ takeCell host cell.cellIdentifier
            releaseCell host cell
            pure $ fmap (cellOutcomeResult cell.cellIdentifier) result

observeCell
    :: CodeModeHost
    -> Cell
    -> Int
    -> IO (Either CodeModeError CodeModeResult)
observeCell host cell yieldMs =
    withCellObservation cell (waitForCell host cell yieldMs)

withCellObservation
    :: Cell
    -> IO (Either CodeModeError a)
    -> IO (Either CodeModeError a)
withCellObservation cell action =
    bracket (beginObservation cell) release \case
        Left err -> pure (Left err)
        Right () -> action
  where
    release (Left _) = pure ()
    release (Right ()) = endObservation cell

beginObservation :: Cell -> IO (Either CodeModeError ())
beginObservation cell =
    modifyMVar cell.cellObservation \case
        CellIdle -> pure (CellObserved, Right ())
        CellObserved ->
            pure
                ( CellObserved
                , Left (CodeModeBusyObserver cell.cellIdentifier)
                )
        CellTerminating ->
            pure
                ( CellTerminating
                , Left (CodeModeAlreadyTerminating cell.cellIdentifier)
                )
        CellClosed ->
            pure
                ( CellClosed
                , Left (CodeModeClosedCell cell.cellIdentifier)
                )

endObservation :: Cell -> IO ()
endObservation cell =
    modifyMVar_ cell.cellObservation \case
        CellObserved -> pure CellIdle
        state -> pure state

beginTermination :: Cell -> IO (Either CodeModeError ())
beginTermination cell =
    modifyMVar cell.cellObservation \case
        CellIdle -> pure (CellTerminating, Right ())
        CellObserved ->
            pure
                ( CellObserved
                , Left (CodeModeBusyObserver cell.cellIdentifier)
                )
        CellTerminating ->
            pure
                ( CellTerminating
                , Left (CodeModeAlreadyTerminating cell.cellIdentifier)
                )
        CellClosed ->
            pure
                ( CellClosed
                , Left (CodeModeClosedCell cell.cellIdentifier)
                )

markCellClosed :: Cell -> IO ()
markCellClosed cell =
    modifyMVar_ cell.cellObservation (const (pure CellClosed))

drainTQueue :: TQueue value -> STM [value]
drainTQueue queue = go []
  where
    go values =
        tryReadTQueue queue >>= \case
            Nothing -> pure (reverse values)
            Just value -> go (value : values)

contentResult :: [Value] -> Value
contentResult values = object ["content" .= values]

combineCellOutput :: [Value] -> [Value] -> Value
combineCellOutput yielded direct =
    contentResult (concatMap resultContentItems yielded <> direct)

resultContentItems :: Value -> [Value]
resultContentItems (Object result)
    | Just (Array content) <- KeyMap.lookup "content" result =
        Vector.toList content
resultContentItems value = [value]

preferStreamedContent :: [Value] -> Value -> Value
preferStreamedContent [] fallback = fallback
preferStreamedContent values _ = contentResult values

-- The host evaluator and public code-mode result API still operate on Aeson
-- values. Keep that materialisation at this explicit boundary rather than in
-- the worker protocol decoder.
protocolValue :: RawJson -> Value
protocolValue = toJSON

cellOutcomeResult :: Text -> CellOutcome -> CodeModeResult
cellOutcomeResult identifier = \case
    CellSucceeded value -> CodeModeFinished
        { cellId = identifier
        , cellValue = value
        }
    CellFailed value err -> CodeModeFailed
        { cellId = identifier
        , cellValue = value
        , cellError = err
        }

releaseCell :: CodeModeHost -> Cell -> IO ()
releaseCell host cell = do
    _ <- waitCatch cell.cellMonitor
    cancelCellCallbacks cell
    outcome <- atomically $ tryReadTMVar cell.cellResult
    case outcome of
        Just (Right _) -> do
            retained <- modifyMVar host.hostWorkerPool \pool ->
                if not pool.poolClosed
                    && host.hostConfig.workerPoolSize > length pool.poolIdle
                    then
                        pure
                            ( pool
                                { poolIdle = IdleWorker
                                    cell.cellInput
                                    cell.cellOutput
                                    cell.cellErrorOutput
                                    cell.cellProcess
                                    cell.cellWriterLock
                                    cell.cellStderr
                                    : pool.poolIdle
                                }
                            , True
                            )
                    else pure (pool, False)
            if not retained then stopCell cell else pure ()
        _ -> stopCell cell

stopCell :: Cell -> IO ()
stopCell cell = do
    terminateWorkerProcess cell.cellProcess
    cancel cell.cellMonitor
    cancelCellCallbacks cell
    closeQuietly cell.cellInput
    cancel cell.cellStderr
    void $ waitCatch cell.cellMonitor
    void $ waitCatch cell.cellStderr
    closeQuietly cell.cellOutput
    closeQuietly cell.cellErrorOutput
    reapWorkerProcess cell.cellProcess

stopIdleWorker :: IdleWorker -> IO ()
stopIdleWorker (IdleWorker input output errOut process _ stderrReader) = do
    terminateWorkerProcess process
    closeQuietly input
    cancel stderrReader
    void $ waitCatch stderrReader
    closeQuietly output
    closeQuietly errOut
    reapWorkerProcess process

cancelCellCallbacks :: Cell -> IO ()
cancelCellCallbacks cell = do
    callbacks <- modifyMVar cell.cellCallbacks \current ->
        pure ([], current)
    mapConcurrently_ cancel callbacks

stopIncomplete
    :: Handle
    -> Handle
    -> Handle
    -> WorkerProcessHandle
    -> Async ()
    -> Async Text
    -> IO ()
stopIncomplete input output stderr processHandle monitor stderrReader = do
    terminateWorkerProcess processHandle
    closeQuietly input
    cancel monitor
    cancel stderrReader
    void $ waitCatch monitor
    void $ waitCatch stderrReader
    closeQuietly output
    closeQuietly stderr
    reapWorkerProcess processHandle

stopIncompleteProcess
    :: Handle
    -> Handle
    -> Handle
    -> WorkerProcessHandle
    -> IO ()
stopIncompleteProcess input output stderr processHandle = do
    terminateWorkerProcess processHandle
    mapM_ closeQuietly [input, output, stderr]
    reapWorkerProcess processHandle

stopIncompleteProcessMaybe
    :: Maybe Handle
    -> Maybe Handle
    -> Maybe Handle
    -> WorkerProcessHandle
    -> IO ()
stopIncompleteProcessMaybe input output stderr processHandle = do
    terminateWorkerProcess processHandle
    mapM_ (mapM_ closeQuietly) [input, output, stderr]
    reapWorkerProcess processHandle

stopIncompleteReader
    :: Handle
    -> Handle
    -> Handle
    -> WorkerProcessHandle
    -> Async Text
    -> IO ()
stopIncompleteReader input output stderr processHandle stderrReader = do
    terminateWorkerProcess processHandle
    closeQuietly input
    cancel stderrReader
    void $ waitCatch stderrReader
    closeQuietly output
    closeQuietly stderr
    reapWorkerProcess processHandle

lookupCell :: CodeModeHost -> Text -> IO (Maybe Cell)
lookupCell host identifier =
    withMVar host.hostCells $
        pure . Map.lookup identifier

takeCell :: CodeModeHost -> Text -> IO (Maybe Cell)
takeCell host identifier =
    modifyMVar host.hostCells \cells ->
        let (found, remaining) = Map.updateLookupWithKey
                (\_ _ -> Nothing)
                identifier
                cells
        in pure (remaining, found)

-- | Codex allocates monotonically increasing decimal cell ids starting at 1;
-- the id is model-visible in @Script running with cell ID {id}@ output.
nextCellId :: CodeModeHost -> IO Text
nextCellId host =
    Text.pack . show <$>
        atomicModifyIORef' host.hostNextId
            (\current ->
                let next = current + 1
                in (next, next))

sendLine :: Cell -> BS.ByteString -> IO ()
sendLine cell bytes =
    withMVar cell.cellWriterLock \() -> do
        BS.hPut cell.cellInput bytes
        BS8.hPutStrLn cell.cellInput ""
        hFlush cell.cellInput

configurePipe :: Handle -> IO ()
configurePipe handle = do
    hSetBinaryMode handle True
    hSetBuffering handle NoBuffering

readAll :: Handle -> IO Text
readAll handle =
    Text.decodeUtf8With
        (\_ _ -> Just '\xfffd')
        <$> BS.hGetContents handle

-- Escalate before joining pipe readers or reaping the process. A TERM-only
-- shutdown can wait forever, including inside Safe.onException's
-- uninterruptible release handler. The shared policy polls one bounded signal
-- sequence rather than nesting timeout/race cleanup threads.
terminateWorkerProcess :: WorkerProcessHandle -> IO ()
terminateWorkerProcess (WorkerProcessHandle processHandle groupId) =
    terminateProcessGroup groupId processHandle

reapWorkerProcess :: WorkerProcessHandle -> IO ()
reapWorkerProcess (WorkerProcessHandle processHandle _) =
    void $ try @_ @SomeException $ waitForProcess processHandle

closeQuietly :: Handle -> IO ()
closeQuietly handle =
    void $ try @_ @SomeException $ hClose handle
