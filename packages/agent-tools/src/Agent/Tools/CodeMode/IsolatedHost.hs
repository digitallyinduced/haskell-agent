{-# LANGUAGE TupleSections #-}
-- | Disposable code-mode cells with bounded host-side resources.
-- Each cell receives a fresh worker; only pristine processes are prewarmed
-- inside this host. All effects cross the typed, allowlisted tool dispatcher.
-- The caller must supply an operating-system containment launcher; there is
-- deliberately no unsandboxed default. Unlike the interactive pooled host,
-- this host never retains a process after it has executed guest code.
module Agent.Tools.CodeMode.IsolatedHost
    ( CodeModeConfig(..), CodeModeError(..), CodeModeHost, CodeModeResult(..)
    , CodeModeToolHandler, ImageDetailVisibility(..), defaultCodeModeConfig
    , checkCodeModeAvailability
    , newCodeModeHost, withCodeModeHost, closeCodeModeHost
    , execCodeCell, execCodeCellWithTools, waitCodeCell, terminateCodeCell
    , readRunningCodeCells, codeModeHostWithToolHandler
    ) where

import Prelude
import Agent.Tools.CodeMode.IsolatedHost.Types
import Agent.Tools.CodeMode.IsolatedHost.Process
import Agent.Tools.CodeMode.IsolatedHost.Limits
import Agent.Tools.CodeMode.Protocol.Value
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, asyncWithUnmask, cancel, mapConcurrently_, race, waitCatch)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception.Safe (SomeException, bracket, bracketOnError, catch, displayException, finally, onException, tryAny)
import Control.Monad (filterM, void)
import Data.Aeson (Value(..), encode, object, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (atomicModifyIORef', newIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Data.Vector as Vector

-- Probe the actual contained worker, never a separate unsandboxed Bun process.
checkCodeModeAvailability :: CodeModeConfig -> IO (Either Text ())
checkCodeModeAvailability config = bracket (spawnWorker config)
    (either (const (pure ())) stopWorker)
    (pure . either (Left . Text.pack . show) (const (Right ())))

newCodeModeHost :: CodeModeConfig -> IO CodeModeHost
newCodeModeHost config =
    bracketOnError
        (CodeModeHost config
            <$> newMVar (HostState False Map.empty [])
            <*> newIORef 0
            <*> newMVar Map.empty)
        closeCodeModeHost
        \host -> do
            -- Warm one worker before the first model turn. Further processes
            -- are created only when concurrent cells actually require them.
            if config.workerPoolSize > 0
                then modifyMVarMasked_ host.hostState \state ->
                    spawnWorker config >>= \case
                        Right worker -> pure state { hostIdle = [worker] }
                        Left _ -> pure state
                else pure ()
            pure host

withCodeModeHost :: CodeModeConfig -> (CodeModeHost -> IO a) -> IO a
withCodeModeHost config = bracket (newCodeModeHost config) closeCodeModeHost

closeCodeModeHost :: CodeModeHost -> IO ()
closeCodeModeHost host = do
    (cells, idle) <- modifyMVar host.hostState \state ->
        pure (HostState True Map.empty [], (Map.elems state.hostCells, state.hostIdle))
    mapM_ stopCell cells
    mapM_ stopWorker idle
    modifyMVar_ host.hostStoredValues (const (pure Map.empty))

-- | Existing cells retain the handler captured at their creation.
codeModeHostWithToolHandler :: CodeModeHost -> CodeModeToolHandler -> CodeModeHost
codeModeHostWithToolHandler host handler =
    host { hostConfig = host.hostConfig { toolHandler = handler } }

execCodeCell :: CodeModeHost -> Text -> [Text] -> Int -> IO (Either CodeModeError CodeModeResult)
execCodeCell host source names =
    execCodeCellWithTools host source [CodeModeToolMetadata name "" | name <- names]

execCodeCellWithTools
    :: CodeModeHost -> Text -> [CodeModeToolMetadata] -> Int
    -> IO (Either CodeModeError CodeModeResult)
execCodeCellWithTools host source tools yieldMs
    | BS.length (Text.encodeUtf8 source) > max 0 host.hostConfig.maxSourceBytes =
        pure (Left (CodeModeResourceError "JavaScript source exceeds the configured byte limit"))
    | otherwise = bracketOnError acquire (either (const (pure ())) (abortCell host)) \case
        Left err -> pure (Left err)
        Right cell -> do
            stored <- readMVar host.hostStoredValues
            sent <- tryAny $ sendLine cell.cellWorker $
                encodeExecRequestWithStateAndImageDetail "exec" source tools stored
                    (host.hostConfig.imageDetailVisibility == ImageDetailVisible)
            case sent of
                Left err -> do
                    abortCell host cell
                    pure (Left (CodeModeProtocolError (Text.pack (displayException err))))
                Right () -> observeCell host cell yieldMs
  where
    acquire = modifyMVarMasked host.hostState \state ->
            if state.hostClosed
                then pure (state, Left (CodeModeResourceError "Code-mode host is closed"))
                else if Map.size state.hostCells >= max 1 host.hostConfig.maxActiveCells
                    then pure (state, Left (CodeModeResourceError "Too many active code-mode cells"))
                    else do
                        (remaining, spawned) <- case state.hostIdle of
                            worker : rest -> pure (rest, Right worker)
                            [] -> ([],) <$> spawnWorker host.hostConfig
                        case spawned of
                            Left err -> pure (state { hostIdle = remaining }, Left err)
                            Right worker -> do
                                identifier <- Text.pack . show <$> atomicModifyIORef' host.hostNextId
                                    (\n -> (n + 1, n + 1))
                                cell <- startCell host identifier tools worker
                                    `onException` stopWorker worker
                                pure
                                    ( state { hostCells = Map.insert identifier cell state.hostCells
                                            , hostIdle = remaining }
                                    , Right cell )
startCell :: CodeModeHost -> Text -> [CodeModeToolMetadata] -> Worker -> IO Cell
startCell host identifier tools worker = do
    result <- newEmptyTMVarIO
    yields <- newTQueueIO
    content <- newTQueueIO
    callbacks <- newMVar []
    observation <- newMVar False
    started <- getCurrentTime
    monitor <- asyncWithUnmask \unmask -> unmask $
        monitorWorker host tools worker callbacks result yields content
    pure (Cell identifier started worker result yields content monitor callbacks observation)

monitorWorker
    :: CodeModeHost -> [CodeModeToolMetadata] -> Worker
    -> MVar [Async ()]
    -> TMVar (Either CodeModeError CellOutcome) -> TQueue Value -> TQueue Value -> IO ()
monitorWorker host tools worker callbacks result yields content =
    (do
        readFrame <- boundedProtocolReader worker.workerOutput
        completed <- race (threadDelay 180000000) (loop readFrame)
        case completed of
            Left () -> publish (Left (CodeModeResourceError "Code-mode cell lifetime exceeded"))
            Right () -> pure ())
        `catch` (\(err :: SomeException) ->
            publish (Left (CodeModeProtocolError (Text.pack (displayException err)))))
        `finally` (killWorker worker >> readMVar callbacks >>= mapConcurrently_ cancel)
  where
    allowed = Set.fromList (map (.toolMetadataName) tools)
    publish outcome = atomically (void (tryPutTMVar result outcome))
    loop readFrame = readFrame >>= \line ->
        case decodeProtocolMessage line of
            Left err -> publish (Left (CodeModeProtocolError err))
            Right WorkerReady -> publish (Left (CodeModeProtocolError "Unexpected ready message"))
            Right (WorkerToolInvocation invocation) -> do
                -- Callbacks belong to the cell, including non-awaited JS calls.
                -- Registration is protected before making the effect runnable.
                gate <- newEmptyMVar
                modifyMVarMasked_ callbacks \current -> do
                    -- Count every invocation, not just currently running calls:
                    -- an escaped guest can forge RPC without the JS-side cap.
                    if length current >= 128
                        then ioError (userError "Code-mode tool invocation limit exceeded")
                        else pure ()
                    callback <- asyncWithUnmask \unmask -> readMVar gate >> unmask (invoke invocation)
                    pure (callback : current)
                putMVar gate ()
                loop readFrame
            Right WorkerYielded{responseValue} -> do
                atomically do
                    streamed <- drainQueue content
                    writeTQueue yields (preferStreamed streamed responseValue)
                loop readFrame
            Right WorkerNotification{notificationText} ->
                host.hostConfig.notifyHandler notificationText >> loop readFrame
            Right WorkerContent{contentValue} ->
                atomically (writeTQueue content contentValue) >> loop readFrame
            Right WorkerExecSucceeded{responseId, responseValue, responseStoredValueWrites}
                | responseId == "exec" -> finish responseStoredValueWrites
                    (\streamed -> CellSucceeded (preferStreamed streamed responseValue))
                | otherwise -> publish (Left (CodeModeProtocolError "Unexpected execution response id"))
            Right WorkerExecFailed{responseId, responseError, responseValue, responseStoredValueWrites}
                | responseId == "exec" -> finish responseStoredValueWrites
                    (\streamed -> CellFailed (preferStreamed streamed responseValue) responseError)
                | otherwise -> publish (Left (CodeModeProtocolError "Unexpected execution error id"))
    finish writes outcome = modifyMVar_ host.hostStoredValues \current -> do
        let merged = Map.union writes current
        if LBS.length (LBS.take (8 * 1024 * 1024 + 1) (encode merged)) > 8 * 1024 * 1024
            then ioError (userError "Code-mode stored state exceeds byte limit")
            else pure ()
        atomically do
            streamed <- drainQueue content
            void (tryPutTMVar result (Right (outcome streamed)))
        pure merged
    invoke invocation = do
        response <- if invocation.invocationName `Set.notMember` allowed
            then pure (Left ("Tool is not available in this cell: " <> invocation.invocationName))
            else do
                handled <- tryAny
                    (host.hostConfig.toolHandler invocation.invocationName invocation.invocationArguments)
                pure (either (Left . Text.pack . displayException) id handled)
        sendLine worker $ either (encodeToolFailure invocation.invocationId)
            (encodeToolSuccess invocation.invocationId) response

waitCodeCell :: CodeModeHost -> Text -> Int -> IO (Either CodeModeError CodeModeResult)
waitCodeCell host identifier yieldMs =
    lookupCell host identifier >>= \case
        Nothing -> pure (Left (CodeModeUnknownCell identifier))
        Just cell -> observeCell host cell yieldMs

observeCell :: CodeModeHost -> Cell -> Int -> IO (Either CodeModeError CodeModeResult)
observeCell host cell yieldMs = withObservation cell do
    waited <- race (threadDelay (max 1 (min 300000 yieldMs) * 1000))
        (atomically ((Left <$> readTQueue cell.cellYields) `orElse`
            (Right <$> readTMVar cell.cellResult)))
    case waited of
        Left () -> do
            output <- atomically (contentResult <$> drainQueue cell.cellContent)
            pure (Right (CodeModeRunning cell.cellIdentifier output))
        Right (Left output) -> pure (Right (CodeModeRunning cell.cellIdentifier output))
        Right (Right outcome) -> do
            releaseCell host cell
            pure (outcomeResult cell.cellIdentifier <$> outcome)

terminateCodeCell :: CodeModeHost -> Text -> IO (Either CodeModeError CodeModeResult)
terminateCodeCell host identifier = lookupCell host identifier >>= \case
    Nothing -> pure (Left (CodeModeUnknownCell identifier))
    Just cell -> withObservation cell do
        outcome <- atomically (tryReadTMVar cell.cellResult)
        case outcome of
            Just terminal -> do
                releaseCell host cell
                pure (outcomeResult identifier <$> terminal)
            Nothing -> do
                abortCell host cell
                output <- atomically do
                    yielded <- drainQueue cell.cellYields
                    direct <- drainQueue cell.cellContent
                    pure (contentResult (concatMap resultContent yielded <> direct))
                pure (Right (CodeModeTerminated identifier output))

withObservation :: Cell -> IO (Either CodeModeError a) -> IO (Either CodeModeError a)
withObservation cell action =
    bracket
        (modifyMVar cell.cellObservation \busy -> pure (True, not busy))
        (\claimed -> if claimed then modifyMVar_ cell.cellObservation (const (pure False)) else pure ())
        \claimed -> if claimed then action else pure (Left (CodeModeBusyObserver cell.cellIdentifier))

readRunningCodeCells :: CodeModeHost -> IO [(Text, UTCTime)]
readRunningCodeCells host = withMVar host.hostState \state -> do
    running <- atomically $ filterM (fmap isNothing . tryReadTMVar . (.cellResult))
        (Map.elems state.hostCells)
    pure [(cell.cellIdentifier, cell.cellStartedAt) | cell <- running]

lookupCell :: CodeModeHost -> Text -> IO (Maybe Cell)
lookupCell host identifier = withMVar host.hostState (pure . Map.lookup identifier . (.hostCells))

removeCell :: CodeModeHost -> Cell -> IO Bool
removeCell host cell = modifyMVar host.hostState \state ->
    pure (state { hostCells = Map.delete cell.cellIdentifier state.hostCells }
         , Map.member cell.cellIdentifier state.hostCells)

abortCell :: CodeModeHost -> Cell -> IO ()
abortCell host cell = bracket (removeCell host cell)
    (\owned -> if owned then stopCell cell else pure ())
    (const (pure ()))

releaseCell :: CodeModeHost -> Cell -> IO ()
releaseCell = abortCell

stopCell :: Cell -> IO ()
stopCell cell = do
    killWorker cell.cellWorker
    cancel cell.cellMonitor
    void (waitCatch cell.cellMonitor)
    cancelCallbacks cell
    stopWorker cell.cellWorker

cancelCallbacks :: Cell -> IO ()
cancelCallbacks cell = do
    callbacks <- modifyMVar cell.cellCallbacks (\current -> pure ([], current))
    mapConcurrently_ cancel callbacks

drainQueue :: TQueue a -> STM [a]
drainQueue queue = go []
  where
    go values = tryReadTQueue queue >>= \case
        Nothing -> pure (reverse values)
        Just value -> go (value : values)

contentResult :: [Value] -> Value
contentResult values = object ["content" .= values]

-- Generic content-envelope plumbing, not domain-specific JSON decoding.
resultContent :: Value -> [Value]
resultContent (Object value) | Just (Array items) <- KeyMap.lookup "content" value = Vector.toList items
resultContent value = [value]

preferStreamed :: [Value] -> Value -> Value
preferStreamed [] fallback = fallback
preferStreamed values _ = contentResult values

outcomeResult :: Text -> CellOutcome -> CodeModeResult
outcomeResult identifier = \case
    CellSucceeded value -> CodeModeFinished identifier value
    CellFailed value err -> CodeModeFailed identifier value err
