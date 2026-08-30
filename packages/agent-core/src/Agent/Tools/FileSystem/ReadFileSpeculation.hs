module Agent.Tools.FileSystem.ReadFileSpeculation
    ( ReadFileSpeculation
    , ReadFileSpeculationMetrics(..)
    , newReadFileSpeculation
    , readFileArgumentInterpreter
    , readFileArgumentInterpreterWithCache
    , closeReadFileSpeculation
    , waitForReadFileSpeculation
    , readReadFileSpeculationMetrics
    ) where

import Agent.OsPath (fromText, unsafeToFilePath)
import Agent.ToolDispatch
    ( StreamedTool(..)
    , StreamedToolFactory
    , ToolInput(..)
    , ToolResult
    )
import Agent.Tools.FileSystem
    ( resolveForRead
    , resolveForReadWithoutAccessRequest
    )
import Agent.Tools.FileSystem.PathPrefix
    ( PathProgress(..)
    , jsonStringFieldProgress
    )
import qualified Agent.Json.Decode as Json
import Agent.Tools.FileSystem.ReadFile.Internal
    ( FileWindow(..)
    , ReadFileArgs(..)
    , readFileArgsDecoder
    , fileWindowCoversArgs
    , formatReadFileContent
    , readFileWindowForArgs
    , runReadFile
    )
import Agent.Tools.Types (ToolEnv(..))
import Control.Concurrent.Async
    ( Async
    , asyncWithUnmask
    , cancel
    , waitCatch
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , modifyMVar_
    , newMVar
    , readMVar
    )
import Control.Exception (evaluate)
import Control.Exception.Safe
    ( mask
    , tryAny
    )
import Control.Monad (forM_, guard, void, when)
import Data.Acquire (mkAcquire)
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    )
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import System.Exit (ExitCode(..))
import System.OsPath (OsPath, equalFilePath, isAbsolute)
import System.Posix.Files
    ( deviceID
    , fileID
    , fileSize
    , getFileStatus
    , isRegularFile
    , modificationTimeHiRes
    , statusChangeTimeHiRes
    )
import System.Process (readProcessWithExitCode)

-- | Session-scoped resources shared by every @read_file@ argument
-- interpreter. Per-call argument and candidate state lives inside the
-- interpreter function rather than in this shared cache.
data ReadFileSpeculation = ReadFileSpeculation
    { environment :: !ToolEnv
    , state :: !(MVar SpeculationState)
    , metrics :: !(IORef ReadFileSpeculationMetrics)
    }

data SpeculationState = SpeculationState
    { closed :: !Bool
    , workspacePaths :: !(Maybe (Set.Set Text))
    , workspaceIndexTask :: !(Maybe (Async ()))
    , nextTaskKey :: !Int
    , activeTasks
        :: !(Map.Map ReadTaskKey (Async (Maybe PrefetchedRead)))
    }

newtype ReadTaskKey = ReadTaskKey Int
    deriving (Eq, Ord, Show)

data PartialReadCall = PartialReadCall
    { partialArguments :: !Text
    , partialCandidate :: !(Maybe ReadCandidate)
    }

data ReadCandidate = ReadCandidate
    { candidateArguments :: !ReadFileArgs
    , candidateTaskKey :: !ReadTaskKey
    , candidateTask :: !(Async (Maybe PrefetchedRead))
    }

data PrefetchedRead = PrefetchedRead
    { prefetchedResolvedPath :: !OsPath
    , prefetchedFingerprint :: !FileFingerprint
    , prefetchedWindow :: !FileWindow
    }

data FileFingerprint = FileFingerprint
    { fingerprintDevice :: !Integer
    , fingerprintFile :: !Integer
    , fingerprintSize :: !Integer
    , fingerprintModified :: !Rational
    , fingerprintChanged :: !Rational
    }
    deriving (Eq, Show)

data PredictionKind
    = PrefixPrediction
    | CompletePrediction
    deriving (Eq, Show)

data ReadFileSpeculationMetrics = ReadFileSpeculationMetrics
    { speculativeReadsStarted :: !Int
    , speculativePrefixPredictions :: !Int
    , speculativeCompletePredictions :: !Int
    , speculativeReadHits :: !Int
    , speculativeReadMisses :: !Int
    , speculativeReadStale :: !Int
    , speculativeReadsCancelled :: !Int
    }
    deriving (Eq, Show)

emptyMetrics :: ReadFileSpeculationMetrics
emptyMetrics = ReadFileSpeculationMetrics
    { speculativeReadsStarted = 0
    , speculativePrefixPredictions = 0
    , speculativeCompletePredictions = 0
    , speculativeReadHits = 0
    , speculativeReadMisses = 0
    , speculativeReadStale = 0
    , speculativeReadsCancelled = 0
    }

newReadFileSpeculation :: ToolEnv -> IO ReadFileSpeculation
newReadFileSpeculation environment = do
    state <- newMVar SpeculationState
        { closed = False
        , workspacePaths = Nothing
        , workspaceIndexTask = Nothing
        , nextTaskKey = 0
        , activeTasks = Map.empty
        }
    metrics <- newIORef emptyMetrics
    let speculation = ReadFileSpeculation { environment, state, metrics }
    startWorkspaceIndex speculation
    pure speculation

-- | Close the shared index and any speculative reads still registered with
-- this session. Normally per-call owner cancellation releases the reads first.
closeReadFileSpeculation :: ReadFileSpeculation -> IO ()
closeReadFileSpeculation speculation = do
    (indexTask, candidateTasks) <-
        modifyMVar speculation.state \current ->
            pure
                ( current
                    { closed = True
                    , workspaceIndexTask = Nothing
                    , activeTasks = Map.empty
                    }
                , (current.workspaceIndexTask, Map.elems current.activeTasks)
                )
    forM_ indexTask cancelAndJoin
    cancelAndJoinAll candidateTasks

readFileArgumentInterpreter :: ToolEnv -> StreamedToolFactory
readFileArgumentInterpreter environment =
    streamedReadFile
        <$> mkAcquire
            (newReadFileSpeculation environment)
            closeReadFileSpeculation

-- | Attach a caller-owned cache, primarily for tests and benchmarks. The
-- factory borrows the cache; its owner remains responsible for closing it.
readFileArgumentInterpreterWithCache
    :: ReadFileSpeculation
    -> StreamedToolFactory
readFileArgumentInterpreterWithCache speculation =
    streamedReadFile
        <$> mkAcquire
            (pure speculation)
            (\_ -> pure ())

streamedReadFile :: ReadFileSpeculation -> StreamedTool
streamedReadFile speculation =
    StreamedTool
        { streamedStart = pure emptyPartialCall
        , streamedInterpret = interpretReadFile speculation
        , streamedConsume = \_call _emit -> consumeReadFile speculation
        , streamedClose = closePartialCall speculation
        }

emptyPartialCall :: PartialReadCall
emptyPartialCall =
    PartialReadCall
        { partialArguments = ""
        , partialCandidate = Nothing
        }

interpretReadFile
    :: ReadFileSpeculation
    -> PartialReadCall
    -> ToolInput
    -> IO (Either (ReadFileArgs, PartialReadCall) PartialReadCall)
interpretReadFile speculation state = \case
    ToolPrefix text ->
        Right <$> refreshAfterArguments speculation state { partialArguments = text }
    ToolDone text -> do
        next <-
            refreshAfterArguments speculation state { partialArguments = text }
        case decodeReadFileArgs next.partialArguments of
            Nothing -> pure (Right next)
            Just args -> pure (Left (args, next))

refreshAfterArguments
    :: ReadFileSpeculation
    -> PartialReadCall
    -> IO PartialReadCall
refreshAfterArguments speculation partial = do
    startWorkspaceIndex speculation
    refreshed <- refreshCallCandidate speculation partial
    pending <- pendingWorkspaceIndex speculation refreshed
    case pending of
        Nothing -> pure refreshed
        Just indexTask -> do
            void (waitCatch indexTask)
            refreshCallCandidate speculation refreshed

closePartialCall :: ReadFileSpeculation -> PartialReadCall -> IO ()
closePartialCall speculation =
    mapM_ (cancelReadCandidate speculation) . (.partialCandidate)

refreshCallCandidate
    :: ReadFileSpeculation
    -> PartialReadCall
    -> IO PartialReadCall
refreshCallCandidate speculation partial = mask \_ -> do
    current <- readMVar speculation.state
    let progress = jsonStringFieldProgress "target_file" partial.partialArguments
        desired =
            desiredCandidate
                current.workspacePaths
                partial
                progress
    case (partial.partialCandidate, desired) of
        (Just existing, Just (arguments, _))
            -- The prefetch stores complete raw file contents. Later
            -- offset/limit fields only change formatting.
            | existing.candidateArguments.targetFile
                == arguments.targetFile ->
                pure partial
        (Just existing, Nothing)
            | candidateStillMatches
                progress
                existing.candidateArguments.targetFile ->
                pure partial
        (existing, next) -> do
            forM_ existing (cancelReadCandidate speculation)
            nextCandidate <-
                case next of
                    Nothing -> pure Nothing
                    Just (arguments, kind) ->
                        startReadCandidate
                            speculation
                            arguments
                            kind
            pure partial { partialCandidate = nextCandidate }

pendingWorkspaceIndex
    :: ReadFileSpeculation
    -> PartialReadCall
    -> IO (Maybe (Async ()))
pendingWorkspaceIndex speculation partial
    | not (isNothing partial.partialCandidate) = pure Nothing
    | otherwise =
        case jsonStringFieldProgress "target_file" partial.partialArguments of
            Just (PathPrefix prefix)
                | Text.length prefix >= minimumPredictionPrefix ->
                    (.workspaceIndexTask)
                        <$> readMVar speculation.state
            _ -> pure Nothing

consumeReadFile
    :: ReadFileSpeculation
    -> ReadFileArgs
    -> PartialReadCall
    -> IO ToolResult
consumeReadFile speculation args partial =
    case partial.partialCandidate of
        Nothing -> do
            recordMiss speculation
            runReadFile speculation.environment args
        Just selected ->
            resolveForRead
                speculation.environment
                (fromText args.targetFile) >>= \case
                    Left _ -> missExecute selected
                    Right finalPath ->
                        waitCatch selected.candidateTask >>= \case
                            Left _ -> do
                                releaseCandidate selected
                                recordMiss speculation
                                runReadFile speculation.environment args
                            Right Nothing -> do
                                releaseCandidate selected
                                recordMiss speculation
                                runReadFile speculation.environment args
                            Right (Just prefetched)
                                | equalFilePath
                                    finalPath
                                    prefetched.prefetchedResolvedPath ->
                                        consumePrefetch selected prefetched
                                | otherwise ->
                                    missExecute selected
  where
    missExecute :: ReadCandidate -> IO ToolResult
    missExecute selected = do
        cancelReadCandidate speculation selected
        recordMiss speculation
        runReadFile speculation.environment args

    releaseCandidate :: ReadCandidate -> IO ()
    releaseCandidate selected =
        void $
            releaseReadTask
                speculation
                selected.candidateTaskKey

    consumePrefetch :: ReadCandidate -> PrefetchedRead -> IO ToolResult
    consumePrefetch selected prefetched = do
        releaseCandidate selected
        resolveForRead
            speculation.environment
            (fromText args.targetFile) >>= \case
                Left _ -> do
                    recordMiss speculation
                    runReadFile speculation.environment args
                Right finalPath
                    | not
                        (equalFilePath
                            finalPath
                            prefetched.prefetchedResolvedPath) -> do
                        recordMiss speculation
                        runReadFile speculation.environment args
                    | otherwise ->
                        fileFingerprint finalPath >>= \case
                            Just current
                                | current == prefetched.prefetchedFingerprint ->
                                    if fileWindowCoversArgs
                                        prefetched.prefetchedWindow
                                        args
                                        then do
                                            modifyMetrics speculation \metrics ->
                                                metrics
                                                    { speculativeReadHits =
                                                        metrics.speculativeReadHits + 1
                                                    }
                                            pure $
                                                formatReadFileContent
                                                    prefetched.prefetchedWindow.fileWindowText
                                                    args
                                        else do
                                            recordMiss speculation
                                            runReadFile speculation.environment args
                            _ -> do
                                modifyMetrics speculation \metrics ->
                                    metrics
                                        { speculativeReadStale =
                                            metrics.speculativeReadStale + 1
                                        }
                                recordMiss speculation
                                runReadFile speculation.environment args

recordMiss :: ReadFileSpeculation -> IO ()
recordMiss speculation =
    modifyMetrics speculation \metrics ->
        metrics
            { speculativeReadMisses =
                metrics.speculativeReadMisses + 1
            }

desiredCandidate
    :: Maybe (Set.Set Text)
    -> PartialReadCall
    -> Maybe PathProgress
    -> Maybe (ReadFileArgs, PredictionKind)
desiredCandidate _ _ Nothing = Nothing
desiredCandidate _ partial (Just (PathComplete target))
    | Text.null target = Nothing
    | otherwise =
        Just
            ( fromMaybe
                (defaultReadFileArgs target)
                (decodeReadFileArgs partial.partialArguments)
            , CompletePrediction
            )
desiredCandidate workspacePaths partial
        (Just progress@(PathPrefix prefix))
    | Text.length prefix < minimumPredictionPrefix = Nothing
    | candidateStillMatches
        (Just progress)
        (maybe
            ""
            ((.targetFile) . (.candidateArguments))
            partial.partialCandidate) =
            (\candidate ->
                (candidate.candidateArguments, PrefixPrediction))
                <$> partial.partialCandidate
    | otherwise =
        workspacePaths >>= \paths ->
            fmap
                (\target ->
                    (defaultReadFileArgs target, PrefixPrediction))
                (uniqueWorkspaceCandidate prefix paths)

candidateStillMatches :: Maybe PathProgress -> Text -> Bool
candidateStillMatches progress candidateTarget =
    case progress of
        Just (PathPrefix prefix) ->
            not (Text.null candidateTarget)
                && prefix `Text.isPrefixOf` candidateTarget
        Just (PathComplete target) -> target == candidateTarget
        Nothing -> False

uniqueWorkspaceCandidate :: Text -> Set.Set Text -> Maybe Text
uniqueWorkspaceCandidate prefix paths
    | isAbsolute (fromText prefix) = Nothing
    | otherwise = do
        candidate <- Set.lookupGE normalizedPrefix paths
        guard (normalizedPrefix `Text.isPrefixOf` candidate)
        case Set.lookupGT candidate paths of
            Just next
                | normalizedPrefix `Text.isPrefixOf` next -> Nothing
            _ -> Just (decorate candidate)
  where
    (normalizedPrefix, decorate)
        | Just rest <- Text.stripPrefix "./" prefix =
            (rest, ("./" <>))
        | otherwise = (prefix, id)

startReadCandidate
    :: ReadFileSpeculation
    -> ReadFileArgs
    -> PredictionKind
    -> IO (Maybe ReadCandidate)
startReadCandidate speculation arguments kind = mask \_ -> do
    candidate <-
        modifyMVar speculation.state \current ->
            if current.closed
                || Map.size current.activeTasks
                    >= maximumConcurrentSpeculativeReads
                then pure (current, Nothing)
                else do
                    let taskKey = ReadTaskKey current.nextTaskKey
                    worker <-
                        asyncWithUnmask \restore ->
                            restore
                                (prefetchRead
                                    speculation.environment
                                    arguments)
                    pure
                        ( current
                            { nextTaskKey = current.nextTaskKey + 1
                            , activeTasks =
                                Map.insert
                                    taskKey
                                    worker
                                    current.activeTasks
                            }
                        , Just ReadCandidate
                            { candidateArguments = arguments
                            , candidateTaskKey = taskKey
                            , candidateTask = worker
                            }
                        )
    forM_ candidate \_ ->
        modifyMetrics speculation (recordStart kind)
    pure candidate

releaseReadTask :: ReadFileSpeculation -> ReadTaskKey -> IO Bool
releaseReadTask speculation taskKey =
    modifyMVar speculation.state \current ->
        let existed = Map.member taskKey current.activeTasks
        in pure
            ( current
                { activeTasks =
                    Map.delete taskKey current.activeTasks
                }
            , existed
            )

cancelReadCandidate
    :: ReadFileSpeculation
    -> ReadCandidate
    -> IO ()
cancelReadCandidate speculation candidate = do
    released <-
        releaseReadTask
            speculation
            candidate.candidateTaskKey
    when released $
        modifyMetrics speculation \current ->
            current
                { speculativeReadsCancelled =
                    current.speculativeReadsCancelled + 1
                }
    cancelAndJoin candidate.candidateTask

startWorkspaceIndex :: ReadFileSpeculation -> IO ()
startWorkspaceIndex speculation =
    modifyMVar_ speculation.state \current ->
        case (current.closed, current.workspacePaths, current.workspaceIndexTask) of
            (False, Nothing, Nothing) -> do
                worker <- asyncWithUnmask \restore ->
                    restore (workspaceFileIndex speculation.environment)
                        >>= installWorkspaceIndex speculation
                pure current { workspaceIndexTask = Just worker }
            _ -> pure current

installWorkspaceIndex :: ReadFileSpeculation -> Set.Set Text -> IO ()
installWorkspaceIndex speculation paths =
    modifyMVar_ speculation.state \current ->
        if current.closed
            then pure current
            else pure current
                { workspacePaths = Just paths
                , workspaceIndexTask = Nothing
                }

workspaceFileIndex :: ToolEnv -> IO (Set.Set Text)
workspaceFileIndex environment = do
    result <- tryAny $
        readProcessWithExitCode
            "git"
            [ "-C"
            , unsafeToFilePath environment.toolCwd
            , "ls-files"
            , "--cached"
            , "--others"
            , "--exclude-standard"
            , "-z"
            ]
            ""
    pure $ Set.fromList $ case result of
        Right (ExitSuccess, output, _) ->
            filter (not . Text.null) (Text.splitOn "\0" (Text.pack output))
        _ -> []

prefetchRead :: ToolEnv -> ReadFileArgs -> IO (Maybe PrefetchedRead)
prefetchRead environment arguments
    | ".pdf" `Text.isSuffixOf` Text.toLower arguments.targetFile =
        pure Nothing
    | otherwise =
        resolveForReadWithoutAccessRequest environment (fromText target) >>= \case
            Left _ -> pure Nothing
            Right path -> do
                before <- fileFingerprint path
                case before of
                    Just fingerprint
                        | fingerprint.fingerprintSize <= maxSpeculativeReadBytes ->
                            readFileWindowForArgs path arguments >>= \case
                                Left _ -> pure Nothing
                                Right window -> do
                                    forceText window.fileWindowText
                                    after <- fileFingerprint path
                                    pure do
                                        guard (after == Just fingerprint)
                                        pure PrefetchedRead
                                            { prefetchedResolvedPath = path
                                            , prefetchedFingerprint = fingerprint
                                            , prefetchedWindow = window
                                            }
                    _ -> pure Nothing
  where
    target = arguments.targetFile

    forceText text = void (evaluate (Text.length text))

waitForReadFileSpeculation :: ReadFileSpeculation -> IO ()
waitForReadFileSpeculation speculation = do
    initial <- readMVar speculation.state
    forM_ initial.workspaceIndexTask (void . waitCatch)
    current <- readMVar speculation.state
    forM_ (Map.elems current.activeTasks) (void . waitCatch)

readReadFileSpeculationMetrics
    :: ReadFileSpeculation
    -> IO ReadFileSpeculationMetrics
readReadFileSpeculationMetrics = readIORef . (.metrics)

fileFingerprint :: OsPath -> IO (Maybe FileFingerprint)
fileFingerprint path = do
    result <- tryAny (getFileStatus (unsafeToFilePath path))
    pure $ case result of
        Right status
            | isRegularFile status -> Just FileFingerprint
                { fingerprintDevice = fromIntegral (deviceID status)
                , fingerprintFile = fromIntegral (fileID status)
                , fingerprintSize = fromIntegral (fileSize status)
                , fingerprintModified = toRational (modificationTimeHiRes status)
                , fingerprintChanged = toRational (statusChangeTimeHiRes status)
                }
        _ -> Nothing


recordStart
    :: PredictionKind
    -> ReadFileSpeculationMetrics
    -> ReadFileSpeculationMetrics
recordStart kind metrics =
    case kind of
        PrefixPrediction ->
            metrics
                { speculativeReadsStarted =
                    metrics.speculativeReadsStarted + 1
                , speculativePrefixPredictions =
                    metrics.speculativePrefixPredictions + 1
                }
        CompletePrediction ->
            metrics
                { speculativeReadsStarted =
                    metrics.speculativeReadsStarted + 1
                , speculativeCompletePredictions =
                    metrics.speculativeCompletePredictions + 1
                }

modifyMetrics
    :: ReadFileSpeculation
    -> (ReadFileSpeculationMetrics -> ReadFileSpeculationMetrics)
    -> IO ()
modifyMetrics speculation update =
    atomicModifyIORef' speculation.metrics \current ->
        (update current, ())

cancelAndJoinAll :: [Async a] -> IO ()
cancelAndJoinAll = mapM_ cancelAndJoin

cancelAndJoin :: Async a -> IO ()
cancelAndJoin worker = do
    cancel worker
    void (waitCatch worker)

minimumPredictionPrefix :: Int
minimumPredictionPrefix = 4

maximumConcurrentSpeculativeReads :: Int
maximumConcurrentSpeculativeReads = 4

maxSpeculativeReadBytes :: Integer
maxSpeculativeReadBytes = 16 * 1024 * 1024

decodeReadFileArgs :: Text -> Maybe ReadFileArgs
decodeReadFileArgs text =
    case Json.decodeText readFileArgsDecoder text of
        Right args -> Just args
        Left _ -> Nothing

defaultReadFileArgs :: Text -> ReadFileArgs
defaultReadFileArgs targetFile =
    ReadFileArgs
        { targetFile
        , offset = Nothing
        , limit = Nothing
        , pages = Nothing
        , format = Nothing
        }
