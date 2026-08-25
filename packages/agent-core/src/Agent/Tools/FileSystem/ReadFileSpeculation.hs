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
    ( PreparedToolResult
    , ToolArgumentInterpreter
    , ToolArgumentInterpreterFactory
    , ToolArgumentStreamItem(..)
    , ToolArgumentUpdate(..)
    , ToolCall(..)
    , canonicalToolName
    , decodeToolArguments
    , toolArgumentsValue
    )
import Agent.Tools.FileSystem (resolveForRead)
import Agent.Tools.FileSystem.ReadFile.Internal
    ( ReadFileArgs(..)
    , formatReadFileContent
    , readFileResolvedContent
    )
import Agent.Tools.Types (ToolEnv(..))
import Control.Applicative ((<|>))
import Control.Concurrent.Async
    ( Async
    , asyncWithUnmask
    , cancel
    , race
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
    ( bracket
    , mask
    , tryAny
    )
import Control.Monad (forM_, guard, void, when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Acquire (mkAcquire)
import Data.Char (isSpace)
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , modifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
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
    { prefetchedArguments :: !ReadFileArgs
    , prefetchedResolvedPath :: !OsPath
    , prefetchedFingerprint :: !FileFingerprint
    , prefetchedContent :: !Text
    , prefetchedOutput :: !(Either Text Text)
    }

data FileFingerprint = FileFingerprint
    { fingerprintDevice :: !Integer
    , fingerprintFile :: !Integer
    , fingerprintSize :: !Integer
    , fingerprintModified :: !Rational
    , fingerprintChanged :: !Rational
    }
    deriving (Eq, Show)

data TargetFileProgress
    = TargetFilePrefix !Text
    | TargetFileComplete !Text
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

readFileArgumentInterpreter :: ToolEnv -> ToolArgumentInterpreterFactory
readFileArgumentInterpreter environment =
    runReadFileArgumentInterpreter
        <$> mkAcquire
            (newReadFileSpeculation environment)
            closeReadFileSpeculation

-- | Attach an already-created cache, primarily for tests and benchmarks.
readFileArgumentInterpreterWithCache
    :: ReadFileSpeculation
    -> ToolArgumentInterpreterFactory
readFileArgumentInterpreterWithCache speculation =
    runReadFileArgumentInterpreter
        <$> mkAcquire
            (pure speculation)
            closeReadFileSpeculation

runReadFileArgumentInterpreter
    :: ReadFileSpeculation
    -> ToolArgumentInterpreter
runReadFileArgumentInterpreter speculation call source
    | canonicalToolName call.name /= "read_file" =
        pure (const (pure Nothing))
    | otherwise = do
        startWorkspaceIndex speculation
        bracket
            (newIORef PartialReadCall
                { partialArguments = call.arguments
                , partialCandidate = Nothing
                })
            (cleanupPartialCall speculation)
            \partialRef -> do
                refreshCallCandidate speculation partialRef
                finalCall <-
                    consumeArgumentStream
                        speculation
                        partialRef
                        source
                updatePartialArguments
                    partialRef
                    (ToolArgumentDoneUpdate finalCall.arguments)
                partial <- readIORef partialRef
                prepared <-
                    prepareReadResult
                        speculation
                        finalCall
                        partial.partialCandidate
                writeIORef
                    partialRef
                    partial { partialCandidate = Nothing }
                pure prepared

consumeArgumentStream
    :: ReadFileSpeculation
    -> IORef PartialReadCall
    -> IO ToolArgumentStreamItem
    -> IO ToolCall
consumeArgumentStream speculation partialRef source =
    awaitArgumentStreamItem speculation partialRef source >>= \case
        ToolArgumentStreamUpdate update -> do
            updatePartialArguments partialRef update
            refreshCallCandidate speculation partialRef
            consumeArgumentStream speculation partialRef source
        ToolArgumentStreamFinal call -> pure call

awaitArgumentStreamItem
    :: ReadFileSpeculation
    -> IORef PartialReadCall
    -> IO ToolArgumentStreamItem
    -> IO ToolArgumentStreamItem
awaitArgumentStreamItem speculation partialRef source = do
    partial <- readIORef partialRef
    pendingIndex <- pendingWorkspaceIndex speculation partial
    case pendingIndex of
        Nothing -> source
        Just indexTask ->
            race (void (waitCatch indexTask)) source >>= \case
                Left () -> do
                    refreshCallCandidate speculation partialRef
                    awaitArgumentStreamItem speculation partialRef source
                Right item -> pure item

pendingWorkspaceIndex
    :: ReadFileSpeculation
    -> PartialReadCall
    -> IO (Maybe (Async ()))
pendingWorkspaceIndex speculation partial
    | not (isNothing partial.partialCandidate) = pure Nothing
    | otherwise =
        case targetFileProgress partial.partialArguments of
            Just (TargetFilePrefix prefix)
                | Text.length prefix >= minimumPredictionPrefix ->
                    (.workspaceIndexTask)
                        <$> readMVar speculation.state
            _ -> pure Nothing

updatePartialArguments
    :: IORef PartialReadCall
    -> ToolArgumentUpdate
    -> IO ()
updatePartialArguments partialRef update =
    modifyIORef' partialRef \partial ->
        partial
            { partialArguments =
                applyArgumentUpdate partial.partialArguments update
            }

applyArgumentUpdate :: Text -> ToolArgumentUpdate -> Text
applyArgumentUpdate current = \case
    ToolArgumentDeltaUpdate delta -> current <> delta
    ToolArgumentDoneUpdate arguments -> arguments

cleanupPartialCall
    :: ReadFileSpeculation
    -> IORef PartialReadCall
    -> IO ()
cleanupPartialCall speculation partialRef =
    readIORef partialRef
        >>= mapM_
            (cancelReadCandidate speculation)
            . (.partialCandidate)

refreshCallCandidate
    :: ReadFileSpeculation
    -> IORef PartialReadCall
    -> IO ()
refreshCallCandidate speculation partialRef = mask \_ -> do
    partial <- readIORef partialRef
    current <- readMVar speculation.state
    let progress = targetFileProgress partial.partialArguments
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
                pure ()
        (Just existing, Nothing)
            | candidateStillMatches
                progress
                existing.candidateArguments.targetFile ->
                pure ()
        (existing, next) -> do
            writeIORef
                partialRef
                partial { partialCandidate = Nothing }
            forM_ existing (cancelReadCandidate speculation)
            nextCandidate <-
                case next of
                    Nothing -> pure Nothing
                    Just (arguments, kind) ->
                        startReadCandidate
                            speculation
                            arguments
                            kind
            writeIORef
                partialRef
                partial { partialCandidate = nextCandidate }

desiredCandidate
    :: Maybe (Set.Set Text)
    -> PartialReadCall
    -> Maybe TargetFileProgress
    -> Maybe (ReadFileArgs, PredictionKind)
desiredCandidate _ _ Nothing = Nothing
desiredCandidate _ partial (Just (TargetFileComplete target))
    | Text.null target = Nothing
    | otherwise =
        Just
            ( fromMaybe
                (defaultReadFileArgs target)
                (decodeReadFileArgs partial.partialArguments)
            , CompletePrediction
            )
desiredCandidate workspacePaths partial
        (Just progress@(TargetFilePrefix prefix))
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

candidateStillMatches :: Maybe TargetFileProgress -> Text -> Bool
candidateStillMatches progress candidateTarget =
    case progress of
        Just (TargetFilePrefix prefix) ->
            not (Text.null candidateTarget)
                && prefix `Text.isPrefixOf` candidateTarget
        Just (TargetFileComplete target) -> target == candidateTarget
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
    | otherwise = resolveForRead environment (fromText target) >>= \case
        Left _ -> pure Nothing
        Right path -> do
            before <- fileFingerprint path
            case before of
                Just fingerprint
                    | fingerprint.fingerprintSize <= maxSpeculativeReadBytes ->
                        readFileResolvedContent path arguments >>= \case
                            Left _ -> pure Nothing
                            Right content -> do
                                forceText content
                                let output =
                                        formatReadFileContent content arguments
                                forceTextResult output
                                after <- fileFingerprint path
                                pure do
                                    guard (after == Just fingerprint)
                                    pure PrefetchedRead
                                        { prefetchedArguments = arguments
                                        , prefetchedResolvedPath = path
                                        , prefetchedFingerprint = fingerprint
                                        , prefetchedContent = content
                                        , prefetchedOutput = output
                                        }
                _ -> pure Nothing
  where
    target = arguments.targetFile

    forceText text = void (evaluate (Text.length text))

    forceTextResult = \case
        Left err -> forceText err
        Right output -> forceText output

prepareReadResult
    :: ReadFileSpeculation
    -> ToolCall
    -> Maybe ReadCandidate
    -> IO PreparedToolResult
prepareReadResult speculation finalCall candidate
    | canonicalToolName finalCall.name /= "read_file" = do
        forM_ candidate (cancelReadCandidate speculation)
        pure (missPreparedResult speculation)
    | otherwise =
        case candidate of
            Nothing -> pure (missPreparedResult speculation)
            Just selected ->
                prepareCandidate selected
  where
    prepareCandidate :: ReadCandidate -> IO PreparedToolResult
    prepareCandidate selected =
        case decodeFinalArguments finalCall of
            Left _ -> cancelMiss selected
            Right finalArguments ->
                resolveForRead
                    speculation.environment
                    (fromText finalArguments.targetFile) >>= \case
                        Left _ -> cancelMiss selected
                        Right finalPath ->
                            waitCatch selected.candidateTask >>= \case
                                Left _ -> do
                                    releaseCandidate selected
                                    pure (missPreparedResult speculation)
                                Right Nothing -> do
                                    releaseCandidate selected
                                    pure (missPreparedResult speculation)
                                Right (Just prefetched)
                                    | equalFilePath
                                        finalPath
                                        prefetched.prefetchedResolvedPath -> do
                                            releaseCandidate selected
                                            pure $
                                                consumePrefetchedRead
                                                    speculation
                                                    prefetched
                                    | otherwise ->
                                        cancelMiss selected

    cancelMiss :: ReadCandidate -> IO PreparedToolResult
    cancelMiss selected = do
        cancelReadCandidate speculation selected
        pure (missPreparedResult speculation)

    releaseCandidate :: ReadCandidate -> IO ()
    releaseCandidate selected =
        void $
            releaseReadTask
                speculation
                selected.candidateTaskKey

consumePrefetchedRead
    :: ReadFileSpeculation
    -> PrefetchedRead
    -> PreparedToolResult
consumePrefetchedRead speculation prefetched call
    | canonicalToolName call.name /= "read_file" =
        missPreparedResult speculation call
    | otherwise =
        case decodeFinalArguments call of
            Left _ -> missPreparedResult speculation call
            Right finalArguments ->
                resolveForRead
                    speculation.environment
                    (fromText finalArguments.targetFile) >>= \case
                        Left _ -> missPreparedResult speculation call
                        Right finalPath
                            | not
                                (equalFilePath
                                    finalPath
                                    prefetched.prefetchedResolvedPath) ->
                                missPreparedResult speculation call
                            | otherwise ->
                                fileFingerprint finalPath >>= \case
                                    Just current
                                        | current
                                            == prefetched.prefetchedFingerprint -> do
                                                modifyMetrics
                                                    speculation
                                                    \metrics ->
                                                        metrics
                                                            { speculativeReadHits =
                                                                metrics.speculativeReadHits
                                                                    + 1
                                                            }
                                                pure . Just $
                                                    if
                                                        prefetched.prefetchedArguments
                                                            == finalArguments
                                                        then
                                                            prefetched.prefetchedOutput
                                                        else
                                                            formatReadFileContent
                                                                prefetched.prefetchedContent
                                                                finalArguments
                                    _ -> do
                                        modifyMetrics speculation \metrics ->
                                            metrics
                                                { speculativeReadStale =
                                                    metrics.speculativeReadStale
                                                        + 1
                                                }
                                        missPreparedResult speculation call

decodeFinalArguments :: ToolCall -> Either Text ReadFileArgs
decodeFinalArguments call =
    decodeToolArguments (toolArgumentsValue call.arguments)

missPreparedResult :: ReadFileSpeculation -> PreparedToolResult
missPreparedResult speculation _ = do
    modifyMetrics speculation \metrics ->
        metrics
            { speculativeReadMisses =
                metrics.speculativeReadMisses + 1
            }
    pure Nothing

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

targetFileProgress :: Text -> Maybe TargetFileProgress
targetFileProgress arguments =
    completeTarget arguments <|> partialTarget arguments
  where
    completeTarget input = do
        Aeson.Object object <-
            Aeson.decodeStrict' (Text.encodeUtf8 input)
        Aeson.String target <-
            KeyMap.lookup (Key.fromText "target_file") object
        pure (TargetFileComplete target)

    partialTarget input =
        findTopLevelStringField "target_file" input

decodeReadFileArgs :: Text -> Maybe ReadFileArgs
decodeReadFileArgs =
    Aeson.decodeStrict' . Text.encodeUtf8

defaultReadFileArgs :: Text -> ReadFileArgs
defaultReadFileArgs targetFile =
    ReadFileArgs
        { targetFile
        , offset = Nothing
        , limit = Nothing
        , pages = Nothing
        , format = Nothing
        }

findTopLevelStringField :: Text -> Text -> Maybe TargetFileProgress
findTopLevelStringField fieldName =
    scan 0 . Text.unpack
  where
    scan :: Int -> String -> Maybe TargetFileProgress
    scan _ [] = Nothing
    scan depth ('{' : rest) = scan (depth + 1) rest
    scan depth ('[' : rest) = scan (depth + 1) rest
    scan depth ('}' : rest) = scan (max 0 (depth - 1)) rest
    scan depth (']' : rest) = scan (max 0 (depth - 1)) rest
    scan depth ('"' : rest) =
        case scanJsonStringToken [] rest of
            Nothing -> Nothing
            Just (JsonStringIncomplete _) -> Nothing
            Just (JsonStringComplete value afterString)
                | depth == 1
                , value == fieldName
                , Just afterColon <- consumeColon afterString ->
                    parseFieldValue afterColon
                | otherwise ->
                    scan depth afterString
    scan depth (_ : rest) = scan depth rest

    consumeColon input =
        case dropWhile isSpace input of
            ':' : rest -> Just (dropWhile isSpace rest)
            _ -> Nothing

    parseFieldValue = \case
        '"' : rest ->
            case scanJsonStringToken [] rest of
                Just (JsonStringIncomplete value) ->
                    Just (TargetFilePrefix value)
                Just (JsonStringComplete value _) ->
                    Just (TargetFileComplete value)
                Nothing -> Nothing
        _ -> Nothing

data JsonStringToken
    = JsonStringIncomplete !Text
    | JsonStringComplete !Text ![Char]

scanJsonStringToken :: [Char] -> [Char] -> Maybe JsonStringToken
scanJsonStringToken reversed = \case
    [] ->
        JsonStringIncomplete <$> decodeJsonString (reverse reversed)
    '"' : rest ->
        (`JsonStringComplete` rest)
            <$> decodeJsonString (reverse reversed)
    '\\' : escaped : rest ->
        scanJsonStringToken (escaped : '\\' : reversed) rest
    ['\\'] -> Nothing
    character : rest ->
        scanJsonStringToken (character : reversed) rest

decodeJsonString :: String -> Maybe Text
decodeJsonString raw =
    Aeson.decodeStrict' $
        Text.encodeUtf8 $
            "\"" <> Text.pack raw <> "\""

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
