module Agent.Tools.FileSystem.ReadFileSpeculation
    ( ReadFileSpeculation
    , ReadFileSpeculationMetrics(..)
    , newReadFileSpeculation
    , closeReadFileSpeculation
    , resetReadFileSpeculation
    , retainFinalReadFileCalls
    , observeReadFileStreamEvent
    , takeSpeculatedRead
    , waitForReadFileSpeculation
    , readReadFileSpeculationMetrics
    ) where

import Agent.OsPath (fromText, unsafeToFilePath)
import Agent.Responses.Types
import Agent.ToolDispatch
    ( ToolCall(..)
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
    ( SomeException
    , mask
    , onException
    , tryAny
    )
import Control.Monad (forM_, guard, void)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Char (isSpace)
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    )
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import qualified Data.Set as Set
import qualified Data.Scientific as Scientific
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

-- | A session-scoped cache for invisible, best-effort @read_file@ prefetches.
--
-- The WebSocket backend feeds streamed function-call events into this value.
-- The normal tool handler consumes a prefetched snapshot only after the model
-- response has completed and the loop has performed its usual approval and
-- resource scheduling.
data ReadFileSpeculation = ReadFileSpeculation
    { environment :: !ToolEnv
    , state :: !(MVar SpeculationState)
    , metrics :: !(IORef ReadFileSpeculationMetrics)
    }

data SpeculationState = SpeculationState
    { closed :: !Bool
    , workspacePaths :: !(Maybe (Set.Set Text))
    , workspaceIndexTask :: !(Maybe (Async ()))
    , partialCalls :: !(Map.Map StreamCallKey PartialReadCall)
    }

data StreamCallKey
    = StreamCallItem !Text
    | StreamCallOutput !Int
    deriving (Eq, Ord, Show)

data PartialReadCall = PartialReadCall
    { partialItemId :: !(Maybe Text)
    , partialOutputIndex :: !(Maybe Int)
    , partialCallId :: !Text
    , partialArguments :: !Text
    , partialCandidate :: !(Maybe ReadCandidate)
    }

data ReadCandidate = ReadCandidate
    { candidateArguments :: !ReadFileArgs
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
        , partialCalls = Map.empty
        }
    metrics <- newIORef emptyMetrics
    let speculation = ReadFileSpeculation { environment, state, metrics }
    -- Build the filename index during session startup so a first read_file
    -- call can be predicted from its earliest useful path prefix.
    startWorkspaceIndex speculation
    pure speculation

-- | Cancel and join every worker owned by this speculation cache.
closeReadFileSpeculation :: ReadFileSpeculation -> IO ()
closeReadFileSpeculation speculation = do
    (indexTask, candidateTasks) <- modifyMVar speculation.state \current ->
        pure
            ( current
                { closed = True
                , workspaceIndexTask = Nothing
                , partialCalls = Map.empty
                }
            , ( current.workspaceIndexTask
              , map (.candidateTask) (activeCandidates current)
              )
            )
    forM_ indexTask cancelAndJoin
    cancelAndJoinAll candidateTasks

-- | Clear per-response predictions while retaining the workspace path index.
resetReadFileSpeculation :: ReadFileSpeculation -> IO ()
resetReadFileSpeculation speculation = do
    tasks <- modifyMVar speculation.state \current ->
        pure
            ( current { partialCalls = Map.empty }
            , map (.candidateTask) (activeCandidates current)
            )
    unlessNull tasks do
        modifyMetrics speculation \current ->
            current
                { speculativeReadsCancelled =
                    current.speculativeReadsCancelled + length tasks
                }
        cancelAndJoinAll tasks

-- | Observe one Responses stream event. This function is deliberately
-- best-effort: speculation failures must never fail the model response.
observeReadFileStreamEvent
    :: ReadFileSpeculation
    -> ResponseStreamEvent
    -> IO ()
observeReadFileStreamEvent speculation event = do
    _ <- tryAny (observe event) :: IO (Either SomeException ())
    pure ()
  where
    observe = \case
        ResponseCreatedEvent{} ->
            resetReadFileSpeculation speculation
        ResponseOutputItemAddedEvent
            { item = FunctionCallItem call, outputIndex } ->
                observeFunctionCall speculation outputIndex call
        ResponseOutputItemDoneEvent
            { item = FunctionCallItem call, outputIndex } ->
                observeFunctionCall speculation outputIndex call
        OtherResponseStreamEvent
            { otherEventType = EventFunctionCallArgumentsDelta
            , eventExtraFields
            } ->
                case
                    ( textField "item_id" eventExtraFields
                    , intField "output_index" eventExtraFields
                    , textField "delta" eventExtraFields
                    )
                of
                    (itemId, outputIndex, Just delta) ->
                        appendArguments speculation itemId outputIndex delta
                    _ -> pure ()
        OtherResponseStreamEvent
            { otherEventType = EventFunctionCallArgumentsDone
            , eventExtraFields
            } ->
                case
                    ( textField "item_id" eventExtraFields
                    , intField "output_index" eventExtraFields
                    , textField "name" eventExtraFields
                    , textField "arguments" eventExtraFields
                    )
                of
                    (itemId, outputIndex, name, Just arguments) ->
                        setArguments
                            speculation
                            itemId
                            outputIndex
                            name
                            arguments
                    _ -> pure ()
        _ -> pure ()

observeFunctionCall
    :: ReadFileSpeculation
    -> Maybe Int
    -> FunctionCall
    -> IO ()
observeFunctionCall speculation outputIndex call
    | canonicalToolName call.name /= "read_file" = pure ()
    | otherwise = forM_ (primaryCallKey call.itemId outputIndex) \callKey -> do
        startWorkspaceIndex speculation
        retired <- modifyMVar speculation.state \current ->
            if current.closed
                then pure (current, [])
                else do
                    let priorEntry =
                            lookupPartialCall
                                call.itemId outputIndex current.partialCalls
                        prior = snd <$> priorEntry
                        partial = PartialReadCall
                            { partialItemId =
                                call.itemId
                                    <|> (prior >>= (.partialItemId))
                            , partialOutputIndex =
                                outputIndex
                                    <|> (prior >>= (.partialOutputIndex))
                            , partialCallId = call.callId
                            , partialArguments =
                                if Text.null call.arguments
                                    then maybe "" (.partialArguments) prior
                                    else call.arguments
                            , partialCandidate =
                                prior >>= (.partialCandidate)
                            }
                        withoutAlias = maybe current.partialCalls
                            (\(priorKey, _) ->
                                Map.delete priorKey current.partialCalls)
                            priorEntry
                        updated = current
                            { partialCalls =
                                Map.insert callKey partial withoutAlias
                            }
                    refreshCallCandidate speculation callKey updated
        cancelRetiredCandidates speculation retired

appendArguments
    :: ReadFileSpeculation
    -> Maybe Text
    -> Maybe Int
    -> Text
    -> IO ()
appendArguments speculation itemId outputIndex delta = do
    retired <- modifyMVar speculation.state \current ->
        case lookupPartialCall itemId outputIndex current.partialCalls of
            Nothing -> pure (current, [])
            Just (callKey, partial)
                | current.closed -> pure (current, [])
                | otherwise ->
                    refreshCallCandidate speculation callKey $
                        current
                            { partialCalls =
                                Map.insert
                                    callKey
                                    partial
                                        { partialItemId =
                                            itemId
                                                <|> partial.partialItemId
                                        , partialOutputIndex =
                                            outputIndex
                                                <|> partial.partialOutputIndex
                                        , partialArguments =
                                            partial.partialArguments <> delta
                                        }
                                    current.partialCalls
                            }
    cancelRetiredCandidates speculation retired

setArguments
    :: ReadFileSpeculation
    -> Maybe Text
    -> Maybe Int
    -> Maybe Text
    -> Text
    -> IO ()
setArguments speculation itemId outputIndex name arguments = do
    let maybeCallKey = primaryCallKey itemId outputIndex
    case maybeCallKey of
        Nothing -> pure ()
        Just callKey -> do
            startWorkspaceIndex speculation
            retired <- modifyMVar speculation.state \current ->
                if current.closed
                    then pure (current, [])
                    else case
                        lookupPartialCall itemId outputIndex current.partialCalls
                    of
                        Just (priorKey, partial) ->
                            refreshCallCandidate speculation callKey $
                                current
                                    { partialCalls =
                                        Map.insert
                                            callKey
                                            partial
                                                { partialItemId =
                                                    itemId
                                                        <|> partial.partialItemId
                                                , partialOutputIndex =
                                                    outputIndex
                                                        <|> partial.partialOutputIndex
                                                , partialArguments = arguments
                                                }
                                            (Map.delete
                                                priorKey
                                                current.partialCalls)
                                    }
                        Nothing
                            | maybe False
                                ((== "read_file") . canonicalToolName)
                                name ->
                                    refreshCallCandidate speculation callKey $
                                        current
                                            { partialCalls =
                                                Map.insert
                                                    callKey
                                                    PartialReadCall
                                                        { partialItemId = itemId
                                                        , partialOutputIndex =
                                                            outputIndex
                                                        , partialCallId = ""
                                                        , partialArguments =
                                                            arguments
                                                        , partialCandidate =
                                                            Nothing
                                                        }
                                                    current.partialCalls
                                            }
                            | otherwise -> pure (current, [])
            cancelRetiredCandidates speculation retired

refreshCallCandidate
    :: ReadFileSpeculation
    -> StreamCallKey
    -> SpeculationState
    -> IO (SpeculationState, [Async (Maybe PrefetchedRead)])
refreshCallCandidate speculation callKey current =
    case Map.lookup callKey current.partialCalls of
        Nothing -> pure (current, [])
        Just partial -> do
            let progress = targetFileProgress partial.partialArguments
            let desired = desiredCandidate current partial progress
            case (partial.partialCandidate, desired) of
                (Just existing, Just (arguments, _))
                    -- The prefetch stores the complete raw file contents.
                    -- Later offset/limit fields only change formatting, so
                    -- keep the existing read whenever the target is stable.
                    | existing.candidateArguments.targetFile
                        == arguments.targetFile ->
                        pure (current, [])
                (Just existing, Nothing)
                    | candidateStillMatches
                        progress
                        existing.candidateArguments.targetFile ->
                        pure (current, [])
                (existing, next) -> do
                    let atCapacity =
                            isNothing existing
                                && length (activeCandidates current)
                                    >= maximumConcurrentSpeculativeReads
                    nextCandidate <-
                        if atCapacity
                            then pure Nothing
                            else traverse
                                (\(arguments, kind) -> do
                                    worker <-
                                        startPrefetch speculation arguments
                                    modifyMetrics
                                        speculation
                                        (recordStart kind)
                                    pure ReadCandidate
                                        { candidateArguments = arguments
                                        , candidateTask = worker
                                        })
                                next
                    pure
                        ( current
                            { partialCalls =
                                Map.insert
                                    callKey
                                    partial
                                        { partialCandidate = nextCandidate }
                                    current.partialCalls
                            }
                        , maybe
                            []
                            (pure . (.candidateTask))
                            existing
                        )

desiredCandidate
    :: SpeculationState
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
desiredCandidate current partial
        (Just progress@(TargetFilePrefix prefix))
    | Text.length prefix < minimumPredictionPrefix = Nothing
    | candidateStillMatches (Just progress)
        (maybe
            ""
            ((.targetFile) . (.candidateArguments))
            partial.partialCandidate) =
            (\candidate ->
                (candidate.candidateArguments, PrefixPrediction))
                <$> partial.partialCandidate
    | otherwise = case current.workspacePaths of
        Just paths ->
            fmap
                (\target ->
                    (defaultReadFileArgs target, PrefixPrediction))
                (uniqueWorkspaceCandidate prefix paths)
        Nothing -> Nothing

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
installWorkspaceIndex speculation paths = do
    retired <- modifyMVar speculation.state \current ->
        if current.closed
            then pure (current, [])
            else do
                let indexed = current
                        { workspacePaths = Just paths
                        , workspaceIndexTask = Nothing
                        }
                foldRefresh indexed [] (Map.keys indexed.partialCalls)
    cancelRetiredCandidates speculation retired
  where
    foldRefresh current retired [] = pure (current, retired)
    foldRefresh current retired (itemId : rest) =
        refreshCallCandidate speculation itemId current
            >>= \(next, newlyRetired) ->
                foldRefresh next (retired <> newlyRetired) rest

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

startPrefetch
    :: ReadFileSpeculation
    -> ReadFileArgs
    -> IO (Async (Maybe PrefetchedRead))
startPrefetch speculation arguments =
    asyncWithUnmask \restore ->
        restore (prefetchRead speculation.environment arguments)

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

-- | Drop predictions that do not correspond to finalized @read_file@ calls.
--
-- Matching predictions remain available for normal post-response approval,
-- scheduling, and dispatch. Everything else is cancelled so an abandoned
-- partial call cannot retain a prefetched file while the session is idle.
retainFinalReadFileCalls
    :: ReadFileSpeculation
    -> [ResponseItem]
    -> IO ()
retainFinalReadFileCalls speculation outputItems = do
    let finalCallIds = Set.fromList
            [ call.callId
            | FunctionCallItem call <- outputItems
            , canonicalToolName call.name == "read_file"
            ]
    tasks <- modifyMVar speculation.state \current ->
        let (retained, removed) =
                Map.partition
                    (\partial ->
                        Set.member partial.partialCallId finalCallIds)
                    current.partialCalls
        in pure
            ( current { partialCalls = retained }
            , map (.candidateTask) (activeCandidatesFor removed)
            )
    recordCancelledTasks speculation tasks
    cancelAndJoinAll tasks

-- | Consume a matching, fresh prefetch for the finalized tool call.
--
-- A miss leaves the normal @read_file@ handler responsible for all work.
takeSpeculatedRead
    :: ReadFileSpeculation
    -> ToolCall
    -> IO (Maybe (Either Text Text))
takeSpeculatedRead speculation call
    | canonicalToolName call.name /= "read_file" = pure Nothing
    | otherwise = mask \restore -> do
        selected <- modifyMVar speculation.state \current ->
            case
                [ (callKey, candidate)
                | (callKey, partial) <- Map.toList current.partialCalls
                , partial.partialCallId == call.callId
                , candidate <- maybe [] pure partial.partialCandidate
                ]
            of
                ((callKey, candidate) : _) ->
                    pure
                        ( current
                            { partialCalls =
                                Map.delete callKey current.partialCalls
                            }
                        , Just candidate
                        )
                [] -> pure (current, Nothing)
        case selected of
            Nothing -> miss speculation >> pure Nothing
            Just candidate ->
                restore (consumeCandidate candidate)
                    `onException` cancelAndJoin candidate.candidateTask
  where
    consumeCandidate candidate =
        case decodeFinalArguments call of
            Left _ -> cancelMiss candidate
            Right finalArguments -> do
                finalResolved <-
                    resolveForRead
                        speculation.environment
                        (fromText finalArguments.targetFile)
                case finalResolved of
                    Left _ -> cancelMiss candidate
                    Right finalPath ->
                        waitAndCheck
                            candidate
                            finalArguments
                            finalPath

    decodeFinalArguments :: ToolCall -> Either Text ReadFileArgs
    decodeFinalArguments finalCall =
        decodeToolArguments
            (toolArgumentsValue finalCall.arguments)

    cancelMiss :: ReadCandidate -> IO (Maybe (Either Text Text))
    cancelMiss candidate = do
        cancelAndJoin candidate.candidateTask
        modifyMetrics speculation \metrics ->
            metrics
                { speculativeReadsCancelled =
                    metrics.speculativeReadsCancelled + 1
                }
        miss speculation
        pure Nothing

    waitAndCheck
        :: ReadCandidate
        -> ReadFileArgs
        -> OsPath
        -> IO (Maybe (Either Text Text))
    waitAndCheck candidate finalArguments finalPath =
        waitCatch candidate.candidateTask >>= \case
            Left _ -> miss speculation >> pure Nothing
            Right Nothing -> miss speculation >> pure Nothing
            Right (Just prefetched)
                | not
                    (equalFilePath
                        finalPath
                        prefetched.prefetchedResolvedPath) ->
                    miss speculation >> pure Nothing
                | otherwise ->
                    fileFingerprint finalPath >>= \case
                        Just current
                            | current == prefetched.prefetchedFingerprint -> do
                                modifyMetrics speculation \metrics ->
                                    metrics
                                        { speculativeReadHits =
                                            metrics.speculativeReadHits + 1
                                        }
                                pure . Just $
                                    if prefetched.prefetchedArguments
                                            == finalArguments
                                        then prefetched.prefetchedOutput
                                        else
                                            formatReadFileContent
                                                prefetched.prefetchedContent
                                                finalArguments
                        _ -> do
                            modifyMetrics speculation \metrics ->
                                metrics
                                    { speculativeReadStale =
                                        metrics.speculativeReadStale + 1
                                    }
                            miss speculation
                            pure Nothing

waitForReadFileSpeculation :: ReadFileSpeculation -> IO ()
waitForReadFileSpeculation speculation = do
    initial <- readMVar speculation.state
    forM_ initial.workspaceIndexTask (void . waitCatch)
    current <- readMVar speculation.state
    forM_
        (activeCandidates current)
        (void . waitCatch . (.candidateTask))

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

textField :: Text -> Aeson.Object -> Maybe Text
textField name object =
    case KeyMap.lookup (Key.fromText name) object of
        Just (Aeson.String value) -> Just value
        _ -> Nothing

intField :: Text -> Aeson.Object -> Maybe Int
intField name object =
    case KeyMap.lookup (Key.fromText name) object of
        Just (Aeson.Number value) -> Scientific.toBoundedInteger value
        _ -> Nothing

primaryCallKey :: Maybe Text -> Maybe Int -> Maybe StreamCallKey
primaryCallKey itemId outputIndex =
    StreamCallItem <$> itemId <|> StreamCallOutput <$> outputIndex

callKeys :: Maybe Text -> Maybe Int -> [StreamCallKey]
callKeys itemId outputIndex =
    maybe [] (pure . StreamCallItem) itemId
        <> maybe [] (pure . StreamCallOutput) outputIndex

lookupPartialCall
    :: Maybe Text
    -> Maybe Int
    -> Map.Map StreamCallKey PartialReadCall
    -> Maybe (StreamCallKey, PartialReadCall)
lookupPartialCall itemId outputIndex calls =
    go (callKeys itemId outputIndex)
        <|> findMatchingAlias (Map.toList calls)
  where
    go [] = Nothing
    go (callKey : rest) =
        case Map.lookup callKey calls of
            Just partial -> Just (callKey, partial)
            Nothing -> go rest

    findMatchingAlias [] = Nothing
    findMatchingAlias ((callKey, partial) : rest)
        | matchesPartialAlias partial = Just (callKey, partial)
        | otherwise = findMatchingAlias rest

    matchesPartialAlias :: PartialReadCall -> Bool
    matchesPartialAlias partial =
        maybe False
            (\value -> partial.partialItemId == Just value)
            itemId
            || maybe False
                (\value -> partial.partialOutputIndex == Just value)
                outputIndex

activeCandidates :: SpeculationState -> [ReadCandidate]
activeCandidates = activeCandidatesFor . (.partialCalls)

activeCandidatesFor
    :: Map.Map StreamCallKey PartialReadCall
    -> [ReadCandidate]
activeCandidatesFor =
    catMaybes . map (.partialCandidate) . Map.elems

recordCancelledTasks :: ReadFileSpeculation -> [Async a] -> IO ()
recordCancelledTasks speculation tasks =
    unlessNull tasks $
        modifyMetrics speculation \current ->
            current
                { speculativeReadsCancelled =
                    current.speculativeReadsCancelled + length tasks
                }

cancelRetiredCandidates
    :: ReadFileSpeculation
    -> [Async (Maybe PrefetchedRead)]
    -> IO ()
cancelRetiredCandidates speculation tasks = do
    recordCancelledTasks speculation tasks
    cancelAndJoinAll tasks

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

miss :: ReadFileSpeculation -> IO ()
miss speculation =
    modifyMetrics speculation \metrics ->
        metrics
            { speculativeReadMisses =
                metrics.speculativeReadMisses + 1
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

unlessNull :: [a] -> IO () -> IO ()
unlessNull values action =
    if null values then pure () else action

minimumPredictionPrefix :: Int
minimumPredictionPrefix = 4

maximumConcurrentSpeculativeReads :: Int
maximumConcurrentSpeculativeReads = 4

maxSpeculativeReadBytes :: Integer
maxSpeculativeReadBytes = 16 * 1024 * 1024
