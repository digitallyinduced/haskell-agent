module Agent.Runtime.Daemon.Journal
    ( Journal
    , JournalError (..)
    , JournalConfig (..)
    , JournalSnapshot (..)
    , Replay (..)
    , defaultJournalConfig
    , openJournal
    , appendEvent
    , persistTask
    , snapshot
    , replayAfter
    , subscribeReplay
    , redactValue
    , boundTaskLog
    ) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception.Safe
    ( Exception
    , IOException
    , catch
    , finally
    , onException
    , throwIO
    , tryIO
    )
import Control.Monad (foldM, forM_, unless, when)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import Data.Word (Word64)
import GHC.Generics (Generic)
import System.Directory hiding (isSymbolicLink)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError, isEOFError)
import System.Posix.Files
    ( fileOwner
    , fileSize
    , getFdStatus
    , getSymbolicLinkStatus
    , isDirectory
    , isRegularFile
    , isSymbolicLink
    , setFdMode
    )
import System.Posix.User (getEffectiveUserID)
import System.Posix.IO
    ( OpenFileFlags (..)
    , OpenMode (ReadOnly, WriteOnly)
    , closeFd
    , defaultFileFlags
    , openFd
    )
import System.Posix.IO.ByteString (fdRead, fdWrite)
import System.Posix.Types (Fd)
import System.Posix.Unistd (fileSynchronise)

import Agent.Runtime.Daemon.Protocol
import Agent.Runtime.Daemon.Task

data JournalConfig = JournalConfig
    { directory :: FilePath
    , maximumEvents :: Int
    , maximumJournalBytes :: Int
    , maximumTaskLogLines :: Int
    , maximumTaskLogCharacters :: Int
    , maximumTaskDescriptionCharacters :: Int
    , maximumTasks :: Int
    , subscriberQueueSize :: Int
    , maximumSnapshotBytes :: Int
    , maximumRecoveryJournalBytes :: Int
    }
    deriving stock (Eq, Show)

defaultJournalConfig :: FilePath -> JournalConfig
defaultJournalConfig directory =
    JournalConfig
        { directory
        , maximumEvents = 10_000
        , maximumJournalBytes = 16 * 1_048_576
        , maximumTaskLogLines = 500
        , maximumTaskLogCharacters = 256_000
        , maximumTaskDescriptionCharacters = 8_192
        , maximumTasks = 1_000
        , subscriberQueueSize = 256
        , maximumSnapshotBytes = 64 * 1_048_576
        , maximumRecoveryJournalBytes = 64 * 1_048_576
        }

data JournalError
    = JournalCorrupt FilePath String
    | JournalEventGap Sequence Sequence
    | JournalEventTooLarge Int Int
    | JournalTaskCapacityExceeded Int
    | JournalFileTooLarge FilePath Integer Integer
    | JournalPoisoned String
    | JournalInsecurePath FilePath
    | JournalSequenceExhausted Sequence
    deriving stock (Eq, Show)

instance Exception JournalError

data JournalSnapshot = JournalSnapshot
    { lastSequence :: Sequence
    , tasks :: Map TaskId DurableTask
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON JournalSnapshot
instance FromJSON JournalSnapshot

data Replay
    = ReplayEvents [EventEnvelope]
    | ReplaySnapshot JournalSnapshot
    deriving stock (Eq, Show)

data JournalState = JournalState
    { durableSnapshot :: JournalSnapshot
    , retainedEvents :: Seq EventEnvelope
    }

data Journal = Journal
    { config :: JournalConfig
    , state :: MVar JournalState
    , subscribers :: TVar (Map Int Subscription)
    , nextSubscriber :: TVar Int
    , poisoned :: TVar (Maybe String)
    }

data Subscription = Subscription
    { events :: TBQueue EventEnvelope
    , overflowed :: TVar Bool
    }

openJournal :: JournalConfig -> IO Journal
openJournal config = do
    createDirectoryIfMissing True config.directory
    verifyPrivateDirectory config.directory
    (snapshotExists, savedSnapshot) <- readSnapshot config
    savedEvents <- readEvents config
    validateEventSequence savedEvents
    validateRecoveryOrigin snapshotExists savedSnapshot.lastSequence savedEvents
    recoveredSnapshot <- foldM (recoverTask config) savedSnapshot (toList savedEvents)
    recoveredTasks <- enforceTaskCapacity config recoveredSnapshot.tasks
    let boundedSnapshot = recoveredSnapshot {tasks = recoveredTasks}
        lastEventSequence =
            maybe 0 (.sequenceNumber) (Seq.lookup (Seq.length savedEvents - 1) savedEvents)
        nextSequence = max boundedSnapshot.lastSequence lastEventSequence
        initialState =
            JournalState
                { durableSnapshot = boundedSnapshot {lastSequence = nextSequence}
                , retainedEvents = savedEvents
                }
    state <- newMVar initialState
    subscribers <- newTVarIO Map.empty
    nextSubscriber <- newTVarIO 0
    poisoned <- newTVarIO Nothing
    let journal = Journal {config, state, subscribers, nextSubscriber, poisoned}
    _ <- modifyMVar state $ \journalState -> do
        compacted <- enforceRetention config journalState
        pure (compacted, ())
    interruptTasksFromPreviousProcess journal
    pure journal

recoverTask :: JournalConfig -> JournalSnapshot -> EventEnvelope -> IO JournalSnapshot
recoverTask config saved event
    | event.sequenceNumber <= saved.lastSequence = pure saved
    | event.eventType /= "task_changed" = pure saved
    | otherwise =
        case fromJSON event.payload of
            Error message -> throwIO (JournalCorrupt "events.jsonl" message)
            Success task -> do
                let boundedTask = boundTaskLog config task
                boundedTasks <-
                    enforceTaskCapacity config $
                        Map.insert boundedTask.taskId boundedTask saved.tasks
                pure saved
                    { lastSequence = event.sequenceNumber
                    , tasks = boundedTasks
                    }

synchronisePath :: FilePath -> IO ()
synchronisePath path =
    bracketFd
        (openFd path ReadOnly defaultFileFlags {nofollow = True, cloexec = True, directory = True})
        fileSynchronise
  where
    bracketFd acquire use = do
        descriptor <- acquire
        _ <- use descriptor `onException` closeFd descriptor
        closeFd descriptor

appendEvent :: Journal -> Text -> Value -> IO EventEnvelope
appendEvent journal eventType payload =
    modifyMVar journal.state $ \journalState -> do
        ensureHealthy journal
        nextSequence <- nextSequenceAfter journalState.durableSnapshot.lastSequence
        let
            event =
                EventEnvelope
                    { sequenceNumber = nextSequence
                    , eventType
                    , payload = redactValue payload
                    }
            updated =
                journalState
                    { durableSnapshot =
                        journalState.durableSnapshot {lastSequence = nextSequence}
                    , retainedEvents = journalState.retainedEvents Seq.|> event
                    }
        ensureEventFits journal.config event
        compacted <- persistOrPoison journal $ do
            appendEventFile journal.config event
            enforceRetention journal.config updated
        publish journal event
        pure (compacted, event)

persistTask :: Journal -> DurableTask -> IO EventEnvelope
persistTask journal task =
    modifyMVar journal.state $ \journalState -> do
        ensureHealthy journal
        let boundedTask = boundTaskLog journal.config task
        boundedTasks <-
            enforceTaskCapacity journal.config $
                Map.insert boundedTask.taskId boundedTask journalState.durableSnapshot.tasks
        nextSequence <- nextSequenceAfter journalState.durableSnapshot.lastSequence
        let
            event =
                EventEnvelope
                    { sequenceNumber = nextSequence
                    , eventType = "task_changed"
                    , payload = toJSON boundedTask
                    }
            updatedSnapshot =
                JournalSnapshot
                    { lastSequence = nextSequence
                    , tasks = boundedTasks
                    }
            updated =
                JournalState
                    { durableSnapshot = updatedSnapshot
                    , retainedEvents = journalState.retainedEvents Seq.|> event
                    }
        ensureEventFits journal.config event
        ensureSnapshotFits journal.config updatedSnapshot
        compacted <- persistOrPoison journal $ do
            appendEventFile journal.config event
            writeSnapshot journal.config updatedSnapshot
            enforceRetention journal.config updated
        publish journal event
        pure (compacted, event)

snapshot :: Journal -> IO JournalSnapshot
snapshot journal =
    withMVar journal.state $ \journalState -> do
        ensureHealthy journal
        pure journalState.durableSnapshot

replayAfter :: Journal -> Sequence -> IO Replay
replayAfter journal cursor =
    withMVar journal.state $ \journalState -> do
        ensureHealthy journal
        pure (replayFromState cursor journalState)

replayFromState :: Sequence -> JournalState -> Replay
replayFromState cursor journalState =
    let events = toList journalState.retainedEvents
        earliest = fmap (.sequenceNumber) (safeHead events)
        replayGap =
            case earliest of
                Nothing -> cursor < journalState.durableSnapshot.lastSequence
                Just sequenceNumber -> hasGapAfter cursor sequenceNumber
     in
        if cursor > journalState.durableSnapshot.lastSequence || replayGap
            then ReplaySnapshot journalState.durableSnapshot
            else ReplayEvents (filter ((> cursor) . (.sequenceNumber)) events)
  where
    hasGapAfter (Sequence cursorValue) (Sequence earliestValue) =
        earliestValue > cursorValue && earliestValue - cursorValue > 1

-- | Atomically with respect to appends, capture handshake state, replay, and
-- a subscription to subsequent events.
subscribeReplay ::
    Journal ->
    Sequence ->
    IO (JournalSnapshot, Replay, TBQueue EventEnvelope, TVar Bool, IO ())
subscribeReplay journal cursor =
    withMVar journal.state $ \journalState -> do
        ensureHealthy journal
        (subscriberId, subscription) <-
            atomically $ do
                subscriberId <- readTVar journal.nextSubscriber
                writeTVar journal.nextSubscriber (subscriberId + 1)
                events <- newTBQueue (fromIntegral (max 1 journal.config.subscriberQueueSize))
                overflowed <- newTVar False
                let subscription = Subscription {events, overflowed}
                modifyTVar' journal.subscribers (Map.insert subscriberId subscription)
                pure (subscriberId, subscription)
        let unsubscribe = atomically (modifyTVar' journal.subscribers (Map.delete subscriberId))
        pure
            ( journalState.durableSnapshot
            , replayFromState cursor journalState
            , subscription.events
            , subscription.overflowed
            , unsubscribe
            )

ensureHealthy :: Journal -> IO ()
ensureHealthy journal =
    readTVarIO journal.poisoned >>= \case
        Nothing -> pure ()
        Just message -> throwIO (JournalPoisoned message)

persistOrPoison :: Journal -> IO value -> IO value
persistOrPoison journal action =
    action `onException`
        atomically (writeTVar journal.poisoned (Just "persistence operation failed"))

publish :: Journal -> EventEnvelope -> IO ()
publish journal event =
    atomically $ do
        active <- readTVar journal.subscribers
        forM_ active $ \subscription -> do
            full <- isFullTBQueue subscription.events
            if full
                then writeTVar subscription.overflowed True
                else writeTBQueue subscription.events event

ensureEventFits :: JournalConfig -> EventEnvelope -> IO ()
ensureEventFits config event = do
    let actual = BS.length (encodeLine event)
        maximumBytes = config.maximumJournalBytes
    when (maximumBytes <= 0 || actual > maximumBytes) $
        throwIO (JournalEventTooLarge actual maximumBytes)

enforceTaskCapacity :: JournalConfig -> Map TaskId DurableTask -> IO (Map TaskId DurableTask)
enforceTaskCapacity config tasks
    | Map.size tasks <= max 0 config.maximumTasks = pure tasks
    | otherwise = throwIO (JournalTaskCapacityExceeded config.maximumTasks)

validateEventSequence :: Seq EventEnvelope -> IO ()
validateEventSequence events =
    case toList events of
        [] -> pure ()
        first : rest
            | first.sequenceNumber == 0 -> throwIO (JournalEventGap 1 0)
            | otherwise -> go first.sequenceNumber rest
  where
    go _ [] = pure ()
    go previous (event : remaining) = do
        expected <- nextSequenceAfter previous
        if event.sequenceNumber == expected
            then go event.sequenceNumber remaining
            else throwIO (JournalEventGap expected event.sequenceNumber)

validateRecoveryOrigin :: Bool -> Sequence -> Seq EventEnvelope -> IO ()
validateRecoveryOrigin snapshotExists snapshotSequence events =
    case (Seq.lookup 0 events, Seq.lookup (Seq.length events - 1) events) of
        (Just first, Just lastEvent)
            | not snapshotExists && first.sequenceNumber /= 1 ->
                throwIO (JournalEventGap 1 first.sequenceNumber)
            | snapshotExists
            , first.sequenceNumber > snapshotSequence -> do
                expected <- nextSequenceAfter snapshotSequence
                unless (first.sequenceNumber == expected) $
                    throwIO (JournalEventGap expected first.sequenceNumber)
            | snapshotExists
            , lastEvent.sequenceNumber < snapshotSequence ->
                throwIO $
                    JournalCorrupt
                        "events.jsonl"
                        "retained event suffix ends before the snapshot sequence"
        _ -> pure ()

nextSequenceAfter :: Sequence -> IO Sequence
nextSequenceAfter sequenceNumber@(Sequence value)
    | value == (maxBound :: Word64) = throwIO (JournalSequenceExhausted sequenceNumber)
    | otherwise = pure (Sequence (value + 1))

verifyPrivateDirectory :: FilePath -> IO ()
verifyPrivateDirectory path =
    tryIO (openFd path ReadOnly defaultFileFlags {nofollow = True, cloexec = True, directory = True}) >>= \case
        Left (_ :: IOException) -> throwIO (JournalInsecurePath path)
        Right descriptor ->
            ( do
                status <- getFdStatus descriptor
                effectiveUser <- getEffectiveUserID
                unless (isDirectory status && fileOwner status == effectiveUser) $
                    throwIO (JournalInsecurePath path)
                setFdMode descriptor 0o700
            )
                `finally` closeFd descriptor

redactValue :: Value -> Value
redactValue = \case
    Object objectValue ->
        Object $
            KeyMap.mapWithKey
                (\key value ->
                    if isSensitiveKey (Key.toText key)
                        then String "[REDACTED]"
                        else redactValue value
                )
                objectValue
    Array values -> Array (fmap redactValue values)
    other -> other

boundTaskLog :: JournalConfig -> DurableTask -> DurableTask
boundTaskLog config task =
    task
        { description =
            Text.take config.maximumTaskDescriptionCharacters $
                redactLogLine task.description
        , logTail =
            boundCharacters config.maximumTaskLogCharacters
                . takeLast config.maximumTaskLogLines
                . fmap redactLogLine
                $ task.logTail
        }

interruptTasksFromPreviousProcess :: Journal -> IO ()
interruptTasksFromPreviousProcess journal = do
    currentSnapshot <- snapshot journal
    now <- getCurrentTime
    forM_ (filter isActive (Map.elems currentSnapshot.tasks)) $ \task -> do
        _ <- persistTask journal (interruptActive now task)
        pure ()

enforceRetention :: JournalConfig -> JournalState -> IO JournalState
enforceRetention config journalState = do
    journalBytes <- fileSizeOrZero (eventsPath config)
    forM_ (Seq.lookup (Seq.length journalState.retainedEvents - 1) journalState.retainedEvents) $
        ensureEventFits config
    let tooMany = Seq.length journalState.retainedEvents > max 0 config.maximumEvents
        tooLarge = journalBytes > fromIntegral (max 0 config.maximumJournalBytes)
    if tooMany || tooLarge
        then do
            let countBounded =
                    if config.maximumEvents <= 0
                        then Seq.empty
                        else
                            Seq.drop
                                (max 0 (Seq.length journalState.retainedEvents - config.maximumEvents))
                                journalState.retainedEvents
                byteBounded = retainWithinBytes config.maximumJournalBytes countBounded
                compacted = journalState {retainedEvents = byteBounded}
            writeSnapshot config compacted.durableSnapshot
            rewriteEvents config byteBounded
            pure compacted
        else pure journalState

retainWithinBytes :: Int -> Seq EventEnvelope -> Seq EventEnvelope
retainWithinBytes maximumBytes events =
    Seq.fromList . reverse $ go 0 (reverse (toList events))
  where
    go _ [] = []
    go used (event : rest)
        | used + size > max 0 maximumBytes = []
        | otherwise = event : go (used + size) rest
      where
        size = BS.length (encodeLine event)

readSnapshot :: JournalConfig -> IO (Bool, JournalSnapshot)
readSnapshot config = do
    readPrivateFile (snapshotPath config) (fromIntegral config.maximumSnapshotBytes) >>= \case
        Just bytes ->
            case eitherDecodeStrict' bytes of
                Left message -> throwIO (JournalCorrupt (snapshotPath config) message)
                Right saved -> pure (True, saved)
        Nothing -> pure (False, emptySnapshot)
  where
    emptySnapshot = JournalSnapshot {lastSequence = 0, tasks = Map.empty}

readEvents :: JournalConfig -> IO (Seq EventEnvelope)
readEvents config = do
    readPrivateFile (eventsPath config) (fromIntegral config.maximumRecoveryJournalBytes) >>= \case
        Just bytes ->
            traverse decodeLine (zip [1 :: Int ..] (BS8.lines bytes)) >>= pure . Seq.fromList
        Nothing -> pure Seq.empty

decodeLine :: FromJSON value => (Int, BS.ByteString) -> IO value
decodeLine (lineNumber, bytes) =
    case eitherDecodeStrict' bytes of
        Left message -> throwIO (JournalCorrupt "events.jsonl" ("line " <> show lineNumber <> ": " <> message))
        Right value -> pure value

readPrivateFile :: FilePath -> Integer -> IO (Maybe BS.ByteString)
readPrivateFile path maximumBytes =
    tryIO (openFd path ReadOnly defaultFileFlags {nofollow = True, cloexec = True}) >>= \case
        Left exception
            | isDoesNotExistError exception -> pure Nothing
            | otherwise -> throwIO (JournalInsecurePath path)
        Right descriptor ->
            Just
                <$> ( (verifyPrivateDescriptor path descriptor >> setFdMode descriptor 0o600 >> readDescriptorBounded path maximumBytes descriptor)
                        `finally` closeFd descriptor
                    )

readDescriptorBounded :: FilePath -> Integer -> Fd -> IO BS.ByteString
readDescriptorBounded path maximumBytes descriptor = do
    status <- getFdStatus descriptor
    let actual = fromIntegral (fileSize status)
    when (maximumBytes < 0 || actual > maximumBytes) $
        throwIO (JournalFileTooLarge path actual maximumBytes)
    go 0 []
  where
    go used chunks = do
        chunk <- fdRead descriptor 65_536 `catch` \exception ->
            if isEOFError exception then pure BS.empty else throwIO (exception :: IOException)
        if BS.null chunk
            then pure (BS.concat (reverse chunks))
            else do
                let total = used + fromIntegral (BS.length chunk)
                when (maximumBytes < 0 || total > maximumBytes) $
                    throwIO (JournalFileTooLarge path total maximumBytes)
                go total (chunk : chunks)

appendEventFile :: JournalConfig -> EventEnvelope -> IO ()
appendEventFile config event = do
    descriptor <-
        openFd
            (eventsPath config)
            WriteOnly
            defaultFileFlags
                { append = True
                , creat = Just 0o600
                , nofollow = True
                , cloexec = True
                }
    verifyPrivateDescriptor (eventsPath config) descriptor `onException` closeFd descriptor
    setFdMode descriptor 0o600 `onException` closeFd descriptor
    writeDescriptor descriptor (encodeLine event) `onException` closeFd descriptor
    fileSynchronise descriptor `onException` closeFd descriptor
    closeFd descriptor

verifyPrivateDescriptor :: FilePath -> Fd -> IO ()
verifyPrivateDescriptor path descriptor = do
    status <- getFdStatus descriptor
    effectiveUser <- getEffectiveUserID
    unless (isRegularFile status && fileOwner status == effectiveUser) $
        throwIO (JournalInsecurePath path)

rewriteEvents :: JournalConfig -> Seq EventEnvelope -> IO ()
rewriteEvents config events =
    atomicWrite config (eventsPath config) (BS.concat (fmap encodeLine (toList events)))

writeSnapshot :: JournalConfig -> JournalSnapshot -> IO ()
writeSnapshot config savedSnapshot = do
    ensureSnapshotFits config savedSnapshot
    atomicWrite config (snapshotPath config) (LBS.toStrict (encode savedSnapshot))

ensureSnapshotFits :: JournalConfig -> JournalSnapshot -> IO ()
ensureSnapshotFits config savedSnapshot = do
    let actual = fromIntegral (LBS.length (encode savedSnapshot))
        maximumBytes = fromIntegral config.maximumSnapshotBytes
    when (maximumBytes < 0 || actual > maximumBytes) $
        throwIO (JournalFileTooLarge (snapshotPath config) actual maximumBytes)

atomicWrite :: JournalConfig -> FilePath -> BS.ByteString -> IO ()
atomicWrite config destination bytes = do
    let temporary = destination <> ".tmp"
        cleanup = removeFile temporary `catch` \(_ :: IOException) -> pure ()
    verifyTemporaryPath temporary
    (do
            descriptor <-
                openFd
                    temporary
                    WriteOnly
                    defaultFileFlags
                        { creat = Just 0o600
                        , exclusive = True
                        , nofollow = True
                        , cloexec = True
                        }
            setFdMode descriptor 0o600 `onException` closeFd descriptor
            writeDescriptor descriptor bytes `onException` closeFd descriptor
            fileSynchronise descriptor `onException` closeFd descriptor
            closeFd descriptor
            renameFile temporary destination
            synchronisePath config.directory
        )
        `onException` cleanup
    verifyPrivateDirectory config.directory

writeDescriptor :: Fd -> BS.ByteString -> IO ()
writeDescriptor _ bytes | BS.null bytes = pure ()
writeDescriptor descriptor bytes = do
    written <- fromIntegral <$> fdWrite descriptor bytes
    when (written <= 0) (ioError (userError "short journal write"))
    writeDescriptor descriptor (BS.drop written bytes)

verifyTemporaryPath :: FilePath -> IO ()
verifyTemporaryPath path = do
    tryIO (getSymbolicLinkStatus path) >>= \case
        Left exception
            | isDoesNotExistError exception -> pure ()
            | otherwise -> throwIO exception
        Right status -> do
            effectiveUser <- getEffectiveUserID
            unless (isRegularFile status && not (isSymbolicLink status) && fileOwner status == effectiveUser) $
                throwIO (JournalInsecurePath path)
            removeFile path

fileSizeOrZero :: FilePath -> IO Integer
fileSizeOrZero path =
    catch (getFileSize path) (\(_ :: IOException) -> pure 0)

snapshotPath, eventsPath :: JournalConfig -> FilePath
snapshotPath config = config.directory </> "snapshot.json"
eventsPath config = config.directory </> "events.jsonl"

encodeLine :: ToJSON value => value -> BS.ByteString
encodeLine value = LBS.toStrict (encode value) <> "\n"

safeHead :: [value] -> Maybe value
safeHead = \case
    value : _ -> Just value
    [] -> Nothing

takeLast :: Int -> [value] -> [value]
takeLast count values = drop (max 0 (length values - max 0 count)) values

boundCharacters :: Int -> [Text] -> [Text]
boundCharacters limit = reverse . go 0 . reverse
  where
    go _ [] = []
    go used (line : rest)
        | used >= max 0 limit = []
        | otherwise =
            let remaining = max 0 limit - used
                bounded = Text.take remaining line
             in bounded : go (used + Text.length bounded) rest

redactLogLine :: Text -> Text
redactLogLine line
    | any (`Text.isInfixOf` Text.toLower line) sensitiveFragments = "[REDACTED]"
    | otherwise = line
  where
    sensitiveFragments = ["authorization:", "api_key=", "apikey=", "password=", "token=", "secret="]

isSensitiveKey :: Text -> Bool
isSensitiveKey key =
    let lowered = fmap toLower (Text.unpack key)
     in any (`isInfixOf` lowered) ["authorization", "password", "secret", "token", "api_key", "apikey"]
