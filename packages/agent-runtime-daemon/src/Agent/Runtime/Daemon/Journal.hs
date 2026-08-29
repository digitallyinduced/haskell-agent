module Agent.Runtime.Daemon.Journal
    ( Journal
    , JournalConfig (..)
    , JournalSnapshot (..)
    , Replay (..)
    , defaultJournalConfig
    , openJournal
    , appendEvent
    , persistTask
    , snapshot
    , replayAfter
    , subscribe
    , subscribeReplay
    , redactValue
    , boundTaskLog
    ) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception.Safe (IOException, catch, onException)
import Control.Monad (forM_)
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
import GHC.Generics (Generic)
import System.Directory
import System.FilePath ((</>))
import System.IO
import System.Posix.Files (setFileMode)
import System.Posix.IO
    ( OpenMode (ReadOnly)
    , closeFd
    , defaultFileFlags
    , openFd
    )
import System.Posix.Unistd (fileSynchronise)

import Agent.Runtime.Daemon.Protocol
import Agent.Runtime.Daemon.Task

data JournalConfig = JournalConfig
    { directory :: FilePath
    , maximumEvents :: Int
    , maximumJournalBytes :: Int
    , maximumTaskLogLines :: Int
    , maximumTaskLogCharacters :: Int
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
        }

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
    , eventChannel :: TChan EventEnvelope
    }

openJournal :: JournalConfig -> IO Journal
openJournal config = do
    createDirectoryIfMissing True config.directory
    setFileMode config.directory 0o700
    savedSnapshot <- readSnapshot config
    savedEvents <- readEvents config
    let recoveredSnapshot = foldl recoverTask savedSnapshot (toList savedEvents)
        lastEventSequence =
            maybe 0 (.sequenceNumber) (Seq.lookup (Seq.length savedEvents - 1) savedEvents)
        nextSequence = max recoveredSnapshot.lastSequence lastEventSequence
        initialState =
            JournalState
                { durableSnapshot = recoveredSnapshot {lastSequence = nextSequence}
                , retainedEvents = savedEvents
                }
    state <- newMVar initialState
    eventChannel <- newBroadcastTChanIO
    let journal = Journal {config, state, eventChannel}
    interruptTasksFromPreviousProcess journal
    pure journal

recoverTask :: JournalSnapshot -> EventEnvelope -> JournalSnapshot
recoverTask saved event
    | event.sequenceNumber <= saved.lastSequence = saved
    | event.eventType /= "task_changed" = saved
    | otherwise =
        case fromJSON event.payload of
            Error _ -> saved
            Success task ->
                saved
                    { lastSequence = event.sequenceNumber
                    , tasks = Map.insert task.taskId task saved.tasks
                    }

synchronisePath :: FilePath -> IO ()
synchronisePath path =
    bracketFd (openFd path ReadOnly defaultFileFlags) fileSynchronise
  where
    bracketFd acquire use = do
        descriptor <- acquire
        _ <- use descriptor `onException` closeFd descriptor
        closeFd descriptor

appendEvent :: Journal -> Text -> Value -> IO EventEnvelope
appendEvent journal eventType payload =
    modifyMVar journal.state $ \journalState -> do
        let nextSequence = journalState.durableSnapshot.lastSequence + 1
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
        appendEventFile journal.config event
        compacted <- enforceRetention journal.config updated
        atomically (writeTChan journal.eventChannel event)
        pure (compacted, event)

persistTask :: Journal -> DurableTask -> IO EventEnvelope
persistTask journal task =
    modifyMVar journal.state $ \journalState -> do
        let boundedTask = boundTaskLog journal.config task
            nextSequence = journalState.durableSnapshot.lastSequence + 1
            event =
                EventEnvelope
                    { sequenceNumber = nextSequence
                    , eventType = "task_changed"
                    , payload = toJSON boundedTask
                    }
            updatedSnapshot =
                JournalSnapshot
                    { lastSequence = nextSequence
                    , tasks = Map.insert boundedTask.taskId boundedTask journalState.durableSnapshot.tasks
                    }
            updated =
                JournalState
                    { durableSnapshot = updatedSnapshot
                    , retainedEvents = journalState.retainedEvents Seq.|> event
                    }
        appendEventFile journal.config event
        writeSnapshot journal.config updatedSnapshot
        compacted <- enforceRetention journal.config updated
        atomically (writeTChan journal.eventChannel event)
        pure (compacted, event)

snapshot :: Journal -> IO JournalSnapshot
snapshot journal = durableSnapshot <$> readMVar journal.state

replayAfter :: Journal -> Sequence -> IO Replay
replayAfter journal cursor =
    withMVar journal.state (pure . replayFromState cursor)

replayFromState :: Sequence -> JournalState -> Replay
replayFromState cursor journalState =
    let events = toList journalState.retainedEvents
        earliest = maybe (journalState.durableSnapshot.lastSequence + 1) (.sequenceNumber) (safeHead events)
     in
        if cursor > journalState.durableSnapshot.lastSequence || cursor + 1 < earliest
            then ReplaySnapshot journalState.durableSnapshot
            else ReplayEvents (filter ((> cursor) . (.sequenceNumber)) events)

subscribe :: Journal -> IO (TChan EventEnvelope)
subscribe journal = atomically (dupTChan journal.eventChannel)

-- | Atomically with respect to appends, capture handshake state, replay, and
-- a subscription to subsequent events.
subscribeReplay :: Journal -> Sequence -> IO (JournalSnapshot, Replay, TChan EventEnvelope)
subscribeReplay journal cursor =
    withMVar journal.state $ \journalState -> do
        channel <- atomically (dupTChan journal.eventChannel)
        pure
            ( journalState.durableSnapshot
            , replayFromState cursor journalState
            , channel
            )

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
        { logTail =
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
    let tooMany = Seq.length journalState.retainedEvents > max 0 config.maximumEvents
        tooLarge = journalBytes > fromIntegral (max 0 config.maximumJournalBytes)
    if tooMany || tooLarge
        then do
            let countBounded =
                    Seq.drop
                        (max 0 (Seq.length journalState.retainedEvents - max 0 config.maximumEvents))
                        journalState.retainedEvents
                byteBounded = retainWithinBytes config.maximumJournalBytes countBounded
                compacted = journalState {retainedEvents = byteBounded}
            rewriteEvents config byteBounded
            pure compacted
        else pure journalState

retainWithinBytes :: Int -> Seq EventEnvelope -> Seq EventEnvelope
retainWithinBytes maximumBytes events =
    snd $
        foldr
            (\event (used, kept) ->
                let size = BS.length (encodeLine event)
                 in if used + size <= max 0 maximumBytes
                        then (used + size, event Seq.<| kept)
                        else (used, kept)
            )
            (0, Seq.empty)
            events

readSnapshot :: JournalConfig -> IO JournalSnapshot
readSnapshot config =
    catch
        (do
            bytes <- BS.readFile (snapshotPath config)
            case eitherDecodeStrict' bytes of
                Left _ -> pure emptySnapshot
                Right saved -> pure saved
        )
        (\(_ :: IOException) -> pure emptySnapshot)
  where
    emptySnapshot = JournalSnapshot {lastSequence = 0, tasks = Map.empty}

readEvents :: JournalConfig -> IO (Seq EventEnvelope)
readEvents config =
    catch
        (do
            bytes <- BS.readFile (eventsPath config)
            pure . Seq.fromList $ mapMaybeDecode (BS8.lines bytes)
        )
        (\(_ :: IOException) -> pure Seq.empty)

mapMaybeDecode :: FromJSON value => [BS.ByteString] -> [value]
mapMaybeDecode = foldr step []
  where
    step bytes values =
        case eitherDecodeStrict' bytes of
            Left _ -> values
            Right value -> value : values

appendEventFile :: JournalConfig -> EventEnvelope -> IO ()
appendEventFile config event =
    withBinaryFile (eventsPath config) AppendMode $ \handle -> do
        BS.hPut handle (encodeLine event)
        hFlush handle
    >> do
        setFileMode (eventsPath config) 0o600
        synchronisePath (eventsPath config)

rewriteEvents :: JournalConfig -> Seq EventEnvelope -> IO ()
rewriteEvents config events =
    atomicWrite config (eventsPath config) (BS.concat (fmap encodeLine (toList events)))

writeSnapshot :: JournalConfig -> JournalSnapshot -> IO ()
writeSnapshot config savedSnapshot =
    atomicWrite config (snapshotPath config) (LBS.toStrict (encode savedSnapshot))

atomicWrite :: JournalConfig -> FilePath -> BS.ByteString -> IO ()
atomicWrite config destination bytes = do
    let temporary = destination <> ".tmp"
        cleanup = removeFile temporary `catch` \(_ :: IOException) -> pure ()
    (do
            BS.writeFile temporary bytes
            setFileMode temporary 0o600
            synchronisePath temporary
            renameFile temporary destination
            synchronisePath config.directory
        )
        `onException` cleanup
    setFileMode config.directory 0o700

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
