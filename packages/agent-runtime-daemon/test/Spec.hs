module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (cancel, concurrently, waitCatch, withAsync)
import Control.Concurrent.STM (readTVarIO)
import Control.Exception.Safe (bracket, isAsyncException)
import Data.Aeson (Value (..), encode, object, toJSON, (.=))
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import Network.Socket
import qualified Network.Socket.ByteString as Socket
import System.Directory
    ( createDirectory
    , doesFileExist
    , removeFile
    , renamePath
    )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory, withTempDirectory)
import System.Posix.Files
    ( createSymbolicLink
    , fileMode
    , getFileStatus
    )
import System.Timeout (timeout)
import Test.Hspec

import Agent.Runtime.Daemon.Framing
import Agent.Runtime.Daemon.Journal
import Agent.Runtime.Daemon.Protocol
import Agent.Runtime.Daemon.Server
import Agent.Runtime.Daemon.Socket
import Agent.Runtime.Daemon.Supervisor
import Agent.Runtime.Daemon.Task

main :: IO ()
main = hspec $ do
    describe "length-framed JSON IPC" $ do
        it "round trips a typed message over a Unix stream" $
            withSocketPair $ \(writer, reader) -> do
                let expected = ClientAck 42
                (_, actual) <-
                    concurrently
                        (sendJSONFrame defaultMaximumFrameBytes writer expected)
                        (receiveJSONFrame defaultMaximumFrameBytes reader)
                actual `shouldBe` expected

        it "rejects an outbound frame above the configured bound" $
            withSocketPair $ \(writer, _) ->
                sendJSONFrame 8 writer (String "this frame is too long")
                    `shouldThrow` \case
                        FrameTooLarge _ 8 -> True
                        _ -> False

        it "rejects an advertised inbound length before reading its body" $
            withSocketPair $ \(writer, reader) -> do
                Socket.sendAll writer (BS.pack [0, 0, 1, 0])
                (receiveJSONFrame 32 reader :: IO Value)
                    `shouldThrow` \case
                        FrameTooLarge 256 32 -> True
                        _ -> False

    describe "protocol negotiation" $ do
        it "chooses the current common version" $ do
            negotiateVersion [ProtocolVersion 99, currentProtocolVersion]
                `shouldBe` Just currentProtocolVersion
            negotiateVersion [ProtocolVersion 99] `shouldBe` Nothing

    describe "durable journal" $ do
        it "keeps sequence numbers monotonic and replays after a cursor" $
            withSystemTempDirectory "daemon-journal" $ \directory -> do
                journal <- openJournal (defaultJournalConfig directory)
                first <- appendEvent journal "one" Null
                second <- appendEvent journal "two" Null
                first.sequenceNumber `shouldBe` 1
                second.sequenceNumber `shouldBe` 2
                replayAfter journal 1 `shouldReturn` ReplayEvents [second]

        it "falls back to a snapshot when retention has removed the cursor" $
            withSystemTempDirectory "daemon-retention" $ \directory -> do
                let config =
                        (defaultJournalConfig directory)
                            { maximumEvents = 2
                            , maximumJournalBytes = 1_048_576
                            }
                journal <- openJournal config
                _ <- appendEvent journal "one" Null
                second <- appendEvent journal "two" Null
                third <- appendEvent journal "three" Null
                replayAfter journal 0 >>= (`shouldSatisfy` \case
                    ReplaySnapshot saved -> saved.lastSequence == 3
                    ReplayEvents _ -> False)
                replayAfter journal 1 `shouldReturn` ReplayEvents [second, third]
                replayAfter journal 99 >>= (`shouldSatisfy` \case
                    ReplaySnapshot saved -> saved.lastSequence == 3
                    ReplayEvents _ -> False)

        it "redacts sensitive fields and bounds retained task logs" $
            withSystemTempDirectory "daemon-redaction" $ \directory -> do
                now <- getCurrentTime
                let config =
                        (defaultJournalConfig directory)
                            { maximumTaskLogLines = 2
                            , maximumTaskLogCharacters = 20
                            }
                    task =
                        DurableTask
                            { taskId = TaskId "task"
                            , status = TaskRunning
                            , description = "test"
                            , updatedAt = now
                            , logTail = ["old", "authorization: bearer private", "safe"]
                            }
                journal <- openJournal config
                event <- appendEvent journal "credentials" (object ["token" .= ("private" :: Text)])
                event.payload `shouldBe` object ["token" .= ("[REDACTED]" :: Text)]
                _ <- persistTask journal task
                saved <- snapshot journal
                (.logTail) (saved.tasks Map.! TaskId "task")
                    `shouldBe` ["[REDACTED]", "safe"]
                (.description) (saved.tasks Map.! TaskId "task")
                    `shouldBe` "test"

        it "marks active tasks interrupted on startup without retrying them" $
            withSystemTempDirectory "daemon-recovery" $ \directory -> do
                now <- getCurrentTime
                let config = defaultJournalConfig directory
                    running =
                        DurableTask
                            { taskId = TaskId "active"
                            , status = TaskRunning
                            , description = "active before crash"
                            , updatedAt = now
                            , logTail = []
                            }
                firstProcess <- openJournal config
                _ <- persistTask firstProcess running
                secondProcess <- openJournal config
                recovered <- snapshot secondProcess
                (.status) (recovered.tasks Map.! TaskId "active")
                    `shouldBe` TaskInterrupted
                let recoveredSequence = recovered.lastSequence
                thirdProcess <- openJournal config
                unchanged <- snapshot thirdProcess
                unchanged.lastSequence `shouldBe` recoveredSequence
                (.status) (unchanged.tasks Map.! TaskId "active")
                    `shouldBe` TaskInterrupted

        it "fails closed on corrupt snapshots and event gaps" $
            withSystemTempDirectory "daemon-corrupt" $ \directory -> do
                BS.writeFile (directory </> "snapshot.json") "{broken"
                openJournal (defaultJournalConfig directory)
                    `shouldThrow` \case
                        JournalCorrupt _ _ -> True
                        _ -> False
                removeFile (directory </> "snapshot.json")
                let event sequenceNumber =
                        EventEnvelope {sequenceNumber, eventType = "test", payload = Null}
                BS.writeFile
                    (directory </> "events.jsonl")
                    (LBS.toStrict (encode (event 1)) <> "\n" <> LBS.toStrict (encode (event 3)) <> "\n")
                openJournal (defaultJournalConfig directory)
                    `shouldThrow` \case
                        JournalEventGap 2 3 -> True
                        _ -> False

        it "rejects a retained suffix that starts after a snapshot gap" $
            withSystemTempDirectory "daemon-snapshot-gap" $ \directory -> do
                let saved = JournalSnapshot {lastSequence = 4, tasks = Map.empty}
                    event = EventEnvelope {sequenceNumber = 6, eventType = "test", payload = Null}
                BS.writeFile (directory </> "snapshot.json") (LBS.toStrict (encode saved))
                BS.writeFile (directory </> "events.jsonl") (LBS.toStrict (encode event) <> "\n")
                openJournal (defaultJournalConfig directory)
                    `shouldThrow` \case
                        JournalEventGap 5 6 -> True
                        _ -> False

        it "rejects a nonempty retained suffix that ends before its snapshot" $
            withSystemTempDirectory "daemon-stale-suffix" $ \directory -> do
                let saved = JournalSnapshot {lastSequence = 4, tasks = Map.empty}
                    event = EventEnvelope {sequenceNumber = 3, eventType = "test", payload = Null}
                BS.writeFile (directory </> "snapshot.json") (LBS.toStrict (encode saved))
                BS.writeFile (directory </> "events.jsonl") (LBS.toStrict (encode event) <> "\n")
                openJournal (defaultJournalConfig directory)
                    `shouldThrow` \case
                        JournalCorrupt _ _ -> True
                        _ -> False

        it "enforces contiguous startup retention and rejects an oversized newest event" $
            withSystemTempDirectory "daemon-startup-retention" $ \directory -> do
                original <- openJournal ((defaultJournalConfig directory) {maximumEvents = 10})
                now <- getCurrentTime
                _ <-
                    persistTask original $
                        DurableTask
                            { taskId = TaskId "snapshot-anchor"
                            , status = TaskCompleted
                            , description = "anchor"
                            , updatedAt = now
                            , logTail = []
                            }
                second <- appendEvent original "two" Null
                third <- appendEvent original "three" Null
                reopened <- openJournal ((defaultJournalConfig directory) {maximumEvents = 2})
                replayAfter reopened 1 `shouldReturn` ReplayEvents [second, third]
                let tinyConfig =
                        (defaultJournalConfig (directory </> "oversized"))
                            { maximumJournalBytes = 4
                            }
                bounded <- openJournal tinyConfig
                appendEvent bounded "large" (String "payload")
                    `shouldThrow` \case
                        JournalEventTooLarge _ 4 -> True
                        _ -> False
                saved <- snapshot bounded
                saved.lastSequence `shouldBe` 0

        it "poisons the journal after persistence I/O failure instead of reusing a sequence" $
            withSystemTempDirectory "daemon-poison" $ \directory -> do
                journal <- openJournal (defaultJournalConfig directory)
                _ <- appendEvent journal "first" Null
                renamePath (directory </> "events.jsonl") (directory </> "events.saved")
                createDirectory (directory </> "events.jsonl")
                appendEvent journal "second" Null `shouldThrow` anyException
                appendEvent journal "third" Null
                    `shouldThrow` \case
                        JournalPoisoned _ -> True
                        _ -> False

        it "redacts descriptions, bounds task count, and bounds subscriber queues" $
            withSystemTempDirectory "daemon-bounds" $ \directory -> do
                now <- getCurrentTime
                let config =
                        (defaultJournalConfig directory)
                            { maximumTasks = 1
                            , subscriberQueueSize = 1
                            }
                    task name description =
                        DurableTask
                            { taskId = TaskId name
                            , status = TaskCompleted
                            , description
                            , updatedAt = now
                            , logTail = []
                            }
                journal <- openJournal config
                _ <- persistTask journal (task "one" "password=private")
                saved <- snapshot journal
                (.description) (saved.tasks Map.! TaskId "one") `shouldBe` "[REDACTED]"
                persistTask journal (task "two" "safe")
                    `shouldThrow` \case
                        JournalTaskCapacityExceeded 1 -> True
                        _ -> False
                (_, _, _, overflowed, unsubscribe) <- subscribeReplay journal saved.lastSequence
                _ <- appendEvent journal "a" Null
                _ <- appendEvent journal "b" Null
                readTVarIO overflowed `shouldReturn` True
                unsubscribe

        it "applies task bounds while recovering task_changed events" $
            withSystemTempDirectory "daemon-recovered-task-bounds" $ \directory -> do
                now <- getCurrentTime
                let task =
                        DurableTask
                            { taskId = TaskId "recovered"
                            , status = TaskCompleted
                            , description = "password=private"
                            , updatedAt = now
                            , logTail = ["token=private"]
                            }
                    event =
                        EventEnvelope
                            { sequenceNumber = 1
                            , eventType = "task_changed"
                            , payload = toJSON task
                            }
                    config =
                        (defaultJournalConfig directory)
                            { maximumTaskDescriptionCharacters = 64
                            , maximumTaskLogLines = 0
                            }
                BS.writeFile (directory </> "events.jsonl") (LBS.toStrict (encode event) <> "\n")
                recovered <- openJournal config >>= snapshot
                let bounded = recovered.tasks Map.! TaskId "recovered"
                bounded.description `shouldBe` "[REDACTED]"
                bounded.logTail `shouldBe` []

        it "retains no events when maximumEvents is zero and preserves the sequence" $
            withSystemTempDirectory "daemon-zero-retention" $ \directory -> do
                let config = (defaultJournalConfig directory) {maximumEvents = 0}
                journal <- openJournal config
                _ <- appendEvent journal "discarded" Null
                BS.readFile (directory </> "events.jsonl") `shouldReturn` BS.empty
                reopened <- openJournal config
                recovered <- snapshot reopened
                recovered.lastSequence `shouldBe` 1
                replayAfter reopened 0 `shouldReturn` ReplaySnapshot recovered

        it "refuses to wrap an exhausted sequence number" $
            withSystemTempDirectory "daemon-sequence-overflow" $ \directory -> do
                let exhausted = JournalSnapshot {lastSequence = Sequence maxBound, tasks = Map.empty}
                BS.writeFile (directory </> "snapshot.json") (LBS.toStrict (encode exhausted))
                journal <- openJournal (defaultJournalConfig directory)
                appendEvent journal "wrapped" Null
                    `shouldThrow` \case
                        JournalSequenceExhausted sequenceNumber ->
                            sequenceNumber == Sequence maxBound
                        _ -> False

        it "rejects symlinked journal directories and files" $
            withSystemTempDirectory "daemon-symlink" $ \directory -> do
                let target = directory </> "target"
                    linked = directory </> "linked"
                createDirectory target
                createSymbolicLink target linked
                openJournal (defaultJournalConfig linked)
                    `shouldThrow` \case
                        JournalInsecurePath _ -> True
                        _ -> False
                let journalDir = directory </> "journal"
                createDirectory journalDir
                BS.writeFile (directory </> "outside") "{}"
                createSymbolicLink (directory </> "outside") (journalDir </> "snapshot.json")
                openJournal (defaultJournalConfig journalDir)
                    `shouldThrow` \case
                        JournalInsecurePath _ -> True
                        _ -> False

    describe "secure Unix socket" $ do
        it "uses private modes and authenticates the same-user peer" $
            withTempDirectory "/tmp" "daemon-socket" $ \directory -> do
                let path = directory </> "private" </> "daemon.sock"
                    config = SocketConfig {path, backlog = 1}
                withUnixListener config $ \listener ->
                    withAsync
                        (bracket (acceptOwnedPeer listener) close verifyPeerOwner)
                        $ \_ -> do
                            client <- socket AF_UNIX Stream defaultProtocol
                            bracket (pure client) close $ \peer -> do
                                connect peer (SockAddrUnix path)
                                verifyPeerOwner peer
                                socketStatus <- getFileStatus path
                                directoryStatus <- getFileStatus (directory </> "private")
                                fileMode socketStatus .&. 0o777 `shouldBe` 0o600
                                fileMode directoryStatus .&. 0o777 `shouldBe` 0o700

        it "does not unlink an active daemon socket" $
            withTempDirectory "/tmp" "daemon-active" $ \directory -> do
                let config =
                        SocketConfig
                            { path = directory </> "daemon.sock"
                            , backlog = 1
                            }
                withUnixListener config $ \_ ->
                    withUnixListener config (const (pure ()))
                        `shouldThrow` anyException

        it "only removes the socket inode created by this listener" $
            withTempDirectory "/tmp" "daemon-identity" $ \directory -> do
                let path = directory </> "daemon.sock"
                    config = SocketConfig {path, backlog = 1}
                withUnixListener config $ \_ -> do
                    renamePath path (path <> ".replaced")
                    BS.writeFile path "replacement"
                doesFileExist path `shouldReturn` True

    describe "server integration" $ do
        it "handshakes, sends a snapshot, streams events, and uses the supervisor seam" $
            withSystemTempDirectory "daemon-server" $ \directory -> do
                journal <- openJournal (defaultJournalConfig directory)
                let config =
                        defaultServerConfig
                            { heartbeatSeconds = 60
                            , maximumFrameBytes = 4_096
                            }
                    supervisor =
                        Supervisor
                            { handleCommand = \_ command -> pure (Right command)
                            }
                withSocketPair $ \(serverPeer, clientPeer) ->
                    withAsync (serveConnection config journal supervisor serverPeer) $ \_ -> do
                        sendJSONFrame config.maximumFrameBytes clientPeer $
                            ClientHello
                                Hello
                                    { clientId = ClientId "test-client"
                                    , versions = [currentProtocolVersion]
                                    , resumeAfter = Nothing
                                    }
                        welcome <- receiveJSONFrame config.maximumFrameBytes clientPeer
                        welcome `shouldSatisfy` \case
                            ServerWelcome accepted -> accepted.version == currentProtocolVersion
                            _ -> False
                        initial <- receiveJSONFrame config.maximumFrameBytes clientPeer
                        initial `shouldSatisfy` \case
                            ServerSnapshotChunk 0 0 1 _ -> True
                            _ -> False
                        event <- appendEvent journal "changed" (object ["value" .= (1 :: Int)])
                        receiveJSONFrame config.maximumFrameBytes clientPeer
                            `shouldReturn` ServerEvent event
                        sendJSONFrame config.maximumFrameBytes clientPeer $
                            ClientCommand (CommandId "command") (String "echo")
                        receiveJSONFrame config.maximumFrameBytes clientPeer
                            `shouldReturn` ServerCommandResult (CommandId "command") (Right (String "echo"))
                        sendJSONFrame config.maximumFrameBytes clientPeer (ClientAck event.sequenceNumber)

        it "times out idle clients and preserves async cancellation" $
            withSystemTempDirectory "daemon-timeout" $ \directory -> do
                journal <- openJournal (defaultJournalConfig directory)
                let config = defaultServerConfig {ioTimeoutSeconds = 1}
                withSocketPair $ \(serverPeer, _) -> do
                    withAsync (serveConnection config journal unavailableSupervisor serverPeer) $ \server -> do
                        result <- timeout 2_500_000 (waitCatch server)
                        result `shouldSatisfy` \case
                            Just (Left exception) -> not (isAsyncException exception)
                            _ -> False
                withTempDirectory "/tmp" "daemon-cancel" $ \socketDirectory -> do
                    let socketConfig =
                            SocketConfig
                                { path = socketDirectory </> "daemon.sock"
                                , backlog = 1
                                }
                    withUnixListener socketConfig $ \listener ->
                        withAsync (runServerOnListener listener config journal unavailableSupervisor) $ \server -> do
                            client <- socket AF_UNIX Stream defaultProtocol
                            bracket (pure client) close $ \peer -> do
                                connect peer (SockAddrUnix socketConfig.path)
                                cancel server
                                outcome <- timeout 2_000_000 (waitCatch server)
                                outcome `shouldSatisfy` \case
                                    Just (Left exception) -> isAsyncException exception
                                    _ -> False

        it "closes an accepted peer when cancellation interrupts full-queue admission" $
            withSystemTempDirectory "daemon-admission-cancel" $ \directory ->
                withTempDirectory "/tmp" "daemon-admission" $ \socketDirectory -> do
                    journal <- openJournal (defaultJournalConfig directory)
                    let socketConfig =
                            SocketConfig
                                { path = socketDirectory </> "daemon.sock"
                                , backlog = 8
                                }
                        config =
                            defaultServerConfig
                                { workerCount = 1
                                , ioTimeoutSeconds = 30
                                }
                    withUnixListener socketConfig $ \listener ->
                        withAsync (runServerOnListener listener config journal unavailableSupervisor) $ \server ->
                            bracket
                                (traverse (const (socket AF_UNIX Stream defaultProtocol)) [1 :: Int .. 3])
                                (mapM_ close)
                                $ \clients -> do
                                    mapM_ (`connect` SockAddrUnix socketConfig.path) clients
                                    threadDelay 200_000
                                    cancel server
                                    _ <- waitCatch server
                                    results <-
                                        traverse
                                            (\client -> timeout 1_000_000 (Socket.recv client 1))
                                            clients
                                    results `shouldBe` replicate 3 (Just BS.empty)

        it "chunks large snapshots below the configured frame bound" $
            withSystemTempDirectory "daemon-snapshot-chunks" $ \directory -> do
                journal <- openJournal (defaultJournalConfig directory)
                now <- getCurrentTime
                _ <-
                    persistTask journal $
                        DurableTask
                            { taskId = TaskId "large"
                            , status = TaskCompleted
                            , description = "large snapshot"
                            , updatedAt = now
                            , logTail = [Text.replicate 2_000 "x"]
                            }
                let config =
                        defaultServerConfig
                            { maximumFrameBytes = 512
                            , heartbeatSeconds = 60
                            }
                withSocketPair $ \(serverPeer, clientPeer) ->
                    withAsync (serveConnection config journal unavailableSupervisor serverPeer) $ \_ -> do
                        sendJSONFrame config.maximumFrameBytes clientPeer $
                            ClientHello
                                Hello
                                    { clientId = ClientId "snapshot-client"
                                    , versions = [currentProtocolVersion]
                                    , resumeAfter = Nothing
                                    }
                        _ <- receiveJSONFrame config.maximumFrameBytes clientPeer :: IO ServerMessage
                        first <- receiveJSONFrame config.maximumFrameBytes clientPeer
                        chunkCount <-
                            case first of
                                ServerSnapshotChunk 1 0 count _ -> do
                                    count `shouldSatisfy` (> 1)
                                    pure count
                                _ -> expectationFailure ("unexpected snapshot message: " <> show first) >> pure 0
                        sequenceNumbers <-
                            traverse
                                ( \_ ->
                                    receiveJSONFrame config.maximumFrameBytes clientPeer >>= \case
                                        ServerSnapshotChunk 1 index count _ | count == chunkCount -> pure index
                                        other -> expectationFailure ("unexpected snapshot chunk: " <> show other) >> pure (-1)
                                )
                                [1 .. chunkCount - 1]
                        sequenceNumbers `shouldBe` [1 .. chunkCount - 1]

        it "chunks events that exceed the transport frame bound" $
            withSystemTempDirectory "daemon-event-chunks" $ \directory -> do
                journal <- openJournal (defaultJournalConfig directory)
                event <- appendEvent journal "large" (String (Text.replicate 2_000 "x"))
                let config =
                        defaultServerConfig
                            { maximumFrameBytes = 512
                            , heartbeatSeconds = 60
                            }
                withSocketPair $ \(serverPeer, clientPeer) ->
                    withAsync (serveConnection config journal unavailableSupervisor serverPeer) $ \_ -> do
                        sendJSONFrame config.maximumFrameBytes clientPeer $
                            ClientHello
                                Hello
                                    { clientId = ClientId "event-client"
                                    , versions = [currentProtocolVersion]
                                    , resumeAfter = Just 0
                                    }
                        _ <- receiveJSONFrame config.maximumFrameBytes clientPeer :: IO ServerMessage
                        first <- receiveJSONFrame config.maximumFrameBytes clientPeer
                        chunkCount <- case first of
                            ServerEventChunk sequenceNumber 0 count _ ->
                                if sequenceNumber == event.sequenceNumber && count > 1
                                    then pure count
                                    else expectationFailure ("unexpected event chunk: " <> show first) >> pure 0
                            _ -> expectationFailure ("unexpected event message: " <> show first) >> pure 0
                        indices <-
                            traverse
                                ( \_ ->
                                    receiveJSONFrame config.maximumFrameBytes clientPeer >>= \case
                                        ServerEventChunk sequenceNumber index count _
                                            | sequenceNumber == event.sequenceNumber
                                            , count == chunkCount ->
                                                pure index
                                        other ->
                                            expectationFailure ("unexpected event chunk: " <> show other) >> pure (-1)
                                )
                                [1 .. chunkCount - 1]
                        indices `shouldBe` [1 .. chunkCount - 1]

withSocketPair :: ((Socket, Socket) -> IO value) -> IO value
withSocketPair =
    bracket
        (socketPair AF_UNIX Stream defaultProtocol)
        (\(left, right) -> close left >> close right)
