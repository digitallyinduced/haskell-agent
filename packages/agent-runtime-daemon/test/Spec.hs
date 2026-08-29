module Main (main) where

import Control.Concurrent.Async (concurrently, withAsync)
import Control.Exception.Safe (bracket)
import Data.Aeson (Value (..), object, (.=))
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Network.Socket
import qualified Network.Socket.ByteString as Socket
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory, withTempDirectory)
import System.Posix.Files (fileMode, getFileStatus)
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
                            ServerSnapshot 0 _ -> True
                            _ -> False
                        event <- appendEvent journal "changed" (object ["value" .= (1 :: Int)])
                        receiveJSONFrame config.maximumFrameBytes clientPeer
                            `shouldReturn` ServerEvent event
                        sendJSONFrame config.maximumFrameBytes clientPeer $
                            ClientCommand (CommandId "command") (String "echo")
                        receiveJSONFrame config.maximumFrameBytes clientPeer
                            `shouldReturn` ServerCommandResult (CommandId "command") (Right (String "echo"))
                        sendJSONFrame config.maximumFrameBytes clientPeer (ClientAck event.sequenceNumber)

withSocketPair :: ((Socket, Socket) -> IO value) -> IO value
withSocketPair =
    bracket
        (socketPair AF_UNIX Stream defaultProtocol)
        (\(left, right) -> close left >> close right)
