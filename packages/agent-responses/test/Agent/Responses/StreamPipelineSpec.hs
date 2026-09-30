module Agent.Responses.StreamPipelineSpec (spec) where

import Agent.Error (ApiError(..))
import Agent.Responses.SSE (decodeSseC, parseSseEventsBytes)
import Agent.Responses.StreamAssembly
import Agent.Responses.StreamPipeline (consumeResponsesSse)
import Agent.Responses.Types
import Control.Exception (AsyncException(..))
import qualified Control.Exception as Exception
import Control.Exception.Safe (bracket_, throwIO)
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.ByteString as BS
import Data.Conduit (await, runConduit, yield, (.|))
import Data.IORef
import qualified Data.Text.Encoding as Text
import Test.Hspec

spec :: Spec
spec = describe "Responses Conduit pipeline" do
    it "decodes UTF-8 and CRLF across every two-chunk split, including EOF flush" do
        let bytes = Text.encodeUtf8
                ": café\r\n\r\ndata: {\"type\":\"response.created\",\"response\":{\"id\":\"réponse\"}}\r\n\r\ndata: {\"type\":\"response.completed\",\"response\":{\"id\":\"réponse\",\"status\":\"completed\"}}"
        expected <- runChunks [bytes]
        mapM_ (\offset -> do
            actual <- runChunks (filter (not . BS.null)
                [BS.take offset bytes, BS.drop offset bytes])
            actual `shouldBe` expected)
            [0 .. BS.length bytes]
        response <- expectRight (fst expected)
        response.responseId `shouldBe` "réponse"
        snd expected `shouldBe` [EventResponseCreated, EventResponseCompleted]

    it "handles single-byte chunks" do
        expected <- runChunks [completeStream]
        actual <- runChunks (map BS.singleton (BS.unpack completeStream))
        actual `shouldBe` expected

    it "does not treat empty decoder input chunks as EOF" do
        result <- runExceptT $ runConduit $
            mapM_ yield [BS.take 3 completeStream, "", BS.drop 3 completeStream]
                .| decodeSseC .| collect
        result `shouldBe` parseSseEventsBytes completeStream

    it "assembles streamed tool arguments without reading ahead of callbacks" do
        let chunks =
                [ created
                , "data: {\"type\":\"response.output_item.added\",\"output_index\":0,\"item\":{\"type\":\"function_call\",\"id\":\"fc-1\",\"call_id\":\"call-1\",\"name\":\"read_file\",\"arguments\":\"\"}}\n\n"
                , "data: {\"type\":\"response.function_call_arguments.delta\",\"item_id\":\"fc-1\",\"output_index\":0,\"delta\":\"{\\\"path\\\":\\\"\"}\n\n"
                , "data: {\"type\":\"response.function_call_arguments.delta\",\"item_id\":\"fc-1\",\"output_index\":0,\"delta\":\"README.md\\\"}\"}\n\n"
                , "data: {\"type\":\"response.completed\",\"response\":{\"id\":\"response-1\",\"status\":\"completed\"}}\n\n"
                ]
        remaining <- newIORef chunks
        reads <- newIORef (0 :: Int)
        callbacks <- newIORef (0 :: Int)
        let readChunk = do
                n <- readIORef reads
                -- The previous callback must finish before another read.
                readIORef callbacks `shouldReturn` n
                modifyIORef' reads (+ 1)
                atomicModifyIORef' remaining \case
                    [] -> ([], "")
                    chunk : rest -> (rest, chunk)
            emit _ = do
                n <- atomicModifyIORef' callbacks (\n -> (n + 1, n + 1))
                readIORef reads `shouldReturn` n
        result <- consumeResponsesSse config (Just "test") readChunk emit
        response <- expectRight result
        [(callId, name, arguments)
            | FunctionCallItem FunctionCall { callId, name, arguments }
                <- response.output]
            `shouldBe` [("call-1", "read_file", "{\"path\":\"README.md\"}")]
        readIORef reads `shouldReturn` length chunks
        readIORef callbacks `shouldReturn` length chunks

    it "stops reads and callbacks at a terminal event in a multi-event chunk" do
        reads <- newIORef (0 :: Int)
        seen <- newIORef []
        let readChunk = do
                n <- atomicModifyIORef' reads (\n -> (n + 1, n))
                if n == 0 then pure (completeStream <> created)
                    else throwIO (userError "read after terminal")
        result <- consumeResponsesSse config (Just "test") readChunk
            (\event -> modifyIORef' seen (<> [responseStreamEventType event]))
        _ <- expectRight result
        readIORef reads `shouldReturn` 1
        readIORef seen `shouldReturn` [EventResponseCreated, EventResponseCompleted]

    it "preserves chunk validation before callbacks, even after a terminal frame" do
        (result, seen) <- runChunks
            [completeStream <> "data: " <> BS.singleton 255 <> "\n\n"]
        result `shouldSatisfy` isDecodeError
        seen `shouldBe` []

    it "preserves callbacks from previous chunks when a later chunk fails" do
        (result, seen) <- runChunks [created, "data: " <> BS.singleton 255 <> "\n\n"]
        result `shouldSatisfy` isDecodeError
        seen `shouldBe` [EventResponseCreated]

    it "reports missing terminal after emitting the final unterminated event" do
        (result, seen) <- runChunks [BS.take (BS.length created - 2) created]
        result `shouldBe` Left (JsonDecodeError "missing terminal" "")
        seen `shouldBe` [EventResponseCreated]

    it "unwinds the caller's bracket when a callback fails without another read" do
        closed <- newIORef False
        reads <- newIORef (0 :: Int)
        let action = bracket_ (pure ()) (writeIORef closed True) $
                consumeResponsesSse config Nothing
                    (modifyIORef' reads (+ 1) >> pure completeStream)
                    (const (throwIO (userError "callback failed")))
        action `shouldThrow` anyIOException
        readIORef closed `shouldReturn` True
        readIORef reads `shouldReturn` 1

    it "propagates source exceptions and releases the caller's resource" do
        closed <- newIORef False
        let action = bracket_ (pure ()) (writeIORef closed True) $
                consumeResponsesSse config Nothing
                    (throwIO (userError "read failed")) (const (pure ()))
        action `shouldThrow` anyIOException
        readIORef closed `shouldReturn` True

    it "propagates asynchronous cancellation and releases the caller's resource" do
        closed <- newIORef False
        let action = bracket_ (pure ()) (writeIORef closed True) $
                consumeResponsesSse config Nothing
                    (pure completeStream) (const (Exception.throwIO UserInterrupt))
        action `shouldThrow` (== UserInterrupt)
        readIORef closed `shouldReturn` True

config :: StreamAssemblyConfig
config = StreamAssemblyConfig
    { missingCompletionMessage = "missing terminal"
    , classifyStreamError = \err -> ConnectionError err.message
    , classifyFailedResponse = ConnectionError . failedStreamResponseMessage
    , incompleteAsFailure = False
    }

created :: BS.ByteString
created = "data: {\"type\":\"response.created\",\"response\":{\"id\":\"response-1\"}}\n\n"

completeStream :: BS.ByteString
completeStream = created <>
    "data: {\"type\":\"response.completed\",\"response\":{\"id\":\"response-1\",\"status\":\"completed\"}}\n\n"

runChunks chunks = do
    remaining <- newIORef chunks
    seen <- newIORef []
    let readChunk = atomicModifyIORef' remaining \case
            [] -> ([], "")
            chunk : rest -> (rest, chunk)
    result <- consumeResponsesSse config (Just "test") readChunk
        (\event -> modifyIORef' seen (<> [responseStreamEventType event]))
    events <- readIORef seen
    pure (result, events)

collect = await >>= \case
    Nothing -> pure []
    Just event -> (event :) <$> collect

isDecodeError (Left JsonDecodeError{}) = True
isDecodeError _ = False

expectRight (Right value) = pure value
expectRight (Left err) = expectationFailure (show err) >> fail "expected Right"
