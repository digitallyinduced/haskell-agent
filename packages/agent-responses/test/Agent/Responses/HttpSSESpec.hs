module Agent.Responses.HttpSSESpec (spec) where

import Agent.Error (ApiError(..))
import Agent.Responses.HttpSSE
import Agent.Responses.StreamAssembly
import Agent.Responses.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.IORef
import qualified Data.Text as Text
import Network.HTTP.Simple (Request)
import Network.HTTP.Types (status200, status500)
import Network.Wai (Application, responseLBS, responseStream)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec

spec :: Spec
spec = describe "performResponsesHttpSse" do
    it "assembles events online, emits in wire order, and stops at terminal" do
        seen <- newIORef []
        testWithApplication (pure streamApplication) \port -> do
            result <- performResponsesHttpSse
                testConfig
                ("http://127.0.0.1:" <> show port)
                5
                "{}"
                (id :: Request -> Request)
                (\event -> modifyIORef' seen
                    (<> [responseStreamEventType event]))
            response <- expectRight result
            eventTypes <- readIORef seen
            eventTypes `shouldBe`
                [ EventResponseCreated
                , EventOutputItemAdded
                , EventFunctionCallArgumentsDelta
                , EventFunctionCallArgumentsDelta
                , EventResponseCompleted
                ]
            [arguments
                | FunctionCallItem FunctionCall { arguments }
                    <- response.output
                ] `shouldBe` ["{\"path\":\"README.md\"}"]

    it "caps oversized non-success response bodies with a truthful marker" do
        testWithApplication (pure oversizedFailureApplication) \port -> do
            result <- performResponsesHttpSse
                testConfig
                ("http://127.0.0.1:" <> show port)
                5
                "{}"
                id
                (const (pure ()))
            case result of
                Left (ConnectionError message) -> do
                    Text.length (Text.takeWhile (== 'x') message)
                        `shouldBe` 1024 * 1024
                    message `shouldSatisfy`
                        Text.isSuffixOf
                            "\n[response body truncated after 1048576 bytes]"
                other -> expectationFailure
                    ("expected bounded failure body, got " <> show other)

testConfig :: HttpSseConfig
testConfig = HttpSseConfig
    { exceptionPrefix = "test request failed"
    , classifyFailure = \_ _ body -> ConnectionError body
    , assemblyConfig = StreamAssemblyConfig
        { missingCompletionMessage = "missing terminal"
        , classifyStreamError =
            \streamError -> ConnectionError streamError.message
        , classifyFailedResponse =
            ConnectionError . failedStreamResponseMessage
        , incompleteAsFailure = False
        }
    , responseModelHint = Just "test-model"
    }

streamApplication :: Application
streamApplication _request respond =
    respond $ responseStream
        status200
        [("Content-Type", "text/event-stream")]
        \send flush ->
            mapM_ (\chunk -> send (Builder.byteString chunk) >> flush)
                streamChunks

oversizedFailureApplication :: Application
oversizedFailureApplication _request respond =
    respond $ responseLBS
        status500
        [("Content-Type", "text/plain")]
        (LBS8.replicate (2 * 1024 * 1024) 'x')

streamChunks :: [BS.ByteString]
streamChunks =
    [ "event: response.created\ndata: {\"type\":\"response.created\",\
      \\"response\":{\"id\":\"resp-http\"}}\n\n"
    , "event: response.output_item.added\ndata: {\"type\":\
      \\"response.output_item.added\",\"output_index\":0,\"item\":{\"type\":\
      \\"function_call\",\"id\":\"fc-1\",\"call_id\":\"call-1\",\"name\":\
      \\"read_file\",\"arguments\":\"\"}}\n\n"
    , "event: response.function_call_arguments.delta\ndata: {\"type\":\
      \\"response.function_call_arguments.delta\",\"item_id\":\"fc-1\",\
      \\"output_index\":0,\"delta\":\"{\\\"path\\\":\\\"\"}\n\n"
    , "event: response.function_call_arguments.delta\ndata: {\"type\":\
      \\"response.function_call_arguments.delta\",\"item_id\":\"fc-1\",\
      \\"output_index\":0,\"delta\":\"README.md\\\"}\"}\n\n"
    , "event: response.completed\ndata: {\"type\":\"response.completed\",\
      \\"response\":{\"id\":\"resp-http\",\"status\":\"completed\"}}\n\n"
    , "event: response.output_item.done\ndata: {\"type\":\
      \\"response.output_item.done\",\"output_index\":0,\"item\":{\"type\":\
      \\"function_call\",\"call_id\":\"late\",\"name\":\"late\",\
      \\"arguments\":\"{}\"}}\n\n"
    ]

expectRight :: Show errorValue => Either errorValue value -> IO value
expectRight = either
    (\err -> expectationFailure (show err) >> fail "expectRight")
    pure
