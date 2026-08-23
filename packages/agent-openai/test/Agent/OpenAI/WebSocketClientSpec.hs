module Agent.OpenAI.WebSocketClientSpec (spec) where

import Test.Hspec
import Agent.Error
import Agent.Provider (Credential(..), Provider(..))
import Agent.Responses.Types
import Agent.OpenAI.WebSocketClient
import Control.Retry (constantDelay, limitRetries)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Text (Text)

spec :: Spec
spec = do
  describe "buildCodexWsHeaders" do
    it "advertises remote compaction v2 on the session handshake" do
        let credential = Credential
                { accessToken = "token"
                , accountId = "account"
                , leaseId = Nothing
                , provider = OpenAIProvider
                }
        lookup "x-codex-beta-features" (buildCodexWsHeaders credential)
            `shouldBe` Just "remote_compaction_v2"

  describe "buildWsPayloadWithOptions" do
    it "forces store=false for the Codex WebSocket contract" do
        let request = sampleRequest { store = Just True }
        field "store" (buildWsPayloadWithOptions defaultCodexWsOptions request Nothing)
            `shouldBe` Just (Aeson.Bool False)

    it "does not request server-managed compaction by default" do
        contextManagement defaultCodexWsOptions `shouldBe` Nothing

    it "serializes a positive server-side compaction threshold" do
        let options = CodexWsOptions { compactThreshold = Just 180000 }
        contextManagement options `shouldBe` Just (Aeson.toJSON
            [ Aeson.object
                [ "type" Aeson..= ("compaction" :: Text)
                , "compact_threshold" Aeson..= (180000 :: Int)
                ]
            ])

    it "omits non-positive thresholds" do
        let options = CodexWsOptions { compactThreshold = Just 0 }
        contextManagement options `shouldBe` Nothing

  describe "retryTransientWsResultWithPolicy" do
    it "retries overloads centrally before returning success" do
        let overload = ProviderError OverloadedError "server_is_overloaded" Nothing
        responses <- newIORef
            [ Left overload
            , Left overload
            , Right ("completed" :: Text)
            ]
        result <- retryTransientWsResultWithPolicy
            (constantDelay 0 <> limitRetries 3)
            (atomicModifyIORef' responses \case
                next : rest -> (rest, next)
                [] -> error "unexpected extra WebSocket request")

        result `shouldBe` Right "completed"
        readIORef responses `shouldReturn` []

    it "leaves connection, connection-limit, and quota failures to callers" do
        attempts <- newIORef (0 :: Int)
        let run err = retryTransientWsResultWithPolicy
                (constantDelay 0 <> limitRetries 3)
                (modifyIORef' attempts (+ 1) >> pure (Left err :: Either ApiError Text))

        run (ConnectionError "socket closed")
            `shouldReturn` Left (ConnectionError "socket closed")
        run (ProviderError WebSocketConnectionLimitReached
                "too many websocket connections" Nothing)
            `shouldReturn` Left (ProviderError WebSocketConnectionLimitReached
                "too many websocket connections" Nothing)
        run (ProviderError UsageLimitReached "quota" (Just 3600))
            `shouldReturn` Left (ProviderError UsageLimitReached "quota" (Just 3600))
        readIORef attempts `shouldReturn` 3

  describe "receiveWsResponseWithActions" do
    it "assembles the Codex indexless function-call sequence through response.done" do
        testPartialTerminalResponse
            [ lifecycleFrame "response.created"
                (Aeson.object ["id" Aeson..= ("resp-test" :: Text)])
            , Aeson.encode $ Aeson.object
                [ "type" Aeson..= ("response.output_item.done" :: Text)
                , "item" Aeson..= Aeson.object
                    [ "type" Aeson..= ("function_call" :: Text)
                    , "call_id" Aeson..= ("call-test" :: Text)
                    , "name" Aeson..= ("shell_command" :: Text)
                    , "arguments" Aeson..= ("{}" :: Text)
                    ]
                ]
            , lifecycleFrame "response.done"
                (Aeson.object
                    [ "usage" Aeson..= Aeson.object
                        [ "input_tokens" Aeson..= (10 :: Int)
                        , "output_tokens" Aeson..= (2 :: Int)
                        , "total_tokens" Aeson..= (12 :: Int)
                        ]
                    ])
            ]
            [EventResponseCreated, EventOutputItemDone, EventResponseDone]
            \response ->
                [name | FunctionCallItem FunctionCall { name } <- response.output]
                    `shouldBe` ["shell_command"]

    it "assembles a minimal response.created followed by response.completed" do
        testPartialTerminalResponse
            [ lifecycleFrame "response.created"
                (Aeson.object ["id" Aeson..= ("resp-test" :: Text)])
            , lifecycleFrame "response.completed" (Aeson.object [])
            ]
            [EventResponseCreated, EventResponseCompleted]
            \response -> response.output `shouldBe` []

testPartialTerminalResponse
    :: [LBS.ByteString]
    -> [StreamEventType]
    -> (Response -> Expectation)
    -> Expectation
testPartialTerminalResponse inputFrames expectedTypes checkResponse = do
    frames <- newIORef inputFrames
    receiveCount <- newIORef (0 :: Int)
    completeCount <- newIORef (0 :: Int)
    invalidations <- newIORef ([] :: [Text])
    callbackTypes <- newIORef ([] :: [StreamEventType])

    let actions = WebSocketReceiveActions
            { receiveFrame = do
                modifyIORef' receiveCount (+ 1)
                atomicModifyIORef' frames \case
                    frame : rest -> (rest, Right frame)
                    [] -> error "unexpected receive after terminal event"
            , completeRequest = modifyIORef' completeCount (+ 1)
            , invalidateRequest = \reason ->
                modifyIORef' invalidations (<> [reason])
            }
        onEvent event =
            modifyIORef' callbackTypes (<> [responseStreamEventType event])

    result <- receiveWsResponseWithActions (Just "gpt-test") actions onEvent

    case result of
        Left err -> expectationFailure ("unexpected error: " <> show err)
        Right response -> do
            response.responseId `shouldBe` "resp-test"
            response.model `shouldBe` "gpt-test"
            response.object `shouldBe` "response"
            response.status `shouldBe` ResponseCompleted
            checkResponse response

    readIORef callbackTypes `shouldReturn` expectedTypes
    readIORef receiveCount `shouldReturn` length inputFrames
    readIORef completeCount `shouldReturn` 1
    readIORef invalidations `shouldReturn` []
    readIORef frames `shouldReturn` []

lifecycleFrame :: Text -> Aeson.Value -> LBS.ByteString
lifecycleFrame eventType responseValue = Aeson.encode $ Aeson.object
    [ "type" Aeson..= eventType
    , "response" Aeson..= responseValue
    ]

contextManagement :: CodexWsOptions -> Maybe Aeson.Value
contextManagement options =
    field "context_management" $
        buildWsPayloadWithOptions options sampleRequest (Just "previous-1")

field :: Key.Key -> Aeson.Value -> Maybe Aeson.Value
field name = \case
    Aeson.Object object -> KeyMap.lookup name object
    _ -> Nothing

sampleRequest :: ResponseCreateParams
sampleRequest = defaultResponseCreateParams
    { model = Just "gpt-test"
    , instructions = Just "test"
    , input = Just (ResponseInputItems [])
    , tools = Just []
    , reasoning = Just ReasoningConfig
        { context = Nothing
        , effort = Just "minimal"
        , generateSummary = Nothing
        , reasoningMode = Nothing
        , summary = Nothing
        , extraFields = mempty
        }
    , include = Just []
    , promptCacheKey = Just "cache-key"
    }
