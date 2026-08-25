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
import Data.Foldable (toList)
import Data.IORef
import Data.Text (Text)

spec :: Spec
spec = do
  describe "CodexTurnState" do
    it "keeps the first token through tool calls and clears it at turn end" do
        turnState <- newCodexTurnState
        recordCodexTurnState turnState " "
        recordCodexTurnState turnState "ts-first"
        recordCodexTurnState turnState "ts-later"
        readCodexTurnState turnState `shouldReturn` Just "ts-first"

        finishCodexTurnStateResponse turnState
            (responseWithOutput
                [ FunctionCallItem FunctionCall
                    { itemId = Nothing
                    , callId = "call-1"
                    , name = "shell_command"
                    , arguments = "{}"
                    , status = Just ItemCompleted
                    , extraFields = KeyMap.empty
                    }
                ])
        readCodexTurnState turnState `shouldReturn` Just "ts-first"

        finishCodexTurnStateResponse turnState
            (responseWithOutput [])
        readCodexTurnState turnState `shouldReturn` Nothing

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

    it "forces stream=true for the Codex WebSocket contract" do
        let request = sampleRequest { stream = Just False }
        field "stream" (buildWsPayloadWithOptions defaultCodexWsOptions request Nothing)
            `shouldBe` Just (Aeson.Bool True)

    it "marks Responses Lite requests in client metadata" do
        let payload =
                buildWsPayloadWithOptions
                    defaultCodexWsOptions sampleRequest Nothing
        (field "client_metadata" payload >>= field
            "ws_request_header_x_openai_internal_codex_responses_lite")
            `shouldBe` Just (Aeson.String "true")

        let generic = withModel (Just "gpt-generic") sampleRequest
        (field "client_metadata"
            (buildWsPayloadWithOptions
                defaultCodexWsOptions generic Nothing)
            >>= field
                "ws_request_header_x_openai_internal_codex_responses_lite")
            `shouldBe` Nothing

    it "strips prompt cache retention from Codex requests" do
        let request = withPromptCacheRetention (Just "24h") sampleRequest
            payload = buildWsPayloadWithOptions
                defaultCodexWsOptions request (Just "previous-1")
        field "prompt_cache_retention" payload `shouldBe` Nothing
        field "previous_response_id" payload
            `shouldBe` Just (Aeson.String "previous-1")

    it "strips Responses Lite content_item_kinds from the wire payload" do
        let payload = buildWsPayloadWithOptions
                defaultCodexWsOptions sampleLitePrefixRequest Nothing
        case field "input" payload of
            Just (Aeson.Array items) ->
                case toList items of
                    [Aeson.Object message] -> do
                        KeyMap.lookup
                            (Key.fromText
                                "internal_chat_message_metadata_passthrough")
                            message
                            `shouldBe` Nothing
                        field "role" (Aeson.Object message)
                            `shouldBe` Just (Aeson.String "developer")
                    other ->
                        expectationFailure
                            ("expected one developer message, got "
                                <> show other)
            other ->
                expectationFailure
                    ("expected input array, got " <> show other)

    it "does not request server-managed compaction by default" do
        contextManagement defaultCodexWsOptions `shouldBe` Nothing

    it "serializes a positive server-side compaction threshold" do
        let options = CodexWsOptions
                { compactThreshold = Just 180000
                , sendIdleTimeoutMicros = defaultCodexWsOptions.sendIdleTimeoutMicros
                , receiveIdleTimeoutMicros = defaultCodexWsOptions.receiveIdleTimeoutMicros
                }
        contextManagement options `shouldBe` Just (Aeson.toJSON
            [ Aeson.object
                [ "type" Aeson..= ("compaction" :: Text)
                , "compact_threshold" Aeson..= (180000 :: Int)
                ]
            ])

    it "omits non-positive thresholds" do
        let options = CodexWsOptions
                { compactThreshold = Just 0
                , sendIdleTimeoutMicros = defaultCodexWsOptions.sendIdleTimeoutMicros
                , receiveIdleTimeoutMicros = defaultCodexWsOptions.receiveIdleTimeoutMicros
                }
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

    it "skips malformed frames and continues to the terminal response" do
        testPartialTerminalResponse
            [ "{not-json"
            , lifecycleFrame "response.created"
                (Aeson.object ["id" Aeson..= ("resp-test" :: Text)])
            , lifecycleFrame "response.completed" (Aeson.object [])
            ]
            [EventResponseCreated, EventResponseCompleted]
            \response -> response.output `shouldBe` []

    it "rejects response.incomplete with the server reason" do
        frames <- newIORef
            [ lifecycleFrame "response.created"
                (Aeson.object ["id" Aeson..= ("resp-test" :: Text)])
            , lifecycleFrame "response.incomplete"
                (Aeson.object
                    [ "incomplete_details" Aeson..= Aeson.object
                        [ "reason" Aeson..=
                            ("max_output_tokens" :: Text)
                        ]
                    ])
            ]
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
                modifyIORef' callbackTypes
                    (<> [responseStreamEventType event])

        result <- receiveWsResponseWithActions
            (Just "gpt-test") actions onEvent

        result `shouldBe` Left
            (ConnectionError "response.incomplete: max_output_tokens")
        readIORef callbackTypes `shouldReturn`
            [EventResponseCreated, EventResponseIncomplete]
        readIORef receiveCount `shouldReturn` 2
        readIORef completeCount `shouldReturn` 0
        readIORef invalidations `shouldReturn`
            ["WebSocket response incomplete"]
        readIORef frames `shouldReturn` []

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

sampleLitePrefixRequest :: ResponseCreateParams
sampleLitePrefixRequest = sampleRequest
    { input = Just (ResponseInputItems
        [ MessageItem ResponseMessage
            { messageId = Nothing
            , content = MessageContentParts
                [InputTextPart "base instructions" Nothing KeyMap.empty]
            , role = RoleDeveloper
            , status = Nothing
            , phase = Nothing
            , extraFields = KeyMap.singleton
                (Key.fromText "internal_chat_message_metadata_passthrough")
                (Aeson.object
                    [ "content_item_kinds"
                        Aeson..= (["model.base_instructions"] :: [Text])
                    ])
            }
        ])
    }

sampleRequest :: ResponseCreateParams
sampleRequest = defaultResponseCreateParams
    { model = Just "gpt-5.6-sol"
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

withPromptCacheRetention
    :: Maybe Text -> ResponseCreateParams -> ResponseCreateParams
withPromptCacheRetention nextRetention
        ResponseCreateParams { promptCacheRetention = _, .. } =
    ResponseCreateParams { promptCacheRetention = nextRetention, .. }

withModel :: Maybe Text -> ResponseCreateParams -> ResponseCreateParams
withModel nextModel ResponseCreateParams { model = _, .. } =
    ResponseCreateParams { model = nextModel, .. }

responseWithOutput :: [ResponseItem] -> Response
responseWithOutput output =
    case Aeson.fromJSON $ Aeson.object
            [ "id" Aeson..= ("resp-test" :: Text)
            , "created_at" Aeson..= (0 :: Int)
            , "model" Aeson..= ("gpt-test" :: Text)
            , "status" Aeson..= ("completed" :: Text)
            , "output" Aeson..= output
            ] of
        Aeson.Success response -> response
        Aeson.Error err -> error err
