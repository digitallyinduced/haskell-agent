-- | Functional tests for the xAI client against an in-process HTTP mock.
module Agent.XAI.ClientSpec (spec) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Loop
    ( Backend(..)
    , BackendResult(..)
    , BackendSnapshot(..)
    , TurnInput(..)
    , advanceBackendSnapshot
    , emptyBackendSnapshot
    )
import qualified Agent.Responses.Codec as ResponsesCodec
import Agent.XAI.Client
import Agent.XAI.LoopBackend
import Agent.XAI.Options
import Agent.XAI.TestSupport (withLoopbackApplication)
import Agent.Provider (Credential(..), Provider(..))
import Agent.Responses.Types
import qualified Agent.Json.Decode as Json
import Control.Concurrent.Async (cancel, withAsync)
import Control.Concurrent.MVar
import Control.Exception.Safe (finally)
import Control.Monad (void, when)
import Control.Retry (constantDelay, limitRetries)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = do
    describe "createResponseWith" do
        it "POSTs the mapped request with subscription headers and parses the SSE response" do
            recorded <- newIORef []
            let handler _request = do
                    pure $ sseResponse
                        [ outputItemDone (assistantMessage "hello world")
                        , completedEvent "resp-1" []
                        ]
            withMockGrok recorded handler \options -> do
                result <- createResponseWith options (xaiCredential "token-a") (helloRequest "hi")
                response <- expectRight result
                response.responseId `shouldBe` "resp-1"
                extractAssistantText response `shouldBe` Just "hello world"

            [request] <- readIORef recorded
            request.path `shouldBe` "/v1/responses"
            lookup "Authorization" request.headers `shouldBe` Just "Bearer token-a"
            lookup "X-XAI-Token-Auth" request.headers `shouldBe` Just grokTokenAuthValue
            lookup "x-authenticateresponse" request.headers
                `shouldBe` Just grokAuthenticateResponseValue
            lookup "x-grok-client-identifier" request.headers
                `shouldBe` Just grokClientIdentifier
            lookup "x-grok-client-version" request.headers
                `shouldBe` Just defaultGrokClientVersion
            lookup "x-grok-client-mode" request.headers
                `shouldBe` Just "interactive"
            lookup "User-Agent" request.headers
                `shouldBe` Just (grokUserAgent defaultGrokClientVersion)
            lookup "x-compaction-at" request.headers
                `shouldBe` Just "400000"
            lookup "x-compactions-remaining" request.headers
                `shouldBe` Just "1"
            requestModel request `shouldBe` Just "grok-4.6"
            -- instructions travel as the leading system item
            requestInputRoles request `shouldBe` Just ["system", "user"]

        it "applies an explicit threshold to the server compaction hint" do
            recorded <- newIORef []
            let handler _request = pure $ sseResponse
                    [ outputItemDone (assistantMessage "hello")
                    , completedEvent "resp-override" []
                    ]
            withMockGrok recorded handler \options -> do
                result <- createResponseWith
                    options { autoCompactTokenLimit = Just 450_000 }
                    (xaiCredential "token-a")
                    (helloRequest "hi")
                void (expectRight result)

            [sent] <- readIORef recorded
            lookup "x-compaction-at" sent.headers `shouldBe` Just "450000"
            lookup "x-compactions-remaining" sent.headers `shouldBe` Just "1"

        it "applies the threshold resolved from an expanded context window" do
            recorded <- newIORef []
            let handler _request = pure $ sseResponse
                    [ outputItemDone (assistantMessage "hello")
                    , completedEvent "resp-expanded-context" []
                    ]
                resolvedThreshold =
                    grokAutoCompactTokenLimit "grok-4.6" 1_000_000
            withMockGrok recorded handler \options -> do
                result <- createResponseWith
                    options
                        { autoCompactTokenLimit =
                            Just resolvedThreshold
                        }
                    (xaiCredential "token-a")
                    (helloRequest "hi")
                void (expectRight result)

            [sent] <- readIORef recorded
            lookup "x-compaction-at" sent.headers `shouldBe` Just "800000"
            lookup "x-compactions-remaining" sent.headers `shouldBe` Just "1"

        it "omits x-compaction-at after a local compaction checkpoint" do
            recorded <- newIORef []
            let handler _request = pure $ sseResponse
                    [ outputItemDone (assistantMessage "continued")
                    , completedEvent "resp-compacted" []
                    ]
                checkpoint = MessageItem ResponseMessage
                    { messageId = Nothing
                    , content = MessageContentParts
                        [ OutputTextPart
                            "Compacted conversation summary:\nretained state"
                            Nothing
                            Nothing
                        ]
                    , role = RoleAssistant
                    , status = Nothing
                    , phase = Nothing
                    , passthrough = Just InternalChatMetadata
                        { turnId = Nothing
                        , createTime = Nothing
                        , contentItemKinds =
                            Just [localCompactionSummaryContentItemKind]
                        , executedToolCalls = Nothing
                        }
                    }
                request = (helloRequest "continue")
                    { model = Just "grok-4.6"
                    , input = Just (ResponseInputItems [checkpoint])
                    }
            withMockGrok recorded handler \options -> do
                result <- createResponseWith
                    options
                    (xaiCredential "token-a")
                    request
                void (expectRight result)

            [sent] <- readIORef recorded
            lookup "x-compaction-at" sent.headers `shouldBe` Nothing
            lookup "x-compactions-remaining" sent.headers `shouldBe` Just "1"
            BS.isInfixOf
                (Text.encodeUtf8 localCompactionSummaryContentItemKind)
                (LBS.toStrict sent.body)
                `shouldBe` False

        it "keeps x-compaction-at for ordinary assistant text with the summary heading" do
            recorded <- newIORef []
            let handler _request = pure $ sseResponse
                    [ outputItemDone (assistantMessage "continued")
                    , completedEvent "resp-ordinary-summary-heading" []
                    ]
                ordinaryReply = MessageItem ResponseMessage
                    { messageId = Nothing
                    , content = MessageContentParts
                        [ OutputTextPart
                            "Compacted conversation summary:\nuser-visible reply"
                            Nothing
                            Nothing
                        ]
                    , role = RoleAssistant
                    , status = Nothing
                    , phase = Nothing
                    , passthrough = Nothing
                    }
                request = (helloRequest "continue")
                    { model = Just "grok-4.6"
                    , input = Just (ResponseInputItems [ordinaryReply])
                    }
            withMockGrok recorded handler \options -> do
                result <- createResponseWith
                    options
                    (xaiCredential "token-a")
                    request
                void (expectRight result)

            [sent] <- readIORef recorded
            lookup "x-compaction-at" sent.headers `shouldBe` Just "400000"
            lookup "x-compactions-remaining" sent.headers `shouldBe` Just "1"

        it "strips checkpoint provenance before sending the request" do
            recorded <- newIORef []
            let handler _request = pure $ sseResponse
                    [ outputItemDone (assistantMessage "continued")
                    , completedEvent "resp-origin-stripped" []
                    ]
                checkpoint =
                    ContextCompactionItemValue ContextCompactionItem
                        { itemId = Just "xai-context"
                        , encryptedContent = Just "opaque"
                        }
                request = (helloRequest "continue")
                    { model = Just "grok-4.6"
                    , input = Just
                        (ResponseInputItems
                            [ checkpoint
                            , xaiCompactionCheckpointOriginItem
                            ])
                    }
                markerEncoding =
                    LBS.toStrict
                        (Aeson.encode
                            xaiCompactionCheckpointOriginItem)
            withMockGrok recorded handler \options -> do
                result <- createResponseWith
                    options
                    (xaiCredential "token-a")
                    request
                void (expectRight result)

            [sent] <- readIORef recorded
            BS.isInfixOf markerEncoding (LBS.toStrict sent.body)
                `shouldBe` False
            BS.isInfixOf
                "\"type\":\"context_compaction\""
                (LBS.toStrict sent.body)
                `shouldBe` True

        it "does not invent server compaction metadata for unknown Grok models" do
            recorded <- newIORef []
            let handler _request = pure $ sseResponse
                    [ outputItemDone (assistantMessage "future")
                    , completedEvent "resp-future" []
                    ]
                request = (helloRequest "hi")
                    { model = Just "grok-future"
                    , input = Just (ResponseInputText "hi")
                    }
            withMockGrok recorded handler \options -> do
                result <- createResponseWith
                    options
                    (xaiCredential "token-a")
                    request
                void (expectRight result)

            [sent] <- readIORef recorded
            requestModel sent `shouldBe` Just "grok-future"
            lookup "x-compaction-at" sent.headers `shouldBe` Nothing
            lookup "x-compactions-remaining" sent.headers `shouldBe` Nothing

        it "streams callbacks before the response completes" do
            recorded <- newIORef []
            callbackSeen <- newEmptyMVar
            serverSawCallback <- newIORef False
            let handler _ = pure $
                    streamingResponse callbackSeen serverSawCallback
            withMockGrok recorded handler \options -> do
                result <- createResponseWithEvents options
                    (xaiCredential "token-a")
                    (helloRequest "hi")
                    (recordOutputItem callbackSeen)
                response <- expectRight result
                response.responseId `shouldBe` "resp-stream"
            readIORef serverSawCallback `shouldReturn` True

        it "propagates cancellation promptly while a streamed response remains open" do
            recorded <- newIORef []
            callbackSeen <- newEmptyMVar
            serverRelease <- newEmptyMVar
            let handler _ = pure $
                    cancellableStreamingResponse callbackSeen serverRelease
            withMockGrok recorded handler \options ->
                flip finally (void (tryPutMVar serverRelease ())) $
                    withAsync
                        (createResponseWithEvents options
                            (xaiCredential "token-a")
                            (helloRequest "hi")
                            (recordOutputItem callbackSeen))
                        \worker -> do
                            seen <- timeout 2_000_000 (readMVar callbackSeen)
                            seen `shouldBe` Just ()
                            stopped <- timeout 2_000_000 (cancel worker)
                            stopped `shouldBe` Just ()

        it "aborts a stalled streaming body instead of hanging forever" do
            recorded <- newIORef []
            serverRelease <- newEmptyMVar
            let handler _ = pure (stalledStreamingResponse serverRelease)
            (withMockGrokTimeout 1 recorded handler \options -> do
                result <- timeout 5_000_000 $
                    createResponseWith options
                        (xaiCredential "token-a")
                        (helloRequest "hi")
                case result of
                    Nothing ->
                        expectationFailure
                            "client hung on a stalled stream despite the timeout"
                    Just (Left (ConnectionError _)) -> pure ()
                    Just other ->
                        expectationFailure
                            ("expected a ConnectionError, got " <> show other))
                `finally` putMVar serverRelease ()

    describe "xaiBackendWith" do
        it "marks newly emitted server checkpoints with xAI provenance" do
            let checkpoint =
                    ContextCompactionItemValue ContextCompactionItem
                        { itemId = Just "xai-context"
                        , encryptedContent = Just "opaque"
                        }
                backend =
                    xaiBackendWith
                        (\_request _onEvent ->
                            pure
                                (Right
                                    (responseWithTypedOutput [checkpoint])))
                        (pure defaultResponseCreateParams)
            result <- backend.submitTurn
                emptyBackendSnapshot
                Nothing
                [UserMessage "continue"]
                (const (pure ()))
            fmap (.backendState.backendItems) result
                `shouldBe`
                    Right
                        [ checkpoint
                        , xaiCompactionCheckpointOriginItem
                        ]

        it "does not claim provenance for checkpoints retained from a request" do
            let foreignCheckpoint =
                    CompactionItemValue CompactionItem
                        { itemId = Just "openai-checkpoint"
                        , encryptedContent = Just "opaque-openai"
                        }
                answer = MessageItem ResponseMessage
                    { messageId = Just "answer"
                    , content = MessageContentParts
                        [OutputTextPart "continued" Nothing Nothing]
                    , role = RoleAssistant
                    , status = Nothing
                    , phase = Nothing
                    , passthrough = Nothing
                    }
                backend =
                    xaiBackendWith
                        (\_request _onEvent ->
                            pure
                                (Right
                                    (responseWithTypedOutput [answer])))
                        (pure defaultResponseCreateParams)
                snapshot =
                    advanceBackendSnapshot
                        emptyBackendSnapshot
                        [foreignCheckpoint]
                        Nothing
            result <- backend.submitTurn
                snapshot
                Nothing
                [UserMessage "continue"]
                (const (pure ()))
            case result of
                Left err ->
                    expectationFailure
                        ("expected Right, got Left " <> show err)
                Right completed -> do
                    completed.backendState.backendItems
                        `shouldSatisfy` elem foreignCheckpoint
                    completed.backendState.backendItems
                        `shouldSatisfy`
                            not
                                . any
                                    isXaiCompactionCheckpointOriginItem

    describe "retry boundaries" do
        it "reports a terminal stream failure after one request" do
            recorded <- newIORef []
            let handler _request = pure $ sseResponse
                    [ outputItemDone (assistantMessage "partial answer")
                    , sseEvent "response.failed" $ Aeson.object
                        [ "type" Aeson..= ("response.failed" :: Text)
                        , "response" Aeson..= Aeson.object
                            [ "id" Aeson..= ("resp-failed" :: Text)
                            , "created_at" Aeson..= (0 :: Int)
                            , "model" Aeson..= ("grok-4.6" :: Text)
                            , "status" Aeson..= ("failed" :: Text)
                            , "incomplete_details" Aeson..= Aeson.object
                                ["reason" Aeson..= ("overloaded" :: Text)]
                            ]
                        ]
                    ]
            withMockGrok recorded handler \options -> do
                result <- createResponseWith options (xaiCredential "token-a") (helloRequest "hi")
                case result of
                    Left ConnectionError{} -> pure ()
                    other -> expectationFailure ("expected ConnectionError, got " <> show other)

            requests <- readIORef recorded
            length requests `shouldBe` 1

        it "retries a transient HTTP failure before streaming starts" do
            recorded <- newIORef []
            attempts <- newIORef (0 :: Int)
            let handler _request = do
                    n <- atomicModifyIORef' attempts \i -> (i + 1, i + 1)
                    pure $ if n == 1
                        then Wai.responseLBS HTTP.status503
                            [("Content-Type", "text/plain")]
                            "temporarily unavailable"
                        else sseResponse
                            [ outputItemDone (assistantMessage "hello after capacity")
                            , completedEvent "resp-retry" []
                            ]
            withMockGrok recorded handler \options -> do
                result <- createResponseWithEventsPolicy
                    (constantDelay 0 <> limitRetries 3)
                    options
                    (xaiCredential "token-a")
                    (helloRequest "hi")
                    (const (pure ()))
                response <- expectRight result
                response.responseId `shouldBe` "resp-retry"

            requests <- readIORef recorded
            length requests `shouldBe` 2

        it "retries capacity stream errors when callbacks are disabled" do
            recorded <- newIORef []
            attempts <- newIORef (0 :: Int)
            let capacityMessage :: Text
                capacityMessage =
                    "The model is currently at capacity due to high demand. \
                    \Please try again in a few minutes, or use a higher service \
                    \tier for priority processing"
                handler _request = do
                    attempt <- atomicModifyIORef' attempts \current ->
                        let next = current + 1
                        in (next, next)
                    pure $ if attempt == 1
                        then sseResponse
                            [ sseEvent "error" $ Aeson.object
                                [ "type" Aeson..= ("error" :: Text)
                                , "error" Aeson..= Aeson.object
                                    [ "message" Aeson..= capacityMessage
                                    ]
                                ]
                            ]
                        else sseResponse
                            [ outputItemDone (assistantMessage "after retry")
                            , completedEvent "resp-stream-retry" []
                            ]
            withMockGrok recorded handler \options -> do
                result <- createResponseWithPolicy
                    (constantDelay 0 <> limitRetries 3)
                    options
                    (xaiCredential "token-a")
                    (helloRequest "hi")
                response <- expectRight result
                response.responseId `shouldBe` "resp-stream-retry"
            length <$> readIORef recorded `shouldReturn` 2

        it "does not retry a capacity stream error after a callback" do
            recorded <- newIORef []
            callbacks <- newIORef (0 :: Int)
            let capacityMessage :: Text
                capacityMessage =
                    "The model is currently at capacity due to high demand"
                handler _request = pure $ sseResponse
                    [ sseEvent "error" $ Aeson.object
                        [ "type" Aeson..= ("error" :: Text)
                        , "error" Aeson..= Aeson.object
                            [ "message" Aeson..= capacityMessage
                            ]
                        ]
                    ]
            withMockGrok recorded handler \options -> do
                result <- createResponseWithEventsPolicy
                    (constantDelay 0 <> limitRetries 3)
                    options
                    (xaiCredential "token-a")
                    (helloRequest "hi")
                    (const (modifyIORef' callbacks (+ 1)))
                result `shouldBe`
                    Left
                        (ProviderError
                            OverloadedError
                            capacityMessage
                            (Just 30))
            length <$> readIORef recorded `shouldReturn` 1
            readIORef callbacks `shouldReturn` 1

        it "does not retry when the stream callback throws" do
            recorded <- newIORef []
            callbacks <- newIORef (0 :: Int)
            let handler _request = pure $ sseResponse
                    [ outputItemDone (assistantMessage "hello")
                    , completedEvent "resp-callback" []
                    ]
            withMockGrok recorded handler \options -> do
                result <- createResponseWithEventsPolicy
                    (constantDelay 0 <> limitRetries 3)
                    options
                    (xaiCredential "token-a")
                    (helloRequest "hi")
                    (\_ -> modifyIORef' callbacks (+ 1)
                        >> ioError (userError "callback failed"))
                case result of
                    Left ConnectionError{} -> pure ()
                    other -> expectationFailure
                        ("expected callback ConnectionError, got " <> show other)
            length <$> readIORef recorded `shouldReturn` 1
            readIORef callbacks `shouldReturn` 1

        it "retries capacity Left values under a zero-delay policy" do
            let capacity = ProviderError OverloadedError
                    "The model is currently at capacity due to high demand"
                    (Just 30)
            responses <- newIORef
                [ Left capacity
                , Left capacity
                , Right ("completed" :: Text)
                ]
            result <- retryTransientXaiResultWithPolicy
                (constantDelay 0 <> limitRetries 3)
                (atomicModifyIORef' responses \case
                    next : rest -> (rest, next)
                    [] -> error "unexpected extra xAI request")
            result `shouldBe` Right "completed"
            readIORef responses `shouldReturn` []

        it "does not retry quota errors" do
            attempts <- newIORef (0 :: Int)
            let quota = ProviderError UsageLimitReached "quota" (Just 3600)
            result <- retryTransientXaiResultWithPolicy
                (constantDelay 0 <> limitRetries 3)
                (modifyIORef' attempts (+ 1)
                    >> pure (Left quota :: Either ApiError Text))
            result `shouldBe` Left quota
            readIORef attempts `shouldReturn` 1

    describe "Grok automatic compaction policy" do
        it "uses the current 80% model override and 85% fallback" do
            grokAutoCompactTokenLimit "grok-4.6" 500_000
                `shouldBe` 400_000
            grokAutoCompactTokenLimit "grok-4.5" 500_000
                `shouldBe` 400_000
            grokAutoCompactTokenLimit "grok-future" 500_000
                `shouldBe` 425_000

--------------------------------------------------------------------------------
-- Mock server
--------------------------------------------------------------------------------

data RecordedRequest = RecordedRequest
    { path :: !Text
    , headers :: ![(Text, Text)]
    , body :: !LBS.ByteString
    }

withMockGrok
    :: IORef [RecordedRequest]
    -> (RecordedRequest -> IO Wai.Response)
    -> (ClientOptions -> IO a)
    -> IO a
withMockGrok = withMockGrokTimeout 10

withMockGrokTimeout
    :: Int
    -> IORef [RecordedRequest]
    -> (RecordedRequest -> IO Wai.Response)
    -> (ClientOptions -> IO a)
    -> IO a
withMockGrokTimeout timeoutSecs recorded handler action =
    withLoopbackApplication (pure app) \port ->
        action defaultClientOptions
            { baseUrl = "http://127.0.0.1:" <> show port <> "/v1"
            , requestTimeoutSeconds = timeoutSecs
            }
  where
    app waiRequest respond = do
        requestBody <- Wai.strictRequestBody waiRequest
        let request = RecordedRequest
                { path = "/" <> Text.intercalate "/" (Wai.pathInfo waiRequest)
                , headers =
                    [ (Text.decodeUtf8 (CI.original name), Text.decodeUtf8 value)
                    | (name, value) <- Wai.requestHeaders waiRequest
                    ]
                , body = requestBody
                }
        atomicModifyIORef' recorded \requests -> (requests <> [request], ())
        respond =<< handler request

sseResponse :: [Text] -> Wai.Response
sseResponse events = Wai.responseLBS HTTP.status200
    [("Content-Type", "text/event-stream")]
    (LBS.fromStrict (Text.encodeUtf8 (Text.concat events)))

recordOutputItem :: MVar () -> ResponseStreamEvent -> IO ()
recordOutputItem callbackSeen event =
    when (responseStreamEventType event == EventOutputItemDone) do
        tryPutMVar callbackSeen ()
        pure ()

streamingResponse :: MVar () -> IORef Bool -> Wai.Response
streamingResponse callbackSeen serverSawCallback =
    Wai.responseStream HTTP.status200
        [("Content-Type", "text/event-stream")]
        \write flush -> do
            writeSse write (outputItemDone (assistantMessage "hello"))
            flush
            seen <- timeout 2_000_000 (readMVar callbackSeen)
            writeIORef serverSawCallback (maybe False (const True) seen)
            writeSse write (completedEvent "resp-stream" [])
            flush

-- | Sends response headers and one SSE event, then stalls forever (until the
-- test releases it). Models a connection that dies without FIN/RST mid-stream:
-- headers arrive, so http-client's responseTimeout is satisfied, and only the
-- body-read timeout can rescue the turn.
stalledStreamingResponse :: MVar () -> Wai.Response
stalledStreamingResponse release =
    Wai.responseStream HTTP.status200
        [("Content-Type", "text/event-stream")]
        \write flush -> do
            writeSse write (outputItemDone (assistantMessage "partial"))
            flush
            readMVar release

cancellableStreamingResponse :: MVar () -> MVar () -> Wai.Response
cancellableStreamingResponse callbackSeen serverRelease =
    Wai.responseStream HTTP.status200
        [("Content-Type", "text/event-stream")]
        \write flush -> do
            writeSse write (outputItemDone (assistantMessage "hello"))
            flush
            readMVar callbackSeen
            readMVar serverRelease

writeSse :: (Builder.Builder -> IO ()) -> Text -> IO ()
writeSse write = write . Builder.byteString . Text.encodeUtf8

outputItemDone :: Aeson.Value -> Text
outputItemDone item = sseEvent "response.output_item.done" $ Aeson.object
    [ "type" Aeson..= ("response.output_item.done" :: Text)
    , "output_index" Aeson..= (0 :: Int)
    , "item" Aeson..= item
    ]

completedEvent :: Text -> [Aeson.Value] -> Text
completedEvent responseId output = sseEvent "response.completed" $ Aeson.object
    [ "type" Aeson..= ("response.completed" :: Text)
    , "response" Aeson..= Aeson.object
        [ "id" Aeson..= responseId
        , "created_at" Aeson..= (0 :: Int)
        , "model" Aeson..= ("grok-4.6" :: Text)
        , "status" Aeson..= ("completed" :: Text)
        , "output" Aeson..= output
        , "usage" Aeson..= Aeson.object
            [ "input_tokens" Aeson..= (10 :: Int)
            , "output_tokens" Aeson..= (5 :: Int)
            , "total_tokens" Aeson..= (15 :: Int)
            ]
        ]
    ]

sseEvent :: Text -> Aeson.Value -> Text
sseEvent eventType payload =
    "event: " <> eventType <> "\ndata: "
        <> Text.decodeUtf8 (LBS.toStrict (Aeson.encode payload))
        <> "\n\n"

assistantMessage :: Text -> Aeson.Value
assistantMessage text = Aeson.object
    [ "type" Aeson..= ("message" :: Text)
    , "role" Aeson..= ("assistant" :: Text)
    , "content" Aeson..= [Aeson.object
        [ "type" Aeson..= ("output_text" :: Text)
        , "text" Aeson..= text
        ]]
    ]

--------------------------------------------------------------------------------
-- Providers and credentials
--------------------------------------------------------------------------------

xaiCredential :: Text -> Credential
xaiCredential token = Credential
    { accessToken = token
    , accountId = "xai-" <> token
    , leaseId = Nothing
    , provider = XAIProvider
    }

--------------------------------------------------------------------------------
-- Request/response helpers
--------------------------------------------------------------------------------

helloRequest :: Text -> ResponseCreateParams
helloRequest prompt = defaultResponseCreateParams
    { model = Just "gpt-5.6-terra"
    , instructions = Just "You are a test agent."
    , input = Just (ResponseInputText prompt)
    , tools = Just []
    , reasoning = Just (ReasoningConfig Nothing (Just "low") Nothing Nothing Nothing)
    , include = Just [ResponseInclude "reasoning.encrypted_content"]
    }

extractAssistantText :: Response -> Maybe Text
extractAssistantText response = case
    [ value
    | MessageItem message <- response.output
    , message.role == RoleAssistant
    , value <- case message.content of
        MessageContentText text -> [text]
        MessageContentParts parts -> [text | OutputTextPart { text } <- parts]
    ] of
        [] -> Nothing
        values -> Just (Text.intercalate "\n" values)

requestModel :: RecordedRequest -> Maybe Text
requestModel request =
    either (const Nothing) Just $
        Json.decodeEither
            (Json.object (Json.atKey "model" Json.text))
            (LBS.toStrict request.body)

requestInputRoles :: RecordedRequest -> Maybe [Text]
requestInputRoles request =
    either (const Nothing) Just $
        Json.decodeEither
            (Json.object
                (Json.atKey "input"
                    (Json.list
                        (Json.object (Json.atKey "role" Json.text)))))
            (LBS.toStrict request.body)

expectRight :: Show e => Either e a -> IO a
expectRight = \case
    Left err -> expectationFailure ("expected Right, got Left " <> show err) >> fail "unreachable"
    Right value -> pure value

responseWithTypedOutput :: [ResponseItem] -> Response
responseWithTypedOutput output =
    either error id
        . ResponsesCodec.decodeResponse
        . LBS.toStrict
        . Aeson.encode
        $ Aeson.object
            [ "id" Aeson..= ("resp-backend" :: Text)
            , "created_at" Aeson..= (0 :: Int)
            , "model" Aeson..= ("grok-4.6" :: Text)
            , "status" Aeson..= ("completed" :: Text)
            , "output" Aeson..= output
            ]
