module Agent.OpenAI.LoopBackendSpec.RecoverySpec (spec) where

import Agent.Error
    ( ApiError(..)
    , ErrorType(..)
    , isInlineRetryableProviderError
    )
import Agent.Loop
import Agent.OpenAI.LoopBackend
import Agent.Responses.LoopBackend (streamOutputObserved)
import Agent.Responses.Types
import Control.Retry (constantDelay, limitRetries)
import qualified Data.Aeson as Aeson
import Data.IORef
import qualified Data.Text as Text
import Test.Hspec
import Agent.OpenAI.LoopBackendSpec.Fixtures

spec :: Spec
spec = do
    describe "openAiBackendWithConnectionRecovery" do
        it "keeps auxiliary requests on the healthy reusable connection" do
            currentCalls <- newIORef (0 :: Int)
            freshCalls <- newIORef (0 :: Int)
            healthy <- newIORef True
            let response = testResponse "resp-current" [assistantItem "ok"]
                sendCurrent _request _previous _onEvent = do
                    modifyIORef' currentCalls (+ 1)
                    pure (Right response)
                sendFresh _failure _request _previous _onEvent = do
                    modifyIORef' freshCalls (+ 1)
                    pure (Right (testResponse "resp-fresh" [assistantItem "no"]))
                sender =
                    openAiAuxiliaryResponseSenderWithConnectionRecovery
                        healthy
                        sendCurrent
                        sendFresh
            sender baseParams Nothing (const (pure ()))
                `shouldReturn` Right response
            readIORef healthy `shouldReturn` True
            readIORef currentCalls `shouldReturn` 1
            readIORef freshCalls `shouldReturn` 0

        it "exposes the same current/fresh recovery to auxiliary requests" do
            currentCalls <- newIORef (0 :: Int)
            freshCalls <- newIORef (0 :: Int)
            failures <- newIORef []
            healthy <- newIORef True
            let connectionFailure = ConnectionError "socket closed"
            let sendCurrent _request _previous _onEvent = do
                    modifyIORef' currentCalls (+ 1)
                    pure (Left connectionFailure)
                sendFresh failure _request _previous _onEvent = do
                    modifyIORef' freshCalls (+ 1)
                    modifyIORef' failures (<> [failure])
                    pure $ Right (testResponse "resp-fresh" [assistantItem "ok"])
                sender =
                    openAiAuxiliaryResponseSenderWithConnectionRecovery
                        healthy
                        sendCurrent
                        sendFresh
            first <- sender baseParams Nothing (const (pure ()))
            second <- sender baseParams Nothing (const (pure ()))
            first `shouldBe`
                Right (testResponse "resp-fresh" [assistantItem "ok"])
            second `shouldBe`
                Right (testResponse "resp-fresh" [assistantItem "ok"])
            readIORef healthy `shouldReturn` False
            readIORef currentCalls `shouldReturn` 1
            readIORef freshCalls `shouldReturn` 2
            readIORef failures `shouldReturn`
                [Just connectionFailure, Nothing]

        it "does not replay auxiliary requests after an output item arrived" do
            freshCalls <- newIORef (0 :: Int)
            healthy <- newIORef True
            let connectionFailure = ConnectionError "socket closed"
                outputEvent = ResponseOutputItemDoneEvent
                    { item = assistantItem "partial"
                    , outputIndex = Just 0
                    , sequenceNumber = Nothing
                    }
                sendCurrent _request _previous onEvent = do
                    onEvent outputEvent
                    pure (Left connectionFailure)
                sendFresh _failure _request _previous _onEvent = do
                    modifyIORef' freshCalls (+ 1)
                    pure $ Right (testResponse "resp-fresh" [assistantItem "ok"])
                sender =
                    openAiAuxiliaryResponseSenderWithConnectionRecovery
                        healthy
                        sendCurrent
                        sendFresh
            sender baseParams Nothing (const (pure ()))
                `shouldReturn` Left
                    (replayUnsafeAuxiliaryFailure connectionFailure)
            readIORef healthy `shouldReturn` False
            readIORef freshCalls `shouldReturn` 0

        it "does not replay after a terminal auxiliary response arrived" do
            freshCalls <- newIORef (0 :: Int)
            healthy <- newIORef True
            let connectionFailure = ConnectionError "decode failed"
                completedEvent = ResponseCompletedEvent
                    { responseValue =
                        testResponse "resp-completed" [assistantItem "done"]
                    , sequenceNumber = Nothing
                    }
                sendCurrent _request _previous onEvent = do
                    onEvent completedEvent
                    pure (Left connectionFailure)
                sendFresh _failure _request _previous _onEvent = do
                    modifyIORef' freshCalls (+ 1)
                    pure $ Right
                        (testResponse "resp-fresh" [assistantItem "duplicate"])
                sender =
                    openAiAuxiliaryResponseSenderWithConnectionRecovery
                        healthy
                        sendCurrent
                        sendFresh
            sender baseParams Nothing (const (pure ()))
                `shouldReturn` Left
                    (replayUnsafeAuxiliaryFailure connectionFailure)
            readIORef freshCalls `shouldReturn` 0

        it "does not replay a failed auxiliary response with partial output" do
            freshCalls <- newIORef (0 :: Int)
            healthy <- newIORef True
            let connectionFailure = ConnectionError "failed response"
                failedEvent = ResponseFailedEvent
                    { responseValue =
                        testResponse "resp-failed" [assistantItem "partial"]
                    , sequenceNumber = Nothing
                    }
                sendCurrent _request _previous onEvent = do
                    onEvent failedEvent
                    pure (Left connectionFailure)
                sendFresh _failure _request _previous _onEvent = do
                    modifyIORef' freshCalls (+ 1)
                    pure $ Right
                        (testResponse "resp-fresh" [assistantItem "duplicate"])
                sender =
                    openAiAuxiliaryResponseSenderWithConnectionRecovery
                        healthy
                        sendCurrent
                        sendFresh
            sender baseParams Nothing (const (pure ()))
                `shouldReturn` Left
                    (replayUnsafeAuxiliaryFailure connectionFailure)
            readIORef freshCalls `shouldReturn` 0

        it "marks output from an initially fresh auxiliary connection as replay-unsafe" do
            currentCalls <- newIORef (0 :: Int)
            freshCalls <- newIORef (0 :: Int)
            healthy <- newIORef False
            let connectionFailure = ConnectionError "fresh socket closed"
                outputEvent = ResponseOutputItemDoneEvent
                    { item = assistantItem "partial"
                    , outputIndex = Just 0
                    , sequenceNumber = Nothing
                    }
                sendCurrent _request _previous _onEvent = do
                    modifyIORef' currentCalls (+ 1)
                    pure (Left (ConnectionError "unexpected current request"))
                sendFresh _failure _request _previous onEvent = do
                    modifyIORef' freshCalls (+ 1)
                    onEvent outputEvent
                    pure (Left connectionFailure)
                sender =
                    openAiAuxiliaryResponseSenderWithConnectionRecovery
                        healthy
                        sendCurrent
                        sendFresh
            sender baseParams Nothing (const (pure ()))
                `shouldReturn` Left
                    (replayUnsafeAuxiliaryFailure connectionFailure)
            readIORef currentCalls `shouldReturn` 0
            readIORef freshCalls `shouldReturn` 1

        it "marks fresh auxiliary output after a pre-output current failure" do
            freshCalls <- newIORef (0 :: Int)
            healthy <- newIORef True
            let currentFailure = ConnectionError "current socket closed"
                freshFailure = ConnectionError "fresh socket closed"
                outputEvent = ResponseOutputItemDoneEvent
                    { item = assistantItem "partial"
                    , outputIndex = Just 0
                    , sequenceNumber = Nothing
                    }
                sendCurrent _request _previous _onEvent =
                    pure (Left currentFailure)
                sendFresh failure _request _previous onEvent = do
                    failure `shouldBe` Just currentFailure
                    modifyIORef' freshCalls (+ 1)
                    onEvent outputEvent
                    pure (Left freshFailure)
                sender =
                    openAiAuxiliaryResponseSenderWithConnectionRecovery
                        healthy
                        sendCurrent
                        sendFresh
            sender baseParams Nothing (const (pure ()))
                `shouldReturn` Left
                    (replayUnsafeAuxiliaryFailure freshFailure)
            readIORef healthy `shouldReturn` False
            readIORef freshCalls `shouldReturn` 1

        it "replays on a fresh connection when the reusable socket dies before output" do
            currentCalls <- newIORef (0 :: Int)
            freshCalls <- newIORef (0 :: Int)
            healthy <- newIORef True
            transcript <- newIORef []
            let sendCurrent _request _previous _onEvent = do
                    modifyIORef' currentCalls (+ 1)
                    pure $ Left $ ConnectionError "socket closed"
                sendFresh _failure _request _previous _onEvent = do
                    modifyIORef' freshCalls (+ 1)
                    pure $ Right (testResponse "resp-fresh" [assistantItem "ok"])
                backend = openAiBackendWithConnectionRecovery
                    healthy sendCurrent sendFresh (pure baseParams)
            first <- submitWithState transcript backend Nothing [UserMessage "one"] (const (pure ()))
            second <- submitWithState transcript backend (Just "resp-fresh")
                [UserMessage "two"] (const (pure ()))
            first `shouldBe` Right (emptyTurnOutput "resp-fresh" [] (Just "ok"))
            second `shouldBe` Right (emptyTurnOutput "resp-fresh" [] (Just "ok"))
            readIORef healthy `shouldReturn` False
            readIORef currentCalls `shouldReturn` 1
            readIORef freshCalls `shouldReturn` 2

        it "still replays after an informational Codex rate-limit warning" do
            freshCalls <- newIORef (0 :: Int)
            healthy <- newIORef True
            transcript <- newIORef []
            events <- newIORef []
            let rateLimitsEvent =
                    codexRateLimitsEvent $ Aeson.object
                        [ "primary" Aeson..= Aeson.object
                            [ "used_percent" Aeson..= (92 :: Int)
                            ]
                        ]
                sendCurrent _request _previous onEvent = do
                    onEvent rateLimitsEvent
                    pure $ Left $ ConnectionError "socket closed"
                sendFresh _failure _request _previous _onEvent = do
                    modifyIORef' freshCalls (+ 1)
                    pure $ Right (testResponse "resp-fresh" [assistantItem "ok"])
                backend = openAiBackendWithConnectionRecovery
                    healthy sendCurrent sendFresh (pure baseParams)
            result <- submitWithState transcript backend Nothing [UserMessage "one"]
                (modifyIORef' events . (:))
            result `shouldBe` Right (emptyTurnOutput "resp-fresh" [] (Just "ok"))
            reverse <$> readIORef events `shouldReturn`
                [ WarningRaised
                    "Codex usage is low: primary 8% left. Check /usage for reset details."
                ]
            readIORef healthy `shouldReturn` False
            readIORef freshCalls `shouldReturn` 1

        it "reconnects behind a restart boundary after loop-visible output streamed" do
            freshCalls <- newIORef (0 :: Int)
            healthy <- newIORef True
            transcript <- newIORef []
            events <- newIORef []
            let sendCurrent _request _previous onEvent = do
                    onEvent (deltaEvent EventOutputTextDelta "partial")
                    pure $ Left $ ConnectionError "socket closed"
                sendFresh _failure _request _previous _onEvent = do
                    modifyIORef' freshCalls (+ 1)
                    pure $ Right (testResponse "resp-fresh" [assistantItem "ok"])
                streamingBackend = openAiBackendWithConnectionRecovery
                    healthy sendCurrent sendFresh (pure baseParams)
            result <- submitWithState transcript streamingBackend Nothing [UserMessage "one"]
                (modifyIORef' events . (:))
            -- The dead socket committed nothing, so the backend closes the
            -- partial attempt with a visible boundary and resubmits the same
            -- request; the sender then dials a fresh connection.
            result `shouldBe` Right (emptyTurnOutput "resp-fresh" [] (Just "ok"))
            reverse <$> readIORef events `shouldReturn`
                [ TextDelta "partial"
                , ActivityUpdated
                    "Connection lost mid-response (socket closed); reconnecting in 1s (attempt 1)…"
                , ResponseRestarted
                    "Connection interrupted the response; restarting automatically. The new attempt may repeat partial output shown above."
                , ActivityUpdated "Reconnecting to Codex (attempt 1)…"
                ]
            readIORef healthy `shouldReturn` False
            readIORef freshCalls `shouldReturn` 1

        it "does not treat provider errors as a dead connection" do
            freshCalls <- newIORef (0 :: Int)
            healthy <- newIORef True
            transcript <- newIORef []
            let sendCurrent _request _previous _onEvent =
                    pure $ Left $ ProviderError InvalidRequestError "bad request" Nothing
                sendFresh _failure _request _previous _onEvent = do
                    modifyIORef' freshCalls (+ 1)
                    pure $ Right (testResponse "resp-fresh" [assistantItem "ok"])
                providerErrorBackend = openAiBackendWithConnectionRecovery
                    healthy sendCurrent sendFresh (pure baseParams)
            result <- submitWithState transcript providerErrorBackend Nothing
                [UserMessage "one"] (const (pure ()))
            result `shouldBe` Left (ProviderError InvalidRequestError "bad request" Nothing)
            readIORef healthy `shouldReturn` True
            readIORef freshCalls `shouldReturn` 0

        it "reconnects immediately after a websocket connection-limit error" do
            currentCalls <- newIORef (0 :: Int)
            freshCalls <- newIORef (0 :: Int)
            healthy <- newIORef True
            transcript <- newIORef []
            let sendCurrent _request _previous _onEvent = do
                    modifyIORef' currentCalls (+ 1)
                    pure $ Left $ ProviderError WebSocketConnectionLimitReached
                        "too many websocket connections" Nothing
                sendFresh _failure _request _previous _onEvent = do
                    modifyIORef' freshCalls (+ 1)
                    pure $ Right (testResponse "resp-fresh" [assistantItem "ok"])
                backend = openAiBackendWithConnectionRecovery
                    healthy sendCurrent sendFresh (pure baseParams)
            result <- submitWithState transcript backend Nothing
                [UserMessage "one"] (const (pure ()))
            result `shouldBe` Right (emptyTurnOutput "resp-fresh" [] (Just "ok"))
            readIORef healthy `shouldReturn` False
            readIORef currentCalls `shouldReturn` 1
            readIORef freshCalls `shouldReturn` 1

        it "reacquires a credential after an in-band usage-limit error" do
            freshCalls <- newIORef (0 :: Int)
            healthy <- newIORef True
            transcript <- newIORef []
            let sendCurrent _request _previous _onEvent =
                    pure $ Left $ ProviderError UsageLimitReached
                        "usage limit reached" (Just 120)
                sendFresh _failure _request _previous _onEvent = do
                    modifyIORef' freshCalls (+ 1)
                    pure $ Right (testResponse "resp-fresh" [assistantItem "ok"])
                backend = openAiBackendWithConnectionRecovery
                    healthy sendCurrent sendFresh (pure baseParams)
            result <- submitWithState transcript backend Nothing
                [UserMessage "one"] (const (pure ()))
            result `shouldBe` Right (emptyTurnOutput "resp-fresh" [] (Just "ok"))
            readIORef healthy `shouldReturn` False
            readIORef freshCalls `shouldReturn` 1

        it "reports the exhausted reusable connection before replaying on a fresh account" do
            healthy <- newIORef True
            transcript <- newIORef (turnInputsToItems [UserMessage "old"])
            failures <- newIORef []
            seen <- newIORef []
            let exhausted = ProviderError UsageLimitReached
                    "usage limit reached" (Just 120)
                sendCurrent _request _previous _onEvent =
                    pure (Left exhausted)
                sendFresh failure request previous _onEvent = do
                    modifyIORef' failures (<> [failure])
                    modifyIORef' seen (<> [(inputItems request, previous)])
                    case previous of
                        Just _ ->
                            pure $ Left $ ProviderError PreviousResponseNotFound
                                "previous_response_id belongs to another account"
                                Nothing
                        Nothing ->
                            pure $ Right $
                                testResponse "resp-fresh" [assistantItem "ok"]
                backend = openAiBackendWithConnectionRecovery
                    healthy sendCurrent sendFresh (pure baseParams)
            result <- submitWithState transcript backend (Just "resp-old")
                [UserMessage "new"] (const (pure ()))
            result `shouldBe`
                Right (emptyTurnOutput "resp-fresh" [] (Just "ok"))
            readIORef failures `shouldReturn` [Just exhausted, Nothing]
            readIORef seen `shouldReturn`
                [ (turnInputsToItems [UserMessage "new"], Just "resp-old")
                , ( turnInputsToItems [UserMessage "old"]
                        <> turnInputsToItems [UserMessage "new"]
                  , Nothing
                  )
                ]

    describe "openAiResponseSenderWithRetryPolicy" do
        it "retries a pre-output fresh connection failure" $
            shouldRetryFreshAuxiliaryFailure
                (ConnectionError "fresh socket closed")

        it "retries a pre-output fresh websocket-limit failure" $
            shouldRetryFreshAuxiliaryFailure
                (ProviderError WebSocketConnectionLimitReached
                    "too many websocket connections"
                    Nothing)

        it "returns the final pre-output failure after exhausting retries" do
            attempts <- newIORef (0 :: Int)
            let failure = ConnectionError "still offline"
                send _request _previous _onEvent = do
                    modifyIORef' attempts (+ 1)
                    pure (Left failure)
                sender =
                    openAiResponseSenderWithRetryPolicy
                        (constantDelay 0 <> limitRetries 1)
                        isAuxiliaryOutputEvent
                        send
            sender baseParams Nothing (const (pure ()))
                `shouldReturn` Left failure
            readIORef attempts `shouldReturn` 2

        it "marks every post-output auxiliary failure as non-retryable" do
            mapM_ shouldMarkPostOutputAuxiliaryFailure
                [ ConnectionError "socket closed"
                , ProviderError WebSocketConnectionLimitReached
                    "too many websocket connections"
                    Nothing
                , ProviderError OverloadedError "busy" Nothing
                , ProviderError ServiceUnavailableError "unavailable" Nothing
                , ProviderError ApiErrorType "server error" Nothing
                , ProviderError UsageLimitReached "usage exhausted" (Just 120)
                , HttpError 503 "unavailable"
                ]

        it "retains WebSocket provenance without making replay safe" do
            let failure =
                    replayUnsafeAuxiliaryFailure
                        (ConnectionError
                            "WebSocket receive error: ParseException \"not enough bytes\"")
            isOpenAiReplayUnsafeWebSocketTransportFailure failure
                `shouldBe` True
            -- Callers use the ordinary predicate only when replaying the same
            -- logical request over HTTP is safe.
            isOpenAiWebSocketTransportFailure failure `shouldBe` False

        it "does not label post-output provider failures as transport failures" do
            let failure =
                    replayUnsafeAuxiliaryFailure
                        (ProviderError ApiErrorType "server error" Nothing)
            isOpenAiReplayUnsafeWebSocketTransportFailure failure
                `shouldBe` False

    describe "isOpenAiWebSocketTransportFailure" do
        it "recognizes an exact WebSocket handshake 403" do
            isOpenAiWebSocketTransportFailure
                (HttpError 403 "WebSocket handshake returned HTTP 403")
                `shouldBe` True

        it "does not hide application permission or authentication errors" do
            isOpenAiWebSocketTransportFailure
                (HttpError 403 "model access denied")
                `shouldBe` False
            isOpenAiWebSocketTransportFailure
                (HttpError 401 "WebSocket handshake returned HTTP 401")
                `shouldBe` False

shouldRetryFreshAuxiliaryFailure :: ApiError -> Expectation
shouldRetryFreshAuxiliaryFailure initialFailure = do
    healthy <- newIORef False
    currentCalls <- newIORef (0 :: Int)
    freshCalls <- newIORef (0 :: Int)
    let response = testResponse "resp-retried" [assistantItem "ok"]
        sendCurrent _request _previous _onEvent = do
            modifyIORef' currentCalls (+ 1)
            pure (Left (ConnectionError "unexpected reusable request"))
        sendFresh _failure _request _previous _onEvent = do
            attempt <- atomicModifyIORef' freshCalls \count ->
                let next = count + 1
                in (next, next)
            pure $
                if attempt == 1
                    then Left initialFailure
                    else Right response
        reconnecting =
            openAiAuxiliaryResponseSenderWithConnectionRecovery
                healthy
                sendCurrent
                sendFresh
        sender =
            openAiResponseSenderWithRetryPolicy
                (constantDelay 0 <> limitRetries 1)
                isAuxiliaryOutputEvent
                reconnecting
    sender baseParams Nothing (const (pure ()))
        `shouldReturn` Right response
    readIORef currentCalls `shouldReturn` 0
    readIORef freshCalls `shouldReturn` 2

shouldMarkPostOutputAuxiliaryFailure :: ApiError -> Expectation
shouldMarkPostOutputAuxiliaryFailure failure = do
    healthy <- newIORef True
    currentCalls <- newIORef (0 :: Int)
    freshCalls <- newIORef (0 :: Int)
    let outputEvent = ResponseOutputItemDoneEvent
            { item = assistantItem "partial"
            , outputIndex = Just 0
            , sequenceNumber = Nothing
            }
        sendCurrent _request _previous onEvent = do
            modifyIORef' currentCalls (+ 1)
            onEvent outputEvent
            pure (Left failure)
        sendFresh _failure _request _previous _onEvent = do
            modifyIORef' freshCalls (+ 1)
            pure $ Right (testResponse "resp-fresh" [assistantItem "duplicate"])
        sender =
            openAiAuxiliaryResponseSenderWithConnectionRecovery
                healthy
                sendCurrent
                sendFresh
        expected = replayUnsafeAuxiliaryFailure failure
    sender baseParams Nothing (const (pure ()))
        `shouldReturn` Left expected
    isInlineRetryableProviderError expected `shouldBe` False
    readIORef currentCalls `shouldReturn` 1
    readIORef freshCalls `shouldReturn` 0

isAuxiliaryOutputEvent :: ResponseStreamEvent -> Bool
isAuxiliaryOutputEvent = streamOutputObserved

replayUnsafeAuxiliaryFailure :: ApiError -> ApiError
replayUnsafeAuxiliaryFailure failure =
    ProviderError replayUnsafeType
        ( "provider failed after auxiliary response output; refusing to replay: "
            <> Text.pack (show failure)
        )
        Nothing
  where
    replayUnsafeType
        | isOpenAiWebSocketTransportFailure failure =
            UnknownErrorType "replay_unsafe_websocket_transport"
        | otherwise = UnknownErrorType "replay_unsafe"
