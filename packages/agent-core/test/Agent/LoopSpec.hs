module Agent.LoopSpec (spec) where

import Agent.Cancel (newCancelFlag, requestCancel)
import Agent.Error (ApiError(..))
import Agent.Loop
import Agent.ToolArgs (objectArgs, reqText)
import Agent.ToolDispatch
import Control.Concurrent (threadDelay)
import Data.Aeson (FromJSON(..))
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "runLoop" do
    it "threads previous_response_id and sends only CompletedTool on the follow-up" do
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right TurnOutput
                { responseId = "resp-1"
                , toolCalls = [functionToolCall "c1" "echo" "{\"message\":\"hi\"}"]
                , assistantText = Just "calling echo"
                }
            , Right TurnOutput
                { responseId = "resp-2"
                , toolCalls = []
                , assistantText = Just "done"
                }
            ]
        config <- testConfig backend
        result <- runLoop config Nothing "hello"
        result `shouldBe` Right LoopResult
            { finalResponseId = "resp-2"
            , finalText = Just "done"
            , turnsUsed = 2
            }
        seen <- readIORef submissions
        seen `shouldBe`
            [ (Nothing, [UserMessage "hello"])
            , (Just "resp-1", [CompletedTool (functionResult "c1" "echo:hi")])
            ]

    it "accepts multimodal first turns via runLoopInputs" do
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right TurnOutput
                { responseId = "resp-m"
                , toolCalls = []
                , assistantText = Just "saw it"
                }
            ]
        let image = ImageAttachment "image/png" "abc"
            inputs =
                [ UserMultimodal
                    { userText = "see this"
                    , userImages = [image]
                    }
                ]
        config <- testConfig backend
        result <- runLoopInputs config Nothing inputs
        result `shouldBe` Right LoopResult
            { finalResponseId = "resp-m"
            , finalText = Just "saw it"
            , turnsUsed = 1
            }
        seen <- readIORef submissions
        seen `shouldBe` [(Nothing, inputs)]

    it "serializes loopOnEvent across parallel tool calls" do
        inFlight <- newIORef (0 :: Int)
        maxInFlight <- newIORef (0 :: Int)
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right TurnOutput
                { responseId = "resp-1"
                , toolCalls =
                    [ functionToolCall "c1" "a" "{}"
                    , functionToolCall "c2" "b" "{}"
                    ]
                , assistantText = Nothing
                }
            , Right TurnOutput
                { responseId = "resp-2"
                , toolCalls = []
                , assistantText = Just "ok"
                }
            ]
        let onEvent _ = do
                now <- atomicModifyIORef' inFlight \n -> (n + 1, n + 1)
                atomicModifyIORef' maxInFlight \seen -> (max seen now, ())
                threadDelay 30000
                atomicModifyIORef' inFlight \n -> (n - 1, ())
            handlers =
                [ noArgsTool "a" (pure (Right "ok"))
                , noArgsTool "b" (pure (Right "ok"))
                ]
        config0 <- testConfig backend
        let config = config0
                { loopHandlers = handlers
                , loopOnEvent = onEvent
                }
        result <- runLoop config Nothing "go"
        result `shouldBe` Right LoopResult
            { finalResponseId = "resp-2"
            , finalText = Just "ok"
            , turnsUsed = 2
            }
        readIORef maxInFlight `shouldReturn` 1

    it "dispatches parallel tool calls" do
        inFlight <- newIORef (0 :: Int)
        maxInFlight <- newIORef (0 :: Int)
        let bump = do
                now <- atomicModifyIORef' inFlight \n -> (n + 1, n + 1)
                atomicModifyIORef' maxInFlight \seen -> (max seen now, ())
                threadDelay 80000
                atomicModifyIORef' inFlight \n -> (n - 1, ())
                pure (Right "ok")
            handlers =
                [ noArgsTool "a" bump
                , noArgsTool "b" bump
                ]
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right TurnOutput
                { responseId = "resp-1"
                , toolCalls =
                    [ functionToolCall "c1" "a" "{}"
                    , functionToolCall "c2" "b" "{}"
                    ]
                , assistantText = Nothing
                }
            , Right TurnOutput
                { responseId = "resp-2"
                , toolCalls = []
                , assistantText = Just "ok"
                }
            ]
        config0 <- testConfig backend
        result <- runLoop config0 { loopHandlers = handlers } Nothing "go"
        result `shouldBe` Right LoopResult
            { finalResponseId = "resp-2"
            , finalText = Just "ok"
            , turnsUsed = 2
            }
        readIORef maxInFlight `shouldReturn` 2

    it "returns a denial as tool output when approval is refused" do
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right TurnOutput
                { responseId = "resp-1"
                , toolCalls = [functionToolCall "c1" "echo" "{\"message\":\"nope\"}"]
                , assistantText = Nothing
                }
            , Right TurnOutput
                { responseId = "resp-2"
                , toolCalls = []
                , assistantText = Just "understood"
                }
            ]
        config0 <- testConfig backend
        let config = config0 { loopApprove = \_ -> pure False }
        result <- runLoop config Nothing "please"
        result `shouldBe` Right LoopResult
            { finalResponseId = "resp-2"
            , finalText = Just "understood"
            , turnsUsed = 2
            }
        seen <- readIORef submissions
        case seen of
            [_, (Just "resp-1", [CompletedTool denied])] ->
                denied.output `shouldBe` "Tool call rejected by user."
            other -> expectationFailure ("unexpected submissions: " <> show other)

    it "returns LoopMaxTurns when the model keeps calling tools" do
        backend <- endlessToolsBackend
        config0 <- testConfig backend
        let config = config0 { loopMaxTurns = 1 }
        result <- runLoop config Nothing "loop forever"
        case result of
            Left (LoopMaxTurns turn) -> do
                turn.responseId `shouldBe` "resp-1"
                turn.toolCalls `shouldNotBe` []
            other -> expectationFailure ("expected LoopMaxTurns, got " <> show other)

    it "keeps looping after a handler exception" do
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right TurnOutput
                { responseId = "resp-1"
                , toolCalls = [functionToolCall "c1" "explode" "{}"]
                , assistantText = Nothing
                }
            , Right TurnOutput
                { responseId = "resp-2"
                , toolCalls = []
                , assistantText = Just "survived"
                }
            ]
        let handlers = [noArgsTool "explode" (error "boom")]
        config0 <- testConfig backend
        result <- runLoop config0 { loopHandlers = handlers } Nothing "go"
        result `shouldBe` Right LoopResult
            { finalResponseId = "resp-2"
            , finalText = Just "survived"
            , turnsUsed = 2
            }
        seen <- readIORef submissions
        case seen of
            [_, (_, [CompletedTool crashed])] ->
                crashed.output `shouldSatisfy` Text.isInfixOf "crashed"
            other -> expectationFailure ("unexpected submissions: " <> show other)

    it "surfaces a transport Left as LoopTransport" do
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [Left (ConnectionError "down")]
        config <- testConfig backend
        result <- runLoop config Nothing "hello"
        result `shouldBe` Left (LoopTransport (ConnectionError "down"))

    it "emits TurnStarted and TurnFinished around each backend submit" do
        events <- newIORef []
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right TurnOutput
                { responseId = "resp-1"
                , toolCalls = []
                , assistantText = Just "hi"
                }
            ]
        config0 <- testConfig backend
        let config = config0
                { loopOnEvent = \event -> modifyIORef' events (event :)
                }
        _ <- runLoop config Nothing "hello"
        seen <- reverse <$> readIORef events
        seen `shouldBe`
            [ TurnStarted
            , TurnFinished TurnOutput
                { responseId = "resp-1"
                , toolCalls = []
                , assistantText = Just "hi"
                }
            ]

    it "emits ToolStarted and ToolFinished around each dispatched call" do
        events <- newIORef []
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right TurnOutput
                { responseId = "resp-1"
                , toolCalls = [functionToolCall "c1" "echo" "{\"message\":\"hi\"}"]
                , assistantText = Nothing
                }
            , Right TurnOutput
                { responseId = "resp-2"
                , toolCalls = []
                , assistantText = Just "done"
                }
            ]
        config0 <- testConfig backend
        let config = config0
                { loopOnEvent = \event -> modifyIORef' events (event :)
                }
        _ <- runLoop config Nothing "hello"
        seen <- reverse <$> readIORef events
        seen `shouldBe`
            [ TurnStarted
            , TurnFinished TurnOutput
                { responseId = "resp-1"
                , toolCalls = [functionToolCall "c1" "echo" "{\"message\":\"hi\"}"]
                , assistantText = Nothing
                }
            , ToolStarted (functionToolCall "c1" "echo" "{\"message\":\"hi\"}")
            , ToolFinished (functionResult "c1" "echo:hi")
            , TurnStarted
            , TurnFinished TurnOutput
                { responseId = "resp-2"
                , toolCalls = []
                , assistantText = Just "done"
                }
            ]


    it "returns LoopCancelled when the cancel flag is set during tools" do
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right TurnOutput
                { responseId = "resp-1"
                , toolCalls = [functionToolCall "c1" "slow" "{}"]
                , assistantText = Nothing
                }
            ]
        config0 <- testConfig backend
        let cancel = case config0 of
                LoopConfig{loopCancel = c} -> c
            handlers =
                [ noArgsTool "slow" do
                    requestCancel cancel
                    threadDelay 10000
                    pure (Right "should-not-continue")
                ]
            config = config0 { loopHandlers = handlers }
        result <- runLoop config Nothing "go"
        case result of
            Left (LoopCancelled results) ->
                results `shouldNotBe` []
            other -> expectationFailure ("expected LoopCancelled, got " <> show other)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

testConfig :: Backend -> IO LoopConfig
testConfig backend = do
    cancel <- newCancelFlag
    pure LoopConfig
        { loopBackend = backend
        , loopHandlers =
            [ typedTool "echo" $ \EchoArgs { message } ->
                pure (Right ("echo:" <> message))
            ]
        , loopDispatch = defaultLoopDispatch
        , loopMaxTurns = defaultLoopMaxTurns
        , loopOnEvent = \_ -> pure ()
        , loopApprove = \_ -> pure True
        , loopCancel = cancel
        }

data EchoArgs = EchoArgs { message :: Text }

instance FromJSON EchoArgs where
    parseJSON = objectArgs $ \object -> EchoArgs <$> reqText object "message"

functionResult :: Text -> Text -> ToolCallResult
functionResult callId output = ToolCallResult
    { callId
    , output
    , callKind = FunctionCallKind
    }

scriptedBackend
    :: IORef [(Maybe Text, [TurnInput])]
    -> [Either ApiError TurnOutput]
    -> IO Backend
scriptedBackend submissions answers = do
    remaining <- newIORef answers
    pure $ Backend \prev inputs _onEvent -> do
        modifyIORef' submissions (++ [(prev, inputs)])
        atomicModifyIORef' remaining \case
            [] -> ([], Left (ConnectionError "scripted backend exhausted"))
            next : rest -> (rest, next)

endlessToolsBackend :: IO Backend
endlessToolsBackend = do
    counter <- newIORef (0 :: Int)
    pure $ Backend \_prev _inputs _onEvent -> do
        n <- atomicModifyIORef' counter \i -> (i + 1, i + 1)
        let responseId = "resp-" <> Text.pack (show n)
        pure $ Right TurnOutput
            { responseId
            , toolCalls = [functionToolCall "c1" "echo" "{\"message\":\"again\"}"]
            , assistantText = Nothing
            }
