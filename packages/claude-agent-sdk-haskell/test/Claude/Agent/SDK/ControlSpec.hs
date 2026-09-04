module Claude.Agent.SDK.ControlSpec (spec) where

import Claude.Agent.SDK
import Control.Concurrent
    ( forkIO
    , newChan
    , newEmptyMVar
    , newMVar
    , putMVar
    , readChan
    , takeMVar
    , threadDelay
    , withMVar
    , writeChan
    )
import Control.Concurrent.Async (wait, withAsync)
import Control.Exception.Safe (finally)
import Control.Monad (void)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import Data.IORef
    ( IORef
    , modifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.Text (Text)
import qualified Data.Text as Text
import System.Exit (ExitCode(ExitSuccess))
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "Claude SDK control protocol" do
    it "initializes handler-aware clients and preserves the response" do
        initialization <- newIORef Nothing
        factory <-
            fakeTransportFactory \emit value ->
                case request value of
                    Just (requestId, "initialize", _) -> do
                        writeIORef initialization (Just value)
                        emit $
                            successResponse requestId $
                                Aeson.object
                                    [ "protocolVersion" Aeson..=
                                        (1 :: Int)
                                    ]
                    _ -> pure ()
        let options = testOptions
            handlers =
                defaultClaudeAgentHandlers
                    { initializeOptions =
                        Just (Aeson.object ["skills" Aeson..= ["review" :: Text]])
                    }
        result <-
            withClaudeSDKClientWithTransportAndHandlers
                options
                factory.transportFactory
                handlers
                \client ->
                    withTurn client \turn ->
                        pure $
                            Right
                                ( initializationResult turn
                                , pure ()
                                )
        result
            `shouldBe`
                Right
                    (Just
                        (Aeson.object
                            [ "protocolVersion" Aeson..= (1 :: Int)
                            ]))
        initialized <- readIORef initialization
        initialized `shouldSatisfy` \case
            Just value ->
                requestSubtype value == Just "initialize"
                    && nestedRequestField "skills" value
                        == Just
                            (Aeson.toJSON ["review" :: Text])
            Nothing -> False

    it "matches concurrent control responses by request id" do
        pending <- newIORef []
        factory <-
            fakeTransportFactory \emit value ->
                case request value of
                    Just (requestId, "initialize", _) ->
                        emit (successResponse requestId (Aeson.object []))
                    Just (requestId, subtype, _) -> do
                        modifyIORef' pending (<> [(requestId, subtype)])
                        requests <- readIORef pending
                        case requests of
                            [(firstId, _), (secondId, _)] -> do
                                emit $
                                    successResponse secondId $
                                        Aeson.object ["which" Aeson..= ("second" :: Text)]
                                emit $
                                    successResponse firstId $
                                        Aeson.object ["which" Aeson..= ("first" :: Text)]
                            _ -> pure ()
                    _ -> pure ()
        withClaudeSDKClientWithTransportAndHandlers
            testOptions
            factory.transportFactory
            defaultClaudeAgentHandlers
            \client -> do
                outcome <-
                    withTurn client \turn -> do
                        first <- newEmptyMVar
                        second <- newEmptyMVar
                        void $ forkIO $
                            sendControlRequest turn
                                (Aeson.object ["subtype" Aeson..= ("alpha" :: Text)])
                                >>= putMVar first
                        void $ forkIO $
                            sendControlRequest turn
                                (Aeson.object ["subtype" Aeson..= ("beta" :: Text)])
                                >>= putMVar second
                        firstResult <- takeMVar first
                        secondResult <- takeMVar second
                        pure (Right ((firstResult, secondResult), pure ()))
                case outcome of
                    Right (Right firstValue, Right secondValue) ->
                        [objectText "which" firstValue, objectText "which" secondValue]
                            `shouldMatchList` [Just "first", Just "second"]
                    other ->
                        expectationFailure ("unexpected results: " <> show other)

    it "handles permission requests without exposing them as messages" do
        permissionSeen <- newEmptyMVar
        permissionResponse <- newEmptyMVar
        factory <-
            fakeTransportFactory \emit value ->
                case request value of
                    Just (requestId, "initialize", _) -> do
                        emit (successResponse requestId (Aeson.object []))
                        emit $
                            Aeson.object
                                [ "type" Aeson..= ("control_request" :: Text)
                                , "request_id" Aeson..= ("permission-1" :: Text)
                                , "request" Aeson..= Aeson.object
                                    [ "subtype" Aeson..= ("can_use_tool" :: Text)
                                    , "tool_name" Aeson..= ("Bash" :: Text)
                                    , "input" Aeson..= Aeson.object
                                        ["command" Aeson..= ("pwd" :: Text)]
                                    , "tool_use_id" Aeson..= ("tool-1" :: Text)
                                    ]
                                ]
                        emit $
                            Aeson.object
                                [ "type" Aeson..= ("future_message" :: Text)
                                , "session_id" Aeson..= ("session" :: Text)
                                ]
                    _ ->
                        case responseRequestId value of
                            Just "permission-1" ->
                                putMVar permissionResponse value
                            _ -> pure ()
        let handlers =
                defaultClaudeAgentHandlers
                    { canUseTool =
                        Just \permission -> do
                            putMVar permissionSeen permission
                            pure $
                                ToolPermissionDeny
                                    "host denied"
                                    False
                    }
        withClaudeSDKClientWithTransportAndHandlers
            testOptions
            factory.transportFactory
            handlers
            \client -> do
                outcome <-
                    withTurn client \turn -> do
                        observed <- receiveMessage turn
                        pure (Right (observed, pure ()))
                outcome `shouldSatisfy` \case
                    Right (Right (Just MessageUnknown{})) -> True
                    _ -> False
                permission <- takeMVar permissionSeen
                permission.toolName `shouldBe` "Bash"
                response <- takeMVar permissionResponse
                nestedResponseField "behavior" response
                    `shouldBe` Just (Aeson.String "deny")

    it "cancels in-flight handlers without writing a stale response" do
        handlerStarted <- newEmptyMVar
        releaseHandler <- newEmptyMVar
        factory <-
            fakeTransportFactory \emit value ->
                case request value of
                    Just (requestId, "initialize", _) -> do
                        emit (successResponse requestId (Aeson.object []))
                        emit $
                            Aeson.object
                                [ "type" Aeson..= ("control_request" :: Text)
                                , "request_id" Aeson..= ("cancel-me" :: Text)
                                , "request" Aeson..= Aeson.object
                                    [ "subtype" Aeson..= ("can_use_tool" :: Text)
                                    , "tool_name" Aeson..= ("Read" :: Text)
                                    , "input" Aeson..= Aeson.object []
                                    ]
                                ]
                    _ -> pure ()
        let handlers =
                defaultClaudeAgentHandlers
                    { canUseTool =
                        Just \_ -> do
                            putMVar handlerStarted ()
                            takeMVar releaseHandler
                            pure (ToolPermissionAllow Nothing [])
                    }
        withClaudeSDKClientWithTransportAndHandlers
            testOptions
            factory.transportFactory
            handlers
            \client -> do
                withTurn client \_ -> do
                    takeMVar handlerStarted
                    factory.emit $
                        Aeson.object
                            [ "type" Aeson..=
                                ("control_cancel_request" :: Text)
                            , "request_id" Aeson..=
                                ("cancel-me" :: Text)
                            ]
                    threadDelay 50_000
                    pure (Right ((), pure ()))
        writes <- readIORef factory.writes
        mapMaybeResponseId writes
            `shouldNotContain` ["cancel-me"]

    it "fails closed for unknown inbound control requests" do
        responseSeen <- newEmptyMVar
        factory <-
            fakeTransportFactory \emit value ->
                case request value of
                    Just (requestId, "initialize", _) -> do
                        emit (successResponse requestId (Aeson.object []))
                        emit $
                            Aeson.object
                                [ "type" Aeson..= ("control_request" :: Text)
                                , "request_id" Aeson..= ("future-1" :: Text)
                                , "request" Aeson..= Aeson.object
                                    [ "subtype" Aeson..=
                                        ("future_control" :: Text)
                                    , "new_field" Aeson..= (True :: Bool)
                                    ]
                                ]
                    _ ->
                        case responseRequestId value of
                            Just "future-1" ->
                                putMVar responseSeen value
                            _ -> pure ()
        result <-
            withClaudeSDKClientWithTransportAndHandlers
                testOptions
                factory.transportFactory
                defaultClaudeAgentHandlers
                \client ->
                    withTurn client \_ -> do
                        response <- takeMVar responseSeen
                        responseSubtype response `shouldBe` Just "error"
                        responseError response
                            `shouldSatisfy`
                                maybe False
                                    ("Unsupported control request"
                                        `Text.isInfixOf`)
                        pure (Right ((), pure ()))
        result `shouldBe` Right ()

    it "times out initialization and closes the failed transport" do
        closed <- newIORef (0 :: Int)
        factory <-
            fakeTransportFactory \_ _ ->
                pure ()
        let transportFactory requestInfo = do
                transport <- factory.transportFactory requestInfo
                pure transport
                    { transportClose =
                        modifyIORef' closed (+ 1)
                    }
            handlers =
                defaultClaudeAgentHandlers
                    { initializeTimeoutMicros = 20_000
                    }
        result <-
            withClaudeSDKClientWithTransportAndHandlers
                testOptions
                transportFactory
                handlers
                \client ->
                    withTurn client \_ ->
                        pure (Right ((), pure ()))
        result `shouldSatisfy` \case
            Left (CLIConnectionError message) ->
                "Control request timed out: initialize"
                    `Text.isInfixOf` message
            _ -> False
        readIORef closed `shouldReturn` 1

    it "restarts after in-band abort races a completed callback" do
        starts <- newIORef (0 :: Int)
        closes <- newIORef (0 :: Int)
        factory <-
            fakeTransportFactory \emit value ->
                case request value of
                    Just (requestId, subtype, _)
                        | subtype == "initialize"
                            || subtype == "interrupt" ->
                            emit (successResponse requestId (Aeson.object []))
                    _ -> pure ()
        let transportFactory requestInfo = do
                modifyIORef' starts (+ 1)
                transport <- factory.transportFactory requestInfo
                pure transport
                    { transportClose =
                        modifyIORef' closes (+ 1)
                            >> transport.transportClose
                    }
        withClaudeSDKClientWithTransportAndHandlers
            testOptions
            transportFactory
            defaultClaudeAgentHandlers
            \client -> do
                started <- newEmptyMVar
                release <- newEmptyMVar
                finished <- newEmptyMVar
                void $ forkIO do
                    outcome <-
                        withTurn client \_ -> do
                            putMVar started ()
                            takeMVar release
                            pure (Right ((), pure ()))
                    putMVar finished outcome
                takeMVar started
                abort client
                putMVar release ()
                takeMVar finished
                    `shouldReturn`
                        Left
                            (CLIConnectionError
                                "Claude Code turn was interrupted.")
                second <-
                    withTurn client \_ ->
                        pure (Right ((), pure ()))
                second `shouldBe` Right ()
                readIORef starts `shouldReturn` 2
                readIORef closes `shouldReturn` 1
        readIORef closes `shouldReturn` 2

    it "bounds and joins a blocked endInput during shutdown" do
        factory <-
            fakeTransportFactory \emit value ->
                case request value of
                    Just (requestId, "initialize", _) ->
                        emit (successResponse requestId (Aeson.object []))
                    _ -> pure ()
        block <- newEmptyMVar
        endInputStarted <- newEmptyMVar
        lifecycleLock <- newMVar ()
        closed <- newIORef False
        endInputObservedClosed <- newEmptyMVar
        let blockedFactory requestInfo = do
                transport <- factory.transportFactory requestInfo
                pure transport
                    { transportClose =
                        withMVar lifecycleLock \_ ->
                            writeIORef closed True
                                >> transport.transportClose
                    , transportEndInput =
                        withMVar lifecycleLock \_ -> do
                            putMVar endInputStarted ()
                            takeMVar block
                                `finally`
                                    (readIORef closed
                                        >>= putMVar endInputObservedClosed)
                    }
            handlers =
                defaultClaudeAgentHandlers
                    { shutdownTimeoutMicros = 20_000
                    }
        withAsync
            (withClaudeSDKClientWithTransportAndHandlers
                testOptions
                blockedFactory
                handlers
                \client ->
                    withTurn client \_ ->
                        pure (Right ((), pure ())))
            \shutdown -> do
                takeMVar endInputStarted
                result <- timeout 500_000 (wait shutdown)
                case result of
                    Nothing -> do
                        -- Keep a regression failure from leaking the blocked
                        -- shutdown worker into the rest of the suite.
                        putMVar block ()
                        void (wait shutdown)
                    Just _ ->
                        pure ()
                result `shouldBe` Just (Right ())
        takeMVar endInputObservedClosed `shouldReturn` False

data FakeTransport = FakeTransport
    { transportFactory :: !TransportFactory
    , emit :: !(Aeson.Value -> IO ())
    , writes :: !(IORef [Aeson.Value])
    }

fakeTransportFactory
    :: ((Aeson.Value -> IO ()) -> Aeson.Value -> IO ())
    -> IO FakeTransport
fakeTransportFactory onWrite = do
    writes <- newIORef []
    activeOutput <- newIORef Nothing
    let emit value =
            readIORef activeOutput >>= \case
                Nothing -> pure ()
                Just output -> writeChan output (Just (encoded value))
        transportFactory _ = do
            output <- newChan
            exited <- newIORef False
            writeIORef activeOutput (Just output)
            pure Transport
                { transportConnect = pure (Right ())
                , transportWrite = \bytes ->
                    case Aeson.eitherDecodeStrict' bytes of
                        Left message ->
                            pure $
                                Left $
                                    CLIJSONDecodeError
                                        (fromString message)
                                        (fromString (show bytes))
                        Right value -> do
                            modifyIORef' writes (<> [value])
                            onWrite
                                (writeChan output . Just . encoded)
                                value
                            pure (Right ())
                , transportRead =
                    Right <$> readChan output
                , transportClose = do
                    writeIORef exited True
                    writeChan output Nothing
                , transportIsReady = not <$> readIORef exited
                , transportEndInput = do
                    writeIORef exited True
                    writeChan output Nothing
                , transportProcessExit = do
                    done <- readIORef exited
                    pure (if done then Just ExitSuccess else Nothing)
                , transportDiagnostic = pure ""
                }
    pure FakeTransport{transportFactory, emit, writes}

testOptions :: ClaudeAgentOptions
testOptions =
    defaultClaudeAgentOptions "unused" "."

withTurn
    :: ClaudeSDKClient
    -> (ClaudeSDKTurn -> IO (Either ClaudeSDKError (a, IO ())))
    -> IO (Either ClaudeSDKError a)
withTurn client =
    withClaudeSDKTurn
        client
        (pure True)
        Nothing
        Nothing
        Nothing

encoded :: Aeson.Value -> ByteString.ByteString
encoded value =
    LazyByteString.toStrict (Aeson.encode value <> "\n")

request :: Aeson.Value -> Maybe (Text, Text, Aeson.Value)
request value@(Aeson.Object object) = do
    Aeson.String "control_request" <- KeyMap.lookup "type" object
    Aeson.String requestId <- KeyMap.lookup "request_id" object
    Aeson.Object requestObject <- KeyMap.lookup "request" object
    Aeson.String subtype <- KeyMap.lookup "subtype" requestObject
    pure (requestId, subtype, value)
request _ = Nothing

requestSubtype :: Aeson.Value -> Maybe Text
requestSubtype value = do
    (_, subtype, _) <- request value
    pure subtype

nestedRequestField :: Aeson.Key -> Aeson.Value -> Maybe Aeson.Value
nestedRequestField key (Aeson.Object object) = do
    Aeson.Object requestObject <- KeyMap.lookup "request" object
    KeyMap.lookup key requestObject
nestedRequestField _ _ = Nothing

successResponse :: Text -> Aeson.Value -> Aeson.Value
successResponse requestId response =
    Aeson.object
        [ "type" Aeson..= ("control_response" :: Text)
        , "response" Aeson..= Aeson.object
            [ "subtype" Aeson..= ("success" :: Text)
            , "request_id" Aeson..= requestId
            , "response" Aeson..= response
            ]
        ]

responseRequestId :: Aeson.Value -> Maybe Text
responseRequestId (Aeson.Object object) = do
    Aeson.String "control_response" <- KeyMap.lookup "type" object
    Aeson.Object response <- KeyMap.lookup "response" object
    Aeson.String requestId <- KeyMap.lookup "request_id" response
    pure requestId
responseRequestId _ = Nothing

nestedResponseField :: Aeson.Key -> Aeson.Value -> Maybe Aeson.Value
nestedResponseField key (Aeson.Object object) = do
    Aeson.Object response <- KeyMap.lookup "response" object
    Aeson.Object body <- KeyMap.lookup "response" response
    KeyMap.lookup key body
nestedResponseField _ _ = Nothing

responseSubtype :: Aeson.Value -> Maybe Text
responseSubtype (Aeson.Object object) = do
    Aeson.Object response <- KeyMap.lookup "response" object
    Aeson.String subtype <- KeyMap.lookup "subtype" response
    pure subtype
responseSubtype _ = Nothing

responseError :: Aeson.Value -> Maybe Text
responseError (Aeson.Object object) = do
    Aeson.Object response <- KeyMap.lookup "response" object
    Aeson.String message <- KeyMap.lookup "error" response
    pure message
responseError _ = Nothing

objectText :: Aeson.Key -> Aeson.Value -> Maybe Text
objectText key (Aeson.Object object) = do
    Aeson.String value <- KeyMap.lookup key object
    pure value
objectText _ _ = Nothing

mapMaybeResponseId :: [Aeson.Value] -> [Text]
mapMaybeResponseId =
    foldr
        (\value rest ->
            maybe rest (: rest) (responseRequestId value))
        []

fromString :: String -> Text
fromString = Text.pack
