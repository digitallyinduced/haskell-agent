module Agent.Server.ClientSpec (spec) where

import Agent.Server.Client
import Agent.Server.Client.Local
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Exception (bracket)
import Control.Exception.Safe qualified as Safe
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder (lazyByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as Text
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (testWithApplication)
import System.Directory (getTemporaryDirectory, removeFile, createDirectory, removePathForcibly)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO (hClose, openBinaryTempFile)
import System.Posix.Files (setFileMode)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "agent-server HTTP client" do
    it "reads a private local descriptor through EOF and validates its protocol" do
        parent <- getTemporaryDirectory
        let allocate = do
                (directory, handle) <- openBinaryTempFile parent "local-service-descriptor"
                hClose handle
                removeFile directory
                createDirectory directory
                pure directory
        bracket allocate removePathForcibly \directory -> do
            let path = directory </> "local-server.json"
                credential = directory </> "server.token"
                write version = do
                    LazyByteString.writeFile path (Aeson.encode (Aeson.object
                        [ "protocolVersion" Aeson..= (version :: Int)
                        , "serverInstanceId" Aeson..= ("fixture" :: Text)
                        , "buildCommit" Aeson..= ("test" :: Text)
                        , "baseUrl" Aeson..= ("http://127.0.0.1:4096" :: Text)
                        , "credentialFile" Aeson..= credential ]))
                    setFileMode path 0o600
            bracket (lookupEnv "HASKELL_AGENT_RUNTIME_DIR" <* setEnv "HASKELL_AGENT_RUNTIME_DIR" directory)
                (maybe (unsetEnv "HASKELL_AGENT_RUNTIME_DIR") (setEnv "HASKELL_AGENT_RUNTIME_DIR")) \_ -> do
                readLocalServiceDescriptor `shouldReturn` Left LocalServiceAbsent
                write 1
                readLocalServiceDescriptor `shouldReturn` Right (LocalServiceDescriptor
                    1 "fixture" "test" "http://127.0.0.1:4096" credential)
                write 2
                readLocalServiceDescriptor `shouldReturn` Left (LocalServiceIncompatible 2)

    it "accepts an empty successful delete response" do
        withTestClient (\_ respond -> respond (responseLBS status204 [] "")) \client ->
            (performAgentServerJsonRequest client "DELETE" "/v1/sessions/fixture" [204] Nothing
                :: IO (Either AgentServerClientError Aeson.Value)) `shouldReturn` Right Aeson.Null

    it "binds frontend requests to a lease and releases it after the client exits" do
        observed <- newIORef []
        let application request respond = do
                atomicModifyIORef' observed \requests ->
                    (requests <> [(request.requestMethod, rawPathInfo request,
                        lookup "X-Agent-Client-Id" request.requestHeaders)], ())
                respond $ responseLBS
                    (if request.pathInfo == ["v1", "clients"] then status201
                        else if request.requestMethod == "DELETE" then status202 else status200)
                    [(hContentType, "application/json")]
                    (if request.pathInfo == ["v1", "clients"]
                        then "{\"id\":\"frontend-1\",\"heartbeatIntervalMs\":100,\"leaseTimeoutMs\":500}"
                        else "{}")
        withTestClient application \client -> do
            result <- withLocalClientLease client \owned lease -> do
                attachLocalClientSession owned lease "session/a" >>= \case
                    Right _ -> pure ()
                    Left err -> expectationFailure (show err)
            result `shouldBe` Right ()
        readIORef observed `shouldReturn`
            [ ("POST", "/v1/clients", Nothing)
            , ("POST", "/v1/clients/frontend-1/sessions/session%2Fa", Just "frontend-1")
            , ("DELETE", "/v1/clients/frontend-1", Nothing)
            ]

    it "cancels the frontend action and releases ownership after a heartbeat failure" do
        cancelled <- newEmptyMVar
        block <- newEmptyMVar
        released <- newIORef False
        let application request respond = case request.pathInfo of
                ["v1", "clients"] -> respond (responseLBS status201 [(hContentType, "application/json")]
                    "{\"id\":\"frontend-1\",\"heartbeatIntervalMs\":100,\"leaseTimeoutMs\":500}")
                ["v1", "clients", _, "heartbeat"] -> respond (responseLBS status503 [(hContentType, "application/json")]
                    "{\"error\":{\"code\":\"unavailable\",\"message\":\"unavailable\"}}")
                _ -> do
                    writeIORef released True
                    respond (responseLBS status202 [(hContentType, "application/json")] "{}")
        withTestClient application \client -> do
            result <- timeout 1_000_000 $ withLocalClientLease client \_ _ ->
                (takeMVar block :: IO ()) `Safe.finally` putMVar cancelled ()
            case result of
                Just (Left (AgentServerHttpError 503 _ _)) -> pure ()
                other -> expectationFailure (show other)
        takeMVar cancelled
        readIORef released `shouldReturn` True

    it "fetches and acknowledges read receipts without retrying conflicts" do
        observed <- newEmptyMVar
        let receipt = "{\"revision\":\"revision-1\",\"unread\":true,\"first_unread_turn\":0}"
            application request respond = do
                body <- strictRequestBody request
                putMVar observed (request.requestMethod, rawPathInfo request, Aeson.decode body)
                respond $ responseLBS
                    (if request.requestMethod == "PATCH" then status409 else status200)
                    [(hContentType, "application/json")]
                    (if request.requestMethod == "PATCH"
                        then "{\"error\":{\"code\":\"read_state_conflict\",\"message\":\"refresh first\",\"requestId\":\"request-1\"}}"
                        else receipt)
        withTestClient application \client -> do
            getAgentServerSessionReadState client "session/a"
                `shouldReturn` Right (AgentServerSessionReadState "revision-1" True (Just 0))
            takeMVar observed `shouldReturn` ("GET", "/v1/sessions/session%2Fa/read-state", Nothing)
            let update = AgentServerUpdateReadState "revision-1" False
            updateAgentServerSessionReadState client "session/a" update >>= \case
                Left (AgentServerHttpError 409 (Just "read_state_conflict") _) -> pure ()
                other -> expectationFailure (show other)
            takeMVar observed `shouldReturn`
                ("PATCH", "/v1/sessions/session%2Fa/read-state", Just (Aeson.toJSON update))

    it "authenticates requests and encodes query values" do
        observed <- newEmptyMVar
        let application request respond = do
                putMVar
                    observed
                    ( rawQueryString request
                    , lookup hAuthorization request.requestHeaders
                    )
                respond $
                    responseLBS
                        status200
                        [(hContentType, "application/json")]
                        "{\"data\":[]}"
        withTestClient application \client -> do
            listAgentServerTurns client "session/a&other=1"
                `shouldReturn` Right (AgentServerTurnList [])
            takeMVar observed
                `shouldReturn` ( "?sessionId=session%2Fa%26other%3D1"
                               , Just ("Bearer " <> testBearer)
                               )

    it "encodes opaque identifiers as single path segments" do
        observed <- newEmptyMVar
        let application request respond = do
                putMVar observed (rawPathInfo request)
                respond $
                    responseLBS
                        status200
                        [(hContentType, "application/json")]
                        turnPayload
        withTestClient application \client -> do
            result <- getAgentServerTurn client "turn/../?admin=true"
            result `shouldSatisfy` isRight
            takeMVar observed
                `shouldReturn` "/v1/turns/turn%2F..%2F%3Fadmin=true"

    it "posts steering to the turn and surfaces a refusal" do
        observed <- newEmptyMVar
        let application request respond = do
                body <- strictRequestBody request
                putMVar observed (rawPathInfo request, Aeson.decode body)
                respond $
                    if rawPathInfo request == "/v1/turns/finished/steer"
                        then
                            responseLBS
                                status409
                                [(hContentType, "application/json")]
                                "{\"error\":{\"code\":\"turn_not_steerable\",\"message\":\"the turn is not running\",\"requestId\":\"request-1\"}}"
                        else
                            responseLBS
                                status202
                                [(hContentType, "application/json")]
                                turnPayload
            steering =
                AgentServerSteerTurnRequest
                    { steerTurnClientRequestId =
                        "01991f6d-7200-7000-8000-000000000004"
                    , steerTurnInput = "use the second unit"
                    }
        withTestClient application \client -> do
            steerAgentServerTurn client validTurnId steering
                >>= (`shouldSatisfy` isRight)
            takeMVar observed
                `shouldReturn`
                    ( "/v1/turns/01991f6d-7200-7000-8000-000000000001/steer"
                    , Just (Aeson.toJSON steering)
                    )
            result <- steerAgentServerTurn client "finished" steering
            case result of
                Left (AgentServerHttpError 409 (Just "turn_not_steerable") _) ->
                    pure ()
                other ->
                    expectationFailure
                        ("unexpected steering refusal: " <> show other)

    it "does not follow redirects" do
        redirectVisits <- newIORef (0 :: Int)
        let application request respond =
                if rawPathInfo request == "/redirect-target"
                    then do
                        modifyIORef' redirectVisits (+ 1)
                        respond $
                            responseLBS
                                status200
                                [(hContentType, "application/json")]
                                turnPayload
                    else
                        respond $
                            responseLBS
                                status302
                                [(hLocation, "/redirect-target")]
                                ""
        withTestClient application \client -> do
            result <- getAgentServerTurn client validTurnId
            case result of
                Left (AgentServerHttpError 302 _ _) -> pure ()
                other ->
                    expectationFailure
                        ("unexpected redirect result: " <> show other)
            readIORef redirectVisits `shouldReturn` 0

    it "rejects plaintext bearer transport beyond literal loopback" do
        result <-
            newAgentServerClient
                AgentServerClientConfig
                    { agentServerBaseUrl = "http://example.com:4096"
                    , agentServerCredentialFile = "not-read"
                    }
        case result of
            Left (AgentServerProtocolError message) ->
                message
                    `shouldBe` "plaintext agent-server URLs must use a literal loopback host"
            Left other ->
                expectationFailure
                    ("unexpected plaintext URL error: " <> show other)
            Right _ ->
                expectationFailure "plaintext remote URL was accepted"

    it "rejects oversized JSON responses" do
        let application _ respond =
                respond $
                    responseLBS
                        status200
                        [(hContentType, "application/json")]
                        (LazyByteString.replicate (1024 * 1024 + 1) 97)
        withTestClient application \client -> do
            result <- getAgentServerTurn client validTurnId
            case result of
                Left (AgentServerProtocolError message) ->
                    message `shouldBe` "agent-server response exceeded the size limit"
                other ->
                    expectationFailure
                        ("unexpected oversized response result: " <> show other)

    it "accepts a full page of maximally projected history" do
        let projectedText = Text.replicate (64 * 1024) "a"
            historyItem index =
                Aeson.object
                    [ "index" Aeson..= (index :: Int)
                    , "turn"
                        Aeson..= Aeson.object
                            [ "userText" Aeson..= projectedText
                            , "assistantText" Aeson..= (Nothing :: Maybe Text)
                            , "error" Aeson..= (Nothing :: Maybe Text)
                            , "projectionTruncated" Aeson..= False
                            ]
                    ]
            application _ respond =
                respond $
                    responseLBS
                        status200
                        [(hContentType, "application/json")]
                        ( Aeson.encode
                            ( Aeson.object
                                ["data" Aeson..= map historyItem [1 .. 20]]
                            )
                        )
        withTestClient application \client -> do
            result <- getAgentServerHistory client "2026-09-04-deadbeef"
            case result of
                Right history ->
                    length history.agentServerHistoryItems `shouldBe` 20
                Left err ->
                    expectationFailure
                        ("unexpected history response error: " <> show err)

    it "accepts a full page of maximally projected turns" do
        let maximumEncodedError =
                Text.replicate (16 * 1024 - 1) "\NUL" <> "…"
            failedTurn =
                Aeson.object
                    [ "id" Aeson..= validTurnId
                    , "sessionId" Aeson..= ("opaque-session" :: Text)
                    , "clientRequestId"
                        Aeson..= ("01991f6d-7200-7000-8000-000000000003" :: Text)
                    , "status" Aeson..= ("failed" :: Text)
                    , "createdAt"
                        Aeson..= ("2026-09-03T00:00:00Z" :: Text)
                    , "startedAt"
                        Aeson..= (Nothing :: Maybe Text)
                    , "finishedAt"
                        Aeson..= (Just ("2026-09-03T00:00:01Z" :: Text))
                    , "error"
                        Aeson..= Just maximumEncodedError
                    ]
            application _ respond =
                respond $
                    responseLBS
                        status200
                        [(hContentType, "application/json")]
                        ( Aeson.encode
                            ( Aeson.object
                                ["data" Aeson..= replicate 200 failedTurn]
                            )
                        )
        withTestClient application \client -> do
            result <- listAgentServerTurns client "opaque-session"
            case result of
                Right turns ->
                    length turns.agentServerTurns `shouldBe` 200
                Left err ->
                    expectationFailure
                        ("unexpected turn-list response error: " <> show err)

    it "accepts a full page of transport-bounded human requests" do
        let request =
                humanRequestValue
                    (Text.replicate (60 * 1024) "p")
            application _ respond =
                respond $
                    responseLBS
                        status200
                        [(hContentType, "application/json")]
                        ( Aeson.encode
                            ( Aeson.object
                                ["data" Aeson..= replicate 200 request]
                            )
                        )
        withTestClient application \client -> do
            result <- listAgentServerRequests client
            case result of
                Right requests ->
                    length requests.agentServerRequests `shouldBe` 200
                Left err ->
                    expectationFailure
                        ("unexpected request-list response error: " <> show err)

    it "streams a transport-bounded human request frame" do
        seenPromptLength <- newIORef Nothing
        let prompt = Text.replicate (60 * 1024) "p"
            requestPayload =
                Aeson.encode
                    ( Aeson.object
                        [ "id" Aeson..= (1 :: Int)
                        , "type" Aeson..= ("request.created" :: Text)
                        , "turnId" Aeson..= validTurnId
                        , "sessionId"
                            Aeson..= ("opaque-session" :: Text)
                        , "data"
                            Aeson..= Aeson.object
                                ["request" Aeson..= humanRequestValue prompt]
                        , "at"
                            Aeson..= ("2026-09-03T00:00:00Z" :: Text)
                        ]
                    )
            requestEvent =
                LazyByteString.concat
                    [ "id: 1\nevent: request.created\ndata: "
                    , requestPayload
                    , "\n\n"
                    ]
            application _ respond =
                respond $
                    responseLBS
                        status200
                        [(hContentType, "text/event-stream")]
                        (requestEvent <> completedEvent)
        withTestClient application \client -> do
            result <-
                streamAgentServerTurn client validTurnId Nothing \event -> do
                    case event.agentServerEventPayload of
                        AgentServerRequestCreated request ->
                            writeIORef
                                seenPromptLength
                                (Just (Text.length request.agentServerRequestPrompt))
                        _ -> pure ()
                    pure (Right ())
            result `shouldBe` Right AgentServerStreamCompleted
            readIORef seenPromptLength
                `shouldReturn` Just (Text.length prompt)

    it "streams terminal events without reconnecting" do
        seen <- newIORef (0 :: Int)
        cursor <- newEmptyMVar
        let application request respond = do
                putMVar cursor (lookup "Last-Event-ID" request.requestHeaders)
                respond $
                    responseLBS
                        status200
                        [(hContentType, "text/event-stream")]
                        completedEvent
        withTestClient application \client -> do
            result <-
                streamAgentServerTurn client validTurnId Nothing \_ -> do
                    modifyIORef' seen (+ 1)
                    pure (Right ())
            result `shouldBe` Right AgentServerStreamCompleted
            readIORef seen `shouldReturn` 1
            takeMVar cursor `shouldReturn` Just "0"

    it "finishes a slow presentation callback before durable terminal refetch" do
        delivered <- newIORef False
        let application request respond
                | request.pathInfo == ["v1", "events"] =
                    respond (responseLBS status200 [(hContentType, "text/event-stream")] completedEvent)
                | otherwise = respond (responseLBS status200 [(hContentType, "application/json")]
                    completedTurnPayload)
        withTestClient application \client -> do
            streamAgentServerTurn client validTurnId Nothing (\_ -> do
                threadDelay 1200000
                writeIORef delivered True
                pure (Right ())) `shouldReturn` Right AgentServerStreamCompleted
            readIORef delivered `shouldReturn` True

    it "surfaces replay resets from the live stream" do
        let application _ respond =
                respond $
                    responseLBS
                        status200
                        [(hContentType, "text/event-stream")]
                        ( "event: replay.reset\n"
                            <> "data: {\"reason\":\"event_gap\",\"refetch\":true}\n\n"
                        )
        withTestClient application \client -> do
            streamAgentServerTurn client validTurnId Nothing (\_ -> pure (Right ()))
                `shouldReturn` Right (AgentServerStreamNeedsRefetch 0)

    it "resumes after the last delivered event when a replay reset follows it" do
        let earlierEvent =
                "id: 7\nevent: turn.started\n\
                \data: {\"id\":7,\"type\":\"turn.started\",\
                \\"turnId\":\"01991f6d-7200-7000-8000-000000000001\",\
                \\"sessionId\":\"01991f6d-7200-7000-8000-000000000002\",\
                \\"data\":{},\"at\":\"2026-09-03T00:00:00Z\"}\n\n"
            resetEvent =
                "event: replay.reset\n\
                \data: {\"reason\":\"event_gap\",\"refetch\":true}\n\n"
            application _ respond =
                respond $
                    responseLBS
                        status200
                        [(hContentType, "text/event-stream")]
                        (earlierEvent <> resetEvent)
        withTestClient application \client -> do
            streamAgentServerTurn client validTurnId Nothing (\_ -> pure (Right ()))
                `shouldReturn` Right (AgentServerStreamNeedsRefetch 7)

    it "refetches when another instance durably finishes the turn" do
        let application request respond
                | rawPathInfo request == "/v1/events" =
                    respond $
                        responseStream
                            status200
                            [(hContentType, "text/event-stream")]
                            \write flush -> do
                                write ": keepalive\n\n"
                                flush
                                threadDelay (10 * 1000 * 1000)
                | otherwise =
                    respond $
                        responseLBS
                            status200
                            [(hContentType, "application/json")]
                            completedTurnPayload
        withTestClient application \client -> do
            result <-
                timeout
                    (3 * 1000 * 1000)
                    ( streamAgentServerTurn
                        client
                        validTurnId
                        Nothing
                        (\_ -> pure (Right ()))
                    )
            result
                `shouldBe` Just (Right (AgentServerStreamNeedsRefetch 0))

    it "refetches when another instance deletes the turn" do
        let application request respond
                | rawPathInfo request == "/v1/events" =
                    respond $
                        responseStream
                            status200
                            [(hContentType, "text/event-stream")]
                            \write flush -> do
                                write ": keepalive\n\n"
                                flush
                                threadDelay (10 * 1000 * 1000)
                | otherwise =
                    respond $
                        responseLBS
                            status404
                            [(hContentType, "application/json")]
                            ""
        withTestClient application \client -> do
            result <-
                timeout
                    (3 * 1000 * 1000)
                    ( streamAgentServerTurn
                        client
                        validTurnId
                        Nothing
                        (\_ -> pure (Right ()))
                    )
            result
                `shouldBe` Just (Right (AgentServerStreamNeedsRefetch 0))

    it "refetches when another instance durably requests human input" do
        requestQuery <- newEmptyMVar
        let application request respond
                | rawPathInfo request == "/v1/events" =
                    respond $
                        responseStream
                            status200
                            [(hContentType, "text/event-stream")]
                            \write flush -> do
                                write ": keepalive\n\n"
                                flush
                                threadDelay (10 * 1000 * 1000)
                | rawPathInfo request == "/v1/requests" =
                    do
                        putMVar requestQuery (rawQueryString request)
                        respond $
                            responseLBS
                                status200
                                [(hContentType, "application/json")]
                                pendingRequestPayload
                | otherwise =
                    respond $
                        responseLBS
                            status200
                            [(hContentType, "application/json")]
                            turnPayload
        withTestClient application \client -> do
            result <-
                timeout
                    (3 * 1000 * 1000)
                    ( streamAgentServerTurn
                        client
                        validTurnId
                        Nothing
                        (\_ -> pure (Right ()))
                    )
            result
                `shouldBe` Just (Right (AgentServerStreamNeedsRefetch 0))
            takeMVar requestQuery
                `shouldReturn` "?turnId=01991f6d-7200-7000-8000-000000000001"

    it "keeps a known pending dialog on one healthy stream across durable polls" do
        subscriptions <- newIORef (0 :: Int)
        let application request respond
                | rawPathInfo request == "/v1/events" = do
                    modifyIORef' subscriptions (+ 1)
                    respond $ responseStream status200 [(hContentType, "text/event-stream")] \write flush -> do
                        write ": connected\n\n"
                        flush
                        threadDelay 1200000
                        write (lazyByteString completedEvent)
                        flush
                | rawPathInfo request == "/v1/requests" =
                    respond (responseLBS status200 [(hContentType, "application/json")] pendingRequestPayload)
                | otherwise = respond (responseLBS status200 [(hContentType, "application/json")] turnPayload)
        withTestClient application \client -> do
            streamAgentServerTurnWithKnownRequests client validTurnId Nothing
                (pure ["01991f6d-7200-7000-8000-000000000004"]) (const (pure (Right ())))
                `shouldReturn` Right AgentServerStreamCompleted
            readIORef subscriptions `shouldReturn` 1

    it "bounds an unterminated SSE frame" do
        let application _ respond =
                respond $
                    responseLBS
                        status200
                        [(hContentType, "text/event-stream")]
                        (LazyByteString.replicate (1024 * 1024 + 1) 97)
        withTestClient application \client -> do
            result <-
                streamAgentServerTurn client validTurnId Nothing (\_ -> pure (Right ()))
            case result of
                Left (AgentServerProtocolError message) ->
                    message `shouldBe` "agent-server SSE frame exceeded the size limit"
                other ->
                    expectationFailure
                        ("unexpected oversized stream result: " <> show other)

withTestClient :: Application -> (AgentServerClient -> IO value) -> IO value
withTestClient application action =
    testWithApplication (pure application) \port ->
        withCredentialFile \credentialFile -> do
            created <-
                newAgentServerClient
                    AgentServerClientConfig
                        { agentServerBaseUrl =
                            "http://127.0.0.1:" <> Text.pack (show port)
                        , agentServerCredentialFile = credentialFile
                        }
            case created of
                Left err ->
                    expectationFailure
                        ("could not create test client: " <> show err)
                        >> fail "could not create test client"
                Right client -> action client

withCredentialFile :: (FilePath -> IO value) -> IO value
withCredentialFile =
    bracket createCredential removeFile
  where
    createCredential = do
        temporaryDirectory <- getTemporaryDirectory
        (path, handle) <-
            openBinaryTempFile temporaryDirectory "agent-server-client-token"
        ByteString.hPut handle testBearer
        hClose handle
        setFileMode path 0o600
        pure path

validTurnId :: Text
validTurnId = "01991f6d-7200-7000-8000-000000000001"

testBearer :: ByteString.ByteString
testBearer = "01234567890123456789012345678901"

turnPayload :: LazyByteString.ByteString
turnPayload =
    "{\"id\":\"01991f6d-7200-7000-8000-000000000001\",\
    \\"sessionId\":\"01991f6d-7200-7000-8000-000000000002\",\
    \\"clientRequestId\":\"01991f6d-7200-7000-8000-000000000003\",\
    \\"status\":\"queued\",\"createdAt\":\"2026-09-03T00:00:00Z\",\
    \\"startedAt\":null,\"finishedAt\":null,\"error\":null}"

completedEvent :: LazyByteString.ByteString
completedEvent =
    "id: 1\nevent: turn.completed\n\
    \data: {\"id\":1,\"type\":\"turn.completed\",\
    \\"turnId\":\"01991f6d-7200-7000-8000-000000000001\",\
    \\"sessionId\":\"01991f6d-7200-7000-8000-000000000002\",\
    \\"data\":{},\"at\":\"2026-09-03T00:00:00Z\"}\n\n"

completedTurnPayload :: LazyByteString.ByteString
completedTurnPayload =
    "{\"id\":\"01991f6d-7200-7000-8000-000000000001\",\
    \\"sessionId\":\"01991f6d-7200-7000-8000-000000000002\",\
    \\"clientRequestId\":\"01991f6d-7200-7000-8000-000000000003\",\
    \\"status\":\"completed\",\"createdAt\":\"2026-09-03T00:00:00Z\",\
    \\"startedAt\":\"2026-09-03T00:00:01Z\",\
    \\"finishedAt\":\"2026-09-03T00:00:02Z\",\"error\":null}"

pendingRequestPayload :: LazyByteString.ByteString
pendingRequestPayload =
    "{\"data\":[{\"id\":\"01991f6d-7200-7000-8000-000000000004\",\
    \\"turnId\":\"01991f6d-7200-7000-8000-000000000001\",\
    \\"sessionId\":\"opaque-session\",\
    \\"kind\":\"tool_approval\",\"prompt\":\"Approve?\",\
    \\"options\":[\"approve\",\"cancel\"],\
    \\"createdAt\":\"2026-09-03T00:00:01Z\"}]}"

humanRequestValue :: Text -> Aeson.Value
humanRequestValue prompt =
    Aeson.object
        [ "id"
            Aeson..= ("01991f6d-7200-7000-8000-000000000004" :: Text)
        , "turnId" Aeson..= validTurnId
        , "sessionId" Aeson..= ("opaque-session" :: Text)
        , "kind" Aeson..= ("tool_approval" :: Text)
        , "prompt" Aeson..= prompt
        , "options" Aeson..= (["approve", "cancel"] :: [Text])
        , "createdAt" Aeson..= ("2026-09-03T00:00:01Z" :: Text)
        ]

isRight :: Either left right -> Bool
isRight = \case
    Left _ -> False
    Right _ -> True
