module Agent.Server.ClientSpec (spec) where

import Agent.Server.Client
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Exception (bracket)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as Text
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (testWithApplication)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openBinaryTempFile)
import System.Posix.Files (setFileMode)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "agent-server HTTP client" do
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
                message `shouldBe`
                    "plaintext agent-server URLs must use a literal loopback host"
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
                        Aeson..=
                            ("01991f6d-7200-7000-8000-000000000003" :: Text)
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
                `shouldReturn` Right AgentServerStreamNeedsRefetch

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
                `shouldBe` Just (Right AgentServerStreamNeedsRefetch)

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
                `shouldBe` Just (Right AgentServerStreamNeedsRefetch)
            takeMVar requestQuery
                `shouldReturn`
                    "?turnId=01991f6d-7200-7000-8000-000000000001"

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
