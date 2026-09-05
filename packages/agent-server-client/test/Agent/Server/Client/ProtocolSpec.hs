module Agent.Server.Client.ProtocolSpec (spec) where

import Agent.Server.Client.Protocol
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = describe "agent-server client protocol" do
    it "decodes the server's opaque non-UUID session ids" do
        Aeson.eitherDecodeStrict'
            "{\"id\":\"2026-09-04-deadbeef\"}"
            `shouldBe` Right
                (AgentServerSession "2026-09-04-deadbeef")

    it "encodes a turn with its required idempotency key" do
        let request =
                AgentServerCreateTurnRequest
                    { createTurnClientRequestId =
                        "01991f6d-7200-7000-8000-000000000003"
                    , createTurnInput = "hello"
                    , createTurnImages = []
                    }
        ( Aeson.eitherDecode (Aeson.encode request) ::
                Either String Aeson.Value
            )
            `shouldBe` Right
                ( Aeson.object
                    [ "clientRequestId"
                        Aeson..= ( "01991f6d-7200-7000-8000-000000000003" ::
                                    Text
                                 )
                    , "input" Aeson..= ("hello" :: Text)
                    ]
                )

    it "base64-encodes attached images with their media type" do
        let request =
                AgentServerCreateTurnRequest
                    { createTurnClientRequestId = "request-1"
                    , createTurnInput = ""
                    , createTurnImages =
                        [ AgentServerTurnImage
                            { turnImageMimeType = "image/png"
                            , turnImageBytes = "\x89PNG"
                            }
                        ]
                    }
        ( Aeson.eitherDecode (Aeson.encode request) ::
                Either String Aeson.Value
            )
            `shouldBe` Right
                ( Aeson.object
                    [ "clientRequestId" Aeson..= ("request-1" :: Text)
                    , "input" Aeson..= ("" :: Text)
                    , "images"
                        Aeson..=
                            [ Aeson.object
                                [ "mimeType" Aeson..= ("image/png" :: Text)
                                , "data" Aeson..= ("iVBORw==" :: Text)
                                ]
                            ]
                    ]
                )

    it "decodes a queued turn with its client request id" do
        let payload =
                "{\"id\":\"01991f6d-7200-7000-8000-000000000001\","
                    <> "\"sessionId\":\"2026-09-04-deadbeef\","
                    <> "\"clientRequestId\":\"01991f6d-7200-7000-8000-000000000003\","
                    <> "\"status\":\"queued\","
                    <> "\"createdAt\":\"2026-09-03T00:00:00Z\","
                    <> "\"startedAt\":null,\"finishedAt\":null,\"error\":null}"
        case Aeson.eitherDecodeStrict' payload of
            Left message -> expectationFailure message
            Right (turn :: AgentServerTurn) -> do
                turn.agentServerTurnStatus
                    `shouldBe` AgentServerTurnQueued
                turn.agentServerTurnClientRequestId
                    `shouldBe` "01991f6d-7200-7000-8000-000000000003"

    it "rejects missing required nullable turn fields" do
        let payload =
                "{\"id\":\"01991f6d-7200-7000-8000-000000000001\","
                    <> "\"sessionId\":\"01991f6d-7200-7000-8000-000000000002\","
                    <> "\"clientRequestId\":\"01991f6d-7200-7000-8000-000000000003\","
                    <> "\"status\":\"queued\","
                    <> "\"createdAt\":\"2026-09-03T00:00:00Z\","
                    <> "\"finishedAt\":null,\"error\":null}"
        (Aeson.eitherDecodeStrict' payload :: Either String AgentServerTurn)
            `shouldSatisfy` isLeft

    it "decodes the canonical terminal result" do
        let payload =
                "{\"turn\":{"
                    <> "\"id\":\"01991f6d-7200-7000-8000-000000000001\","
                    <> "\"sessionId\":\"01991f6d-7200-7000-8000-000000000002\","
                    <> "\"clientRequestId\":\"01991f6d-7200-7000-8000-000000000003\","
                    <> "\"status\":\"completed\","
                    <> "\"createdAt\":\"2026-09-03T00:00:00Z\","
                    <> "\"startedAt\":\"2026-09-03T00:00:01Z\","
                    <> "\"finishedAt\":\"2026-09-03T00:00:02Z\","
                    <> "\"error\":null},\"output\":{"
                    <> "\"responseId\":\"resp_123\","
                    <> "\"assistantText\":\"done\","
                    <> "\"assistantTextTruncated\":true,"
                    <> "\"completion\":{\"status\":\"completed\"}}}"
        case Aeson.eitherDecodeStrict' payload of
            Left message -> expectationFailure message
            Right (result :: AgentServerTurnResult) ->
                result.agentServerResultOutput
                    `shouldBe` Just
                        ( AgentServerTurnOutput
                            "resp_123"
                            (Just "done")
                            True
                            AgentServerTurnComplete
                        )

    it "rejects a result that omits its required nullable output" do
        let payload =
                "{\"turn\":{"
                    <> "\"id\":\"01991f6d-7200-7000-8000-000000000001\","
                    <> "\"sessionId\":\"01991f6d-7200-7000-8000-000000000002\","
                    <> "\"clientRequestId\":\"01991f6d-7200-7000-8000-000000000003\","
                    <> "\"status\":\"failed\","
                    <> "\"createdAt\":\"2026-09-03T00:00:00Z\","
                    <> "\"startedAt\":null,\"finishedAt\":\"2026-09-03T00:00:02Z\","
                    <> "\"error\":\"failed\"}}"
        ( Aeson.eitherDecodeStrict' payload ::
                Either String AgentServerTurnResult
            )
            `shouldSatisfy` isLeft

    it "decodes an SSE event and validates its envelope" do
        let frame =
                "id: 41\r\nevent: response.text.delta\r\n"
                    <> "data: {\"id\":41,\"type\":\"response.text.delta\","
                    <> "\"turnId\":\"01991f6d-7200-7000-8000-000000000001\","
                    <> "\"sessionId\":\"2026-09-04-deadbeef\","
                    <> "\"data\":{\"text\":\"Hallo\"},"
                    <> "\"at\":\"2026-09-03T00:00:00Z\"}\r\n"
        case parseAgentServerSseFrame frame of
            Right
                ( Just
                        ( AgentServerSseEvent
                                AgentServerEvent
                                    { agentServerEventId = 41
                                    , agentServerEventPayload =
                                        AgentServerResponseTextDelta "Hallo"
                                    }
                            )
                    ) ->
                    pure ()
            other ->
                expectationFailure
                    ("unexpected SSE result: " <> show other)

    it "surfaces replay.reset separately from normal events" do
        parseAgentServerSseFrame
            ( "event: replay.reset\n"
                <> "data: {\"reason\":\"event_gap\",\"refetch\":true}\n"
            )
            `shouldBe` Right
                ( Just
                    ( AgentServerSseReplayReset
                        (AgentServerReplayReset "event_gap")
                    )
                )

    it "rejects a wire event id that disagrees with the JSON body" do
        let frame =
                "id: 8\nevent: turn.completed\n"
                    <> "data: {\"id\":9,\"type\":\"turn.completed\","
                    <> "\"turnId\":\"01991f6d-7200-7000-8000-000000000001\","
                    <> "\"sessionId\":\"01991f6d-7200-7000-8000-000000000002\","
                    <> "\"data\":{},"
                    <> "\"at\":\"2026-09-03T00:00:00Z\"}\n"
        parseAgentServerSseFrame frame
            `shouldBe` Left "SSE event id does not match its JSON envelope"

isLeft :: Either left right -> Bool
isLeft = \case
    Left _ -> True
    Right _ -> False
