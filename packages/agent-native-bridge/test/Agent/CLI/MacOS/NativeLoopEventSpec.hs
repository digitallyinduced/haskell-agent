module Agent.CLI.MacOS.NativeLoopEventSpec (spec) where

import Agent.CLI.MacOS.NativeLoopEvent
    ( encodeNativeLoopEvent
    , encodeNativeLoopEventWithChartCalls
    , encodeNativeUsageEvent
    )
import Agent.CLI.MacOS.NativeInteraction (boundedApprovalArguments)
import Agent.CLI.SessionAdmin (sessionToolEvent, sessionToolEventWithChartCalls)
import qualified Agent.Responses.Types as Responses
import Agent.Json (rawJsonFromEncoding)
import Agent.Tools.RenderChart (chartResultDocument, chartResultSummary)
import Agent.Loop
    ( LoopEvent(..)
    , TokenUsage(..)
    , TurnOutput(..)
    , emptyTurnOutput
    )
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , ToolCallMode(..)
    , ToolCallResult(..)
    , ToolOutcome(..)
    , withToolCallMode
    )
import qualified Data.ByteString as BS
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word8, Word32)
import Test.Hspec

spec :: Spec
spec = describe "native loop event binary encoding" do
    it "transports the complete chart independently of the bounded output preview" do
        let result = ToolCallResult
                { callId = "chart-call"
                , output = chartEnvelope
                , callKind = FunctionCallKind
                , toolResultMode = BlockingToolCall
                , toolResultImages = []
                , toolResultOutcome = Nothing
                }
        Text.length chartEnvelope `shouldSatisfy` (> 8192)
        let document = fromJust (chartResultDocument chartEnvelope)
            summary = fromJust (chartResultSummary chartEnvelope)
        encodeNativeLoopEventWithChartCalls (Set.singleton "chart-call") "turn" (ToolFinished result)
            `shouldBe` Just (frame 5 8
                ["turn", "chart-call", Text.unpack summary, Text.unpack document])
        fmap (BS.take 8) (encodeNativeLoopEvent "turn" (ToolFinished result))
            `shouldBe` Just (header 5 2)

    it "projects the same complete chart from durable response items on reload" do
        let item = Responses.FunctionCallOutputItem Responses.FunctionCallOutput
                { Responses.itemId = Nothing
                , Responses.callId = "chart-call"
                , Responses.name = Nothing
                , Responses.namespace = Nothing
                , Responses.provider = Nothing
                , Responses.output = rawJsonFromEncoding (Aeson.toEncoding chartEnvelope)
                , Responses.status = Nothing
                , Responses.async = Nothing
                , Responses.localOutcome = Nothing
                }
        case sessionToolEventWithChartCalls (Set.singleton "chart-call") item of
                    Just (Aeson.Object event) -> do
                        KeyMap.lookup "chart" event
                            `shouldBe` (chartResultDocument chartEnvelope
                                >>= Aeson.decodeStrict' . TextEncoding.encodeUtf8)
                        KeyMap.lookup "output" event
                            `shouldBe` (Aeson.String <$> chartResultSummary chartEnvelope)
                        KeyMap.lookup "truncated" event `shouldBe` Just (Aeson.Bool False)
                        case sessionToolEvent item of
                            Just (Aeson.Object ordinary) ->
                                KeyMap.lookup "chart" ordinary `shouldBe` Nothing
                            _ -> expectationFailure "ordinary output event missing"
                    _ -> expectationFailure "chart history event missing"

    it "does not attach a chart to a failed tool result" do
        let result = ToolCallResult
                { callId = "chart-call"
                , output = chartEnvelope
                , callKind = FunctionCallKind
                , toolResultMode = BlockingToolCall
                , toolResultImages = []
                , toolResultOutcome = Just ToolFailed
                }
        fmap (BS.take 8) (encodeNativeLoopEventWithChartCalls (Set.singleton "chart-call") "turn" (ToolFinished result))
            `shouldBe` Just (header 5 2)

    it "encodes text deltas with a versioned HAEV frame" do
        encodeNativeLoopEvent "turn" (TextDelta "hé")
            `shouldBe` Just (frame 2 0 ["turn", "hé"])

    it "re-emits updated tool metadata through the keyed tool frame" do
        let call = ToolCall
                { callId = "call-1"
                , name = "shell_command"
                , arguments = "{\"command\":\"git status\"}"
                , callKind = FunctionCallKind
                , argumentsEncrypted = False
                }
        encodeNativeLoopEvent "turn" (ToolUpdated call)
            `shouldSatisfy` hasKind 4
        encodeNativeLoopEvent "turn" (ToolArgumentsUpdated call)
            `shouldSatisfy` hasKind 4

    it "encodes reasoning and status events as distinct kinds" do
        encodeNativeLoopEvent "turn" (ReasoningDelta "checking")
            `shouldSatisfy` hasKind 1
        encodeNativeLoopEvent "turn" (ActivityUpdated "working")
            `shouldSatisfy` hasKind 3

    it "preserves tool flags and fields" do
        let call = ToolCall
                { callId = "call-1"
                , name = "read_file"
                , arguments = "{\"path\":\"/tmp/a\"}"
                , callKind = FunctionCallKind
                , argumentsEncrypted = True
                }
        case encodeNativeLoopEvent "turn" (ToolStarted call) of
            Nothing -> expectationFailure "native tool event failed to encode"
            Just encoded -> do
                BS.take 8 encoded `shouldBe` header 4 1
                BS.isInfixOf "call-1" encoded `shouldBe` True
                BS.isInfixOf "read_file" encoded `shouldBe` True
                BS.index encoded 7 `shouldBe` 1

    it "marks asynchronous tool calls with the forward-compatible flag" do
        let call = withToolCallMode AsyncToolCall ToolCall
                { callId = "call-async"
                , name = "exec"
                , arguments = "return tools.echo({})"
                , callKind = CustomCallKind
                , argumentsEncrypted = False
                }
        case encodeNativeLoopEvent "turn" (ToolStarted call) of
            Nothing -> expectationFailure "native tool event failed to encode"
            Just encoded -> BS.take 8 encoded `shouldBe` header 4 4

    it "marks asynchronous tool results with the forward-compatible flag" do
        let result = ToolCallResult
                { callId = "call-async"
                , toolResultMode = AsyncToolCall
                , toolResultImages = []
                , toolResultOutcome = Nothing
                , output = "done"
                , callKind = CustomCallKind
                }
        case encodeNativeLoopEvent "turn" (ToolFinished result) of
            Nothing -> expectationFailure "native tool event failed to encode"
            Just encoded -> BS.take 8 encoded `shouldBe` header 5 4

    it "encodes truncated tool output with its flag" do
        let result = ToolCallResult
                { callId = "call-1"
                , toolResultMode = BlockingToolCall
                , toolResultImages = []
                , toolResultOutcome = Nothing
                , output = Text.replicate 8193 "x"
                , callKind = FunctionCallKind
                }
        case encodeNativeLoopEvent "turn" (ToolFinished result) of
            Nothing -> expectationFailure "native tool event failed to encode"
            Just encoded -> BS.take 8 encoded `shouldBe` header 5 2

    it "keeps complete bounded integration arguments for native approval" do
        let arguments = Text.replicate 100000 "x"
            integrationCall = ToolCall
                { callId = "integration-1"
                , name = "mcp_call"
                , arguments
                , callKind = FunctionCallKind
                , argumentsEncrypted = False
                }
            ordinaryCall = integrationCall { name = "other_tool" }
        boundedApprovalArguments integrationCall `shouldBe` (arguments, False)
        boundedApprovalArguments ordinaryCall
            `shouldBe` (Text.take 8192 arguments, True)

    it "redacts computer screenshots from tool-finish frames" do
        let secret = "data:image/png;base64,private-screenshot" :: Text.Text
            result = ToolCallResult
                { callId = "computer-1"
                , toolResultMode = BlockingToolCall
                , toolResultImages = []
                , toolResultOutcome = Nothing
                , output = secret
                , callKind = ComputerFunctionCallKind
                }
        case encodeNativeLoopEvent "turn" (ToolFinished result) of
            Nothing -> expectationFailure "native tool event failed to encode"
            Just encoded -> do
                encoded `shouldSatisfy` BS.isInfixOf "Screenshot omitted from event"
                encoded `shouldNotSatisfy`
                    BS.isInfixOf (TextEncoding.encodeUtf8 secret)

    it "redacts nested JSON screenshot fields and image data" do
        let secret = "DATA:IMAGE/png;base64,nested-secret" :: Text.Text
            output =
                "{\"nested\":{\"screenshot_data_url\":\"discard\",\
                \\"preview\":\"" <> secret <> "\"}}"
            result = ToolCallResult
                { callId = "computer-nested"
                , toolResultMode = BlockingToolCall
                , toolResultImages = []
                , toolResultOutcome = Nothing
                , output
                , callKind = ComputerFunctionCallKind
                }
        case encodeNativeLoopEvent "turn" (ToolFinished result) of
            Nothing -> expectationFailure "native tool event failed to encode"
            Just encoded -> do
                encoded `shouldSatisfy`
                    BS.isInfixOf "Screenshot omitted from event"
                encoded `shouldNotSatisfy`
                    BS.isInfixOf (TextEncoding.encodeUtf8 secret)
                encoded `shouldNotSatisfy`
                    BS.isInfixOf "screenshot_data_url"

    it "redacts legacy JSON screenshots but preserves accessibility JSON" do
        let secret = "data:image/png;base64,private-screenshot" :: Text.Text
            accessibility = "\"accessibility_state\":{\"kind\":\"full\"}"
            result = ToolCallResult
                { callId = "computer-json"
                , toolResultMode = BlockingToolCall
                , toolResultImages = []
                , toolResultOutcome = Nothing
                , output =
                    "{\"screenshotDataUrl\":\"" <> secret <> "\","
                        <> accessibility <> "}"
                , callKind = ComputerFunctionCallKind
                }
        case encodeNativeLoopEvent "turn" (ToolFinished result) of
            Nothing -> expectationFailure "native tool event failed to encode"
            Just encoded -> do
                encoded `shouldNotSatisfy`
                    BS.isInfixOf (TextEncoding.encodeUtf8 secret)
                encoded `shouldSatisfy`
                    BS.isInfixOf (TextEncoding.encodeUtf8 accessibility)

    it "encodes terminal provider token usage without inventing cost" do
        let output =
                (emptyTurnOutput "response" [] Nothing)
                    { tokenUsage = TokenUsage 120 34 56 }
        encodeNativeLoopEvent "turn" (TurnFinished output)
            `shouldBe`
                Just
                    (frame 6 0 ["turn", "120", "34", "56"]
                        <> word32BE maxBound)

    it "encodes aggregate usage and exact provider-reported cost" do
        encodeNativeUsageEvent
            True
            "turn"
            (TokenUsage 200 50 80)
            (Just 0.0125)
            `shouldBe`
                Just
                    (frame 7 0
                        ["turn", "200", "50", "80", "1.25e-2"])

    it "does not encode turn-start lifecycle events as native loop frames" do
        encodeNativeLoopEvent "turn" TurnStarted `shouldBe` Nothing

chartEnvelope :: Text.Text
chartEnvelope = TextEncoding.decodeUtf8 . LBS.toStrict . Aeson.encode $
    Aeson.object
        [ "type" Aeson..= ("chart" :: Text.Text)
        , "summary" Aeson..= ("Measurements: 1 series, 1000 points." :: Text.Text)
        , "chart" Aeson..= Aeson.object
            [ "version" Aeson..= (1 :: Int)
            , "kind" Aeson..= ("line" :: Text.Text)
            , "title" Aeson..= ("Measurements" :: Text.Text)
            , "x_axis" Aeson..= Aeson.object ["type" Aeson..= ("number" :: Text.Text)]
            , "y_axis" Aeson..= Aeson.object []
            , "series" Aeson..=
                [ Aeson.object
                    [ "name" Aeson..= ("Observations" :: Text.Text)
                    , "points" Aeson..=
                        [ Aeson.object ["x" Aeson..= index, "y" Aeson..= index]
                        | index <- [1 .. 1000 :: Int]
                        ]
                    ]
                ]
            ]
        ]

frame :: Word8 -> Word8 -> [String] -> BS.ByteString
frame kind flags fields =
    header kind flags
        <> BS.concat
            [ field (TextEncoding.encodeUtf8 (Text.pack value))
            | value <- fields
            ]

header :: Word8 -> Word8 -> BS.ByteString
header kind flags =
    "HAEV" <> BS.pack [1, kind, 0, flags]

field :: BS.ByteString -> BS.ByteString
field bytes = word32BE (fromIntegral (BS.length bytes)) <> bytes

word32BE :: Word32 -> BS.ByteString
word32BE value = BS.pack
    [ fromIntegral (value `div` 16777216)
    , fromIntegral (value `div` 65536)
    , fromIntegral (value `div` 256)
    , fromIntegral value
    ]

hasKind :: Word8 -> Maybe BS.ByteString -> Bool
hasKind kind (Just encoded) =
    BS.take 8 encoded == header kind 0
hasKind _ Nothing = False
