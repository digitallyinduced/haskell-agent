module Agent.Server.EventSpec (spec) where

import Agent.Loop
    ( LoopEvent(..) )
import Agent.Server.Event
import Agent.Tools.DisplayMap (MapResult(..), MapLocation(..), renderMapResult)
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , ToolCallMode(..)
    , ToolCallResult(..)
    , functionToolCall
    , withToolCallMode
    )
import Data.Aeson
    ( encode
    , object
    , (.=)
    )
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as Text
import Test.Hspec

spec :: Spec
spec = describe "public loop-event projection" do
    it "preserves complete bounded map documents in SSE" do
        let result = MapResult "Places"
                [MapLocation (Text.pack (show index)) (Text.replicate 100 "x") 0 0 Nothing Nothing
                | index <- [1..100 :: Int]]
        case renderMapResult result of
            Left err -> expectationFailure (Text.unpack err)
            Right output -> do
                Text.length output `shouldSatisfy` (> 16 * 1024)
                let (eventType, value) = projectLoopEvent
                        (ToolFinished (ToolCallResult
                            "map-1" output FunctionCallKind BlockingToolCall [] Nothing))
                eventType `shouldBe` "tool.finished"
                value `shouldBe` object
                    [ "callId" .= ("map-1" :: Text.Text)
                    , "kind" .= ("function" :: Text.Text)
                    , "async" .= False
                    , "output" .= output
                    , "truncated" .= False
                    , "imageCount" .= (0 :: Int)
                    ]

    it "still truncates malformed or unsupported map output" do
        let output = "{\"type\":\"map\",\"version\":2,\"padding\":\""
                <> Text.replicate (20 * 1024) "x" <> "\"}"
            (_, value) = projectLoopEvent
                (ToolFinished (ToolCallResult
                    "map-invalid" output FunctionCallKind BlockingToolCall [] Nothing))
        LBS8.unpack (encode value) `shouldContain` "\"truncated\":true"
        LBS8.length (encode value) `shouldSatisfy` (< 18 * 1024)

    it "never serializes encrypted tool arguments" do
        let (_, value) = projectLoopEvent
                (ToolStarted ToolCall
                    { callId = "call-1"
                    , name = "secret_tool"
                    , arguments = "highly-secret-argument"
                    , callKind = FunctionCallKind
                    , argumentsEncrypted = True
                    })
            bytes = encode value
        LBS8.unpack bytes
            `shouldNotContain` "highly-secret-argument"
        LBS8.unpack bytes
            `shouldContain` "\"argumentsEncrypted\":true"

    it "projects asynchronous tool-call mode" do
        let (_, value) = projectLoopEvent
                (ToolStarted
                    (withToolCallMode AsyncToolCall
                        (functionToolCall "call-async" "exec" "{}")))
        LBS8.unpack (encode value)
            `shouldContain` "\"async\":true"

    it "projects asynchronous tool-result mode" do
        let (_, value) = projectLoopEvent
                (ToolFinished
                    (ToolCallResult
                        "call-async"
                        "completed"
                        FunctionCallKind AsyncToolCall [] Nothing))
        LBS8.unpack (encode value)
            `shouldContain` "\"async\":true"

    it "bounds streamed public text" do
        let input = Text.replicate (20 * 1024) "x"
            (bounded, truncated) = boundedPublicText input
        truncated `shouldBe` True
        Text.length bounded `shouldSatisfy` (< 17 * 1024)

    it "marks failed-attempt lifecycle as display-only" do
        let (discardedType, discarded) =
                projectLoopEvent ResponseAttemptDiscarded
            (failedType, failed) =
                projectLoopEvent ResponseAttemptFailed
        discardedType `shouldBe` "response.attempt.discarded"
        failedType `shouldBe` "response.attempt.failed"
        LBS8.unpack (encode discarded)
            `shouldContain` "\"displayOnly\":true"
        LBS8.unpack (encode failed)
            `shouldContain` "\"displayOnly\":true"

    it "projects model-context resets as display-only" do
        let (eventType, value) = projectLoopEvent ModelContextReset
        eventType `shouldBe` "model.context.reset"
        value `shouldBe` object ["displayOnly" .= True]

    it "redacts encrypted durable values and applies a total budget" do
        let public = projectPublicValue $
                object
                    [ "item" .= object
                        [ "encrypted_content"
                            .= ("opaque-ciphertext" :: String)
                        ]
                    , "large" .= Text.replicate (80 * 1024) "x"
                    ]
            bytes = LBS8.unpack (encode public)
        bytes `shouldNotContain` "opaque-ciphertext"
        bytes `shouldContain` "<redacted>"
        bytes `shouldContain` "\"projectionTruncated\":true"
