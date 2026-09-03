module Agent.Server.EventSpec (spec) where

import Agent.Loop
    ( LoopEvent(..) )
import Agent.Server.Event
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
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
