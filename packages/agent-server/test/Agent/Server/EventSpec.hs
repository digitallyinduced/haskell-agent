module Agent.Server.EventSpec (spec) where

import Agent.Loop
    ( LoopEvent(..) )
import Agent.Server.Event
import Agent.Runtime.AgentSnapshot
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
    , toJSON
    , object
    , (.=)
    )
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as Text
import Test.Hspec

spec :: Spec
spec = describe "public loop-event projection" do
    it "preserves the agent snapshot wire fields and all step states" do
        let states =
                [ (AgentStepRunning, "running")
                , (AgentStepCompleted, "completed")
                , (AgentStepFailed, "failed")
                , (AgentStepInfo, "info")
                ]
            snapshot = AgentSnapshot
                { agentPath = "/root/worker"
                , agentStatus = "active"
                , agentModel = Just "model"
                , agentSteps =
                    [ AgentStep state "title" (Just "detail")
                    | (state, _) <- states
                    ]
                }
        projectAgentEntries [snapshot] `shouldBe` toJSON
            [ object
                [ "path" .= ("/root/worker" :: Text.Text)
                , "status" .= ("active" :: Text.Text)
                , "model" .= ("model" :: Text.Text)
                , "steps" .=
                    [ object
                        [ "state" .= (state :: Text.Text)
                        , "title" .= ("title" :: Text.Text)
                        , "detail" .= ("detail" :: Text.Text)
                        ]
                    | (_, state) <- states
                    ]
                ]
            ]

    it "preserves absent snapshot metadata as JSON null" do
        projectAgentEntries [AgentSnapshot "/root" "active" Nothing
                [AgentStep AgentStepInfo "notice" Nothing]]
            `shouldBe` toJSON
                [ object
                    [ "path" .= ("/root" :: Text.Text)
                    , "status" .= ("active" :: Text.Text)
                    , "model" .= (Nothing :: Maybe Text.Text)
                    , "steps" .=
                        [ object
                            [ "state" .= ("info" :: Text.Text)
                            , "title" .= ("notice" :: Text.Text)
                            , "detail" .= (Nothing :: Maybe Text.Text)
                            ]
                        ]
                    ]
                ]

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
