{-# LANGUAGE OverloadedStrings #-}

module Agent.ClaudeCode.TranscriptSpec (spec) where

import Agent.ClaudeCode.Transcript
import Agent.Loop
    ( LoopEvent(..)
    , TokenUsage(..)
    )
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , ToolCallResult(..)
    )
import qualified Data.ByteString.Char8 as ByteString
import Test.Hspec

spec :: Spec
spec = do
    describe "decodeTranscriptLine" do
        it "ignores thinking while exposing assistant text and tool use" do
            facts <- expectRight $ decodeTranscriptLine $ ByteString.pack
                "{\"type\":\"assistant\",\"message\":{\"id\":\"msg-1\",\
                \\"content\":[{\"type\":\"thinking\",\"thinking\":\"secret\"},\
                \{\"type\":\"text\",\"text\":\"Checking...\"},\
                \{\"type\":\"tool_use\",\"id\":\"tool-1\",\"name\":\"Bash\",\
                \\"input\":{\"command\":\"pwd\"}}],\"stop_reason\":\"tool_use\",\
                \\"usage\":{\"input_tokens\":10,\"cache_creation_input_tokens\":3,\
                \\"cache_read_input_tokens\":5,\"output_tokens\":7}}}"
            facts `shouldBe`
                [ AssistantTextFact "Checking..."
                , ToolUseFact ToolCall
                    { callId = "tool-1"
                    , name = "Bash"
                    , arguments = "{\"command\":\"pwd\"}"
                    , callKind = FunctionCallKind
                    , argumentsEncrypted = False
                    }
                , UsageFact "msg-1" TokenUsage
                    { inputTokens = 18
                    , outputTokens = 7
                    , cachedTokens = 5
                    }
                ]
            show facts `shouldNotContain` "secret"

        it "parses string and structured tool results" do
            facts <- expectRight $ decodeTranscriptLine $ ByteString.pack
                "{\"type\":\"user\",\"message\":{\"content\":[\
                \{\"type\":\"tool_result\",\"tool_use_id\":\"tool-1\",\
                \\"content\":[{\"type\":\"text\",\"text\":\"line one\"},\
                \{\"type\":\"text\",\"text\":\"line two\"}]},\
                \{\"type\":\"tool_result\",\"tool_use_id\":\"tool-2\",\
                \\"content\":\"permission denied\",\"is_error\":true}]}}"
            facts `shouldBe`
                [ ToolResultFact ToolCallResult
                    { callId = "tool-1"
                    , output = "line one\nline two"
                    , callKind = FunctionCallKind
                    }
                , ToolResultFact ToolCallResult
                    { callId = "tool-2"
                    , output = "Error: permission denied"
                    , callKind = FunctionCallKind
                    }
                ]

        it "ignores unrelated valid JSONL records" do
            decodeTranscriptLine
                (ByteString.pack "{\"type\":\"mode\",\"mode\":\"default\"}")
                `shouldBe` Right []

        it "maps stop_sequence to a terminal transcript error" do
            facts <- expectRight $ decodeTranscriptLine $ ByteString.pack
                "{\"type\":\"assistant\",\"message\":{\"id\":\"msg-stop\",\
                \\"content\":[],\"stop_reason\":\"stop_sequence\"}}"
            facts `shouldBe`
                [TerminalErrorFact
                    "Claude Code ended the response with stop_sequence before completing the turn."]

        it "maps generic error records to terminal transcript errors" do
            facts <- expectRight $ decodeTranscriptLine $ ByteString.pack
                "{\"type\":\"error\",\"error\":{\"message\":\"trust failed\"}}"
            facts `shouldBe`
                [TerminalErrorFact "Claude Code error: trust failed"]

    describe "turn accumulation" do
        it "emits display events, deduplicates usage, and completes after turn_duration" do
            assistantFacts <- expectRight $ decodeTranscriptLine $ ByteString.pack
                "{\"type\":\"assistant\",\"message\":{\"id\":\"msg-1\",\
                \\"content\":[{\"type\":\"text\",\"text\":\"Checking...\"},\
                \{\"type\":\"tool_use\",\"id\":\"tool-1\",\"name\":\"Read\",\
                \\"input\":{\"file_path\":\"README.md\"}}],\
                \\"stop_reason\":\"tool_use\",\"usage\":{\"input_tokens\":2,\
                \\"cache_creation_input_tokens\":3,\"cache_read_input_tokens\":5,\
                \\"output_tokens\":7}}}"
            duplicateUsage <- expectRight $ decodeTranscriptLine $ ByteString.pack
                "{\"type\":\"assistant\",\"message\":{\"id\":\"msg-1\",\
                \\"content\":[{\"type\":\"thinking\",\"thinking\":\"hidden\"}],\
                \\"usage\":{\"input_tokens\":2,\"cache_creation_input_tokens\":3,\
                \\"cache_read_input_tokens\":5,\"output_tokens\":7}}}"
            resultFacts <- expectRight $ decodeTranscriptLine $ ByteString.pack
                "{\"type\":\"user\",\"message\":{\"content\":[\
                \{\"type\":\"tool_result\",\"tool_use_id\":\"tool-1\",\
                \\"content\":\"contents\"}]}}"
            finalFacts <- expectRight $ decodeTranscriptLine $ ByteString.pack
                "{\"type\":\"assistant\",\"message\":{\"id\":\"msg-2\",\
                \\"content\":[{\"type\":\"text\",\"text\":\"Done\"}],\
                \\"stop_reason\":\"end_turn\",\"usage\":{\"input_tokens\":1,\
                \\"cache_creation_input_tokens\":0,\"cache_read_input_tokens\":4,\
                \\"output_tokens\":2}}}"

            let (afterAssistant, assistantEvents, earlyCompletion) =
                    applyTranscriptFacts emptyTurnAccumulator assistantFacts
                (afterDuplicate, duplicateEvents, duplicateCompletion) =
                    applyTranscriptFacts afterAssistant duplicateUsage
                (afterResult, resultEvents, resultCompletion) =
                    applyTranscriptFacts afterDuplicate resultFacts
                (afterFinal, finalEvents, finalCompletion) =
                    applyTranscriptFacts afterResult finalFacts
                (_final, durationEvents, completion) =
                    applyTranscriptFacts afterFinal [TurnDurationFact]

            assistantEvents `shouldBe`
                [ TextDelta "Checking..."
                , ToolStarted ToolCall
                    { callId = "tool-1"
                    , name = "Read"
                    , arguments = "{\"file_path\":\"README.md\"}"
                    , callKind = FunctionCallKind
                    , argumentsEncrypted = False
                    }
                ]
            duplicateEvents `shouldBe` []
            resultEvents `shouldBe`
                [ ToolFinished ToolCallResult
                    { callId = "tool-1"
                    , output = "contents"
                    , callKind = FunctionCallKind
                    }
                ]
            finalEvents `shouldBe` [TextDelta "Done"]
            durationEvents `shouldBe` []
            earlyCompletion `shouldBe` Nothing
            duplicateCompletion `shouldBe` Nothing
            resultCompletion `shouldBe` Nothing
            finalCompletion `shouldBe` Nothing
            completion `shouldBe`
                Just CompletedTurn
                    { assistantText = Just "Checking...Done"
                    , tokenUsage = TokenUsage
                        { inputTokens = 15
                        , outputTokens = 9
                        , cachedTokens = 9
                        }
                    }

        it "does not complete on turn_duration without end_turn" do
            let (_accumulator, _events, completion) =
                    applyTranscriptFacts
                        emptyTurnAccumulator
                        [TurnDurationFact]
            completion `shouldBe` Nothing

        it "uses end_turn as a completion fallback only when the process exits" do
            let (accumulator, _events, completion) =
                    applyTranscriptFacts
                        emptyTurnAccumulator
                        [ AssistantTextFact "fallback"
                        , AssistantEndTurnFact
                        ]
            completion `shouldBe` Nothing
            finishTranscriptOnExit accumulator `shouldBe`
                Just CompletedTurn
                        { assistantText = Just "fallback"
                        , tokenUsage = TokenUsage 0 0 0
                        }

        it "recognizes terminal Claude API errors" do
            facts <- expectRight $ decodeTranscriptLine $ ByteString.pack
                "{\"type\":\"system\",\"subtype\":\"api_error\",\
                \\"retryAttempt\":1,\"maxRetries\":10,\
                \\"error\":{\"status\":401,\"message\":\"login expired\"}}"
            facts `shouldBe`
                [TerminalErrorFact
                    "Claude Code API error (HTTP 401): login expired"]

        it "only finishes tool results corresponding to a seen tool use" do
            let missingResult = ToolCallResult
                    { callId = "missing"
                    , output = "ignored"
                    , callKind = FunctionCallKind
                    }
                (_accumulator, events, _completion) =
                    applyTranscriptFacts
                        emptyTurnAccumulator
                        [ToolResultFact missingResult]
            events `shouldBe` []

        it "deduplicates complete transcript records by outer uuid" do
            record <- expectRight $ decodeClaudeTranscriptLine $
                ByteString.pack
                    "{\"uuid\":\"record-1\",\"type\":\"assistant\",\
                    \\"message\":{\"id\":\"msg-record\",\"content\":[\
                    \{\"type\":\"text\",\"text\":\"once\"}],\
                    \\"usage\":{\"input_tokens\":2,\
                    \\"cache_creation_input_tokens\":3,\
                    \\"cache_read_input_tokens\":5,\
                    \\"output_tokens\":7}}}"
            let (afterFirst, firstEvents) =
                    accumulateClaudeTranscriptRecord
                        emptyClaudeTranscriptAccumulator
                        record
                (afterDuplicate, duplicateEvents) =
                    accumulateClaudeTranscriptRecord afterFirst record
            firstEvents `shouldBe` [TextDelta "once"]
            duplicateEvents `shouldBe` []
            claudeTranscriptText afterDuplicate `shouldBe` Just "once"
            claudeTranscriptUsage afterDuplicate `shouldBe`
                TokenUsage
                    { inputTokens = 10
                    , outputTokens = 7
                    , cachedTokens = 5
                    }

expectRight :: Show error => Either error value -> IO value
expectRight = \case
    Left err ->
        expectationFailure ("expected Right, got Left " <> show err)
            >> fail "unreachable"
    Right value ->
        pure value
