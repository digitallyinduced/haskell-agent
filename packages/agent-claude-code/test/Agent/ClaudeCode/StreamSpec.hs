{-# LANGUAGE OverloadedStrings #-}

module Agent.ClaudeCode.StreamSpec (spec) where

import Agent.ClaudeCode.Stream
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
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = do
    describe "Claude Code stream-json" do
        it "buffers canonical records until result and aggregates model usage" do
            (afterAssistant, assistantEvents, _) <-
                consume subscriptionInitialized $
                    "{\"type\":\"assistant\",\"uuid\":\"assistant-1\",\
                    \\"session_id\":\"" <> session <> "\",\
                    \\"message\":{\"content\":[{\"type\":\"text\",\
                    \\"text\":\"checking\"},{\"type\":\"tool_use\",\
                    \\"id\":\"tool-1\",\"name\":\"Read\",\
                    \\"input\":{\"file_path\":\"README.md\"}}]}}"
            (afterTool, toolEvents, _) <-
                consume afterAssistant $
                    "{\"type\":\"user\",\"uuid\":\"user-1\",\
                    \\"session_id\":\"" <> session <> "\",\
                    \\"message\":{\"content\":[{\"type\":\"tool_result\",\
                    \\"tool_use_id\":\"tool-1\",\"content\":\"contents\"}]}}"
            (_afterResult, resultEvents, completion) <-
                consume afterTool $
                    "{\"type\":\"result\",\"uuid\":\"result-1\",\
                    \\"subtype\":\"success\",\"is_error\":false,\
                    \\"session_id\":\"" <> session <> "\",\
                    \\"result\":\"final answer\",\"usage\":{\"input_tokens\":2,\
                    \\"cache_creation_input_tokens\":3,\
                    \\"cache_read_input_tokens\":5,\"output_tokens\":7},\
                    \\"modelUsage\":{\"claude-main\":{\"inputTokens\":2,\
                    \\"outputTokens\":7,\"cacheReadInputTokens\":5,\
                    \\"cacheCreationInputTokens\":3},\"claude-helper\":{\
                    \\"inputTokens\":100,\"outputTokens\":4,\
                    \\"cacheReadInputTokens\":0,\
                    \\"cacheCreationInputTokens\":0}}}"

            assistantEvents `shouldBe` []
            toolEvents `shouldBe` []
            resultEvents `shouldBe`
                [ ToolStarted expectedToolCall
                , ToolFinished expectedToolResult
                , TextDelta "final answer"
                ]
            completion `shouldBe`
                Just CompletedTurn
                    { sessionId = sessionText
                    , assistantText = Just "final answer"
                    , tokenUsage = TokenUsage
                        { inputTokens = 10
                        , outputTokens = 7
                        , cachedTokens = 5
                        }
                    , cumulativeModelUsage =
                        Just TokenUsage
                            { inputTokens = 110
                            , outputTokens = 11
                            , cachedTokens = 5
                            }
                    }

        it "uses result text as the authoritative assistant response" do
            (afterAssistant, events, _) <-
                consume subscriptionInitialized
                    "{\"type\":\"assistant\",\"uuid\":\"refused\",\
                    \\"message\":{\"content\":[{\"type\":\"text\",\
                    \\"text\":\"refused text\"}]}}"
            (_, resultEvents, completion) <-
                consume afterAssistant $
                    "{\"type\":\"result\",\"uuid\":\"result-authoritative\",\
                    \\"subtype\":\"success\",\"is_error\":false,\
                    \\"session_id\":\"" <> session <> "\",\
                    \\"result\":\"replacement text\",\"usage\":{}}"
            events `shouldBe` []
            resultEvents `shouldBe` [TextDelta "replacement text"]
            completion `shouldBe`
                Just CompletedTurn
                    { sessionId = sessionText
                    , assistantText = Just "replacement text"
                    , tokenUsage = TokenUsage 0 0 0
                    , cumulativeModelUsage = Nothing
                    }

        it "falls back to buffered assistant text when result text is absent" do
            (afterAssistant, events, _) <-
                consume subscriptionInitialized
                    "{\"type\":\"assistant\",\"uuid\":\"assistant-fallback\",\
                    \\"message\":{\"content\":[{\"type\":\"text\",\
                    \\"text\":\"\\n fallback \\n\"}]}}"
            (_, resultEvents, completion) <-
                consume afterAssistant $
                    "{\"type\":\"result\",\"uuid\":\"result-fallback\",\
                    \\"subtype\":\"success\",\"is_error\":false,\
                    \\"session_id\":\"" <> session <> "\",\
                    \\"result\":\"\",\"usage\":{}}"
            events `shouldBe` []
            resultEvents `shouldBe` [TextDelta "\n fallback \n"]
            completion `shouldBe`
                Just CompletedTurn
                    { sessionId = sessionText
                    , assistantText = Just "\n fallback \n"
                    , tokenUsage = TokenUsage 0 0 0
                    , cumulativeModelUsage = Nothing
                    }

        it "evicts superseded assistant and tool records" do
            (afterOldAssistant, _, _) <-
                consume subscriptionInitialized
                    "{\"type\":\"assistant\",\"uuid\":\"old-assistant\",\
                    \\"message\":{\"content\":[{\"type\":\"text\",\
                    \\"text\":\"old text\"},{\"type\":\"tool_use\",\
                    \\"id\":\"old-tool\",\"name\":\"Read\",\"input\":{}}]}}"
            (afterOldResult, _, _) <-
                consume afterOldAssistant
                    "{\"type\":\"user\",\"uuid\":\"old-result\",\
                    \\"message\":{\"content\":[{\"type\":\"tool_result\",\
                    \\"tool_use_id\":\"old-tool\",\"content\":\"old\"}]}}"
            (afterReplacement, replacementEvents, _) <-
                consume afterOldResult
                    "{\"type\":\"assistant\",\"uuid\":\"replacement\",\
                    \\"supersedes\":[\"old-assistant\",\"old-result\"],\
                    \\"message\":{\"content\":[{\"type\":\"text\",\
                    \\"text\":\"new text\"}]}}"
            (_, finalEvents, completion) <-
                consume afterReplacement $
                    "{\"type\":\"result\",\"uuid\":\"result-superseded\",\
                    \\"subtype\":\"success\",\"is_error\":false,\
                    \\"session_id\":\"" <> session <> "\",\
                    \\"result\":\"\",\"usage\":{}}"
            replacementEvents `shouldBe` []
            finalEvents `shouldBe` [TextDelta "new text"]
            fmap (.assistantText) completion `shouldBe` Just (Just "new text")

        it "honors fallback retraction notices and tombstones late duplicates" do
            let retractedAssistant =
                    "{\"type\":\"assistant\",\"uuid\":\"retracted-assistant\",\
                    \\"message\":{\"content\":[{\"type\":\"text\",\
                    \\"text\":\"must disappear\"},{\"type\":\"tool_use\",\
                    \\"id\":\"retracted-tool\",\"name\":\"Read\",\
                    \\"input\":{}}]}}"
            (afterAssistant, _, _) <-
                consume subscriptionInitialized retractedAssistant
            (afterResult, _, _) <-
                consume afterAssistant
                    "{\"type\":\"user\",\"uuid\":\"retracted-result\",\
                    \\"message\":{\"content\":[{\"type\":\"tool_result\",\
                    \\"tool_use_id\":\"retracted-tool\",\
                    \\"content\":\"must disappear\"}]}}"
            (afterNotice, noticeEvents, _) <-
                consume afterResult
                    "{\"type\":\"system\",\
                    \\"subtype\":\"model_refusal_fallback\",\
                    \\"uuid\":\"fallback-notice\",\
                    \\"retracted_message_uuids\":[\
                    \\"retracted-assistant\",\"retracted-result\"]}"
            (afterLateDuplicate, lateEvents, _) <-
                consume afterNotice retractedAssistant
            (_, finalEvents, _) <-
                consume afterLateDuplicate $
                    "{\"type\":\"result\",\"uuid\":\"result-retracted\",\
                    \\"subtype\":\"success\",\"is_error\":false,\
                    \\"session_id\":\"" <> session <> "\",\
                    \\"result\":\"replacement\",\"usage\":{}}"
            noticeEvents `shouldBe` []
            lateEvents `shouldBe` []
            finalEvents `shouldBe` [TextDelta "replacement"]

        it "emits only matching surviving tool starts and results" do
            (afterMissingResult, _, _) <-
                consume subscriptionInitialized
                    "{\"type\":\"user\",\"uuid\":\"missing-result\",\
                    \\"message\":{\"content\":[{\"type\":\"tool_result\",\
                    \\"tool_use_id\":\"missing\",\"content\":\"ignored\"}]}}"
            (afterCall, _, _) <-
                consume afterMissingResult
                    "{\"type\":\"assistant\",\"uuid\":\"assistant-tool\",\
                    \\"message\":{\"content\":[{\"type\":\"tool_use\",\
                    \\"id\":\"tool-1\",\"name\":\"Read\",\
                    \\"input\":{\"file_path\":\"README.md\"}}]}}"
            (afterResult, _, _) <-
                consume afterCall
                    "{\"type\":\"user\",\"uuid\":\"user-tool\",\
                    \\"message\":{\"content\":[{\"type\":\"tool_result\",\
                    \\"tool_use_id\":\"tool-1\",\
                    \\"content\":[{\"type\":\"text\",\
                    \\"text\":\"contents\"}]}]}}"
            (_, events, _) <-
                consume afterResult $
                    "{\"type\":\"result\",\"uuid\":\"result-tool\",\
                    \\"subtype\":\"success\",\"is_error\":false,\
                    \\"session_id\":\"" <> session <> "\",\
                    \\"result\":\"done\",\"usage\":{}}"
            events `shouldBe`
                [ ToolStarted expectedToolCall
                , ToolFinished expectedToolResult
                , TextDelta "done"
                ]

        it "ignores partial, thinking, unknown, and forwarded subagent records" do
            (afterPartial, partialEvents, _) <-
                consume subscriptionInitialized
                    "{\"type\":\"stream_event\",\"uuid\":\"partial-1\",\
                    \\"event\":{\"type\":\"content_block_delta\",\
                    \\"delta\":{\"type\":\"text_delta\",\
                    \\"text\":\"uncommitted\"}}}"
            (afterThinking, thinkingEvents, _) <-
                consume afterPartial
                    "{\"type\":\"assistant\",\"uuid\":\"thinking-1\",\
                    \\"message\":{\"content\":[{\"type\":\"thinking\",\
                    \\"thinking\":\"private\"}]}}"
            (afterNested, nestedEvents, _) <-
                consume afterThinking
                    "{\"type\":\"assistant\",\"uuid\":\"nested-1\",\
                    \\"parent_tool_use_id\":\"agent-tool\",\
                    \\"message\":{\"content\":[{\"type\":\"text\",\
                    \\"text\":\"nested private output\"}]}}"
            (afterUnknown, unknownEvents, _) <-
                consume afterNested
                    "{\"type\":\"future_protocol_record\",\"uuid\":\"future-1\"}"
            partialEvents `shouldBe` []
            thinkingEvents `shouldBe` []
            nestedEvents `shouldBe` []
            unknownEvents `shouldBe` []
            show afterUnknown `shouldNotContain` "private"
            show afterUnknown `shouldNotContain` "uncommitted"

        it "deduplicates complete records by wire UUID" do
            let line =
                    "{\"type\":\"assistant\",\"uuid\":\"duplicate\",\
                    \\"message\":{\"content\":[{\"type\":\"text\",\
                    \\"text\":\"once\"}]}}"
            (afterFirst, firstEvents, _) <-
                consume subscriptionInitialized line
            (afterSecond, secondEvents, _) <-
                consume afterFirst line
            (_, resultEvents, _) <-
                consume afterSecond $
                    "{\"type\":\"result\",\"uuid\":\"result-duplicate\",\
                    \\"subtype\":\"success\",\"is_error\":false,\
                    \\"session_id\":\"" <> session <> "\",\
                    \\"result\":\"\",\"usage\":{}}"
            firstEvents `shouldBe` []
            secondEvents `shouldBe` []
            resultEvents `shouldBe` [TextDelta "once"]

        it "turns result failures and control requests into terminal errors" do
            (afterFailure, _, failureCompletion) <-
                consume emptyStreamAccumulator
                    "{\"type\":\"result\",\"uuid\":\"failed\",\
                    \\"subtype\":\"error_during_execution\",\
                    \\"is_error\":true,\"api_error_status\":401,\
                    \\"errors\":[\"login expired\"],\"usage\":{},\
                    \\"session_id\":\"00000000-0000-4000-8000-000000000001\"}"
            failureCompletion `shouldBe` Nothing
            streamAccumulatorError afterFailure `shouldBe`
                Just
                    "Claude Code error_during_execution (HTTP 401): login expired"

            (afterControl, _, controlCompletion) <-
                consume emptyStreamAccumulator
                    "{\"type\":\"control_request\",\"uuid\":\"control-1\",\
                    \\"request_id\":\"request-1\",\
                    \\"request\":{\"subtype\":\"can_use_tool\"}}"
            controlCompletion `shouldBe` Nothing
            streamAccumulatorError afterControl `shouldBe`
                Just
                    "Claude Code requested interactive protocol input that this backend does not support."

        it "suppresses synthetic assistant text on API-error frames" do
            (afterAssistant, events, completion) <-
                consume emptyStreamAccumulator
                    "{\"type\":\"assistant\",\"uuid\":\"assistant-error\",\
                    \\"session_id\":\"00000000-0000-4000-8000-000000000001\",\
                    \\"error\":\"rate_limit\",\"message\":{\"content\":[{\
                    \\"type\":\"text\",\"text\":\"Rate limit reached\"}]}}"
            events `shouldBe` []
            completion `shouldBe` Nothing
            show afterAssistant `shouldNotContain` "Rate limit reached"

        it "rejects an unexpected API-key credential source" do
            (afterInit, events, completion) <-
                consume emptyStreamAccumulator
                    "{\"type\":\"system\",\"subtype\":\"init\",\
                    \\"uuid\":\"init-api-key\",\"apiKeySource\":\
                    \\"ANTHROPIC_API_KEY\"}"
            events `shouldBe` []
            completion `shouldBe` Nothing
            streamAccumulatorError afterInit `shouldBe`
                Just
                    "Claude Code selected non-subscription credential source ANTHROPIC_API_KEY."
            (missingSource, _, _) <-
                consume emptyStreamAccumulator
                    "{\"type\":\"system\",\"subtype\":\"init\",\
                    \\"uuid\":\"init-no-source\"}"
            streamAccumulatorError missingSource `shouldBe`
                Just "Claude Code did not identify its credential source."

        it "requires an exact successful result shape" do
            (withoutSession, eventsWithoutSession, completionWithoutSession) <-
                consume subscriptionInitialized
                    "{\"type\":\"result\",\"uuid\":\"result-no-session\",\
                    \\"subtype\":\"success\",\"is_error\":false,\
                    \\"result\":\"done\",\"usage\":{}}"
            eventsWithoutSession `shouldBe` []
            completionWithoutSession `shouldBe` Nothing
            streamAccumulatorError withoutSession `shouldBe`
                Just
                    "Claude Code completed a turn without reporting a session ID."

            (withoutErrorFlag, eventsWithoutFlag, completionWithoutFlag) <-
                consume subscriptionInitialized $
                    "{\"type\":\"result\",\"uuid\":\"result-no-flag\",\
                    \\"subtype\":\"success\",\"session_id\":\""
                        <> session
                        <> "\",\"result\":\"done\",\"usage\":{}}"
            eventsWithoutFlag `shouldBe` []
            completionWithoutFlag `shouldBe` Nothing
            streamAccumulatorError withoutErrorFlag
                `shouldSatisfy` (/= Nothing)
            (withoutInit, eventsWithoutInit, completionWithoutInit) <-
                consume emptyStreamAccumulator $
                    "{\"type\":\"result\",\"uuid\":\"result-no-init\",\
                    \\"subtype\":\"success\",\"is_error\":false,\
                    \\"session_id\":\"" <> session <> "\",\
                    \\"result\":\"done\",\"usage\":{}}"
            eventsWithoutInit `shouldBe` []
            completionWithoutInit `shouldBe` Nothing
            streamAccumulatorError withoutInit `shouldBe`
                Just
                    "Claude Code completed before confirming subscription authentication."

        it "rejects malformed records and visible records without UUIDs" do
            consumeStreamLine emptyStreamAccumulator "{not-json"
                `shouldSatisfy` \case
                    Left message ->
                        "Invalid Claude Code stream JSON"
                            `Text.isInfixOf` message
                    Right _ -> False
            consumeStreamLine emptyStreamAccumulator "[]"
                `shouldBe`
                    Left
                        "Invalid Claude Code stream record: expected a JSON object."
            (withoutUuid, events, completion) <-
                consume emptyStreamAccumulator
                    "{\"type\":\"assistant\",\"message\":{\"content\":[{\
                    \\"type\":\"text\",\"text\":\"unsafe\"}]}}"
            events `shouldBe` []
            completion `shouldBe` Nothing
            streamAccumulatorError withoutUuid `shouldBe`
                Just
                    "Claude Code emitted a visible record without a wire UUID."

        it "rejects unexpected conversation resets" do
            (afterReset, events, completion) <-
                consume emptyStreamAccumulator
                    "{\"type\":\"conversation_reset\",\"uuid\":\"reset-1\",\
                    \\"session_id\":\"00000000-0000-4000-8000-000000000001\",\
                    \\"new_conversation_id\":\"conversation-2\"}"
            events `shouldBe` []
            completion `shouldBe` Nothing
            streamAccumulatorError afterReset `shouldBe`
                Just
                    "Claude Code reset its conversation while a harness session was active."

        it "falls back to top-level usage when modelUsage is malformed" do
            (_, _, completion) <-
                consume subscriptionInitialized $
                    "{\"type\":\"result\",\"uuid\":\"result-bad-usage\",\
                    \\"subtype\":\"success\",\"is_error\":false,\
                    \\"session_id\":\"" <> session <> "\",\
                    \\"result\":\"done\",\"usage\":{\"input_tokens\":2,\
                    \\"cache_creation_input_tokens\":3,\
                    \\"cache_read_input_tokens\":5,\"output_tokens\":7},\
                    \\"modelUsage\":{\"bad\":{\"inputTokens\":\"nope\"}}}"
            fmap (.tokenUsage) completion `shouldBe`
                Just TokenUsage
                    { inputTokens = 10
                    , outputTokens = 7
                    , cachedTokens = 5
                    }
            fmap (.cumulativeModelUsage) completion `shouldBe` Just Nothing

session :: ByteString.ByteString
session = "00000000-0000-4000-8000-000000000001"

sessionText :: Text.Text
sessionText = "00000000-0000-4000-8000-000000000001"

subscriptionInitialized :: StreamAccumulator
subscriptionInitialized =
    case consumeStreamLine
        emptyStreamAccumulator
        "{\"type\":\"system\",\"subtype\":\"init\",\
        \\"uuid\":\"subscription-init\",\"apiKeySource\":\"none\"}" of
        Right (accumulator, [], Nothing) ->
            accumulator
        other ->
            error ("invalid subscription init fixture: " <> show other)

expectedToolCall :: ToolCall
expectedToolCall = ToolCall
    { callId = "tool-1"
    , name = "Read"
    , arguments = "{\"file_path\":\"README.md\"}"
    , callKind = FunctionCallKind
    , argumentsEncrypted = False
    }

expectedToolResult :: ToolCallResult
expectedToolResult = ToolCallResult
    { callId = "tool-1"
    , output = "contents"
    , callKind = FunctionCallKind
    }

consume
    :: StreamAccumulator
    -> ByteString.ByteString
    -> IO (StreamAccumulator, [LoopEvent], Maybe CompletedTurn)
consume accumulator line =
    case consumeStreamLine accumulator line of
        Left err ->
            expectationFailure ("expected Right, got Left " <> show err)
                >> fail "unreachable"
        Right value ->
            pure value
