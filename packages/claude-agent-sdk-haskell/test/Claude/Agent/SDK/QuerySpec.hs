module Claude.Agent.SDK.QuerySpec (spec) where

import Agent.Json (rawJsonBytes)
import Claude.Agent.SDK
import Claude.Agent.SDK.TestSupport
import qualified Data.ByteString as ByteString
import Data.IORef
    ( modifyIORef'
    , newIORef
    , readIORef
    )
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Test.Hspec

spec :: Spec
spec = describe "query" do
    it "parses typed messages and applies deduplication and retractions" do
        (result, messages) <- runQueryLines canonicalResponseLines

        completed <- expectRight result
        completed.sessionId `shouldBe` testSessionId
        completed.result `shouldBe` Just "canonical answer"
        completed.usage `shouldBe` Usage
            { inputTokens = 17
            , outputTokens = 5
            , cachedTokens = 4
            }
        Map.lookup "claude-test" completed.modelUsage
            `shouldSatisfy` \case
                Just ModelUsage
                    { inputTokens = 20
                    , outputTokens = 8
                    , cacheReadInputTokens = 6
                    , cacheCreationInputTokens = 2
                    } -> True
                _ -> False

        map messageUuid messages
            `shouldBe`
                [ Just "system-init"
                , Just "assistant-replacement"
                , Just "fallback"
                , Just "result"
                ]
        messages `shouldSatisfy` any hasCanonicalAssistant
        messages `shouldSatisfy` all (not . hasRetractedContent)

    it "renders structured tool_result content and retains its raw JSON" do
        (result, messages) <- runQueryLines
            [ structuredToolResultUser
            , successResult testSessionId
            ]

        _ <- expectRight result
        let toolResults =
                [ (toolUseId, rawJsonBytes content.raw, content.renderedText)
                | MessageUser UserMessage{content = blocks} <- messages
                , ToolResultBlock{toolUseId, content = Just content} <- blocks
                ]
        toolResults `shouldBe`
            [ ( "tool-references"
              , toolReferenceContent
              , "Tool reference: WebSearch\nTool reference: WebFetch"
              )
            , ( "tool-text-blocks"
              , "[{\"type\":\"text\",\"text\":\"first\"},\
                \{\"type\":\"text\",\"text\":\"second\"}]"
              , "first\nsecond"
              )
            , ( "tool-image"
              , imageContent
              , "[image image/png]"
              )
            , ( "tool-object"
              , "{\"custom\":1}"
              , "{\"custom\":1}"
              )
            , ( "tool-unknown-block"
              , "[{\"type\":\"mystery\",\"value\":2}]"
              , "{\"type\":\"mystery\",\"value\":2}"
              )
            , ( "tool-plain"
              , "\"plain output\""
              , "plain output"
              )
            ]

    it "retains unknown message and content variants for forward compatibility" do
        (result, messages) <- runQueryLines
            [ assistantWithUnknownBlock
            , "{\"type\":\"future_event\",\"payload\":{\"value\":1}}"
            , successResult testSessionId
            ]

        _ <- expectRight result
        messages `shouldSatisfy` any \case
            MessageAssistant AssistantMessage{content} ->
                any isUnknownBlock content
            _ -> False
        messages `shouldSatisfy` any \case
            MessageUnknown{} -> True
            _ -> False

    it "scopes nested supersedes so subagents cannot retract top-level output" do
        (result, messages) <- runQueryLines
            [ assistantLine "top-level" "keep me"
            , "{\"type\":\"assistant\",\"uuid\":\"nested\",\
              \\"parent_tool_use_id\":\"agent-tool\",\
              \\"supersedes\":[\"top-level\"],\"session_id\":\""
                <> testSessionId
                <> "\",\"message\":{\"content\":[{\"type\":\"text\",\
                   \\"text\":\"nested output\"}]}}"
            , successResult testSessionId
            ]

        _ <- expectRight result
        messages `shouldSatisfy` any \case
            MessageAssistant AssistantMessage
                { uuid = Just "top-level"
                , content = [TextBlock "keep me"]
                } ->
                True
            _ ->
                False

    it "deduplicates unknown UUIDs in their parent scope" do
        (result, messages) <- runQueryLines
            [ "{\"type\":\"assistant\",\"uuid\":\"parent-tool\",\
              \\"session_id\":\"" <> testSessionId <> "\",\
              \\"message\":{\"content\":[{\"type\":\"tool_use\",\
              \\"id\":\"agent-tool\",\"name\":\"Agent\",\"input\":{}}]}}"
            , "{\"type\":\"future_event\",\"uuid\":\"future-nested\",\
              \\"parent_tool_use_id\":\"agent-tool\",\
              \\"payload\":{\"value\":\"original\"}}"
            , "{\"type\":\"future_event\",\"uuid\":\"future-nested\",\
              \\"parent_tool_use_id\":\"agent-tool\",\
              \\"payload\":{\"value\":\"duplicate\"}}"
            , "{\"type\":\"assistant\",\"uuid\":\"top-level-replacement\",\
              \\"supersedes\":[\"future-nested\"],\"session_id\":\""
                <> testSessionId
                <> "\",\"message\":{\"content\":[{\"type\":\"text\",\
                   \\"text\":\"top-level output\"}]}}"
            , successResult testSessionId
            ]

        _ <- expectRight result
        filter ((== Just "future-nested") . messageUuid) messages
            `shouldSatisfy` \case
                [message] ->
                    messageParentToolUseId message == Just "agent-tool"
                        && case message of
                            MessageUnknown opaque ->
                                "\"original\""
                                    `ByteString.isInfixOf`
                                        rawJsonBytes opaque.raw
                            _ -> False
                        && not
                            (case message of
                                MessageUnknown opaque ->
                                    "\"duplicate\""
                                        `ByteString.isInfixOf`
                                            rawJsonBytes opaque.raw
                                _ -> False)
                _ ->
                    False

    it "applies refusal fallback retractions globally across parent scopes" do
        (result, messages) <- runQueryLines
            [ "{\"type\":\"future_event\",\"uuid\":\"future-nested\",\
              \\"parent_tool_use_id\":\"agent-tool\",\
              \\"payload\":{\"value\":\"must be retracted\"}}"
            , "{\"type\":\"system\",\"subtype\":\"model_refusal_fallback\",\
              \\"session_id\":\""
                <> testSessionId
                <> "\",\"uuid\":\"fallback\",\
                   \\"retracted_message_uuids\":[\"future-nested\"]}"
            , "{\"type\":\"future_event\",\"uuid\":\"future-nested\",\
              \\"parent_tool_use_id\":\"different-agent-tool\",\
              \\"payload\":{\"value\":\"must stay tombstoned\"}}"
            , successResult testSessionId
            ]

        _ <- expectRight result
        map messageUuid messages
            `shouldBe` [Just "fallback", Just "result"]
        show messages `shouldNotContain` "must be retracted"
        show messages `shouldNotContain` "must stay tombstoned"

    it "fails closed on malformed retraction identifier arrays" do
        mapM_
            (\malformedLine -> do
                (result, messages) <-
                    runQueryLines
                        [ assistantLine "visible-before-error" "do not publish"
                        , malformedLine
                        , successResult testSessionId
                        ]
                result `shouldSatisfy` \case
                    Left MessageParseError{parseError} ->
                        "non-empty strings"
                            `Text.isInfixOf` parseError
                    _ -> False
                messages `shouldBe` []
            )
            [ "{\"type\":\"assistant\",\"uuid\":\"malformed-supersedes\",\
              \\"supersedes\":[\"visible-before-error\",7],\
              \\"session_id\":\""
                <> testSessionId
                <> "\",\"message\":{\"content\":[{\"type\":\"text\",\
                   \\"text\":\"replacement\"}]}}"
            , "{\"type\":\"system\",\"subtype\":\"model_refusal_fallback\",\
              \\"session_id\":\""
                <> testSessionId
                <> "\",\"uuid\":\"malformed-fallback\",\
                   \\"retracted_message_uuids\":\"visible-before-error\"}"
            ]

    it "delivers tool results whose content is an array of blocks" do
        (result, messages) <- runQueryLines
            [ "{\"type\":\"assistant\",\"uuid\":\"tool-search\",\
              \\"session_id\":\""
                <> testSessionId
                <> "\",\"message\":{\"content\":[{\"type\":\"tool_use\",\
                   \\"id\":\"search-1\",\"name\":\"ToolSearch\",\
                   \\"input\":{\"query\":\"select:WebFetch\"}}]}}"
            , "{\"type\":\"user\",\"uuid\":\"tool-search-result\",\
              \\"session_id\":\""
                <> testSessionId
                <> "\",\"message\":{\"role\":\"user\",\"content\":[{\
                   \\"type\":\"tool_result\",\"tool_use_id\":\"search-1\",\
                   \\"content\":[{\"type\":\"tool_reference\",\
                   \\"tool_name\":\"WebFetch\"},{\"type\":\"text\",\
                   \\"text\":\"loaded\"}]}]}}"
            , successResult testSessionId
            ]

        completed <- expectRight result
        completed.result `shouldBe` Just "ok"
        [ renderedText
            | MessageUser UserMessage
                { content =
                    [ ToolResultBlock
                        { toolUseId = "search-1"
                        , content = Just ToolResultContent{renderedText}
                        }
                    ]
                } <- messages
            ]
            `shouldBe`
                [ "Tool reference: WebFetch\nloaded" ]

    it "ignores autonomous results and keeps waiting for the human result" do
        (result, messages) <- runQueryLines
            [ assistantLine "human-before-background" "human text"
            , "{\"type\":\"user\",\"uuid\":\"background-user\",\
              \\"session_id\":\""
                <> testSessionId
                <> "\",\"origin\":{\"kind\":\"task-notification\"},\
                   \\"message\":{\"role\":\"user\",\"content\":\"background\"}}"
            , "{\"type\":\"assistant\",\"uuid\":\"background-assistant\",\
              \\"session_id\":\""
                <> testSessionId
                <> "\",\"supersedes\":[\"human-before-background\"],\
                   \\"message\":{\"content\":[{\"type\":\"text\",\
                   \\"text\":\"background answer\"}]}}"
            , "{\"type\":\"user\",\"uuid\":\"background-tool-result\",\
              \\"session_id\":\""
                <> testSessionId
                <> "\",\"message\":{\"role\":\"user\",\"content\":[{\
                   \\"type\":\"tool_result\",\"tool_use_id\":\"background-tool\",\
                   \\"content\":\"background tool output\"}]}}"
            , "{\"type\":\"result\",\"subtype\":\"success\",\
              \\"is_error\":false,\"session_id\":\""
                <> testSessionId
                <> "\",\"uuid\":\"background-result\",\
                   \\"origin\":{\"kind\":\"task-notification\"},\
                   \\"result\":\"background answer\"}"
            , "{\"type\":\"result\",\"subtype\":\"success\",\
              \\"is_error\":false,\"session_id\":\""
                <> testSessionId
                <> "\",\"uuid\":\"human-result\",\
                   \\"origin\":{\"kind\":\"human\"},\
                   \\"result\":\"human answer\"}"
            ]

        completed <- expectRight result
        completed.result `shouldBe` Just "human answer"
        messages `shouldSatisfy` any \case
            MessageAssistant AssistantMessage
                { uuid = Just "human-before-background"
                } ->
                True
            _ ->
                False
        show messages `shouldNotContain` "background answer"
        show messages `shouldNotContain` "background tool output"

    it "isolates multiple interleaved autonomous routes by origin identifiers" do
        let foreignUser identifier =
                "{\"type\":\"user\",\"uuid\":\"user-" <> identifier <> "\",\
                \\"session_id\":\"" <> testSessionId <> "\",\
                \\"origin\":{\"kind\":\"task-notification\",\
                \\"senderTaskId\":\"" <> identifier <> "\"},\
                \\"message\":{\"role\":\"user\",\"content\":\"start "
                    <> identifier <> "\"}}"
            foreignAssistant identifier =
                assistantLine ("assistant-" <> identifier)
                    ("private-" <> identifier)
            foreignResult identifier =
                "{\"type\":\"result\",\"subtype\":\"success\",\
                \\"is_error\":false,\"session_id\":\"" <> testSessionId <> "\",\
                \\"uuid\":\"result-" <> identifier <> "\",\
                \\"origin\":{\"kind\":\"task-notification\",\
                \\"senderTaskId\":\"" <> identifier <> "\"}}"
            explicitHuman =
                "{\"type\":\"user\",\"uuid\":\"human-user\",\
                \\"session_id\":\"" <> testSessionId <> "\",\
                \\"origin\":{\"kind\":\"human\",\"fromSession\":\"request-1\"},\
                \\"message\":{\"role\":\"user\",\"content\":\"human\"}}"
            humanResult =
                "{\"type\":\"result\",\"subtype\":\"success\",\
                \\"is_error\":false,\"session_id\":\"" <> testSessionId <> "\",\
                \\"uuid\":\"human-result\",\
                \\"origin\":{\"kind\":\"human\",\"fromSession\":\"request-1\"},\
                \\"result\":\"human answer\"}"
        (result, messages) <- runQueryLines
            [ foreignUser "task-a"
            , foreignAssistant "task-a"
            , foreignUser "task-b"
            , foreignAssistant "task-b"
            , foreignResult "task-a"
            , foreignResult "task-b"
            , explicitHuman
            , assistantLine "human-assistant" "public answer"
            , humanResult
            ]

        completed <- expectRight result
        completed.result `shouldBe` Just "human answer"
        show messages `shouldContain` "public answer"
        show messages `shouldNotContain` "private-task-a"
        show messages `shouldNotContain` "private-task-b"

    it "hides unoriginated output when its autonomous route is ambiguous" do
        let foreignUser identifier =
                "{\"type\":\"user\",\"uuid\":\"user-" <> identifier <> "\",\
                \\"origin\":{\"kind\":\"task-notification\",\
                \\"senderTaskId\":\"" <> identifier <> "\"},\
                \\"message\":{\"role\":\"user\",\"content\":\"start\"}}"
            foreignResult identifier =
                "{\"type\":\"result\",\"subtype\":\"success\",\
                \\"is_error\":false,\"session_id\":\"" <> testSessionId <> "\",\
                \\"origin\":{\"kind\":\"task-notification\",\
                \\"senderTaskId\":\"" <> identifier <> "\"}}"
        (result, messages) <- runQueryLines
            [ foreignUser "task-a"
            , foreignUser "task-b"
            , foreignResult "task-b"
            , assistantLine "ambiguous" "must remain hidden"
            , "{\"type\":\"result\",\"subtype\":\"success\",\
              \\"is_error\":false,\"session_id\":\""
                <> testSessionId
                <> "\",\"uuid\":\"human-result\",\
                   \\"origin\":{\"kind\":\"human\"},\"result\":\"done\"}"
            ]

        _ <- expectRight result
        show messages `shouldNotContain` "must remain hidden"

    it "classifies live own and nested records while hiding foreign turns" do
        let topTool =
                "{\"type\":\"assistant\",\"uuid\":\"top-tool\",\
                \\"session_id\":\"" <> testSessionId <> "\",\
                \\"message\":{\"content\":[{\"type\":\"tool_use\",\
                \\"id\":\"agent-tool\",\"name\":\"Agent\",\"input\":{}}]}}"
            nestedText =
                "{\"type\":\"assistant\",\"uuid\":\"nested-text\",\
                \\"parent_tool_use_id\":\"agent-tool\",\
                \\"session_id\":\"" <> testSessionId <> "\",\
                \\"message\":{\"content\":[{\"type\":\"text\",\
                \\"text\":\"child progress\"}]}}"
            nestedResult =
                "{\"type\":\"result\",\"subtype\":\"success\",\
                \\"is_error\":false,\"session_id\":\"" <> testSessionId <> "\",\
                \\"uuid\":\"nested-result\",\
                \\"parent_tool_use_id\":\"agent-tool\",\
                \\"result\":\"child complete\"}"
            backgroundUser =
                "{\"type\":\"user\",\"uuid\":\"background-user\",\
                \\"session_id\":\"" <> testSessionId <> "\",\
                \\"origin\":{\"kind\":\"task-notification\"},\
                \\"message\":{\"role\":\"user\",\"content\":\"background\"}}"
            backgroundAssistant =
                "{\"type\":\"assistant\",\"uuid\":\"background-assistant\",\
                \\"session_id\":\"" <> testSessionId <> "\",\
                \\"message\":{\"content\":[{\"type\":\"text\",\
                \\"text\":\"must stay hidden\"}]}}"
            backgroundResult =
                "{\"type\":\"result\",\"subtype\":\"success\",\
                \\"is_error\":false,\"session_id\":\"" <> testSessionId <> "\",\
                \\"uuid\":\"background-result\",\
                \\"origin\":{\"kind\":\"task-notification\"}}"
        (result, progress) <- runQueryProgress
            [ topTool
            , nestedText
            , nestedResult
            , backgroundUser
            , backgroundAssistant
            , backgroundResult
            , successResult testSessionId
            ]

        _ <- expectRight result
        progress `shouldSatisfy` any \case
            QueryMessageObserved QueryTopLevel message ->
                messageUuid message == Just "top-tool"
            _ -> False
        progress `shouldSatisfy` any \case
            QueryMessageObserved
                (QueryNested (Just "agent-tool"))
                message ->
                    messageUuid message == Just "nested-text"
            _ -> False
        progress `shouldSatisfy` any \case
            QueryMessageObserved
                (QueryNested (Just "agent-tool"))
                message ->
                    messageUuid message == Just "nested-result"
            _ -> False
        show progress `shouldNotContain` "must stay hidden"
        show progress `shouldNotContain` "background-result"

    it "reports scoped and global retractions before replacement records" do
        let replacement =
                "{\"type\":\"assistant\",\"uuid\":\"replacement\",\
                \\"supersedes\":[\"old\"],\"session_id\":\""
                    <> testSessionId
                    <> "\",\"message\":{\"content\":[{\"type\":\"text\",\
                    \\"text\":\"new\"}]}}"
            fallback =
                "{\"type\":\"system\",\"subtype\":\"model_refusal_fallback\",\
                \\"session_id\":\"" <> testSessionId <> "\",\
                \\"uuid\":\"fallback\",\
                \\"retracted_message_uuids\":[\"replacement\"]}"
        (result, progress) <- runQueryProgress
            [ assistantLine "old" "old"
            , replacement
            , fallback
            , successResult testSessionId
            ]

        _ <- expectRight result
        map progressTag progress `shouldBe`
            [ "message:old"
            , "retract:top:old"
            , "message:replacement"
            , "retract:global:replacement"
            , "message:fallback"
            ]

    it "deduplicates live progress by UUID without changing canonical output" do
        (result, progress) <- runQueryProgress
            [ assistantLine "same" "first"
            , assistantLine "same" "duplicate"
            , successResult testSessionId
            ]

        _ <- expectRight result
        filter (Text.isInfixOf "message:same" . progressTag) progress
            `shouldSatisfy` \case
                [_] -> True
                _ -> False

    it "rejects mismatched live tool and nested-result messages without progress" do
        let wrongSessionId =
                "123e4567-e89b-42d3-a456-426614174999"
            wrongTool =
                "{\"type\":\"assistant\",\"uuid\":\"wrong-tool\",\
                \\"session_id\":\"" <> wrongSessionId <> "\",\
                \\"message\":{\"content\":[{\"type\":\"tool_use\",\
                \\"id\":\"agent-tool\",\"name\":\"Agent\",\"input\":{}}]}}"
            wrongNestedResult =
                "{\"type\":\"result\",\"subtype\":\"success\",\
                \\"is_error\":false,\"uuid\":\"wrong-nested-result\",\
                \\"parent_tool_use_id\":\"agent-tool\",\
                \\"session_id\":\"" <> wrongSessionId <> "\",\
                \\"result\":\"must stay hidden\"}"
            parentTool =
                "{\"type\":\"assistant\",\"uuid\":\"parent-tool\",\
                \\"session_id\":\"" <> testSessionId <> "\",\
                \\"message\":{\"content\":[{\"type\":\"tool_use\",\
                \\"id\":\"agent-tool\",\"name\":\"Agent\",\"input\":{}}]}}"
        (wrongToolResult, wrongToolProgress) <- runQueryProgress
            [wrongTool, successResult testSessionId]
        wrongToolResult `shouldSatisfy` isSessionMismatch
        wrongToolProgress `shouldBe` []

        (wrongNestedResultValue, wrongNestedProgress) <- runQueryProgress
            [parentTool, wrongNestedResult, successResult testSessionId]
        wrongNestedResultValue `shouldSatisfy` isSessionMismatch
        show wrongNestedProgress `shouldNotContain` "wrong-nested-result"

    it "treats an unambiguous origin-less result as human for compatibility" do
        (result, messages) <- runQueryLines
            [successResult testSessionId]

        completed <- expectRight result
        completed.result `shouldBe` Just "ok"
        map messageUuid messages `shouldBe` [Just "result"]

    it "adopts conversation resets and discards pre-reset query state" do
        let newSessionId = "223e4567-e89b-42d3-a456-426614174000"
            reset =
                "{\"type\":\"conversation_reset\",\"uuid\":\"reset\",\
                \\"session_id\":\"" <> testSessionId <> "\",\
                \\"new_conversation_id\":\"" <> newSessionId <> "\"}"
            newAssistant =
                "{\"type\":\"assistant\",\"uuid\":\"after-reset\",\
                \\"session_id\":\"" <> newSessionId <> "\",\
                \\"message\":{\"content\":[{\"type\":\"text\",\
                \\"text\":\"new answer\"}]}}"
            newResult =
                "{\"type\":\"result\",\"subtype\":\"success\",\
                \\"is_error\":false,\"session_id\":\"" <> newSessionId <> "\",\
                \\"uuid\":\"new-result\",\"origin\":{\"kind\":\"human\"},\
                \\"result\":\"new answer\"}"
        (result, progress, messages) <- runQueryProgressAndMessages
            [ assistantLine "before-reset" "stale answer"
            , reset
            , newAssistant
            , newResult
            ]

        completed <- expectRight result
        completed.sessionId `shouldBe` newSessionId
        progress `shouldSatisfy` any \case
            QueryConversationReset
                ConversationResetMessage{newConversationId = Just adopted} ->
                    adopted == newSessionId
            _ -> False
        show messages `shouldContain` "new answer"
        show messages `shouldNotContain` "stale answer"

    it "skips non-JSON stdout diagnostics before parsing protocol records" do
        (result, messages) <- runQueryLines
            [ "[SandboxDebug] sandbox setup detail"
            , successResult testSessionId
            ]

        _ <- expectRight result
        map messageUuid messages `shouldBe` [Just "result"]

    it "keeps the startup timeout until submitted-turn progress arrives" do
        let delayedResultScript firstRecord =
                Text.unpack $ Text.unlines
                    [ "#!/bin/sh"
                    , "IFS= read -r _query"
                    , "printf '%s\\n' " <> shellQuote firstRecord
                    , "sleep 0.3"
                    , "printf '%s\\n' "
                        <> shellQuote (successResult testSessionId)
                    ]
            background =
                "{\"type\":\"user\",\"uuid\":\"background\",\
                \\"origin\":{\"kind\":\"task-notification\",\
                \\"senderTaskId\":\"task-1\"},\
                \\"message\":{\"role\":\"user\",\"content\":\"background\"}}"
            unknown =
                "{\"type\":\"future_event\",\"uuid\":\"unknown\",\
                \\"payload\":{\"diagnostic\":true}}"
        mapM_
            (\firstRecord ->
                withFakeClaude
                    (delayedResultScript firstRecord)
                    \directory executable -> do
                        result <-
                            query
                                ((testOptions executable directory)
                                    { streamStartupTimeoutMicros = 100_000
                                    , streamInactivityTimeoutMicros = 1_000_000
                                    , environment = Nothing
                                    })
                                "hello"
                                (\_ -> pure ())
                        result `shouldSatisfy` \case
                            Left (CLIConnectionError message) ->
                                "did not produce structured output"
                                    `Text.isInfixOf` message
                            _ -> False)
            [background, unknown]

    it "returns structured JSON decode errors without publishing buffered messages" do
        (result, messages) <- runQueryLines
            [assistantLine "buffered" "not visible", "{not-json"]

        case result of
            Left CLIJSONDecodeError{rawBody} ->
                rawBody `shouldBe` "{not-json"
            other ->
                expectationFailure
                    ("expected CLIJSONDecodeError, got " <> show other)
        messages `shouldBe` []

    it "ignores parent-scoped terminal and control records for completion" do
        (result, messages) <- runQueryLines
            [ assistantLine "outer-assistant" "outer answer"
            , "{\"type\":\"result\",\"subtype\":\"success\",\
              \\"is_error\":false,\"session_id\":\""
                <> testSessionId
                <> "\",\"uuid\":\"nested-result\",\
                   \\"parent_tool_use_id\":\"agent-tool\",\
                   \\"result\":\"nested answer\"}"
            , "{\"type\":\"control_request\",\"request_id\":\"nested-control\",\
              \\"parent_tool_use_id\":\"agent-tool\"}"
            , "{\"type\":\"conversation_reset\",\"uuid\":\"nested-reset\",\
              \\"session_id\":\""
                <> testSessionId
                <> "\",\"parent_tool_use_id\":\"agent-tool\"}"
            , "{\"type\":\"result\",\"subtype\":\"success\",\
              \\"is_error\":false,\"session_id\":\""
                <> testSessionId
                <> "\",\"uuid\":\"outer-result\",\
                   \\"result\":\"outer answer\"}"
            ]

        completed <- expectRight result
        completed.result `shouldBe` Just "outer answer"
        messageUuid (last messages) `shouldBe` Just "outer-result"

    it "keeps partial stream events out of the canonical response" do
        (result, messages) <- runQueryLines
            [ "{\"type\":\"stream_event\",\"uuid\":\"partial-1\",\
              \\"session_id\":\""
                <> testSessionId
                <> "\",\"event\":{\"type\":\"content_block_delta\",\
                   \\"delta\":{\"type\":\"text_delta\",\
                   \\"text\":\"stale partial\"}}}"
            , assistantLine "canonical-assistant" "canonical answer"
            , "{\"type\":\"result\",\"subtype\":\"success\",\
              \\"is_error\":false,\"session_id\":\""
                <> testSessionId
                <> "\",\"uuid\":\"result\",\
                   \\"result\":\"canonical answer\"}"
            ]

        _ <- expectRight result
        messages `shouldSatisfy` all \case
            MessageStreamEvent _ -> False
            _ -> True
        show messages `shouldNotContain` "stale partial"

    it "preserves result whitespace and normalizes its session UUID" do
        let uppercaseSessionId = Text.toUpper testSessionId
        (result, _) <- runQueryLines
            [ "{\"type\":\"result\",\"subtype\":\"success\",\
              \\"is_error\":false,\"session_id\":\"  "
                <> uppercaseSessionId
                <> "  \",\"uuid\":\"result\",\
                   \\"result\":\"\\n  formatted answer  \\n\"}"
            ]

        completed <- expectRight result
        completed.sessionId `shouldBe` testSessionId
        completed.result `shouldBe` Just "\n  formatted answer  \n"

    it "rejects a blank result session ID" do
        (result, messages) <- runQueryLines
            [ "{\"type\":\"result\",\"subtype\":\"success\",\
              \\"is_error\":false,\"session_id\":\"   \",\
                   \\"uuid\":\"result\",\"result\":\"answer\"}"
            ]

        result `shouldSatisfy` \case
            Left MessageParseError{parseError} ->
                "non-empty string `session_id`"
                    `Text.isInfixOf` parseError
            _ -> False
        messages `shouldBe` []

    it "rejects empty tool identifiers and names" do
        mapM_
            (\assistantRecord -> do
                (result, messages) <- runQueryLines [assistantRecord]
                result `shouldSatisfy` \case
                    Left MessageParseError{} -> True
                    _ -> False
                messages `shouldBe` [])
            [ "{\"type\":\"assistant\",\"uuid\":\"tool-empty-id\",\
              \\"session_id\":\""
                <> testSessionId
                <> "\",\"message\":{\"content\":[{\"type\":\"tool_use\",\
                   \\"id\":\"\",\"name\":\"Read\",\"input\":{}}]}}"
            , "{\"type\":\"assistant\",\"uuid\":\"tool-empty-name\",\
              \\"session_id\":\""
                <> testSessionId
                <> "\",\"message\":{\"content\":[{\"type\":\"tool_use\",\
                   \\"id\":\"tool-1\",\"name\":\"  \",\"input\":{}}]}}"
            , "{\"type\":\"user\",\"uuid\":\"result-empty-id\",\
              \\"session_id\":\""
                <> testSessionId
                <> "\",\"message\":{\"role\":\"user\",\"content\":[{\
                   \\"type\":\"tool_result\",\"tool_use_id\":\"\",\
                   \\"content\":\"output\"}]}}"
            ]

    it "rejects non-object JSON records" do
        (result, messages) <- runQueryLines ["[]"]

        case result of
            Left MessageParseError{parseError} ->
                parseError `shouldBe` "expected a JSON object"
            other ->
                expectationFailure
                    ("expected MessageParseError, got " <> show other)
        messages `shouldBe` []

    it "promotes unsuccessful result records to ResultError" do
        (result, messages) <- runQueryLines
            [ "{\"type\":\"result\",\"subtype\":\"error_during_execution\",\
              \\"is_error\":true,\"session_id\":\""
                <> testSessionId
                <> "\",\"api_error_status\":529,\
                    \\"errors\":[\"overloaded\",\"retry later\"],\
                    \\"result\":\"request failed\"}"
            ]

        result `shouldBe`
            Left ResultError
                { subtype = "error_during_execution"
                , apiErrorStatus = Just 529
                , errors = ["overloaded", "retry later"]
                , result = Just "request failed"
                }
        messages `shouldBe` []

    it "validates the terminal session before publishing any messages" do
        (result, messages) <- runQueryLines
            [ assistantLine "buffered" "must not escape"
            , successResult "123e4567-e89b-42d3-a456-426614174999"
            ]

        result `shouldSatisfy` \case
            Left (CLIProtocolError message) ->
                "while 123e4567-e89b-42d3-a456-426614174000 was active"
                    `Text.isInfixOf` message
            _ -> False
        messages `shouldBe` []

    it "rejects unsupported interactive control requests" do
        (result, messages) <- runQueryLines
            ["{\"type\":\"control_request\",\"request_id\":\"permission-1\"}"]

        result `shouldBe`
            Left
                (CLIProtocolError
                    "Claude Code requested interactive protocol input that this client does not support.")
        messages `shouldBe` []

    it "rejects visible assistant records without a wire UUID" do
        (result, messages) <- runQueryLines
            [ "{\"type\":\"assistant\",\"session_id\":\""
                <> testSessionId
                <> "\",\"message\":{\"content\":[{\"type\":\"text\",\
                   \\"text\":\"cannot be retracted safely\"}]}}"
            , successResult testSessionId
            ]

        result `shouldBe`
            Left
                (CLIProtocolError
                    "Claude Code emitted a visible message without a wire UUID.")
        messages `shouldBe` []

    it "rejects visible user records without a wire UUID" do
        (result, messages) <- runQueryLines
            [ "{\"type\":\"user\",\"session_id\":\""
                <> testSessionId
                <> "\",\"message\":{\"role\":\"user\",\"content\":[{\
                   \\"type\":\"tool_result\",\"tool_use_id\":\"tool-1\",\
                   \\"content\":\"cannot be retracted safely\"}]}}"
            , successResult testSessionId
            ]

        result `shouldBe`
            Left
                (CLIProtocolError
                    "Claude Code emitted a visible message without a wire UUID.")
        messages `shouldBe` []

    it "falls back to result usage when modelUsage is malformed or partially malformed" do
        mapM_
            (\modelUsage -> do
                (result, _) <-
                    runQueryLines
                        [successResultWithModelUsage modelUsage]
                completed <- expectRight result
                completed.modelUsage `shouldBe` Map.empty
                completed.usage `shouldBe` Usage
                    { inputTokens = 12
                    , outputTokens = 4
                    , cachedTokens = 3
                    })
            [ "\"not-an-object\""
            , "{\"valid\":{\"inputTokens\":20,\"outputTokens\":5},\
              \\"malformed\":{\"inputTokens\":\"unknown\",\
              \\"outputTokens\":2}}"
            ]

runQueryLines
    :: [Text]
    -> IO
        ( Either ClaudeSDKError ResultMessage
        , [Message]
        )
runQueryLines linesToEmit =
    withFakeClaude (oneShotScript linesToEmit) \directory executable -> do
        messagesRef <- newIORef []
        result <-
            query
                (testOptions executable directory)
                "hello"
                (\message ->
                    modifyIORef' messagesRef (<> [message]))
        messages <- readIORef messagesRef
        pure (result, messages)

runQueryProgress
    :: [Text]
    -> IO (Either ClaudeSDKError ResultMessage, [QueryProgress])
runQueryProgress linesToEmit =
    withFakeClaude (oneShotScript linesToEmit) \directory executable -> do
        progressRef <- newIORef []
        result <-
            queryWithProgress
                (testOptions executable directory)
                "hello"
                (\progress ->
                    modifyIORef' progressRef (<> [progress]))
                (\_ -> pure ())
        progress <- readIORef progressRef
        pure (result, progress)

runQueryProgressAndMessages
    :: [Text]
    -> IO
        ( Either ClaudeSDKError ResultMessage
        , [QueryProgress]
        , [Message]
        )
runQueryProgressAndMessages linesToEmit =
    withFakeClaude (oneShotScript linesToEmit) \directory executable -> do
        progressRef <- newIORef []
        messagesRef <- newIORef []
        result <-
            queryWithProgress
                (testOptions executable directory)
                "hello"
                (\progress ->
                    modifyIORef' progressRef (<> [progress]))
                (\message ->
                    modifyIORef' messagesRef (<> [message]))
        progress <- readIORef progressRef
        messages <- readIORef messagesRef
        pure (result, progress, messages)

progressTag :: QueryProgress -> Text
progressTag = \case
    QueryMessageObserved _ message ->
        "message:" <> maybe "anonymous" id (messageUuid message)
    QueryMessagesRetracted scope identifiers ->
        "retract:" <> scopeTag scope <> ":" <> Text.intercalate "," identifiers
    QueryConversationReset{} ->
        "conversation-reset"
  where
    scopeTag = \case
        Nothing -> "global"
        Just QueryTopLevel -> "top"
        Just QueryNested{} -> "nested"

isSessionMismatch :: Either ClaudeSDKError a -> Bool
isSessionMismatch = \case
    Left (CLIProtocolError message) ->
        "while 123e4567-e89b-42d3-a456-426614174000 was active"
            `Text.isInfixOf` message
    _ -> False

canonicalResponseLines :: [Text]
canonicalResponseLines =
    [ "{\"type\":\"system\",\"subtype\":\"init\",\
      \\"session_id\":\"123e4567-e89b-42d3-a456-426614174000\",\
      \\"uuid\":\"system-init\",\"apiKeySource\":\"none\"}"
    , assistantLine "assistant-old" "refused answer"
    , "{\"type\":\"user\",\"uuid\":\"tool-result-old\",\
      \\"session_id\":\"123e4567-e89b-42d3-a456-426614174000\",\
      \\"message\":{\"role\":\"user\",\"content\":[{\
      \\"type\":\"tool_result\",\"tool_use_id\":\"tool-old\",\
      \\"content\":\"denied\",\"is_error\":true}]}}"
    , "{\"type\":\"assistant\",\"uuid\":\"assistant-replacement\",\
      \\"session_id\":\"123e4567-e89b-42d3-a456-426614174000\",\
      \\"supersedes\":[\"assistant-old\"],\"message\":{\
      \\"id\":\"message-new\",\"model\":\"claude-test\",\
      \\"stop_reason\":\"end_turn\",\"content\":[\
      \{\"type\":\"thinking\",\"thinking\":\"considered\",\
      \\"signature\":\"signed\"},\
      \{\"type\":\"text\",\"text\":\"canonical answer\"}]}}"
    , "{\"type\":\"system\",\"subtype\":\"model_refusal_fallback\",\
      \\"session_id\":\"123e4567-e89b-42d3-a456-426614174000\",\
      \\"uuid\":\"fallback\",\
      \\"retracted_message_uuids\":[\"tool-result-old\"]}"
    , "{\"type\":\"assistant\",\"uuid\":\"assistant-replacement\",\
      \\"session_id\":\"123e4567-e89b-42d3-a456-426614174000\",\
      \\"message\":{\"content\":[{\"type\":\"text\",\
      \\"text\":\"duplicate must be ignored\"}]}}"
    , "{\"type\":\"result\",\"subtype\":\"success\",\"is_error\":false,\
      \\"session_id\":\"123e4567-e89b-42d3-a456-426614174000\",\
      \\"uuid\":\"result\",\"result\":\"canonical answer\",\
      \\"usage\":{\"input_tokens\":10,\"cache_creation_input_tokens\":3,\
      \\"cache_read_input_tokens\":4,\"output_tokens\":5},\
      \\"modelUsage\":{\"claude-test\":{\"inputTokens\":20,\
      \\"outputTokens\":8,\"cacheReadInputTokens\":6,\
      \\"cacheCreationInputTokens\":2,\"costUSD\":0.01}}}"
    ]

assistantLine :: Text -> Text -> Text
assistantLine uuid text =
    "{\"type\":\"assistant\",\"uuid\":\""
        <> uuid
        <> "\",\"session_id\":\""
        <> testSessionId
        <> "\",\"message\":{\"id\":\"message-"
        <> uuid
        <> "\",\"model\":\"claude-test\",\"content\":[\
           \{\"type\":\"text\",\"text\":\""
        <> text
        <> "\"}]}}"

assistantWithUnknownBlock :: Text
assistantWithUnknownBlock =
    "{\"type\":\"assistant\",\"uuid\":\"assistant-future\",\
    \\"session_id\":\"123e4567-e89b-42d3-a456-426614174000\",\
    \\"message\":{\"content\":[{\"type\":\"future_block\",\
    \\"value\":1}]}}"

-- | A ToolSearch-style user record: structured tool_result content plus the
-- top-level @tool_use_result@ summary Claude Code attaches to it.
structuredToolResultUser :: Text
structuredToolResultUser =
    "{\"type\":\"user\",\"uuid\":\"structured-tool-results\",\
    \\"session_id\":\""
        <> testSessionId
        <> "\",\"parent_tool_use_id\":null,\
           \\"message\":{\"role\":\"user\",\"content\":["
        <> Text.intercalate
            ","
            [ toolResult "tool-references" (Text.decodeUtf8 toolReferenceContent)
            , toolResult
                "tool-text-blocks"
                "[{\"type\":\"text\",\"text\":\"first\"},\
                \{\"type\":\"text\",\"text\":\"second\"}]"
            , toolResult "tool-image" (Text.decodeUtf8 imageContent)
            , toolResult "tool-object" "{\"custom\":1}"
            , toolResult
                "tool-unknown-block"
                "[{\"type\":\"mystery\",\"value\":2}]"
            , toolResult "tool-plain" "\"plain output\""
            ]
        <> "]},\"tool_use_result\":{\"matches\":[\"WebFetch\",\"WebSearch\"],\
           \\"query\":\"select:WebFetch,WebSearch\",\"total_deferred_tools\":15}}"
  where
    toolResult toolUseId content =
        "{\"type\":\"tool_result\",\"tool_use_id\":\""
            <> toolUseId
            <> "\",\"content\":"
            <> content
            <> "}"

toolReferenceContent :: ByteString.ByteString
toolReferenceContent =
    "[{\"type\":\"tool_reference\",\"tool_name\":\"WebSearch\"},\
    \{\"type\":\"tool_reference\",\"tool_name\":\"WebFetch\"}]"

imageContent :: ByteString.ByteString
imageContent =
    "[{\"type\":\"image\",\"source\":{\"type\":\"base64\",\
    \\"media_type\":\"image/png\",\"data\":\"iVBORw0KGgo=\"}}]"

successResult :: Text -> Text
successResult sessionId =
    "{\"type\":\"result\",\"subtype\":\"success\",\"is_error\":false,\
    \\"session_id\":\""
        <> sessionId
        <> "\",\"uuid\":\"result\",\"result\":\"ok\"}"

successResultWithModelUsage :: Text -> Text
successResultWithModelUsage modelUsage =
    "{\"type\":\"result\",\"subtype\":\"success\",\"is_error\":false,\
    \\"session_id\":\""
        <> testSessionId
        <> "\",\"uuid\":\"result\",\"result\":\"ok\",\
           \\"usage\":{\"input_tokens\":7,\
           \\"cache_creation_input_tokens\":2,\
           \\"cache_read_input_tokens\":3,\"output_tokens\":4},\
           \\"modelUsage\":"
        <> modelUsage
        <> "}"

hasCanonicalAssistant :: Message -> Bool
hasCanonicalAssistant = \case
    MessageAssistant AssistantMessage{content} ->
        content
            == [ ThinkingBlock
                    { thinking = "considered"
                    , signature = Just "signed"
                    }
               , TextBlock "canonical answer"
               ]
    _ -> False

hasRetractedContent :: Message -> Bool
hasRetractedContent = \case
    MessageAssistant AssistantMessage{content} ->
        any \case
            TextBlock "refused answer" -> True
            TextBlock "duplicate must be ignored" -> True
            _ -> False
            $ content
    MessageUser UserMessage{uuid} ->
        uuid == Just "tool-result-old"
    _ -> False

isUnknownBlock :: ContentBlock -> Bool
isUnknownBlock = \case
    UnknownContentBlock{} -> True
    _ -> False

expectRight
    :: Show error
    => Either error value
    -> IO value
expectRight = \case
    Left err -> do
        expectationFailure ("expected Right, got Left " <> show err)
        fail "unreachable"
    Right value ->
        pure value
