module Agent.OpenAI.CompactionSpec (spec) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.OpenAI.CompactClient
import Agent.OpenAI.Compaction
import Agent.OpenAI.ModelMetadata
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import Agent.Provider
import Agent.Responses.Types
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Either (isLeft)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Vector as Vector
import Test.Hspec

spec :: Spec
spec = do
    describe "remote compaction v2" do
        it "builds an exact final compaction trigger and preserves request controls" do
            let reasoningConfig = ReasoningConfig
                    { context = Nothing
                    , effort = Just "high"
                    , generateSummary = Nothing
                    , reasoningMode = Nothing
                    , summary = Nothing
                    , extraFields = KeyMap.empty
                    }
                params = defaultResponseCreateParams
                    { instructions = Just "instructions"
                    , tools = Just []
                    , reasoning = Just reasoningConfig
                    , previousResponseId = Just "old-response"
                    , parallelToolCalls = Just False
                    , store = Just True
                    , stream = Just False
                    }
                history = [user "hello"]
                request = buildRemoteCompactionRequest params history
            request.instructions `shouldBe` Just "instructions"
            request.tools `shouldBe` Just []
            request.reasoning `shouldBe` Just reasoningConfig
            request.previousResponseId `shouldBe` Nothing
            request.parallelToolCalls `shouldBe` Just True
            request.store `shouldBe` Just False
            request.stream `shouldBe` Just True
            request.toolChoice `shouldBe` Just (ToolChoiceMode ToolChoiceAuto)
            requestItems request `shouldBe` history <> [compactionTriggerItem]
            Aeson.toJSON compactionTriggerItem
                `shouldBe` Aeson.object ["type" .= ("compaction_trigger" :: Text.Text)]

        it "disables parallel tool calls for Responses Lite compaction" do
            let params = defaultResponseCreateParams
                    { model = Just "gpt-5.6-sol"
                    , store = Just True
                    , parallelToolCalls = Just True
                    }
                request = buildRemoteCompactionRequest params [user "hello"]
            request.parallelToolCalls `shouldBe` Just False

        it "includes request-level fields in request token estimates" do
            let params = (defaultResponseCreateParams :: ResponseCreateParams)
                    { instructions = Just (Text.replicate 1_000 "i")
                    , tools = Just []
                    }
                history = [user "hello"]
            estimateRequestTokensWithItems params history
                `shouldSatisfy` (> estimateItemsTokens history)

        it "trims against the complete remote request size" do
            let params = (defaultResponseCreateParams :: ResponseCreateParams)
                    { instructions = Just (Text.replicate 1_000 "i")
                    , tools = Just []
                    }
                history = [user (Text.replicate 4_000 "x")]
                trimmed =
                    trimRemoteCompactionRequestToFit 500 params history
                request = buildRemoteCompactionRequest params trimmed
            estimateResponseCreateParamsTokens request `shouldSatisfy` (<= 500)
            trimmed `shouldSatisfy` \items ->
                case items of
                    [MessageItem message] ->
                        Text.length (userOnly message) < 4_000
                    _ -> True

        it "accounts for large tool schemas when trimming remote requests" do
            let schemaText = Text.replicate 20_000 "s"
                tool = FunctionToolValue FunctionTool
                    { name = "large_schema"
                    , description = Just schemaText
                    , parameters = Just (Aeson.object
                        [ "type" .= ("object" :: Text.Text)
                        , "properties" .= Aeson.object
                            [ "value" .= Aeson.object
                                [ "type" .= ("string" :: Text.Text)
                                , "description" .= schemaText
                                ]
                            ]
                        ])
                    , strict = Nothing
                    , extraFields = KeyMap.empty
                    }
                params = (defaultResponseCreateParams :: ResponseCreateParams)
                    { tools = Just [tool]
                    }
                history = [user (Text.replicate 10_000 "x")]
                trimmed = trimRemoteCompactionRequestToFit 11_000 params history
                request = buildRemoteCompactionRequest params trimmed
            estimateRequestTokensWithItems params history
                `shouldSatisfy` (> estimateItemsTokens history)
            estimateResponseCreateParamsTokens request
                `shouldSatisfy` (<= 11_000)

        it "accepts exactly one compaction item alongside unrelated output" do
            let opaque = checkpoint "opaque"
                response = completedResponse [assistant "ignored", opaque]
            extractRemoteCompactionItem response `shouldBe` Right opaque

        it "rejects zero or multiple compaction output items" do
            extractRemoteCompactionItem (completedResponse [assistant "none"])
                `shouldSatisfy` isLeft
            extractRemoteCompactionItem
                (completedResponse [checkpoint "one", checkpoint "two"])
                `shouldSatisfy` isLeft

        it "installs retained user messages before the opaque checkpoint" do
            let opaque = checkpoint "opaque"
                call = FunctionCallItem FunctionCall
                    { itemId = Nothing
                    , callId = "call-1"
                    , name = "shell_command"
                    , namespace = Nothing
                    , arguments = "{}"
                    , encryptedFunctionArgs = Nothing
                    , status = Nothing
                    , extraFields = KeyMap.empty
                    }
                history =
                    [ user "old"
                    , assistant "discard me"
                    , call
                    , user "recent"
                    ]
            buildRemoteCompactedHistory 64_000 history opaque
                `shouldBe` [user "old", user "recent", opaque]

        it "sanitizes rich user content before retaining it" do
            let payload = Text.replicate 100_000 "x"
                rich = MessageItem ResponseMessage
                    { messageId = Nothing
                    , content = MessageContentParts
                        [ InputTextPart "keep this" Nothing KeyMap.empty
                        , InputImagePart
                            { detail = Just "auto"
                            , fileId = Nothing
                            , imageUrl = Just ("data:image/png;base64," <> payload)
                            , promptCacheBreakpoint = Nothing
                            , extraFields = KeyMap.empty
                            }
                        , InputFilePart
                            { detail = Just "auto"
                            , fileData = Just ("data:text/plain;base64," <> payload)
                            , fileId = Nothing
                            , fileUrl = Nothing
                            , filename = Just "notes.txt"
                            , promptCacheBreakpoint = Nothing
                            , extraFields = KeyMap.empty
                            }
                        , InputAudioPart
                            { inputAudio = Aeson.String payload
                            , extraFields = KeyMap.empty
                            }
                        , UnknownContentPart TaggedObject
                            { tag = "input_unknown"
                            , fields = KeyMap.empty
                            }
                        ]
                    , role = RoleUser
                    , status = Nothing
                    , phase = Nothing
                    , extraFields = KeyMap.empty
                    }
                compacted =
                    buildRemoteCompactedHistory 1_000 [rich] (checkpoint "opaque")
            estimateItemsTokens [rich] `shouldSatisfy` (> 50_000)
            case compacted of
                retained : _ -> do
                    let serialized = show retained
                    length serialized `shouldSatisfy` (< 5_000)
                    serialized `shouldNotContain` "data:image/png;base64"
                    serialized `shouldNotContain` "data:text/plain;base64"
                [] ->
                    expectationFailure "rich user message was dropped"

        it "omits inline image payloads from retained user messages" do
            let imageUrl =
                    "data:image/png;base64,"
                        <> Text.replicate 1_000_000 "x"
                multimodal = MessageItem ResponseMessage
                    { messageId = Nothing
                    , content = MessageContentParts
                        [ InputTextPart "inspect this" Nothing KeyMap.empty
                        , InputImagePart
                            { detail = Just "auto"
                            , fileId = Nothing
                            , imageUrl = Just imageUrl
                            , promptCacheBreakpoint = Nothing
                            , extraFields = KeyMap.empty
                            }
                        ]
                    , role = RoleUser
                    , status = Nothing
                    , phase = Nothing
                    , extraFields = KeyMap.empty
                    }
                compacted =
                    buildRemoteCompactedHistory
                        64_000
                        [multimodal]
                        (checkpoint "opaque")
            estimateItemsTokens compacted `shouldSatisfy` (< 1_000)
            compacted `shouldSatisfy` \case
                MessageItem message : _ ->
                    case message.content of
                        MessageContentParts parts ->
                            all (\case InputImagePart{} -> False; _ -> True) parts
                                && any (\case
                                    InputTextPart text _ _ ->
                                        Text.isInfixOf
                                            "image attachment omitted"
                                            text
                                    _ -> False)
                                    parts
                        _ -> False
                _ -> False

        it "uses output text notices for assistant rich content" do
            let rich = MessageItem ResponseMessage
                    { messageId = Nothing
                    , content = MessageContentParts
                        [ InputImagePart
                            { detail = Nothing
                            , fileId = Nothing
                            , imageUrl = Just "data:image/png;base64,huge"
                            , promptCacheBreakpoint = Nothing
                            , extraFields = KeyMap.empty
                            }
                        ]
                    , role = RoleAssistant
                    , status = Nothing
                    , phase = Nothing
                    , extraFields = KeyMap.empty
                    }
            sanitizeCompactionHistory [rich] `shouldSatisfy` \case
                [MessageItem message] ->
                    case message.content of
                        MessageContentParts [OutputTextPart{}] -> True
                        _ -> False
                _ -> False

        it "rewrites image-bearing messages when trimming a compaction request" do
            let imageUrl =
                    "data:image/png;base64,"
                        <> Text.replicate 1_000_000 "x"
                multimodal = MessageItem ResponseMessage
                    { messageId = Nothing
                    , content = MessageContentParts
                        [ InputTextPart "inspect this" Nothing KeyMap.empty
                        , InputImagePart
                            { detail = Just "auto"
                            , fileId = Nothing
                            , imageUrl = Just imageUrl
                            , promptCacheBreakpoint = Nothing
                            , extraFields = KeyMap.empty
                            }
                        ]
                    , role = RoleUser
                    , status = Nothing
                    , phase = Nothing
                    , extraFields = KeyMap.empty
                    }
                trimmed =
                    trimRemoteCompactionHistoryToFit
                        1_000
                        Nothing
                        [multimodal]
            estimateItemsTokens trimmed `shouldSatisfy` (< 1_000)
            trimmed `shouldSatisfy` \case
                [MessageItem message] ->
                    case message.content of
                        MessageContentParts parts ->
                            all (\case InputImagePart{} -> False; _ -> True) parts
                        _ -> False
                _ -> False

        it "drops an unrewritable oldest item to guarantee the request fits" do
            let params = (defaultResponseCreateParams :: ResponseCreateParams)
                    { tools = Just []
                    }
                opaque = UnknownResponseItem TaggedObject
                    { tag = "vendor_blob"
                    , fields = KeyMap.fromList
                        [ ("type", Aeson.String "vendor_blob")
                        , ("blob", Aeson.String (Text.replicate 20_000 "x"))
                        ]
                    }
                recent = user "recent"
                trimmed =
                    trimRemoteCompactionRequestToFit
                        200
                        params
                        [opaque, recent]
            estimateResponseCreateParamsTokens
                (buildRemoteCompactionRequest params trimmed)
                `shouldSatisfy` (<= 200)
            trimmed `shouldSatisfy` elem recent

        it "does not silently discard an irreducible prior checkpoint" do
            let params = (defaultResponseCreateParams :: ResponseCreateParams)
                    { tools = Just []
                    }
                prior = KnownResponseItem ItemCompaction TaggedObject
                    { tag = "compaction"
                    , fields = KeyMap.fromList
                        [ ("encrypted_content",
                            Aeson.String (Text.replicate 20_000 "x"))
                        ]
                    }
                trimmed =
                    trimRemoteCompactionRequestToFit
                        200
                        params
                        [prior]
            trimmed `shouldBe` [prior]
            estimateResponseCreateParamsTokens
                (buildRemoteCompactionRequest params trimmed)
                `shouldSatisfy` (> 200)

        it "drops harness-generated user-role context wrappers" do
            let opaque = checkpoint "opaque"
                history =
                    [ user "# AGENTS.md instructions for /tmp/project\n\n<INSTRUCTIONS>generated</INSTRUCTIONS>"
                    , user "# Skill instructions: example\n\n<SKILL_INSTRUCTIONS>generated</SKILL_INSTRUCTIONS>"
                    , user "## Skills\nThe following reusable skills are available in this session.\n"
                    , user "<learned-skills>\nThese are durable, reusable instructions learned from earlier sessions.\n"
                    , user "<system-reminder>\nAs you answer the user's questions, you can use the following context\n"
                    , user "<subagent_notification>\nstatus: completed\n</subagent_notification>"
                    , user "real user request"
                    ]
            buildRemoteCompactedHistory 64_000 history opaque
                `shouldBe` [user "real user request", opaque]

        it "retains task agent messages but drops descendant progress and completions" do
            let opaque = checkpoint "opaque"
                task = agentMessage "root" "root/child"
                    "Message Type: NEW_TASK\nPayload:\ndo it"
                progress = agentMessage "root/child/grandchild" "root/child"
                    "Message Type: MESSAGE\nPayload:\nworking"
                completion = agentMessage "root/child" "root"
                    "Message Type: FINAL_ANSWER\nPayload:\ndone"
            buildRemoteCompactedHistory 64_000
                [task, progress, completion]
                opaque
                `shouldBe` [task, opaque]

        it "uses the retained budget newest-first and truncates the boundary user" do
            let opaque = checkpoint "opaque"
                old = Text.replicate 40 "o"
                recent = "recent"
                compacted =
                    buildRemoteCompactedHistory 45
                        [user old, user recent]
                        opaque
                retainedTexts =
                    [ userOnly message
                    | MessageItem message <- compacted
                    , message.role == RoleUser
                    ]
            retainedTexts `shouldSatisfy` \texts ->
                case texts of
                    [truncated, newest] ->
                        Text.length truncated < Text.length old
                            && newest == recent
                    _ -> False
            last compacted `shouldBe` opaque

        it "rewrites a trailing oversized tool output to fit the request window" do
            let oversized = FunctionCallOutputItem FunctionCallOutput
                    { itemId = Nothing
                    , callId = "call-1"
                    , name = Nothing
                    , namespace = Nothing
                    , output = Aeson.String (Text.replicate 10_000 "x")
                    , status = Just ItemCompleted
                    , extraFields = KeyMap.empty
                    }
                trimmed =
                    trimRemoteCompactionHistoryToFit
                        100
                        Nothing
                        [user "keep", oversized]
            case reverse trimmed of
                FunctionCallOutputItem output : _ ->
                    output.output `shouldBe` Aeson.String
                        "Output exceeded the available model context and was truncated"
                other ->
                    expectationFailure
                        ("expected rewritten tool output, got " <> show other)

        it "omits function arguments above the provider string limit" do
            let oversized = FunctionCallItem FunctionCall
                    { itemId = Nothing
                    , callId = "call-oversized"
                    , name = "apply_patch"
                    , namespace = Nothing
                    , arguments =
                        Text.replicate
                            (remoteCompactionMaxStringLength + 1)
                            "x"
                    , encryptedFunctionArgs = Nothing
                    , status = Just ItemCompleted
                    , extraFields = KeyMap.empty
                    }
                trimmed =
                    trimRemoteCompactionHistoryToFit
                        2_000_000
                        Nothing
                        [user "keep", oversized]
            case trimmed of
                [_, FunctionCallItem call] -> do
                    call.callId `shouldBe` "call-oversized"
                    call.name `shouldBe` "apply_patch"
                    Text.length call.arguments
                        `shouldSatisfy` (<= remoteCompactionMaxStringLength)
                    Aeson.eitherDecodeStrict'
                        (TextEncoding.encodeUtf8 call.arguments)
                        `shouldSatisfy`
                            (either (const False) (const True)
                                :: Either String Aeson.Value -> Bool)
                other ->
                    expectationFailure
                        ("expected sanitized function call, got " <> show other)

        it "omits custom-tool input above the provider string limit" do
            let oversized = CustomToolCallItem CustomToolCall
                    { itemId = Nothing
                    , callId = "call-custom"
                    , name = "apply_patch"
                    , namespace = Nothing
                    , input =
                        Text.replicate
                            (remoteCompactionMaxStringLength + 1)
                            "x"
                    , status = Just ItemCompleted
                    , extraFields = KeyMap.empty
                    }
                trimmed =
                    trimRemoteCompactionHistoryToFit
                        2_000_000
                        Nothing
                        [oversized]
            case trimmed of
                [CustomToolCallItem call] -> do
                    call.callId `shouldBe` "call-custom"
                    call.name `shouldBe` "apply_patch"
                    Text.length call.input
                        `shouldSatisfy` (<= remoteCompactionMaxStringLength)
                other ->
                    expectationFailure
                        ("expected sanitized custom tool call, got " <> show other)

        it "truncates oversized messages without rewriting a tiny trailing output" do
            let huge = user (Text.replicate 20_000 "x")
                tiny = FunctionCallOutputItem FunctionCallOutput
                    { itemId = Nothing
                    , callId = "call-1"
                    , name = Nothing
                    , namespace = Nothing
                    , output = Aeson.String "ok"
                    , status = Just ItemCompleted
                    , extraFields = KeyMap.empty
                    }
                trimmed =
                    trimRemoteCompactionHistoryToFit
                        100
                        Nothing
                        [huge, tiny]
            trimmed `shouldSatisfy` any \case
                FunctionCallOutputItem output ->
                    output.output == Aeson.String "ok"
                _ -> False
            trimmed `shouldSatisfy` any \case
                MessageItem message ->
                    case message.content of
                        MessageContentParts [InputTextPart text _ _] ->
                            Text.length text < 20_000
                        MessageContentText text ->
                            Text.length text < 20_000
                        _ -> False
                _ -> False

        it "revisits old messages after rewriting later oversized outputs" do
            let huge = user (Text.replicate 20_000 "x")
                output = FunctionCallOutputItem FunctionCallOutput
                    { itemId = Nothing
                    , callId = "call-1"
                    , name = Nothing
                    , namespace = Nothing
                    , output = Aeson.String (Text.replicate 20_000 "y")
                    , status = Just ItemCompleted
                    , extraFields = KeyMap.empty
                    }
                trimmed =
                    trimRemoteCompactionHistoryToFit
                        200
                        Nothing
                        [huge, output]
            estimateItemsTokens (trimmed <> [compactionTriggerItem])
                `shouldSatisfy` (<= 200)
            trimmed `shouldSatisfy` any \case
                MessageItem{} -> True
                _ -> False
            trimmed `shouldSatisfy` any \case
                FunctionCallOutputItem result ->
                    result.output
                        == Aeson.String
                            "Output exceeded the available model context and was truncated"
                _ -> False

        it "drops an irreducible old item so a recent small output can fit" do
            let old = KnownResponseItem ItemReasoning TaggedObject
                    { tag = "reasoning"
                    , fields = KeyMap.fromList
                        [ ("type", Aeson.String "reasoning")
                        , ("summary", Aeson.String (Text.replicate 20_000 "x"))
                        ]
                    }
                recent = FunctionCallOutputItem FunctionCallOutput
                    { itemId = Nothing
                    , callId = "call-1"
                    , output = Aeson.String "ok"
                    , status = Just ItemCompleted
                    , extraFields = KeyMap.empty
                    }
                trimmed =
                    trimRemoteCompactionHistoryToFit
                        100
                        Nothing
                        [old, recent]
            estimateItemsTokens (trimmed <> [compactionTriggerItem])
                `shouldSatisfy` (<= 100)
            trimmed `shouldBe` [recent]

        it "does not delete tagged outputs when pairing ids are missing" do
            let callWith identifier = KnownResponseItem ItemShellCall TaggedObject
                    { tag = "shell_call"
                    , fields = KeyMap.fromList $
                        [ ("command", Aeson.String (Text.replicate 20_000 "x"))
                        ]
                            <> maybe []
                                (\value -> [("id", Aeson.String value)])
                                identifier
                    }
                outputWith identifier =
                    KnownResponseItem ItemShellCallOutput TaggedObject
                        { tag = "shell_call_output"
                        , fields = KeyMap.fromList $
                            [ ("output", Aeson.String "ok")
                            ]
                                <> maybe []
                                    (\value ->
                                        [("call_id", Aeson.String value)])
                                    identifier
                        }
                identifiedOutput = outputWith (Just "other-call")
                unidentifiedOutput = outputWith Nothing
                recent = user "recent"
                trim =
                    trimRemoteCompactionHistoryToFit 200 Nothing
            trim [callWith Nothing, identifiedOutput, recent]
                `shouldBe` [identifiedOutput, recent]
            trim [callWith (Just "call-1"), unidentifiedOutput, recent]
                `shouldBe` [unidentifiedOutput, recent]

        it "does not pair typed calls with empty identifiers" do
            let call = FunctionCallItem FunctionCall
                    { itemId = Nothing
                    , callId = ""
                    , name = "shell_command"
                    , arguments = Text.replicate 20_000 "x"
                    , status = Nothing
                    , extraFields = KeyMap.empty
                    }
                output = FunctionCallOutputItem FunctionCallOutput
                    { itemId = Nothing
                    , callId = ""
                    , output = Aeson.String "ok"
                    , status = Just ItemCompleted
                    , extraFields = KeyMap.empty
                    }
                recent = user "recent"
                trimmed =
                    trimRemoteCompactionHistoryToFit
                        200
                        Nothing
                        [call, output, recent]
            trimmed `shouldBe` [output, recent]

        it "drops paired tagged outputs with their oversized calls" do
            let call = KnownResponseItem ItemShellCall TaggedObject
                    { tag = "shell_call"
                    , fields = KeyMap.fromList
                        [ ("id", Aeson.String "call-1")
                        , ("command", Aeson.String (Text.replicate 20_000 "x"))
                        ]
                    }
                output = KnownResponseItem ItemShellCallOutput TaggedObject
                    { tag = "shell_call_output"
                    , fields = KeyMap.fromList
                        [ ("call_id", Aeson.String "call-1")
                        , ("output", Aeson.String "ok")
                        ]
                    }
                recent = user "recent"
                trimmed =
                    trimRemoteCompactionHistoryToFit
                        100
                        Nothing
                        [call, output, recent]
            estimateItemsTokens (trimmed <> [compactionTriggerItem])
                `shouldSatisfy` (<= 100)
            trimmed `shouldBe` [recent]

        it "rewrites large tagged output items when they expose a payload field" do
            let output = KnownResponseItem ItemShellCallOutput TaggedObject
                    { tag = "shell_call_output"
                    , fields = KeyMap.fromList
                        [ ("type", Aeson.String "shell_call_output")
                        , ("id", Aeson.String "shell-output-1")
                        , ("output", Aeson.String (Text.replicate 20_000 "x"))
                        ]
                    }
                trimmed =
                    trimRemoteCompactionHistoryToFit
                        100
                        Nothing
                        [user "keep", output]
            trimmed `shouldSatisfy` any \case
                KnownResponseItem ItemShellCallOutput tagged ->
                    KeyMap.lookup "output" tagged.fields
                        == Just (Aeson.String
                            "Output exceeded the available model context and was truncated")
                _ -> False

        it "rewrites plural payload fields on provider-managed outputs" do
            let output = KnownResponseItem ItemCodeInterpreterCall TaggedObject
                    { tag = "code_interpreter_call"
                    , fields = KeyMap.fromList
                        [ ("id", Aeson.String "code-1")
                        , ("outputs", Aeson.Array
                            (Vector.singleton
                                (Aeson.String (Text.replicate 20_000 "x"))))
                        ]
                    }
                trimmed =
                    trimRemoteCompactionHistoryToFit
                        100
                        Nothing
                        [user "keep", output]
            trimmed `shouldSatisfy` any \case
                KnownResponseItem ItemCodeInterpreterCall tagged ->
                    KeyMap.lookup "outputs" tagged.fields
                        == Just (Aeson.Array Vector.empty)
                _ -> False

        it "rewrites provider-specific output items without dropping their identity" do
            let output = KnownResponseItem
                    (ItemUnknownType "vendor_widget_output")
                    TaggedObject
                        { tag = "vendor_widget_output"
                        , fields = KeyMap.fromList
                            [ ("type", Aeson.String "vendor_widget_output")
                            , ("id", Aeson.String "vendor-output-1")
                            , ("result", Aeson.String (Text.replicate 20_000 "x"))
                            ]
                        }
                trimmed =
                    trimRemoteCompactionHistoryToFit
                        100
                        Nothing
                        [user "keep", output]
            trimmed `shouldSatisfy` any \case
                KnownResponseItem (ItemUnknownType "vendor_widget_output") tagged ->
                    KeyMap.lookup "type" tagged.fields
                        == Just (Aeson.String "vendor_widget_output")
                        && KeyMap.lookup "result" tagged.fields
                            == Just (Aeson.String
                                "Output exceeded the available model context and was truncated")
                _ -> False

        it "rewrites inline results on call-shaped provider items" do
            let output = KnownResponseItem ItemMcpCall TaggedObject
                    { tag = "mcp_call"
                    , fields = KeyMap.fromList
                        [ ("type", Aeson.String "mcp_call")
                        , ("id", Aeson.String "mcp-1")
                        , ("status", Aeson.String "completed")
                        , ("result", Aeson.String (Text.replicate 20_000 "x"))
                        ]
                    }
                trimmed =
                    trimRemoteCompactionHistoryToFit
                        100
                        Nothing
                        [user "keep", output]
            trimmed `shouldSatisfy` any \case
                KnownResponseItem ItemMcpCall tagged ->
                    KeyMap.lookup "type" tagged.fields
                        == Just (Aeson.String "mcp_call")
                        && KeyMap.lookup "status" tagged.fields
                            == Just (Aeson.String "completed")
                        && KeyMap.lookup "result" tagged.fields
                            == Just (Aeson.String
                                "Output exceeded the available model context and was truncated")
                _ -> False

        it "drops call-shaped items rather than corrupting input-like payloads" do
            let recent = user "recent"
                knownCall = KnownResponseItem ItemMcpCall TaggedObject
                    { tag = "mcp_call"
                    , fields = KeyMap.fromList
                        [ ("type", Aeson.String "mcp_call")
                        , ("id", Aeson.String "mcp-1")
                        , ("content", Aeson.String (Text.replicate 20_000 "x"))
                        ]
                    }
                unknownCall = UnknownResponseItem TaggedObject
                    { tag = "vendor_call"
                    , fields = KeyMap.fromList
                        [ ("type", Aeson.String "vendor_call")
                        , ("id", Aeson.String "vendor-1")
                        , ("payload", Aeson.String (Text.replicate 20_000 "x"))
                        ]
                    }
                trim call =
                    trimRemoteCompactionHistoryToFit
                        100
                        Nothing
                        [call, recent]
            map trim [knownCall, unknownCall]
                `shouldBe` replicate 2 [recent]

        it "rewrites completely unknown output tags when their payload is recognizable" do
            let output = UnknownResponseItem TaggedObject
                    { tag = "acme_output"
                    , fields = KeyMap.fromList
                        [ ("type", Aeson.String "acme_output")
                        , ("output", Aeson.String (Text.replicate 20_000 "x"))
                        ]
                    }
                trimmed =
                    trimRemoteCompactionHistoryToFit
                        100
                        Nothing
                        [user "keep", output]
            trimmed `shouldSatisfy` any \case
                UnknownResponseItem tagged ->
                    KeyMap.lookup "type" tagged.fields
                        == Just (Aeson.String "acme_output")
                        && KeyMap.lookup "output" tagged.fields
                            == Just (Aeson.String
                                "Output exceeded the available model context and was truncated")
                _ -> False

    describe "Codex model metadata" do
        it "derives the 90% auto-compaction limit for curated 272k models" do
            codexModelMetadata "gpt-5.6-luna"
                `shouldBe` Just CodexModelMetadata
                    { modelContextWindow = 272_000
                    , modelEffectiveContextWindow = 258_400
                    , modelAutoCompactTokenLimit = 244_800
                    }
            codexAutoCompactTokenLimitFor (Just "gpt-5.6-sol")
                `shouldBe` 244_800

    describe "buildLocalCompactedHistory" do
        it "asks summaries to preserve durable project constraints" do
            let prompt = summarizationPrompt Nothing
            prompt `shouldSatisfy` Text.isInfixOf "active project instructions"
            prompt `shouldSatisfy` Text.isInfixOf "always-active"
            prompt `shouldSatisfy` Text.isInfixOf "safety and policy constraints"
            prompt `shouldSatisfy` Text.isInfixOf "required workflows"

        it "keeps recent user texts and an assistant summary" do
            let history =
                    [ user "one"
                    , assistant "a1"
                    , user "two"
                    , assistant "a2"
                    , user "three"
                    ]
                compacted = buildLocalCompactedHistory 2 history "did stuff"
            length compacted `shouldBe` 3
            compacted `shouldSatisfy` any isSummary
            compacted `shouldSatisfy` any isWireValidAssistantSummary
            -- newest users retained
            let texts = [userOnly m | MessageItem m <- compacted, m.role == RoleUser]
            texts `shouldBe` ["two", "three"]

        it "skips prior /compact user markers" do
            let history = [user "/compact", user "keep me", assistant "x"]
                compacted = buildLocalCompactedHistory 3 history "summary"
            let texts = [userOnly m | MessageItem m <- compacted, m.role == RoleUser]
            texts `shouldBe` ["keep me"]

        it "does not skip ordinary messages beginning with /compaction" do
            let history =
                    [ user "/compaction algorithm"
                    , user "keep me"
                    ]
                compacted = buildLocalCompactedHistory 3 history "summary"
                texts = [userOnly m | MessageItem m <- compacted, m.role == RoleUser]
            texts `shouldBe` ["/compaction algorithm", "keep me"]

        it "omits generated project and skill wrappers from local snapshots" do
            let history =
                    [ user "# AGENTS.md instructions for /tmp/project\nrules"
                    , user "# Skill instructions: test\nrules"
                    , user "## Skills\nThe following reusable skills are available in this session.\nrules"
                    , user "<learned-skills>\nThese are durable, reusable instructions learned from earlier sessions.\nrules"
                    , user "<system-reminder>\nAs you answer the user's questions, you can use the following context\nrules"
                    , user "old request"
                    , user "new request"
                    ]
                compacted = buildLocalCompactedHistory 1 history "summary"
                texts = [userOnly m | MessageItem m <- compacted, m.role == RoleUser]
            texts `shouldBe` ["new request"]

        it "bounds installed local snapshots with oversized recent users" do
            let params = (defaultResponseCreateParams :: ResponseCreateParams)
                    { tools = Just []
                    }
                contextWindow = defaultCodexEffectiveContextWindow
                history =
                    [ user (Text.replicate 100_000
                        ("message-" <> Text.pack (show index)))
                    | index <- [1 :: Int .. 6]
                    ]
                compacted =
                    buildLocalCompactedHistoryToFit
                        contextWindow
                        params
                        6
                        history
                        "bounded summary"
                targetWindow =
                    min contextWindow
                        ( estimateRequestTokensWithItems params []
                            + remoteCompactionRetainedTokenBudget
                        )
            estimateRequestTokensWithItems params compacted
                `shouldSatisfy` (<= targetWindow)
            case reverse compacted of
                summaryItem : _ ->
                    summaryItem `shouldSatisfy` isSummary
                [] ->
                    expectationFailure "expected a protected local summary"

        it "bounds normal Responses history with a fixed summary prompt" do
            let params = (defaultResponseCreateParams :: ResponseCreateParams)
                    { tools = Nothing
                    }
                prompt = user "summarize"
                trimmed =
                    trimResponseHistoryToFit
                        300
                        params
                        [prompt]
                        [user (Text.replicate 20_000 "x")]
            estimateRequestTokensWithItems params (trimmed <> [prompt])
                `shouldSatisfy` (<= 300)

        it "uses serialized item sizes for retained-message budgeting" do
            let history = replicate 100 (user "x")
                compacted =
                    buildRemoteCompactedHistory 200 history (checkpoint "opaque")
                retained =
                    [ item
                    | item <- compacted
                    , not (isCheckpoint item)
                    ]
            estimateItemsTokens retained `shouldSatisfy` (<= 200)
            length retained `shouldSatisfy` (< 100)

    describe "compactTranscriptAtLastCheckpoint" do
        it "retains its legacy raw-checkpoint behavior for API compatibility" do
            let first = checkpoint "first"
                latest = checkpoint "latest"
                history =
                    [ user "old"
                    , first
                    , assistant "middle"
                    , latest
                    , user "recent"
                    ]
            compactTranscriptAtLastCheckpoint history
                `shouldBe` [latest, user "recent"]

    describe "hasCompactionCheckpoint" do
        it "recognizes only typed remote compaction checkpoints" do
            hasCompactionCheckpoint [checkpoint "remote"] `shouldBe` True
            hasCompactionCheckpoint
                (buildLocalCompactedHistory 1 [user "old"] "local summary")
                `shouldBe` False
            hasCompactionCheckpoint
                [assistantSummaryItem "ordinary model-controlled text"]
                `shouldBe` False
            hasCompactionCheckpoint [assistant "ordinary response"]
                `shouldBe` False

    describe "isCompactSessionTurn" do
        it "recognizes compact markers" do
            isCompactSessionTurn "/compact" `shouldBe` True
            isCompactSessionTurn "/compact focus auth" `shouldBe` True
            isCompactSessionTurn "hello" `shouldBe` False

    describe "clear/new session markers" do
        it "recognizes /clear and /new as transcript resets" do
            isClearSessionTurn "/clear" `shouldBe` True
            isNewSessionTurn "/new" `shouldBe` True
            isTranscriptResetTurn "/clear" `shouldBe` True
            isTranscriptResetTurn "/new" `shouldBe` True
            isTranscriptResetTurn "/compact" `shouldBe` True
            isTranscriptResetTurn "hello" `shouldBe` False

    describe "compactConversationAt" do
        it "rejects non-OpenAI credentials before making a request" do
            let provider = tokenProvider SubscriptionBilled \_ ->
                    pure $ Right Credential
                    { accessToken = "xai-secret"
                    , accountId = "account"
                    , leaseId = Nothing
                    , provider = XAIProvider
                    }
                request = CompactRequest
                    { compactModel = "gpt-test"
                    , compactInput = []
                    , compactInstructions = Nothing
                    , compactTools = Nothing
                    , compactParallelToolCalls = False
                    , compactReasoning = Nothing
                    }
            compactConversationAt "http://127.0.0.1:1" provider request
                `shouldReturn` Left (ProviderError ApiErrorType
                    "Codex compaction requires an OpenAI credential" Nothing)

  where
    user text = MessageItem ResponseMessage
        { messageId = Nothing
        , content = MessageContentParts [InputTextPart text Nothing KeyMap.empty]
        , role = RoleUser
        , status = Nothing
        , phase = Nothing
        , passthrough = Nothing
        , extraFields = KeyMap.empty
        }
    assistant text = MessageItem ResponseMessage
        { messageId = Nothing
        , content = MessageContentParts [InputTextPart text Nothing KeyMap.empty]
        , role = RoleAssistant
        , status = Nothing
        , phase = Nothing
        , passthrough = Nothing
        , extraFields = KeyMap.empty
        }
    isSummary (MessageItem m) =
        m.role == RoleAssistant
            && case m.content of
                MessageContentParts (OutputTextPart text _ _ _ : _) ->
                    Text.isPrefixOf summaryPrefix text
                _ -> False
    isSummary _ = False
    isCheckpoint (KnownResponseItem ItemCompaction _) = True
    isCheckpoint _ = False
    isWireValidAssistantSummary (MessageItem m) =
        m.role == RoleAssistant
            && case m.content of
                MessageContentParts [OutputTextPart {}] -> True
                _ -> False
    isWireValidAssistantSummary _ = False
    userOnly m = case m.content of
        MessageContentParts (InputTextPart text _ _ : _) -> text
        MessageContentText text -> text
        _ -> ""
    requestItems request = case request.input of
        Just (ResponseInputItems items) -> items
        _ -> []
    completedResponse output =
        case Aeson.fromJSON $ Aeson.object
            [ "id" .= ("resp-compact" :: Text.Text)
            , "created_at" .= (0 :: Int)
            , "status" .= ("completed" :: Text.Text)
            , "model" .= ("gpt-test" :: Text.Text)
            , "output" .= output
            ] of
            Aeson.Success response -> response
            Aeson.Error err -> error err
    agentMessage :: Text.Text -> Text.Text -> Text.Text -> ResponseItem
    agentMessage author recipient text =
        AgentMessageItem ResponseAgentMessage
            { messageId = Nothing
            , author = Just author
            , recipient = Just recipient
            , content = [InputTextPart text Nothing KeyMap.empty]
            , passthrough = Nothing
            , extraFields = KeyMap.empty
            }
    checkpoint name = CompactionItemValue CompactionItem
        { itemId = Nothing
        , encryptedContent = Nothing
        , extraFields = KeyMap.fromList [("name", Aeson.String name)]
        }
