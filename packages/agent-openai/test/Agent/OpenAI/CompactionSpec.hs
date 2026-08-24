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
                    , arguments = "{}"
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

        it "drops harness-generated user-role context wrappers" do
            let opaque = checkpoint "opaque"
                history =
                    [ user "# AGENTS.md instructions for /tmp/project\n\n<INSTRUCTIONS>generated</INSTRUCTIONS>"
                    , user "# Skill instructions: example\n\n<SKILL_INSTRUCTIONS>generated</SKILL_INSTRUCTIONS>"
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
                    buildRemoteCompactedHistory 3
                        [user old, user recent]
                        opaque
                retainedTexts =
                    [ userOnly message
                    | MessageItem message <- compacted
                    , message.role == RoleUser
                    ]
            retainedTexts `shouldBe` [Text.take 8 old, recent]
            last compacted `shouldBe` opaque

        it "omits inline image payloads from retained user messages" do
            let opaque = checkpoint "opaque"
                imageUrl =
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
                    buildRemoteCompactedHistory 64_000 [multimodal] opaque
            estimateItemsTokens compacted `shouldSatisfy` (< 1_000)
            case compacted of
                MessageItem message : _ ->
                    case message.content of
                        MessageContentParts parts -> do
                            parts `shouldSatisfy`
                                all (\case InputImagePart {} -> False; _ -> True)
                            parts `shouldSatisfy`
                                any (\case
                                    InputTextPart text _ _ ->
                                        Text.isInfixOf
                                            "image attachment omitted"
                                            text
                                    _ -> False)
                        other ->
                            expectationFailure
                                ("expected retained content parts, got " <> show other)
                other ->
                    expectationFailure
                        ("expected retained user message, got " <> show other)

        it "rewrites a trailing oversized tool output to fit the request window" do
            let oversized = FunctionCallOutputItem FunctionCallOutput
                    { itemId = Nothing
                    , callId = "call-1"
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
                            all (\case InputImagePart {} -> False; _ -> True) parts
                        _ -> False
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
        it "recognizes remote and local compaction snapshots" do
            hasCompactionCheckpoint [checkpoint "remote"] `shouldBe` True
            hasCompactionCheckpoint
                (buildLocalCompactedHistory 1 [user "old"] "local summary")
                `shouldBe` True
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
        , extraFields = KeyMap.empty
        }
    assistant text = MessageItem ResponseMessage
        { messageId = Nothing
        , content = MessageContentParts [InputTextPart text Nothing KeyMap.empty]
        , role = RoleAssistant
        , status = Nothing
        , phase = Nothing
        , extraFields = KeyMap.empty
        }
    isSummary (MessageItem m) =
        m.role == RoleAssistant
            && case m.content of
                MessageContentParts (OutputTextPart text _ _ _ : _) ->
                    Text.isPrefixOf summaryPrefix text
                _ -> False
    isSummary _ = False
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
        KnownResponseItem ItemAgentMessage TaggedObject
            { tag = "agent_message"
            , fields = KeyMap.fromList
                [ ("author", Aeson.String author)
                , ("recipient", Aeson.String recipient)
                , ("content", Aeson.toJSON
                    [ Aeson.object
                        [ "type" .= ("input_text" :: Text.Text)
                        , "text" .= text
                        ]
                    ])
                ]
            }
    checkpoint name = KnownResponseItem ItemCompaction TaggedObject
        { tag = "compaction"
        , fields = KeyMap.fromList [("name", Aeson.String name)]
        }
