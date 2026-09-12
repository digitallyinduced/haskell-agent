module Agent.CLI.ContextSpec (spec) where

import Agent.CLI.Compaction
    ( estimatedOccupancy
    , reportedOccupancy
    , occupancyOnTurnFinished
    , occupancyForSubmission
    , occupancySnapshot
    , projectRequestTokens
    )
import Agent.CLI.Context (contextUsageTokens, formatContextReport)
import Agent.Runtime.ProviderRequest (requestParams)
import Agent.CLI.Session.ConversationStore
    ( commitConversationBackendState
    , newConversationStore
    , withConversationBackendState
    )
import Agent.Loop
    ( BackendContinuation(..)
    , BackendResult(..)
    , BackendRevision(..)
    , BackendSnapshot(..)
    , TokenUsage(..)
    , TurnOutput(..)
    , emptyTurnOutput
    , advanceBackendSnapshot
    , initialBackendSnapshot
    )
import Agent.Provider (Provider(OpenAIProvider))
import Agent.Responses.Types
import Data.List (isInfixOf)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.Context" do
    let params = defaultResponseCreateParams
        history =
            [ MessageItem
                ResponseMessage
                    { messageId = Nothing
                    , content = MessageContentText "hello"
                    , role = RoleUser
                    , status = Nothing
                    , phase = Nothing
                    , passthrough = Nothing
                    }
            ]
    it "labels a matching provider occupancy" do
        let output = formatContextReport
                "gpt-test"
                (Just 1000)
                (Just (reportedOccupancy 42 (length history)))
                params
                history
                ["shell"]
        Text.unpack output `shouldSatisfy` isInfixOf "provider reported"
        Text.unpack output `shouldSatisfy` isInfixOf "Active tools: 1"

    it "uses the latest response context usage rather than aggregate billing usage" do
        let output = (emptyTurnOutput "claude-session" [] (Just "done"))
                { tokenUsage = TokenUsage 23650616 192904 23059308
                , contextUsage = Just (TokenUsage 120000 500 110000)
                }
            result = BackendResult
                { backendOutput = output
                , backendState = initialBackendSnapshot history
                }
        occupancySnapshot result
            `shouldBe` Just (reportedOccupancy 120500 (length history))
        contextUsageTokens (occupancySnapshot result) params history
            `shouldBe` 120500

    it "does not substitute aggregate billing usage for missing context usage" do
        let output = (emptyTurnOutput "claude-session" [] (Just "done"))
                { tokenUsage = TokenUsage 23650616 192904 23059308 }
            result = BackendResult
                { backendOutput = output
                , backendState = initialBackendSnapshot history
                }
        occupancySnapshot result `shouldBe` Nothing
        contextUsageTokens (occupancySnapshot result) params history
            `shouldBe` contextUsageTokens Nothing params history

    it "uses live Claude occupancy only for the same checkpoint and continuation" do
        let continuation = BackendContinuation "anthropic.claude-code" "claude-session"
            state = (initialBackendSnapshot history)
                { backendContinuation = Just continuation }
            output = (emptyTurnOutput "claude-session" [] (Just "done"))
                { contextUsage = Just (TokenUsage 120000 500 110000) }
            occupancy = occupancySnapshot (BackendResult output state)
        occupancyForSubmission state (Just "claude-session") occupancy
            `shouldBe` occupancy
        occupancyForSubmission state Nothing occupancy `shouldBe` Nothing
        occupancyForSubmission state (Just "another-session") occupancy
            `shouldBe` Nothing
        occupancyForSubmission
            (state { backendContinuation = Nothing })
            (Just "claude-session")
            occupancy
            `shouldBe` Nothing
        occupancyForSubmission
            (advanceBackendSnapshot state history (Just continuation))
            (Just "claude-session")
            occupancy
            `shouldBe` Nothing

    it "rejects live occupancy after a same-length host transcript replacement" do
        let state = (initialBackendSnapshot history)
                { backendContinuation =
                    Just (BackendContinuation "anthropic.claude-code" "claude-session")
                }
            output = (emptyTurnOutput "claude-session" [] (Just "done"))
                { contextUsage = Just (TokenUsage 120000 500 110000) }
            occupancy = occupancySnapshot (BackendResult output state)
            replacement =
                [ MessageItem message { content = MessageContentText "changed" }
                | MessageItem message <- history
                ]
        length replacement `shouldBe` length history
        occupancyForSubmission
            (state { backendItems = replacement })
            (Just "claude-session")
            occupancy
            `shouldBe` Nothing
        projectRequestTokens (Just params) occupancy replacement []
            `shouldBe` projectRequestTokens (Just params) Nothing replacement []
        contextUsageTokens occupancy params replacement
            `shouldBe` contextUsageTokens Nothing params replacement

    it "accepts an authoritative non-Claude checkpoint without the legacy response id" do
        let state = (initialBackendSnapshot history)
                { backendContinuation =
                    Just (BackendContinuation "openai.responses" "resp-1")
                }
            output = (emptyTurnOutput "resp-1" [] (Just "done"))
                { contextUsage = Just (TokenUsage 120000 500 110000) }
            occupancy = occupancySnapshot (BackendResult output state)
        occupancyForSubmission state Nothing occupancy `shouldBe` occupancy
        occupancyForSubmission state (Just "resp-other") occupancy
            `shouldBe` Nothing

    it "keeps measured occupancy valid across an ordinary authoritative commit" do
        store <- newConversationStore Nothing history []
        before <- withConversationBackendState store pure
        let continuation = BackendContinuation "anthropic.claude-code" "claude-session"
            candidate = advanceBackendSnapshot before history (Just continuation)
            turn = (emptyTurnOutput "claude-session" [] (Just "done"))
                { tokenUsage = TokenUsage 23650616 192904 23059308
                , contextUsage = Just (TokenUsage 120000 500 110000)
                }
        committed <- commitConversationBackendState store candidate
        committed `shouldBe` candidate
        let occupancy = occupancyOnTurnFinished committed turn
                (occupancySnapshot (BackendResult turn candidate))
        occupancy `shouldBe` occupancySnapshot (BackendResult turn candidate)
        occupancyForSubmission committed (Just "claude-session") occupancy
            `shouldBe` occupancy
        contextUsageTokens occupancy params history `shouldBe` 120500

    it "preserves the provider checkpoint when completion renumbers the commit" do
        store <- newConversationStore Nothing history []
        let candidate = (initialBackendSnapshot history)
                { backendRevision = BackendRevision 999
                , backendContinuation =
                    Just (BackendContinuation "anthropic.claude-code" "claude-session")
                }
            turn = (emptyTurnOutput "claude-session" [] (Just "done"))
                { tokenUsage = TokenUsage 23650616 192904 23059308
                , contextUsage = Just (TokenUsage 120000 500 110000)
                }
        committed <- commitConversationBackendState store candidate
        committed.backendRevision `shouldBe` BackendRevision 1
        let providerOccupancy = occupancySnapshot (BackendResult turn candidate)
            occupancy = occupancyOnTurnFinished committed turn providerOccupancy
        occupancy `shouldBe` providerOccupancy
        occupancyForSubmission committed (Just "claude-session") occupancy
            `shouldBe` Nothing
        occupancyForSubmission candidate (Just "claude-session") occupancy
            `shouldBe` occupancy
        contextUsageTokens occupancy params history `shouldBe` 120500
        occupancyOnTurnFinished committed (turn { contextUsage = Nothing }) occupancy
            `shouldBe` Nothing
    it "does not retain measured occupancy without a response continuation" do
        let output = (emptyTurnOutput "" [] (Just "done"))
                { contextUsage = Just (TokenUsage 120000 500 110000) }
            result = BackendResult
                { backendOutput = output
                , backendState = initialBackendSnapshot history
                }
        occupancySnapshot result `shouldBe` Nothing
    it "falls back to an estimate for stale or estimated occupancy" do
        let output = formatContextReport
                "grok-test"
                Nothing
                (Just (estimatedOccupancy 42 0))
                params
                history
                []
        Text.unpack output `shouldSatisfy` isInfixOf "estimated"
        Text.unpack output `shouldSatisfy` isInfixOf "Window: unknown"

    it "shares provider-reported versus estimated semantics with compact UI" do
        contextUsageTokens
            (Just (reportedOccupancy 42 (length history)))
            params
            history
            `shouldBe` 42
        contextUsageTokens
            (Just (reportedOccupancy 42 0))
            params
            history
            `shouldBe`
                contextUsageTokens Nothing params history

    it "attributes Responses Lite instructions and tool schemas" do
        let liteParams =
                requestParams
                    OpenAIProvider
                    "gpt-5.6-sol"
                    (Text.replicate 40 "instruction ")
                    [FunctionToolValue FunctionTool
                        { name = "shell_command"
                        , description =
                            Just (Text.replicate 20 "description ")
                        , parameters = Nothing
                        , strict = Just True
                        , async = Nothing
                        }]
                    "medium"
            output =
                formatContextReport
                    "gpt-5.6-sol"
                    (Just 272000)
                    Nothing
                    liteParams
                    []
                    ["shell_command"]
        Text.unpack output
            `shouldNotSatisfy` isInfixOf "Instructions: 0"
        Text.unpack output
            `shouldNotSatisfy` isInfixOf "Tool schemas: 0"

    it "preserves over-capacity percentages while clamping the bar and free space" do
        let output =
                formatContextReport
                    "gpt-test"
                    (Just 1000)
                    (Just (reportedOccupancy 1200 (length history)))
                    params
                    history
                    []
        Text.unpack output `shouldSatisfy` isInfixOf "(120.0%)"
        Text.unpack output
            `shouldSatisfy` isInfixOf "Usage: [####################]"
        Text.unpack output `shouldSatisfy` isInfixOf "Free: 0 tokens"
