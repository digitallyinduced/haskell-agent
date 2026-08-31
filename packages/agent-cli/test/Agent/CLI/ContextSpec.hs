module Agent.CLI.ContextSpec (spec) where

import Agent.CLI.Compaction (estimatedOccupancy, reportedOccupancy)
import Agent.CLI.Context (contextUsageTokens, formatContextReport)
import Agent.CLI.Request (requestParams)
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
