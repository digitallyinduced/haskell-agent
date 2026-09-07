module Agent.Integrations.ServerSpec (spec) where

import Agent.Integrations.Registry
import Agent.Integrations.Server
import Agent.Integrations.Types
import Agent.Json (rawJsonFromEncoding)
import qualified Agent.MCP as MCP
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = describe "integration MCP host" do
    it "decodes at the boundary and exposes typed tool results" do
        registry <- case exampleModule >>= \module_ ->
                createIntegrationRegistry [module_] of
            Left err -> expectationFailure (show err) >> fail "registry setup failed"
            Right value -> pure value
        host <- newIntegrationHost registry
        let server = integrationMcpServer host
        listed <- server.toolServerListTools
        fmap length listed `shouldBe` Right 1
        result <- server.toolServerCallTool
            MCP.McpCallToolRequest
                { MCP.callToolName = "example_echo"
                , MCP.callToolArguments = rawJsonFromEncoding (Aeson.toEncoding Aeson.Null)
                , MCP.callToolRequestId = Nothing
                }
        case result of
            Right response -> do
                response.callToolIsError `shouldBe` False
                response.callToolText `shouldBe` ["\"typed result\""]
            Left err -> expectationFailure (show err)
        closeIntegrationHost host

exampleModule :: Either Text IntegrationModule
exampleModule = do
    moduleId <- integrationId "example"
    toolName <- integrationToolName "example_echo"
    pure $ IntegrationModule { integrationModuleId = moduleId
    , integrationModuleInstructions = ""
    , integrationModuleTools = pure
        [ SomeIntegrationTool IntegrationTool
            { integrationToolNameValue = toolName
            , integrationToolDescription = "Returns a typed result."
            , integrationToolInput =
                jsonInputContract Aeson.Null (pure ())
            , integrationToolOutput =
                aesonOutputContract Aeson.Null
            , integrationToolEffect = IntegrationReadOnly
            , integrationToolDestructive = False
            , integrationToolIdempotent = True
            , integrationToolOpenWorld = False
            , integrationToolMaximumOutputBytes = 1024
            , integrationToolHandler = \() ->
                pure (Right ("typed result" :: Text))
            }
        ]
    }
