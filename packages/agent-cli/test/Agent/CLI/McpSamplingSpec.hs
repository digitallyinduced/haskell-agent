module Agent.CLI.McpSamplingSpec (spec) where

import Agent.CLI.McpSampling (mcpSamplingHandler)
import Agent.Json (rawJsonFromEncoding)
import Agent.Loop
    ( Backend(..), BackendResult(..), BackendSnapshot(..), emptyTurnOutput )
import Agent.MCP
import Agent.Responses.Types
    (ResponseCreateParams(..), ResponseItem(..), ResponseMessage(..), ResponseRole(..))
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.IORef
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = describe "mcpSamplingHandler" do
    it "runs a private tool-free completion with the requested limits" do
        seen <- newIORef Nothing
        let factory params = Backend \state previous inputs _ -> do
                writeIORef seen (Just (params, state.backendItems, previous, inputs))
                pure $ Right BackendResult
                    { backendOutput =
                        emptyTurnOutput "sampling-response" [] (Just "sampled")
                    , backendState = state
                    }
        result <- mcpSamplingHandler "active-model" factory sampleRequest
        case result of
            Left err -> expectationFailure (show err)
            Right sampled -> do
                sampled.samplingResultModel `shouldBe` "active-model"
                sampled.samplingResultRole `shouldBe` "assistant"
        Just (params, messages, previous, inputs) <- readIORef seen
        params.instructions `shouldBe` Just "Be concise."
        params.maxOutputTokens `shouldBe` Just 23
        params.tools `shouldBe` Just []
        params.store `shouldBe` Just False
        previous `shouldBe` Nothing
        inputs `shouldBe` []
        map messageRole messages `shouldBe` [Just "user", Just "assistant"]

    it "rejects non-text content before invoking a backend" do
        called <- newIORef False
        let factory _ = Backend \state _ _ _ -> do
                writeIORef called True
                pure $ Right BackendResult
                    { backendOutput = emptyTurnOutput "response" [] (Just "oops")
                    , backendState = state
                    }
            request = sampleRequest
                { samplingMessages =
                    [McpSamplingMessage "user" (rawJsonFromEncoding $
                        Aeson.toEncoding $ Aeson.object
                            ["type" .= ("image" :: String)])]
                }
        mcpSamplingHandler "model" factory request
            `shouldReturn` Left "MCP sampling only supports text content"
        readIORef called `shouldReturn` False

sampleRequest :: McpSamplingRequest
sampleRequest = McpSamplingRequest
    { samplingServerName = "test"
    , samplingMessages =
        [ textMessage "user" "question"
        , textMessage "assistant" "prior answer"
        ]
    , samplingModelPreferences = Nothing
    , samplingSystemPrompt = Just "Be concise."
    , samplingIncludeContext = Nothing
    , samplingTemperature = Just 0.2
    , samplingMaxTokens = 23
    , samplingStopSequences = []
    , samplingMetadata = Nothing
    , samplingTools = Nothing
    , samplingToolChoice = Nothing
    }

textMessage :: Text -> Text -> McpSamplingMessage
textMessage role text =
    McpSamplingMessage role $ rawJsonFromEncoding $
        Aeson.toEncoding $ Aeson.object
            [ "type" .= ("text" :: String)
            , "text" .= text
            ]

messageRole :: ResponseItem -> Maybe String
messageRole (MessageItem message) = Just $ case message.role of
    RoleUser -> "user"
    RoleAssistant -> "assistant"
    _ -> "other"
messageRole _ = Nothing
