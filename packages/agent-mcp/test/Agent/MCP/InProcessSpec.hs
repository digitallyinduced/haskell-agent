module Agent.MCP.InProcessSpec (spec) where

import qualified Agent.Json.Decode as Json
import Agent.Loop (defaultLoopDispatch)
import Agent.MCP.InProcess
import Agent.ToolDSL
    ( PropertySchema(..)
    , PropertyType(..)
    )
import Agent.ToolDispatch
    ( ToolCall(..)
    , noArgsTool
    , typedTool
    )
import Agent.Tools.Types
    ( AppTool
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , freeformApplyPatchAppToolWithExecution
    , jsonAppToolWithExecution
    )
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (toList)
import Data.IORef
    ( modifyIORef'
    , newIORef
    , readIORef
    )
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "in-process MCP server" do
    it "initializes and advertises JSON schemas" do
        server <- testServer (const (pure (Right True))) [echoTool]
        response <- handleInProcessMcpMessage server $
            request 1 "initialize" (object
                [ "protocolVersion" .= ("2025-11-25" :: Text)
                ])
        response `shouldSatisfy`
            hasPath ["result", "capabilities", "tools"]

        listed <- handleInProcessMcpMessage server $
            request 2 "tools/list" (object [])
        listed `shouldSatisfy`
            hasPath ["result", "tools"]
        inProcessMcpToolNames server `shouldBe` ["echo"]

    it "runs approved calls through the registered handler" do
        approved <- newIORef []
        server <- testServer
            (\call -> do
                modifyIORef' approved (<> [call.name])
                pure (Right True))
            [echoTool]
        response <- handleInProcessMcpMessage server $
            request 3 "tools/call" (object
                [ "name" .= ("echo" :: Text)
                , "arguments" .= object ["message" .= ("hello" :: Text)]
                ])
        response `shouldSatisfy` hasTextResult "echo:hello" False
        readIORef approved `shouldReturn` ["echo"]

    it "does not execute a user-rejected call" do
        executions <- newIORef (0 :: Int)
        let tool = jsonAppToolWithExecution
                "count"
                "Count executions"
                []
                AlwaysPrompt
                TurnSequential
                (noArgsTool "count" do
                    modifyIORef' executions (+ 1)
                    pure (Right "counted"))
        server <- testServer (const (pure (Right False))) [tool]
        response <- handleInProcessMcpMessage server $
            request 4 "tools/call" (object
                [ "name" .= ("count" :: Text)
                , "arguments" .= object []
                ])
        response `shouldSatisfy`
            hasTextResult "Tool call rejected by user." True
        readIORef executions `shouldReturn` 0

    it "preserves handler failure independently of rendered output" do
        let failing = jsonAppToolWithExecution
                "fail"
                "Fail"
                []
                AlwaysReadOnly
                TurnSequential
                (noArgsTool "fail" (pure (Left "expected failure")))
        server <- testServer (const (pure (Right True))) [failing]
        response <- handleInProcessMcpMessage server $
            request 5 "tools/call" (object
                [ "name" .= ("fail" :: Text)
                , "arguments" .= object []
                ])
        response `shouldSatisfy` hasTextResult "Error: expected failure" True

    it "rejects freeform tools instead of changing their input contract" do
        let freeform = freeformApplyPatchAppToolWithExecution
                "patch"
                "Patch"
                AlwaysPrompt
                TurnSequential
                (noArgsTool "patch" (pure (Right "unused")))
        case createInProcessMcpServer
                "test" "1" defaultLoopDispatch
                (const (pure (Right True)))
                [freeform] of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected freeform tool rejection"

    it "does not answer JSON-RPC notifications" do
        server <- testServer (const (pure (Right True))) [echoTool]
        handleInProcessMcpMessage server
            (object
                [ "jsonrpc" .= ("2.0" :: Text)
                , "method" .= ("notifications/initialized" :: Text)
                ])
            `shouldReturn` Nothing

echoTool :: AppTool
echoTool =
    jsonAppToolWithExecution
        "echo"
        "Echo a message"
        [ PropertySchema
            { propertyName = "message"
            , propertyType = PropertyString
            , required = True
            , description = Nothing
            }
        ]
        AlwaysReadOnly
        ParallelSafe
        (typedTool "echo"
            (Json.object (Json.atKey "message" Json.text))
            (\message -> pure (Right ("echo:" <> message))))

testServer
    :: InProcessMcpApproval
    -> [AppTool]
    -> IO InProcessMcpServer
testServer approval tools =
    either (fail . Text.unpack) pure $
        createInProcessMcpServer
            "haskell-agent"
            "0.1.0"
            defaultLoopDispatch
            approval
            tools

request :: Int -> Text -> Value -> Value
request requestId method parameters =
    object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= requestId
        , "method" .= method
        , "params" .= parameters
        ]

hasPath :: [Text] -> Maybe Value -> Bool
hasPath keys = maybe False (go keys)
  where
    go [] _ = True
    go (key : rest) (Object value) =
        maybe False (go rest) (KeyMap.lookup (Key.fromText key) value)
    go _ _ = False

hasTextResult :: Text -> Bool -> Maybe Value -> Bool
hasTextResult expectedText expectedError value =
    lookupPath ["result", "isError"] value == Just (Bool expectedError)
        && resultText value == Just expectedText
  where
    resultText root =
        case lookupPath ["result", "content"] root of
            Just (Array content)
                | Object first : _ <- toList content
                , Just (String text) <- KeyMap.lookup "text" first ->
                    Just text
            _ -> Nothing

lookupPath :: [Text] -> Maybe Value -> Maybe Value
lookupPath keys root = root >>= go keys
  where
    go [] value = Just value
    go (key : rest) (Object value) =
        KeyMap.lookup (Key.fromText key) value >>= go rest
    go _ _ = Nothing
