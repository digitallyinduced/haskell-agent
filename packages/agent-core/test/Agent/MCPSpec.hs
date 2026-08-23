module Agent.MCPSpec (spec) where

import Agent.Loop (defaultLoopDispatch)
import Agent.MCP
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , dispatchToolCall
    , functionToolCall
    )
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , appToolHandlers
    )
import Control.Exception.Safe (bracket)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (wait, withAsync)
import Data.Aeson (object, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (find)
import qualified Data.Text as Text
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)
import System.Posix.Files (setFileMode)
import Test.Hspec

spec :: Spec
spec = describe "Agent.MCP" do
    it "redacts configured environment values from Show" do
        let rendered = show McpServerConfig
                { mcpServerName = "private"
                , mcpServerCommand = "/bin/server"
                , mcpServerArgs = []
                , mcpServerCwd = Nothing
                , mcpServerEnv = [("API_TOKEN", "super-secret")]
                , mcpServerStartupTimeoutSeconds = 5
                , mcpServerRequestTimeoutSeconds = 5
                }
        rendered `shouldContain` "API_TOKEN"
        rendered `shouldContain` "<redacted>"
        rendered `shouldNotContain` "super-secret"

    describe "normalizeMcpToolResult" do
        it "extracts successful text content" do
            normalizeMcpToolResult
                (object
                    [ "content" .=
                        [ object
                            [ "type" .= ("text" :: Text.Text)
                            , "text" .= ("hello" :: Text.Text)
                            ]
                        ]
                    ])
                `shouldBe` Right "hello"

        it "keeps structured content as compact JSON" do
            normalizeMcpToolResult
                (object ["structuredContent" .= object ["answer" .= (42 :: Int)]])
                `shouldBe` Right "{\"answer\":42}"

        it "turns MCP isError results into handler errors" do
            normalizeMcpToolResult
                (object
                    [ "isError" .= True
                    , "content" .=
                        [ object
                            [ "type" .= ("text" :: Text.Text)
                            , "text" .= ("denied" :: Text.Text)
                            ]
                        ]
                    ])
                `shouldBe` Left "denied"

    it "initializes a stdio server, exposes only read-only tools, and calls one" $
        withFakeServer \script -> do
            started <- newIORef []
            fleet <- startMcpFleetWithProgress
                (\name -> modifyIORef' started (<> [name]))
                [ McpServerConfig
                    { mcpServerName = "fake"
                    , mcpServerCommand = script
                    , mcpServerArgs = []
                    , mcpServerCwd = Nothing
                    , mcpServerEnv = []
                    , mcpServerStartupTimeoutSeconds = 5
                    , mcpServerRequestTimeoutSeconds = 5
                    }
                ]
            bracket (pure fleet) closeMcpFleet \_ -> do
                readIORef started `shouldReturn` ["fake"]
                let tools = mcpFleetTools fleet
                map (.appToolName) tools `shouldBe` ["echo_read"]
                fleet.mcpFleetWarnings `shouldBe`
                    ["MCP server fake skipped non-read-only tool mutate"]
                Just tool <- pure (find ((== "echo_read") . (.appToolName)) tools)
                case tool.appToolApproval of
                    AlwaysReadOnly -> pure ()
                    _ -> expectationFailure "expected read-only approval"
                let dispatch ident message = dispatchToolCall
                        defaultLoopDispatch
                        (appToolHandlers tools)
                        (functionToolCall ident "echo_read" message)
                withAsync (dispatch "call-1" "{\"message\":\"first\"}") \first -> do
                    threadDelay 50000
                    second <- dispatch "call-2" "{\"message\":\"second\"}"
                    firstResult <- wait first
                    firstResult.output `shouldBe` "first response"
                    second.output `shouldBe` "second response"

withFakeServer :: (FilePath -> IO a) -> IO a
withFakeServer action = do
    temporary <- getTemporaryDirectory
    bracket
        (do
            (path, handle) <- openTempFile temporary "agent-mcp-fake.sh"
            LBS.hPutStr handle fakeServer
            hClose handle
            setFileMode path 0o700
            pure path)
        removeFile
        action

fakeServer :: LBS.ByteString
fakeServer =
    "#!/bin/sh\n\
    \while IFS= read -r line; do\n\
    \  case \"$line\" in\n\
    \    *'\"method\":\"initialize\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},\"serverInfo\":{\"name\":\"fake\",\"version\":\"1\"}}}'\n\
    \      ;;\n\
    \    *'\"method\":\"notifications/initialized\"'*) ;;\n\
    \    *'\"method\":\"tools/list\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{\"tools\":[{\"name\":\"echo_read\",\"description\":\"Echo.\",\"inputSchema\":{\"type\":\"object\",\"properties\":{\"message\":{\"type\":\"string\"}},\"required\":[\"message\"],\"additionalProperties\":false},\"annotations\":{\"readOnlyHint\":true}},{\"name\":\"mutate\",\"description\":\"Write.\",\"inputSchema\":{\"type\":\"object\"},\"annotations\":{\"readOnlyHint\":false}}]}}'\n\
    \      ;;\n\
    \    *'\"method\":\"tools/call\"'*)\n\
    \      if [ -z \"$first_call_seen\" ]; then\n\
    \        first_call_seen=1\n\
    \      else\n\
    \        printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":4,\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"second response\"}]}}'\n\
    \        printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":3,\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"first response\"}]}}'\n\
    \      fi\n\
    \      ;;\n\
    \  esac\n\
    \done\n"
