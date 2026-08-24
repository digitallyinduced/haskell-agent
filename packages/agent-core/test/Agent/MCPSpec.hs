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
import System.Directory
    ( createDirectory
    , getTemporaryDirectory
    , removeDirectoryRecursive
    , removeFile
    )
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
                (\names -> modifyIORef' started (<> [names]))
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
                readIORef started `shouldReturn` [["fake"], []]
                let tools = mcpFleetTools fleet
                map (.appToolName) tools `shouldBe` ["fake__echo_read"]
                fleet.mcpFleetWarnings `shouldBe`
                    ["MCP server fake skipped non-read-only tool mutate"]
                mcpFleetStatuses fleet `shouldReturn`
                    [McpServerStatus "fake" McpReady 1]
                Just tool <-
                    pure (find ((== "fake__echo_read") . (.appToolName)) tools)
                case tool.appToolApproval of
                    AlwaysReadOnly -> pure ()
                    _ -> expectationFailure "expected read-only approval"
                let dispatch ident message = dispatchToolCall
                        defaultLoopDispatch
                        (appToolHandlers tools)
                        (functionToolCall ident "fake__echo_read" message)
                withAsync (dispatch "call-1" "{\"message\":\"first\"}") \first -> do
                    threadDelay 50000
                    second <- dispatch "call-2" "{\"message\":\"second\"}"
                    firstResult <- wait first
                    firstResult.output `shouldBe` "first response"
                    second.output `shouldBe` "second response"

    it "starts servers concurrently and preserves configured tool order" $
        withConcurrentFakeServer \script barrier -> do
            progress <- newIORef []
            fleet <- startMcpFleetWithProgress
                (\names -> modifyIORef' progress (<> [names]))
                [ concurrentConfig script barrier "first"
                , concurrentConfig script barrier "second"
                ]
            bracket (pure fleet) closeMcpFleet \_ -> do
                map (.appToolName) (mcpFleetTools fleet)
                    `shouldBe`
                        ["first__shared_read", "second__shared_read"]
                updates <- readIORef progress
                updates `shouldContain` [["first", "second"]]
                last updates `shouldBe` []

    it "reports failed configured servers without hiding healthy ones" $
        withFakeServer \script -> do
            fleet <- startMcpFleet
                [ baseConfig "missing" "/definitely/not/an/mcp-server"
                , baseConfig "healthy" script
                ]
            bracket (pure fleet) closeMcpFleet \_ -> do
                statuses <- mcpFleetStatuses fleet
                map (.mcpStatusName) statuses
                    `shouldBe` ["missing", "healthy"]
                case statuses of
                    [ McpServerStatus "missing" (McpFailed _) 0
                        , McpServerStatus "healthy" McpReady 1
                        ] -> pure ()
                    _ -> expectationFailure ("unexpected statuses: " <> show statuses)

    it "rejects duplicate server names before spawning" do
        startMcpFleet
            [baseConfig "duplicate" "/bin/false", baseConfig "duplicate" "/bin/false"]
            `shouldThrow` anyIOException

    it "escapes qualified-name separators without collisions" $
        withConcurrentFakeServer \script barrier -> do
            fleet <- startMcpFleet
                [ concurrentConfig script barrier "a"
                , concurrentConfig script barrier "a__shared"
                ]
            bracket (pure fleet) closeMcpFleet \_ ->
                map (.appToolName) (mcpFleetTools fleet)
                    `shouldBe`
                        [ "a__shared_read"
                        , "a%5F%5Fshared__shared_read"
                        ]

    it "returns a progressive fleet before handshake completion and publishes meta-tools" $
        withDelayedFakeServer \script -> do
            progress <- newIORef []
            fleet <- startMcpFleetProgressive
                (\statuses -> modifyIORef' progress (<> [statuses]))
                [progressiveConfig script "0.2" "slow"]
            bracket (pure fleet) closeMcpFleet \_ -> do
                initial <- mcpFleetStatuses fleet
                case initial of
                    [McpServerStatus "slow" state 0] ->
                        state `shouldSatisfy`
                            (`elem` [McpPending, McpInitializing])
                    _ -> expectationFailure ("unexpected initial status: " <> show initial)
                let tools = mcpFleetMetaTools fleet
                    dispatch ident name arguments = dispatchToolCall
                        defaultLoopDispatch
                        (appToolHandlers tools)
                        (functionToolCall ident name arguments)
                early <- dispatch "early" "mcp_call"
                    "{\"name\":\"slow__delayed_read\",\"arguments\":{}}"
                early.output `shouldSatisfy`
                    Text.isInfixOf "still connecting"
                waitUntilReady fleet
                searched <- dispatch "search" "mcp_search"
                    "{\"query\":\"delayed\"}"
                searched.output `shouldSatisfy`
                    Text.isInfixOf "slow__delayed_read"
                called <- dispatch "call" "mcp_call"
                    "{\"name\":\"slow__delayed_read\",\"arguments\":{}}"
                called.output `shouldBe` "delayed response"
                updates <- readIORef progress
                updates `shouldSatisfy` (not . null)

    it "publishes a fast server before a slow progressive peer settles" $
        withDelayedFakeServer \script -> do
            fleet <- startMcpFleetProgressive
                (const (pure ()))
                [ progressiveConfig script "0.02" "fast"
                , progressiveConfig script "0.5" "slow"
                ]
            bracket (pure fleet) closeMcpFleet \_ -> do
                waitForServerReady fleet "fast"
                statuses <- mcpFleetStatuses fleet
                find ((== "slow") . (.mcpStatusName)) statuses
                    `shouldSatisfy`
                        maybe False
                            ((`elem` [McpPending, McpInitializing])
                                . (.mcpStatusState))
                let tools = mcpFleetMetaTools fleet
                searched <- dispatchToolCall
                    defaultLoopDispatch
                    (appToolHandlers tools)
                    (functionToolCall "search-fast" "mcp_search"
                        "{\"server\":\"fast\"}")
                searched.output `shouldSatisfy`
                    Text.isInfixOf "fast__delayed_read"

    it "reports a handshake failure after successful process construction" do
        let config =
                (baseConfig "exits" "/bin/sh")
                    { mcpServerArgs = ["-c", "exit 1"]
                    }
        fleet <- startMcpFleetProgressive (const (pure ())) [config]
        bracket (pure fleet) closeMcpFleet \_ -> do
            waitForServerFailure fleet "exits"
            statuses <- mcpFleetStatuses fleet
            case statuses of
                [McpServerStatus "exits" (McpFailed _) 0] -> pure ()
                _ -> expectationFailure ("unexpected statuses: " <> show statuses)

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

concurrentConfig :: FilePath -> FilePath -> Text.Text -> McpServerConfig
concurrentConfig script barrier name = McpServerConfig
    { mcpServerName = name
    , mcpServerCommand = script
    , mcpServerArgs = [barrier, Text.unpack name]
    , mcpServerCwd = Nothing
    , mcpServerEnv = []
    , mcpServerStartupTimeoutSeconds = 2
    , mcpServerRequestTimeoutSeconds = 2
    }

progressiveConfig :: FilePath -> String -> Text.Text -> McpServerConfig
progressiveConfig script delay name =
    (baseConfig name script)
        { mcpServerArgs = [delay]
        }

baseConfig :: Text.Text -> FilePath -> McpServerConfig
baseConfig name command = McpServerConfig
    { mcpServerName = name
    , mcpServerCommand = command
    , mcpServerArgs = []
    , mcpServerCwd = Nothing
    , mcpServerEnv = []
    , mcpServerStartupTimeoutSeconds = 2
    , mcpServerRequestTimeoutSeconds = 2
    }

withConcurrentFakeServer :: (FilePath -> FilePath -> IO a) -> IO a
withConcurrentFakeServer action = do
    temporary <- getTemporaryDirectory
    bracket
        (do
            (barrier, barrierHandle) <-
                openTempFile temporary "agent-mcp-barrier"
            hClose barrierHandle
            removeFile barrier
            createDirectory barrier
            (script, scriptHandle) <-
                openTempFile temporary "agent-mcp-concurrent.sh"
            LBS.hPutStr scriptHandle concurrentFakeServer
            hClose scriptHandle
            setFileMode script 0o700
            pure (script, barrier))
        (\(script, barrier) -> do
            removeFile script
            removeDirectoryRecursive barrier)
        (uncurry action)

withDelayedFakeServer :: (FilePath -> IO a) -> IO a
withDelayedFakeServer action = do
    temporary <- getTemporaryDirectory
    bracket
        (do
            (path, handle) <- openTempFile temporary "agent-mcp-delayed.sh"
            LBS.hPutStr handle delayedFakeServer
            hClose handle
            setFileMode path 0o700
            pure path)
        removeFile
        action

waitUntilReady :: McpFleet -> IO ()
waitUntilReady fleet = waitForServerReady fleet "slow"

waitForServerReady :: McpFleet -> Text.Text -> IO ()
waitForServerReady fleet name = go (200 :: Int)
  where
    go 0 = expectationFailure "MCP server did not become ready"
    go remaining = do
        statuses <- mcpFleetStatuses fleet
        if any
            (\status ->
                status.mcpStatusName == name
                    && status.mcpStatusState == McpReady)
            statuses
            then pure ()
            else threadDelay 10000 >> go (remaining - 1)

waitForServerFailure :: McpFleet -> Text.Text -> IO ()
waitForServerFailure fleet name = go (200 :: Int)
  where
    go 0 = expectationFailure "MCP server did not fail"
    go remaining = do
        statuses <- mcpFleetStatuses fleet
        if any
            (\status ->
                status.mcpStatusName == name
                    && case status.mcpStatusState of
                        McpFailed _ -> True
                        _ -> False)
            statuses
            then pure ()
            else threadDelay 10000 >> go (remaining - 1)

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

delayedFakeServer :: LBS.ByteString
delayedFakeServer =
    "#!/bin/sh\n\
    \delay=\"$1\"\n\
    \while IFS= read -r line; do\n\
    \  case \"$line\" in\n\
    \    *'\"method\":\"initialize\"'*)\n\
    \      sleep \"$delay\"\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},\"serverInfo\":{\"name\":\"delayed\",\"version\":\"1\"}}}'\n\
    \      ;;\n\
    \    *'\"method\":\"notifications/initialized\"'*) ;;\n\
    \    *'\"method\":\"tools/list\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{\"tools\":[{\"name\":\"delayed_read\",\"description\":\"Delayed read.\",\"inputSchema\":{\"type\":\"object\"},\"annotations\":{\"readOnlyHint\":true}}]}}'\n\
    \      ;;\n\
    \    *'\"method\":\"tools/call\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":3,\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"delayed response\"}]}}'\n\
    \      ;;\n\
    \  esac\n\
    \done\n"

concurrentFakeServer :: LBS.ByteString
concurrentFakeServer =
    "#!/bin/sh\n\
    \barrier=\"$1\"\n\
    \name=\"$2\"\n\
    \while IFS= read -r line; do\n\
    \  case \"$line\" in\n\
    \    *'\"method\":\"initialize\"'*)\n\
    \      : > \"$barrier/$name\"\n\
    \      while [ \"$(find \"$barrier\" -type f | wc -l)\" -lt 2 ]; do\n\
    \        sleep 0.01\n\
    \      done\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},\"serverInfo\":{\"name\":\"fake\",\"version\":\"1\"}}}'\n\
    \      ;;\n\
    \    *'\"method\":\"notifications/initialized\"'*) ;;\n\
    \    *'\"method\":\"tools/list\"'*)\n\
    \      printf '%s\\n' \"{\\\"jsonrpc\\\":\\\"2.0\\\",\\\"id\\\":2,\\\"result\\\":{\\\"tools\\\":[{\\\"name\\\":\\\"shared_read\\\",\\\"description\\\":\\\"Read.\\\",\\\"inputSchema\\\":{\\\"type\\\":\\\"object\\\"},\\\"annotations\\\":{\\\"readOnlyHint\\\":true}}]}}\"\n\
    \      ;;\n\
    \  esac\n\
    \done\n"
