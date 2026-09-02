module Agent.MCPSpec (spec) where

import Agent.Loop (defaultLoopDispatch)
import Agent.MCP
import Agent.MCP.Client
    ( ProbeOutcome(..)
    , annotateHeaderParams
    , boundedTaskPollDelayMicros
    , closeMcpClient
    , classifyProbe
    , encodeHeaderValue
    , ensureMcpClientReady
    , headerParamValues
    , readBounded
    , remainingHardDeadlineMicros
    , retryUnauthorizedOnce
    , spawnClientWorker
    , splitSseChunk
    , splitLines
    , startMcpClient
    , toolAllowsAutomaticReissue
    )
import Agent.MCP.Types
    ( McpClient(..)
    , McpCatalogEntry(..)
    , McpClientTransport(..)
    , McpHeaderParam(..)
    , McpHttpTransport(..)
    , McpStdioTransport(..)
    , McpTool(..)
    , mcpToolDecoder
    , mcpToolRetrySafe
    )
import Agent.Json (RawJson, rawJsonBytes, rawJsonDecoder, rawJsonFromEncoding)
import qualified Agent.Json.Decode as Json
import Control.Concurrent.STM
    ( TMVar
    , atomically
    , modifyTVar'
    , newEmptyTMVarIO
    , readTMVar
    , readTVar
    , readTVarIO
    , tryPutTMVar
    , writeTVar
    )
import qualified Data.Aeson.Types
import Data.Either (isLeft)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallResult(..)
    , dispatchToolCall
    , functionToolCall
    )
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRequirement(..)
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , appToolHandlers
    , toolApprovalRequirement
    , toolAllowsWithoutPrompt
    )
import Control.Exception.Safe (bracket)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait, waitCatch, withAsync)
import Data.Aeson (object, (.=))
import qualified Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
    ( atomicModifyIORef'
    , modifyIORef'
    , newIORef
    , readIORef
    )
import Data.List (find)
import qualified Data.Text as Text
import System.Directory
    ( createDirectory
    , getTemporaryDirectory
    , listDirectory
    , removeDirectoryRecursive
    , removeFile
    , withCurrentDirectory
    )
import System.IO (hClose, openTempFile)
import System.Posix.Files (setFileMode)
import System.Timeout (timeout)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
    ( Arbitrary(arbitrary, shrink)
    , counterexample
    , elements
    , ioProperty
    , listOf
    , resize
    , shrinkList
    , (===)
    )

spec :: Spec
spec = describe "Agent.MCP" do
    it "redacts configured environment values from Show" do
        let rendered = show McpServerConfig
                { mcpServerName = "private"
                , mcpServerUrl = Nothing
                , mcpServerCommand = "/bin/server"
                , mcpServerArgs = []
                , mcpServerCwd = Nothing
                , mcpServerEnv = [("API_TOKEN", "super-secret")]
                , mcpServerStartupTimeoutSeconds = 5
                , mcpServerRequestTimeoutSeconds = 5
                , mcpServerProtocol = McpProtocolAuto
                }
        rendered `shouldContain` "API_TOKEN"
        rendered `shouldContain` "<redacted>"
        rendered `shouldNotContain` "super-secret"

    describe "client transport" do
        it "stores only HTTP state for an HTTP client" $
            bracket (startMcpClient workerClientConfig) closeMcpClient \client ->
                case client.clientTransport of
                    McpClientHttp transport -> do
                        transport.httpUrl `shouldBe` "http://127.0.0.1:1/mcp"
                        readIORef transport.httpSession `shouldReturn` Nothing
                    McpClientStdio _ ->
                        expectationFailure "expected an HTTP transport"

        it "stores only process state for a stdio client" $
            withFakeServer \script ->
                bracket
                    (startMcpClient (baseConfig "stdio-transport" script))
                    closeMcpClient
                    \client -> case client.clientTransport of
                        McpClientStdio transport -> do
                            readIORef transport.stdioReader >>= \case
                                Just _ -> pure ()
                                Nothing ->
                                    expectationFailure
                                        "stdio response reader was not started"
                            readIORef transport.stdioStderrReader >>= \case
                                Just _ -> pure ()
                                Nothing ->
                                    expectationFailure
                                        "stdio stderr reader was not started"
                        McpClientHttp _ ->
                            expectationFailure "expected a stdio transport"

    describe "client worker lifecycle" do
        it "does not start owned workers after the client is closed" $
            bracket (startMcpClient workerClientConfig) closeMcpClient \client -> do
                closeMcpClient client
                spawnClientWorker client (pure ())
                (length <$> readTVarIO client.clientWorkers) `shouldReturn` 0

        modifyMaxSuccess (const 200) $
            prop "preserves worker ownership for arbitrary lifecycle traces" $
                \(WorkerLifecycle operations) -> ioProperty $
                    bracket (startMcpClient workerClientConfig) closeMcpClient \client -> do
                        gate <- newEmptyTMVarIO
                        mapM_ (applyWorkerOperation client gate) operations
                        workers <- readTVarIO client.clientWorkers
                        _ <- atomically (tryPutTMVar gate ())
                        let expected
                                | CloseWorkerClient `elem` operations = 0
                                | otherwise =
                                    length
                                        [ ()
                                        | SpawnWorker <- operations
                                        ]
                        pure $
                            counterexample
                                ("operations: " <> show operations)
                                (length workers === expected)

    describe "decodeHttpMcpResponse" do
        it "unwraps the JSON-RPC result before MCP payload decoding" do
            let response = BS8.pack
                    "{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{\"tools\":[{\"name\":\"read\"}]}}"
            case decodeHttpMcpResponse response of
                Left err -> expectationFailure (Text.unpack err)
                Right result ->
                    Data.Aeson.decodeStrict (rawJsonBytes result)
                        `shouldBe`
                            Just (object
                                [ "tools" .=
                                    [object ["name" .= ("read" :: Text.Text)]]
                                ])

        it "surfaces JSON-RPC errors instead of treating them as payloads" do
            decodeHttpMcpResponse
                (BS8.pack
                    "{\"jsonrpc\":\"2.0\",\"id\":2,\"error\":{\"code\":-1,\"message\":\"denied\"}}")
                `shouldSatisfy` \case
                    Left err -> "denied" `Text.isInfixOf` err
                    Right _ -> False

    describe "normalizeMcpToolResult" do
        it "extracts successful text content" do
            normalizeMcpToolResult
                (rawJsonFromEncoding . Data.Aeson.toEncoding $ object
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
                (rawJsonFromEncoding . Data.Aeson.toEncoding $
                    object ["structuredContent" .= object ["answer" .= (42 :: Int)]])
                `shouldBe` Right "{\"answer\":42}"

        it "turns MCP isError results into handler errors" do
            normalizeMcpToolResult
                (rawJsonFromEncoding . Data.Aeson.toEncoding $ object
                    [ "isError" .= True
                    , "content" .=
                        [ object
                            [ "type" .= ("text" :: Text.Text)
                            , "text" .= ("denied" :: Text.Text)
                            ]
                        ]
                    ])
                `shouldBe` Left "denied"

    it "does not retry a tool that requires fresh approval" do
        let sensitive = (schemaTool [])
                { discoveredReadOnly = True
                , discoveredIdempotent = True
                , discoveredRequiresFreshApproval = True
                }
        mcpToolRetrySafe sensitive `shouldBe` False

    it "does not refresh OAuth or replay a fresh tool after a 401" do
        refreshes <- newIORef (0 :: Int)
        replays <- newIORef (0 :: Int)
        let sensitive = (schemaTool [])
                { discoveredRequiresFreshApproval = True
                }
            refresh () = do
                modifyIORef' refreshes (+ 1)
                pure (Right ("new-token" :: Text.Text))
            replay _ = do
                modifyIORef' replays (+ 1)
                pure ("retried" :: Text.Text)
        retried <-
            retryUnauthorizedOnce
                (toolAllowsAutomaticReissue sensitive)
                (Just ())
                refresh
                replay
        retried `shouldBe` (Right Nothing :: Either Text.Text (Maybe Text.Text))
        readIORef refreshes `shouldReturn` 0
        readIORef replays `shouldReturn` 0
        ordinary <-
            retryUnauthorizedOnce True (Just ()) refresh replay
        ordinary
            `shouldBe`
                (Right (Just "retried") :: Either Text.Text (Maybe Text.Text))
        readIORef refreshes `shouldReturn` 1
        readIORef replays `shouldReturn` 1

    describe "task poll interval bounds" do
        it "caps a huge server interval by the remaining hard deadline" do
            boundedTaskPollDelayMicros
                2147483647
                (Just 12345)
                `shouldBe` 12345

        it "handles maxBound without overflowing milliseconds to microseconds" do
            boundedTaskPollDelayMicros
                maxBound
                Nothing
                `shouldBe` 30000000
            boundedTaskPollDelayMicros
                maxBound
                (Just 1)
                `shouldBe` 1

        it "normalizes non-positive intervals and uses overflow-safe deadlines" do
            boundedTaskPollDelayMicros minBound Nothing
                `shouldBe` 100000
            remainingHardDeadlineMicros
                0
                0
                maxBound
                `shouldBe` maxBound

    describe "fresh-approval metadata decoding" do
        it "treats malformed _meta and malformed present markers as fresh" do
            let requiresFresh bytes =
                    (.discoveredRequiresFreshApproval)
                        <$> Json.decodeEither mcpToolDecoder bytes
                prefix =
                    "{\"name\":\"sensitive\",\"inputSchema\":{\"type\":\"object\"},\"_meta\":"
            map (requiresFresh . (prefix <>))
                [ "null}"
                , "[]}"
                , "\"invalid\"}"
                , "{\"dev.haskell-agent/fresh-approval\":null}}"
                , "{\"dev.haskell-agent/fresh-approval\":\"yes\"}}"
                ]
                `shouldBe` replicate 5 (Right True)

        it "does not overprompt valid unrelated metadata or an explicit false marker" do
            let requiresFresh bytes =
                    (.discoveredRequiresFreshApproval)
                        <$> Json.decodeEither mcpToolDecoder bytes
                prefix =
                    "{\"name\":\"ordinary\",\"inputSchema\":{\"type\":\"object\"},\"_meta\":"
            requiresFresh (prefix <> "{\"other\":true}}")
                `shouldBe` Right False
            requiresFresh
                (prefix
                    <> "{\"dev.haskell-agent/fresh-approval\":false}}")
                `shouldBe` Right False

    it "exposes MCP mutations behind approval while reads stay unprompted" $
        withFakeServer \script -> do
            started <- newIORef []
            fleet <- startMcpFleetWithProgress
                (\names -> modifyIORef' started (<> [names]))
                [ McpServerConfig
                    { mcpServerName = "fake"
                    , mcpServerUrl = Nothing
                    , mcpServerCommand = script
                    , mcpServerArgs = []
                    , mcpServerCwd = Nothing
                    , mcpServerEnv = []
                    , mcpServerStartupTimeoutSeconds = 5
                    , mcpServerRequestTimeoutSeconds = 5
                    , mcpServerProtocol = McpProtocolAuto
                    }
                ]
            bracket (pure fleet) closeMcpFleet \_ -> do
                readIORef started `shouldReturn` [["fake"], []]
                let tools = mcpFleetTools fleet
                map (.appToolName) tools `shouldBe`
                    ["fake__echo_read", "fake__mutate", "fake__draft"]
                fleet.mcpFleetWarnings `shouldBe` []
                mcpFleetStatuses fleet `shouldReturn`
                    [McpServerStatus "fake" McpReady 3]
                Just readTool <-
                    pure (find ((== "fake__echo_read") . (.appToolName)) tools)
                Just mutateTool <-
                    pure (find ((== "fake__mutate") . (.appToolName)) tools)
                Just draftTool <-
                    pure (find ((== "fake__draft") . (.appToolName)) tools)
                case readTool.appToolApproval of
                    AlwaysReadOnly -> pure ()
                    _ -> expectationFailure "expected read-only approval"
                readTool.appToolExecution `shouldBe` ParallelSafe
                case mutateTool.appToolApproval of
                    AlwaysPrompt -> pure ()
                    _ -> expectationFailure "expected mutation approval"
                mutateTool.appToolExecution `shouldBe` TurnSequential
                case draftTool.appToolApproval of
                    AlwaysConfirm -> pure ()
                    _ -> expectationFailure "expected fresh approval"
                draftTool.appToolExecution `shouldBe` TurnSequential
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

    it "rejects a repeated catalog cursor during initialization" $
        withPaginationCycleServer \script ->
            bracket
                (startMcpClient (baseConfig "pagination-cycle" script))
                closeMcpClient
                \client -> do
                    result <- ensureMcpClientReady client
                    case result of
                        Left err ->
                            err `shouldSatisfy`
                                Text.isInfixOf "cursor cycle"
                        Right _ ->
                            expectationFailure
                                "expected cyclic pagination to fail"

    it "discovers and reads Skills over MCP without fetching content during listing" $
        withSkillsFakeServer \script -> do
            fleet <- startMcpFleet [baseConfig "skills" script]
            bracket (pure fleet) closeMcpFleet \_ -> do
                registrations <- mcpFleetSkillRegistrations fleet
                case registrations of
                    [McpSkillRegistration "skills" entry] -> do
                        entry.mcpSkillUri
                            `shouldBe` "skill://demo/SKILL.md"
                        entry.mcpSkillResources
                            `shouldBe`
                                McpSkillResourcesListed
                                    [ McpSkillResource
                                        "skill://demo/SKILL.md"
                                        "sha256:demo"
                                        42
                                    ]
                        mcpFleetGetSkill fleet "skills"
                            "skill://demo/SKILL.md"
                            `shouldReturn` Right entry
                        mcpFleetReadResource fleet "skills"
                            "skill://demo/SKILL.md"
                            `shouldReturn`
                                Right
                                    [ McpResourceContent
                                        "skill://demo/SKILL.md"
                                        (Just "text/markdown")
                                        (Just "---\nname: demo\n---\n")
                                        Nothing
                                    ]
                    other -> expectationFailure
                        ("unexpected skill registrations: " <> show other)

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

    it "starts other servers while a progress callback is stalled" $
        withConcurrentFakeServer \script barrier -> do
            started <- timeout 5000000 $
                startMcpFleetWithProgress
                    (\case
                        [_] -> waitForConcurrentStarts barrier
                        _ -> pure ())
                    [ concurrentConfig script barrier "first"
                    , concurrentConfig script barrier "second"
                    ]
            case started of
                Nothing ->
                    expectationFailure
                        "fleet startup deadlocked behind the progress callback"
                Just fleet ->
                    bracket (pure fleet) closeMcpFleet \_ -> pure ()

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
                        , McpServerStatus "healthy" McpReady 3
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
                    dispatch ident name arguments =
                        dispatchApprovedTool tools
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

    it "classifies progressive MCP calls using the selected tool annotation" $
        withFakeServer \script -> do
            fleet <- startMcpFleetProgressive
                (const (pure ()))
                [baseConfig "fake" script]
            bracket (pure fleet) closeMcpFleet \_ -> do
                waitForServerReady fleet "fake"
                let tools = mcpFleetMetaTools fleet
                Just callTool <-
                    pure (find ((== "mcp_call") . (.appToolName)) tools)
                toolAllowsWithoutPrompt callTool
                    (functionToolCall "read" "mcp_call"
                        "{\"name\":\"fake__echo_read\",\"arguments\":{}}")
                    `shouldReturn` True
                toolAllowsWithoutPrompt callTool
                    (functionToolCall "write" "mcp_call"
                        "{\"name\":\"fake__mutate\",\"arguments\":{}}")
                    `shouldReturn` False
                toolApprovalRequirement callTool
                    (functionToolCall "write" "mcp_call"
                        "{\"name\":\"fake__mutate\",\"arguments\":{}}")
                    `shouldReturn` ApprovalPromptRequired
                toolApprovalRequirement callTool
                    (functionToolCall "draft" "mcp_call"
                        "{\"name\":\"fake__draft\",\"arguments\":{}}")
                    `shouldReturn` FreshApprovalRequired
                toolApprovalRequirement callTool
                    (functionToolCall "invalid" "mcp_call" "{}")
                    `shouldReturn` ApprovalPromptRequired

                let grokTools = mcpFleetGrokMetaTools fleet
                Just useTool <-
                    pure (find ((== "use_tool") . (.appToolName)) grokTools)
                toolApprovalRequirement useTool
                    (functionToolCall "draft-grok" "use_tool"
                        "{\"tool_name\":\"fake__draft\",\"tool_input\":{}}")
                    `shouldReturn` FreshApprovalRequired
                toolApprovalRequirement useTool
                    (functionToolCall "write-grok" "use_tool"
                        "{\"tool_name\":\"fake__mutate\",\"tool_input\":{}}")
                    `shouldReturn` ApprovalPromptRequired

    it "binds mcp_call and Grok use_tool to the exact approval-time fingerprint" $
        withCountingServer policyCallServer \script callLog -> do
            fleet <- startMcpFleetProgressive
                (const (pure ()))
                [(baseConfig "policy" script) { mcpServerArgs = [callLog] }]
            bracket (pure fleet) closeMcpFleet \_ -> do
                waitForServerReady fleet "policy"
                assertCatalogEscalationBlocked
                    fleet
                    "policy__read"
                    (mcpFleetMetaTools fleet)
                    "mcp_call"
                    "{\"name\":\"policy__read\",\"arguments\":{}}"
                    True
                assertCatalogEscalationBlocked
                    fleet
                    "policy__read"
                    (mcpFleetGrokMetaTools fleet)
                    "use_tool"
                    "{\"tool_name\":\"policy__read\",\"tool_input\":{}}"
                    False
                Just callTool <-
                    pure (find
                        ((== "mcp_call") . (.appToolName))
                        (mcpFleetMetaTools fleet))
                let lateCall = functionToolCall "late-entry" "mcp_call"
                        "{\"name\":\"policy__read\",\"arguments\":{}}"
                Just removed <- atomically do
                    entries <- readTVar fleet.mcpFleetCatalog
                    writeTVar fleet.mcpFleetCatalog
                        (Map.delete "policy__read" entries)
                    pure (Map.lookup "policy__read" entries)
                toolApprovalRequirement callTool lateCall
                    `shouldReturn` ApprovalPromptRequired
                atomically $
                    modifyTVar' fleet.mcpFleetCatalog
                        (Map.insert "policy__read"
                            removed
                                { catalogGeneration =
                                    removed.catalogGeneration + 1
                                })
                lateResult <- dispatchToolCall
                    defaultLoopDispatch
                    (appToolHandlers [callTool])
                    lateCall
                lateResult.output `shouldSatisfy`
                    Text.isInfixOf "Unknown MCP tool"
                countLogEntries callLog "call" `shouldReturn` 0

    it "projects progressive MCP discovery through Grok search_tool and use_tool" $
        withDelayedFakeServer \script -> do
            fleet <- startMcpFleetProgressive
                (const (pure ()))
                [progressiveConfig script "0.02" "grok"]
            bracket (pure fleet) closeMcpFleet \_ -> do
                waitForServerReady fleet "grok"
                let tools = mcpFleetGrokMetaTools fleet
                    dispatch ident name arguments =
                        dispatchApprovedTool tools
                            (functionToolCall ident name arguments)
                map (.appToolName) tools
                    `shouldBe` ["search_tool", "use_tool"]
                searched <- dispatch "search-grok" "search_tool"
                    "{\"query\":\"delayed\",\"limit\":5}"
                searched.output `shouldSatisfy`
                    Text.isInfixOf "\"tool_name\":\"grok__delayed_read\""
                searched.output `shouldSatisfy`
                    Text.isInfixOf "\"status\":\"ready\""
                searched.output `shouldSatisfy`
                    Text.isInfixOf "\"total_hidden_tools\":1"
                searched.output `shouldSatisfy`
                    Text.isInfixOf "\"score\":"
                natural <- dispatch "search-natural" "search_tool"
                    "{\"query\":\"grok delayed read\"}"
                natural.output `shouldSatisfy`
                    Text.isInfixOf "\"tool_name\":\"grok__delayed_read\""
                invalidLimit <- dispatch "search-limit" "search_tool"
                    "{\"query\":\"delayed\",\"limit\":\"many\"}"
                invalidLimit.output `shouldSatisfy`
                    Text.isInfixOf "limit must be an integer"
                called <- dispatch "call-grok" "use_tool"
                    "{\"tool_name\":\"grok__delayed_read\",\"tool_input\":{}}"
                called.output `shouldBe` "delayed response"
                missingInput <- dispatch "missing-input" "use_tool"
                    "{\"tool_name\":\"grok__delayed_read\"}"
                missingInput.output `shouldSatisfy`
                    Text.isInfixOf "requires tool_input"
                unqualified <- dispatch "unqualified" "use_tool"
                    "{\"tool_name\":\"read_file\",\"tool_input\":{}}"
                unqualified.output `shouldSatisfy`
                    Text.isInfixOf "qualified server__tool name"

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


    describe "protocol negotiation" do
        it "classifies discovery probes by era" do
            classifyProbe (Left (McpRpcError (-32601) "Method not found" Nothing))
                `shouldBe` ProbeLegacy "error -32601"
            classifyProbe (Left (McpTimeout "no response"))
                `shouldBe` ProbeLegacy "no response"
            classifyProbe (Left (McpHttpStatus 404 ""))
                `shouldBe` ProbeLegacy "HTTP 404"
            classifyProbe
                (Left (McpRpcError (-32022) "Unsupported"
                    (Just (raw "{\"supported\":[\"2025-11-25\"]}"))))
                `shouldBe` ProbeVersions ["2025-11-25"]
            classifyProbe (Left (McpRpcError (-32020) "Header mismatch" Nothing))
                `shouldSatisfy` \case
                    ProbeFailure _ -> True
                    _ -> False
            classifyProbe (Left (McpTransportError "boom"))
                `shouldBe` ProbeFailure "boom"

    describe "Streamable HTTP headers" do
        it "encodes header values per the value-encoding rules" do
            encodeHeaderValue "us-west1" `shouldBe` "us-west1"
            encodeHeaderValue "Hello, 世界"
                `shouldBe` "=?base64?SGVsbG8sIOS4lueVjA==?="
            encodeHeaderValue " padded " `shouldBe` "=?base64?IHBhZGRlZCA=?="
            encodeHeaderValue "line1\nline2"
                `shouldBe` "=?base64?bGluZTEKbGluZTI=?="
            encodeHeaderValue "=?base64?literal?="
                `shouldBe` "=?base64?PT9iYXNlNjQ/bGl0ZXJhbD89?="

        it "accepts statically reachable x-mcp-header annotations and rejects the rest" do
            let region =
                    "region" .= object
                        [ "type" .= ("string" :: Text.Text)
                        , "x-mcp-header" .= ("Region" :: Text.Text)
                        ]
            fmap (.discoveredHeaderParams)
                (annotateHeaderParams True (schemaTool [region]))
                `shouldBe` Right [McpHeaderParam ["region"] "Region"]
            fmap (.discoveredHeaderParams)
                (annotateHeaderParams False (schemaTool [region]))
                `shouldBe` Right []
            isLeft (annotateHeaderParams True
                (schemaTool
                    [ "choice" .= object
                        [ "oneOf" .=
                            [ object
                                [ "type" .= ("string" :: Text.Text)
                                , "x-mcp-header" .= ("A" :: Text.Text)
                                ]
                            ]
                        ]
                    ]))
                `shouldBe` True
            isLeft (annotateHeaderParams True
                (schemaTool
                    [ "ratio" .= object
                        [ "type" .= ("number" :: Text.Text)
                        , "x-mcp-header" .= ("Ratio" :: Text.Text)
                        ]
                    ]))
                `shouldBe` True
            isLeft (annotateHeaderParams True
                (schemaTool
                    [ region
                    , "other" .= object
                        [ "type" .= ("string" :: Text.Text)
                        , "x-mcp-header" .= ("region" :: Text.Text)
                        ]
                    ]))
                `shouldBe` True
            case annotateHeaderParams True (schemaTool [region]) of
                Left err -> expectationFailure (Text.unpack err)
                Right annotated ->
                    headerParamValues annotated
                        (raw "{\"region\":\"us-west1\",\"query\":\"SELECT 1\"}")
                        `shouldBe` [("Mcp-Param-Region", "us-west1")]

        it "splits SSE buffers into complete lines" do
            splitLines "data: a\r\n\r\ndata: b"
                `shouldBe` (["data: a", ""], "data: b")

        it "accepts an HTTP body exactly at the byte limit" do
            reader <- scriptedBodyReader
                [BS.replicate (16 * 1024 * 1024) 97]
            readBounded reader
                `shouldReturn` Right (BS.replicate (16 * 1024 * 1024) 97)

        it "rejects an HTTP body one byte over the limit" do
            reader <- scriptedBodyReader
                [BS.replicate (16 * 1024 * 1024) 97, "b"]
            result <- readBounded reader
            result `shouldSatisfy` isLeft

        it "accepts large reader chunks made of bounded SSE lines" do
            let chunk = BS.concat
                    (replicate 70000 "data: xxxxxxxx\n")
            splitSseChunk "" chunk `shouldSatisfy` \case
                Right (lines_, rest) ->
                    length lines_ == 70000 && BS.null rest
                Left _ -> False

        it "rejects an oversized unterminated SSE line" do
            splitSseChunk
                (BS.replicate (16 * 1024 * 1024) 97)
                "b"
                `shouldSatisfy` isLeft

        it "strips CR from complete SSE lines across chunks" do
            splitSseChunk "data: a" "\r\n\r\ndata: b"
                `shouldBe` Right (["data: a", ""], "data: b")

    it "renders resource links and binary content blocks" do
        normalizeMcpToolResult
            (raw "{\"content\":[{\"type\":\"resource_link\",\"uri\":\"file:///a.rs\",\"name\":\"a.rs\",\"mimeType\":\"text/x-rust\"},{\"type\":\"image\",\"data\":\"AAAA\",\"mimeType\":\"image/png\"}]}")
            `shouldBe`
                Right "[resource_link] file:///a.rs (a.rs) [text/x-rust]\n[image image/png, 4 base64 bytes; binary content is not shown]"

    it "drives a modern server through discovery, elicitation, subscriptions, and tasks" $
        withCountingServer modernFakeServer \script log -> do
            elicited <- newIORef []
            let hooks = defaultMcpHostHooks
                    { mcpHostElicit = pure $ Just \request -> do
                        modifyIORef' elicited (<> [request])
                        pure (McpElicitAccept (Just (raw "{\"name\":\"octocat\"}")))
                    }
            fleet <- startMcpFleetWithProgressHooks hooks (const (pure ()))
                [(baseConfig "modern" script) { mcpServerArgs = [log] }]
            bracket (pure fleet) closeMcpFleet \_ -> do
                fleet.mcpFleetWarnings `shouldBe` []
                map (.appToolName) (mcpFleetTools fleet)
                    `shouldBe` ["modern__greet", "modern__slow_task"]
                map (.appToolDescription) (mcpFleetTools fleet)
                    `shouldBe` ["Greeter: Greets.", "Slow."]
                mcpFleetInstructions fleet `shouldReturn` [("modern", "Be nice.")]
                infos <- mcpFleetServerInfos fleet
                map (\(name, info) -> (name, info.serverInfoEra, info.serverInfoName)) infos
                    `shouldBe` [("modern", McpEraModern, Just "modern")]
                greet <- callFleetTool fleet "modern__greet" "{}"
                greet.output `shouldBe` "hello octocat"
                requests <- readIORef elicited
                map (\request -> (request.elicitServerName, request.elicitMessage)) requests
                    `shouldBe` [("modern", "Your name?")]
                task <- callFleetTool fleet "modern__slow_task" "{}"
                task.output `shouldBe` "task done"
                waitForLog log "listen"

    it "does not reissue a fresh direct tool after input_required" $
        withCountingServer freshInputServer \script callLog -> do
            elicited <- newIORef (0 :: Int)
            let hooks = defaultMcpHostHooks
                    { mcpHostElicit = pure $ Just \_ -> do
                        modifyIORef' elicited (+ 1)
                        pure McpElicitCancel
                    }
            fleet <- startMcpFleetWithProgressHooks hooks (const (pure ()))
                [ (baseConfig "fresh-input" script)
                    { mcpServerArgs = [callLog]
                    }
                ]
            bracket (pure fleet) closeMcpFleet \_ -> do
                result <- callFleetTool fleet "fresh-input__draft" "{}"
                result.output `shouldSatisfy`
                    Text.isInfixOf "requires fresh approval"
                countLogEntries callLog "call" `shouldReturn` 1
                readIORef elicited `shouldReturn` 0

    it "does not continue a fresh task after input_required" $
        withCountingServer freshTaskInputServer \script callLog -> do
            elicited <- newIORef (0 :: Int)
            let hooks = defaultMcpHostHooks
                    { mcpHostElicit = pure $ Just \_ -> do
                        modifyIORef' elicited (+ 1)
                        pure McpElicitCancel
                    }
            fleet <- startMcpFleetWithProgressHooks hooks (const (pure ()))
                [ (baseConfig "fresh-task-input" script)
                    { mcpServerArgs = [callLog]
                    }
                ]
            bracket (pure fleet) closeMcpFleet \_ -> do
                result <-
                    callFleetTool fleet "fresh-task-input__draft" "{}"
                result.output `shouldSatisfy`
                    Text.isInfixOf "requires fresh approval"
                countLogEntries callLog "call" `shouldReturn` 1
                countLogEntries callLog "update" `shouldReturn` 0
                readIORef elicited `shouldReturn` 0

    it "answers pings, extends timeouts on progress, refreshes changed tool lists, and cancels timeouts" $
        withCountingServer legacyEventsServer \script log -> do
            fleet <- startMcpFleet
                [ (baseConfig "events" script)
                    { mcpServerArgs = [log]
                    , mcpServerRequestTimeoutSeconds = 1
                    }
                ]
            bracket (pure fleet) closeMcpFleet \_ -> do
                fleet.mcpFleetWarnings `shouldBe` []
                let tools = mcpFleetTools fleet
                Just slow <- pure (find ((== "events__slow") . (.appToolName)) tools)
                Just mutable <-
                    pure
                        (find
                            ((== "events__mutable") . (.appToolName))
                            tools)
                slow.appToolDescription `shouldBe` "pong=1"
                case mutable.appToolApproval of
                    AlwaysReadOnly -> pure ()
                    _ -> expectationFailure
                        "expected initial mutable tool to be read-only"
                infos <- mcpFleetServerInfos fleet
                map (\(_, info) -> (info.serverInfoEra, info.serverInfoProtocolVersion)) infos
                    `shouldBe` [(McpEraLegacy, "2025-11-25")]
                result <- callFleetTool fleet "events__slow" "{}"
                result.output `shouldBe` "slow done"
                waitForCatalogEntry fleet "events__late"
                changed <-
                    dispatchToolCall
                        defaultLoopDispatch
                        (appToolHandlers tools)
                        (functionToolCall "changed" "events__mutable" "{}")
                changed.output `shouldSatisfy`
                    Text.isInfixOf "changed after registration"
                countLogEntries log "mutable-call" `shouldReturn` 0
                hung <- callFleetTool fleet "events__hang" "{}"
                hung.output `shouldSatisfy` Text.isInfixOf "timed out"
                waitForLog log "cancelled"

    it "re-lists a tool invalidated during blocking startup before publishing a static handler" $
        withCountingServer initializationListChangedServer \script log -> do
            fleet <- startMcpFleet
                [ (baseConfig "init-change" script)
                    { mcpServerArgs = [log]
                    }
                ]
            bracket (pure fleet) closeMcpFleet \_ -> do
                countLogEntries log "list" `shouldReturn` 2
                let tools = mcpFleetTools fleet
                Just draft <-
                    pure
                        (find
                            ((== "init-change__draft") . (.appToolName))
                            tools)
                case draft.appToolApproval of
                    AlwaysConfirm -> pure ()
                    _ -> expectationFailure
                        "expected the replacement draft to require fresh approval"
                toolApprovalRequirement
                    draft
                    (functionToolCall "init-change-call" "init-change__draft" "{}")
                    `shouldReturn` FreshApprovalRequired
                countLogEntries log "call" `shouldReturn` 0

    it "replays an invalidation received after ready but before fleet event attachment" $
        withCountingServer preAttachListChangedServer \script log ->
            withCountingFakeServer \slowScript slowCounter -> do
                fleet <- startMcpFleet
                    [ (baseConfig "pre-attach" script)
                        { mcpServerArgs = [log]
                        }
                    , (baseConfig "slow-peer" slowScript)
                        { mcpServerArgs = [slowCounter, "0.4"]
                        }
                    ]
                bracket (pure fleet) closeMcpFleet \_ -> do
                    waitForLog log "replacement-list"
                    let tools = mcpFleetTools fleet
                    Just stale <-
                        pure
                            (find
                                ((== "pre-attach__draft") . (.appToolName))
                                tools)
                    toolApprovalRequirement
                        stale
                        (functionToolCall
                            "pre-attach-stale"
                            "pre-attach__draft"
                            "{}")
                        `shouldReturn` ApprovalNotRequired
                    attempted <-
                        callFleetTool fleet "pre-attach__draft" "{}"
                    attempted.output `shouldSatisfy`
                        Text.isInfixOf "changed after registration"
                    countLogEntries log "call" `shouldReturn` 0

    it "re-lists a tool invalidated while progressive initialization is publishing" $
        withCountingServer initializationListChangedServer \script log -> do
            fleet <- startMcpFleetProgressive
                (const (pure ()))
                [ (baseConfig "init-progressive" script)
                    { mcpServerArgs = [log]
                    }
                ]
            bracket (pure fleet) closeMcpFleet \_ -> do
                waitForServerReady fleet "init-progressive"
                countLogEntries log "list" `shouldReturn` 2
                let tools = mcpFleetMetaTools fleet
                    call =
                        functionToolCall
                            "progressive-init-change"
                            "mcp_call"
                            "{\"name\":\"init-progressive__draft\",\"arguments\":{}}"
                Just meta <-
                    pure (find ((== "mcp_call") . (.appToolName)) tools)
                toolApprovalRequirement meta call
                    `shouldReturn` FreshApprovalRequired
                countLogEntries log "call" `shouldReturn` 0

    describe "McpSupervisor" do
        it "single-flights concurrent acquisitions for the same configuration" $
            withCountingFakeServer \script counter -> do
                supervisor <- newMcpSupervisor
                bracket (pure supervisor) closeMcpSupervisor \_ -> do
                    let config =
                            (baseConfig "shared" script)
                                { mcpServerArgs = [counter, "0.2"]
                                }
                    withAsync (acquireMcpFleet supervisor [config]) \first ->
                        withAsync (acquireMcpFleet supervisor [config]) \second -> do
                            firstLease <- wait first
                            secondLease <- wait second
                            releaseMcpFleetLease firstLease
                            releaseMcpFleetLease secondLease
                    countStarts counter `shouldReturn` 1

        it "reuses an idle fleet with the same canonical configuration" $
            withCountingFakeServer \script counter -> do
                supervisor <- newMcpSupervisor
                bracket (pure supervisor) closeMcpSupervisor \_ -> do
                    let config =
                            (baseConfig "cached" script)
                                { mcpServerArgs = [counter]
                                , mcpServerEnv =
                                    [("SECOND", "2"), ("FIRST", "1")]
                                }
                    first <- acquireMcpFleet supervisor [config]
                    releaseMcpFleetLease first
                    second <- acquireMcpFleet supervisor
                        [ config
                            { mcpServerEnv =
                                [("FIRST", "1"), ("SECOND", "2")]
                            }
                        ]
                    releaseMcpFleetLease second
                    countStarts counter `shouldReturn` 1

        it "starts a replacement fleet when the configuration changes" $
            withCountingFakeServer \script counter -> do
                supervisor <- newMcpSupervisor
                bracket (pure supervisor) closeMcpSupervisor \_ -> do
                    first <- acquireMcpFleet supervisor
                        [ (baseConfig "cached" script)
                            { mcpServerArgs = [counter, "first"]
                            }
                        ]
                    releaseMcpFleetLease first
                    second <- acquireMcpFleet supervisor
                        [ (baseConfig "cached" script)
                            { mcpServerArgs = [counter, "second"]
                            }
                        ]
                    releaseMcpFleetLease second
                    countStarts counter `shouldReturn` 2

        it "discards an idle fleet when restarted" $
            withCountingFakeServer \script counter -> do
                supervisor <- newMcpSupervisor
                bracket (pure supervisor) closeMcpSupervisor \_ -> do
                    let config =
                            (baseConfig "restart" script)
                                { mcpServerArgs = [counter]
                                }
                    first <- acquireMcpFleet supervisor [config]
                    releaseMcpFleetLease first
                    restartMcpSupervisor supervisor
                    second <- acquireMcpFleet supervisor [config]
                    releaseMcpFleetLease second
                    countStarts counter `shouldReturn` 2

        it "does not reuse an inherited-cwd fleet after the cwd changes" $
            withCountingFakeServer \script counter ->
                withDistinctWorkingDirectories \firstDir secondDir -> do
                    supervisor <- newMcpSupervisor
                    bracket (pure supervisor) closeMcpSupervisor \_ -> do
                        let config =
                                (baseConfig "cwd-sensitive" script)
                                    { mcpServerArgs = [counter]
                                    }
                        first <- withCurrentDirectory firstDir $
                            acquireMcpFleet supervisor [config]
                        releaseMcpFleetLease first
                        second <- withCurrentDirectory secondDir $
                            acquireMcpFleet supervisor [config]
                        releaseMcpFleetLease second
                        countStarts counter `shouldReturn` 2

        it "cancels and joins a pending startup when the supervisor closes" $
            withCountingFakeServer \script counter -> do
                supervisor <- newMcpSupervisor
                acquiring <- async $
                    acquireMcpFleet supervisor
                        [ (baseConfig "slow-close" script)
                            { mcpServerArgs = [counter, "2"]
                            }
                        ]
                threadDelay 50000
                timeout 1000000 (closeMcpSupervisor supervisor)
                    `shouldReturn` Just ()
                _ <- waitCatch acquiring
                pure ()

        it "reconnects once and retries a meta-tool call after transport loss" $
            withCountingReconnectServer \script counter -> do
                fleet <- startMcpFleetProgressive
                    (const (pure ()))
                    [ (baseConfig "reconnect" script)
                        { mcpServerArgs = [counter]
                        }
                    ]
                bracket (pure fleet) closeMcpFleet \_ -> do
                    waitForServerReady fleet "reconnect"
                    let tools = mcpFleetMetaTools fleet
                    called <- dispatchApprovedTool tools
                        (functionToolCall "reconnect-call" "mcp_call"
                            "{\"name\":\"reconnect__read\",\"arguments\":{}}")
                    called.output `shouldBe` "reconnected response"
                    countStarts counter `shouldReturn` 2

        it "does not execute a reconnect replacement with a changed fingerprint" $
            withEscalatingReconnectServer \script log -> do
                fleet <- startMcpFleetProgressive
                    (const (pure ()))
                    [ (baseConfig "escalating" script)
                        { mcpServerArgs = [log]
                        }
                    ]
                bracket (pure fleet) closeMcpFleet \_ -> do
                    waitForServerReady fleet "escalating"
                    let tools = mcpFleetMetaTools fleet
                    called <- dispatchApprovedTool tools
                        (functionToolCall "escalating-call" "mcp_call"
                            "{\"name\":\"escalating__read\",\"arguments\":{}}")
                    called.output `shouldSatisfy`
                        Text.isInfixOf "changed after approval"
                    countLogEntries log "started" `shouldReturn` 2
                    countLogEntries log "initial-call" `shouldReturn` 1
                    countLogEntries log "replacement-call" `shouldReturn` 0

        it "does not lose a tool invalidation while initializing a reconnect" $
            withReconnectListChangedServer \script log -> do
                fleet <- startMcpFleetProgressive
                    (const (pure ()))
                    [ (baseConfig "reconnect-change" script)
                        { mcpServerArgs = [log]
                        }
                    ]
                bracket (pure fleet) closeMcpFleet \_ -> do
                    waitForServerReady fleet "reconnect-change"
                    let tools = mcpFleetMetaTools fleet
                    called <- dispatchApprovedTool tools
                        (functionToolCall "reconnect-change-call" "mcp_call"
                            "{\"name\":\"reconnect-change__draft\",\"arguments\":{}}")
                    called.output `shouldSatisfy`
                        Text.isInfixOf "changed after approval"
                    countLogEntries log "started" `shouldReturn` 2
                    countLogEntries log "replacement-list" `shouldReturn` 2
                    countLogEntries log "initial-call" `shouldReturn` 1
                    countLogEntries log "replacement-call" `shouldReturn` 0

        it "applies the reconnect fingerprint guard to Grok use_tool" $
            withEscalatingReconnectServer \script log -> do
                fleet <- startMcpFleetProgressive
                    (const (pure ()))
                    [ (baseConfig "escalating" script)
                        { mcpServerArgs = [log]
                        }
                    ]
                bracket (pure fleet) closeMcpFleet \_ -> do
                    waitForServerReady fleet "escalating"
                    let tools = mcpFleetGrokMetaTools fleet
                    called <- dispatchApprovedTool tools
                        (functionToolCall "escalating-grok" "use_tool"
                            "{\"tool_name\":\"escalating__read\",\"tool_input\":{}}")
                    called.output `shouldSatisfy`
                        Text.isInfixOf "changed after approval"
                    countLogEntries log "started" `shouldReturn` 2
                    countLogEntries log "initial-call" `shouldReturn` 1
                    countLogEntries log "replacement-call" `shouldReturn` 0

        it "does not reconnect and retry a mutation after transport loss" $
            withCountingMutationServer \script counter -> do
                fleet <- startMcpFleetProgressive
                    (const (pure ()))
                    [ (baseConfig "mutation" script)
                        { mcpServerArgs = [counter]
                        }
                    ]
                bracket (pure fleet) closeMcpFleet \_ -> do
                    waitForServerReady fleet "mutation"
                    let tools = mcpFleetMetaTools fleet
                    _ <- dispatchApprovedTool tools
                        (functionToolCall "mutation-call" "mcp_call"
                            "{\"name\":\"mutation__write\",\"arguments\":{}}")
                    countStarts counter `shouldReturn` 1

        it "rebuilds an idle fleet whose transport failed" $
            withCountingFailingServer \script counter -> do
                supervisor <- newMcpSupervisor
                bracket (pure supervisor) closeMcpSupervisor \_ -> do
                    let config =
                            (baseConfig "recover" script)
                                { mcpServerArgs = [counter]
                                }
                    first <- acquireMcpFleet supervisor [config]
                    waitForServerFailure first.mcpLeaseFleet "recover"
                    releaseMcpFleetLease first
                    second <- acquireMcpFleet supervisor [config]
                    releaseMcpFleetLease second
                    countStarts counter `shouldReturn` 2

raw :: BS8.ByteString -> RawJson
raw bytes =
    either (error . show) id (Json.decodeEither rawJsonDecoder bytes)

schemaTool :: [Data.Aeson.Types.Pair] -> McpTool
schemaTool properties = McpTool
    { discoveredName = "execute_sql"
    , discoveredTitle = Nothing
    , discoveredDescription = ""
    , discoveredInputSchema =
        rawJsonFromEncoding . Data.Aeson.toEncoding $ object
            [ "type" .= ("object" :: Text.Text)
            , "properties" .= object properties
            ]
    , discoveredOutputSchema = Nothing
    , discoveredReadOnly = False
    , discoveredRequiresFreshApproval = False
    , discoveredDestructive = True
    , discoveredIdempotent = False
    , discoveredOpenWorld = True
    , discoveredHeaderParams = []
    }

callFleetTool :: McpFleet -> Text.Text -> Text.Text -> IO ToolCallResult
callFleetTool fleet name arguments =
    dispatchToolCall
        defaultLoopDispatch
        (appToolHandlers (mcpFleetTools fleet))
        (functionToolCall "call" name arguments)

dispatchApprovedTool :: [AppTool] -> ToolCall -> IO ToolCallResult
dispatchApprovedTool tools call = do
    case find ((== call.name) . (.appToolName)) tools of
        Nothing -> pure ()
        Just tool -> do
            _ <- toolApprovalRequirement tool call
            pure ()
    dispatchToolCall
        defaultLoopDispatch
        (appToolHandlers tools)
        call

assertCatalogEscalationBlocked
    :: McpFleet
    -> Text.Text
    -> [AppTool]
    -> Text.Text
    -> Text.Text
    -> Bool
    -> Expectation
assertCatalogEscalationBlocked
        fleet catalogName tools metaToolName arguments escalate = do
    atomically do
        entries <- readTVar fleet.mcpFleetCatalog
        case Map.lookup catalogName entries of
            Nothing -> pure ()
            Just entry ->
                writeTVar fleet.mcpFleetCatalog $
                    Map.insert catalogName
                        entry
                            { catalogTool =
                                entry.catalogTool
                                    { discoveredReadOnly = True
                                    , discoveredRequiresFreshApproval = False
                                    }
                            , catalogGeneration =
                                entry.catalogGeneration + 1
                            }
                        entries
    Just metaTool <-
        pure (find ((== metaToolName) . (.appToolName)) tools)
    let call = functionToolCall
            ("escalation-" <> metaToolName)
            metaToolName
            arguments
    toolApprovalRequirement metaTool call
        `shouldReturn` ApprovalNotRequired
    atomically do
        entries <- readTVar fleet.mcpFleetCatalog
        case Map.lookup catalogName entries of
            Nothing -> pure ()
            Just entry ->
                writeTVar fleet.mcpFleetCatalog $
                    Map.insert catalogName
                        entry
                            { catalogTool =
                                entry.catalogTool
                                    { discoveredRequiresFreshApproval =
                                        escalate
                                    , discoveredDescription =
                                        if escalate
                                            then
                                                entry.catalogTool.discoveredDescription
                                            else "same policy, changed metadata"
                                    }
                            , catalogGeneration =
                                entry.catalogGeneration + 1
                            }
                        entries
    result <- dispatchToolCall
        defaultLoopDispatch
        (appToolHandlers tools)
        call
    result.output `shouldSatisfy`
        Text.isInfixOf "changed after approval"

waitForLog :: FilePath -> String -> IO ()
waitForLog path needle = go (300 :: Int)
  where
    go 0 = expectationFailure ("log never contained " <> needle)
    go remaining = do
        contents <- readFile path
        if needle `isInfixOf` contents
            then pure ()
            else threadDelay 10000 >> go (remaining - 1)

waitForCatalogEntry :: McpFleet -> Text.Text -> IO ()
waitForCatalogEntry fleet name = go (300 :: Int)
  where
    go 0 = expectationFailure ("catalog never contained " <> Text.unpack name)
    go remaining = do
        entries <- readTVarIO fleet.mcpFleetCatalog
        if Map.member name entries
            then pure ()
            else threadDelay 10000 >> go (remaining - 1)

waitForConcurrentStarts :: FilePath -> IO ()
waitForConcurrentStarts barrier = go (300 :: Int)
  where
    go 0 =
        expectationFailure
            "other MCP servers did not start while progress reporting was blocked"
    go remaining = do
        starts <- listDirectory barrier
        if length starts >= 2
            then pure ()
            else threadDelay 10000 >> go (remaining - 1)

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

withPaginationCycleServer :: (FilePath -> IO a) -> IO a
withPaginationCycleServer action = do
    temporary <- getTemporaryDirectory
    bracket
        (do
            (path, handle) <-
                openTempFile temporary "agent-mcp-pagination-cycle.sh"
            LBS.hPutStr handle paginationCycleServer
            hClose handle
            setFileMode path 0o700
            pure path)
        removeFile
        action

withSkillsFakeServer :: (FilePath -> IO a) -> IO a
withSkillsFakeServer action = do
    temporary <- getTemporaryDirectory
    bracket
        (do
            (path, handle) <- openTempFile temporary "agent-mcp-skills.sh"
            LBS.hPutStr handle skillsFakeServer
            hClose handle
            setFileMode path 0o700
            pure path)
        removeFile
        action

withCountingFakeServer :: (FilePath -> FilePath -> IO a) -> IO a
withCountingFakeServer = withCountingServer countingFakeServer

withCountingFailingServer :: (FilePath -> FilePath -> IO a) -> IO a
withCountingFailingServer = withCountingServer countingFailingServer

withCountingReconnectServer :: (FilePath -> FilePath -> IO a) -> IO a
withCountingReconnectServer = withCountingServer countingReconnectServer

withEscalatingReconnectServer :: (FilePath -> FilePath -> IO a) -> IO a
withEscalatingReconnectServer =
    withCountingServer escalatingReconnectServer

withReconnectListChangedServer :: (FilePath -> FilePath -> IO a) -> IO a
withReconnectListChangedServer =
    withCountingServer reconnectListChangedServer

withCountingMutationServer :: (FilePath -> FilePath -> IO a) -> IO a
withCountingMutationServer = withCountingServer countingMutationServer

withCountingServer
    :: LBS.ByteString
    -> (FilePath -> FilePath -> IO a)
    -> IO a
withCountingServer body action = do
    temporary <- getTemporaryDirectory
    bracket
        (do
            (counter, counterHandle) <-
                openTempFile temporary "agent-mcp-start-count"
            hClose counterHandle
            (script, scriptHandle) <-
                openTempFile temporary "agent-mcp-counting.sh"
            LBS.hPutStr scriptHandle body
            hClose scriptHandle
            setFileMode script 0o700
            pure (script, counter))
        (\(script, counter) -> do
            removeFile script
            removeFile counter)
        (uncurry action)

withDistinctWorkingDirectories :: (FilePath -> FilePath -> IO a) -> IO a
withDistinctWorkingDirectories action = do
    temporary <- getTemporaryDirectory
    bracket
        (do
            (first, firstHandle) <-
                openTempFile temporary "agent-mcp-cwd-first"
            hClose firstHandle
            removeFile first
            createDirectory first
            (second, secondHandle) <-
                openTempFile temporary "agent-mcp-cwd-second"
            hClose secondHandle
            removeFile second
            createDirectory second
            pure (first, second))
        (\(first, second) -> do
            removeDirectoryRecursive first
            removeDirectoryRecursive second)
        (uncurry action)

countStarts :: FilePath -> IO Int
countStarts path = length . lines <$> readFile path

countLogEntries :: FilePath -> String -> IO Int
countLogEntries path entry =
    length . filter (== entry) . lines <$> readFile path

concurrentConfig :: FilePath -> FilePath -> Text.Text -> McpServerConfig
concurrentConfig script barrier name = McpServerConfig
    { mcpServerName = name
    , mcpServerUrl = Nothing
    , mcpServerCommand = script
    , mcpServerArgs = [barrier, Text.unpack name]
    , mcpServerCwd = Nothing
    , mcpServerEnv = []
    , mcpServerStartupTimeoutSeconds = 2
    , mcpServerRequestTimeoutSeconds = 2
    , mcpServerProtocol = McpProtocolAuto
    }

data WorkerLifecycleOperation
    = SpawnWorker
    | CloseWorkerClient
    deriving (Eq, Show)

newtype WorkerLifecycle = WorkerLifecycle [WorkerLifecycleOperation]
    deriving (Show)

instance Arbitrary WorkerLifecycle where
    arbitrary =
        WorkerLifecycle
            <$> resize 20 (listOf (elements [SpawnWorker, CloseWorkerClient]))
    shrink (WorkerLifecycle operations) =
        WorkerLifecycle <$> shrinkList (const []) operations

applyWorkerOperation
    :: McpClient
    -> TMVar ()
    -> WorkerLifecycleOperation
    -> IO ()
applyWorkerOperation client gate = \case
    SpawnWorker -> spawnClientWorker client (atomically (readTMVar gate))
    CloseWorkerClient -> closeMcpClient client

workerClientConfig :: McpServerConfig
workerClientConfig =
    (baseConfig "worker-lifecycle" "/unused")
        { mcpServerUrl = Just "http://127.0.0.1:1/mcp"
        }

progressiveConfig :: FilePath -> String -> Text.Text -> McpServerConfig
progressiveConfig script delay name =
    (baseConfig name script)
        { mcpServerArgs = [delay]
        }

baseConfig :: Text.Text -> FilePath -> McpServerConfig
baseConfig name command = McpServerConfig
    { mcpServerName = name
    , mcpServerUrl = Nothing
    , mcpServerCommand = command
    , mcpServerArgs = []
    , mcpServerCwd = Nothing
    , mcpServerEnv = []
    , mcpServerStartupTimeoutSeconds = 2
    , mcpServerRequestTimeoutSeconds = 2
    , mcpServerProtocol = McpProtocolAuto
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
    \  id=$(printf '%s' \"$line\" | sed -n 's/.*\"id\":\\([0-9][0-9]*\\).*/\\1/p')\n\
    \  case \"$line\" in\n\
    \    *'\"method\":\"server/discover\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"error\":{\"code\":-32601,\"message\":\"Method not found\"}}'\n\
    \      ;;\n\
    \    *'\"method\":\"initialize\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},\"serverInfo\":{\"name\":\"fake\",\"version\":\"1\"}}}'\n\
    \      ;;\n\
    \    *'\"method\":\"notifications/initialized\"'*) ;;\n\
    \    *'\"method\":\"tools/list\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"tools\":[{\"name\":\"echo_read\",\"description\":\"Echo.\",\"inputSchema\":{\"type\":\"object\",\"properties\":{\"message\":{\"type\":\"string\"}},\"required\":[\"message\"],\"additionalProperties\":false},\"annotations\":{\"readOnlyHint\":true}},{\"name\":\"mutate\",\"description\":\"Write.\",\"inputSchema\":{\"type\":\"object\"}},{\"name\":\"draft\",\"description\":\"Draft.\",\"inputSchema\":{\"type\":\"object\"},\"_meta\":{\"dev.haskell-agent/fresh-approval\":true}}]}}'\n\
    \      ;;\n\
    \    *'\"method\":\"tools/call\"'*)\n\
    \      if [ -z \"$first_call_seen\" ]; then\n\
    \        first_call_seen=1\n\
    \        first_id=$id\n\
    \      else\n\
    \        printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"second response\"}]}}'\n\
    \        printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$first_id\"',\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"first response\"}]}}'\n\
    \      fi\n\
    \      ;;\n\
    \  esac\n\
    \done\n"

paginationCycleServer :: LBS.ByteString
paginationCycleServer =
    "#!/bin/sh\n\
    \while IFS= read -r line; do\n\
    \  id=$(printf '%s' \"$line\" | sed -n 's/.*\"id\":\\([0-9][0-9]*\\).*/\\1/p')\n\
    \  case \"$line\" in\n\
    \    *'\"method\":\"server/discover\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"error\":{\"code\":-32601,\"message\":\"Method not found\"}}'\n\
    \      ;;\n\
    \    *'\"method\":\"initialize\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},\"serverInfo\":{\"name\":\"pagination-cycle\",\"version\":\"1\"}}}'\n\
    \      ;;\n\
    \    *'\"method\":\"notifications/initialized\"'*) ;;\n\
    \    *'\"method\":\"tools/list\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"tools\":[],\"nextCursor\":\"same\"}}'\n\
    \      ;;\n\
    \  esac\n\
    \done\n"

scriptedBodyReader :: [BS.ByteString] -> IO (IO BS.ByteString)
scriptedBodyReader chunks = do
    remaining <- newIORef chunks
    pure $ atomicModifyIORef' remaining \case
        [] -> ([], BS.empty)
        chunk : rest -> (rest, chunk)

skillsFakeServer :: LBS.ByteString
skillsFakeServer =
    "#!/bin/sh\n\
    \while IFS= read -r line; do\n\
    \  id=$(printf '%s' \"$line\" | sed -n 's/.*\"id\":\\([0-9][0-9]*\\).*/\\1/p')\n\
    \  case \"$line\" in\n\
    \    *'\"method\":\"server/discover\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"resultType\":\"complete\",\"supportedVersions\":[\"2026-07-28\"],\"capabilities\":{\"tools\":{},\"extensions\":{\"io.modelcontextprotocol/skills\":{}}},\"instructions\":\"Use the demo skill.\",\"_meta\":{\"io.modelcontextprotocol/serverInfo\":{\"name\":\"skills\",\"version\":\"1\"}}}}'\n\
    \      ;;\n\
    \    *'\"method\":\"initialize\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"error\":{\"code\":-32601,\"message\":\"initialize is not supported; use server/discover\"}}'\n\
    \      ;;\n\
    \    *'\"method\":\"tools/list\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"tools\":[]}}'\n\
    \      ;;\n\
    \    *'\"method\":\"skills/list\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"resultType\":\"complete\",\"skills\":[{\"uri\":\"skill://demo/SKILL.md\",\"frontmatter\":{\"name\":\"demo\",\"description\":\"Demo skill\"},\"resources\":[{\"uri\":\"skill://demo/SKILL.md\",\"digest\":\"sha256:demo\",\"size\":42}]}]}}'\n\
    \      ;;\n\
    \    *'\"method\":\"skills/get\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"resultType\":\"complete\",\"skill\":{\"uri\":\"skill://demo/SKILL.md\",\"frontmatter\":{\"name\":\"demo\",\"description\":\"Demo skill\"},\"resources\":[{\"uri\":\"skill://demo/SKILL.md\",\"digest\":\"sha256:demo\",\"size\":42}]}}}'\n\
    \      ;;\n\
    \    *'\"method\":\"resources/read\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"contents\":[{\"uri\":\"skill://demo/SKILL.md\",\"mimeType\":\"text/markdown\",\"text\":\"---\\nname: demo\\n---\\n\"}]}}'\n\
    \      ;;\n\
    \  esac\n\
    \done\n"

policyCallServer :: LBS.ByteString
policyCallServer =
    "#!/bin/sh\n\
    \log=\"$1\"\n\
    \while IFS= read -r line; do\n\
    \  id=$(printf '%s' \"$line\" | sed -n 's/.*\"id\":\\([0-9][0-9]*\\).*/\\1/p')\n\
    \  case \"$line\" in\n\
    \    *'\"method\":\"server/discover\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"error\":{\"code\":-32601,\"message\":\"Method not found\"}}'\n\
    \      ;;\n\
    \    *'\"method\":\"initialize\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},\"serverInfo\":{\"name\":\"policy\",\"version\":\"1\"}}}'\n\
    \      ;;\n\
    \    *'\"method\":\"notifications/initialized\"'*) ;;\n\
    \    *'\"method\":\"tools/list\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"tools\":[{\"name\":\"read\",\"description\":\"Read.\",\"inputSchema\":{\"type\":\"object\"},\"annotations\":{\"readOnlyHint\":true}}]}}'\n\
    \      ;;\n\
    \    *'\"method\":\"tools/call\"'*)\n\
    \      printf 'call\\n' >> \"$log\"\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"called\"}]}}'\n\
    \      ;;\n\
    \  esac\n\
    \done\n"

countingReconnectServer :: LBS.ByteString
countingReconnectServer =
    "#!/bin/sh\n\
    \counter=\"$1\"\n\
    \printf 'started\\n' >> \"$counter\"\n\
    \instance=\"$(wc -l < \"$counter\" | tr -d ' ')\"\n\
    \while IFS= read -r line; do\n\
    \  id=$(printf '%s' \"$line\" | sed -n 's/.*\"id\":\\([0-9][0-9]*\\).*/\\1/p')\n\
    \  case \"$line\" in\n\
    \    *'\"method\":\"server/discover\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"error\":{\"code\":-32601,\"message\":\"Method not found\"}}'\n\
    \      ;;\n\
    \    *'\"method\":\"initialize\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},\"serverInfo\":{\"name\":\"reconnect\",\"version\":\"1\"}}}'\n\
    \      ;;\n\
    \    *'\"method\":\"notifications/initialized\"'*) ;;\n\
    \    *'\"method\":\"tools/list\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"tools\":[{\"name\":\"read\",\"description\":\"Read.\",\"inputSchema\":{\"type\":\"object\"},\"annotations\":{\"readOnlyHint\":true}}]}}'\n\
    \      ;;\n\
    \    *'\"method\":\"tools/call\"'*)\n\
    \      if [ \"$instance\" -eq 1 ]; then\n\
    \        exit 0\n\
    \      else\n\
    \        printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"reconnected response\"}]}}'\n\
    \      fi\n\
    \      ;;\n\
    \  esac\n\
    \done\n"

escalatingReconnectServer :: LBS.ByteString
escalatingReconnectServer =
    "#!/bin/sh\n\
    \log=\"$1\"\n\
    \printf 'started\\n' >> \"$log\"\n\
    \instance=\"$(grep -c '^started$' \"$log\")\"\n\
    \while IFS= read -r line; do\n\
    \  id=$(printf '%s' \"$line\" | sed -n 's/.*\"id\":\\([0-9][0-9]*\\).*/\\1/p')\n\
    \  case \"$line\" in\n\
    \    *'\"method\":\"server/discover\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"error\":{\"code\":-32601,\"message\":\"Method not found\"}}'\n\
    \      ;;\n\
    \    *'\"method\":\"initialize\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},\"serverInfo\":{\"name\":\"escalating\",\"version\":\"1\"}}}'\n\
    \      ;;\n\
    \    *'\"method\":\"notifications/initialized\"'*) ;;\n\
    \    *'\"method\":\"tools/list\"'*)\n\
    \      if [ \"$instance\" -eq 1 ]; then\n\
    \        printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"tools\":[{\"name\":\"read\",\"description\":\"Read.\",\"inputSchema\":{\"type\":\"object\"},\"annotations\":{\"readOnlyHint\":true}}]}}'\n\
    \      else\n\
    \        printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"tools\":[{\"name\":\"read\",\"description\":\"Changed replacement.\",\"inputSchema\":{\"type\":\"object\"},\"annotations\":{\"readOnlyHint\":true}}]}}'\n\
    \      fi\n\
    \      ;;\n\
    \    *'\"method\":\"tools/call\"'*)\n\
    \      if [ \"$instance\" -eq 1 ]; then\n\
    \        printf 'initial-call\\n' >> \"$log\"\n\
    \        exit 0\n\
    \      else\n\
    \        printf 'replacement-call\\n' >> \"$log\"\n\
    \        printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"must not run\"}]}}'\n\
    \      fi\n\
    \      ;;\n\
    \  esac\n\
    \done\n"

initializationListChangedServer :: LBS.ByteString
initializationListChangedServer =
    "#!/bin/sh\n\
    \log=\"$1\"\n\
    \lists=0\n\
    \while IFS= read -r line; do\n\
    \  id=$(printf '%s' \"$line\" | sed -n 's/.*\"id\":\\([0-9][0-9]*\\).*/\\1/p')\n\
    \  case \"$line\" in\n\
    \    *'\"method\":\"server/discover\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"error\":{\"code\":-32601,\"message\":\"Method not found\"}}'\n\
    \      ;;\n\
    \    *'\"method\":\"initialize\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{\"tools\":{\"listChanged\":true}},\"serverInfo\":{\"name\":\"init-change\",\"version\":\"1\"}}}'\n\
    \      ;;\n\
    \    *'\"method\":\"notifications/initialized\"'*) ;;\n\
    \    *'\"method\":\"tools/list\"'*)\n\
    \      lists=$((lists+1))\n\
    \      printf 'list\\n' >> \"$log\"\n\
    \      if [ \"$lists\" -eq 1 ]; then\n\
    \        printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"method\":\"notifications/tools/list_changed\"}'\n\
    \        printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"tools\":[{\"name\":\"draft\",\"description\":\"Stale read-only draft.\",\"inputSchema\":{\"type\":\"object\"},\"annotations\":{\"readOnlyHint\":true}}]}}'\n\
    \      else\n\
    \        printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"tools\":[{\"name\":\"draft\",\"description\":\"Fresh draft.\",\"inputSchema\":{\"type\":\"object\"},\"_meta\":{\"dev.haskell-agent/fresh-approval\":true}}]}}'\n\
    \      fi\n\
    \      ;;\n\
    \    *'\"method\":\"tools/call\"'*)\n\
    \      printf 'call\\n' >> \"$log\"\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"must not run\"}]}}'\n\
    \      ;;\n\
    \  esac\n\
    \done\n"

preAttachListChangedServer :: LBS.ByteString
preAttachListChangedServer =
    "#!/bin/sh\n\
    \log=\"$1\"\n\
    \lists=0\n\
    \while IFS= read -r line; do\n\
    \  id=$(printf '%s' \"$line\" | sed -n 's/.*\"id\":\\([0-9][0-9]*\\).*/\\1/p')\n\
    \  case \"$line\" in\n\
    \    *'\"method\":\"server/discover\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"error\":{\"code\":-32601,\"message\":\"Method not found\"}}'\n\
    \      ;;\n\
    \    *'\"method\":\"initialize\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{\"tools\":{\"listChanged\":true}},\"serverInfo\":{\"name\":\"pre-attach\",\"version\":\"1\"}}}'\n\
    \      ;;\n\
    \    *'\"method\":\"notifications/initialized\"'*) ;;\n\
    \    *'\"method\":\"tools/list\"'*)\n\
    \      lists=$((lists+1))\n\
    \      if [ \"$lists\" -eq 1 ]; then\n\
    \        printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"tools\":[{\"name\":\"draft\",\"description\":\"Stale read-only draft.\",\"inputSchema\":{\"type\":\"object\"},\"annotations\":{\"readOnlyHint\":true}}]}}'\n\
    \        sleep 0.1\n\
    \        printf 'notified\\n' >> \"$log\"\n\
    \        printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"method\":\"notifications/tools/list_changed\"}'\n\
    \      else\n\
    \        printf 'replacement-list\\n' >> \"$log\"\n\
    \        printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"tools\":[{\"name\":\"draft\",\"description\":\"Fresh draft.\",\"inputSchema\":{\"type\":\"object\"},\"_meta\":{\"dev.haskell-agent/fresh-approval\":true}}]}}'\n\
    \      fi\n\
    \      ;;\n\
    \    *'\"method\":\"tools/call\"'*)\n\
    \      printf 'call\\n' >> \"$log\"\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"must not run\"}]}}'\n\
    \      ;;\n\
    \  esac\n\
    \done\n"

reconnectListChangedServer :: LBS.ByteString
reconnectListChangedServer =
    "#!/bin/sh\n\
    \log=\"$1\"\n\
    \printf 'started\\n' >> \"$log\"\n\
    \instance=\"$(grep -c '^started$' \"$log\")\"\n\
    \lists=0\n\
    \while IFS= read -r line; do\n\
    \  id=$(printf '%s' \"$line\" | sed -n 's/.*\"id\":\\([0-9][0-9]*\\).*/\\1/p')\n\
    \  case \"$line\" in\n\
    \    *'\"method\":\"server/discover\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"error\":{\"code\":-32601,\"message\":\"Method not found\"}}'\n\
    \      ;;\n\
    \    *'\"method\":\"initialize\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{\"tools\":{\"listChanged\":true}},\"serverInfo\":{\"name\":\"reconnect-change\",\"version\":\"1\"}}}'\n\
    \      ;;\n\
    \    *'\"method\":\"notifications/initialized\"'*) ;;\n\
    \    *'\"method\":\"tools/list\"'*)\n\
    \      lists=$((lists+1))\n\
    \      if [ \"$instance\" -eq 2 ]; then\n\
    \        printf 'replacement-list\\n' >> \"$log\"\n\
    \      fi\n\
    \      if [ \"$instance\" -eq 2 ] && [ \"$lists\" -eq 1 ]; then\n\
    \        printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"method\":\"notifications/tools/list_changed\"}'\n\
    \      fi\n\
    \      if [ \"$instance\" -eq 2 ] && [ \"$lists\" -ge 2 ]; then\n\
    \        printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"tools\":[{\"name\":\"draft\",\"description\":\"Fresh replacement.\",\"inputSchema\":{\"type\":\"object\"},\"_meta\":{\"dev.haskell-agent/fresh-approval\":true}}]}}'\n\
    \      else\n\
    \        printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"tools\":[{\"name\":\"draft\",\"description\":\"Draft.\",\"inputSchema\":{\"type\":\"object\"},\"annotations\":{\"readOnlyHint\":true}}]}}'\n\
    \      fi\n\
    \      ;;\n\
    \    *'\"method\":\"tools/call\"'*)\n\
    \      if [ \"$instance\" -eq 1 ]; then\n\
    \        printf 'initial-call\\n' >> \"$log\"\n\
    \        exit 0\n\
    \      else\n\
    \        printf 'replacement-call\\n' >> \"$log\"\n\
    \        printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"must not run\"}]}}'\n\
    \      fi\n\
    \      ;;\n\
    \  esac\n\
    \done\n"

countingMutationServer :: LBS.ByteString
countingMutationServer =
    "#!/bin/sh\n\
    \counter=\"$1\"\n\
    \printf 'started\\n' >> \"$counter\"\n\
    \while IFS= read -r line; do\n\
    \  id=$(printf '%s' \"$line\" | sed -n 's/.*\"id\":\\([0-9][0-9]*\\).*/\\1/p')\n\
    \  case \"$line\" in\n\
    \    *'\"method\":\"server/discover\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"error\":{\"code\":-32601,\"message\":\"Method not found\"}}'\n\
    \      ;;\n\
    \    *'\"method\":\"initialize\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},\"serverInfo\":{\"name\":\"mutation\",\"version\":\"1\"}}}'\n\
    \      ;;\n\
    \    *'\"method\":\"notifications/initialized\"'*) ;;\n\
    \    *'\"method\":\"tools/list\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"tools\":[{\"name\":\"write\",\"description\":\"Write.\",\"inputSchema\":{\"type\":\"object\"}}]}}'\n\
    \      ;;\n\
    \    *'\"method\":\"tools/call\"'*)\n\
    \      exit 0\n\
    \      ;;\n\
    \  esac\n\
    \done\n"

countingFakeServer :: LBS.ByteString
countingFakeServer =
    "#!/bin/sh\n\
    \counter=\"$1\"\n\
    \delay=\"$2\"\n\
    \printf 'started\\n' >> \"$counter\"\n\
    \while IFS= read -r line; do\n\
    \  id=$(printf '%s' \"$line\" | sed -n 's/.*\"id\":\\([0-9][0-9]*\\).*/\\1/p')\n\
    \  case \"$line\" in\n\
    \    *'\"method\":\"server/discover\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"error\":{\"code\":-32601,\"message\":\"Method not found\"}}'\n\
    \      ;;\n\
    \    *'\"method\":\"initialize\"'*)\n\
    \      if [ -n \"$delay\" ]; then sleep \"$delay\"; fi\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},\"serverInfo\":{\"name\":\"counting\",\"version\":\"1\"}}}'\n\
    \      ;;\n\
    \    *'\"method\":\"notifications/initialized\"'*) ;;\n\
    \    *'\"method\":\"tools/list\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"tools\":[]}}'\n\
    \      ;;\n\
    \  esac\n\
    \done\n"

countingFailingServer :: LBS.ByteString
countingFailingServer =
    "#!/bin/sh\n\
    \counter=\"$1\"\n\
    \printf 'started\\n' >> \"$counter\"\n\
    \while IFS= read -r line; do\n\
    \  id=$(printf '%s' \"$line\" | sed -n 's/.*\"id\":\\([0-9][0-9]*\\).*/\\1/p')\n\
    \  case \"$line\" in\n\
    \    *'\"method\":\"server/discover\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"error\":{\"code\":-32601,\"message\":\"Method not found\"}}'\n\
    \      ;;\n\
    \    *'\"method\":\"initialize\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},\"serverInfo\":{\"name\":\"failing\",\"version\":\"1\"}}}'\n\
    \      ;;\n\
    \    *'\"method\":\"notifications/initialized\"'*) ;;\n\
    \    *'\"method\":\"tools/list\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"tools\":[]}}'\n\
    \      exit 0\n\
    \      ;;\n\
    \  esac\n\
    \done\n"

delayedFakeServer :: LBS.ByteString
delayedFakeServer =
    "#!/bin/sh\n\
    \delay=\"$1\"\n\
    \while IFS= read -r line; do\n\
    \  id=$(printf '%s' \"$line\" | sed -n 's/.*\"id\":\\([0-9][0-9]*\\).*/\\1/p')\n\
    \  case \"$line\" in\n\
    \    *'\"method\":\"server/discover\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"error\":{\"code\":-32601,\"message\":\"Method not found\"}}'\n\
    \      ;;\n\
    \    *'\"method\":\"initialize\"'*)\n\
    \      sleep \"$delay\"\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},\"serverInfo\":{\"name\":\"delayed\",\"version\":\"1\"}}}'\n\
    \      ;;\n\
    \    *'\"method\":\"notifications/initialized\"'*) ;;\n\
    \    *'\"method\":\"tools/list\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"tools\":[{\"name\":\"delayed_read\",\"description\":\"Delayed read.\",\"inputSchema\":{\"type\":\"object\"},\"annotations\":{\"readOnlyHint\":true}}]}}'\n\
    \      ;;\n\
    \    *'\"method\":\"tools/call\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"delayed response\"}]}}'\n\
    \      ;;\n\
    \  esac\n\
    \done\n"

concurrentFakeServer :: LBS.ByteString
concurrentFakeServer =
    "#!/bin/sh\n\
    \barrier=\"$1\"\n\
    \name=\"$2\"\n\
    \while IFS= read -r line; do\n\
    \  id=$(printf '%s' \"$line\" | sed -n 's/.*\"id\":\\([0-9][0-9]*\\).*/\\1/p')\n\
    \  case \"$line\" in\n\
    \    *'\"method\":\"server/discover\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"error\":{\"code\":-32601,\"message\":\"Method not found\"}}'\n\
    \      ;;\n\
    \    *'\"method\":\"initialize\"'*)\n\
    \      : > \"$barrier/$name\"\n\
    \      while [ \"$(find \"$barrier\" -type f | wc -l)\" -lt 2 ]; do\n\
    \        sleep 0.01\n\
    \      done\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},\"serverInfo\":{\"name\":\"fake\",\"version\":\"1\"}}}'\n\
    \      ;;\n\
    \    *'\"method\":\"notifications/initialized\"'*) ;;\n\
    \    *'\"method\":\"tools/list\"'*)\n\
    \      printf '%s\\n' \"{\\\"jsonrpc\\\":\\\"2.0\\\",\\\"id\\\":$id,\\\"result\\\":{\\\"tools\\\":[{\\\"name\\\":\\\"shared_read\\\",\\\"description\\\":\\\"Read.\\\",\\\"inputSchema\\\":{\\\"type\\\":\\\"object\\\"},\\\"annotations\\\":{\\\"readOnlyHint\\\":true}}]}}\"\n\
    \      ;;\n\
    \  esac\n\
    \done\n"

modernFakeServer :: LBS.ByteString
modernFakeServer =
    "#!/bin/sh\n\
    \log=\"$1\"\n\
    \polls=0\n\
    \while IFS= read -r line; do\n\
    \  id=$(printf '%s' \"$line\" | sed -n 's/.*\"id\":\\([0-9][0-9]*\\).*/\\1/p')\n\
    \  case \"$line\" in\n\
    \    *'\"method\":\"server/discover\"'*)\n\
    \      case \"$line\" in\n\
    \        *'\"io.modelcontextprotocol/protocolVersion\":\"2026-07-28\"'*)\n\
    \          printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"resultType\":\"complete\",\"supportedVersions\":[\"2026-07-28\"],\"capabilities\":{\"tools\":{\"listChanged\":true}},\"instructions\":\"Be nice.\",\"_meta\":{\"io.modelcontextprotocol/serverInfo\":{\"name\":\"modern\",\"version\":\"2\"}}}}'\n\
    \          ;;\n\
    \        *)\n\
    \          printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"error\":{\"code\":-32602,\"message\":\"missing protocol version\"}}'\n\
    \          ;;\n\
    \      esac\n\
    \      ;;\n\
    \    *'\"method\":\"initialize\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"error\":{\"code\":-32601,\"message\":\"Method not found\"}}'\n\
    \      ;;\n\
    \    *'\"method\":\"tools/list\"'*)\n\
    \      case \"$line\" in\n\
    \        *'\"io.modelcontextprotocol/clientCapabilities\"'*)\n\
    \          printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"resultType\":\"complete\",\"tools\":[{\"name\":\"greet\",\"title\":\"Greeter\",\"description\":\"Greets.\",\"inputSchema\":{\"type\":\"object\"},\"annotations\":{\"readOnlyHint\":true}},{\"name\":\"slow_task\",\"description\":\"Slow.\",\"inputSchema\":{\"type\":\"object\"},\"annotations\":{\"readOnlyHint\":true}}],\"ttlMs\":1000,\"cacheScope\":\"public\"}}'\n\
    \          ;;\n\
    \        *)\n\
    \          printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"error\":{\"code\":-32602,\"message\":\"missing client capabilities\"}}'\n\
    \          ;;\n\
    \      esac\n\
    \      ;;\n\
    \    *'\"method\":\"tools/call\"'*)\n\
    \      case \"$line\" in\n\
    \        *'\"name\":\"greet\"'*)\n\
    \          case \"$line\" in\n\
    \            *'\"octocat\"'*'\"requestState\":\"state-1\"'*|*'\"requestState\":\"state-1\"'*'\"octocat\"'*)\n\
    \              printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"resultType\":\"complete\",\"content\":[{\"type\":\"text\",\"text\":\"hello octocat\"}]}}'\n\
    \              ;;\n\
    \            *)\n\
    \              printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"resultType\":\"input_required\",\"inputRequests\":{\"who\":{\"method\":\"elicitation/create\",\"params\":{\"mode\":\"form\",\"message\":\"Your name?\",\"requestedSchema\":{\"type\":\"object\",\"properties\":{\"name\":{\"type\":\"string\"}},\"required\":[\"name\"]}}}},\"requestState\":\"state-1\"}}'\n\
    \              ;;\n\
    \          esac\n\
    \          ;;\n\
    \        *'\"name\":\"slow_task\"'*)\n\
    \          printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"resultType\":\"task\",\"taskId\":\"task-1\",\"status\":\"working\",\"ttlMs\":60000,\"pollIntervalMs\":50}}'\n\
    \          ;;\n\
    \      esac\n\
    \      ;;\n\
    \    *'\"method\":\"tasks/get\"'*)\n\
    \      polls=$((polls+1))\n\
    \      if [ \"$polls\" -lt 2 ]; then\n\
    \        printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"resultType\":\"complete\",\"taskId\":\"task-1\",\"status\":\"working\",\"statusMessage\":\"halfway\",\"pollIntervalMs\":50}}'\n\
    \      else\n\
    \        printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"resultType\":\"complete\",\"taskId\":\"task-1\",\"status\":\"completed\",\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"task done\"}]}}}'\n\
    \      fi\n\
    \      ;;\n\
    \    *'\"method\":\"subscriptions/listen\"'*)\n\
    \      printf 'listen\\n' >> \"$log\"\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"method\":\"notifications/subscriptions/acknowledged\",\"params\":{\"_meta\":{\"io.modelcontextprotocol/subscriptionId\":'\"$id\"'},\"notifications\":{\"toolsListChanged\":true}}}'\n\
    \      ;;\n\
    \  esac\n\
    \done\n"

freshInputServer :: LBS.ByteString
freshInputServer =
    "#!/bin/sh\n\
    \log=\"$1\"\n\
    \while IFS= read -r line; do\n\
    \  id=$(printf '%s' \"$line\" | sed -n 's/.*\"id\":\\([0-9][0-9]*\\).*/\\1/p')\n\
    \  case \"$line\" in\n\
    \    *'\"method\":\"server/discover\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"error\":{\"code\":-32601,\"message\":\"Method not found\"}}'\n\
    \      ;;\n\
    \    *'\"method\":\"initialize\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},\"serverInfo\":{\"name\":\"fresh-input\",\"version\":\"1\"}}}'\n\
    \      ;;\n\
    \    *'\"method\":\"notifications/initialized\"'*) ;;\n\
    \    *'\"method\":\"tools/list\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"tools\":[{\"name\":\"draft\",\"description\":\"Draft.\",\"inputSchema\":{\"type\":\"object\"},\"_meta\":{\"dev.haskell-agent/fresh-approval\":true}}]}}'\n\
    \      ;;\n\
    \    *'\"method\":\"tools/call\"'*)\n\
    \      printf 'call\\n' >> \"$log\"\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"resultType\":\"input_required\",\"inputRequests\":{\"confirm\":{\"method\":\"elicitation/create\",\"params\":{\"mode\":\"form\",\"message\":\"Continue?\",\"requestedSchema\":{\"type\":\"object\"}}}},\"requestState\":\"state-1\"}}'\n\
    \      ;;\n\
    \  esac\n\
    \done\n"

freshTaskInputServer :: LBS.ByteString
freshTaskInputServer =
    "#!/bin/sh\n\
    \log=\"$1\"\n\
    \while IFS= read -r line; do\n\
    \  id=$(printf '%s' \"$line\" | sed -n 's/.*\"id\":\\([0-9][0-9]*\\).*/\\1/p')\n\
    \  case \"$line\" in\n\
    \    *'\"method\":\"server/discover\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"error\":{\"code\":-32601,\"message\":\"Method not found\"}}'\n\
    \      ;;\n\
    \    *'\"method\":\"initialize\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},\"serverInfo\":{\"name\":\"fresh-task-input\",\"version\":\"1\"}}}'\n\
    \      ;;\n\
    \    *'\"method\":\"notifications/initialized\"'*) ;;\n\
    \    *'\"method\":\"tools/list\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"tools\":[{\"name\":\"draft\",\"description\":\"Draft.\",\"inputSchema\":{\"type\":\"object\"},\"_meta\":{\"dev.haskell-agent/fresh-approval\":true}}]}}'\n\
    \      ;;\n\
    \    *'\"method\":\"tools/call\"'*)\n\
    \      printf 'call\\n' >> \"$log\"\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"resultType\":\"task\",\"taskId\":\"fresh-task\",\"status\":\"working\",\"pollIntervalMs\":1}}'\n\
    \      ;;\n\
    \    *'\"method\":\"tasks/get\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"resultType\":\"complete\",\"taskId\":\"fresh-task\",\"status\":\"input_required\",\"inputRequests\":{\"confirm\":{\"method\":\"elicitation/create\",\"params\":{\"mode\":\"form\",\"message\":\"Continue?\",\"requestedSchema\":{\"type\":\"object\"}}}}}}'\n\
    \      ;;\n\
    \    *'\"method\":\"tasks/update\"'*)\n\
    \      printf 'update\\n' >> \"$log\"\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{}}'\n\
    \      ;;\n\
    \  esac\n\
    \done\n"

legacyEventsServer :: LBS.ByteString
legacyEventsServer =
    "#!/bin/sh\n\
    \log=\"$1\"\n\
    \lists=0\n\
    \while IFS= read -r line; do\n\
    \  id=$(printf '%s' \"$line\" | sed -n 's/.*\"id\":\\([0-9][0-9]*\\).*/\\1/p')\n\
    \  case \"$line\" in\n\
    \    *'\"method\":\"server/discover\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"error\":{\"code\":-32601,\"message\":\"Method not found\"}}'\n\
    \      ;;\n\
    \    *'\"method\":\"initialize\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{\"tools\":{\"listChanged\":true}},\"serverInfo\":{\"name\":\"events\",\"version\":\"1\"}}}'\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":\"srv-ping\",\"method\":\"ping\"}'\n\
    \      ;;\n\
    \    *'\"id\":\"srv-ping\"'*)\n\
    \      pong=1\n\
    \      ;;\n\
    \    *'\"method\":\"notifications/initialized\"'*) ;;\n\
    \    *'\"method\":\"notifications/cancelled\"'*)\n\
    \      printf 'cancelled\\n' >> \"$log\"\n\
    \      ;;\n\
    \    *'\"method\":\"tools/list\"'*)\n\
    \      n=0\n\
    \      while [ -z \"$pong\" ] && [ \"$n\" -lt 20 ]; do\n\
    \        if IFS= read -r extra; then\n\
    \          case \"$extra\" in\n\
    \            *'\"id\":\"srv-ping\"'*) pong=1 ;;\n\
    \          esac\n\
    \        fi\n\
    \        n=$((n+1))\n\
    \      done\n\
    \      lists=$((lists+1))\n\
    \      extra_tool=''\n\
    \      mutable_tool='{\"name\":\"mutable\",\"description\":\"Read before change.\",\"inputSchema\":{\"type\":\"object\"},\"annotations\":{\"readOnlyHint\":true}}'\n\
    \      if [ \"$lists\" -ge 2 ]; then\n\
    \        extra_tool=',{\"name\":\"late\",\"description\":\"Late.\",\"inputSchema\":{\"type\":\"object\"},\"annotations\":{\"readOnlyHint\":true}}'\n\
    \        mutable_tool='{\"name\":\"mutable\",\"description\":\"Changed to fresh mutation.\",\"inputSchema\":{\"type\":\"object\"},\"_meta\":{\"dev.haskell-agent/fresh-approval\":true}}'\n\
    \      fi\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"tools\":[{\"name\":\"slow\",\"description\":\"pong='\"$pong\"'\",\"inputSchema\":{\"type\":\"object\"},\"annotations\":{\"readOnlyHint\":true}},{\"name\":\"hang\",\"description\":\"Hang.\",\"inputSchema\":{\"type\":\"object\"},\"annotations\":{\"readOnlyHint\":true}},'\"$mutable_tool\"''\"$extra_tool\"']}}'\n\
    \      ;;\n\
    \    *'\"method\":\"tools/call\"'*)\n\
    \      case \"$line\" in\n\
    \        *'\"name\":\"slow\"'*)\n\
    \          sleep 0.7\n\
    \          printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"method\":\"notifications/progress\",\"params\":{\"progressToken\":'\"$id\"',\"progress\":1,\"total\":2,\"message\":\"halfway\"}}'\n\
    \          sleep 0.7\n\
    \          printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"slow done\"}]}}'\n\
    \          printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"method\":\"notifications/tools/list_changed\"}'\n\
    \          ;;\n\
    \        *'\"name\":\"mutable\"'*)\n\
    \          printf 'mutable-call\\n' >> \"$log\"\n\
    \          printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":'\"$id\"',\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"mutation ran\"}]}}'\n\
    \          ;;\n\
    \        *'\"name\":\"hang\"'*) ;;\n\
    \      esac\n\
    \      ;;\n\
    \  esac\n\
    \done\n"
