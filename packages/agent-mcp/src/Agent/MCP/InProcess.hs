-- | A small in-process MCP server used to expose host-owned 'AppTool's to
-- provider SDKs without starting another subprocess.
--
-- The provider remains an MCP client. Every @tools/call@ is routed back
-- through the harness approval callback before the registered handler runs,
-- so provider-side allowlists can never bypass host policy.
module Agent.MCP.InProcess
    ( InProcessMcpServer
    , InProcessMcpApproval
    , createInProcessMcpServer
    , inProcessMcpToolNames
    , handleInProcessMcpMessage
    , inProcessMcpToolServer
    , handleToolServerMessage
    ) where

import Agent.ToolDSL (parametersObjectLoose)
import Agent.Json (rawJsonFromEncoding, rawJsonBytes)
import Agent.MCP.Types
    ( McpToolServer(..), McpCallToolRequest(..), McpCallToolResult(..)
    , McpServerInfo(..), McpProtocolEra(..), emptyServerCapabilities
    , McpServerCapabilities(..), McpListCapability(..), McpTool(..), McpError(..)
    )
import Agent.ToolDispatch
    ( ToolCall
    , ToolCallResult(..)
    , ToolDispatchConfig
    , ToolDispatchOutcome(..)
    , functionToolCall
    )
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , ToolRegistry
    , ToolSchema(..)
    , dispatchRegisteredToolCallDetailed
    , lookupRegisteredTool
    , mkToolRegistry
    , toolRegistryTools
    )
import Control.Exception.Safe (tryAny)
import Data.Aeson
    ( Value(..)
    , object
    , (.=)
    )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

type InProcessMcpApproval = ToolCall -> IO (Either Text Bool)

data InProcessMcpServer = InProcessMcpServer
    { serverName :: !Text
    , serverVersion :: !Text
    , serverTools :: !ToolRegistry
    , serverDispatch :: !ToolDispatchConfig
    , serverApprove :: !InProcessMcpApproval
    }

-- | Build a server from a deliberately selected tool set. MCP tool calls carry
-- JSON objects, so exposing a freeform provider tool is rejected rather than
-- silently changing its input contract.
createInProcessMcpServer
    :: Text
    -> Text
    -> ToolDispatchConfig
    -> InProcessMcpApproval
    -> [AppTool]
    -> Either Text InProcessMcpServer
createInProcessMcpServer serverName serverVersion serverDispatch serverApprove tools = do
    case
        [ tool.appToolName
        | tool <- tools
        , not (hasJsonSchema tool.appToolSchema)
        ] of
        [] -> pure ()
        unsupported ->
            Left
                ("in-process MCP only supports JSON tools; unsupported: "
                    <> Text.intercalate ", " unsupported)
    serverTools <- mkToolRegistry tools
    pure InProcessMcpServer{..}

inProcessMcpToolNames :: InProcessMcpServer -> [Text]
inProcessMcpToolNames =
    map (.appToolName) . toolRegistryTools . (.serverTools)

-- | Typed endpoint for the existing host-tool adapter. Its approval callback
-- is deliberately preserved, including when invoked without a JSON adapter.
inProcessMcpToolServer :: InProcessMcpServer -> McpToolServer
inProcessMcpToolServer server = McpToolServer
    { toolServerInitialize = pure McpServerInfo
        { serverInfoEra = McpEraLegacy
        , serverInfoProtocolVersion = "2025-11-25"
        , serverInfoName = Just server.serverName
        , serverInfoVersion = Just server.serverVersion
        , serverInfoTitle = Nothing
        , serverInfoInstructions = Nothing
        , serverInfoCapabilities = emptyServerCapabilities
            { capabilityTools = Just (McpListCapability False) }
        }
    , toolServerListTools = pure (Right
        [ McpTool
            { discoveredName = tool.appToolName
            , discoveredTitle = Nothing
            , discoveredDescription = tool.appToolDescription
            , discoveredInputSchema = rawJsonFromEncoding
                (Aeson.toEncoding (schemaValue tool.appToolSchema))
            , discoveredOutputSchema = Nothing
            , discoveredReadOnly = isStaticallyReadOnly tool.appToolApproval
            , discoveredRequiresFreshApproval = isStaticallyFreshApproval tool.appToolApproval
            , discoveredDestructive = True
            , discoveredIdempotent = False
            , discoveredOpenWorld = True
            , discoveredHeaderParams = []
            }
        | tool <- toolRegistryTools server.serverTools
        ])
    , toolServerCallTool = \request ->
        Right <$> callToolTyped server
            ("mcp:" <> fromMaybe request.callToolName request.callToolRequestId) request
    , toolServerSubscribe = \_ -> pure (pure ())
    }

-- | Handle one JSON-RPC message carried by Claude Code's @mcp_message@
-- control request. Notifications return 'Nothing'; the SDK control layer
-- acknowledges those with an empty MCP result as required by Claude Code.
handleInProcessMcpMessage
    :: InProcessMcpServer
    -> Value
    -> IO (Maybe Value)
handleInProcessMcpMessage = handleToolServerMessage . inProcessMcpToolServer

-- | JSON-RPC is an external adapter, not the in-memory operation interface.
handleToolServerMessage :: McpToolServer -> Value -> IO (Maybe Value)
handleToolServerMessage server = \case
    Object message
        | KeyMap.lookup "jsonrpc" message /= Just (String "2.0") ->
            pure (Just (rpcError Null (-32600) "Invalid Request"))
        | otherwise ->
            case KeyMap.lookup "method" message of
                Just (String method) ->
                    case KeyMap.lookup "id" message of
                        Nothing -> handleNotification method
                        Just requestId -> do
                            outcome <- tryAny
                                (handleRequest server requestId method
                                    (KeyMap.lookup "params" message))
                            pure . Just $ case outcome of
                                Right response -> response
                                Left _ ->
                                    rpcError requestId (-32603) "Internal MCP error"
                _ ->
                    pure (Just (rpcError
                        (fromMaybe Null (KeyMap.lookup "id" message))
                        (-32600)
                        "Invalid Request"))
    _ -> pure (Just (rpcError Null (-32600) "Invalid Request"))

handleNotification :: Text -> IO (Maybe Value)
handleNotification _ = pure Nothing

handleRequest
    :: McpToolServer
    -> Value
    -> Text
    -> Maybe Value
    -> IO Value
handleRequest server requestId method parameters =
    case method of
        "initialize" -> do
            info <- server.toolServerInitialize
            pure . rpcSuccess requestId $ object
                [ "protocolVersion" .= requestedProtocolVersion parameters
                , "capabilities" .= object
                    [ "tools" .= object
                        [ "listChanged" .= maybe False (.listChanged)
                            info.serverInfoCapabilities.capabilityTools
                        ]
                    ]
                , "serverInfo" .= object
                    [ "name" .= info.serverInfoName
                    , "version" .= info.serverInfoVersion
                    ]
                , "instructions" .= info.serverInfoInstructions
                ]
        "ping" ->
            pure (rpcSuccess requestId (object []))
        "tools/list" -> server.toolServerListTools >>= pure . either
            (encodeError requestId)
            (\tools -> rpcSuccess requestId (object ["tools" .= map encodeTool tools]))
        "tools/call" ->
            case decodeToolCall parameters of
                Left err -> pure (rpcError requestId (-32602) err)
                Right (toolName, argumentsValue) -> do
                    outcome <- server.toolServerCallTool
                        (McpCallToolRequest toolName
                            (rawJsonFromEncoding (Aeson.toEncoding argumentsValue))
                            (Just (case requestId of
                                String ident -> ident
                                _ -> TextEncoding.decodeUtf8
                                    (rawJsonBytes (rawJsonFromEncoding (Aeson.toEncoding requestId))))))
                    pure $ either (encodeError requestId)
                        (\result -> rpcSuccess requestId $ object $
                            [ "content" .= [object ["type" .= ("text" :: Text), "text" .= text]
                                | text <- result.callToolText]
                            , "isError" .= result.callToolIsError
                            ] <> ["structuredContent" .= content
                                 | Just content <- [result.callToolStructuredContent]])
                        outcome
        _ ->
            pure (rpcError requestId (-32601)
                ("Method not found: " <> method))

encodeError :: Value -> McpError -> Value
encodeError requestId (McpRpcError code message _) = rpcError requestId code message
encodeError requestId _ = rpcError requestId (-32603) "Internal MCP error"

encodeTool :: McpTool -> Value
encodeTool tool = object $
    [ "name" .= tool.discoveredName
    , "description" .= tool.discoveredDescription
    , "inputSchema" .= tool.discoveredInputSchema
    , "annotations" .= object
        [ "readOnlyHint" .= tool.discoveredReadOnly
        , "destructiveHint" .= tool.discoveredDestructive
        , "idempotentHint" .= tool.discoveredIdempotent
        , "openWorldHint" .= tool.discoveredOpenWorld
        ]
    ] <> ["outputSchema" .= schema | Just schema <- [tool.discoveredOutputSchema]]
      <> ["_meta" .= object ["dev.haskell-agent/fresh-approval" .= True]
         | tool.discoveredRequiresFreshApproval]

callToolTyped :: InProcessMcpServer -> Text -> McpCallToolRequest -> IO McpCallToolResult
callToolTyped server callId request =
    case lookupRegisteredTool toolName server.serverTools of
        Nothing ->
            pure $ result True ("Unknown tool: " <> toolName)
        Just _ -> do
            let call = functionToolCall
                    callId
                    toolName
                    (TextEncoding.decodeUtf8 (rawJsonBytes request.callToolArguments))
            server.serverApprove call >>= \case
                Left denial ->
                    pure $ result True denial
                Right False ->
                    pure $ result True "Tool call rejected by user."
                Right True -> do
                    outcome <- dispatchRegisteredToolCallDetailed
                        server.serverDispatch
                        server.serverTools
                        call
                    pure $
                        result
                            (not outcome.toolDispatchSucceeded)
                            outcome.toolDispatchResult.output
  where
    toolName = request.callToolName
    result failure output = McpCallToolResult failure [output] Nothing

decodeToolCall :: Maybe Value -> Either Text (Text, Value)
decodeToolCall = \case
    Just (Object parameters) -> do
        toolName <- case KeyMap.lookup "name" parameters of
            Just (String value)
                | not (Text.null (Text.strip value)) -> Right value
            _ -> Left "tools/call requires a non-empty string name"
        argumentsValue <- case KeyMap.lookup "arguments" parameters of
            Nothing -> Right (Object mempty)
            Just value@(Object _) -> Right value
            Just _ -> Left "tools/call arguments must be an object"
        Right (toolName, argumentsValue)
    _ -> Left "tools/call params must be an object"

schemaValue :: ToolSchema -> Value
schemaValue = \case
    JsonFunctionSchema properties -> parametersObjectLoose properties
    RawJsonFunctionSchema value -> value
    FreeformApplyPatchSchema -> object []
    FreeformGrammarSchema _ _ -> object []
    HostedComputerSchema -> object []
    HostedComputerFunctionSchema _ -> object []

hasJsonSchema :: ToolSchema -> Bool
hasJsonSchema = \case
    JsonFunctionSchema _ -> True
    RawJsonFunctionSchema _ -> True
    FreeformApplyPatchSchema -> False
    FreeformGrammarSchema _ _ -> False
    HostedComputerSchema -> False
    HostedComputerFunctionSchema _ -> False

isStaticallyReadOnly :: ApprovalRule -> Bool
isStaticallyReadOnly = \case
    AlwaysReadOnly -> True
    AlwaysAllowed -> False
    AlwaysPrompt -> False
    AlwaysConfirm -> False
    ClassifyReadOnly _ -> False
    ClassifyApproval _ -> False
    AutoApprove original -> isStaticallyReadOnly original

isStaticallyFreshApproval :: ApprovalRule -> Bool
isStaticallyFreshApproval = \case
    AlwaysConfirm -> True
    AutoApprove original -> isStaticallyFreshApproval original
    _ -> False

requestedProtocolVersion :: Maybe Value -> Text
requestedProtocolVersion = \case
    Just (Object parameters)
        | Just (String version) <- KeyMap.lookup "protocolVersion" parameters ->
            version
    _ -> "2025-11-25"

rpcSuccess :: Value -> Value -> Value
rpcSuccess requestId result =
    object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= requestId
        , "result" .= result
        ]

rpcError :: Value -> Int -> Text -> Value
rpcError requestId code message =
    object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= requestId
        , "error" .= object
            [ "code" .= code
            , "message" .= message
            ]
        ]
