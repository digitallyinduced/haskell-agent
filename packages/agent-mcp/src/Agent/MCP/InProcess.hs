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
    ) where

import Agent.ToolDSL (parametersObjectLoose)
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
import Control.Exception.Safe
    ( displayException
    , tryAny
    )
import Data.Aeson
    ( Value(..)
    , object
    , (.=)
    )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Scientific (formatScientific, FPFormat(Generic))
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

-- | Handle one JSON-RPC message carried by Claude Code's @mcp_message@
-- control request. Notifications return 'Nothing'; the SDK control layer
-- acknowledges those with an empty MCP result as required by Claude Code.
handleInProcessMcpMessage
    :: InProcessMcpServer
    -> Value
    -> IO (Maybe Value)
handleInProcessMcpMessage server = \case
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
                                Left exception ->
                                    rpcError requestId (-32603)
                                        ("Internal error: "
                                            <> Text.pack
                                                (displayException exception))
                _ ->
                    pure (Just (rpcError
                        (fromMaybe Null (KeyMap.lookup "id" message))
                        (-32600)
                        "Invalid Request"))
    _ -> pure (Just (rpcError Null (-32600) "Invalid Request"))

handleNotification :: Text -> IO (Maybe Value)
handleNotification _ = pure Nothing

handleRequest
    :: InProcessMcpServer
    -> Value
    -> Text
    -> Maybe Value
    -> IO Value
handleRequest server requestId method parameters =
    case method of
        "initialize" ->
            pure . rpcSuccess requestId $ object
                [ "protocolVersion" .= requestedProtocolVersion parameters
                , "capabilities" .= object
                    [ "tools" .= object
                        [ "listChanged" .= False
                        ]
                    ]
                , "serverInfo" .= object
                    [ "name" .= server.serverName
                    , "version" .= server.serverVersion
                    ]
                ]
        "ping" ->
            pure (rpcSuccess requestId (object []))
        "tools/list" ->
            pure . rpcSuccess requestId $ object
                [ "tools" .=
                    map toolDescription
                        (toolRegistryTools server.serverTools)
                ]
        "tools/call" ->
            case decodeToolCall parameters of
                Left err -> pure (rpcError requestId (-32602) err)
                Right (toolName, argumentsValue) ->
                    callTool server requestId toolName argumentsValue
        _ ->
            pure (rpcError requestId (-32601)
                ("Method not found: " <> method))

callTool
    :: InProcessMcpServer
    -> Value
    -> Text
    -> Value
    -> IO Value
callTool server requestId toolName argumentsValue =
    case lookupRegisteredTool toolName server.serverTools of
        Nothing ->
            pure . rpcSuccess requestId $
                toolResult True ("Unknown tool: " <> toolName)
        Just _ -> do
            let call = functionToolCall
                    (mcpCallId requestId)
                    toolName
                    (encodeValueText argumentsValue)
            server.serverApprove call >>= \case
                Left denial ->
                    pure . rpcSuccess requestId $ toolResult True denial
                Right False ->
                    pure . rpcSuccess requestId $
                        toolResult True "Tool call rejected by user."
                Right True -> do
                    outcome <- dispatchRegisteredToolCallDetailed
                        server.serverDispatch
                        server.serverTools
                        call
                    pure . rpcSuccess requestId $
                        toolResult
                            (not outcome.toolDispatchSucceeded)
                            outcome.toolDispatchResult.output

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

toolDescription :: AppTool -> Value
toolDescription tool =
    object $
        [ "name" .= tool.appToolName
        , "description" .= tool.appToolDescription
        , "inputSchema" .= schemaValue tool.appToolSchema
        , "annotations" .= object
            [ "readOnlyHint" .= isStaticallyReadOnly tool.appToolApproval
            ]
        ]
        <> [ "_meta" .= object
                [ "dev.haskell-agent/fresh-approval" .= True
                ]
           | isStaticallyFreshApproval tool.appToolApproval
           ]

schemaValue :: ToolSchema -> Value
schemaValue = \case
    JsonFunctionSchema properties -> parametersObjectLoose properties
    RawJsonFunctionSchema value -> value
    FreeformApplyPatchSchema -> object []
    FreeformGrammarSchema _ _ -> object []
    HostedComputerSchema -> object []

hasJsonSchema :: ToolSchema -> Bool
hasJsonSchema = \case
    JsonFunctionSchema _ -> True
    RawJsonFunctionSchema _ -> True
    FreeformApplyPatchSchema -> False
    FreeformGrammarSchema _ _ -> False
    HostedComputerSchema -> False

isStaticallyReadOnly :: ApprovalRule -> Bool
isStaticallyReadOnly = \case
    AlwaysReadOnly -> True
    AlwaysAllowed -> False
    AlwaysPrompt -> False
    AlwaysConfirm -> False
    ClassifyReadOnly _ -> False
    ClassifyApproval _ -> False

isStaticallyFreshApproval :: ApprovalRule -> Bool
isStaticallyFreshApproval = \case
    AlwaysConfirm -> True
    _ -> False

requestedProtocolVersion :: Maybe Value -> Text
requestedProtocolVersion = \case
    Just (Object parameters)
        | Just (String version) <- KeyMap.lookup "protocolVersion" parameters ->
            version
    _ -> "2025-11-25"

toolResult :: Bool -> Text -> Value
toolResult isError output =
    object
        [ "content" .=
            [ object
                [ "type" .= ("text" :: Text)
                , "text" .= output
                ]
            ]
        , "isError" .= isError
        ]

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

mcpCallId :: Value -> Text
mcpCallId = \case
    String value -> "mcp:" <> value
    Number value ->
        "mcp:" <> Text.pack (formatScientific Generic Nothing value)
    value -> "mcp:" <> encodeValueText value

encodeValueText :: Value -> Text
encodeValueText =
    TextEncoding.decodeUtf8 . LBS.toStrict . Aeson.encode
