module Agent.Integrations.Server
    ( IntegrationHost
    , newIntegrationHost
    , integrationMcpServer
    , notifyIntegrationToolsChanged
    , closeIntegrationHost
    ) where

import Agent.Integrations.Registry
import Agent.Integrations.Types
import Agent.Json (rawJsonBytes)
import qualified Agent.MCP as MCP
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , modifyMVar_
    , newMVar
    , readMVar
    )
import Control.Exception.Safe (tryAny)
import Control.Monad (forM_, void)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef (atomicModifyIORef', newIORef)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

data IntegrationHost = IntegrationHost
    { hostRegistry :: !IntegrationRegistry
    , hostSubscribers :: !(MVar (Int, IntMap.IntMap (IO ())))
    }

newIntegrationHost :: IntegrationRegistry -> IO IntegrationHost
newIntegrationHost hostRegistry = do
    hostSubscribers <- newMVar (0, IntMap.empty)
    pure IntegrationHost{..}

integrationMcpServer :: IntegrationHost -> MCP.McpToolServer
integrationMcpServer host = MCP.McpToolServer
    { MCP.toolServerInitialize =
        pure MCP.McpServerInfo
            { MCP.serverInfoEra = MCP.McpEraModern
            , MCP.serverInfoProtocolVersion = "2025-11-25"
            , MCP.serverInfoName = Just "integrations"
            , MCP.serverInfoVersion = Just "1"
            , MCP.serverInfoTitle = Just "Haskell Agent Integrations"
            , MCP.serverInfoIcons = []
            , MCP.serverInfoInstructions =
                nonEmpty
                    (integrationRegistryInstructions host.hostRegistry)
            , MCP.serverInfoCapabilities =
                MCP.emptyServerCapabilities
                    { MCP.capabilityTools =
                        Just (MCP.McpListCapability True)
                    }
            }
    , MCP.toolServerListTools =
        integrationRegistryTools host.hostRegistry >>= \case
            Left err ->
                pure (Left (MCP.McpTransportError err))
            Right tools ->
                pure (Right (map toMcpTool tools))
    , MCP.toolServerCallTool = callIntegrationTool host
    , MCP.toolServerSubscribe = subscribe host
    }

callIntegrationTool
    :: IntegrationHost
    -> MCP.McpCallToolRequest
    -> IO (Either MCP.McpError MCP.McpCallToolResult)
callIntegrationTool host request =
    lookupIntegrationTool request.callToolName host.hostRegistry >>= \case
        Left err -> pure (Left (MCP.McpTransportError err))
        Right Nothing ->
            pure
                (Left
                    (MCP.McpRpcError
                        (-32601)
                        "Unknown integration tool."
                        Nothing))
        Right (Just (SomeIntegrationTool tool)) ->
            case
                decodeIntegrationInput
                    tool.integrationToolInput
                    request.callToolArguments
            of
                Left err -> pure (Right (failedResult err))
                Right input ->
                    tool.integrationToolHandler input >>= \case
                        Left err -> pure (Right (failedResult err))
                        Right output -> do
                            let structured =
                                    encodeIntegrationOutput
                                        tool.integrationToolOutput
                                        output
                            if
                                BS.length (rawJsonBytes structured)
                                    > max
                                        1
                                        tool.integrationToolMaximumOutputBytes
                                then
                                    pure . Right . failedResult $
                                        IntegrationOperationFailed
                                            "The integration result exceeded its output limit."
                                else
                                    pure . Right $ MCP.McpCallToolResult
                                        { MCP.callToolIsError = False
                                        , MCP.callToolText =
                                            [ TextEncoding.decodeUtf8
                                                (rawJsonBytes structured)
                                            ]
                                        , MCP.callToolStructuredContent =
                                            Just structured
                                        }

toMcpTool :: SomeIntegrationTool -> MCP.McpTool
toMcpTool (SomeIntegrationTool tool) = MCP.McpTool
    { MCP.discoveredName =
        integrationToolNameText tool.integrationToolNameValue
    , MCP.discoveredTitle = Nothing
    , MCP.discoveredIcons = []
    , MCP.discoveredDescription = tool.integrationToolDescription
    , MCP.discoveredInputSchema =
        tool.integrationToolInput.inputContractSchema
    , MCP.discoveredOutputSchema =
        Just tool.integrationToolOutput.outputContractSchema
    , MCP.discoveredReadOnly =
        tool.integrationToolEffect == IntegrationReadOnly
    , MCP.discoveredRequiresFreshApproval =
        tool.integrationToolEffect == IntegrationFreshApproval
    , MCP.discoveredDestructive = tool.integrationToolDestructive
    , MCP.discoveredIdempotent = tool.integrationToolIdempotent
    , MCP.discoveredOpenWorld = tool.integrationToolOpenWorld
    , MCP.discoveredHeaderParams = []
    }

failedResult :: IntegrationError -> MCP.McpCallToolResult
failedResult err = MCP.McpCallToolResult
    { MCP.callToolIsError = True
    , MCP.callToolText = [renderIntegrationError err]
    , MCP.callToolStructuredContent = Nothing
    }

renderIntegrationError :: IntegrationError -> Text
renderIntegrationError = \case
    IntegrationInvalidInput message -> message
    IntegrationUnavailable message -> message
    IntegrationOperationFailed message -> message

subscribe :: IntegrationHost -> IO () -> IO (IO ())
subscribe host callback = do
    identifier <- modifyMVar host.hostSubscribers \(next, callbacks) ->
        let identifier = next + 1
        in pure
            ( (identifier, IntMap.insert identifier callback callbacks)
            , identifier
            )
    released <- newIORef False
    pure do
        shouldRelease <- atomicModifyIORef' released \done ->
            (True, not done)
        if shouldRelease
            then modifyMVar_ host.hostSubscribers \(next, callbacks) ->
                pure (next, IntMap.delete identifier callbacks)
            else pure ()

notifyIntegrationToolsChanged :: IntegrationHost -> IO ()
notifyIntegrationToolsChanged host = do
    (_, callbacks) <- readMVar host.hostSubscribers
    forM_ (IntMap.elems callbacks) \callback ->
        void (tryAny callback)

closeIntegrationHost :: IntegrationHost -> IO ()
closeIntegrationHost host =
    modifyMVar_ host.hostSubscribers \(next, _) ->
        pure (next, IntMap.empty)

nonEmpty :: Text -> Maybe Text
nonEmpty value
    | Text.null (Text.strip value) = Nothing
    | otherwise = Just (Text.strip value)
