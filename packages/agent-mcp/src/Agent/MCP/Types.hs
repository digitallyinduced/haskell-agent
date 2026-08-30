-- | Shared types for the Model Context Protocol client.
--
-- The client speaks both protocol eras defined by the specification:
--
-- * /modern/ revisions (@2026-07-28@ and later) are stateless. Every request
--   carries the protocol version, client identity, and client capabilities in
--   @_meta@, and servers are discovered through @server/discover@.
-- * /legacy/ revisions (@2025-11-25@ and earlier) establish a session with an
--   @initialize@ handshake and, over Streamable HTTP, an @Mcp-Session-Id@.
module Agent.MCP.Types where

import Agent.Json
    ( RawJson
    , rawJsonBytes
    , rawJsonDecoder
    , rawJsonFromEncoding
    )
import qualified Agent.Json.Decode as Json
import Agent.Tools.Types (AppTool(..))
import Control.Concurrent.Async (Async)
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM (TMVar, TVar)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.IORef (IORef)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)
import System.IO (Handle)
import System.Posix.Types (ProcessGroupID)
import System.Process (ProcessHandle)

-- | Which protocol era to use for a configured server. 'McpProtocolAuto'
-- probes with @server/discover@ and falls back to the legacy @initialize@
-- handshake; the explicit values skip the probe.
data McpProtocolPreference
    = McpProtocolAuto
    | McpProtocolModern
    | McpProtocolLegacy
    deriving (Eq, Show)

data McpServerConfig = McpServerConfig
    { mcpServerName :: !Text
    , mcpServerUrl :: !(Maybe Text)
    , mcpServerCommand :: !FilePath
    , mcpServerArgs :: ![String]
    , mcpServerCwd :: !(Maybe FilePath)
    , mcpServerEnv :: ![(String, String)]
    , mcpServerStartupTimeoutSeconds :: !Int
    , mcpServerRequestTimeoutSeconds :: !Int
    , mcpServerProtocol :: !McpProtocolPreference
    } deriving (Eq)

instance Show McpServerConfig where
    show config =
        "McpServerConfig"
            <> " { mcpServerName = " <> show config.mcpServerName
            <> ", mcpServerUrl = " <> show config.mcpServerUrl
            <> ", mcpServerCommand = " <> show config.mcpServerCommand
            <> ", mcpServerArgs = " <> show config.mcpServerArgs
            <> ", mcpServerCwd = " <> show config.mcpServerCwd
            <> ", mcpServerEnv = "
            <> show [(name, "<redacted>" :: String) | (name, _) <- config.mcpServerEnv]
            <> ", mcpServerStartupTimeoutSeconds = "
            <> show config.mcpServerStartupTimeoutSeconds
            <> ", mcpServerRequestTimeoutSeconds = "
            <> show config.mcpServerRequestTimeoutSeconds
            <> ", mcpServerProtocol = "
            <> show config.mcpServerProtocol
            <> " }"

-- | Host-provided integration points shared by every server in a fleet.
data McpHostHooks = McpHostHooks
    { mcpHostElicit :: !(IO (Maybe (McpElicitRequest -> IO McpElicitResult)))
    -- ^ Interactive elicitation, resolved when a request is built so hosts
    -- can install or remove their UI over the life of a process. 'Nothing'
    -- means the host cannot ask the user for input, so the @elicitation@
    -- capability is not declared and any request for input is cancelled.
    , mcpHostClientName :: !Text
    , mcpHostClientVersion :: !Text
    }

defaultMcpHostHooks :: McpHostHooks
defaultMcpHostHooks = McpHostHooks
    { mcpHostElicit = pure Nothing
    , mcpHostClientName = "haskell-agent"
    , mcpHostClientVersion = "0.1.0"
    }

-- | A server's request for user input, delivered either as a legacy
-- server-initiated @elicitation/create@ request or inside a multi round-trip
-- @input_required@ result.
data McpElicitRequest = McpElicitRequest
    { elicitServerName :: !Text
    , elicitMessage :: !Text
    , elicitMode :: !McpElicitMode
    } deriving (Eq, Show)

data McpElicitMode
    = McpElicitForm !RawJson
    -- ^ Form mode with the (restricted) JSON Schema of the requested object.
    | McpElicitUrl !Text
    -- ^ URL mode. The user must consent before the URL is opened.
    deriving (Eq, Show)

data McpElicitResult
    = McpElicitAccept !(Maybe RawJson)
    | McpElicitDecline
    | McpElicitCancel
    deriving (Eq, Show)

encodeElicitResult :: McpElicitResult -> RawJson
encodeElicitResult result = rawJsonFromEncoding . Aeson.toEncoding $ case result of
    McpElicitAccept content ->
        object
            ("action" .= ("accept" :: Text)
                : maybe [] (\value -> ["content" .= value]) content)
    McpElicitDecline -> object ["action" .= ("decline" :: Text)]
    McpElicitCancel -> object ["action" .= ("cancel" :: Text)]

data McpToolRegistration = McpToolRegistration
    { mcpRegistrationServer :: !Text
    , mcpRegistrationTool :: !AppTool
    }

data McpCatalogEntry = McpCatalogEntry
    { catalogClient :: !McpClient
    , catalogTool :: !McpTool
    }

-- | Initialization state exposed to status and UI code. Reading this state
-- never starts a process, performs a handshake, or sends an MCP request.
data McpInitState
    = McpPending
    | McpInitializing
    | McpReady
    | McpFailed !Text
    | McpClosed
    deriving (Eq, Show)

data McpServerStatus = McpServerStatus
    { mcpStatusName :: !Text
    , mcpStatusState :: !McpInitState
    , mcpStatusToolCount :: !Int
    } deriving (Eq, Show)

-- | Protocol era negotiated with a server.
data McpProtocolEra
    = McpEraModern
    | McpEraLegacy
    deriving (Eq, Show)

-- | Identity and capabilities learned from @server/discover@ or the legacy
-- @initialize@ result.
data McpServerInfo = McpServerInfo
    { serverInfoEra :: !McpProtocolEra
    , serverInfoProtocolVersion :: !Text
    , serverInfoName :: !(Maybe Text)
    , serverInfoVersion :: !(Maybe Text)
    , serverInfoTitle :: !(Maybe Text)
    , serverInfoInstructions :: !(Maybe Text)
    , serverInfoCapabilities :: !McpServerCapabilities
    } deriving (Eq, Show)

data McpServerCapabilities = McpServerCapabilities
    { capabilityTools :: !(Maybe McpListCapability)
    , capabilityPrompts :: !(Maybe McpListCapability)
    , capabilityResources :: !(Maybe McpResourcesCapability)
    , capabilityCompletions :: !Bool
    , capabilityLogging :: !Bool
    , capabilityExtensions :: !(Map.Map Text RawJson)
    , capabilitySkills :: !(Maybe McpSkillsCapability)
    } deriving (Eq, Show)

emptyServerCapabilities :: McpServerCapabilities
emptyServerCapabilities = McpServerCapabilities
    { capabilityTools = Nothing
    , capabilityPrompts = Nothing
    , capabilityResources = Nothing
    , capabilityCompletions = False
    , capabilityLogging = False
    , capabilityExtensions = Map.empty
    , capabilitySkills = Nothing
    }

newtype McpListCapability = McpListCapability
    { listChanged :: Bool
    } deriving (Eq, Show)

data McpResourcesCapability = McpResourcesCapability
    { resourcesListChanged :: !Bool
    , resourcesSubscribe :: !Bool
    } deriving (Eq, Show)

serverCapabilitiesDecoder :: Json.Decoder McpServerCapabilities
serverCapabilitiesDecoder = Json.object do
    capabilityTools <- Json.optionalKey "tools" listCapabilityDecoder
    capabilityPrompts <- Json.optionalKey "prompts" listCapabilityDecoder
    capabilityResources <- Json.optionalKey "resources" resourcesCapabilityDecoder
    completions <- Json.optionalKey "completions" rawJsonDecoder
    logging <- Json.optionalKey "logging" rawJsonDecoder
    extensions <-
        Json.optionalKey "extensions"
            (Json.objectAsMap pure rawJsonDecoder)
    let capabilityExtensions = maybe Map.empty id extensions
        capabilitySkills =
            case Map.lookup "io.modelcontextprotocol/skills" capabilityExtensions of
                Nothing -> Nothing
                Just raw ->
                    Just McpSkillsCapability
                        { mcpSkillsDirectoryRead =
                            projectRawOr False skillsDirectoryDecoder raw
                        }
    pure McpServerCapabilities
        { capabilityTools
        , capabilityPrompts
        , capabilityResources
        , capabilityCompletions = maybe False (const True) completions
        , capabilityLogging = maybe False (const True) logging
        , capabilityExtensions
        , capabilitySkills
        }
  where
    listCapabilityDecoder =
        Json.object (McpListCapability <$> Json.defaultKey False "listChanged" Json.bool)
    resourcesCapabilityDecoder = Json.object do
        resourcesListChanged <- Json.defaultKey False "listChanged" Json.bool
        resourcesSubscribe <- Json.defaultKey False "subscribe" Json.bool
        pure McpResourcesCapability{..}
    skillsDirectoryDecoder =
        Json.object (Json.defaultKey False "directoryRead" Json.bool)

-- | Failure of one MCP request.
data McpError
    = McpTransportError !Text
    -- ^ The transport failed or the message could not be decoded.
    | McpTimeout !Text
    -- ^ No response arrived within the request timeout. The request was
    -- cancelled.
    | McpHttpStatus !Int !Text
    -- ^ A non-2xx HTTP status whose body was not a JSON-RPC error.
    | McpRpcError !Int !Text !(Maybe RawJson)
    -- ^ A JSON-RPC error response: code, message, and optional data.
    deriving (Eq, Show)

renderMcpError :: McpError -> Text
renderMcpError = \case
    McpTransportError message -> message
    McpTimeout message -> message
    McpHttpStatus status body ->
        "MCP HTTP request failed with status "
            <> Text.pack (show status)
            <> (if Text.null body then "" else ": " <> Text.take 500 body)
    McpRpcError code message payload ->
        "MCP error " <> Text.pack (show code) <> ": " <> message
            <> maybe ""
                (\value -> " " <> decodeUtf8Lenient (rawJsonBytes value))
                payload

-- Error codes reserved by the specification.
errorCodeHeaderMismatch, errorCodeMissingClientCapability, errorCodeUnsupportedProtocolVersion, errorCodeMethodNotFound, errorCodeInvalidParams, errorCodeInternal :: Int
errorCodeHeaderMismatch = -32020
errorCodeMissingClientCapability = -32021
errorCodeUnsupportedProtocolVersion = -32022
errorCodeMethodNotFound = -32601
errorCodeInvalidParams = -32602
errorCodeInternal = -32603

-- | Progress reported for an in-flight request.
data McpProgress = McpProgress
    { progressValue :: !Double
    , progressTotal :: !(Maybe Double)
    , progressMessage :: !(Maybe Text)
    } deriving (Eq, Show)

-- | Server-initiated notifications that are not tied to a single request.
data McpServerEvent
    = McpToolsListChanged
    | McpPromptsListChanged
    | McpResourcesListChanged
    | McpResourceUpdated !Text
    | McpLogMessage !Text !(Maybe Text) !RawJson
    -- ^ Level, logger, and data of a @notifications/message@.
    deriving (Eq, Show)

data PendingRequest = PendingRequest
    { pendingResponse :: !(TMVar (Either McpError RawJson))
    , pendingActivity :: !(TVar Int)
    -- ^ Incremented for every progress notification so the waiter can extend
    -- its timeout while the server is demonstrably working.
    , pendingOnProgress :: !(McpProgress -> IO ())
    }

mcpSkillEntryDecoder :: Json.Decoder McpSkillEntry
mcpSkillEntryDecoder = Json.object do
    mcpSkillUri <- Json.atKey "uri" Json.text
    mcpSkillFrontmatter <- Json.atKey "frontmatter" rawJsonDecoder
    mcpSkillResources <- Json.atKey "resources" mcpSkillResourcesDecoder
    pure McpSkillEntry{..}

mcpSkillResourcesDecoder :: Json.Decoder McpSkillResources
mcpSkillResourcesDecoder =
    Json.getType >>= \case
        Json.VString -> do
            value <- Json.text
            if value == "dynamic"
                then pure McpSkillResourcesDynamic
                else fail "resources must be an array or \"dynamic\""
        Json.VArray ->
            McpSkillResourcesListed <$> Json.list mcpSkillResourceDecoder
        _ -> fail "resources must be an array or \"dynamic\""

mcpSkillResourceDecoder :: Json.Decoder McpSkillResource
mcpSkillResourceDecoder = Json.object do
    mcpSkillResourceUri <- Json.atKey "uri" Json.text
    mcpSkillResourceDigest <- Json.atKey "digest" Json.text
    mcpSkillResourceSize <- Json.atKey "size" Json.int
    pure McpSkillResource{..}

mcpResourceContentDecoder :: Json.Decoder McpResourceContent
mcpResourceContentDecoder = Json.object do
    mcpResourceUri <- Json.atKey "uri" Json.text
    mcpResourceMimeType <- Json.optionalKey "mimeType" Json.text
    mcpResourceText <- Json.optionalKey "text" Json.text
    mcpResourceBlob <- Json.optionalKey "blob" Json.text
    pure McpResourceContent{..}

data McpFleet = McpFleet
    { mcpFleetRegistrations :: ![McpToolRegistration]
    , mcpFleetSkills :: !(TVar [McpSkillRegistration])
    , mcpFleetWarnings :: ![Text]
    , mcpFleetClients :: !(TVar (Map.Map Text McpClient))
    , mcpFleetServerOrder :: ![Text]
    , mcpFleetFailures :: !(Map.Map Text Text)
    , mcpFleetCatalog :: !(TVar (Map.Map Text McpCatalogEntry))
    , mcpFleetReconnects :: !(Map.Map Text (MVar ()))
    , mcpFleetWorkers :: !(MVar [Async ()])
    , mcpFleetClosed :: !(MVar Bool)
    , mcpFleetHooks :: !McpHostHooks
    }

data McpSupervisor = McpSupervisor
    { supervisorState :: !(MVar McpSupervisorState)
    , supervisorHooks :: !McpHostHooks
    }

data McpSupervisorState = McpSupervisorState
    { supervisorClosed :: !Bool
    , supervisorNextLeaseId :: !Int
    , supervisorEntries :: ![McpSupervisorEntry]
    , supervisorPending :: ![McpSupervisorPending]
    }

data McpSupervisorEntry = McpSupervisorEntry
    { supervisorEntryId :: !Int
    , supervisorEntryProgressive :: !Bool
    , supervisorEntryConfigs :: ![McpServerConfig]
    , supervisorEntryFleet :: !McpFleet
    , supervisorEntryLeases :: !Int
    }

data McpSupervisorPending = McpSupervisorPending
    { supervisorPendingId :: !Int
    , supervisorPendingProgressive :: !Bool
    , supervisorPendingConfigs :: ![McpServerConfig]
    , supervisorPendingResult :: !(TMVar (Either Text McpFleet))
    , supervisorPendingWorker ::
        !(TMVar (Async (Either Text McpFleet)))
    , supervisorPendingLeases :: !Int
    }

data McpAcquireDecision
    = UseReady !(Int, McpFleet)
    | WaitPending !Int !(TMVar (Either Text McpFleet))
    | StartPending
        !Int
        !(TMVar (Either Text McpFleet))
        !(TMVar (Async (Either Text McpFleet)))

data McpFleetLease = McpFleetLease
    { mcpLeaseFleet :: !McpFleet
    , mcpLeaseRelease :: !(IO ())
    }

-- | Resources owned by a local stdio transport.
data McpStdioTransport = McpStdioTransport
    { stdioInput :: !Handle
    , stdioProcess :: !ProcessHandle
    , stdioGroupId :: !(Maybe ProcessGroupID)
    , stdioWriteLock :: !(MVar ())
    , stdioStderr :: !(IORef CapturedStderr)
    , stdioReader :: !(IORef (Maybe (Async ())))
    , stdioStderrReader :: !(IORef (Maybe (Async ())))
    }

-- | State owned by a remote Streamable HTTP transport.
data McpHttpTransport = McpHttpTransport
    { httpUrl :: !Text
    , httpSession :: !(IORef (Maybe Text))
    -- ^ Legacy session id. Modern servers never mint one.
    }

-- | The live transport owned by one MCP client.
data McpClientTransport
    = McpClientStdio !McpStdioTransport
    | McpClientHttp !McpHttpTransport

-- | One connection to an MCP server over stdio or Streamable HTTP.
data McpClient = McpClient
    { clientConfig :: !McpServerConfig
    , clientHooks :: !McpHostHooks
    , clientTransport :: !McpClientTransport
    , clientNextId :: !(IORef Int)
    , clientPending :: !(TVar (IntMap.IntMap PendingRequest))
    , clientFailure :: !(TVar (Maybe Text))
    , clientWorkers :: !(TVar [Async ()])
    -- ^ Background work owned by the client: handlers for server-initiated
    -- requests and long-lived subscription streams.
    , clientClosed :: !(MVar Bool)
    , clientLifecycle :: !(TVar McpClientLifecycle)
    , clientServerInfo :: !(TVar (Maybe McpServerInfo))
    -- ^ Set once the protocol era has been negotiated.
    , clientDiscoveredSkills :: !(TVar [McpSkillEntry])
    , clientEventHandler :: !(IORef (McpServerEvent -> IO ()))
    , clientEraHint :: !(Maybe McpProtocolEra)
    -- ^ Era observed by a previous connection to the same server. Skips the
    -- discovery probe after a reconnect.
    }

data McpSkillsCapability = McpSkillsCapability
    { mcpSkillsDirectoryRead :: !Bool
    } deriving (Eq, Show)

data McpSkillRegistration = McpSkillRegistration
    { mcpSkillServer :: !Text
    , mcpSkillEntry :: !McpSkillEntry
    } deriving (Eq, Show)

data McpSkillResources
    = McpSkillResourcesListed ![McpSkillResource]
    | McpSkillResourcesDynamic
    deriving (Eq, Show)

data McpSkillResource = McpSkillResource
    { mcpSkillResourceUri :: !Text
    , mcpSkillResourceDigest :: !Text
    , mcpSkillResourceSize :: !Int
    } deriving (Eq, Show)

data McpSkillEntry = McpSkillEntry
    { mcpSkillUri :: !Text
    , mcpSkillFrontmatter :: !RawJson
    , mcpSkillResources :: !McpSkillResources
    } deriving (Eq, Show)

data McpResourceContent = McpResourceContent
    { mcpResourceUri :: !Text
    , mcpResourceMimeType :: !(Maybe Text)
    , mcpResourceText :: !(Maybe Text)
    , mcpResourceBlob :: !(Maybe Text)
    } deriving (Eq, Show)

-- | A resource advertised by @resources/list@.
data McpResource = McpResource
    { resourceUri :: !Text
    , resourceName :: !Text
    , resourceTitle :: !(Maybe Text)
    , resourceDescription :: !(Maybe Text)
    , resourceMimeType :: !(Maybe Text)
    , resourceSize :: !(Maybe Int)
    } deriving (Eq, Show)

mcpResourceDecoder :: Json.Decoder McpResource
mcpResourceDecoder = Json.object do
    resourceUri <- Json.atKey "uri" Json.text
    resourceName <- Json.defaultKey "" "name" Json.text
    resourceTitle <- Json.optionalKey "title" Json.text
    resourceDescription <- Json.optionalKey "description" Json.text
    resourceMimeType <- Json.optionalKey "mimeType" Json.text
    resourceSize <- Json.optionalKey "size" Json.int
    pure McpResource{..}

-- | A parameterized resource advertised by @resources/templates/list@.
data McpResourceTemplate = McpResourceTemplate
    { templateUri :: !Text
    , templateName :: !Text
    , templateTitle :: !(Maybe Text)
    , templateDescription :: !(Maybe Text)
    , templateMimeType :: !(Maybe Text)
    } deriving (Eq, Show)

mcpResourceTemplateDecoder :: Json.Decoder McpResourceTemplate
mcpResourceTemplateDecoder = Json.object do
    templateUri <- Json.atKey "uriTemplate" Json.text
    templateName <- Json.defaultKey "" "name" Json.text
    templateTitle <- Json.optionalKey "title" Json.text
    templateDescription <- Json.optionalKey "description" Json.text
    templateMimeType <- Json.optionalKey "mimeType" Json.text
    pure McpResourceTemplate{..}

-- | A prompt template advertised by @prompts/list@.
data McpPrompt = McpPrompt
    { promptName :: !Text
    , promptTitle :: !(Maybe Text)
    , promptDescription :: !(Maybe Text)
    , promptArguments :: ![McpPromptArgument]
    } deriving (Eq, Show)

data McpPromptArgument = McpPromptArgument
    { promptArgumentName :: !Text
    , promptArgumentDescription :: !(Maybe Text)
    , promptArgumentRequired :: !Bool
    } deriving (Eq, Show)

mcpPromptDecoder :: Json.Decoder McpPrompt
mcpPromptDecoder = Json.object do
    promptName <- Json.atKey "name" Json.text
    promptTitle <- Json.optionalKey "title" Json.text
    promptDescription <- Json.optionalKey "description" Json.text
    promptArguments <- Json.defaultKey [] "arguments" (Json.list argumentDecoder)
    pure McpPrompt{..}
  where
    argumentDecoder = Json.object do
        promptArgumentName <- Json.atKey "name" Json.text
        promptArgumentDescription <- Json.optionalKey "description" Json.text
        promptArgumentRequired <- Json.defaultKey False "required" Json.bool
        pure McpPromptArgument{..}

-- | One message of a resolved prompt. The content is kept as the wire JSON
-- because prompt messages carry the same content blocks as tool results.
data McpPromptMessage = McpPromptMessage
    { promptMessageRole :: !Text
    , promptMessageContent :: !RawJson
    } deriving (Eq, Show)

data McpPromptResult = McpPromptResult
    { promptResultDescription :: !(Maybe Text)
    , promptResultMessages :: ![McpPromptMessage]
    } deriving (Eq, Show)

mcpPromptResultDecoder :: Json.Decoder McpPromptResult
mcpPromptResultDecoder = Json.object do
    promptResultDescription <- Json.optionalKey "description" Json.text
    promptResultMessages <- Json.defaultKey [] "messages" (Json.list messageDecoder)
    pure McpPromptResult{..}
  where
    messageDecoder = Json.object do
        promptMessageRole <- Json.defaultKey "user" "role" Json.text
        promptMessageContent <- Json.atKey "content" rawJsonDecoder
        pure McpPromptMessage{..}

-- | Argument completion suggestions from @completion/complete@.
data McpCompletion = McpCompletion
    { completionValues :: ![Text]
    , completionTotal :: !(Maybe Int)
    , completionHasMore :: !Bool
    } deriving (Eq, Show)

mcpCompletionDecoder :: Json.Decoder McpCompletion
mcpCompletionDecoder = Json.object $ Json.atKey "completion" $ Json.object do
    completionValues <- Json.defaultKey [] "values" (Json.list Json.text)
    completionTotal <- Json.optionalKey "total" Json.int
    completionHasMore <- Json.defaultKey False "hasMore" Json.bool
    pure McpCompletion{..}

data McpClientLifecycle
    = ClientPending
    | ClientInitializing
        !(TMVar (Either Text ([McpTool], [Text])))
    | ClientReady ![McpTool] ![Text]
    | ClientFailed !Text
    | ClientClosed

data CapturedStderr = CapturedStderr
    { stderrBytes :: !BS.ByteString
    , stderrDropped :: !Int
    }

emptyCapturedStderr :: CapturedStderr
emptyCapturedStderr = CapturedStderr BS.empty 0

stderrLimit :: Int
stderrLimit = 16 * 1024

-- | A tool parameter mirrored into an HTTP header via @x-mcp-header@.
data McpHeaderParam = McpHeaderParam
    { headerParamPath :: ![Text]
    -- ^ Chain of @properties@ keys from the schema root.
    , headerParamName :: !Text
    -- ^ The name portion of the resulting @Mcp-Param-{name}@ header.
    } deriving (Eq, Show)

data McpTool = McpTool
    { discoveredName :: !Text
    , discoveredTitle :: !(Maybe Text)
    , discoveredDescription :: !Text
    , discoveredInputSchema :: !RawJson
    , discoveredOutputSchema :: !(Maybe RawJson)
    , discoveredReadOnly :: !Bool
    , discoveredDestructive :: !Bool
    , discoveredIdempotent :: !Bool
    , discoveredOpenWorld :: !Bool
    , discoveredHeaderParams :: ![McpHeaderParam]
    }

-- | Whether a failed call may be retried without risking a duplicated side
-- effect. Annotations are only hints, so this remains conservative.
mcpToolRetrySafe :: McpTool -> Bool
mcpToolRetrySafe tool = tool.discoveredReadOnly || tool.discoveredIdempotent

mcpToolDecoder :: Json.Decoder McpTool
mcpToolDecoder = Json.object do
    discoveredName <- Json.atKey "name" Json.text
    discoveredTitle <- Json.optionalKey "title" Json.text
    rawDescription <- Json.optionalKey "description" rawJsonDecoder
    let discoveredDescription =
            maybe "" (projectRawOr "" Json.text) rawDescription
    discoveredInputSchema <-
        maybe emptyInputSchema id
            <$> Json.atKeyOptional "inputSchema" rawJsonDecoder
    discoveredOutputSchema <- Json.optionalKey "outputSchema" rawJsonDecoder
    rawAnnotations <- Json.optionalKey "annotations" rawJsonDecoder
    let annotations =
            maybe defaultAnnotations (projectRawOr defaultAnnotations annotationsDecoder) rawAnnotations
        (discoveredReadOnly, discoveredDestructive, discoveredIdempotent, discoveredOpenWorld) =
            annotations
        discoveredHeaderParams = []
    pure McpTool{..}
  where
    -- Specification defaults: destructive and open-world unless stated.
    defaultAnnotations = (False, True, False, True)
    annotationsDecoder = Json.object do
        readOnly <- Json.defaultKey False "readOnlyHint" Json.bool
        destructive <- Json.defaultKey True "destructiveHint" Json.bool
        idempotent <- Json.defaultKey False "idempotentHint" Json.bool
        openWorld <- Json.defaultKey True "openWorldHint" Json.bool
        pure (readOnly, destructive, idempotent, openWorld)

projectRawOr :: a -> Json.Decoder a -> RawJson -> a
projectRawOr fallback decoder value =
    either (const fallback) id $
        Json.decodeEither decoder (rawJsonBytes value)

emptyInputSchema :: RawJson
emptyInputSchema =
    rawJsonFromEncoding . Aeson.toEncoding $ object
        [ "type" .= ("object" :: Text)
        , "properties" .= object []
        , "additionalProperties" .= False
        ]

decodeUtf8Lenient :: BS.ByteString -> Text
decodeUtf8Lenient = TextEncoding.decodeUtf8With lenientDecode . BS.take 2000
