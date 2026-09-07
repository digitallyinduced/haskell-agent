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
import qualified Data.Set as Set
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

-- | RFC 9110 log levels used by MCP, ordered from most to least verbose.
data McpLogLevel
    = McpLogDebug
    | McpLogInfo
    | McpLogNotice
    | McpLogWarning
    | McpLogError
    | McpLogCritical
    | McpLogAlert
    | McpLogEmergency
    deriving (Eq, Ord, Show, Enum, Bounded)

mcpLogLevelText :: McpLogLevel -> Text
mcpLogLevelText = \case
    McpLogDebug -> "debug"
    McpLogInfo -> "info"
    McpLogNotice -> "notice"
    McpLogWarning -> "warning"
    McpLogError -> "error"
    McpLogCritical -> "critical"
    McpLogAlert -> "alert"
    McpLogEmergency -> "emergency"

mcpLogLevelDecoder :: Json.Decoder McpLogLevel
mcpLogLevelDecoder = Json.text >>= \case
    "debug" -> pure McpLogDebug
    "info" -> pure McpLogInfo
    "notice" -> pure McpLogNotice
    "warning" -> pure McpLogWarning
    "error" -> pure McpLogError
    "critical" -> pure McpLogCritical
    "alert" -> pure McpLogAlert
    "emergency" -> pure McpLogEmergency
    level -> fail ("unknown MCP log level: " <> Text.unpack level)

-- | Image metadata attached to MCP tools, prompts, resources, and
-- implementations. Sizes use the specification's CSS-like values (for
-- example @"48x48"@ or @"any"@).
data McpIcon = McpIcon
    { iconSrc :: !Text
    , iconMimeType :: !(Maybe Text)
    , iconSizes :: ![Text]
    } deriving (Eq, Show)

mcpIconDecoder :: Json.Decoder McpIcon
mcpIconDecoder = Json.object do
    iconSrc <- Json.atKey "src" Json.text
    iconMimeType <- Json.optionalKey "mimeType" Json.text
    iconSizes <- Json.defaultKey [] "sizes" (Json.list Json.text)
    pure McpIcon{..}

-- | A filesystem root offered by the host to an MCP server.
data McpRoot = McpRoot
    { rootUri :: !Text
    , rootName :: !(Maybe Text)
    } deriving (Eq, Show)

encodeMcpRoots :: [McpRoot] -> RawJson
encodeMcpRoots roots =
    rawJsonFromEncoding . Aeson.toEncoding $ object
        [ "roots" .= map encodeRoot roots ]
  where
    encodeRoot :: McpRoot -> Aeson.Value
    encodeRoot root =
        object $
            ["uri" .= root.rootUri]
                <> maybe [] (\name -> ["name" .= name]) root.rootName

-- | One conversation message supplied to an MCP sampling request. Content is
-- intentionally opaque so newly introduced content-block variants survive.
data McpSamplingMessage = McpSamplingMessage
    { samplingMessageRole :: !Text
    , samplingMessageContent :: !RawJson
    } deriving (Eq, Show)

data McpModelHint = McpModelHint
    { modelHintName :: !(Maybe Text)
    } deriving (Eq, Show)

data McpModelPreferences = McpModelPreferences
    { modelPreferenceHints :: ![McpModelHint]
    , modelPreferenceCostPriority :: !(Maybe Double)
    , modelPreferenceSpeedPriority :: !(Maybe Double)
    , modelPreferenceIntelligencePriority :: !(Maybe Double)
    } deriving (Eq, Show)

-- | A server's request for an isolated model completion.
data McpSamplingRequest = McpSamplingRequest
    { samplingServerName :: !Text
    , samplingMessages :: ![McpSamplingMessage]
    , samplingModelPreferences :: !(Maybe McpModelPreferences)
    , samplingSystemPrompt :: !(Maybe Text)
    , samplingIncludeContext :: !(Maybe Text)
    , samplingTemperature :: !(Maybe Double)
    , samplingMaxTokens :: !Int
    , samplingStopSequences :: ![Text]
    , samplingMetadata :: !(Maybe RawJson)
    , samplingTools :: !(Maybe RawJson)
    , samplingToolChoice :: !(Maybe RawJson)
    } deriving (Eq, Show)

mcpSamplingRequestDecoder :: Text -> Json.Decoder McpSamplingRequest
mcpSamplingRequestDecoder samplingServerName = Json.object do
    samplingMessages <-
        Json.defaultKey [] "messages" (Json.list messageDecoder)
    samplingModelPreferences <-
        Json.optionalKey "modelPreferences" modelPreferencesDecoder
    samplingSystemPrompt <- Json.optionalKey "systemPrompt" Json.text
    samplingIncludeContext <- Json.optionalKey "includeContext" Json.text
    samplingTemperature <- Json.optionalKey "temperature" Json.double
    samplingMaxTokens <- Json.atKey "maxTokens" Json.int
    samplingStopSequences <-
        Json.defaultKey [] "stopSequences" (Json.list Json.text)
    samplingMetadata <- Json.optionalKey "metadata" rawJsonDecoder
    samplingTools <- Json.optionalKey "tools" rawJsonDecoder
    samplingToolChoice <- Json.optionalKey "toolChoice" rawJsonDecoder
    pure McpSamplingRequest{..}
  where
    messageDecoder = Json.object do
        samplingMessageRole <- Json.atKey "role" Json.text
        samplingMessageContent <- Json.atKey "content" rawJsonDecoder
        pure McpSamplingMessage{..}
    modelPreferencesDecoder = Json.object do
        modelPreferenceHints <-
            Json.defaultKey [] "hints" (Json.list hintDecoder)
        modelPreferenceCostPriority <-
            Json.optionalKey "costPriority" Json.double
        modelPreferenceSpeedPriority <-
            Json.optionalKey "speedPriority" Json.double
        modelPreferenceIntelligencePriority <-
            Json.optionalKey "intelligencePriority" Json.double
        pure McpModelPreferences{..}
    hintDecoder =
        Json.object (McpModelHint <$> Json.optionalKey "name" Json.text)

data McpSamplingResult = McpSamplingResult
    { samplingResultRole :: !Text
    , samplingResultContent :: !RawJson
    , samplingResultModel :: !Text
    , samplingResultStopReason :: !(Maybe Text)
    } deriving (Eq, Show)

encodeMcpSamplingResult :: McpSamplingResult -> RawJson
encodeMcpSamplingResult result =
    rawJsonFromEncoding . Aeson.toEncoding $ object $
        [ "role" .= result.samplingResultRole
        , "content" .= result.samplingResultContent
        , "model" .= result.samplingResultModel
        ]
            <> maybe []
                (\reason -> ["stopReason" .= reason])
                result.samplingResultStopReason

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
    , mcpServerRootsEnabled :: !Bool
    , mcpServerSamplingEnabled :: !Bool
    , mcpServerLogLevel :: !(Maybe McpLogLevel)
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
            <> ", mcpServerRootsEnabled = "
            <> show config.mcpServerRootsEnabled
            <> ", mcpServerSamplingEnabled = "
            <> show config.mcpServerSamplingEnabled
            <> ", mcpServerLogLevel = "
            <> show config.mcpServerLogLevel
            <> " }"

-- | Host-provided integration points shared by every server in a fleet.
data McpHostHooks = McpHostHooks
    { mcpHostElicit :: !(IO (Maybe (McpElicitRequest -> IO McpElicitResult)))
    -- ^ Interactive elicitation, resolved when a request is built so hosts
    -- can install or remove their UI over the life of a process. 'Nothing'
    -- means the host cannot ask the user for input, so the @elicitation@
    -- capability is not declared and any request for input is cancelled.
    , mcpHostRoots :: !(IO (Maybe (Text -> IO [McpRoot])))
    -- ^ Roots visible to a server, resolved at request time. The server name
    -- is passed to the handler so a host can apply server-specific policy.
    , mcpHostSample ::
        !(IO (Maybe (McpSamplingRequest -> IO (Either Text McpSamplingResult))))
    -- ^ A deliberately one-shot sampling handler, resolved at request time.
    -- 'Nothing' means sampling must not be advertised to the server.
    , mcpHostClientName :: !Text
    , mcpHostClientVersion :: !Text
    , mcpHostArtifactDirectory :: !(Maybe FilePath)
    -- ^ Caller-owned private process directory. Nothing disables automatic
    -- materialization of explicitly tagged MCP artifact resources.
    }

defaultMcpHostHooks :: McpHostHooks
defaultMcpHostHooks = McpHostHooks
    { mcpHostElicit = pure Nothing
    , mcpHostRoots = pure Nothing
    , mcpHostSample = pure Nothing
    , mcpHostClientName = "haskell-agent"
    , mcpHostClientVersion = "0.1.0"
    , mcpHostArtifactDirectory = Nothing
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
    , catalogGeneration :: !Integer
    }

data McpApprovedCall = McpApprovedCall
    { approvedCallArguments :: !Text
    , approvedCatalogName :: !Text
    , approvedCatalogEntry :: !(Maybe McpCatalogEntry)
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
    , serverInfoIcons :: ![McpIcon]
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
    | McpTaskStatusChanged !McpTask
    -- ^ A task snapshot from @notifications/tasks/status@.
    | McpSubscriptionsAcknowledged ![Text]
    -- ^ Resource URIs accepted by a modern subscription listener.
    deriving (Eq, Show)

-- | Lifecycle states defined by the MCP tasks extension.
data McpTaskStatus
    = McpTaskWorking
    | McpTaskInputRequired
    | McpTaskCompleted
    | McpTaskFailed
    | McpTaskCancelled
    | McpTaskStatusUnknown !Text
    deriving (Eq, Show)

mcpTaskStatusText :: McpTaskStatus -> Text
mcpTaskStatusText = \case
    McpTaskWorking -> "working"
    McpTaskInputRequired -> "input_required"
    McpTaskCompleted -> "completed"
    McpTaskFailed -> "failed"
    McpTaskCancelled -> "cancelled"
    McpTaskStatusUnknown status -> status

mcpTaskStatusDecoder :: Json.Decoder McpTaskStatus
mcpTaskStatusDecoder = Json.text >>= \case
    "working" -> pure McpTaskWorking
    "input_required" -> pure McpTaskInputRequired
    "completed" -> pure McpTaskCompleted
    "failed" -> pure McpTaskFailed
    "cancelled" -> pure McpTaskCancelled
    status -> pure (McpTaskStatusUnknown status)

data McpTaskInputRequest = McpTaskInputRequest
    { taskInputMethod :: !Text
    , taskInputParams :: !(Maybe RawJson)
    } deriving (Eq, Show)

-- | A task snapshot returned by the MCP tasks extension. Both the modern
-- millisecond fields and legacy names are normalized to milliseconds.
data McpTask = McpTask
    { taskId :: !Text
    , taskStatus :: !McpTaskStatus
    , taskStatusMessage :: !(Maybe Text)
    , taskCreatedAt :: !(Maybe Text)
    , taskLastUpdatedAt :: !(Maybe Text)
    , taskTtlMs :: !(Maybe Int)
    , taskPollIntervalMs :: !Int
    , taskResult :: !(Maybe RawJson)
    , taskError :: !(Maybe RawJson)
    , taskInputRequests :: !(Map.Map Text McpTaskInputRequest)
    } deriving (Eq, Show)

mcpTaskDecoder :: Json.Decoder McpTask
mcpTaskDecoder = Json.object do
    taskId <- Json.atKey "taskId" Json.text
    taskStatus <- Json.defaultKey McpTaskWorking "status" mcpTaskStatusDecoder
    taskStatusMessage <- Json.optionalKey "statusMessage" Json.text
    taskCreatedAt <- Json.optionalKey "createdAt" Json.text
    taskLastUpdatedAt <- Json.optionalKey "lastUpdatedAt" Json.text
    ttlMs <- Json.optionalKey "ttlMs" Json.int
    ttlLegacy <- Json.optionalKey "ttl" Json.int
    pollMs <- Json.optionalKey "pollIntervalMs" Json.int
    pollLegacy <- Json.optionalKey "pollInterval" Json.int
    taskResult <- Json.optionalKey "result" rawJsonDecoder
    taskError <- Json.optionalKey "error" rawJsonDecoder
    requests <-
        Json.optionalKey "inputRequests"
            (Json.objectAsMap pure inputRequestDecoder)
    pure McpTask
        { taskId
        , taskStatus
        , taskStatusMessage
        , taskCreatedAt
        , taskLastUpdatedAt
        , taskTtlMs = maybe ttlLegacy Just ttlMs
        , taskPollIntervalMs = maybe 1000 id (maybe pollLegacy Just pollMs)
        , taskResult
        , taskError
        , taskInputRequests = maybe Map.empty id requests
        }
  where
    inputRequestDecoder = Json.object do
        taskInputMethod <- Json.defaultKey "" "method" Json.text
        taskInputParams <- Json.optionalKey "params" rawJsonDecoder
        pure McpTaskInputRequest{..}

data McpTaskList = McpTaskList
    { taskListTasks :: ![McpTask]
    , taskListNextCursor :: !(Maybe RawJson)
    } deriving (Eq, Show)

mcpTaskListDecoder :: Json.Decoder McpTaskList
mcpTaskListDecoder = Json.object do
    taskListTasks <- Json.defaultKey [] "tasks" (Json.list mcpTaskDecoder)
    taskListNextCursor <- Json.optionalKey "nextCursor" rawJsonDecoder
    pure McpTaskList{..}

data PendingRequest = PendingRequest
    { pendingResponse :: !(TMVar (Either McpError RawJson))
    , pendingActivity :: !(TVar Int)
    -- ^ Incremented for every progress notification so the waiter can extend
    -- its timeout while the server is demonstrably working.
    , pendingOnProgress :: !(McpProgress -> IO ())
    }

-- | Request ids and their response destinations. Keeping both values in one
-- STM cell makes id allocation and waiter registration one atomic transition.
data RequestRegistry = RequestRegistry
    { requestRegistryNextId :: !Int
    , requestRegistryPending :: !(IntMap.IntMap PendingRequest)
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
    -- ^ Entries currently advertised for progressive dispatch.
    , mcpFleetCatalogRevisions :: !(TVar (Map.Map Text Integer))
    -- ^ Per-server invalidation revisions. A stale refresh may never
    -- republish entries after a newer list-change notification.
    , mcpFleetApprovedCalls ::
        !(TVar (Map.Map (Text, Text) McpApprovedCall))
    -- ^ Approval-time snapshots keyed by @(call id, meta-tool name)@. The
    -- progressive handler validates the exact arguments and consumes the
    -- snapshot once, preventing a catalog update between approval and
    -- dispatch from silently escalating the selected tool's policy.
    , mcpFleetNextCatalogGeneration :: !(TVar Integer)
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
    | McpClientInMemory !McpToolServer !(IORef (IO ()))

-- | Typed host-owned MCP operations. JSON is retained only for the dynamic
-- tool payload, not for protocol envelopes or control operations.
data McpToolServer = McpToolServer
    { toolServerInitialize :: IO McpServerInfo
    , toolServerListTools :: IO (Either McpError [McpTool])
    , toolServerCallTool :: McpCallToolRequest -> IO (Either McpError McpCallToolResult)
    , toolServerSubscribe :: IO () -> IO (IO ())
    -- ^ Register invalidation, returning an idempotent unsubscribe action.
    }

data McpCallToolRequest = McpCallToolRequest
    { callToolName :: !Text
    , callToolArguments :: !RawJson
    , callToolRequestId :: !(Maybe Text)
    }

data McpCallToolResult = McpCallToolResult
    { callToolIsError :: !Bool
    , callToolText :: ![Text]
    , callToolStructuredContent :: !(Maybe RawJson)
    }

-- | One connection to an MCP server over stdio or Streamable HTTP.
data McpClient = McpClient
    { clientConfig :: !McpServerConfig
    , clientHooks :: !McpHostHooks
    , clientTransport :: !McpClientTransport
    , clientRequestRegistry :: !(TVar RequestRegistry)
    , clientFailure :: !(TVar (Maybe Text))
    , clientWorkers :: !(TVar [Async ()])
    -- ^ Background work owned by the client: handlers for server-initiated
    -- requests and long-lived subscription streams.
    , clientClosed :: !(MVar Bool)
    , clientLifecycle :: !(TVar McpClientLifecycle)
    , clientServerInfo :: !(TVar (Maybe McpServerInfo))
    -- ^ Set once the protocol era has been negotiated.
    , clientDiscoveredSkills :: !(TVar [McpSkillEntry])
    , clientToolsRevision :: !(TVar Integer)
    -- ^ Incremented synchronously for every tools/list_changed notification.
    -- Initialization and refresh only publish a catalog discovered at a
    -- stable revision.
    , clientReadyToolsRevision :: !(TVar (Maybe Integer))
    -- ^ Revision represented by 'ClientReady'. 'Nothing' is a buffered
    -- invalidation which a subsequently attached fleet handler must replay.
    , clientEventHandler :: !(IORef (McpServerEvent -> IO ()))
    , clientResourceSubscriptionsRequested :: !(TVar (Set.Set Text))
    -- ^ Resource URIs requested by the host. Modern servers acknowledge an
    -- accepted subset; legacy servers accept each URI request individually.
    , clientResourceSubscriptionsAccepted :: !(TVar (Set.Set Text))
    , clientTaskStatuses :: !(TVar (Map.Map Text McpTask))
    -- ^ Latest notification or polling result for each observed task.
    , clientSubscriptionWorker :: !(MVar (Maybe (Async ())))
    -- ^ The one replaceable modern @subscriptions/listen@ worker.
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
    , resourceIcons :: ![McpIcon]
    } deriving (Eq, Show)

mcpResourceDecoder :: Json.Decoder McpResource
mcpResourceDecoder = Json.object do
    resourceUri <- Json.atKey "uri" Json.text
    resourceName <- Json.defaultKey "" "name" Json.text
    resourceTitle <- Json.optionalKey "title" Json.text
    resourceDescription <- Json.optionalKey "description" Json.text
    resourceMimeType <- Json.optionalKey "mimeType" Json.text
    resourceSize <- Json.optionalKey "size" Json.int
    resourceIcons <- Json.defaultKey [] "icons" (Json.list mcpIconDecoder)
    pure McpResource{..}

-- | A parameterized resource advertised by @resources/templates/list@.
data McpResourceTemplate = McpResourceTemplate
    { templateUri :: !Text
    , templateName :: !Text
    , templateTitle :: !(Maybe Text)
    , templateDescription :: !(Maybe Text)
    , templateMimeType :: !(Maybe Text)
    , templateIcons :: ![McpIcon]
    } deriving (Eq, Show)

mcpResourceTemplateDecoder :: Json.Decoder McpResourceTemplate
mcpResourceTemplateDecoder = Json.object do
    templateUri <- Json.atKey "uriTemplate" Json.text
    templateName <- Json.defaultKey "" "name" Json.text
    templateTitle <- Json.optionalKey "title" Json.text
    templateDescription <- Json.optionalKey "description" Json.text
    templateMimeType <- Json.optionalKey "mimeType" Json.text
    templateIcons <- Json.defaultKey [] "icons" (Json.list mcpIconDecoder)
    pure McpResourceTemplate{..}

-- | A prompt template advertised by @prompts/list@.
data McpPrompt = McpPrompt
    { promptName :: !Text
    , promptTitle :: !(Maybe Text)
    , promptDescription :: !(Maybe Text)
    , promptArguments :: ![McpPromptArgument]
    , promptIcons :: ![McpIcon]
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
    promptIcons <- Json.defaultKey [] "icons" (Json.list mcpIconDecoder)
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
    , discoveredRequiresFreshApproval :: !Bool
    , discoveredDestructive :: !Bool
    , discoveredIdempotent :: !Bool
    , discoveredOpenWorld :: !Bool
    , discoveredHeaderParams :: ![McpHeaderParam]
    , discoveredIcons :: ![McpIcon]
    } deriving (Eq)

-- | Whether a failed call may be retried without risking a duplicated side
-- effect. Annotations are only hints, so this remains conservative.
mcpToolRetrySafe :: McpTool -> Bool
mcpToolRetrySafe tool =
    not tool.discoveredRequiresFreshApproval
        && (tool.discoveredReadOnly || tool.discoveredIdempotent)

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
    -- Use the underlying optional field decoder rather than 'optionalKey':
    -- explicit null is malformed metadata, not the same as an absent field.
    rawMeta <- Json.atKeyOptional "_meta" rawJsonDecoder
    let annotations =
            maybe defaultAnnotations (projectRawOr defaultAnnotations annotationsDecoder) rawAnnotations
        (discoveredReadOnly, discoveredDestructive, discoveredIdempotent, discoveredOpenWorld) =
            annotations
        discoveredRequiresFreshApproval =
            maybe False
                -- A malformed proprietary marker must never downgrade a
                -- sensitive tool to ordinary approval. Treat malformed
                -- @_meta@ or a non-boolean present marker conservatively.
                (projectRawOr True freshApprovalDecoder)
                rawMeta
        discoveredHeaderParams = []
    discoveredIcons <- Json.defaultKey [] "icons" (Json.list mcpIconDecoder)
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
    freshApprovalDecoder = Json.object do
        rawMarker <-
            Json.atKeyOptional
                "dev.haskell-agent/fresh-approval"
                rawJsonDecoder
        pure $ maybe False (projectRawOr True Json.bool) rawMarker

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
