module Agent.MCP.Types where


import Agent.Json
    ( RawJson
    , rawJsonBytes
    , rawJsonDecoder
    , rawJsonFromEncoding
    )
import qualified Agent.Json.Decode as Json
import Agent.Tools.IO (terminateProcessGroup)
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , ToolSchema(..)
    )
import Agent.ToolDispatch (typedTool)
import Agent.Concurrent (forConcurrentlyBounded_)
import Control.Concurrent.Async
    ( Async
    , asyncWithUnmask
    , cancel
    , mapConcurrently
    , waitCatch
    )
import Control.Concurrent.QSem
    ( newQSem
    , signalQSem
    , waitQSem
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , modifyMVar_
    , newMVar
    , withMVar
    )
import Control.Concurrent.STM
    ( STM
    , TMVar
    , TVar
    , atomically
    , modifyTVar'
    , newEmptyTMVar
    , newEmptyTMVarIO
    , newTVarIO
    , readTMVar
    , readTVar
    , readTVarIO
    , takeTMVar
    , tryPutTMVar
    , writeTVar
    )
import Control.Exception.Safe
    ( SomeException
    , displayException
    , bracket_
    , finally
    , mask
    , onException
    , throwIO
    , tryAny
    )
import Control.Monad (forM, unless, void, when)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum)
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    )
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.List (find, sortOn)
import Data.Maybe (catMaybes, isJust)
import Data.Ord (Down(..))
import Data.Scientific (floatingOrInteger)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Vector as Vector
import System.Environment (getEnvironment)
import System.Directory (getCurrentDirectory)
import System.IO
    ( BufferMode(..)
    , Handle
    , hClose
    , hFlush
    , hSetBinaryMode
    , hSetBuffering
    )
import System.Posix.Types (ProcessGroupID)
import System.Process
    ( CreateProcess(..)
    , ProcessHandle
    , StdStream(..)
    , createProcess
    , getPid
    , proc
    )
import System.Timeout (timeout)

data McpServerConfig = McpServerConfig
    { mcpServerName :: !Text
    , mcpServerCommand :: !FilePath
    , mcpServerArgs :: ![String]
    , mcpServerCwd :: !(Maybe FilePath)
    , mcpServerEnv :: ![(String, String)]
    , mcpServerStartupTimeoutSeconds :: !Int
    , mcpServerRequestTimeoutSeconds :: !Int
    } deriving (Eq)

instance Show McpServerConfig where
    show config =
        "McpServerConfig"
            <> " { mcpServerName = " <> show config.mcpServerName
            <> ", mcpServerCommand = " <> show config.mcpServerCommand
            <> ", mcpServerArgs = " <> show config.mcpServerArgs
            <> ", mcpServerCwd = " <> show config.mcpServerCwd
            <> ", mcpServerEnv = "
            <> show [(name, "<redacted>" :: String) | (name, _) <- config.mcpServerEnv]
            <> ", mcpServerStartupTimeoutSeconds = "
            <> show config.mcpServerStartupTimeoutSeconds
            <> ", mcpServerRequestTimeoutSeconds = "
            <> show config.mcpServerRequestTimeoutSeconds
            <> " }"

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
    }

data McpSupervisor = McpSupervisor
    { supervisorState :: !(MVar McpSupervisorState)
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

-- | A process-scoped cache of MCP fleets. Released leases remain warm for a
-- later provider/session rebuild; changed configurations replace idle fleets,
-- and 'closeMcpSupervisor' performs the final deterministic shutdown.
data McpClient = McpClient
    { clientConfig :: !McpServerConfig
    , clientInput :: !Handle
    , clientProcess :: !ProcessHandle
    , clientGroupId :: !(Maybe ProcessGroupID)
    , clientNextId :: !(IORef Int)
    , clientPending :: !(TVar (IntMap.IntMap (TMVar (Either Text RawJson))))
    , clientFailure :: !(TVar (Maybe Text))
    , clientWriteLock :: !(MVar ())
    , clientStderr :: !(IORef CapturedStderr)
    , clientReader :: !(Async ())
    , clientStderrReader :: !(Async ())
    , clientClosed :: !(MVar Bool)
    , clientLifecycle :: !(TVar McpClientLifecycle)
    -- Set after initialize.  A missing value means the server did not
    -- negotiate the optional Skills over MCP extension.
    , clientSkillsCapability :: !(TVar (Maybe McpSkillsCapability))
    , clientDiscoveredSkills :: !(TVar [McpSkillEntry])
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

data McpTool = McpTool
    { discoveredName :: !Text
    , discoveredDescription :: !Text
    , discoveredInputSchema :: !RawJson
    , discoveredReadOnly :: !Bool
    }

mcpToolDecoder :: Json.Decoder McpTool
mcpToolDecoder = Json.object do
    discoveredName <- Json.atKey "name" Json.text
    rawDescription <- Json.optionalKey "description" rawJsonDecoder
    let discoveredDescription =
            maybe "" (projectRawOr "" Json.text) rawDescription
    discoveredInputSchema <-
        maybe emptyInputSchema id
            <$> Json.atKeyOptional "inputSchema" rawJsonDecoder
    rawAnnotations <- Json.optionalKey "annotations" rawJsonDecoder
    let discoveredReadOnly =
            maybe False (projectRawOr False annotationsDecoder) rawAnnotations
    pure McpTool{..}
  where
    annotationsDecoder =
        Json.object (Json.defaultKey False "readOnlyHint" Json.bool)

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
