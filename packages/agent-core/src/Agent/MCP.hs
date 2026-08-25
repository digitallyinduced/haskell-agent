-- | Local Model Context Protocol clients over the stdio transport.
--
-- Each configured server is started once, initialized, and queried for its
-- read-only tools. The returned 'AppTool' handlers share the retained client;
-- 'closeMcpFleet' must run after all loops and subagents using those handlers
-- have stopped.
module Agent.MCP
    ( McpServerConfig(..)
    , McpInitState(..)
    , McpServerStatus(..)
    , McpToolRegistration(..)
    , McpFleet(..)
    , McpSupervisor
    , McpFleetLease(..)
    , newMcpSupervisor
    , acquireMcpFleet
    , acquireMcpFleetWithProgress
    , acquireMcpFleetProgressive
    , releaseMcpFleetLease
    , closeMcpSupervisor
    , startMcpFleet
    , startMcpFleetWithProgress
    , startMcpFleetProgressive
    , closeMcpFleet
    , mcpFleetTools
    , mcpFleetMetaTools
    , mcpFleetGrokMetaTools
    , mcpFleetStatuses
    , normalizeMcpToolResult
    ) where

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
import Data.Aeson
    ( FromJSON(..)
    , Value(..)
    , object
    , withObject
    , (.:)
    , (.:?)
    , (.!=)
    , (.=)
    )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as AesonTypes
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

data McpFleet = McpFleet
    { mcpFleetRegistrations :: ![McpToolRegistration]
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
newMcpSupervisor :: IO McpSupervisor
newMcpSupervisor =
    McpSupervisor <$> newMVar McpSupervisorState
        { supervisorClosed = False
        , supervisorNextLeaseId = 1
        , supervisorEntries = []
        , supervisorPending = []
        }

acquireMcpFleet
    :: McpSupervisor
    -> [McpServerConfig]
    -> IO McpFleetLease
acquireMcpFleet supervisor configs =
    resolveEffectiveCwds configs >>= acquireMcpFleetWith supervisor False
        (startMcpFleetWithProgress (const (pure ())))

acquireMcpFleetWithProgress
    :: McpSupervisor
    -> ([Text] -> IO ())
    -> [McpServerConfig]
    -> IO McpFleetLease
acquireMcpFleetWithProgress supervisor report configs =
    resolveEffectiveCwds configs >>= acquireMcpFleetWith supervisor False
        (startMcpFleetWithProgress report)

acquireMcpFleetProgressive
    :: McpSupervisor
    -> ([McpServerStatus] -> IO ())
    -> [McpServerConfig]
    -> IO McpFleetLease
acquireMcpFleetProgressive supervisor report configs =
    resolveEffectiveCwds configs >>= acquireMcpFleetWith supervisor True
        (startMcpFleetProgressive report)

acquireMcpFleetWith
    :: McpSupervisor
    -> Bool
    -> ([McpServerConfig] -> IO McpFleet)
    -> [McpServerConfig]
    -> IO McpFleetLease
acquireMcpFleetWith supervisor progressive start configs =
  mask \restore -> do
    (decision, obsolete) <-
        modifyMVar supervisor.supervisorState \state ->
            if state.supervisorClosed
                then ioError (userError "MCP supervisor closed")
                else do
                    (healthyEntries, failedIdle) <-
                        partitionReusable state.supervisorEntries
                    matching <- findMatching healthyEntries
                    case matching of
                        Just entry -> do
                            let retained = entry
                                    { supervisorEntryLeases =
                                        entry.supervisorEntryLeases + 1
                                    }
                                otherIdle =
                                    idleExcept entry.supervisorEntryId
                                        healthyEntries
                                removed = failedIdle <> otherIdle
                            pure
                                ( state
                                    { supervisorEntries =
                                        replaceEntry retained
                                            (withoutEntries removed
                                                healthyEntries)
                                    }
                                , ( UseReady
                                        ( entry.supervisorEntryId
                                        , entry.supervisorEntryFleet
                                        )
                                  , map (.supervisorEntryFleet) removed
                                  )
                                )
                        Nothing ->
                            case findPending state.supervisorPending of
                                Just pending -> do
                                    let retained = pending
                                            { supervisorPendingLeases =
                                                pending.supervisorPendingLeases + 1
                                            }
                                    pure
                                        ( state
                                            { supervisorEntries = healthyEntries
                                            , supervisorPending =
                                                replacePending retained
                                                    state.supervisorPending
                                            }
                                        , ( WaitPending
                                                pending.supervisorPendingId
                                                pending.supervisorPendingResult
                                          , map (.supervisorEntryFleet) failedIdle
                                          )
                                        )
                                Nothing -> do
                                    completion <- newEmptyTMVarIO
                                    workerSlot <- newEmptyTMVarIO
                                    let pending = McpSupervisorPending
                                            { supervisorPendingId =
                                                state.supervisorNextLeaseId
                                            , supervisorPendingProgressive =
                                                progressive
                                            , supervisorPendingConfigs = configs
                                            , supervisorPendingResult = completion
                                            , supervisorPendingWorker = workerSlot
                                            , supervisorPendingLeases = 1
                                            }
                                        healthyIdle = filter isIdle healthyEntries
                                        removed = failedIdle <> healthyIdle
                                    pure
                                        ( state
                                            { supervisorNextLeaseId =
                                                state.supervisorNextLeaseId + 1
                                            , supervisorEntries =
                                                filter (not . isIdle) healthyEntries
                                            , supervisorPending =
                                                pending : state.supervisorPending
                                            }
                                        , ( StartPending
                                                pending.supervisorPendingId
                                                completion
                                                workerSlot
                                          , map (.supervisorEntryFleet) removed
                                          )
                                        )
    case decision of
        UseReady acquired -> do
            mapM_ closeMcpFleet obsolete
            makeLease acquired
        WaitPending entryId completion -> do
            mapM_ closeMcpFleet obsolete
            result <-
                restore (atomically (readTMVar completion))
                    `onException` releaseSupervisorEntry supervisor entryId
            either (ioError . userError . Text.unpack)
                (\fleet -> makeLease (entryId, fleet))
                result
        StartPending entryId completion workerSlot -> do
            worker <-
                asyncWithUnmask \unmask ->
                    tryAny (unmask (start configs)) >>= \case
                        Left exception ->
                            pure (Left (exceptionSummary exception))
                        Right fleet -> pure (Right fleet)
            atomically $ void (tryPutTMVar workerSlot worker)
            mapM_ closeMcpFleet obsolete
            outcome <-
                restore (waitCatch worker)
                    `onException` do
                        cancel worker
                        void (waitCatch worker)
                        failPending entryId completion
                            "MCP fleet startup cancelled"
            case outcome of
                Left exception -> do
                    let err = exceptionSummary exception
                    failPending entryId completion err
                    ioError (userError (Text.unpack err))
                Right (Left err) -> do
                    failPending entryId completion err
                    ioError (userError (Text.unpack err))
                Right (Right fleet) -> do
                    accepted <-
                        publishPending entryId completion fleet
                            `onException` closeMcpFleet fleet
                    if accepted
                        then makeLease (entryId, fleet)
                        else do
                            closeMcpFleet fleet
                            ioError (userError "MCP supervisor closed")
  where
    makeLease :: (Int, McpFleet) -> IO McpFleetLease
    makeLease (entryId, fleet) = do
        released <- newIORef False
        let release =
                atomicModifyIORef' released (\done -> (True, done))
                    >>= \alreadyReleased ->
                        unless alreadyReleased $
                            releaseSupervisorEntry supervisor entryId
        pure McpFleetLease
            { mcpLeaseFleet = fleet
            , mcpLeaseRelease = release
            }

    findPending :: [McpSupervisorPending] -> Maybe McpSupervisorPending
    findPending = go
      where
        go
            :: [McpSupervisorPending]
            -> Maybe McpSupervisorPending
        go [] = Nothing
        go (pending : rest)
            | pending.supervisorPendingProgressive == progressive
            , sameServerConfigs pending.supervisorPendingConfigs configs =
                Just pending
            | otherwise = go rest

    replacePending
        :: McpSupervisorPending
        -> [McpSupervisorPending]
        -> [McpSupervisorPending]
    replacePending replacement =
        map \pending ->
            if pending.supervisorPendingId == replacement.supervisorPendingId
                then replacement
                else pending

    failPending
        :: Int
        -> TMVar (Either Text McpFleet)
        -> Text
        -> IO ()
    failPending entryId completion err =
        modifyMVar_ supervisor.supervisorState \state -> do
            atomically $ void (tryPutTMVar completion (Left err))
            pure state
                { supervisorPending =
                    filter
                        ((/= entryId) . (.supervisorPendingId))
                        state.supervisorPending
                }

    publishPending
        :: Int
        -> TMVar (Either Text McpFleet)
        -> McpFleet
        -> IO Bool
    publishPending entryId completion fleet =
        modifyMVar supervisor.supervisorState \state ->
            case find
                ((== entryId) . (.supervisorPendingId))
                state.supervisorPending of
                Nothing -> do
                    atomically $
                        void (tryPutTMVar completion
                            (Left "MCP supervisor closed"))
                    pure (state, False)
                Just pending
                    | state.supervisorClosed -> do
                        atomically $
                            void (tryPutTMVar completion
                                (Left "MCP supervisor closed"))
                        pure
                            ( state
                                { supervisorPending =
                                    filter
                                        ((/= entryId) . (.supervisorPendingId))
                                        state.supervisorPending
                                }
                            , False
                            )
                    | otherwise -> do
                        let entry = McpSupervisorEntry
                                { supervisorEntryId = entryId
                                , supervisorEntryProgressive = progressive
                                , supervisorEntryConfigs = configs
                                , supervisorEntryFleet = fleet
                                , supervisorEntryLeases =
                                    pending.supervisorPendingLeases
                                }
                        atomically $
                            void (tryPutTMVar completion (Right fleet))
                        pure
                            ( state
                                { supervisorEntries =
                                    entry : state.supervisorEntries
                                , supervisorPending =
                                    filter
                                        ((/= entryId) . (.supervisorPendingId))
                                        state.supervisorPending
                                }
                            , True
                            )

    findMatching
        :: [McpSupervisorEntry]
        -> IO (Maybe McpSupervisorEntry)
    findMatching = go
      where
        go :: [McpSupervisorEntry] -> IO (Maybe McpSupervisorEntry)
        go [] = pure Nothing
        go (entry : rest)
            | entry.supervisorEntryProgressive /= progressive
                || not
                    (sameServerConfigs
                        entry.supervisorEntryConfigs configs) =
                    go rest
            | otherwise = do
                statuses <- mcpFleetStatuses entry.supervisorEntryFleet
                if all statusReusable statuses
                    then pure (Just entry)
                    else go rest

    replaceEntry
        :: McpSupervisorEntry
        -> [McpSupervisorEntry]
        -> [McpSupervisorEntry]
    replaceEntry replacement =
        map \entry ->
            if entry.supervisorEntryId == replacement.supervisorEntryId
                then replacement
                else entry

    isIdle :: McpSupervisorEntry -> Bool
    isIdle entry = entry.supervisorEntryLeases == 0

    idleExcept :: Int -> [McpSupervisorEntry] -> [McpSupervisorEntry]
    idleExcept retainedId =
        filter \entry ->
            isIdle entry && entry.supervisorEntryId /= retainedId

    withoutEntries
        :: [McpSupervisorEntry]
        -> [McpSupervisorEntry]
        -> [McpSupervisorEntry]
    withoutEntries removed =
        filter \entry ->
            all
                ((/= entry.supervisorEntryId) . (.supervisorEntryId))
                removed

    partitionReusable
        :: [McpSupervisorEntry]
        -> IO ([McpSupervisorEntry], [McpSupervisorEntry])
    partitionReusable = go [] []
      where
        go
            :: [McpSupervisorEntry]
            -> [McpSupervisorEntry]
            -> [McpSupervisorEntry]
            -> IO ([McpSupervisorEntry], [McpSupervisorEntry])
        go healthy failed [] =
            pure (reverse healthy, reverse failed)
        go healthy failed (entry : rest) = do
            statuses <- mcpFleetStatuses entry.supervisorEntryFleet
            let reusable =
                    all
                        (\status -> case status.mcpStatusState of
                            McpFailed _ -> False
                            McpClosed -> False
                            _ -> True)
                        statuses
            if reusable || entry.supervisorEntryLeases > 0
                then go (entry : healthy) failed rest
                else go healthy (entry : failed) rest

    statusReusable :: McpServerStatus -> Bool
    statusReusable status = case status.mcpStatusState of
        McpFailed _ -> False
        McpClosed -> False
        _ -> True

releaseMcpFleetLease :: McpFleetLease -> IO ()
releaseMcpFleetLease = (.mcpLeaseRelease)

releaseSupervisorEntry :: McpSupervisor -> Int -> IO ()
releaseSupervisorEntry supervisor entryId = do
    modifyMVar_ supervisor.supervisorState \state ->
        pure state
            { supervisorEntries =
                map releaseOne state.supervisorEntries
            , supervisorPending =
                map releasePending state.supervisorPending
            }
  where
    releaseOne :: McpSupervisorEntry -> McpSupervisorEntry
    releaseOne entry
        | entry.supervisorEntryId == entryId =
            entry
                { supervisorEntryLeases =
                    max 0 (entry.supervisorEntryLeases - 1)
                }
        | otherwise = entry

    releasePending :: McpSupervisorPending -> McpSupervisorPending
    releasePending pending
        | pending.supervisorPendingId == entryId =
            pending
                { supervisorPendingLeases =
                    max 0 (pending.supervisorPendingLeases - 1)
                }
        | otherwise = pending

closeMcpSupervisor :: McpSupervisor -> IO ()
closeMcpSupervisor supervisor = do
    (fleets, pending) <-
        modifyMVar supervisor.supervisorState \state ->
            if state.supervisorClosed
                then pure (state, ([], []))
                else
                    pure
                        ( state
                            { supervisorClosed = True
                            , supervisorEntries = []
                            , supervisorPending = []
                            }
                        , ( map (.supervisorEntryFleet)
                                state.supervisorEntries
                          , state.supervisorPending
                          )
                        )
    atomically $
        mapM_
            (\entry ->
                void (tryPutTMVar entry.supervisorPendingResult
                    (Left "MCP supervisor closed")))
            pending
    forConcurrentlyBounded_ 8
        (\entry -> do
            worker <- atomically
                (readTMVar entry.supervisorPendingWorker)
            cancel worker
            void (waitCatch worker))
        pending
    forConcurrentlyBounded_ 4 closeMcpFleet fleets

resolveEffectiveCwds :: [McpServerConfig] -> IO [McpServerConfig]
resolveEffectiveCwds configs = do
    current <- getCurrentDirectory
    pure
        [ case config.mcpServerCwd of
            Just _ -> config
            Nothing -> config { mcpServerCwd = Just current }
        | config <- configs
        ]

sameServerConfigs :: [McpServerConfig] -> [McpServerConfig] -> Bool
sameServerConfigs left right =
    map normalize left == map normalize right
  where
    normalize config =
        config
            { mcpServerEnv = sortOn fst config.mcpServerEnv
            }

mcpFleetTools :: McpFleet -> [AppTool]
mcpFleetTools = map (.mcpRegistrationTool) . (.mcpFleetRegistrations)

-- | Snapshot server status without triggering initialization or other I/O.
mcpFleetStatuses :: McpFleet -> IO [McpServerStatus]
mcpFleetStatuses fleet = do
    clients <- Map.elems <$> readTVarIO fleet.mcpFleetClients
    clientStatuses <- mapM mcpClientStatus clients
    let byName =
            Map.fromList
                [ (status.mcpStatusName, status)
                | status <- clientStatuses
                ]
    pure
        [ case Map.lookup name byName of
            Just status -> status
            Nothing -> McpServerStatus
                { mcpStatusName = name
                , mcpStatusState =
                    maybe McpPending McpFailed
                        (Map.lookup name fleet.mcpFleetFailures)
                , mcpStatusToolCount = 0
                }
        | name <- fleet.mcpFleetServerOrder
        ]

data McpClient = McpClient
    { clientConfig :: !McpServerConfig
    , clientInput :: !Handle
    , clientProcess :: !ProcessHandle
    , clientGroupId :: !(Maybe ProcessGroupID)
    , clientNextId :: !(IORef Int)
    , clientPending :: !(TVar (Map.Map Int (TMVar (Either Text Value))))
    , clientFailure :: !(TVar (Maybe Text))
    , clientWriteLock :: !(MVar ())
    , clientStderr :: !(IORef CapturedStderr)
    , clientReader :: !(Async ())
    , clientStderrReader :: !(Async ())
    , clientClosed :: !(MVar Bool)
    , clientLifecycle :: !(TVar McpClientLifecycle)
    }

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
    , discoveredInputSchema :: !Value
    , discoveredReadOnly :: !Bool
    }

instance FromJSON McpTool where
    parseJSON = withObject "MCP tool" \fields -> do
        annotations <- fields .:? "annotations" .!= object []
        readOnly <- withObject "MCP tool annotations"
            (\values -> values .:? "readOnlyHint" .!= False)
            annotations
        McpTool
            <$> fields .: "name"
            <*> fields .:? "description" .!= ""
            <*> fields .:? "inputSchema" .!= emptyInputSchema
            <*> pure readOnly

emptyInputSchema :: Value
emptyInputSchema = object
    [ "type" .= ("object" :: Text)
    , "properties" .= object []
    , "additionalProperties" .= False
    ]

-- | Start every server independently. Ordinary server failures become
-- warnings so one unavailable integration does not disable healthy servers.
startMcpFleet :: [McpServerConfig] -> IO McpFleet
startMcpFleet = startMcpFleetWithProgress (const (pure ()))

-- | Start every server concurrently while reporting the configured names that
-- are still initializing. The callback is intended for startup UI and
-- deliberately receives no command arguments or environment values.
startMcpFleetWithProgress
    :: ([Text] -> IO ())
    -> [McpServerConfig]
    -> IO McpFleet
startMcpFleetWithProgress reportActive configs = mask \restore -> do
    validateServerNames configs
    closed <- newMVar False
    ownedClients <- newIORef []
    activeServers <- newMVar Set.empty
    results <-
        restore
            (mapConcurrently
                (startServerTracked ownedClients activeServers)
                configs)
            `onException` closeOwnedClients ownedClients
    let (clients, registrations, warnings, failures) =
            foldr collectServerResult ([], [], [], Map.empty) results
    catalog <- newTVarIO $
        Map.fromList
            [ (registration.mcpRegistrationTool.appToolName, McpCatalogEntry client tool)
            | Right (client, tools, _) <- results
            , tool <- tools
            , let registration = registrationFor client tool
            ]
    clientsVar <- newTVarIO $
        Map.fromList
            [ (client.clientConfig.mcpServerName, client)
            | client <- clients
            ]
    workers <- newMVar []
    reconnects <- Map.fromList <$> mapM
        (\config -> do
            lock <- newMVar ()
            pure (config.mcpServerName, lock))
        configs
    let
        fleet = McpFleet
            { mcpFleetRegistrations = registrations
            , mcpFleetWarnings = warnings
            , mcpFleetClients = clientsVar
            , mcpFleetServerOrder = map (.mcpServerName) configs
            , mcpFleetFailures = failures
            , mcpFleetCatalog = catalog
            , mcpFleetReconnects = reconnects
            , mcpFleetWorkers = workers
            , mcpFleetClosed = closed
            }
    pure fleet
  where
    startServerTracked ownedClients activeServers config = mask \restore -> do
        updateActive activeServers (Set.insert config.mcpServerName)
        (do
            attempt <- tryAny (restore (startServer config))
            case attempt of
                Left exception ->
                    let err =
                            redactConfiguredValues config
                                (exceptionSummary exception)
                    in pure
                        (Left
                            ( config
                            , startupWarningFromText config err
                            , err
                            ))
                Right result@(client, _, _) -> do
                    atomicModifyIORef' ownedClients \clients ->
                        (client : clients, ())
                    pure (Right result))
            `finally` updateActive activeServers (Set.delete config.mcpServerName)

    updateActive activeServers update =
        modifyMVar_ activeServers \current -> do
            let active = update current
            reportActive (Set.toAscList active)
            pure active

    closeOwnedClients ownedClients =
        atomicModifyIORef' ownedClients (\clients -> ([], clients))
            >>= mapM_ closeMcpClient

    collectServerResult
        :: Either (McpServerConfig, Text, Text)
                (McpClient, [McpTool], [Text])
            -> ( [McpClient]
               , [McpToolRegistration]
               , [Text]
               , Map.Map Text Text
               )
            -> ( [McpClient]
               , [McpToolRegistration]
               , [Text]
               , Map.Map Text Text
               )
    collectServerResult result
        (clients, registrations, warnings, failures) =
        case result of
            Left (config, warning, err) ->
                ( clients
                , registrations
                , warning : warnings
                , Map.insert config.mcpServerName err failures
                )
            Right (client, tools, serverWarnings) ->
                ( client : clients
                , map (registrationFor client) tools <> registrations
                , serverWarnings <> warnings
                , failures
                )

    startServer config = mask \restore -> do
        client <- startMcpClient config
        flip onException (closeMcpClient client) $ restore do
            ensureMcpClientReady client >>= \case
                Left err -> throwIO (userError (Text.unpack err))
                Right (tools, warnings) ->
                    pure (client, tools, warnings)

    registrationFor :: McpClient -> McpTool -> McpToolRegistration
    registrationFor client tool = McpToolRegistration
        { mcpRegistrationServer = client.clientConfig.mcpServerName
        , mcpRegistrationTool = appToolFor client tool
        }

    startupWarningFromText :: McpServerConfig -> Text -> Text
    startupWarningFromText config err =
        "MCP server "
            <> config.mcpServerName
            <> " failed to start: "
            <> err

validateServerNames :: [McpServerConfig] -> IO ()
validateServerNames = go Set.empty
  where
    go :: Set.Set Text -> [McpServerConfig] -> IO ()
    go _ [] = pure ()
    go seen (config : rest)
        | Set.member config.mcpServerName seen =
            ioError . userError . Text.unpack $
                "duplicate MCP server name: " <> config.mcpServerName
        | otherwise =
            go (Set.insert config.mcpServerName seen) rest

-- | Spawn configured stdio clients with bounded concurrency, then initialize
-- and discover each server in tracked background workers. The fleet can be
-- used immediately through 'mcpFleetMetaTools'.
startMcpFleetProgressive
    :: ([McpServerStatus] -> IO ())
    -> [McpServerConfig]
    -> IO McpFleet
startMcpFleetProgressive reportStatuses configs = mask \restore -> do
    validateServerNames configs
    closed <- newMVar False
    workers <- newMVar []
    catalog <- newTVarIO Map.empty
    ownedClients <- newIORef []
    clientsVar <- newTVarIO Map.empty
    reconnects <- Map.fromList <$> mapM
        (\config -> do
            lock <- newMVar ()
            pure (config.mcpServerName, lock))
        configs
    semaphore <- newQSem progressiveSpawnLimit
    spawnResults <-
        restore
            (mapConcurrently
                (startClientTracked ownedClients semaphore)
                configs)
            `onException` closeOwnedClients ownedClients
    let clients =
            [ client
            | Right client <- spawnResults
            ]
        failures =
            Map.fromList
                [ (config.mcpServerName, err)
                | (config, Left exception) <- zip configs spawnResults
                , let err =
                        redactConfiguredValues config
                            (exceptionSummary exception)
                ]
        warnings =
            [ "MCP server "
                <> config.mcpServerName
                <> " failed to start: "
                <> err
            | config <- configs
            , Just err <- [Map.lookup config.mcpServerName failures]
            ]
        fleet = McpFleet
            { mcpFleetRegistrations = []
            , mcpFleetWarnings = warnings
            , mcpFleetClients = clientsVar
            , mcpFleetServerOrder = map (.mcpServerName) configs
            , mcpFleetFailures = failures
            , mcpFleetCatalog = catalog
            , mcpFleetReconnects = reconnects
            , mcpFleetWorkers = workers
            , mcpFleetClosed = closed
            }
        initializeOne client = do
            void (reportFleetStatuses reportStatuses fleet)
            ensureMcpClientReadyWith
                (publishCatalogEntries catalog client)
                client >>= \case
                Left _ -> pure ()
                Right _ -> pure ()
            void (reportFleetStatuses reportStatuses fleet)
    atomically $
        writeTVar clientsVar $
            Map.fromList
                [ (client.clientConfig.mcpServerName, client)
                | client <- clients
                ]
    spawned <- newIORef []
    started <-
        (forM clients \client -> do
            worker <-
                asyncWithUnmask \unmask ->
                    unmask (initializeOne client)
            atomicModifyIORef' spawned \current ->
                (worker : current, ())
            pure worker)
            `onException`
                (readIORef spawned >>= mapM_ stopWorker)
    modifyMVar_ workers (pure . (started <>))
    void (reportFleetStatuses reportStatuses fleet)
    pure fleet
        `onException` closeMcpFleet fleet
  where
    startClientTracked ownedClients semaphore config = mask \restore -> do
        attempt <-
            bracket_
                (waitQSem semaphore)
                (signalQSem semaphore)
                (tryAny (restore (startMcpClient config)))
        case attempt of
            Left exception -> pure (Left exception)
            Right client -> do
                atomicModifyIORef' ownedClients \clients ->
                    (client : clients, ())
                pure (Right client)

    closeOwnedClients ownedClients =
        atomicModifyIORef' ownedClients (\clients -> ([], clients))
            >>= mapM_ closeMcpClient

    publishCatalogEntries catalog client tools =
        modifyTVar' catalog \current ->
            foldl
                (\entries tool ->
                    Map.insert
                        (qualifiedMcpToolName
                            client.clientConfig.mcpServerName
                            tool.discoveredName)
                        (McpCatalogEntry client tool)
                        entries)
                current
                tools

progressiveSpawnLimit :: Int
progressiveSpawnLimit = 8

reportFleetStatuses
    :: ([McpServerStatus] -> IO ())
    -> McpFleet
    -> IO [McpServerStatus]
reportFleetStatuses report fleet = do
    statuses <- mcpFleetStatuses fleet
    void (tryAny (report statuses))
    pure statuses

-- | Stable concise MCP tools backed by the fleet's background-populated
-- catalog. These schemas do not change as servers become ready.
mcpFleetMetaTools :: McpFleet -> [AppTool]
mcpFleetMetaTools fleet =
    [ mcpSearchTool fleet
    , mcpCallTool fleet
    ]

mcpFleetGrokMetaTools :: McpFleet -> [AppTool]
mcpFleetGrokMetaTools fleet =
    [ grokSearchTool fleet
    , grokUseTool fleet
    ]

mcpSearchTool :: McpFleet -> AppTool
mcpSearchTool fleet = AppTool
    { appToolName = "mcp_search"
    , appToolDescription =
        "Search currently available MCP tools. Servers may still be connecting."
    , appToolSchema = RawJsonFunctionSchema $ object
        [ "type" .= ("object" :: Text)
        , "properties" .= object
            [ "query" .= object ["type" .= ("string" :: Text)]
            , "server" .= object ["type" .= ("string" :: Text)]
            , "limit" .= object
                [ "type" .= ("integer" :: Text)
                , "minimum" .= (1 :: Int)
                , "maximum" .= (50 :: Int)
                ]
            ]
        , "additionalProperties" .= False
        ]
    , appToolHandler = typedTool "mcp_search" \arguments -> do
        entries <- readTVarIO fleet.mcpFleetCatalog
        statuses <- mcpFleetStatuses fleet
        let (query, server, limit) = searchArguments arguments
            matches :: (Text, McpCatalogEntry) -> Bool
            matches (name, entry) =
                maybe True
                    (\needle ->
                        Text.toCaseFold needle
                            `Text.isInfixOf`
                                Text.toCaseFold
                                    (name <> " "
                                        <> entry.catalogTool.discoveredDescription))
                    query
                    && maybe True
                        (== entry.catalogClient.clientConfig.mcpServerName)
                        server
            found = take limit (filter matches (Map.toAscList entries))
            payload = object
                [ "tools" .=
                    [ object
                        [ "name" .= name
                        , "server" .=
                            entry.catalogClient.clientConfig.mcpServerName
                        , "description" .=
                            entry.catalogTool.discoveredDescription
                        , "inputSchema" .=
                            entry.catalogTool.discoveredInputSchema
                        ]
                    | (name, entry) <- found
                    ]
                , "servers" .= map statusJson statuses
                ]
        pure (Right (compactJson payload))
    , appToolApproval = AlwaysReadOnly
    , appToolExecution = ParallelSafe
    , appToolResourceClaims = Nothing
    , appToolArgumentInterpreter = Nothing
    }

grokSearchTool :: McpFleet -> AppTool
grokSearchTool fleet = AppTool
    { appToolName = "search_tool"
    , appToolDescription =
        "Search for MCP tools by keyword and retrieve their input schemas.\n\n\
        \If status is \"partial\", some servers may still be connecting."
    , appToolSchema = RawJsonFunctionSchema $ object
        [ "type" .= ("object" :: Text)
        , "properties" .= object
            [ "query" .= object
                [ "type" .= ("string" :: Text)
                , "description" .=
                    ("Keywords to match against tool names, server names, and descriptions." :: Text)
                ]
            , "limit" .= object
                [ "type" .= ("integer" :: Text)
                , "minimum" .= (1 :: Int)
                , "maximum" .= (255 :: Int)
                , "description" .=
                    ("Maximum number of results to return (default 5)." :: Text)
                ]
            ]
        , "required" .= (["query"] :: [Text])
        , "additionalProperties" .= False
        ]
    , appToolHandler = typedTool "search_tool" \arguments ->
        case grokSearchArguments arguments of
            Left err -> pure (Left err)
            Right (query, limit) -> do
                entries <- readTVarIO fleet.mcpFleetCatalog
                statuses <- mcpFleetStatuses fleet
                let queryTokens = searchTokens query
                    scoreEntry :: (Text, McpCatalogEntry) -> Int
                    scoreEntry (name, entry) =
                        let normalizedName = normalizeSearchText name
                            normalizedServer =
                                normalizeSearchText
                                    entry.catalogClient.clientConfig.mcpServerName
                            normalizedDescription =
                                normalizeSearchText
                                    entry.catalogTool.discoveredDescription
                            haystack =
                                normalizedName
                                    <> " "
                                    <> normalizedServer
                                    <> " "
                                    <> normalizedDescription
                            tokenScore =
                                sum
                                    [ if token `Text.isInfixOf` normalizedName
                                        then 20
                                        else if token
                                            `Text.isInfixOf` normalizedServer
                                            then 10
                                            else 1
                                    | token <- queryTokens
                                    , token `Text.isInfixOf` haystack
                                    ]
                        in tokenScore
                    matches entry =
                        not (null queryTokens)
                            && scoreEntry entry > 0
                    ranked =
                        sortOn
                            (\entry ->
                                (Down (scoreEntry entry), fst entry))
                            (filter matches (Map.toAscList entries))
                    found = take limit ranked
                    grouped =
                        foldl'
                            (\current pair@(name, entry) ->
                                let server =
                                        entry.catalogClient.clientConfig.mcpServerName
                                    toolJson = object
                                        [ "tool_name" .= name
                                        , "description" .=
                                            truncateMcpDescription
                                                entry.catalogTool.discoveredDescription
                                        , "score" .= scoreEntry pair
                                        , "input_schema" .=
                                            entry.catalogTool.discoveredInputSchema
                                        ]
                                    (before, rest) =
                                        break ((== server) . fst) current
                                in case rest of
                                    [] ->
                                        current <> [(server, [toolJson])]
                                    (matchedServer, tools) : after ->
                                        before
                                            <> [ ( matchedServer
                                                 , tools <> [toolJson]
                                                 )
                                               ]
                                            <> after)
                            []
                            found
                    connecting = any isConnecting statuses
                    payload = object
                        [ "results" .=
                            [ object
                                [ "server" .= server
                                , "tools" .= tools
                                ]
                            | (server, tools) <- grouped
                            ]
                        , "total_hidden_tools" .= Map.size entries
                        , "status" .=
                            (if connecting then ("partial" :: Text) else "ready")
                        , "note" .=
                            if connecting
                                then Just
                                    ("Some MCP servers are still connecting. Results may be incomplete." :: Text)
                                else if Map.null entries
                                    then Just
                                        "No MCP tools are available in this session."
                                    else Nothing
                        ]
                pure (Right (compactJson payload))
    , appToolApproval = AlwaysReadOnly
    , appToolExecution = ParallelSafe
    , appToolResourceClaims = Nothing
    , appToolArgumentInterpreter = Nothing
    }

callCatalogEntryWithReconnect
    :: McpFleet
    -> Text
    -> McpCatalogEntry
    -> Value
    -> IO (Either Text Text)
callCatalogEntryWithReconnect fleet qualifiedName entry arguments =
    callDiscoveredTool entry.catalogClient entry.catalogTool arguments
        >>= \case
            Right result -> pure (Right result)
            Left originalError -> do
                failed <- readTVarIO entry.catalogClient.clientFailure
                case failed of
                    Nothing -> pure (Left originalError)
                    Just _ ->
                        -- Stable meta-tool handlers can transparently replace
                        -- a failed stdio transport and retry the read-only
                        -- call once. The per-server lock makes this
                        -- single-flight across concurrent calls.
                        reconnectCatalogEntry fleet qualifiedName entry
                            >>= \case
                                Left reconnectError ->
                                    pure . Left $
                                        originalError
                                            <> "; MCP reconnect failed: "
                                            <> reconnectError
                                Right replacement ->
                                    callDiscoveredTool
                                        replacement.catalogClient
                                        replacement.catalogTool
                                        arguments

reconnectCatalogEntry
    :: McpFleet
    -> Text
    -> McpCatalogEntry
    -> IO (Either Text McpCatalogEntry)
reconnectCatalogEntry fleet qualifiedName failedEntry =
    case Map.lookup serverName fleet.mcpFleetReconnects of
        Nothing -> pure (Left "MCP server is not supervised")
        Just reconnectLock ->
            withMVar reconnectLock \_ ->
                withMVar fleet.mcpFleetClosed \closed ->
                    if closed
                        then pure (Left "MCP server closed")
                        else do
                            current <- readTVarIO fleet.mcpFleetCatalog
                            case Map.lookup qualifiedName current of
                                Just replacement
                                    | replacement.catalogClient.clientFailure
                                        /= failedEntry.catalogClient.clientFailure ->
                                            pure (Right replacement)
                                _ -> restart current
  where
    serverName =
        failedEntry.catalogClient.clientConfig.mcpServerName
    config = failedEntry.catalogClient.clientConfig

    restart _ = do
        started <- tryAny (startMcpClient config)
        case started of
            Left exception ->
                pure . Left $
                    redactConfiguredValues config
                        (exceptionSummary exception)
            Right replacementClient ->
                ensureMcpClientReady replacementClient >>= \case
                    Left err -> do
                        closeMcpClient replacementClient
                        pure (Left err)
                    Right (tools, _) -> do
                        let replacementEntries =
                                Map.fromList
                                    [ ( qualifiedMcpToolName
                                            serverName tool.discoveredName
                                      , McpCatalogEntry replacementClient tool
                                      )
                                    | tool <- tools
                                    ]
                        previousClient <- atomically do
                            clients <- readTVar fleet.mcpFleetClients
                            currentCatalog <- readTVar fleet.mcpFleetCatalog
                            let withoutServer =
                                    Map.filter
                                        ((/= serverName)
                                            . (.clientConfig.mcpServerName)
                                            . (.catalogClient))
                                        currentCatalog
                            writeTVar fleet.mcpFleetClients
                                (Map.insert serverName replacementClient clients)
                            writeTVar fleet.mcpFleetCatalog
                                (replacementEntries <> withoutServer)
                            pure (Map.lookup serverName clients)
                        mapM_ closeMcpClient previousClient
                        case Map.lookup qualifiedName replacementEntries of
                            Nothing ->
                                pure (Left "MCP tool disappeared after reconnect")
                            Just replacement -> pure (Right replacement)

mcpCallTool :: McpFleet -> AppTool
mcpCallTool fleet = AppTool
    { appToolName = "mcp_call"
    , appToolDescription =
        "Call a currently available read-only MCP tool by its qualified server__tool name."
    , appToolSchema = RawJsonFunctionSchema $ object
        [ "type" .= ("object" :: Text)
        , "properties" .= object
            [ "name" .= object ["type" .= ("string" :: Text)]
            , "arguments" .= object ["type" .= ("object" :: Text)]
            ]
        , "required" .= (["name"] :: [Text])
        , "additionalProperties" .= False
        ]
    , appToolHandler = typedTool "mcp_call" \arguments ->
        case callArguments arguments of
            Left err -> pure (Left err)
            Right (name, toolArguments) -> do
                entries <- readTVarIO fleet.mcpFleetCatalog
                case Map.lookup name entries of
                    Just entry ->
                        callCatalogEntryWithReconnect
                            fleet name entry toolArguments
                    Nothing -> do
                        statuses <- mcpFleetStatuses fleet
                        pure . Left $
                            if any isConnecting statuses
                                then
                                    "MCP tool is not available yet; one or more servers are still connecting"
                                else "Unknown MCP tool: " <> name
    , appToolApproval = AlwaysReadOnly
    , appToolExecution = ParallelSafe
    , appToolResourceClaims = Nothing
    , appToolArgumentInterpreter = Nothing
    }

grokUseTool :: McpFleet -> AppTool
grokUseTool fleet = AppTool
    { appToolName = "use_tool"
    , appToolDescription =
        "Call an MCP integration tool.\n\n\
        \The `tool_name` must be the qualified `server__tool` name returned by \
        \`search_tool`. The `tool_input` must conform exactly to that tool's \
        \input schema."
    , appToolSchema = RawJsonFunctionSchema $ object
        [ "type" .= ("object" :: Text)
        , "properties" .= object
            [ "tool_name" .= object ["type" .= ("string" :: Text)]
            , "tool_input" .= object
                [ "type" .= ("object" :: Text)
                , "additionalProperties" .= True
                ]
            ]
        , "required" .= (["tool_name", "tool_input"] :: [Text])
        , "additionalProperties" .= False
        ]
    , appToolHandler = typedTool "use_tool" \arguments ->
        case grokCallArguments arguments of
            Left err -> pure (Left err)
            Right (name, toolArguments) -> do
                entries <- readTVarIO fleet.mcpFleetCatalog
                case Map.lookup name entries of
                    Just entry ->
                        callCatalogEntryWithReconnect
                            fleet name entry toolArguments
                    Nothing -> do
                        statuses <- mcpFleetStatuses fleet
                        pure . Left $
                            if any isConnecting statuses
                                then
                                    "MCP tool is not available yet; one or more servers are still connecting"
                                else "Unknown MCP tool: " <> name
    , appToolApproval = AlwaysReadOnly
    , appToolExecution = ParallelSafe
    , appToolResourceClaims = Nothing
    , appToolArgumentInterpreter = Nothing
    }

searchArguments :: Value -> (Maybe Text, Maybe Text, Int)
searchArguments (Object fields) =
    ( textField "query"
    , textField "server"
    , case KeyMap.lookup "limit" fields >>= responseId of
        Just value -> max 1 (min 50 value)
        Nothing -> 20
    )
  where
    textField name = case KeyMap.lookup name fields of
        Just (String value)
            | not (Text.null (Text.strip value)) -> Just (Text.strip value)
        _ -> Nothing
searchArguments _ = (Nothing, Nothing, 20)

grokSearchArguments :: Value -> Either Text (Text, Int)
grokSearchArguments (Object fields) =
    case KeyMap.lookup "query" fields of
        Just (String raw)
            | not (Text.null (Text.strip raw)) -> do
                limit <- case KeyMap.lookup "limit" fields of
                    Nothing -> Right 5
                    Just Null -> Right 5
                    Just value ->
                        case responseId value of
                            Just parsed
                                | parsed >= 1 && parsed <= 255 ->
                                    Right parsed
                            _ ->
                                Left
                                    "search_tool limit must be an integer from 1 through 255"
                Right (Text.strip raw, limit)
        _ -> Left "search_tool requires a non-empty query"
grokSearchArguments _ =
    Left "search_tool arguments must be an object"

searchTokens :: Text -> [Text]
searchTokens =
    Text.words . normalizeSearchText

normalizeSearchText :: Text -> Text
normalizeSearchText =
    Text.unwords
        . Text.words
        . Text.map
            (\character ->
                if isAlphaNum character then character else ' ')
        . Text.toCaseFold

truncateMcpDescription :: Text -> Text
truncateMcpDescription description
    | Text.length description <= 2048 = description
    | otherwise = Text.take 2034 description <> "… [truncated]"

callArguments :: Value -> Either Text (Text, Value)
callArguments (Object fields) =
    case KeyMap.lookup "name" fields of
        Just (String name)
            | not (Text.null (Text.strip name)) ->
                Right
                    ( Text.strip name
                    , maybe (object []) id (KeyMap.lookup "arguments" fields)
                    )
        _ -> Left "mcp_call requires a non-empty name"
callArguments _ = Left "mcp_call arguments must be an object"

grokCallArguments :: Value -> Either Text (Text, Value)
grokCallArguments (Object fields) =
    case KeyMap.lookup "tool_name" fields of
        Just (String name)
            | not (Text.null (Text.strip name))
            , "__" `Text.isInfixOf` name ->
                case KeyMap.lookup "tool_input" fields of
                    Just value@(Object _) -> Right (Text.strip name, value)
                    Nothing ->
                        Left "use_tool requires tool_input"
                    _ -> Left "use_tool tool_input must be an object"
            | not (Text.null (Text.strip name)) ->
                Left
                    "use_tool tool_name must be a qualified server__tool name returned by search_tool"
        _ -> Left "use_tool requires a non-empty tool_name"
grokCallArguments _ = Left "use_tool arguments must be an object"

statusJson :: McpServerStatus -> Value
statusJson status = object
    [ "name" .= status.mcpStatusName
    , "status" .= case status.mcpStatusState of
        McpPending -> ("pending" :: Text)
        McpInitializing -> "initializing"
        McpReady -> "ready"
        McpFailed _ -> "failed"
        McpClosed -> "closed"
    , "toolCount" .= status.mcpStatusToolCount
    ]

isConnecting :: McpServerStatus -> Bool
isConnecting status = case status.mcpStatusState of
    McpPending -> True
    McpInitializing -> True
    _ -> False

closeMcpFleet :: McpFleet -> IO ()
closeMcpFleet fleet =
    modifyMVar_ fleet.mcpFleetClosed \closed ->
        if closed
            then pure True
            else do
                activeWorkers <-
                    modifyMVar fleet.mcpFleetWorkers \workers ->
                        pure ([], workers)
                forConcurrentlyBounded_ 8 stopWorker activeWorkers
                clients <- Map.elems <$> readTVarIO fleet.mcpFleetClients
                forConcurrentlyBounded_ 8 closeMcpClient clients
                pure True

startMcpClient :: McpServerConfig -> IO McpClient
startMcpClient config = mask \_ -> do
    processEnvironment <- mergedEnvironment config.mcpServerEnv
    let processSpec =
            (proc config.mcpServerCommand config.mcpServerArgs)
                { cwd = config.mcpServerCwd
                , env = Just processEnvironment
                , std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = CreatePipe
                , create_group = True
                }
    created <- createProcess processSpec
    case created of
        (Just input, Just output, Just errOutput, processHandle) -> do
            groupId <- getPid processHandle
            hSetBinaryMode input True
            hSetBinaryMode output True
            hSetBinaryMode errOutput True
            hSetBuffering input LineBuffering
            nextId <- newIORef 1
            pending <- newTVarIO Map.empty
            failure <- newTVarIO Nothing
            writeLock <- newMVar ()
            stderrRef <- newIORef emptyCapturedStderr
            closed <- newMVar False
            lifecycle <- newTVarIO ClientPending
            reader <- asyncWithUnmask \unmask ->
                unmask (readerLoop output pending failure)
                    `finally` void (tryAny (hClose output))
            stderrReader <- asyncWithUnmask \unmask ->
                unmask (stderrLoop errOutput stderrRef)
                    `finally` void (tryAny (hClose errOutput))
            let client = McpClient
                    { clientConfig = config
                    , clientInput = input
                    , clientProcess = processHandle
                    , clientGroupId = groupId
                    , clientNextId = nextId
                    , clientPending = pending
                    , clientFailure = failure
                    , clientWriteLock = writeLock
                    , clientStderr = stderrRef
                    , clientReader = reader
                    , clientStderrReader = stderrReader
                    , clientClosed = closed
                    , clientLifecycle = lifecycle
                    }
            pure client
        _ -> do
            let (_, _, _, processHandle) = created
            groupId <- getPid processHandle
            terminateProcessGroup groupId processHandle
            closeOptionalHandles created
            ioError (userError "MCP server did not provide all stdio pipes")

data InitializeRole
    = InitializeLeader
        !(TMVar (Either Text ([McpTool], [Text])))
    | InitializeWaiter
        !(TMVar (Either Text ([McpTool], [Text])))
    | InitializeComplete
        !(Either Text ([McpTool], [Text]))

-- | Initialize and discover one client exactly once. Concurrent callers wait
-- on the same result. If the leader is cancelled, waiters are released and
-- the partially initialized stdio client becomes terminally failed.
ensureMcpClientReady
    :: McpClient
    -> IO (Either Text ([McpTool], [Text]))
ensureMcpClientReady = ensureMcpClientReadyWith (const (pure ()))

ensureMcpClientReadyWith
    :: ([McpTool] -> STM ())
    -> McpClient
    -> IO (Either Text ([McpTool], [Text]))
ensureMcpClientReadyWith publishReady client = mask \restore -> do
    role <- atomically do
        readTVar client.clientLifecycle >>= \case
            ClientPending -> do
                completion <- newEmptyTMVar
                writeTVar client.clientLifecycle
                    (ClientInitializing completion)
                pure (InitializeLeader completion)
            ClientInitializing completion ->
                pure (InitializeWaiter completion)
            ClientReady tools warnings ->
                pure (InitializeComplete (Right (tools, warnings)))
            ClientFailed err ->
                pure (InitializeComplete (Left err))
            ClientClosed ->
                pure (InitializeComplete (Left "MCP server closed"))
    case role of
        InitializeComplete result -> pure result
        InitializeWaiter completion ->
            restore (atomically (readTMVar completion))
        InitializeLeader completion -> do
            let cancelled = do
                    atomically do
                        state <- readTVar client.clientLifecycle
                        case state of
                            ClientInitializing current
                                | current == completion ->
                                    writeTVar client.clientLifecycle
                                        (ClientFailed
                                            "MCP initialization cancelled")
                            _ -> pure ()
                        void $
                            tryPutTMVar completion
                                (Left "MCP initialization cancelled")
                    closeMcpClient client
                initialize = do
                    initializeClient client
                    discoverMcpTools client
            outcome <-
                restore (tryAny initialize)
                    `onException` cancelled
            let result = case outcome of
                    Left exception ->
                        Left
                            (redactConfiguredValues client.clientConfig
                                (exceptionSummary exception))
                    Right ready -> Right ready
            atomically do
                state <- readTVar client.clientLifecycle
                case state of
                    ClientClosed ->
                        void $
                            tryPutTMVar completion
                                (Left "MCP server closed")
                    ClientInitializing current
                        | current == completion -> do
                            case result of
                                Left err ->
                                    writeTVar client.clientLifecycle
                                        (ClientFailed err)
                                Right (tools, warnings) -> do
                                    publishReady tools
                                    writeTVar client.clientLifecycle
                                        (ClientReady tools warnings)
                            void (tryPutTMVar completion result)
                    _ -> void (tryPutTMVar completion result)
            pure result

mcpClientStatus :: McpClient -> IO McpServerStatus
mcpClientStatus client = do
    state <- readTVarIO client.clientLifecycle
    transportFailure <- readTVarIO client.clientFailure
    pure McpServerStatus
        { mcpStatusName = client.clientConfig.mcpServerName
        , mcpStatusState = case (state, transportFailure) of
            (ClientClosed, _) -> McpClosed
            (_, Just err) -> McpFailed err
            (ClientPending, _) -> McpPending
            (ClientInitializing _, _) -> McpInitializing
            (ClientReady _ _, _) -> McpReady
            (ClientFailed err, _) -> McpFailed err
        , mcpStatusToolCount = case state of
            ClientReady tools _ -> length tools
            _ -> 0
        }

initializeClient :: McpClient -> IO ()
initializeClient client = do
    let timeoutMicros =
            secondsToMicros client.clientConfig.mcpServerStartupTimeoutSeconds
        parameters = object
            [ "protocolVersion" .= ("2025-11-25" :: Text)
            , "capabilities" .= object []
            , "clientInfo" .= object
                [ "name" .= ("haskell-agent" :: Text)
                , "version" .= ("0.1.0" :: Text)
                ]
            ]
    result <- requestMcp client timeoutMicros "initialize" parameters
    case result of
        Left err -> startupFailure client err
        Right _ ->
            sendNotification client "notifications/initialized" (object [])
                >>= either (startupFailure client) pure

startupFailure :: McpClient -> Text -> IO a
startupFailure client err = do
    stderrText <- capturedStderrText <$> readIORef client.clientStderr
    ioError . userError . Text.unpack $
        redactConfiguredValues client.clientConfig
            (err <> if Text.null stderrText then "" else "\nstderr:\n" <> stderrText)

discoverMcpTools :: McpClient -> IO ([McpTool], [Text])
discoverMcpTools client = go Nothing [] []
  where
    go cursor tools warnings = do
        let parameters = maybe (object []) (\value -> object ["cursor" .= value]) cursor
            timeoutMicros =
                secondsToMicros client.clientConfig.mcpServerRequestTimeoutSeconds
        requestMcp client timeoutMicros "tools/list" parameters >>= \case
            Left err -> ioError (userError (Text.unpack err))
            Right result ->
                case AesonTypes.parseEither parsePage result of
                    Left err -> ioError (userError ("invalid tools/list response: " <> err))
                    Right (pageTools, nextCursor) -> do
                        let (readOnlyTools, skipped) =
                                foldr classify ([], []) pageTools
                            skippedWarnings =
                                [ "MCP server "
                                    <> client.clientConfig.mcpServerName
                                    <> " skipped non-read-only tool "
                                    <> tool.discoveredName
                                | tool <- skipped
                                ]
                        if isJust nextCursor
                            then go nextCursor
                                (tools <> readOnlyTools)
                                (warnings <> skippedWarnings)
                            else pure
                                (tools <> readOnlyTools, warnings <> skippedWarnings)

    classify
        :: McpTool
        -> ([McpTool], [McpTool])
        -> ([McpTool], [McpTool])
    classify tool (allowed, skipped)
        | tool.discoveredReadOnly = (tool : allowed, skipped)
        | otherwise = (allowed, tool : skipped)

    parsePage :: Value -> AesonTypes.Parser ([McpTool], Maybe Value)
    parsePage = withObject "tools/list result" \fields ->
        (,)
            <$> fields .:? "tools" .!= []
            <*> fields .:? "nextCursor"

appToolFor :: McpClient -> McpTool -> AppTool
appToolFor client tool = AppTool
    { appToolName = qualifiedName
    , appToolDescription = tool.discoveredDescription
    , appToolSchema = RawJsonFunctionSchema tool.discoveredInputSchema
    , appToolHandler =
        typedTool qualifiedName \arguments -> do
            callDiscoveredTool client tool arguments
    , appToolApproval = AlwaysReadOnly
    , appToolExecution = ParallelSafe
    , appToolResourceClaims = Nothing
    , appToolArgumentInterpreter = Nothing
    }
  where
    qualifiedName = qualifiedMcpToolName
        client.clientConfig.mcpServerName
        tool.discoveredName

qualifiedMcpToolName :: Text -> Text -> Text
qualifiedMcpToolName serverName toolName =
    escapeComponent serverName <> "__" <> escapeComponent toolName
  where
    escapeComponent =
        Text.replace "__" "%5F%5F"
            . Text.replace "%" "%25"

callDiscoveredTool :: McpClient -> McpTool -> Value -> IO (Either Text Text)
callDiscoveredTool client tool arguments = do
    let parameters = object
            [ "name" .= tool.discoveredName
            , "arguments" .= arguments
            ]
        timeoutMicros =
            secondsToMicros
                client.clientConfig.mcpServerRequestTimeoutSeconds
    requestMcp client timeoutMicros "tools/call" parameters >>= \case
        Left err -> pure (Left err)
        Right result -> pure (normalizeMcpToolResult result)

normalizeMcpToolResult :: Value -> Either Text Text
normalizeMcpToolResult result@(Object fields) =
    let isError = case KeyMap.lookup "isError" fields of
            Just (Bool value) -> value
            _ -> False
        structured = KeyMap.lookup "structuredContent" fields
        textParts = maybe [] extractTextParts (KeyMap.lookup "content" fields)
        output
            | isJust structured && not (null textParts) = compactJson result
            | Just value <- structured = compactJson value
            | not (null textParts) = Text.intercalate "\n" textParts
            | otherwise = compactJson result
    in if isError then Left output else Right output
normalizeMcpToolResult result = Right (compactJson result)

extractTextParts :: Value -> [Text]
extractTextParts (Array items) =
    [ text
    | Object item <- Vector.toList items
    , Just (String "text") <- [KeyMap.lookup "type" item]
    , Just (String text) <- [KeyMap.lookup "text" item]
    ]
extractTextParts _ = []

compactJson :: Value -> Text
compactJson = TextEncoding.decodeUtf8 . LBS.toStrict . Aeson.encode

requestMcp
    :: McpClient
    -> Int
    -> Text
    -> Value
    -> IO (Either Text Value)
requestMcp client timeoutMicros method parameters = do
    failed <- readTVarIO client.clientFailure
    case failed of
        Just err -> pure (Left err)
        Nothing -> do
            requestId <- atomicModifyIORef' client.clientNextId \current ->
                (current + 1, current)
            response <- newEmptyTMVarIO
            atomically $
                modifyTVar' client.clientPending (Map.insert requestId response)
            let message = object
                    [ "jsonrpc" .= ("2.0" :: Text)
                    , "id" .= requestId
                    , "method" .= method
                    , "params" .= parameters
                    ]
            sendMessage client message >>= \case
                Left err -> do
                    atomically $
                        modifyTVar' client.clientPending (Map.delete requestId)
                    pure (Left err)
                Right () -> do
                    timed <- timeout (max 1 timeoutMicros)
                        (atomically (takeTMVar response))
                    case timed of
                        Just value -> pure value
                        Nothing -> do
                            atomically $
                                modifyTVar' client.clientPending
                                    (Map.delete requestId)
                            pure . Left $
                                "MCP request "
                                    <> method
                                    <> " timed out after "
                                    <> Text.pack
                                        (show
                                            ((timeoutMicros + 999999) `div` 1000000))
                                    <> " seconds"

sendNotification :: McpClient -> Text -> Value -> IO (Either Text ())
sendNotification client method parameters =
    sendMessage client $ object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "method" .= method
        , "params" .= parameters
        ]

sendMessage :: McpClient -> Value -> IO (Either Text ())
sendMessage client message =
    tryAny
        (withMVar client.clientWriteLock \_ -> do
            LBS.hPutStr client.clientInput (Aeson.encode message <> "\n")
            hFlush client.clientInput)
        >>= \case
            Left exception -> do
                let err = "MCP write failed: " <> exceptionSummary exception
                failClient client.clientPending client.clientFailure err
                pure (Left err)
            Right () -> pure (Right ())

readerLoop
    :: Handle
    -> TVar (Map.Map Int (TMVar (Either Text Value)))
    -> TVar (Maybe Text)
    -> IO ()
readerLoop output pending failure =
    loop `finally` failPending pending failure "MCP server stdout closed"
  where
    loop = do
        line <- BS8.hGetLine output
        unless (BS.null line) $
            case Aeson.eitherDecodeStrict' line of
                Left err ->
                    failPending pending failure
                        ("Invalid MCP JSON response: " <> Text.pack err)
                Right value ->
                    routeResponse pending value
        loop

routeResponse
    :: TVar (Map.Map Int (TMVar (Either Text Value)))
    -> Value
    -> IO ()
routeResponse pending (Object fields) =
    case KeyMap.lookup "id" fields >>= responseId of
        Nothing -> pure ()
        Just ident -> do
            destination <- atomically do
                current <- readTVar pending
                writeTVar pending (Map.delete ident current)
                pure (Map.lookup ident current)
            case destination of
                Nothing -> pure ()
                Just response ->
                    atomically . void . tryPutTMVar response $
                        case KeyMap.lookup "error" fields of
                            Just err -> Left ("MCP error: " <> compactJson err)
                            Nothing -> case KeyMap.lookup "result" fields of
                                Just result -> Right result
                                Nothing -> Left "MCP response omitted result"
routeResponse _ _ = pure ()

responseId :: Value -> Maybe Int
responseId (Number value) =
    case floatingOrInteger value of
        Right integer -> Just integer
        Left (_ :: Double) -> Nothing
responseId _ = Nothing

stderrLoop :: Handle -> IORef CapturedStderr -> IO ()
stderrLoop handle captured =
    let loop = do
            chunk <- BS.hGetSome handle 4096
            if BS.null chunk
                then pure ()
                else appendStderr captured chunk >> loop
    in loop

appendStderr :: IORef CapturedStderr -> BS.ByteString -> IO ()
appendStderr ref chunk =
    atomicModifyIORef' ref \current ->
        let combined = current.stderrBytes <> chunk
            overflow = max 0 (BS.length combined - stderrLimit)
            kept = BS.drop overflow combined
        in ( CapturedStderr
                { stderrBytes = kept
                , stderrDropped = current.stderrDropped + overflow
                }
           , ()
           )

capturedStderrText :: CapturedStderr -> Text
capturedStderrText captured =
    let body =
            TextEncoding.decodeUtf8With lenientDecode captured.stderrBytes
    in if captured.stderrDropped <= 0
        then body
        else
            "[... "
                <> Text.pack (show captured.stderrDropped)
                <> " stderr bytes omitted ...]\n"
                <> body

failClient
    :: TVar (Map.Map Int (TMVar (Either Text Value)))
    -> TVar (Maybe Text)
    -> Text
    -> IO ()
failClient = failPending

failPending
    :: TVar (Map.Map Int (TMVar (Either Text Value)))
    -> TVar (Maybe Text)
    -> Text
    -> IO ()
failPending pending failure err =
    atomically do
        existing <- readTVar failure
        when (existing == Nothing) (writeTVar failure (Just err))
        requests <- readTVar pending
        writeTVar pending Map.empty
        mapM_ (\response -> void (tryPutTMVar response (Left err)))
            (Map.elems requests)

closeMcpClient :: McpClient -> IO ()
closeMcpClient client =
    modifyMVar_ client.clientClosed \closed ->
        if closed
            then pure True
            else do
                atomically do
                    readTVar client.clientLifecycle >>= \case
                        ClientInitializing completion -> do
                            writeTVar client.clientLifecycle ClientClosed
                            void $
                                tryPutTMVar completion
                                    (Left "MCP server closed")
                        _ ->
                            writeTVar client.clientLifecycle ClientClosed
                void $ tryAny (hClose client.clientInput)
                terminateProcessGroup client.clientGroupId client.clientProcess
                stopWorker client.clientReader
                stopWorker client.clientStderrReader
                failClient client.clientPending client.clientFailure
                    "MCP server closed"
                pure True

stopWorker :: Async () -> IO ()
stopWorker worker = do
    cancel worker
    void (waitCatch worker)

closeOptionalHandles
    :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
    -> IO ()
closeOptionalHandles (input, output, errOutput, _) =
    mapM_ (\handle -> void (tryAny (hClose handle)))
        (catMaybes [input, output, errOutput])

mergedEnvironment :: [(String, String)] -> IO [(String, String)]
mergedEnvironment overrides = do
    inherited <- getEnvironment
    pure . Map.toList $
        foldl
            (\environment (name, value) -> Map.insert name value environment)
            (Map.fromList inherited)
            overrides

secondsToMicros :: Int -> Int
secondsToMicros seconds = max 1 seconds * 1000000

exceptionSummary :: SomeException -> Text
exceptionSummary =
    Text.take 1000
        . fst
        . Text.breakOn "\nHasCallStack backtrace:"
        . Text.pack
        . displayException

redactConfiguredValues :: McpServerConfig -> Text -> Text
redactConfiguredValues config input =
    foldl redact input (map (Text.pack . snd) config.mcpServerEnv)
  where
    redact current secret
        | Text.null secret = current
        | otherwise = Text.replace secret "<redacted>" current
