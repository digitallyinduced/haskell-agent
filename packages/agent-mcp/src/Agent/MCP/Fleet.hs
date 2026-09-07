-- | A fleet of MCP servers: concurrent startup, the tool catalog shared with
-- the model, meta-tools for progressive discovery, and reconnection.
module Agent.MCP.Fleet where

import Agent.Json
    ( RawJson
    , rawJsonBytes
    , rawJsonDecoder
    , rawJsonFromEncoding
    )
import qualified Agent.Json.Decode as Json
import Agent.MCP.Client
import Agent.MCP.Types
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRequirement(..)
    , ApprovalRule(..)
    , ToolAsyncCapability(..)
    , ToolExecutionPolicy(..)
    , ToolSchema(..)
    )
import Agent.ToolDispatch (ToolCall(..), typedTool, typedToolWithCall)
import Agent.Concurrent (forConcurrentlyBounded_)
import Control.Concurrent.Async
    ( asyncWithUnmask
    , concurrently
    , mapConcurrently
    , poll
    )
import Control.Concurrent.QSem
    ( newQSem
    , signalQSem
    , waitQSem
    )
import Control.Concurrent.MVar
    ( modifyMVar
    , modifyMVar_
    , newMVar
    , withMVar
    )
import Control.Concurrent.STM
    ( STM
    , TVar
    , atomically
    , check
    , modifyTVar'
    , newTQueueIO
    , newTVarIO
    , readTQueue
    , readTVar
    , readTVarIO
    , writeTQueue
    , writeTVar
    )
import Control.Exception.Safe
    ( bracket_
    , finally
    , mask
    , mask_
    , onException
    , throwIO
    , tryAny
    )
import Control.Monad (forM, forM_, unless, void, when)
import Data.Aeson
    ( Value
    , object
    , (.=)
    )
import qualified Data.Aeson as Aeson
import Data.Char (isAlphaNum)
import Data.IORef
    ( atomicModifyIORef'
    , newIORef
    , readIORef
    )
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isJust)
import Data.Ord (Down(..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import System.Directory (getCurrentDirectory)

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

-- | Snapshot the metadata advertised by every Skills-over-MCP server.  The
-- server name is deliberately retained alongside each URI: URIs are only
-- unique within an MCP server.
mcpFleetSkillRegistrations :: McpFleet -> IO [McpSkillRegistration]
mcpFleetSkillRegistrations fleet = readTVarIO fleet.mcpFleetSkills

-- | Wait until the fleet's advertised skill registrations differ from a
-- previously observed snapshot.  The wait is interruptible, so callers can
-- own it with their existing structured worker lifecycle.
mcpFleetWaitForSkillRegistrations
    :: McpFleet
    -> [McpSkillRegistration]
    -> IO [McpSkillRegistration]
mcpFleetWaitForSkillRegistrations fleet previous =
    atomically do
        current <- readTVar fleet.mcpFleetSkills
        check (current /= previous)
        pure current

mcpFleetGetSkill :: McpFleet -> Text -> Text -> IO (Either Text McpSkillEntry)
mcpFleetGetSkill fleet server uri =
    withFleetClient fleet server \client -> getMcpSkill client uri

mcpFleetReadResource
    :: McpFleet -> Text -> Text -> IO (Either Text [McpResourceContent])
mcpFleetReadResource fleet server uri =
    withFleetClient fleet server \client -> readMcpResource client uri

mcpFleetListResources :: McpFleet -> Text -> IO (Either Text [McpResource])
mcpFleetListResources fleet server =
    withFleetClient fleet server \client ->
        either (Left . renderMcpError) Right <$> listMcpResources client

mcpFleetListResourceTemplates
    :: McpFleet -> Text -> IO (Either Text [McpResourceTemplate])
mcpFleetListResourceTemplates fleet server =
    withFleetClient fleet server \client ->
        either (Left . renderMcpError) Right <$> listMcpResourceTemplates client

mcpFleetListPrompts :: McpFleet -> Text -> IO (Either Text [McpPrompt])
mcpFleetListPrompts fleet server =
    withFleetClient fleet server \client ->
        either (Left . renderMcpError) Right <$> listMcpPrompts client

mcpFleetGetPrompt
    :: McpFleet -> Text -> Text -> [(Text, Text)] -> IO (Either Text McpPromptResult)
mcpFleetGetPrompt fleet server name arguments =
    withFleetClient fleet server \client ->
        either (Left . renderMcpError) Right <$> getMcpPrompt client name arguments

mcpFleetComplete
    :: McpFleet
    -> Text
    -> McpCompletionRef
    -> Text
    -> Text
    -> [(Text, Text)]
    -> IO (Either Text McpCompletion)
mcpFleetComplete fleet server ref argument partial context =
    withFleetClient fleet server \client ->
        either (Left . renderMcpError) Right
            <$> completeMcpArgument client ref argument partial context

-- | Identity, capabilities, and instructions of every initialized server.
mcpFleetServerInfos :: McpFleet -> IO [(Text, McpServerInfo)]
mcpFleetServerInfos fleet = do
    clients <- readTVarIO fleet.mcpFleetClients
    fmap catMaybes $ forM fleet.mcpFleetServerOrder \name ->
        case Map.lookup name clients of
            Nothing -> pure Nothing
            Just client -> fmap (\info -> (name, info)) <$> mcpClientServerInfo client

-- | Natural-language guidance servers provide for the model.
mcpFleetInstructions :: McpFleet -> IO [(Text, Text)]
mcpFleetInstructions fleet =
    catMaybes . map instructionsOf <$> mcpFleetServerInfos fleet
  where
    instructionsOf :: (Text, McpServerInfo) -> Maybe (Text, Text)
    instructionsOf (name, info) = case info.serverInfoInstructions of
        Just instructions
            | not (Text.null (Text.strip instructions)) ->
                Just (name, Text.strip instructions)
        _ -> Nothing

withFleetClient
    :: McpFleet -> Text -> (McpClient -> IO (Either Text a)) -> IO (Either Text a)
withFleetClient fleet server action = do
    clients <- readTVarIO fleet.mcpFleetClients
    case Map.lookup server clients of
        Nothing -> pure (Left ("unknown MCP server: " <> server))
        Just client -> action client

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

-- | Start every server independently. Ordinary server failures become
-- warnings so one unavailable integration does not disable healthy servers.
startMcpFleet :: [McpServerConfig] -> IO McpFleet
startMcpFleet = startMcpFleetWithProgress (const (pure ()))

startMcpFleetWithProgress
    :: ([Text] -> IO ())
    -> [McpServerConfig]
    -> IO McpFleet
startMcpFleetWithProgress = startMcpFleetWithProgressHooks defaultMcpHostHooks

-- | Start every server concurrently while reporting the configured names that
-- are still initializing. The callback is intended for startup UI and
-- deliberately receives no command arguments or environment values.
startMcpFleetWithProgressHooks
    :: McpHostHooks
    -> ([Text] -> IO ())
    -> [McpServerConfig]
    -> IO McpFleet
startMcpFleetWithProgressHooks hooks reportActive configs =
    startMcpFleetWithInMemory hooks reportActive configs []

-- | Start external and typed host-owned servers in one fleet. Host endpoints
-- are supplied by the embedding application, never by model configuration.
startMcpFleetWithInMemory
    :: McpHostHooks
    -> ([Text] -> IO ())
    -> [McpServerConfig]
    -> [(McpServerConfig, McpToolServer)]
    -> IO McpFleet
startMcpFleetWithInMemory hooks reportActive external inMemory = mask \restore -> do
    validateServerNames configs
    closed <- newMVar False
    ownedClients <- newIORef []
    activeServers <- newMVar Set.empty
    results <-
        restore
            (withProgressReporter reportActive \publishActive ->
                mapConcurrently
                    (startServerTracked
                        ownedClients
                        activeServers
                        publishActive)
                    configs)
            `onException` closeOwnedClients ownedClients
    let (clients, registrations, warnings, failures) =
            foldr collectServerResult ([], [], [], Map.empty) results
    skills <- fmap concat $ forM clients \client -> do
        entries <- readTVarIO client.clientDiscoveredSkills
        pure
            [ McpSkillRegistration client.clientConfig.mcpServerName entry
            | entry <- entries
            ]
    skillsVar <- newTVarIO skills
    catalog <- newTVarIO $
        Map.fromList
            [ (registration.mcpRegistrationTool.appToolName, McpCatalogEntry client tool 1)
            | Right (client, tools, _) <- results
            , tool <- tools
            , let registration = registrationFor client tool
            ]
    catalogRevisions <- newTVarIO Map.empty
    approvedCalls <- newTVarIO Map.empty
    catalogGeneration <- newTVarIO 1
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
            , mcpFleetSkills = skillsVar
            , mcpFleetWarnings = warnings
            , mcpFleetClients = clientsVar
            , mcpFleetServerOrder = map (.mcpServerName) configs
            , mcpFleetFailures = failures
            , mcpFleetCatalog = catalog
            , mcpFleetCatalogRevisions = catalogRevisions
            , mcpFleetApprovedCalls = approvedCalls
            , mcpFleetNextCatalogGeneration = catalogGeneration
            , mcpFleetReconnects = reconnects
            , mcpFleetWorkers = workers
            , mcpFleetClosed = closed
            , mcpFleetHooks = hooks
            }
    forM_ clients (attachFleetEvents fleet)
    pure fleet
  where
    configs = external <> map fst inMemory
    startClient config = case lookup config.mcpServerName
            [(entry.mcpServerName, server) | (entry, server) <- inMemory] of
        Just server -> startInMemoryMcpClient hooks config server
        Nothing -> startMcpClientWith hooks Nothing config
    startServerTracked
        ownedClients
        activeServers
        publishActive
        config = mask \restore -> do
        updateActive
            activeServers
            publishActive
            (Set.insert config.mcpServerName)
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
            `finally`
                updateActive
                    activeServers
                    publishActive
                    (Set.delete config.mcpServerName)

    updateActive activeServers publishActive update =
        modifyMVar_ activeServers \current -> do
            let active = update current
            publishActive (Set.toAscList active)
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
        client <- startClient config
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

-- | Deliver progress updates in state-transition order without running the
-- callback under the state lock or blocking individual server workers.
-- The enclosing startup still waits for queued callbacks to drain.
withProgressReporter
    :: (a -> IO ())
    -> ((a -> IO ()) -> IO b)
    -> IO b
withProgressReporter report action = do
    updates <- newTQueueIO
    fst <$> concurrently
        ( action (atomically . writeTQueue updates . Just)
            `finally` atomically (writeTQueue updates Nothing)
        )
        (reportUpdates updates)
  where
    reportUpdates updates =
        atomically (readTQueue updates) >>= \case
            Nothing -> pure ()
            Just update -> report update >> reportUpdates updates

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

startMcpFleetProgressive
    :: ([McpServerStatus] -> IO ())
    -> [McpServerConfig]
    -> IO McpFleet
startMcpFleetProgressive = startMcpFleetProgressiveHooks defaultMcpHostHooks

-- | Spawn configured stdio clients with bounded concurrency, then initialize
-- and discover each server in tracked background workers. The fleet can be
-- used immediately through 'mcpFleetMetaTools'.
startMcpFleetProgressiveHooks
    :: McpHostHooks
    -> ([McpServerStatus] -> IO ())
    -> [McpServerConfig]
    -> IO McpFleet
startMcpFleetProgressiveHooks hooks reportStatuses configs = mask \restore -> do
    validateServerNames configs
    closed <- newMVar False
    workers <- newMVar []
    catalog <- newTVarIO Map.empty
    catalogRevisions <- newTVarIO Map.empty
    approvedCalls <- newTVarIO Map.empty
    catalogGeneration <- newTVarIO 0
    ownedClients <- newIORef []
    clientsVar <- newTVarIO Map.empty
    skillsVar <- newTVarIO []
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
            , mcpFleetSkills = skillsVar
            , mcpFleetWarnings = warnings
            , mcpFleetClients = clientsVar
            , mcpFleetServerOrder = map (.mcpServerName) configs
            , mcpFleetFailures = failures
            , mcpFleetCatalog = catalog
            , mcpFleetCatalogRevisions = catalogRevisions
            , mcpFleetApprovedCalls = approvedCalls
            , mcpFleetNextCatalogGeneration = catalogGeneration
            , mcpFleetReconnects = reconnects
            , mcpFleetWorkers = workers
            , mcpFleetClosed = closed
            , mcpFleetHooks = hooks
            }
        initializeOne client = do
            void (reportFleetStatuses reportStatuses fleet)
            ensureMcpClientReadyWith
                (publishCatalogEntries
                    catalog
                    catalogGeneration
                    client)
                client >>= \case
                Left _ -> pure ()
                Right _ -> do
                    entries <- readTVarIO client.clientDiscoveredSkills
                    atomically $
                        replaceServerSkills
                            skillsVar
                            client.clientConfig.mcpServerName
                            entries
            void (reportFleetStatuses reportStatuses fleet)
    atomically $
        writeTVar clientsVar $
            Map.fromList
                [ (client.clientConfig.mcpServerName, client)
                | client <- clients
                ]
    forM_ clients (attachFleetEvents fleet)
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
                (tryAny (restore (startMcpClientWith hooks Nothing config)))
        case attempt of
            Left exception -> pure (Left exception)
            Right client -> do
                atomicModifyIORef' ownedClients \clients ->
                    (client : clients, ())
                pure (Right client)

    closeOwnedClients ownedClients =
        atomicModifyIORef' ownedClients (\clients -> ([], clients))
            >>= mapM_ closeMcpClient

publishCatalogEntries
    :: TVar (Map.Map Text McpCatalogEntry)
    -> TVar Integer
    -> McpClient
    -> [McpTool]
    -> STM ()
publishCatalogEntries catalog generationVar client tools = do
    generation <- nextCatalogGeneration generationVar
    modifyTVar' catalog \current ->
        foldl'
            (\entries tool ->
                Map.insert
                    (qualifiedMcpToolName
                        client.clientConfig.mcpServerName
                        tool.discoveredName)
                    (McpCatalogEntry client tool generation)
                    entries)
            (withoutServer client.clientConfig.mcpServerName current)
            tools

nextCatalogGeneration :: TVar Integer -> STM Integer
nextCatalogGeneration generationVar = do
    current <- readTVar generationVar
    let next = current + 1
    writeTVar generationVar next
    pure next

replaceServerSkills
    :: TVar [McpSkillRegistration]
    -> Text
    -> [McpSkillEntry]
    -> STM ()
replaceServerSkills skillsVar serverName entries =
    modifyTVar' skillsVar \current ->
        filter ((/= serverName) . (.mcpSkillServer)) current
            <> [ McpSkillRegistration serverName entry
               | entry <- entries
               ]

withoutServer :: Text -> Map.Map Text McpCatalogEntry -> Map.Map Text McpCatalogEntry
withoutServer serverName =
    Map.filter ((/= serverName) . (.clientConfig.mcpServerName) . (.catalogClient))

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

-- * Server events

-- | Route a client's server-initiated notifications into fleet maintenance.
-- Tool list changes refresh the catalog in a tracked worker.
attachFleetEvents :: McpFleet -> McpClient -> IO ()
attachFleetEvents fleet client = do
    let handleEvent = \case
            McpToolsListChanged -> do
                -- Invalidate synchronously on the reader thread so neither
                -- static registrations nor meta-tools can dispatch a stale
                -- privilege classification while the replacement catalog is
                -- fetched.
                revision <- atomically do
                    clients <- readTVar fleet.mcpFleetClients
                    case Map.lookup serverName clients of
                        Just current
                            | sameClient client current -> do
                                revisions <-
                                    readTVar fleet.mcpFleetCatalogRevisions
                                let nextRevision =
                                        Map.findWithDefault
                                            0
                                            serverName
                                            revisions
                                            + 1
                                writeTVar
                                    fleet.mcpFleetCatalogRevisions
                                    (Map.insert
                                        serverName
                                        nextRevision
                                        revisions)
                                modifyTVar'
                                    fleet.mcpFleetCatalog
                                    (Map.filter
                                        (\entry ->
                                            entry.catalogClient.clientConfig.mcpServerName
                                                /= serverName))
                                shouldRefresh <-
                                    readTVar client.clientLifecycle >>= \case
                                        ClientReady _ warnings -> do
                                            writeTVar
                                                client.clientLifecycle
                                                (ClientReady [] warnings)
                                            pure True
                                        -- Initialization observes the
                                        -- client-local revision and performs
                                        -- the stable re-list itself.
                                        _ -> pure False
                                pure (Just (nextRevision, shouldRefresh))
                        _ -> pure Nothing
                forM_ revision \(currentRevision, shouldRefresh) ->
                    when shouldRefresh $
                        spawnFleetWorker fleet
                            (refreshServerTools fleet client currentRevision)
            _ -> pure ()
    setMcpClientEventHandler client handleEvent
    -- A notification can arrive after a blocking initialization publishes
    -- ClientReady but before the fleet exists. The client buffers that fact
    -- by clearing clientReadyToolsRevision; replay it after installing the
    -- real handler so the stale startup result is never left advertised.
    buffered <- atomically do
        current <- readTVar client.clientReadyToolsRevision
        lifecycle <- readTVar client.clientLifecycle
        pure $ case (current, lifecycle) of
            (Nothing, ClientReady _ _) -> True
            _ -> False
    when buffered (handleEvent McpToolsListChanged)
  where
    serverName = client.clientConfig.mcpServerName
    sameClient :: McpClient -> McpClient -> Bool
    sameClient left right =
        left.clientRequestRegistry == right.clientRequestRegistry

-- | Run fleet maintenance in the background. Finished workers are pruned on
-- the next spawn; the rest are cancelled by 'closeMcpFleet'.
spawnFleetWorker :: McpFleet -> IO () -> IO ()
spawnFleetWorker fleet action =
    withMVar fleet.mcpFleetClosed \closed ->
        unless closed $ mask_ do
            worker <- asyncWithUnmask \unmask -> unmask (void (tryAny action))
            modifyMVar_ fleet.mcpFleetWorkers \current -> do
                live <- fmap catMaybes $ forM current \existing ->
                    poll existing >>= \case
                        Nothing -> pure (Just existing)
                        Just _ -> pure Nothing
                pure (worker : live)

-- | Re-list a server's tools after @notifications/tools/list_changed@ and
-- replace its catalog entries. Statically registered tools keep working for
-- as long as the server still offers them.
refreshServerTools :: McpFleet -> McpClient -> Integer -> IO ()
refreshServerTools fleet client expectedRevision =
    forM_ (Map.lookup serverName fleet.mcpFleetReconnects) \lock ->
        withMVar lock \_ -> do
            current <- Map.lookup serverName <$> readTVarIO fleet.mcpFleetClients
            when (maybe False (sameClient client) current) do
                expectedClientRevision <-
                    readTVarIO client.clientToolsRevision
                tryAny (discoverMcpTools client) >>= \case
                    Left _ -> pure ()
                    Right (tools, warnings) -> atomically do
                        revisions <-
                            readTVar fleet.mcpFleetCatalogRevisions
                        clientRevision <-
                            readTVar client.clientToolsRevision
                        when
                            (Map.lookup serverName revisions
                                == Just expectedRevision
                                && clientRevision == expectedClientRevision)
                            do
                                publishCatalogEntries
                                    fleet.mcpFleetCatalog
                                    fleet.mcpFleetNextCatalogGeneration
                                    client
                                    tools
                                readTVar client.clientLifecycle >>= \case
                                    ClientReady _ _ ->
                                        do
                                            writeTVar
                                                client.clientReadyToolsRevision
                                                (Just clientRevision)
                                            writeTVar client.clientLifecycle
                                                (ClientReady tools warnings)
                                    _ -> pure ()
  where
    serverName = client.clientConfig.mcpServerName
    sameClient :: McpClient -> McpClient -> Bool
    sameClient left right =
        left.clientRequestRegistry == right.clientRequestRegistry

-- * Meta-tools

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

-- | Read-only tools for browsing and reading server resources. They are
-- useful in every startup mode because resource links appear in tool
-- results.
mcpFleetResourceTools :: McpFleet -> [AppTool]
mcpFleetResourceTools fleet =
    [ mcpListResourcesTool fleet
    , mcpReadResourceTool fleet
    ]

mcpSearchTool :: McpFleet -> AppTool
mcpSearchTool fleet = AppTool
    { appToolName = "mcp_search"
    , appToolDescription =
        "Search currently available MCP tools. Servers may still be connecting. \
        \Returns readable labeled text."
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
    , appToolHandler = typedTool "mcp_search" searchArgumentsDecoder \arguments -> do
        entries <- readTVarIO fleet.mcpFleetCatalog
        statuses <- mcpFleetStatuses fleet
        let (query, server, limit) = arguments
            matches :: (Text, McpCatalogEntry) -> Bool
            matches (name, entry) =
                maybe True
                    (\needle ->
                        Text.toCaseFold needle
                            `Text.isInfixOf`
                                Text.toCaseFold
                                    (name <> " "
                                        <> describeTool entry.catalogTool))
                    query
                    && maybe True
                        (== entry.catalogClient.clientConfig.mcpServerName)
                        server
            found = take limit (filter matches (Map.toAscList entries))
        pure (Right (renderMcpSearch statuses found))
    , appToolApproval = AlwaysReadOnly
    , appToolExecution = ParallelSafe
    , appToolResourceClaims = Nothing
    , appToolAsyncCapability = BlockingOnly
    }

grokSearchTool :: McpFleet -> AppTool
grokSearchTool fleet = AppTool
    { appToolName = "search_tool"
    , appToolDescription =
        "Search for MCP tools by keyword and retrieve their input schemas as \
        \readable labeled text.\n\n\
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
    , appToolHandler = typedTool "search_tool" grokSearchArgumentsDecoder
        \(query, limit) -> do
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
                                    (describeTool entry.catalogTool)
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
                                    toolMetadata = GrokSearchTool
                                        { grokSearchToolName = name
                                        , grokSearchToolDescription =
                                            truncateMcpDescription
                                                (describeTool entry.catalogTool)
                                        , grokSearchToolScore = scoreEntry pair
                                        , grokSearchToolSchema =
                                            entry.catalogTool.discoveredInputSchema
                                        }
                                    (before, rest) =
                                        break ((== server) . fst) current
                                in case rest of
                                    [] ->
                                        current <> [(server, [toolMetadata])]
                                    (matchedServer, tools) : after ->
                                        before
                                            <> [ ( matchedServer
                                                 , tools <> [toolMetadata]
                                                 )
                                               ]
                                            <> after)
                            []
                            found
                    connecting = any isConnecting statuses
                    note
                        | connecting =
                            Just
                                ("Some MCP servers are still connecting. Results may be incomplete." :: Text)
                        | Map.null entries =
                            Just "No MCP tools are available in this session."
                        | otherwise = Nothing
                pure $ Right $ renderGrokSearch
                    connecting
                    (Map.size entries)
                    note
                    grouped
    , appToolApproval = AlwaysReadOnly
    , appToolExecution = ParallelSafe
    , appToolResourceClaims = Nothing
    , appToolAsyncCapability = BlockingOnly
    }

callCatalogEntryWithReconnect
    :: McpFleet
    -> Text
    -> McpCatalogEntry
    -> RawJson
    -> IO (Either Text Text)
callCatalogEntryWithReconnect fleet qualifiedName entry arguments =
    catalogEntryIsLive entry >>= \case
        False -> pure (Left changedCatalogEntryMessage)
        True ->
            callDiscoveredTool entry.catalogClient entry.catalogTool arguments
                >>= \case
                    Right result -> pure (Right result)
                    Left originalError
                        | not (mcpToolRetrySafe entry.catalogTool) ->
                            -- A failed mutation may have reached the server.
                            -- Retrying it after reconnect could duplicate the
                            -- side effect.
                            pure (Left originalError)
                        | otherwise -> do
                            failed <-
                                readTVarIO entry.catalogClient.clientFailure
                            case failed of
                                Nothing -> pure (Left originalError)
                                Just _ ->
                                    -- Read-only and idempotent calls are safe
                                    -- to retry once after a transport failure.
                                    -- The per-server lock makes this
                                    -- single-flight across concurrent calls.
                                    reconnectCatalogEntry
                                        fleet
                                        qualifiedName
                                        entry
                                        >>= \case
                                            Left reconnectError ->
                                                pure . Left $
                                                    originalError
                                                        <> "; MCP reconnect failed: "
                                                        <> reconnectError
                                            Right replacement ->
                                                if not
                                                    (sameCatalogTool
                                                        entry
                                                        replacement)
                                                    then
                                                        pure
                                                            (Left
                                                                changedCatalogEntryMessage)
                                                    else
                                                        callDiscoveredTool
                                                            replacement.catalogClient
                                                            replacement.catalogTool
                                                            arguments

catalogEntryIsLive :: McpCatalogEntry -> IO Bool
catalogEntryIsLive entry = do
    readTVarIO entry.catalogClient.clientLifecycle >>= \case
        ClientReady tools _ ->
            pure (any (== entry.catalogTool) tools)
        _ -> pure False

-- | Dispatch against the catalog snapshot used by the approval classifier.
-- A generation change is allowed only when every advertised tool property
-- still matches the snapshot that the parent approved.
callApprovedCatalogTool
    :: McpFleet
    -> ToolCall
    -> Text
    -> RawJson
    -> IO (Either Text Text)
callApprovedCatalogTool fleet call name toolArguments = do
    approved <- atomically do
        approvals <- readTVar fleet.mcpFleetApprovedCalls
        writeTVar fleet.mcpFleetApprovedCalls
            (Map.delete (catalogApprovalKey call) approvals)
        pure (Map.lookup (catalogApprovalKey call) approvals)
    case approved of
        Nothing ->
            pure (Left "MCP call has no matching approval-time catalog snapshot")
        Just binding
            | binding.approvedCallArguments /= call.arguments
                || binding.approvedCatalogName /= name ->
                    pure (Left "MCP call changed after approval")
            | Nothing <- binding.approvedCatalogEntry ->
                catalogUnavailableError fleet name
            | Just approvedEntry <- binding.approvedCatalogEntry -> do
                current <- Map.lookup name <$> readTVarIO fleet.mcpFleetCatalog
                case current of
                    Nothing -> pure (Left "MCP tool disappeared after approval")
                    Just replacement -> do
                        let selected
                                | replacement.catalogGeneration
                                    == approvedEntry.catalogGeneration =
                                        approvedEntry
                                | otherwise = replacement
                        if not (sameCatalogTool approvedEntry replacement)
                            then pure (Left changedCatalogEntryMessage)
                            else
                                callCatalogEntryWithReconnect
                                    fleet
                                    name
                                    selected
                                    toolArguments

catalogUnavailableError :: McpFleet -> Text -> IO (Either Text a)
catalogUnavailableError fleet name = do
    statuses <- mcpFleetStatuses fleet
    pure . Left $
        if any isConnecting statuses
            then
                "MCP tool is not available yet; one or more servers are still connecting"
            else "Unknown MCP tool: " <> name

catalogApprovalKey :: ToolCall -> (Text, Text)
catalogApprovalKey call = (call.callId, call.name)

catalogEntryApproval :: McpCatalogEntry -> ApprovalRequirement
catalogEntryApproval entry
    | entry.catalogTool.discoveredRequiresFreshApproval =
        FreshApprovalRequired
    | entry.catalogTool.discoveredReadOnly = ApprovalNotRequired
    | otherwise = ApprovalPromptRequired

sameCatalogTool :: McpCatalogEntry -> McpCatalogEntry -> Bool
sameCatalogTool approved replacement =
    catalogToolFingerprint approved == catalogToolFingerprint replacement

-- | Stable, connection-independent identity for the exact catalog entry a
-- parent approved. Schemas are compared as JSON values rather than source
-- bytes, so harmless object-key ordering does not invalidate a reconnect.
data CatalogToolFingerprint = CatalogToolFingerprint
    !McpServerConfig
    !Text
    !(Maybe Text)
    !Text
    !Value
    !(Maybe Value)
    !Bool
    !Bool
    !Bool
    !Bool
    !Bool
    ![McpHeaderParam]
    deriving (Eq)

catalogToolFingerprint :: McpCatalogEntry -> CatalogToolFingerprint
catalogToolFingerprint entry =
    CatalogToolFingerprint
        entry.catalogClient.clientConfig
        tool.discoveredName
        tool.discoveredTitle
        tool.discoveredDescription
        (Aeson.toJSON tool.discoveredInputSchema)
        (Aeson.toJSON <$> tool.discoveredOutputSchema)
        tool.discoveredReadOnly
        tool.discoveredRequiresFreshApproval
        tool.discoveredDestructive
        tool.discoveredIdempotent
        tool.discoveredOpenWorld
        tool.discoveredHeaderParams
  where
    tool = entry.catalogTool

changedCatalogEntryMessage :: Text
changedCatalogEntryMessage =
    "MCP tool changed after approval; ask the user to approve the call again"

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
                                _ -> restart
  where
    serverName =
        failedEntry.catalogClient.clientConfig.mcpServerName
    config = failedEntry.catalogClient.clientConfig

    restart = do
        eraHint <- mcpClientEra failedEntry.catalogClient
        started <- tryAny (startMcpClientWith fleet.mcpFleetHooks eraHint config)
        case started of
            Left exception ->
                pure . Left $
                    redactConfiguredValues config
                        (exceptionSummary exception)
            Right replacementClient -> do
                -- Install the event route before initialization. Until the
                -- replacement becomes the fleet's current client, its
                -- client-local revision still makes initialization and the
                -- publication transaction below fail closed.
                attachFleetEvents fleet replacementClient
                ensureMcpClientReady replacementClient >>= \case
                    Left err -> do
                        closeMcpClient replacementClient
                        pure (Left err)
                    Right (tools, _) -> do
                        skills <- readTVarIO replacementClient.clientDiscoveredSkills
                        installed <- atomically do
                            readyRevision <-
                                readTVar
                                    replacementClient.clientReadyToolsRevision
                            readTVar replacementClient.clientLifecycle >>= \case
                                ClientReady liveTools _
                                    | liveTools == tools
                                    , Just _ <- readyRevision -> do
                                        generation <-
                                            nextCatalogGeneration
                                                fleet.mcpFleetNextCatalogGeneration
                                        let replacementEntries =
                                                Map.fromList
                                                    [ ( qualifiedMcpToolName
                                                            serverName tool.discoveredName
                                                      , McpCatalogEntry
                                                            replacementClient
                                                            tool
                                                            generation
                                                      )
                                                    | tool <- tools
                                                    ]
                                        clients <- readTVar fleet.mcpFleetClients
                                        currentCatalog <-
                                            readTVar fleet.mcpFleetCatalog
                                        writeTVar fleet.mcpFleetClients
                                            (Map.insert
                                                serverName
                                                replacementClient
                                                clients)
                                        writeTVar fleet.mcpFleetCatalog
                                            ( replacementEntries
                                                <> withoutServer
                                                    serverName
                                                    currentCatalog
                                            )
                                        replaceServerSkills
                                            fleet.mcpFleetSkills
                                            serverName
                                            skills
                                        pure . Just $
                                            ( Map.lookup serverName clients
                                            , replacementEntries
                                            )
                                _ -> pure Nothing
                        case installed of
                            Nothing -> do
                                closeMcpClient replacementClient
                                pure
                                    (Left
                                        "MCP tools changed while reconnecting; retry the call")
                            Just (previousClient, replacementEntries) -> do
                                mapM_ closeMcpClient previousClient
                                case Map.lookup qualifiedName replacementEntries of
                                    Nothing ->
                                        pure
                                            (Left
                                                "MCP tool disappeared after reconnect")
                                    Just replacement ->
                                        pure (Right replacement)

mcpCallTool :: McpFleet -> AppTool
mcpCallTool fleet = AppTool
    { appToolName = "mcp_call"
    , appToolDescription =
        "Call a currently available MCP tool by its qualified server__tool name. Mutating tools require user approval."
    , appToolSchema = RawJsonFunctionSchema $ object
        [ "type" .= ("object" :: Text)
        , "properties" .= object
            [ "name" .= object ["type" .= ("string" :: Text)]
            , "arguments" .= object ["type" .= ("object" :: Text)]
            ]
        , "required" .= (["name"] :: [Text])
        , "additionalProperties" .= False
        ]
    , appToolHandler = typedToolWithCall "mcp_call" callArgumentsDecoder
        \call (name, toolArguments) ->
            callApprovedCatalogTool fleet call name toolArguments
    , appToolApproval =
        ClassifyApproval
            (catalogCallApproval fleet callArgumentsDecoder)
    , appToolExecution = TurnSequential
    , appToolResourceClaims = Nothing
    , appToolAsyncCapability = BlockingOnly
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
    , appToolHandler = typedToolWithCall "use_tool" grokCallArgumentsDecoder
        \call (name, toolArguments) ->
            callApprovedCatalogTool fleet call name toolArguments
    , appToolApproval =
        ClassifyApproval
            (catalogCallApproval fleet grokCallArgumentsDecoder)
    , appToolExecution = TurnSequential
    , appToolResourceClaims = Nothing
    , appToolAsyncCapability = BlockingOnly
    }

mcpListResourcesTool :: McpFleet -> AppTool
mcpListResourcesTool fleet = AppTool
    { appToolName = "mcp_list_resources"
    , appToolDescription =
        "List the resources and resource templates an MCP server exposes. \
        \Omit `server` to query every connected server. Returns readable \
        \labeled text."
    , appToolSchema = RawJsonFunctionSchema $ object
        [ "type" .= ("object" :: Text)
        , "properties" .= object
            [ "server" .= object ["type" .= ("string" :: Text)]
            ]
        , "additionalProperties" .= False
        ]
    , appToolHandler = typedTool "mcp_list_resources"
        (Json.object (nonEmptyOptionalText "server"))
        \server -> do
            infos <- mcpFleetServerInfos fleet
            let selected =
                    [ name
                    | (name, info) <- infos
                    , maybe True (== name) server
                    , isJust info.serverInfoCapabilities.capabilityResources
                    ]
            listings <- forM selected \name -> do
                resources <- mcpFleetListResources fleet name
                templates <- mcpFleetListResourceTemplates fleet name
                pure (renderMcpResourceServer name resources templates)
            pure $ Right $ case listings of
                [] -> "(no MCP resource servers)"
                _ -> Text.intercalate "\n\n" listings
    , appToolApproval = AlwaysReadOnly
    , appToolExecution = ParallelSafe
    , appToolResourceClaims = Nothing
    , appToolAsyncCapability = BlockingOnly
    }

mcpReadResourceTool :: McpFleet -> AppTool
mcpReadResourceTool fleet = AppTool
    { appToolName = "mcp_read_resource"
    , appToolDescription =
        "Read a resource from an MCP server by URI, for example one returned \
        \by mcp_list_resources or referenced as a resource_link in a tool result."
    , appToolSchema = RawJsonFunctionSchema $ object
        [ "type" .= ("object" :: Text)
        , "properties" .= object
            [ "server" .= object ["type" .= ("string" :: Text)]
            , "uri" .= object ["type" .= ("string" :: Text)]
            ]
        , "required" .= (["server", "uri"] :: [Text])
        , "additionalProperties" .= False
        ]
    , appToolHandler = typedTool "mcp_read_resource" readArgumentsDecoder
        \(server, uri) ->
            mcpFleetReadResource fleet server uri >>= \case
                Left err -> pure (Left err)
                Right contents ->
                    pure . Right . Text.intercalate "\n" $
                        map renderResourceContent contents
    , appToolApproval = AlwaysReadOnly
    , appToolExecution = ParallelSafe
    , appToolResourceClaims = Nothing
    , appToolAsyncCapability = BlockingOnly
    }
  where
    readArgumentsDecoder = Json.object do
        server <- Text.strip <$> Json.atKey "server" Json.text
        uri <- Text.strip <$> Json.atKey "uri" Json.text
        when (Text.null server || Text.null uri) $
            fail "mcp_read_resource requires server and uri"
        pure (server, uri)
    renderResourceContent :: McpResourceContent -> Text
    renderResourceContent content =
        case (content.mcpResourceText, content.mcpResourceBlob) of
            (Just text, _) ->
                "[" <> content.mcpResourceUri
                    <> maybe "" (\value -> " (" <> value <> ")") content.mcpResourceMimeType
                    <> "]\n" <> text
            (Nothing, Just blob) ->
                "[" <> content.mcpResourceUri
                    <> maybe "" (\value -> " (" <> value <> ")") content.mcpResourceMimeType
                    <> ": " <> Text.pack (show (Text.length blob))
                    <> " base64 bytes; binary content is not shown]"
            (Nothing, Nothing) -> "[" <> content.mcpResourceUri <> "]"

catalogCallApproval
    :: McpFleet
    -> Json.Decoder (Text, RawJson)
    -> ToolCall
    -> IO ApprovalRequirement
catalogCallApproval fleet decoder call =
    case Json.decodeEither decoder (TextEncoding.encodeUtf8 call.arguments) of
        Left _ -> do
            atomically $
                modifyTVar' fleet.mcpFleetApprovedCalls
                    (rememberApprovedCall
                        (catalogApprovalKey call)
                        McpApprovedCall
                            { approvedCallArguments = call.arguments
                            , approvedCatalogName = ""
                            , approvedCatalogEntry = Nothing
                            })
            pure ApprovalPromptRequired
        Right (name, _) ->
            atomically do
                entries <- readTVar fleet.mcpFleetCatalog
                let selected = Map.lookup name entries
                modifyTVar' fleet.mcpFleetApprovedCalls $
                    rememberApprovedCall
                        (catalogApprovalKey call)
                        McpApprovedCall
                            { approvedCallArguments = call.arguments
                            , approvedCatalogName = name
                            , approvedCatalogEntry = selected
                            }
                pure $ maybe ApprovalPromptRequired catalogEntryApproval selected

rememberApprovedCall
    :: (Text, Text)
    -> McpApprovedCall
    -> Map.Map (Text, Text) McpApprovedCall
    -> Map.Map (Text, Text) McpApprovedCall
rememberApprovedCall key binding current =
    Map.insert key binding bounded
  where
    bounded
        | Map.size current < maxApprovalSnapshots
            || Map.member key current = current
        | otherwise = maybe current snd (Map.minViewWithKey current)

maxApprovalSnapshots :: Int
maxApprovalSnapshots = 1024

searchArgumentsDecoder :: Json.Decoder (Maybe Text, Maybe Text, Int)
searchArgumentsDecoder = Json.object do
    query <- nonEmptyOptionalText "query"
    server <- nonEmptyOptionalText "server"
    limit <- max 1 . min 50 <$> Json.defaultKey 20 "limit" Json.int
    pure (query, server, limit)

grokSearchArgumentsDecoder :: Json.Decoder (Text, Int)
grokSearchArgumentsDecoder = Json.object do
    query <- Text.strip <$> Json.atKey "query" Json.text
    when (Text.null query) $
        fail "search_tool requires a non-empty query"
    rawLimit <- Json.optionalKey "limit" rawJsonDecoder
    limit <- case rawLimit of
        Nothing -> pure 5
        Just value ->
            case Json.decodeEither Json.int (rawJsonBytes value) of
                Right parsed -> pure parsed
                Left _ ->
                    fail
                        "search_tool limit must be an integer from 1 through 255"
    unless (limit >= 1 && limit <= 255) $
        fail "search_tool limit must be an integer from 1 through 255"
    pure (query, limit)

nonEmptyOptionalText
    :: Text
    -> Json.FieldsDecoder (Maybe Text)
nonEmptyOptionalText key =
    fmap (Text.strip <$>) (Json.optionalKey key Json.text)
        >>= \case
            Just "" -> pure Nothing
            value -> pure value

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

callArgumentsDecoder :: Json.Decoder (Text, RawJson)
callArgumentsDecoder = Json.object do
    name <- Text.strip <$> Json.atKey "name" Json.text
    when (Text.null name) $
        fail "mcp_call requires a non-empty name"
    arguments <- Json.defaultKey emptyObject "arguments" rawObjectDecoder
    pure (name, arguments)

grokCallArgumentsDecoder :: Json.Decoder (Text, RawJson)
grokCallArgumentsDecoder = Json.object do
    name <- Text.strip <$> Json.atKey "tool_name" Json.text
    when (Text.null name) $
        fail "use_tool requires a non-empty tool_name"
    unless ("__" `Text.isInfixOf` name) $
        fail
            "use_tool tool_name must be a qualified server__tool name returned by search_tool"
    rawArguments <- Json.optionalKey "tool_input" rawJsonDecoder
        >>= maybe (fail "use_tool requires tool_input") pure
    arguments <-
        case Json.decodeEither rawObjectDecoder (rawJsonBytes rawArguments) of
            Right value -> pure value
            Left _ -> fail "use_tool tool_input must be an object"
    pure (name, arguments)

emptyObject :: RawJson
emptyObject = rawJsonFromEncoding (Aeson.toEncoding (object []))

data GrokSearchTool = GrokSearchTool
    { grokSearchToolName :: !Text
    , grokSearchToolDescription :: !Text
    , grokSearchToolScore :: !Int
    , grokSearchToolSchema :: !RawJson
    }

renderMcpSearch :: [McpServerStatus] -> [(Text, McpCatalogEntry)] -> Text
renderMcpSearch statuses found =
    Text.intercalate "\n\n" $
        renderMcpServerStatuses statuses
            : case found of
                [] -> ["(no matching MCP tools)"]
                _ -> map renderMcpSearchTool found

renderMcpSearchTool :: (Text, McpCatalogEntry) -> Text
renderMcpSearchTool (name, entry) =
    Text.intercalate "\n" $
        [ name
        , "  Server: " <> entry.catalogClient.clientConfig.mcpServerName
        , "  Description: " <> describeTool entry.catalogTool
        , "  Read-only: "
            <> if entry.catalogTool.discoveredReadOnly then "true" else "false"
        , "  Input schema:"
        ]
            <> indentBlock (rawJsonText entry.catalogTool.discoveredInputSchema)
            <> maybe
                []
                (\schema -> "  Output schema:" : indentBlock (rawJsonText schema))
                entry.catalogTool.discoveredOutputSchema

renderGrokSearch
    :: Bool
    -> Int
    -> Maybe Text
    -> [(Text, [GrokSearchTool])]
    -> Text
renderGrokSearch connecting hidden note grouped =
    Text.intercalate "\n" $
        [ "Status: " <> if connecting then "partial" else "ready"
        , "Total hidden tools: " <> Text.pack (show hidden)
        ]
            <> maybe [] (\text -> ["Note: " <> text]) note
            <> case grouped of
                [] -> []
                _ -> "" : Text.lines (Text.intercalate "\n\n" (map renderGrokServer grouped))

renderGrokServer :: (Text, [GrokSearchTool]) -> Text
renderGrokServer (server, tools) =
    Text.intercalate "\n" $
        server : concatMap renderGrokSearchMatch tools

renderGrokSearchMatch :: GrokSearchTool -> [Text]
renderGrokSearchMatch tool =
    [ "  " <> tool.grokSearchToolName
    , "    Score: " <> Text.pack (show tool.grokSearchToolScore)
    , "    Description: " <> tool.grokSearchToolDescription
    , "    Input schema:"
    ]
        <> map ("      " <>) (Text.lines (rawJsonText tool.grokSearchToolSchema))

renderMcpServerStatuses :: [McpServerStatus] -> Text
renderMcpServerStatuses = \case
    [] -> "Servers: (none)"
    statuses ->
        Text.intercalate "\n" $
            "Servers:" : map renderMcpServerStatus statuses

renderMcpServerStatus :: McpServerStatus -> Text
renderMcpServerStatus status =
    "  "
        <> status.mcpStatusName
        <> ": "
        <> mcpStateLabel status.mcpStatusState
        <> " ("
        <> Text.pack (show status.mcpStatusToolCount)
        <> if status.mcpStatusToolCount == 1 then " tool)" else " tools)"

mcpStateLabel :: McpInitState -> Text
mcpStateLabel = \case
    McpPending -> "pending"
    McpInitializing -> "initializing"
    McpReady -> "ready"
    McpFailed _ -> "failed"
    McpClosed -> "closed"

renderMcpResourceServer
    :: Text
    -> Either Text [McpResource]
    -> Either Text [McpResourceTemplate]
    -> Text
renderMcpResourceServer name resources templates =
    Text.intercalate "\n" $
        [name]
            <> case resources of
                Left err ->
                    ["  Error: " <> err, "  Resources: (none)"]
                Right [] ->
                    ["  Resources: (none)"]
                Right items ->
                    "  Resources:" : concatMap renderMcpResource items
            <> case templates of
                Left err -> ["  Resource templates error: " <> err]
                Right [] -> ["  Resource templates: (none)"]
                Right items ->
                    "  Resource templates:" : concatMap renderMcpTemplate items

renderMcpResource :: McpResource -> [Text]
renderMcpResource resource =
    ("    " <> resource.resourceName)
        : ("      URI: " <> resource.resourceUri)
        : catMaybes
            [ ("      Title: " <>) <$> resource.resourceTitle
            , ("      Description: " <>) <$> resource.resourceDescription
            , ("      MIME type: " <>) <$> resource.resourceMimeType
            , ("      Size: " <>) . Text.pack . show <$> resource.resourceSize
            ]

renderMcpTemplate :: McpResourceTemplate -> [Text]
renderMcpTemplate template =
    ("    " <> template.templateName)
        : ("      URI template: " <> template.templateUri)
        : catMaybes
            [ ("      Title: " <>) <$> template.templateTitle
            , ("      Description: " <>) <$> template.templateDescription
            , ("      MIME type: " <>) <$> template.templateMimeType
            ]

rawJsonText :: RawJson -> Text
rawJsonText = TextEncoding.decodeUtf8 . rawJsonBytes

indentBlock :: Text -> [Text]
indentBlock = map ("    " <>) . Text.lines

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
