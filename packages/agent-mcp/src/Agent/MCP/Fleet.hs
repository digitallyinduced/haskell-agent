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
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , ToolPlacement(..)
    , ToolSchema(..)
    )
import Agent.ToolDispatch (ToolCall(..), typedTool)
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
    ( Value(..)
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
startMcpFleetWithProgressHooks hooks reportActive configs = mask \restore -> do
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
            , mcpFleetSkills = skillsVar
            , mcpFleetWarnings = warnings
            , mcpFleetClients = clientsVar
            , mcpFleetServerOrder = map (.mcpServerName) configs
            , mcpFleetFailures = failures
            , mcpFleetCatalog = catalog
            , mcpFleetReconnects = reconnects
            , mcpFleetWorkers = workers
            , mcpFleetClosed = closed
            , mcpFleetHooks = hooks
            }
    forM_ clients (attachFleetEvents fleet)
    pure fleet
  where
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
        client <- startMcpClientWith hooks Nothing config
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
            , mcpFleetReconnects = reconnects
            , mcpFleetWorkers = workers
            , mcpFleetClosed = closed
            , mcpFleetHooks = hooks
            }
        initializeOne client = do
            void (reportFleetStatuses reportStatuses fleet)
            ensureMcpClientReadyWith
                (publishCatalogEntries catalog client)
                client >>= \case
                Left _ -> pure ()
                Right _ -> do
                    entries <- readTVarIO client.clientDiscoveredSkills
                    atomically $ modifyTVar' skillsVar \current ->
                        current
                            <> [ McpSkillRegistration
                                    client.clientConfig.mcpServerName entry
                                | entry <- entries
                                ]
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
    -> McpClient
    -> [McpTool]
    -> STM ()
publishCatalogEntries catalog client tools =
    modifyTVar' catalog \current ->
        foldl'
            (\entries tool ->
                Map.insert
                    (qualifiedMcpToolName
                        client.clientConfig.mcpServerName
                        tool.discoveredName)
                    (McpCatalogEntry client tool)
                    entries)
            (withoutServer client.clientConfig.mcpServerName current)
            tools

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
attachFleetEvents fleet client =
    setMcpClientEventHandler client \case
        McpToolsListChanged ->
            spawnFleetWorker fleet (refreshServerTools fleet client)
        _ -> pure ()

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
refreshServerTools :: McpFleet -> McpClient -> IO ()
refreshServerTools fleet client =
    forM_ (Map.lookup serverName fleet.mcpFleetReconnects) \lock ->
        withMVar lock \_ -> do
            current <- Map.lookup serverName <$> readTVarIO fleet.mcpFleetClients
            when (maybe False (sameClient client) current) do
                tryAny (discoverMcpTools client) >>= \case
                    Left _ -> pure ()
                    Right (tools, warnings) -> atomically do
                        publishCatalogEntries fleet.mcpFleetCatalog client tools
                        readTVar client.clientLifecycle >>= \case
                            ClientReady _ _ ->
                                writeTVar client.clientLifecycle
                                    (ClientReady tools warnings)
                            _ -> pure ()
  where
    serverName = client.clientConfig.mcpServerName
    sameClient :: McpClient -> McpClient -> Bool
    sameClient left right = left.clientNextId == right.clientNextId

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
            payload = object
                [ "tools" .=
                    [ object $
                        [ "name" .= name
                        , "server" .=
                            entry.catalogClient.clientConfig.mcpServerName
                        , "description" .= describeTool entry.catalogTool
                        , "inputSchema" .=
                            entry.catalogTool.discoveredInputSchema
                        , "readOnly" .= entry.catalogTool.discoveredReadOnly
                        ]
                        <> [ "outputSchema" .= schema
                           | Just schema <- [entry.catalogTool.discoveredOutputSchema]
                           ]
                    | (name, entry) <- found
                    ]
                , "servers" .= map statusJson statuses
                ]
        pure (Right (compactJson payload))
    , appToolApproval = AlwaysReadOnly
    , appToolExecution = ParallelSafe
    , appToolResourceClaims = Nothing
    , appToolPlacement = UnclassifiedTool
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
                                    toolJson = object
                                        [ "tool_name" .= name
                                        , "description" .=
                                            truncateMcpDescription
                                                (describeTool entry.catalogTool)
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
    , appToolPlacement = UnclassifiedTool
    }

callCatalogEntryWithReconnect
    :: McpFleet
    -> Text
    -> McpCatalogEntry
    -> RawJson
    -> IO (Either Text Text)
callCatalogEntryWithReconnect fleet qualifiedName entry arguments =
    callDiscoveredTool entry.catalogClient entry.catalogTool arguments
        >>= \case
            Right result -> pure (Right result)
            Left originalError
                | not (mcpToolRetrySafe entry.catalogTool) ->
                    -- A failed mutation may have reached the server. Retrying
                    -- it after reconnect could duplicate the side effect.
                    pure (Left originalError)
                | otherwise -> do
                    failed <- readTVarIO entry.catalogClient.clientFailure
                    case failed of
                        Nothing -> pure (Left originalError)
                        Just _ ->
                            -- Read-only and idempotent calls are safe to retry
                            -- once after a transport failure. The per-server
                            -- lock makes this single-flight across concurrent
                            -- calls.
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

callCatalogTool
    :: McpFleet
    -> Text
    -> RawJson
    -> IO (Either Text Text)
callCatalogTool fleet name toolArguments = do
    entries <- readTVarIO fleet.mcpFleetCatalog
    case Map.lookup name entries of
        Just entry ->
            callCatalogEntryWithReconnect
                fleet
                name
                entry
                toolArguments
        Nothing -> do
            statuses <- mcpFleetStatuses fleet
            pure . Left $
                if any isConnecting statuses
                    then
                        "MCP tool is not available yet; one or more servers are still connecting"
                    else "Unknown MCP tool: " <> name

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
                            writeTVar fleet.mcpFleetClients
                                (Map.insert serverName replacementClient clients)
                            writeTVar fleet.mcpFleetCatalog
                                (replacementEntries <> withoutServer serverName currentCatalog)
                            pure (Map.lookup serverName clients)
                        attachFleetEvents fleet replacementClient
                        mapM_ closeMcpClient previousClient
                        case Map.lookup qualifiedName replacementEntries of
                            Nothing ->
                                pure (Left "MCP tool disappeared after reconnect")
                            Just replacement -> pure (Right replacement)

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
    , appToolHandler = typedTool "mcp_call" callArgumentsDecoder
        \(name, toolArguments) ->
            callCatalogTool fleet name toolArguments
    , appToolApproval =
        ClassifyReadOnly (catalogCallIsReadOnly fleet callArgumentsDecoder)
    , appToolExecution = TurnSequential
    , appToolResourceClaims = Nothing
    , appToolPlacement = UnclassifiedTool
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
    , appToolHandler = typedTool "use_tool" grokCallArgumentsDecoder
        \(name, toolArguments) ->
            callCatalogTool fleet name toolArguments
    , appToolApproval =
        ClassifyReadOnly (catalogCallIsReadOnly fleet grokCallArgumentsDecoder)
    , appToolExecution = TurnSequential
    , appToolResourceClaims = Nothing
    , appToolPlacement = UnclassifiedTool
    }

mcpListResourcesTool :: McpFleet -> AppTool
mcpListResourcesTool fleet = AppTool
    { appToolName = "mcp_list_resources"
    , appToolDescription =
        "List the resources and resource templates an MCP server exposes. \
        \Omit `server` to query every connected server."
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
                pure $ object
                    [ "server" .= name
                    , "resources" .= either (const []) (map resourceJson) resources
                    , "resourceTemplates" .=
                        either (const []) (map templateJson) templates
                    , "error" .= either Just (const Nothing) resources
                    ]
            pure (Right (compactJson (object ["servers" .= listings])))
    , appToolApproval = AlwaysReadOnly
    , appToolExecution = ParallelSafe
    , appToolResourceClaims = Nothing
    , appToolPlacement = UnclassifiedTool
    }
  where
    resourceJson :: McpResource -> Value
    resourceJson resource = object
        [ "uri" .= resource.resourceUri
        , "name" .= resource.resourceName
        , "title" .= resource.resourceTitle
        , "description" .= resource.resourceDescription
        , "mimeType" .= resource.resourceMimeType
        , "size" .= resource.resourceSize
        ]
    templateJson :: McpResourceTemplate -> Value
    templateJson template = object
        [ "uriTemplate" .= template.templateUri
        , "name" .= template.templateName
        , "title" .= template.templateTitle
        , "description" .= template.templateDescription
        , "mimeType" .= template.templateMimeType
        ]

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
    , appToolPlacement = UnclassifiedTool
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

catalogCallIsReadOnly
    :: McpFleet
    -> Json.Decoder (Text, RawJson)
    -> ToolCall
    -> IO Bool
catalogCallIsReadOnly fleet decoder call =
    case Json.decodeEither decoder (TextEncoding.encodeUtf8 call.arguments) of
        Left _ -> pure False
        Right (name, _) -> do
            entries <- readTVarIO fleet.mcpFleetCatalog
            pure $ maybe False
                (.catalogTool.discoveredReadOnly)
                (Map.lookup name entries)

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
