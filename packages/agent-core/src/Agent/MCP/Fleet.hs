module Agent.MCP.Fleet where


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
import Agent.MCP.Types
import Agent.MCP.Client
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
