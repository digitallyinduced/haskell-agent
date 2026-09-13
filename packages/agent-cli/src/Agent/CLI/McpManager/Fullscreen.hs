-- | Fullscreen Brick MCP manager. Every prompt stays in the alternate
-- screen, matching Grok Build's @/mcps@ modal: add a URL or command, inspect
-- tools, toggle, remove, and run HTTP OAuth from the same UI.
module Agent.CLI.McpManager.Fullscreen
    ( McpDashboardAction(..)
    , McpNotice(..)
    , McpServerMenuAction(..)
    , mcpAuthorizationBody
    , mcpDashboardBody
    , mcpDashboardEntries
    , mcpServerMenuBody
    , mcpServerMenuEntries
    , runFullscreenMcpManager
    ) where

import Agent.Runtime.Config
    ( HarnessConfig(..)
    , McpServerConfig(..)
    , loadHarnessConfigSnapshot
    , modifyHarnessConfig
    )
import Agent.CLI.McpAdd
    ( mcpServerForTarget
    , parseMcpAddName
    , parseMcpTarget
    , suggestMcpTargetName
    )
import Agent.CLI.McpManager
    ( McpEntry(..)
    , McpEntryStatus(..)
    , McpManagerState(..)
    , authorizedMcpUrls
    , initialMcpManagerState
    , mcpEntryTransport
    , pendingHttpAuthorizationUrl
    )
import Agent.Runtime.Browser (openBrowser)
import Agent.Runtime.McpOAuth
    ( McpLoginHost(..)
    , defaultLoginOptions
    , loginMcpWithHost
    , mcpOAuthCallbackTimeoutMicros
    )
import Agent.CLI.TUI.App
    ( FullscreenRuntime
    , emitUiEvent
    , requestFullscreenChoiceUntil
    , requestFullscreenChoiceWithBody
    , requestFullscreenText
    )
import Agent.MCP (McpToolRegistration)
import Agent.TUI.Model (UiEvent(UiSetNotice), progressNotice)
import System.Timeout (timeout)
import Data.Char (isControl)
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import System.OsPath (OsPath)

data McpDashboardAction
    = McpDashboardAdd
    | McpDashboardRestart
    | McpDashboardOpen !Int
    deriving (Eq, Show)

data McpServerMenuAction
    = McpServerToggle
    | McpServerAuth
    | McpServerRemove
    | McpServerBack
    deriving (Eq, Show)

data McpNotice = McpNotice !Bool !Text

data ManagerSnapshot = ManagerSnapshot
    { snapshotConfig :: !HarnessConfig
    , snapshotPending :: !(Set Text)
    , snapshotChanged :: !Bool
    , snapshotAuthorized :: !(Set Text)
    }

runFullscreenMcpManager
    :: FullscreenRuntime
    -> OsPath
    -> [McpToolRegistration]
    -> [Text]
    -> IO Bool
runFullscreenMcpManager runtime home registrations warnings =
    loadHarnessConfigSnapshot home >>= \case
        Left err -> do
            _ <-
                requestFullscreenChoiceWithBody
                    runtime
                    "MCP servers"
                    err
                    0
                    [("Close", "Return to the session")]
            pure False
        Right (revision, config) -> do
            authorized <- authorizedMcpUrls home config
            dashboard
                Nothing
                revision
                ManagerSnapshot
                    { snapshotConfig = config
                    , snapshotPending = Set.empty
                    , snapshotChanged = False
                    , snapshotAuthorized = authorized
                    }
  where
    dashboard notice revision snapshot = do
        let state = managerState snapshot notice
            entries = mcpDashboardEntries state
        choice <-
            requestFullscreenChoiceWithBody
                runtime
                "MCP servers"
                (mcpDashboardBody notice state)
                0
                (map snd entries)
        case choice >>= (`atIndex` entries) of
            Nothing -> pure snapshot.snapshotChanged
            Just (McpDashboardAdd, _) ->
                addServer notice revision snapshot
                    >>= continueDashboard
            Just (McpDashboardRestart, _) ->
                pure True
            Just (McpDashboardOpen index, _) ->
                case atIndex index state.mcpManagerEntries of
                    Nothing -> dashboard notice revision snapshot
                    Just entry -> serverMenu notice revision snapshot entry

    continueDashboard (nextNotice, nextRevision, nextSnapshot) =
        dashboard nextNotice nextRevision nextSnapshot

    addServer notice revision snapshot = do
        targetText <-
            requestFullscreenText
                runtime
                "Add MCP server"
                ( Text.unlines
                    [ "Paste a remote **http(s)** URL or a local stdio command."
                    , "Examples: `https://mcp.sentry.dev/mcp` or `npx -y @modelcontextprotocol/server-filesystem /path`."
                    ]
                )
                ""
        case targetText of
            Nothing -> pure (notice, revision, snapshot)
            Just raw -> case parseMcpTarget raw of
                Left err ->
                    addServer (Just (McpNotice False err)) revision snapshot
                Right target -> do
                    let suggested = suggestMcpTargetName target
                    nameText <-
                        requestFullscreenText
                            runtime
                            "Name"
                            ("Leave empty to use **"
                                <> markdownText 80 suggested
                                <> "**.")
                            suggested
                    case nameText of
                        Nothing -> pure (notice, revision, snapshot)
                        Just rawName ->
                            case parseMcpAddName
                                (if Text.null (Text.strip rawName)
                                    then suggested
                                    else rawName) of
                                Left err ->
                                    addServer
                                        (Just (McpNotice False err))
                                        revision
                                        snapshot
                                Right label
                                    | Map.member label
                                        snapshot.snapshotConfig.configMcpServers ->
                                        addServer
                                            (Just
                                                (McpNotice False
                                                    ("MCP server '"
                                                        <> label
                                                        <> "' already exists")))
                                            revision
                                            snapshot
                                    | otherwise ->
                                        persist
                                            revision
                                            snapshot
                                            (snapshot.snapshotConfig
                                                { configMcpServers =
                                                    Map.insert label
                                                        (mcpServerForTarget target)
                                                        snapshot.snapshotConfig.configMcpServers
                                                })
                                            (Set.insert label snapshot.snapshotPending)
                                            ("Added " <> label)
                                            >>= \case
                                                Left err ->
                                                    pure
                                                        ( Just (McpNotice False err)
                                                        , revision
                                                        , snapshot
                                                        )
                                                Right next ->
                                                    authorizeAddedHttpServer
                                                        next
                                                        label
                                                        (mcpServerForTarget target)

    serverMenu notice revision snapshot entry = do
        let actions = mcpServerMenuEntries entry
        choice <-
            requestFullscreenChoiceWithBody
                runtime
                entry.mcpEntryName
                (mcpServerMenuBody notice entry)
                0
                (map snd actions)
        case choice >>= (`atIndex` actions) of
            Nothing -> dashboard notice revision snapshot
            Just (McpServerBack, _) -> dashboard notice revision snapshot
            Just (McpServerToggle, _) ->
                let enabled = not entry.mcpEntryConfig.mcpEnabled
                    updated =
                        snapshot.snapshotConfig
                            { configMcpServers =
                                Map.insert entry.mcpEntryName
                                    (entry.mcpEntryConfig { mcpEnabled = enabled })
                                    snapshot.snapshotConfig.configMcpServers
                            }
                    message =
                        entry.mcpEntryName
                            <> if enabled then " enabled" else " disabled"
                in persist revision snapshot updated
                    (Set.insert entry.mcpEntryName snapshot.snapshotPending)
                    message
                    >>= afterPersist revision snapshot
            Just (McpServerRemove, _) -> do
                confirmed <- confirmRemove entry.mcpEntryName
                if not confirmed
                    then serverMenu notice revision snapshot entry
                    else
                        persist
                            revision
                            snapshot
                            (snapshot.snapshotConfig
                                { configMcpServers =
                                    Map.delete entry.mcpEntryName
                                        snapshot.snapshotConfig.configMcpServers
                                })
                            (Set.insert entry.mcpEntryName snapshot.snapshotPending)
                            ("Removed " <> entry.mcpEntryName)
                            >>= afterPersist revision snapshot
            Just (McpServerAuth, _) ->
                case entry.mcpEntryConfig.mcpUrl of
                    Nothing ->
                        serverMenu
                            (Just
                                (McpNotice False
                                    (entry.mcpEntryName
                                        <> " is a local stdio server; OAuth is only used for HTTP servers")))
                            revision
                            snapshot
                            entry
                    Just url ->
                        authorizeHttpServer
                            False
                            entry.mcpEntryName
                            url
                            revision
                            snapshot
                            >>= continueDashboard

    afterPersist revision snapshot = \case
        Left err -> dashboard (Just (McpNotice False err)) revision snapshot
        Right next ->
            dashboard
                (Just (McpNotice True next.message))
                next.persistedRevision
                next.persistedSnapshot

    persist revision snapshot updated pending message =
        persistSnapshot home revision snapshot updated pending message

    authorizeAddedHttpServer next label server =
        case pendingHttpAuthorizationUrl
            next.persistedSnapshot.snapshotAuthorized
            server of
            Nothing ->
                pure
                    ( Just (McpNotice True next.message)
                    , next.persistedRevision
                    , next.persistedSnapshot
                    )
            Just url ->
                authorizeHttpServer
                    True
                    label
                    url
                    next.persistedRevision
                    next.persistedSnapshot

    authorizeHttpServer addedFirst label url revision snapshot = do
        loginMcpWithHost
            (fullscreenMcpLoginHost runtime)
            defaultLoginOptions
            url
            >>= \case
                Left err ->
                    pure
                        ( Just
                            (McpNotice False $
                                if addedFirst
                                    then "Added " <> label <> ". " <> err
                                    else err)
                        , revision
                        , snapshot
                        )
                Right message -> do
                    nextAuthorized <-
                        authorizedMcpUrls home snapshot.snapshotConfig
                    pure
                        ( Just (McpNotice True message)
                        , revision
                        , snapshot
                            { snapshotPending = Set.insert label snapshot.snapshotPending
                            , snapshotChanged = True
                            , snapshotAuthorized = nextAuthorized
                            }
                        )

    confirmRemove name = do
        choice <-
            requestFullscreenChoiceWithBody
                runtime
                "Remove MCP server?"
                ("Remove **" <> markdownText 100 name
                    <> "** from ~/.haskell-agent/config.json?\n\n"
                    <> "The next MCP restart drops this server from the session.")
                1
                [ ("Remove", "Delete the saved server")
                , ("Keep server", "Return without changing anything")
                ]
        pure (choice == Just 0)

    managerState snapshot notice =
        initialMcpManagerState
            snapshot.snapshotConfig
            registrations
            warnings
            snapshot.snapshotPending
            snapshot.snapshotAuthorized
            (noticeText notice)

data PersistedUpdate = PersistedUpdate
    { persistedRevision :: !Word64
    , persistedSnapshot :: !ManagerSnapshot
    , message :: !Text
    }

persistSnapshot
    :: OsPath
    -> Word64
    -> ManagerSnapshot
    -> HarnessConfig
    -> Set Text
    -> Text
    -> IO (Either Text PersistedUpdate)
persistSnapshot home revision snapshot updated pending message =
    modifyHarnessConfig home
        (\current _ ->
            if current /= revision
                then Left "MCP configuration changed; reopen /mcp and retry"
                else Right (updated, ())) >>= \case
        Left err -> pure (Left err)
        Right (nextRevision, _, ()) -> do
            nextAuthorized <- authorizedMcpUrls home updated
            pure $ Right PersistedUpdate
                { persistedRevision = nextRevision
                , persistedSnapshot =
                    snapshot
                        { snapshotConfig = updated
                        , snapshotPending = pending
                        , snapshotChanged = True
                        , snapshotAuthorized = nextAuthorized
                        }
                , message = message
                }

mcpDashboardEntries
    :: McpManagerState
    -> [(McpDashboardAction, (Text, Text))]
mcpDashboardEntries state =
    [ ( McpDashboardAdd
      , ( "＋ Add MCP server"
        , "Paste a remote URL or a local stdio command"
        )
      )
    ]
        <> [ ( McpDashboardRestart
             , ( "↻ Restart MCP runtime"
               , "Apply pending configuration changes"
               )
             )
           | state.mcpManagerRestartPending
           ]
        <> zipWith
            (\index entry -> (McpDashboardOpen index, dashboardRow entry))
            [0 ..]
            state.mcpManagerEntries

dashboardRow :: McpEntry -> (Text, Text)
dashboardRow entry =
    ( health <> entry.mcpEntryName
    , Text.intercalate " · " $
        [ statusLabel entry.mcpEntryStatus
        , mcpEntryTransport entry
        ]
            <> case entry.mcpEntryConfig.mcpUrl of
                Just url -> [truncateText 48 url]
                Nothing -> [truncateText 48 entry.mcpEntryConfig.mcpCommand]
    )
  where
    health =
        if entry.mcpEntryConfig.mcpEnabled then "● " else "○ "

mcpDashboardBody :: Maybe McpNotice -> McpManagerState -> Text
mcpDashboardBody notice state =
    Text.intercalate "\n\n" $
        [ "Add, enable, authorize, or remove MCP servers without leaving the fullscreen UI."
        , "**"
            <> Text.pack (show (length state.mcpManagerEntries))
            <> " servers**"
            <> if state.mcpManagerRestartPending
                then " · restart pending"
                else ""
        ]
            <> maybe [] (pure . formatNotice) notice
            <> [ if null state.mcpManagerEntries
                    then "No MCP servers are configured. Add a remote URL or a local command to get started."
                    else "Select a server for tools, OAuth, enable/disable, or remove."
               ]

mcpServerMenuEntries :: McpEntry -> [(McpServerMenuAction, (Text, Text))]
mcpServerMenuEntries entry =
    catMaybes
        [ Just
            ( McpServerToggle
            , if entry.mcpEntryConfig.mcpEnabled
                then ("Disable", "Keep the server configured but skip it on the next restart")
                else ("Enable", "Include this server on the next MCP restart")
            )
        , case entry.mcpEntryConfig.mcpUrl of
            Nothing -> Nothing
            Just _ ->
                Just
                    ( McpServerAuth
                    , ( "Authenticate"
                      , "Open the browser OAuth flow for this HTTP server"
                      )
                    )
        , Just
            ( McpServerRemove
            , ("Remove", "Delete this server from ~/.haskell-agent/config.json")
            )
        , Just
            ( McpServerBack
            , ("Back", "Return to the MCP server list")
            )
        ]

mcpServerMenuBody :: Maybe McpNotice -> McpEntry -> Text
mcpServerMenuBody notice entry =
    Text.intercalate "\n\n" $
        [ "**" <> markdownText 80 entry.mcpEntryName <> "** · "
            <> statusLabel entry.mcpEntryStatus
            <> " · "
            <> mcpEntryTransport entry
        , case entry.mcpEntryConfig.mcpUrl of
            Just url -> "URL: `" <> url <> "`"
            Nothing ->
                "Command: `"
                    <> Text.unwords
                        (entry.mcpEntryConfig.mcpCommand
                            : entry.mcpEntryConfig.mcpArgs)
                    <> "`"
        ]
            <> maybe [] (pure . formatNotice) notice
            <> toolSection
            <> warningSection
  where
    toolSection = case entry.mcpEntryTools of
        [] -> ["Tools: none exposed yet."]
        tools ->
            ("Tools (" <> Text.pack (show (length tools)) <> "):")
                : [ "- **" <> markdownText 60 name <> "**"
                        <> if Text.null description
                            then ""
                            else " — " <> markdownText 88 description
                  | (name, description) <- take 24 tools
                  ]
                <> [ "… " <> Text.pack (show (length tools - 24)) <> " more"
                   | length tools > 24
                   ]
    warningSection =
        [ "Warning: " <> markdownText 120 (warningSummary warning)
        | warning <- entry.mcpEntryWarnings
        , not (" failed to start:" `Text.isInfixOf` warning)
        ]

statusLabel :: McpEntryStatus -> Text
statusLabel = \case
    McpDisabled -> "disabled"
    McpPendingRestart -> "restart pending"
    McpNeedsAuth -> "needs auth"
    McpReady count ->
        "ready · " <> Text.pack (show count)
            <> if count == 1 then " tool" else " tools"
    McpUnavailable reason -> "unavailable · " <> truncateText 72 reason

formatNotice :: McpNotice -> Text
formatNotice (McpNotice success message)
    | success = message
    | otherwise = "**" <> markdownText 160 message <> "**"

noticeText :: Maybe McpNotice -> Maybe (Bool, Text)
noticeText = fmap \(McpNotice success message) -> (success, message)

warningSummary :: Text -> Text
warningSummary warning =
    let marker = " skipped "
        (_, suffix) = Text.breakOn marker warning
    in if Text.null suffix
        then warning
        else "skipped " <> Text.strip (Text.drop (Text.length marker) suffix)

atIndex :: Int -> [a] -> Maybe a
atIndex index values
    | index < 0 = Nothing
    | otherwise = case drop index values of
        value : _ -> Just value
        [] -> Nothing

truncateText :: Int -> Text -> Text
truncateText limit value
    | Text.length value <= limit = value
    | otherwise = Text.take (max 0 (limit - 1)) value <> "…"

fullscreenMcpLoginHost :: FullscreenRuntime -> McpLoginHost
fullscreenMcpLoginHost runtime =
    McpLoginHost
        { mcpLoginSay =
            \message ->
                emitUiEvent runtime
                    (UiSetNotice (Just (progressNotice message)))
        , mcpLoginAuthorize = authorizeMcpFullscreen runtime
        }

authorizeMcpFullscreen
    :: FullscreenRuntime
    -> Text
    -> IO a
    -> IO (Either Text (Maybe a))
authorizeMcpFullscreen runtime url wait = do
    opened <- openBrowser url
    outcome <-
        requestFullscreenChoiceUntil
            runtime
            "Authorize MCP server"
            (mcpAuthorizationBody opened url)
            0
            [("Cancel", "Stop without saving credentials")]
            (timeout mcpOAuthCallbackTimeoutMicros wait)
    pure $ case outcome of
        Left _ -> Left "MCP authorization was cancelled."
        Right callback -> Right callback

mcpAuthorizationBody :: Bool -> Text -> Text
mcpAuthorizationBody opened url =
    Text.intercalate "\n\n"
        [ "[Open the authorization page](" <> url <> ")."
        , if opened
            then
                "A browser window was opened automatically. Complete the sign-in there. This view continues when the browser redirects back; you do not need to return here first."
            else
                "The browser could not be opened automatically. Use the link above. This view continues when the browser redirects back."
        ]

markdownText :: Int -> Text -> Text
markdownText limit =
    Text.concatMap escape . displayText limit
  where
    escape character
        | character `elem` ("\\`*_[]<>" :: String) =
            "\\" <> Text.singleton character
        | otherwise = Text.singleton character

displayText :: Int -> Text -> Text
displayText limit value =
    let stripped = Text.filter (not . isControl) value
    in if Text.length stripped <= limit
        then stripped
        else Text.take (max 0 (limit - 1)) stripped <> "…"
