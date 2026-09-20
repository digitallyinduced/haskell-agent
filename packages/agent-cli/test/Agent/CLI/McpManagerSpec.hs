module Agent.CLI.McpManagerSpec (spec) where

import Agent.Runtime.Config
import Agent.CLI.McpAdd
import Agent.CLI.McpManager
import Agent.CLI.McpManager.Fullscreen
import Agent.CLI.Picker (PickerKey(..))
import qualified Agent.MCP as MCP
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.McpManager" do
    describe "command parsing" do
        it "splits quoted stdio commands without invoking a shell" do
            parseMcpCommand
                "nix run '/path with spaces/seo-mcp' --flag=\"two words\""
                `shouldBe`
                    Right
                        ( "nix"
                        , [ "run"
                          , "/path with spaces/seo-mcp"
                          , "--flag=two words"
                          ]
                        )

        it "supports escaped spaces and empty quoted arguments" do
            parseMcpCommand "server one\\ two ''"
                `shouldBe` Right ("server", ["one two", ""])

        it "rejects incomplete quoting and escapes" do
            parseMcpCommand "server 'unfinished"
                `shouldBe`
                    Left "MCP command contains an unterminated quote"
            parseMcpCommand "server trailing\\"
                `shouldBe`
                    Left "MCP command ends with an incomplete escape"

        it "suggests useful labels for common launchers" do
            suggestMcpName "nix" ["run", "/repo/seo-mcp"]
                `shouldBe` "seo-mcp"
            suggestMcpName
                "npx"
                ["-y", "@modelcontextprotocol/server-filesystem", "/tmp"]
                `shouldBe` "server-filesystem"
            suggestMcpName "/usr/local/bin/my-server" []
                `shouldBe` "my-server"

        it "treats http(s) URLs as remote servers and suggests a host name" do
            parseMcpTarget "https://mcp.sentry.dev/mcp"
                `shouldBe` Right (McpAddHttpUrl "https://mcp.sentry.dev/mcp")
            suggestMcpNameFromUrl "https://mcp.sentry.dev/mcp"
                `shouldBe` "sentry"
            suggestMcpNameFromUrl "https://mcp.linear.app/mcp"
                `shouldBe` "linear"
            parseMcpTarget "npx -y @modelcontextprotocol/server-filesystem /tmp"
                `shouldBe`
                    Right
                        ( McpAddStdioCommand
                            "npx"
                            ["-y", "@modelcontextprotocol/server-filesystem", "/tmp"]
                        )

        it "rejects empty names and names with punctuation" do
            parseMcpAddName "  "
                `shouldBe` Left "MCP server name must not be empty"
            parseMcpAddName "cool server"
                `shouldBe`
                    Left
                        "MCP server names may only contain letters, numbers, hyphens, and underscores"
            parseMcpAddName "sentry" `shouldBe` Right "sentry"

    describe "navigation and actions" do
        let alpha = server True "alpha-command"
            beta = server False "beta-command"
            remote = httpServer "https://mcp.example.test/mcp"
            state =
                initialMcpManagerState
                    defaultHarnessConfig
                        { configMcpServers =
                            Map.fromList
                                [ ("alpha", alpha)
                                , ("beta", beta)
                                , ("remote", remote)
                                ]
                        }
                    []
                    []
                    []
                    Set.empty
                    Set.empty
                    Nothing

        it "moves, expands, toggles, removes, adds, authorizes, and refreshes" do
            applyMcpManagerKey PickerKeyDown state
                `shouldSatisfy` \case
                    Right moved -> moved.mcpManagerIndex == 1
                    Left _ -> False
            applyMcpManagerKey PickerKeyConfirm state
                `shouldSatisfy` \case
                    Right expanded ->
                        expanded.mcpManagerExpanded == Just "alpha"
                    Left _ -> False
            applyMcpManagerKey (PickerKeyChar ' ') state
                `shouldBe` Left (McpManagerToggle "alpha")
            applyMcpManagerKey (PickerKeyChar 'x') state
                `shouldSatisfy` \case
                    Right confirming ->
                        confirming.mcpManagerConfirmRemove == Just "alpha"
                    Left _ -> False
            applyMcpManagerKey (PickerKeyChar 'a') state
                `shouldSatisfy` \case
                    Right adding -> adding.mcpManagerAddForm == Just emptyMcpAddForm
                    Left _ -> False
            applyMcpManagerKey (PickerKeyChar 'i') state
                `shouldBe` Left (McpManagerAuth "alpha")
            applyMcpManagerKey (PickerKeyChar 'r') state
                `shouldBe` Left McpManagerRestart

        it "confirms removal only on lowercase y" do
            let confirming =
                    state { mcpManagerConfirmRemove = Just "alpha" }
            applyMcpManagerKey (PickerKeyChar 'y') confirming
                `shouldBe` Left (McpManagerRemove "alpha")
            applyMcpManagerKey (PickerKeyChar 'n') confirming
                `shouldSatisfy` \case
                    Right cancelled ->
                        cancelled.mcpManagerConfirmRemove == Nothing
                    Left _ -> False

        it "submits an HTTP server from the in-overlay add form" do
            let typed =
                    typeAddForm
                        "https://mcp.sentry.dev/mcp"
                        (fromRightState
                            (applyMcpManagerKey (PickerKeyChar 'a') state))
            applyMcpManagerKey PickerKeyConfirm typed
                `shouldBe`
                    Left
                        (McpManagerSubmitAdd "sentry"
                            (httpServer "https://mcp.sentry.dev/mcp"))

        it "tabs to the name field and uses an explicit label" do
            let opened =
                    fromRightState
                        (applyMcpManagerKey (PickerKeyChar 'a') state)
                withUrl = typeAddForm "https://mcp.sentry.dev/mcp" opened
                named =
                    typeAddForm "custom"
                        (fromRightState
                            (applyMcpManagerKey PickerKeyTab withUrl))
            applyMcpManagerKey PickerKeyConfirm named
                `shouldBe`
                    Left
                        (McpManagerSubmitAdd "custom"
                            (httpServer "https://mcp.sentry.dev/mcp"))

        it "renders status, command details, remote URLs, and hidden environment values" do
            let configured = alpha
                    { mcpArgs = ["arg with spaces"]
                    , mcpEnv = Map.singleton "TOKEN" "do-not-render"
                    }
                expanded =
                    (initialMcpManagerState
                        defaultHarnessConfig
                            { configMcpServers =
                                Map.fromList
                                    [ ("alpha", configured)
                                    , ("remote", remote)
                                    ]
                            }
                        []
                        [ "MCP server remote failed to start: MCP server requires OAuth authorization; run `agent mcp login <url>`"
                        ]
                        []
                        Set.empty
                        Set.empty
                        Nothing)
                        { mcpManagerExpanded = Just "alpha" }
                frame = renderMcpManagerFrame False expanded
            frame `shouldSatisfy` Text.isInfixOf "[ready]"
            frame `shouldSatisfy`
                Text.isInfixOf "alpha-command 'arg with spaces'"
            frame `shouldSatisfy` Text.isInfixOf "TOKEN (values hidden)"
            frame `shouldNotSatisfy` Text.isInfixOf "do-not-render"
            frame `shouldSatisfy` Text.isInfixOf " · http"
            frame `shouldSatisfy` Text.isInfixOf "[needs auth]"
            let remoteExpanded =
                    expanded { mcpManagerExpanded = Just "remote" }
            renderMcpManagerFrame False remoteExpanded
                `shouldSatisfy` Text.isInfixOf "url: https://mcp.example.test/mcp"

        it "renders the add form with Grok-style field labels" do
            let adding =
                    fromRightState
                        (applyMcpManagerKey (PickerKeyChar 'a') state)
                frame = renderMcpManagerFrame False adding
            frame `shouldSatisfy` Text.isInfixOf "URL / Command"
            frame `shouldSatisfy` Text.isInfixOf "Auto generated"
            frame `shouldSatisfy` Text.isInfixOf "Tab/Shift+Tab field"

    describe "live server status" do
        let manager current count enabled pending warnings =
                initialMcpManagerState
                    defaultHarnessConfig
                        { configMcpServers =
                            Map.singleton "posthog"
                                ((httpServer "https://mcp.posthog.com/mcp")
                                    { mcpEnabled = enabled })
                        }
                    []
                    warnings
                    [MCP.McpServerStatus "posthog" current count]
                    pending
                    (Set.singleton "https://mcp.posthog.com/mcp")
                    Nothing
            entryStatus state = map (.mcpEntryStatus) state.mcpManagerEntries

        it "uses the live tool count even when the startup registrations are empty" do
            let state = manager MCP.McpReady 2 True Set.empty []
            entryStatus state `shouldBe` [McpReady 2]
            renderMcpManagerFrame False state
                `shouldSatisfy` Text.isInfixOf "[ready] · 2 tools"
            map (snd . snd) (mcpDashboardEntries state)
                `shouldSatisfy` any (Text.isInfixOf "ready · 2 tools")

        it "does not describe pending or initializing servers as ready with zero tools" do
            mapM_ (\current -> do
                let state = manager current 0 True Set.empty []
                entryStatus state `shouldBe` [McpConnecting]
                renderMcpManagerFrame False state
                    `shouldSatisfy` Text.isInfixOf "[connecting]")
                [MCP.McpPending, MCP.McpInitializing]

        it "reports failures and closed connections from the live state" do
            entryStatus (manager (MCP.McpFailed "connection refused") 0 True Set.empty [])
                `shouldBe` [McpUnavailable "connection refused"]
            entryStatus (manager MCP.McpClosed 0 True Set.empty [])
                `shouldBe` [McpUnavailable "connection closed"]

        it "retains OAuth guidance for authorization failures" do
            entryStatus (manager (MCP.McpFailed "MCP server requires OAuth authorization") 0 True Set.empty [])
                `shouldBe` [McpNeedsAuth]

        it "renders live failures once while retaining discovery diagnostics" do
            let state = (manager (MCP.McpFailed "connection refused") 0 True Set.empty
                    [ "MCP server posthog failed: connection refused"
                    , "MCP server posthog skipped rejected tool"
                    ]) { mcpManagerExpanded = Just "posthog" }
                frame = renderMcpManagerFrame False state
            Text.count "connection refused" frame `shouldBe` 1
            frame `shouldSatisfy` Text.isInfixOf "skipped rejected tool"
            map (Text.count "connection refused" . mcpServerMenuBody Nothing)
                state.mcpManagerEntries `shouldBe` [1]
            map (mcpServerMenuBody Nothing) state.mcpManagerEntries
                `shouldSatisfy` all (Text.isInfixOf "skipped rejected tool")

        it "shows discovery diagnostics for ready servers" do
            let state = (manager MCP.McpReady 1 True Set.empty
                    ["MCP server posthog skipped rejected tool"])
                    { mcpManagerExpanded = Just "posthog" }
            renderMcpManagerFrame False state
                `shouldSatisfy` Text.isInfixOf "skipped rejected tool"
            map (mcpServerMenuBody Nothing) state.mcpManagerEntries
                `shouldSatisfy` all (Text.isInfixOf "skipped rejected tool")

        it "does not infer OAuth from a missing token for network failures" do
            let config = defaultHarnessConfig
                    { configMcpServers = Map.singleton "public"
                        (httpServer "https://public.example.test/mcp") }
                state reason = initialMcpManagerState config [] []
                    [MCP.McpServerStatus "public" (MCP.McpFailed reason) 0]
                    Set.empty Set.empty Nothing
            mapM_ (\reason ->
                entryStatus (state reason) `shouldBe` [McpUnavailable reason])
                ["connection refused", "DNS lookup failed", "request timed out"]
            entryStatus (state "HTTP 401 Unauthorized") `shouldBe` [McpNeedsAuth]

        it "preserves nested failures and non-duplicate top-level warnings in both renderers" do
            let diagnostics =
                    [ "MCP server posthog skills/list failed: catalog unavailable"
                    , "MCP server posthog prompts/list failed to start: catalog unavailable"
                    , "MCP server posthog failed: earlier failure"
                    ]
            mapM_ (\current -> do
                let state = (manager current 1 True Set.empty diagnostics)
                        { mcpManagerExpanded = Just "posthog" }
                    frames = renderMcpManagerFrame False state
                        : map (mcpServerMenuBody Nothing) state.mcpManagerEntries
                mapM_ (\warning ->
                    frames `shouldSatisfy` all (Text.isInfixOf warning)) diagnostics)
                [MCP.McpReady, MCP.McpFailed "connection refused"]

        it "suppresses only matching current or legacy top-level failures" do
            mapM_ (\marker -> do
                let state = (manager (MCP.McpFailed "connection refused") 0 True Set.empty
                        ["MCP server posthog" <> marker <> "connection refused"])
                        { mcpManagerExpanded = Just "posthog" }
                    frames = renderMcpManagerFrame False state
                        : map (mcpServerMenuBody Nothing) state.mcpManagerEntries
                map (Text.count "connection refused") frames `shouldBe` [1, 1])
                [" failed: ", " failed to start: "]

        it "refreshes runtime entries without resetting interaction state" do
            let config = defaultHarnessConfig
                    { configMcpServers = Map.fromList
                        [ ("pending", server True "server")
                        , ("posthog", httpServer "https://mcp.posthog.com/mcp")
                        ] }
                pending = Set.singleton "pending"
                initial = (initialMcpManagerState config [] []
                    [MCP.McpServerStatus "posthog" MCP.McpInitializing 0]
                    pending Set.empty (Just (True, "Saved")))
                    { mcpManagerIndex = 1
                    , mcpManagerExpanded = Just "posthog"
                    , mcpManagerAddForm = Just emptyMcpAddForm
                        { mcpAddTarget = "https://partial", mcpAddTargetCursor = 15 }
                    , mcpManagerConfirmRemove = Just "posthog"
                    }
                updated = refreshMcpManagerState config pending Set.empty
                    ([], ["MCP server posthog skipped rejected tool"],
                        [MCP.McpServerStatus "posthog" MCP.McpReady 2]) initial
            entryStatus updated `shouldBe` [McpPendingRestart, McpReady 2]
            map (.mcpEntryWarnings) updated.mcpManagerEntries
                `shouldBe` [[], ["MCP server posthog skipped rejected tool"]]
            updated { mcpManagerEntries = initial.mcpManagerEntries }
                `shouldBe` initial

        it "prefers current readiness to obsolete startup failure warnings" do
            entryStatus (manager MCP.McpReady 1 True Set.empty
                ["MCP server posthog failed to start: connection refused"])
                `shouldBe` [McpReady 1]

        it "keeps configuration changes ahead of live runtime status" do
            entryStatus (manager MCP.McpReady 1 False Set.empty [])
                `shouldBe` [McpDisabled]
            entryStatus (manager MCP.McpReady 1 True (Set.singleton "posthog") [])
                `shouldBe` [McpPendingRestart]

    describe "fullscreen dashboard" do
        it "offers add, restart when pending, and per-server actions including auth for HTTP" do
            let pending =
                    initialMcpManagerState
                        defaultHarnessConfig
                            { configMcpServers =
                                Map.singleton "remote"
                                    (httpServer "https://mcp.example.test/mcp")
                            }
                        []
                        []
                        []
                        (Set.singleton "remote")
                        Set.empty
                        Nothing
                entries = mcpDashboardEntries pending
            fmap fst entries
                `shouldBe` [McpDashboardAdd, McpDashboardRestart, McpDashboardOpen 0]
            mcpDashboardBody Nothing pending
                `shouldSatisfy` Text.isInfixOf "restart pending"
            case pending.mcpManagerEntries of
                entry : _ ->
                    fmap fst (mcpServerMenuEntries entry)
                        `shouldBe`
                            [ McpServerToggle
                            , McpServerAuth
                            , McpServerRemove
                            , McpServerBack
                            ]
                [] -> expectationFailure "expected a configured HTTP server"

        it "renders the authorization URL in the overlay when the browser cannot open" do
            let url = "https://auth.example.test/authorize?client_id=mcp"
                body = mcpAuthorizationBody False url
            body `shouldSatisfy` Text.isInfixOf url
            body `shouldSatisfy`
                Text.isInfixOf "could not be opened automatically"
            body `shouldSatisfy` Text.isInfixOf "redirects back"
            body `shouldNotSatisfy` Text.isInfixOf "Continue"

        it "starts HTTP OAuth after add when no token is stored" do
            let remote = httpServer "https://mcp.example.test/mcp"
            pendingHttpAuthorizationUrl Set.empty remote
                `shouldBe` Just "https://mcp.example.test/mcp"
            pendingHttpAuthorizationUrl
                (Set.singleton "https://mcp.example.test/mcp")
                remote
                `shouldBe` Nothing
            pendingHttpAuthorizationUrl Set.empty (server True "stdio-command")
                `shouldBe` Nothing

server :: Bool -> Text.Text -> McpServerConfig
server enabled command =
    defaultMcpServerConfig
        { mcpEnabled = enabled
        , mcpCommand = command
        }

httpServer :: Text.Text -> McpServerConfig
httpServer url =
    defaultMcpServerConfig { mcpUrl = Just url }

typeAddForm :: Text.Text -> McpManagerState -> McpManagerState
typeAddForm text state =
    foldl'
        (\current char ->
            fromRightState (applyMcpManagerKey (PickerKeyChar char) current))
        state
        (Text.unpack text)

fromRightState
    :: Either McpManagerAction McpManagerState -> McpManagerState
fromRightState = \case
    Right state -> state
    Left action ->
        error ("expected overlay state, got " <> show action)
