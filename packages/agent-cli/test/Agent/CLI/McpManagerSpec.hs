module Agent.CLI.McpManagerSpec (spec) where

import Agent.CLI.Config
import Agent.CLI.McpAdd
import Agent.CLI.McpManager
import Agent.CLI.McpManager.Fullscreen
import Agent.CLI.Picker (PickerKey(..))
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
