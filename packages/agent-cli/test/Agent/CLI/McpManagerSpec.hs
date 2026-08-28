module Agent.CLI.McpManagerSpec (spec) where

import Agent.CLI.Config
import Agent.CLI.McpManager
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

    describe "navigation and actions" do
        let alpha = server True "alpha-command"
            beta = server False "beta-command"
            state =
                initialMcpManagerState
                    defaultHarnessConfig
                        { configMcpServers =
                            Map.fromList
                                [ ("alpha", alpha)
                                , ("beta", beta)
                                ]
                        }
                    []
                    []
                    Set.empty
                    Nothing

        it "moves, expands, toggles, removes, adds, and refreshes" do
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
                `shouldBe` Left (McpManagerRemove "alpha")
            applyMcpManagerKey (PickerKeyChar 'a') state
                `shouldBe` Left McpManagerAdd
            applyMcpManagerKey (PickerKeyChar 'r') state
                `shouldBe` Left McpManagerRestart

        it "renders status, command details, and hidden environment values" do
            let configured = alpha
                    { mcpArgs = ["arg with spaces"]
                    , mcpEnv = Map.singleton "TOKEN" "do-not-render"
                    }
                expanded =
                    (initialMcpManagerState
                        defaultHarnessConfig
                            { configMcpServers =
                                Map.singleton "alpha" configured
                            }
                        []
                        []
                        Set.empty
                        Nothing)
                        { mcpManagerExpanded = Just "alpha" }
                frame = renderMcpManagerFrame False expanded
            frame `shouldSatisfy` Text.isInfixOf "[ready]"
            frame `shouldSatisfy`
                Text.isInfixOf "alpha-command 'arg with spaces'"
            frame `shouldSatisfy` Text.isInfixOf "TOKEN (values hidden)"
            frame `shouldNotSatisfy` Text.isInfixOf "do-not-render"

server :: Bool -> Text.Text -> McpServerConfig
server enabled command = McpServerConfig
    { mcpEnabled = enabled
    , mcpUrl = Nothing
    , mcpCommand = command
    , mcpArgs = []
    , mcpCwd = Nothing
    , mcpEnv = Map.empty
    , mcpStartupTimeoutSeconds = 30
    , mcpRequestTimeoutSeconds = 60
    , mcpOAuth = Nothing
    }
