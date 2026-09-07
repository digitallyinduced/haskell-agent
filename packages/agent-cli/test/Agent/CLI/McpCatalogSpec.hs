module Agent.CLI.McpCatalogSpec (spec) where

import Agent.CLI.Config
    ( HarnessConfig(..)
    , McpServerConfig(..)
    , defaultHarnessConfig
    , loadHarnessConfig
    , saveHarnessConfig
    )
import Agent.CLI.McpCatalog
import Agent.MCP (McpProtocolPreference(..))
import Control.Exception.Safe (bracket)
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import System.Directory.OsPath (getTemporaryDirectory)
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.OsPath (OsPath, decodeUtf, unsafeEncodeUtf)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.McpCatalog" do
    it "lists configured servers without exposing environment values" $
        withTempDir \home -> do
            saveHarnessConfig home catalogConfig `shouldReturn` Right ()
            listMcpCatalog home `shouldReturn` Right
                [ docsEntry False
                , remoteEntry
                ]
            formatMcpCatalogHuman
                [ docsEntry False
                , remoteEntry
                ]
                `shouldBe` Text.unlines
                    [ "docs (disabled)  stdio  mcp-docs --stdio"
                    , Text.justifyLeft 15 ' ' "remote"
                        <> "  http  https://example.test/mcp"
                    ]
            formatMcpCatalogHuman [docsEntry False, remoteEntry]
                `shouldSatisfy` (not . Text.isInfixOf "top-secret")
            show (docsEntry False) `shouldNotContain` "top-secret"
            Aeson.decode (formatMcpCatalogJSON [docsEntry False, remoteEntry])
                `shouldBe` Just (Aeson.toJSON
                    [ Aeson.object
                        [ "name" Aeson..= ("docs" :: Text.Text)
                        , "enabled" Aeson..= False
                        , "transport" Aeson..= ("stdio" :: Text.Text)
                        , "url" Aeson..= Aeson.Null
                        , "command" Aeson..= ("mcp-docs" :: Text.Text)
                        , "args" Aeson..= ["--stdio" :: Text.Text]
                        , "cwd" Aeson..= ("/tmp" :: Text.Text)
                        , "envKeys" Aeson..= ["TOKEN" :: Text.Text]
                        ]
                    , Aeson.object
                        [ "name" Aeson..= ("remote" :: Text.Text)
                        , "enabled" Aeson..= True
                        , "transport" Aeson..= ("http" :: Text.Text)
                        , "url" Aeson..= ("https://example.test/mcp" :: Text.Text)
                        , "command" Aeson..= ("" :: Text.Text)
                        , "args" Aeson..= ([] :: [Text.Text])
                        , "cwd" Aeson..= Aeson.Null
                        , "envKeys" Aeson..= ([] :: [Text.Text])
                        ]
                    ])

    it "says when no servers are configured" do
        formatMcpCatalogHuman []
            `shouldBe` "No MCP servers configured\n"

    it "enables and disables a named server idempotently" $
        withTempDir \home -> do
            saveHarnessConfig home catalogConfig `shouldReturn` Right ()
            setMcpCatalogEnabled home "docs" False `shouldReturn`
                Right McpCatalogChange
                    { mcpCatalogChanged = False
                    , mcpCatalogEntry = docsEntry False
                    }
            setMcpCatalogEnabled home "docs" True `shouldReturn`
                Right McpCatalogChange
                    { mcpCatalogChanged = True
                    , mcpCatalogEntry = docsEntry True
                    }
            fmap (fmap (Map.lookup "docs" . (.configMcpServers)))
                (loadHarnessConfig home)
                `shouldReturn` Right (Just docsServer { mcpEnabled = True })
            setMcpCatalogEnabled home "docs" True `shouldReturn`
                Right McpCatalogChange
                    { mcpCatalogChanged = False
                    , mcpCatalogEntry = docsEntry True
                    }
            formatMcpCatalogChange True McpCatalogChange
                { mcpCatalogChanged = True
                , mcpCatalogEntry = docsEntry True
                }
                `shouldBe` "Enabled MCP server docs"
            formatMcpCatalogChange False McpCatalogChange
                { mcpCatalogChanged = False
                , mcpCatalogEntry = docsEntry False
                }
                `shouldBe` "MCP server docs is already disabled"

    it "rejects unknown and empty names without rewriting config" $
        withTempDir \home -> do
            saveHarnessConfig home catalogConfig `shouldReturn` Right ()
            setMcpCatalogEnabled home "missing" False
                `shouldReturn` Left (McpCatalogNotFound "missing")
            setMcpCatalogEnabled home "  " True
                `shouldReturn` Left
                    (McpCatalogInvalid "MCP server name must not be empty")
            loadHarnessConfig home `shouldReturn` Right catalogConfig

docsEntry :: Bool -> McpCatalogEntry
docsEntry enabled =
    McpCatalogEntry
        { mcpCatalogName = "docs"
        , mcpCatalogEnabled = enabled
        , mcpCatalogUrl = Nothing
        , mcpCatalogCommand = "mcp-docs"
        , mcpCatalogArgs = ["--stdio"]
        , mcpCatalogCwd = Just "/tmp"
        , mcpCatalogEnvKeys = ["TOKEN"]
        }

remoteEntry :: McpCatalogEntry
remoteEntry =
    McpCatalogEntry
        { mcpCatalogName = "remote"
        , mcpCatalogEnabled = True
        , mcpCatalogUrl = Just "https://example.test/mcp"
        , mcpCatalogCommand = ""
        , mcpCatalogArgs = []
        , mcpCatalogCwd = Nothing
        , mcpCatalogEnvKeys = []
        }

catalogConfig :: HarnessConfig
catalogConfig =
    defaultHarnessConfig
        { configMcpServers =
            Map.fromList
                [ ("docs", docsServer)
                , ("remote", remoteServer)
                ]
        }

docsServer :: McpServerConfig
docsServer =
    McpServerConfig
        { mcpEnabled = False
        , mcpUrl = Nothing
        , mcpCommand = "mcp-docs"
        , mcpArgs = ["--stdio"]
        , mcpCwd = Just "/tmp"
        , mcpEnv = Map.singleton "TOKEN" "top-secret"
        , mcpStartupTimeoutSeconds = 30
        , mcpRequestTimeoutSeconds = 60
        , mcpOAuth = Nothing
        , mcpProtocol = McpProtocolAuto
        , mcpRoots = False
        , mcpSampling = False
        , mcpLogLevel = Nothing
        }

remoteServer :: McpServerConfig
remoteServer =
    McpServerConfig
        { mcpEnabled = True
        , mcpUrl = Just "https://example.test/mcp"
        , mcpCommand = ""
        , mcpArgs = []
        , mcpCwd = Nothing
        , mcpEnv = Map.empty
        , mcpStartupTimeoutSeconds = 30
        , mcpRequestTimeoutSeconds = 60
        , mcpOAuth = Nothing
        , mcpProtocol = McpProtocolAuto
        , mcpRoots = False
        , mcpSampling = False
        , mcpLogLevel = Nothing
        }

withTempDir :: (OsPath -> IO a) -> IO a
withTempDir action = do
    tmp <- getTemporaryDirectory
    bracket
        (mkdtemp (filePath tmp FilePath.</> "mcp-catalog-"))
        Directory.removeDirectoryRecursive
        (action . unsafeEncodeUtf)

filePath :: OsPath -> FilePath
filePath value = either (error . show) id (decodeUtf value)
