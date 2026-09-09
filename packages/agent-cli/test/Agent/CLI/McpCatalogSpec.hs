module Agent.CLI.McpCatalogSpec (spec) where

import Agent.CLI.Config
    ( HarnessConfig(..)
    , McpOAuthConfig(..)
    , McpServerConfig(..)
    , defaultHarnessConfig
    , loadHarnessConfig
    , saveHarnessConfig
    )
import Agent.CLI.McpCatalog
import Agent.CLI.McpOAuth (lookupServerOAuthConfig, registerAuthorizedMcpServer)
import Agent.CLI.Options (McpAddCommand(..), McpAddTransport(..))
import Agent.MCP (McpProtocolPreference(..))
import Control.Exception.Safe (bracket)
import Control.Monad (forM_)
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
    describe "authorized MCP server registration" do
        it "persists a newly authorized endpoint enabled and visible in the catalog" $
            withTempDir \home -> do
                registerAuthorizedMcpServer home "https://mcp.stripe.com/"
                    `shouldReturn` Right ("mcp-stripe-com", True)
                config <- loadHarnessConfig home >>= either (fail . Text.unpack) pure
                Map.lookup "mcp-stripe-com" config.configMcpServers
                    `shouldBe` Just remoteServer { mcpUrl = Just "https://mcp.stripe.com/" }
                entries <- listMcpCatalog home >>= either (fail . show) pure
                map (.mcpCatalogName) entries `shouldBe` ["mcp-stripe-com"]

        it "preserves configured names, settings, and disabled state on repeated login" $
            withTempDir \home -> do
                let configured = catalogConfig
                        { configMcpServers = Map.singleton "billing" remoteServer
                            { mcpEnabled = False
                            , mcpRequestTimeoutSeconds = 123
                            , mcpEnv = Map.singleton "EXAMPLE" "preserved"
                            }
                        }
                saveHarnessConfig home configured `shouldReturn` Right ()
                registerAuthorizedMcpServer home "https://example.test/mcp"
                    `shouldReturn` Right ("billing", False)
                registerAuthorizedMcpServer home "https://example.test/mcp"
                    `shouldReturn` Right ("billing", False)
                loadHarnessConfig home `shouldReturn` Right configured

        forM_ [False, True] \enabled ->
            it ("reuses equivalent URL spellings with enabled = " <> show enabled) $
                withTempDir \home -> do
                    let oauth = McpOAuthConfig (Just "billing-client") Nothing Nothing ["billing"]
                        server = remoteServer
                            { mcpUrl = Just "https://EXAMPLE.test/mcp/?account=first"
                            , mcpEnabled = enabled
                            , mcpRequestTimeoutSeconds = 123
                            , mcpOAuth = Just oauth
                            }
                        configured = catalogConfig
                            { configMcpServers = Map.singleton "billing" server }
                        loginUrl = "https://example.test/mcp?account=first"
                    lookupServerOAuthConfig loginUrl configured `shouldBe` Just oauth
                    saveHarnessConfig home configured `shouldReturn` Right ()
                    registerAuthorizedMcpServer home loginUrl
                        `shouldReturn` Right ("billing", enabled)
                    registerAuthorizedMcpServer home loginUrl
                        `shouldReturn` Right ("billing", enabled)
                    loadHarnessConfig home `shouldReturn` Right configured
                        { configMcpServers = Map.singleton "billing" server
                            { mcpUrl = Just loginUrl } }

        it "does not reuse OAuth settings from a different endpoint query" do
            let oauth = McpOAuthConfig (Just "first-client") Nothing Nothing ["first"]
                configured = catalogConfig
                    { configMcpServers = Map.singleton "billing" remoteServer
                        { mcpUrl = Just "https://example.test/mcp?account=first"
                        , mcpOAuth = Just oauth
                        }
                    }
            lookupServerOAuthConfig "https://example.test/mcp?account=second" configured
                `shouldBe` Nothing
            lookupServerOAuthConfig "https://example.test/mcp" configured
                `shouldBe` Nothing

        it "allocates a stable unused name without overwriting other endpoints" $
            withTempDir \home -> do
                let configured = catalogConfig
                        { configMcpServers = Map.fromList
                            [ ("example-test", remoteServer { mcpUrl = Just "https://other.test/" })
                            , ("example-test-2", remoteServer { mcpUrl = Just "https://other.test/second" })
                            ]
                        }
                saveHarnessConfig home configured `shouldReturn` Right ()
                registerAuthorizedMcpServer home "https://example.test/mcp"
                    `shouldReturn` Right ("example-test-3", True)
                registerAuthorizedMcpServer home "https://example.test/mcp"
                    `shouldReturn` Right ("example-test-3", True)
                updated <- loadHarnessConfig home >>= either (fail . Text.unpack) pure
                Map.delete "example-test-3" updated.configMcpServers
                    `shouldBe` configured.configMcpServers

        it "does not collapse distinct endpoint queries into one credential key" $
            withTempDir \home -> do
                registerAuthorizedMcpServer home "https://example.test/mcp?account=first"
                    `shouldReturn` Right ("example-test", True)
                registerAuthorizedMcpServer home "https://example.test/mcp?account=second"
                    `shouldReturn` Right ("example-test-2", True)

        it "preserves unrelated configuration when adding a server" $
            withTempDir \home -> do
                saveHarnessConfig home catalogConfig `shouldReturn` Right ()
                registerAuthorizedMcpServer home "https://mcp.stripe.com/"
                    `shouldReturn` Right ("mcp-stripe-com", True)
                updated <- loadHarnessConfig home >>= either (fail . Text.unpack) pure
                updated { configMcpServers = Map.delete "mcp-stripe-com" updated.configMcpServers }
                    `shouldBe` catalogConfig

        it "rejects invalid endpoints without changing configuration" $
            withTempDir \home -> do
                saveHarnessConfig home catalogConfig `shouldReturn` Right ()
                result <- registerAuthorizedMcpServer home "file:///etc/example"
                result `shouldSatisfy` either (const True) (const False)
                loadHarnessConfig home `shouldReturn` Right catalogConfig

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

    it "adds remote HTTP and local stdio servers" $
        withTempDir \home -> do
            saveHarnessConfig home catalogConfig `shouldReturn` Right ()
            addMcpCatalogServer home
                McpAddCommand
                    { mcpAddName = "sentry"
                    , mcpAddTransport = Just McpAddTransportHttp
                    , mcpAddTarget = "https://mcp.sentry.dev/mcp"
                    , mcpAddArgs = []
                    }
                `shouldReturn` Right
                    remoteEntry
                        { mcpCatalogName = "sentry"
                        , mcpCatalogUrl = Just "https://mcp.sentry.dev/mcp"
                        }
            addMcpCatalogServer home
                McpAddCommand
                    { mcpAddName = "files"
                    , mcpAddTransport = Nothing
                    , mcpAddTarget = "npx"
                    , mcpAddArgs = ["-y", "@modelcontextprotocol/server-filesystem", "/tmp"]
                    }
                `shouldReturn` Right
                    McpCatalogEntry
                        { mcpCatalogName = "files"
                        , mcpCatalogEnabled = True
                        , mcpCatalogUrl = Nothing
                        , mcpCatalogCommand = "npx"
                        , mcpCatalogArgs =
                            ["-y", "@modelcontextprotocol/server-filesystem", "/tmp"]
                        , mcpCatalogCwd = Nothing
                        , mcpCatalogEnvKeys = []
                        }
            addMcpCatalogServer home
                McpAddCommand
                    { mcpAddName = "docs"
                    , mcpAddTransport = Nothing
                    , mcpAddTarget = "other"
                    , mcpAddArgs = []
                    }
                `shouldReturn` Left
                    (McpCatalogInvalid "MCP server docs already exists")

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
        , mcpConnectionId = Nothing
        , mcpConnectionGeneration = Nothing
        , mcpDisplayName = Nothing
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
        , mcpConnectionId = Nothing
        , mcpConnectionGeneration = Nothing
        , mcpDisplayName = Nothing
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
