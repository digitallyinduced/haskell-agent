module Agent.CLI.McpConnectionSpec (spec) where

import Agent.CLI.Config (HarnessConfig(..), McpServerConfig(..), loadHarnessConfig, harnessConfigPath, saveHarnessConfig, mcpUsesConnectionCredentials)
import Control.Concurrent.Async (concurrently)
import Agent.CLI.McpAdmin
import Agent.CLI.McpConnection
import Control.Exception.Safe (bracket)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import System.Directory.OsPath (getTemporaryDirectory)
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.OsPath (OsPath, decodeUtf, unsafeEncodeUtf)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.McpConnection" do
    it "migrates legacy CLI connections once and manages them by identity without renaming keys" $
        withTempDir \home -> do
            path <- decodeUtf (harnessConfigPath home)
            Directory.createDirectoryIfMissing True (FilePath.takeDirectory path)
            writeFile path "{\"version\":1,\"mcpServers\":{\"saasflow\":{\"url\":\"https://example.test/mcp\",\"env\":{\"MCP_ACCESS_TOKEN\":\"test-token\"},\"roots\":true},\"local\":{\"command\":\"test-server\"}}}"
            (Right first, Right second) <- concurrently
                (listMcpConnections home) (listMcpConnections home)
            first `shouldBe` second
            [connection] <- pure first.mcpAdminValue
            connection.connectionDisplayName `shouldBe` "saasflow"
            Right config <- loadHarnessConfig home
            let server = config.configMcpServers Map.! "saasflow"
            mcpUsesConnectionCredentials server `shouldBe` False
            server.mcpEnv `shouldBe` Map.singleton "MCP_ACCESS_TOKEN" "test-token"
            server.mcpRoots `shouldBe` True
            (config.configMcpServers Map.! "local").mcpConnectionId `shouldBe` Nothing
            saveHarnessConfig home config `shouldReturn` Right ()
            listMcpConnections home `shouldReturn` Right first
            let host = McpConnectionAuthorizationHost
                    { connectionLoadCredential = \_ -> expectationFailure "Unexpected credential read" >> pure (Right Nothing)
                    , connectionSaveCredential = \_ _ -> expectationFailure "Unexpected credential write" >> pure (Right ())
                    , connectionProbe = \_ _ -> expectationFailure "Unexpected probe" >> pure (Right McpConnectionReady)
                    }
            authorizeMcpConnectionWith host home first.mcpAdminRevision connection.connectionId
                (\_ -> expectationFailure "Unexpected browser launch" >> pure (Right ()))
                `shouldReturn` Left (McpAdminInvalid "This connection uses CLI-configured credentials. Use agent-cli mcp login to update its authorization.")
            listMcpConnections home `shouldReturn` Right first
            Right renamed <- renameMcpConnection home first.mcpAdminRevision connection.connectionId "Finance"
            Right disabled <- setMcpConnectionEnabled home renamed.mcpAdminRevision connection.connectionId False
            Right changed <- loadHarnessConfig home
            Map.keys changed.configMcpServers `shouldBe` ["local", "saasflow"]
            mcpUsesConnectionCredentials (changed.configMcpServers Map.! "saasflow") `shouldBe` False
            Right _ <- removeMcpConnectionWith
                (const (expectationFailure "must not delete unrelated protected credentials" >> pure (Right ())))
                home disabled.mcpAdminRevision connection.connectionId
            Right remaining <- loadHarnessConfig home
            Map.keys remaining.configMcpServers `shouldBe` ["local"]

    it "creates distinct identities and namespaces for the same endpoint" $
        withTempDir \home -> do
            Right initial <- listMcpConnections home
            Right first <- createMcpConnection home initial.mcpAdminRevision
                "First account" "https://example.test/mcp"
            Right second <- createMcpConnection home first.mcpAdminRevision
                "Second account" "https://example.test/mcp"
            first.mcpAdminValue.connectionId
                `shouldNotBe` second.mcpAdminValue.connectionId
            Right listed <- listMcpConnections home
            listed.mcpAdminValue `shouldMatchList`
                [first.mcpAdminValue, second.mcpAdminValue]
            Right config <- loadHarnessConfig home
            Map.keys config.configMcpServers `shouldMatchList`
                [ connectionServerName first.mcpAdminValue.connectionId
                , connectionServerName second.mcpAdminValue.connectionId
                ]
            map (.mcpEnv) (Map.elems config.configMcpServers)
                `shouldBe` [Map.empty, Map.empty]

    it "renames without changing endpoint or identity and rejects stale writes" $
        withTempDir \home -> do
            Right initial <- listMcpConnections home
            Right added <- createMcpConnection home initial.mcpAdminRevision
                "Account" "https://example.test/mcp"
            let identifier = added.mcpAdminValue.connectionId
            Right renamed <- renameMcpConnection home added.mcpAdminRevision
                identifier "Renamed account"
            renamed.mcpAdminValue `shouldBe`
                added.mcpAdminValue { connectionDisplayName = "Renamed account" }
            setMcpConnectionEnabled home added.mcpAdminRevision identifier False
                `shouldReturn` Left (McpAdminConflict renamed.mcpAdminRevision)
            Right disabled <- setMcpConnectionEnabled home renamed.mcpAdminRevision
                identifier False
            disabled.mcpAdminValue.connectionEnabled `shouldBe` False
            Right removed <- removeMcpConnectionWith (const (pure (Right ()))) home disabled.mcpAdminRevision identifier
            Right listed <- listMcpConnections home
            listed.mcpAdminRevision `shouldBe` removed.mcpAdminRevision
            listed.mcpAdminValue `shouldBe` []

    it "keeps removal retryable when protected credential deletion fails" $
        withTempDir \home -> do
            Right initial <- listMcpConnections home
            Right added <- createMcpConnection home initial.mcpAdminRevision
                "Account" "https://example.test/mcp"
            let identifier = added.mcpAdminValue.connectionId
            removeMcpConnectionWith (const (pure (Left "Store unavailable")))
                home added.mcpAdminRevision identifier
                `shouldReturn` Left (McpAdminInvalid "Store unavailable")
            Right listed <- listMcpConnections home
            listed.mcpAdminRevision `shouldBe` added.mcpAdminRevision
            listed.mcpAdminValue `shouldBe` [added.mcpAdminValue]
            Right _ <- removeMcpConnectionWith (const (pure (Right ())))
                home listed.mcpAdminRevision identifier
            Right empty <- listMcpConnections home
            empty.mcpAdminValue `shouldBe` []

    it "does not delete credentials for a stale removal request" $
        withTempDir \home -> do
            Right initial <- listMcpConnections home
            Right added <- createMcpConnection home initial.mcpAdminRevision
                "Account" "https://example.test/mcp"
            removeMcpConnectionWith
                (\_ -> expectationFailure "Unexpected credential deletion" >> pure (Right ()))
                home initial.mcpAdminRevision added.mcpAdminValue.connectionId
                `shouldReturn` Left (McpAdminConflict added.mcpAdminRevision)

    it "preserves endpoint query parameters used for server selection" $
        withTempDir \home -> do
            Right initial <- listMcpConnections home
            Right added <- createMcpConnection home initial.mcpAdminRevision
                "Account" "https://example.test/mcp?workspace=example"
            added.mcpAdminValue.connectionUrl
                `shouldBe` "https://example.test/mcp?workspace=example"

    it "verifies unauthenticated servers without opening a browser or storing credentials" $
        withTempDir \home -> do
            Right initial <- listMcpConnections home
            Right added <- createMcpConnection home initial.mcpAdminRevision
                "Local server" "http://127.0.0.1:8080/mcp"
            observed <- newIORef False
            let host = McpConnectionAuthorizationHost
                    { connectionLoadCredential = \_ -> expectationFailure "Unexpected credential read" >> pure (Right Nothing)
                    , connectionSaveCredential = \_ _ -> expectationFailure "Unexpected credential write" >> pure (Right ())
                    , connectionProbe = \_ _ -> writeIORef observed True >> pure (Right McpConnectionReady)
                    }
            Right authorized <- authorizeMcpConnectionWith host home added.mcpAdminRevision
                added.mcpAdminValue.connectionId
                (\_ -> expectationFailure "Unexpected browser authorization" >> pure (Right ()))
            authorized.mcpAdminValue `shouldBe` added.mcpAdminValue
                { connectionGeneration = authorized.mcpAdminValue.connectionGeneration }
            authorized.mcpAdminValue.connectionGeneration
                `shouldNotBe` added.mcpAdminValue.connectionGeneration
            authorized.mcpAdminRevision `shouldNotBe` added.mcpAdminRevision
            readIORef observed `shouldReturn` True

    it "rejects a verification completed after the connection was removed" $
        withTempDir \home -> do
            Right initial <- listMcpConnections home
            Right added <- createMcpConnection home initial.mcpAdminRevision
                "Local server" "http://127.0.0.1:8080/mcp"
            let identifier = added.mcpAdminValue.connectionId
                host = McpConnectionAuthorizationHost
                    { connectionLoadCredential = \_ -> pure (Right Nothing)
                    , connectionSaveCredential = \_ _ -> expectationFailure "Stale credential write" >> pure (Right ())
                    , connectionProbe = \_ _ -> do
                        Right current <- listMcpConnections home
                        Right _ <- removeMcpConnectionWith (const (pure (Right ()))) home current.mcpAdminRevision identifier
                        pure (Right McpConnectionReady)
                    }
            result <- authorizeMcpConnectionWith host home added.mcpAdminRevision identifier
                (\_ -> pure (Right ()))
            result `shouldSatisfy` \case Left _ -> True; _ -> False

    it "persists fresh generations across disable and re-enable and rejects stale verification" $
        withTempDir \home -> do
            Right initial <- listMcpConnections home
            Right added <- createMcpConnection home initial.mcpAdminRevision
                "Local server" "http://127.0.0.1:8080/mcp"
            let identifier = added.mcpAdminValue.connectionId
                host = McpConnectionAuthorizationHost
                    { connectionLoadCredential = \_ -> pure (Right Nothing)
                    , connectionSaveCredential = \_ _ -> expectationFailure "Stale credential write" >> pure (Right ())
                    , connectionProbe = \_ _ -> do
                        Right current <- listMcpConnections home
                        Right disabled <- setMcpConnectionEnabled home current.mcpAdminRevision identifier False
                        Right enabled <- setMcpConnectionEnabled home disabled.mcpAdminRevision identifier True
                        enabled.mcpAdminRevision `shouldNotBe` added.mcpAdminRevision
                        enabled.mcpAdminRevision `shouldNotBe` current.mcpAdminRevision
                        pure (Right McpConnectionReady)
                    }
            result <- authorizeMcpConnectionWith host home added.mcpAdminRevision identifier
                (\_ -> pure (Right ()))
            result `shouldSatisfy` \case Left (McpAdminConflict _) -> True; _ -> False

    it "does not permit the command editor to replace a remote connection" $
        withTempDir \home -> do
            Right initial <- listMcpConnections home
            Right added <- createMcpConnection home initial.mcpAdminRevision
                "Account" "https://example.test/mcp"
            editMcpAdminServer home added.mcpAdminRevision
                (connectionServerName added.mcpAdminValue.connectionId)
                McpAdminServerInput
                    { mcpAdminInputCommand = "replacement"
                    , mcpAdminInputArgs = []
                    , mcpAdminInputCwd = Nothing
                    , mcpAdminInputEnv = Map.empty
                    , mcpAdminInputStartupTimeoutSeconds = 30
                    , mcpAdminInputRequestTimeoutSeconds = 60
                    }
                `shouldReturn` Left (McpAdminInvalid
                    "remote HTTP MCP servers cannot be edited through this API")

    it "rejects insecure and credential-bearing endpoints" $
        withTempDir \home -> do
            Right initial <- listMcpConnections home
            mapM_ (\url -> do
                result <- createMcpConnection home initial.mcpAdminRevision "Account" url
                result `shouldSatisfy` \case Left _ -> True; Right _ -> False)
                [ "http://example.test/mcp"
                , "https://user:secret@example.test/mcp"
                , "https://example.test/mcp#fragment"
                , "file:///etc/passwd"
                , "https://example.test:0/mcp"
                , "https://example.test:65536/mcp"
                , "https://example.test:invalid/mcp"
                ]
            Right listed <- listMcpConnections home
            listed.mcpAdminValue `shouldBe` []

withTempDir :: (OsPath -> IO a) -> IO a
withTempDir action = do
    temporary <- getTemporaryDirectory >>= decodeUtf
    bracket
        (mkdtemp (temporary FilePath.</> "agent-mcp-connections-"))
        Directory.removePathForcibly
        (action . unsafeEncodeUtf)
