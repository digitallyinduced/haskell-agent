module Agent.CLI.McpConnectionCredentialsSpec (spec) where

import Agent.CLI.McpConnectionCredentials
import Agent.MCP (McpCredentialProvider(..))
import Agent.MCP.OAuth
import Control.Concurrent.Async (withAsync, wait)
import Control.Concurrent.MVar
import Data.IORef
import qualified Data.Map.Strict as Map
import Test.Hspec
import System.OsPath (encodeUtf)
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import System.Timeout (timeout)

spec :: Spec
spec = describe "connection credential storage" do
    it "serializes shared refresh locks and reloads rotated credentials" $
      withSystemTempDirectory "mcp-refresh-lock" \directory -> do
        installMemoryStore
        count <- newIORef (0 :: Int)
        started <- newEmptyMVar
        resume <- newEmptyMVar
        lockPath <- encodeUtf (directory </> "connection.lock")
        let record = OAuthTokenFile "client" "https://authorization.example/token" "old" "refresh" (Just 0)
            refresh _ = do
                atomicModifyIORef' count (\n -> (n + 1, ()))
                putMVar started ()
                takeMVar resume
                pure (OAuthTokenSuccess (OAuthTokens "rotated" (Just "rotated-refresh") (Just 3600) Nothing))
            provider = withMcpConnectionRefreshLock lockPath
                (mcpConnectionCredentialProviderWithRefresh "shared" id refresh)
        saveMcpConnectionRecord "shared" record emptyOAuthTokenFileExtra `shouldReturn` Right ()
        result <- timeout 5000000 $
            withAsync provider.mcpCredentialAccessToken \first -> do
                takeMVar started
                withAsync provider.mcpCredentialAccessToken \second -> do
                    putMVar resume ()
                    a <- wait first
                    b <- wait second
                    pure (a, b)
        result `shouldBe` Just (Right (Just "rotated"), Right (Just "rotated"))
        readIORef count `shouldReturn` 1

    it "keeps two accounts at one endpoint independent through replacement and deletion" do
        records <- newIORef Map.empty
        installMcpCredentialStore McpCredentialStore
            { credentialStoreLoad = \identifier -> Right . Map.lookup identifier <$> readIORef records
            , credentialStoreSave = \identifier bytes ->
                modifyIORef' records (Map.insert identifier bytes) >> pure (Right ())
            , credentialStoreDelete = \identifier ->
                modifyIORef' records (Map.delete identifier) >> pure (Right ())
            }
        let first = OAuthTokenFile "client" "https://authorization.example/token" "first-token" "first-refresh" Nothing
            second = first { tokenAccessToken = "second-token", tokenRefreshToken = "second-refresh" }
            extra = emptyOAuthTokenFileExtra { extraResource = Just "https://server.example/mcp" }
            firstProvider = mcpConnectionCredentialProvider "first"
            secondProvider = mcpConnectionCredentialProvider "second"
        saveMcpConnectionRecord "first" first extra `shouldReturn` Right ()
        saveMcpConnectionRecord "second" second extra `shouldReturn` Right ()
        firstProvider.mcpCredentialAccessToken `shouldReturn` Right (Just "first-token")
        secondProvider.mcpCredentialAccessToken `shouldReturn` Right (Just "second-token")
        saveMcpConnectionRecord "first" (first { tokenAccessToken = "replacement" }) extra `shouldReturn` Right ()
        firstProvider.mcpCredentialAccessToken `shouldReturn` Right (Just "replacement")
        secondProvider.mcpCredentialAccessToken `shouldReturn` Right (Just "second-token")
        deleteMcpConnectionRecord "first" `shouldReturn` Right ()
        firstProvider.mcpCredentialAccessToken `shouldReturn` Right Nothing
        secondProvider.mcpCredentialAccessToken `shouldReturn` Right (Just "second-token")

    it "rejects a refresh after generation changes without blocking lifecycle writes" do
        installMemoryStore
        generation <- newIORef (0 :: Int)
        started <- newEmptyMVar
        resume <- newEmptyMVar
        let record = OAuthTokenFile "client" "https://authorization.example/token" "old" "refresh" (Just 0)
            gate action = readIORef generation >>= \current ->
                if current == 0 then action else pure (Left "stale generation")
            refresh _ = putMVar started () >> takeMVar resume >>
                pure (OAuthTokenSuccess (OAuthTokens "stale-refreshed" Nothing (Just 3600) Nothing))
            provider = mcpConnectionCredentialProviderWithRefresh "racing" gate refresh
        saveMcpConnectionRecord "racing" record emptyOAuthTokenFileExtra `shouldReturn` Right ()
        withAsync provider.mcpCredentialAccessToken \worker -> do
            takeMVar started
            writeIORef generation 1
            saveMcpConnectionRecord "racing" (record { tokenAccessToken = "replacement" })
                emptyOAuthTokenFileExtra `shouldReturn` Right ()
            putMVar resume ()
            wait worker `shouldReturn` Left "stale generation"
        loaded <- loadMcpConnectionRecord "racing"
        fmap (fmap ((.tokenAccessToken) . fst)) loaded `shouldBe` Right (Just "replacement")

    it "does not resurrect credentials deleted while refresh is in flight" do
        installMemoryStore
        started <- newEmptyMVar
        resume <- newEmptyMVar
        let record = OAuthTokenFile "client" "https://authorization.example/token" "old" "refresh" (Just 0)
            refresh _ = putMVar started () >> takeMVar resume >>
                pure (OAuthTokenSuccess (OAuthTokens "refreshed" Nothing (Just 3600) Nothing))
            provider = mcpConnectionCredentialProviderWithRefresh "deleted" id refresh
        saveMcpConnectionRecord "deleted" record emptyOAuthTokenFileExtra `shouldReturn` Right ()
        withAsync provider.mcpCredentialAccessToken \worker -> do
            takeMVar started
            deleteMcpConnectionRecord "deleted" `shouldReturn` Right ()
            putMVar resume ()
            wait worker `shouldReturn` Left "MCP authorization changed during refresh"
        loadMcpConnectionRecord "deleted" `shouldReturn` Right Nothing

    it "does not disclose malformed protected storage bytes in errors" do
        installMcpCredentialStore McpCredentialStore
            { credentialStoreLoad = const (pure (Right (Just "secret-invalid-record")))
            , credentialStoreSave = \_ _ -> pure (Right ())
            , credentialStoreDelete = const (pure (Right ()))
            }
        loadMcpConnectionRecord "malformed"
            `shouldReturn` Left "Protected MCP credential record is invalid"

installMemoryStore :: IO ()
installMemoryStore = do
    records <- newIORef Map.empty
    installMcpCredentialStore McpCredentialStore
        { credentialStoreLoad = \identifier -> Right . Map.lookup identifier <$> readIORef records
        , credentialStoreSave = \identifier bytes ->
            atomicModifyIORef' records (\current -> (Map.insert identifier bytes current, Right ()))
        , credentialStoreDelete = \identifier ->
            atomicModifyIORef' records (\current -> (Map.delete identifier current, Right ()))
        }
