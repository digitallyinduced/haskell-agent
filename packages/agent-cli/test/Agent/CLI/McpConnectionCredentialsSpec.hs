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
    it "constructs unavailable storage explicitly and keeps independent runtimes isolated" do
        unavailable <- newCredentialRuntime Nothing
        first <- newMemoryRuntime
        let record = OAuthTokenFile "client" "https://authorization.example/token" "first" "refresh" Nothing
            firstProvider = mcpConnectionCredentialProvider first "same"
            unavailableProvider = mcpConnectionCredentialProvider unavailable "same"
        saveMcpConnectionRecord first "same" record emptyOAuthTokenFileExtra `shouldReturn` Right ()
        second <- newMemoryRuntime
        saveMcpConnectionRecord second "same" (record { tokenAccessToken = "second" })
            emptyOAuthTokenFileExtra `shouldReturn` Right ()
        firstProvider.mcpCredentialAccessToken `shouldReturn` Right (Just "first")
        unavailableProvider.mcpCredentialAccessToken
            `shouldReturn` Left "Protected MCP credential storage is unavailable"
        saveMcpConnectionRecord unavailable "same" record emptyOAuthTokenFileExtra
            `shouldReturn` Left "Protected MCP credential storage is unavailable"
        deleteMcpConnectionRecord unavailable "same"
            `shouldReturn` Left "Protected MCP credential storage is unavailable"
        deleteMcpConnectionRecord second "same" `shouldReturn` Right ()
        firstProvider.mcpCredentialAccessToken `shouldReturn` Right (Just "first")

    it "shares refresh serialization across providers but not across runtimes" do
        runtime <- newMemoryRuntime
        independent <- newMemoryRuntime
        started <- newEmptyMVar
        resume <- newEmptyMVar
        count <- newIORef (0 :: Int)
        let record = OAuthTokenFile "client" "https://authorization.example/token" "old" "refresh" (Just 0)
            refreshed = OAuthTokenSuccess (OAuthTokens "rotated" Nothing (Just 3600) Nothing)
            refresh _ = do
                atomicModifyIORef' count (\n -> (n + 1, ()))
                putMVar started ()
                takeMVar resume
                pure refreshed
            first = mcpConnectionCredentialProviderWithRefresh runtime "same" id refresh
            second = mcpConnectionCredentialProviderWithRefresh runtime "same" id refresh
            separate = mcpConnectionCredentialProviderWithRefresh independent "same" id (const (pure refreshed))
        saveMcpConnectionRecord runtime "same" record emptyOAuthTokenFileExtra `shouldReturn` Right ()
        saveMcpConnectionRecord independent "same" record emptyOAuthTokenFileExtra `shouldReturn` Right ()
        result <- timeout 5000000 $
            withAsync first.mcpCredentialAccessToken \worker -> do
                takeMVar started
                -- A hidden process-wide registry would deadlock here.
                separate.mcpCredentialAccessToken `shouldReturn` Right (Just "rotated")
                timeout 100000 second.mcpCredentialAccessToken `shouldReturn` Nothing
                readIORef count `shouldReturn` 1
                withAsync second.mcpCredentialAccessToken \other -> do
                    putMVar resume ()
                    a <- wait worker
                    b <- wait other
                    pure (a, b)
        result `shouldBe` Just (Right (Just "rotated"), Right (Just "rotated"))
        readIORef count `shouldReturn` 1

    it "serializes store access within a runtime without locking other runtimes" do
        started <- newEmptyMVar
        resume <- newEmptyMVar
        runtime <- newCredentialRuntime $ Just McpCredentialStore
            { credentialStoreLoad = \_ -> putMVar started () >> takeMVar resume >> pure (Right Nothing)
            , credentialStoreSave = \_ _ -> pure (Right ())
            , credentialStoreDelete = const (pure (Right ()))
            }
        independent <- newMemoryRuntime
        result <- timeout 5000000 $
            withAsync (loadMcpConnectionRecord runtime "same") \worker -> do
                takeMVar started
                loadMcpConnectionRecord independent "same" `shouldReturn` Right Nothing
                timeout 100000 (deleteMcpConnectionRecord runtime "same") `shouldReturn` Nothing
                putMVar resume ()
                wait worker `shouldReturn` Right Nothing
                deleteMcpConnectionRecord runtime "same"
        result `shouldBe` Just (Right ())

    it "serializes shared refresh locks and reloads rotated credentials" $
      withSystemTempDirectory "mcp-refresh-lock" \directory -> do
        runtime <- newMemoryRuntime
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
                (mcpConnectionCredentialProviderWithRefresh runtime "shared" id refresh)
        saveMcpConnectionRecord runtime "shared" record emptyOAuthTokenFileExtra `shouldReturn` Right ()
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
        runtime <- newCredentialRuntime $ Just McpCredentialStore
            { credentialStoreLoad = \identifier -> Right . Map.lookup identifier <$> readIORef records
            , credentialStoreSave = \identifier bytes ->
                modifyIORef' records (Map.insert identifier bytes) >> pure (Right ())
            , credentialStoreDelete = \identifier ->
                modifyIORef' records (Map.delete identifier) >> pure (Right ())
            }
        let first = OAuthTokenFile "client" "https://authorization.example/token" "first-token" "first-refresh" Nothing
            second = first { tokenAccessToken = "second-token", tokenRefreshToken = "second-refresh" }
            extra = emptyOAuthTokenFileExtra { extraResource = Just "https://server.example/mcp" }
            firstProvider = mcpConnectionCredentialProvider runtime "first"
            secondProvider = mcpConnectionCredentialProvider runtime "second"
        saveMcpConnectionRecord runtime "first" first extra `shouldReturn` Right ()
        saveMcpConnectionRecord runtime "second" second extra `shouldReturn` Right ()
        firstProvider.mcpCredentialAccessToken `shouldReturn` Right (Just "first-token")
        secondProvider.mcpCredentialAccessToken `shouldReturn` Right (Just "second-token")
        saveMcpConnectionRecord runtime "first" (first { tokenAccessToken = "replacement" }) extra `shouldReturn` Right ()
        firstProvider.mcpCredentialAccessToken `shouldReturn` Right (Just "replacement")
        secondProvider.mcpCredentialAccessToken `shouldReturn` Right (Just "second-token")
        deleteMcpConnectionRecord runtime "first" `shouldReturn` Right ()
        firstProvider.mcpCredentialAccessToken `shouldReturn` Right Nothing
        secondProvider.mcpCredentialAccessToken `shouldReturn` Right (Just "second-token")

    it "rejects a refresh after generation changes without blocking lifecycle writes" do
        runtime <- newMemoryRuntime
        generation <- newIORef (0 :: Int)
        started <- newEmptyMVar
        resume <- newEmptyMVar
        let record = OAuthTokenFile "client" "https://authorization.example/token" "old" "refresh" (Just 0)
            gate action = readIORef generation >>= \current ->
                if current == 0 then action else pure (Left "stale generation")
            refresh _ = putMVar started () >> takeMVar resume >>
                pure (OAuthTokenSuccess (OAuthTokens "stale-refreshed" Nothing (Just 3600) Nothing))
            provider = mcpConnectionCredentialProviderWithRefresh runtime "racing" gate refresh
        saveMcpConnectionRecord runtime "racing" record emptyOAuthTokenFileExtra `shouldReturn` Right ()
        withAsync provider.mcpCredentialAccessToken \worker -> do
            takeMVar started
            writeIORef generation 1
            saveMcpConnectionRecord runtime "racing" (record { tokenAccessToken = "replacement" })
                emptyOAuthTokenFileExtra `shouldReturn` Right ()
            putMVar resume ()
            wait worker `shouldReturn` Left "stale generation"
        loaded <- loadMcpConnectionRecord runtime "racing"
        fmap (fmap ((.tokenAccessToken) . fst)) loaded `shouldBe` Right (Just "replacement")

    it "does not resurrect credentials deleted while refresh is in flight" do
        runtime <- newMemoryRuntime
        started <- newEmptyMVar
        resume <- newEmptyMVar
        let record = OAuthTokenFile "client" "https://authorization.example/token" "old" "refresh" (Just 0)
            refresh _ = putMVar started () >> takeMVar resume >>
                pure (OAuthTokenSuccess (OAuthTokens "refreshed" Nothing (Just 3600) Nothing))
            provider = mcpConnectionCredentialProviderWithRefresh runtime "deleted" id refresh
        saveMcpConnectionRecord runtime "deleted" record emptyOAuthTokenFileExtra `shouldReturn` Right ()
        withAsync provider.mcpCredentialAccessToken \worker -> do
            takeMVar started
            deleteMcpConnectionRecord runtime "deleted" `shouldReturn` Right ()
            putMVar resume ()
            wait worker `shouldReturn` Left "MCP authorization changed during refresh"
        loadMcpConnectionRecord runtime "deleted" `shouldReturn` Right Nothing

    it "does not disclose malformed protected storage bytes in errors" do
        runtime <- newCredentialRuntime $ Just McpCredentialStore
            { credentialStoreLoad = const (pure (Right (Just "secret-invalid-record")))
            , credentialStoreSave = \_ _ -> pure (Right ())
            , credentialStoreDelete = const (pure (Right ()))
            }
        loadMcpConnectionRecord runtime "malformed"
            `shouldReturn` Left "Protected MCP credential record is invalid"

newMemoryRuntime :: IO CredentialRuntime
newMemoryRuntime = do
    records <- newIORef Map.empty
    newCredentialRuntime $ Just McpCredentialStore
        { credentialStoreLoad = \identifier -> Right . Map.lookup identifier <$> readIORef records
        , credentialStoreSave = \identifier bytes ->
            atomicModifyIORef' records (\current -> (Map.insert identifier bytes current, Right ()))
        , credentialStoreDelete = \identifier ->
            atomicModifyIORef' records (\current -> (Map.delete identifier current, Right ()))
        }
