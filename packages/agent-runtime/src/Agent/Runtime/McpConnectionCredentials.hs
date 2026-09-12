{-# LANGUAGE RankNTypes #-}
-- | Protected, immutable-connection-indexed OAuth records. Storage is supplied
-- by the native host; no URL-indexed or plaintext-file fallback is permitted.
module Agent.Runtime.McpConnectionCredentials
    ( McpCredentialStore(..)
    , installMcpCredentialStore
    , loadMcpConnectionRecord
    , saveMcpConnectionRecord
    , deleteMcpConnectionRecord
    , mcpConnectionCredentialProvider
    , mcpConnectionCredentialProviderWith
    , mcpConnectionCredentialProviderWithRefresh
    , withMcpConnectionRefreshLock
    ) where

import Agent.MCP (McpCredentialProvider(..))
import Agent.PrivateFileLock (withPrivateFileLock)
import Agent.MCP.OAuth
import Control.Applicative ((<|>))
import Control.Concurrent.MVar
import Control.Exception.Safe (tryAny)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Client.TLS (getGlobalManager)
import System.IO.Unsafe (unsafePerformIO)
import System.OsPath (OsPath)

-- | A nonsecret stable lock file serializes refresh across application
-- processes. Never acquire this lock while holding the catalog/store lock:
-- the provider acquires those only briefly before/after network I/O.
-- Lifecycle mutation deliberately does not wait for refresh; the generation
-- gate rejects an obsolete response instead.
withMcpConnectionRefreshLock :: OsPath -> McpCredentialProvider -> McpCredentialProvider
withMcpConnectionRefreshLock path provider = McpCredentialProvider
    { mcpCredentialAccessToken = locked provider.mcpCredentialAccessToken
    , mcpCredentialRefreshAccessToken = locked provider.mcpCredentialRefreshAccessToken
    }
  where
    locked action = tryAny (withPrivateFileLock path action) >>= \case
        Left _ -> pure (Left "MCP credential refresh coordination failed")
        Right result -> pure result

data McpCredentialStore = McpCredentialStore
    { credentialStoreLoad :: Text -> IO (Either Text (Maybe BS.ByteString))
    , credentialStoreSave :: Text -> BS.ByteString -> IO (Either Text ())
    , credentialStoreDelete :: Text -> IO (Either Text ())
    }

credentialStore :: IORef (Maybe McpCredentialStore)
credentialStore = unsafePerformIO (newIORef Nothing)
{-# NOINLINE credentialStore #-}

-- | Install once during native process initialization, before operations start.
installMcpCredentialStore :: McpCredentialStore -> IO ()
installMcpCredentialStore store = writeIORef credentialStore (Just store)

connectionLocks :: MVar (Map.Map Text (MVar ()))
connectionLocks = unsafePerformIO (newMVar Map.empty)
{-# NOINLINE connectionLocks #-}

refreshLocks :: MVar (Map.Map Text (MVar ()))
refreshLocks = unsafePerformIO (newMVar Map.empty)
{-# NOINLINE refreshLocks #-}

withIdentifierLock :: MVar (Map.Map Text (MVar ())) -> Text -> IO a -> IO a
withIdentifierLock registry identifier action = do
    lock <- modifyMVar registry \locks ->
        case Map.lookup identifier locks of
            Just existing -> pure (locks, existing)
            Nothing -> do
                created <- newMVar ()
                pure (Map.insert identifier created locks, created)
    withMVar lock (const action)

withConnectionStore
    :: Text -> (McpCredentialStore -> IO (Either Text a)) -> IO (Either Text a)
withConnectionStore identifier action =
    withIdentifierLock connectionLocks identifier $
        readIORef credentialStore >>= \case
            Nothing -> pure (Left "Protected MCP credential storage is unavailable")
            Just store -> tryAny (action store) >>= \case
                Left _ -> pure (Left "Protected MCP credential operation failed")
                Right result -> pure result

loadRecord :: McpCredentialStore -> Text
    -> IO (Either Text (Maybe (OAuthTokenFile, OAuthTokenFileExtra)))
loadRecord store identifier =
    store.credentialStoreLoad identifier >>= \case
        Left err -> pure (Left err)
        Right Nothing -> pure (Right Nothing)
        Right (Just bytes) -> pure $
            case decodeOAuthTokenRecord (LBS.fromStrict bytes) of
                Left _ -> Left "Protected MCP credential record is invalid"
                Right record -> Right (Just record)

loadMcpConnectionRecord :: Text
    -> IO (Either Text (Maybe (OAuthTokenFile, OAuthTokenFileExtra)))
loadMcpConnectionRecord identifier =
    withConnectionStore identifier \store -> loadRecord store identifier

saveMcpConnectionRecord :: Text -> OAuthTokenFile -> OAuthTokenFileExtra
    -> IO (Either Text ())
saveMcpConnectionRecord identifier record extra =
    withConnectionStore identifier \store ->
        store.credentialStoreSave identifier
            (LBS.toStrict (encodeOAuthTokenRecord record extra))

deleteMcpConnectionRecord :: Text -> IO (Either Text ())
deleteMcpConnectionRecord identifier =
    withConnectionStore identifier \store -> store.credentialStoreDelete identifier

mcpConnectionCredentialProvider :: Text -> McpCredentialProvider
mcpConnectionCredentialProvider identifier =
    mcpConnectionCredentialProviderWith identifier id

-- | The host gate validates the captured generation under the catalog lock.
-- It wraps only protected storage, never a network request. Lock ordering is
-- consistently catalog -> store; refresh serialization is independent.
mcpConnectionCredentialProviderWith
    :: Text
    -> (forall a. IO (Either Text a) -> IO (Either Text a))
    -> McpCredentialProvider
mcpConnectionCredentialProviderWith identifier gate =
    mcpConnectionCredentialProviderWithRefresh identifier gate \request -> do
        manager <- getGlobalManager
        refreshAccessTokenWith manager request

mcpConnectionCredentialProviderWithRefresh
    :: Text
    -> (forall a. IO (Either Text a) -> IO (Either Text a))
    -> (RefreshRequest -> IO OAuthTokenResponse)
    -> McpCredentialProvider
mcpConnectionCredentialProviderWithRefresh identifier gate refresh = McpCredentialProvider
    { mcpCredentialAccessToken = accessToken False
    , mcpCredentialRefreshAccessToken =
        accessToken True >>= pure . (>>= maybe (Left "MCP authorization is required") Right)
    }
  where
    accessToken forceRefresh = withIdentifierLock refreshLocks identifier $
        gate (loadMcpConnectionRecord identifier) >>= \case
            Left err -> pure (Left err)
            Right Nothing -> pure (Right Nothing)
            Right (Just (current, extra)) -> do
                now :: Int <- round <$> getPOSIXTime
                if not forceRefresh && maybe True (> now + 60) current.tokenExpiresAt
                    then pure (Right (Just current.tokenAccessToken))
                    else do
                        refresh RefreshRequest
                            { refreshEndpoint = current.tokenEndpoint
                            , refreshClientId = current.tokenClientId
                            , refreshClientSecret = extra.extraClientSecret
                            , refreshRefreshToken = current.tokenRefreshToken
                            , refreshResource = extra.extraResource
                            } >>= \case
                            OAuthTokenFailure _ -> pure (Left "MCP authorization refresh failed; reconnect this connection")
                            OAuthTokenSuccess tokens -> do
                                completed :: Int <- round <$> getPOSIXTime
                                let updated = current
                                        { tokenAccessToken = tokens.accessToken
                                        , tokenRefreshToken = fromMaybe current.tokenRefreshToken tokens.refreshToken
                                        , tokenExpiresAt = fmap (completed +) tokens.expiresIn
                                        }
                                    updatedExtra = extra { extraScope = tokens.scope <|> extra.extraScope }
                                gate (withConnectionStore identifier \store ->
                                    loadRecord store identifier >>= \case
                                        Right (Just (latest, latestExtra))
                                            | encodeOAuthTokenRecord latest latestExtra
                                                == encodeOAuthTokenRecord current extra ->
                                                store.credentialStoreSave identifier
                                                    (LBS.toStrict (encodeOAuthTokenRecord updated updatedExtra))
                                        _ -> pure (Left "MCP authorization changed during refresh"))
                                    >>= pure . fmap (const (Just updated.tokenAccessToken))
