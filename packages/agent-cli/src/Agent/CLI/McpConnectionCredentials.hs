{-# LANGUAGE RankNTypes #-}
-- | Protected, immutable-connection-indexed OAuth records. Storage is supplied
-- by the native host; no URL-indexed or plaintext-file fallback is permitted.
module Agent.CLI.McpConnectionCredentials
    ( McpCredentialStore(..)
    , CredentialRuntime
    , newCredentialRuntime
    , loadMcpConnectionRecord
    , saveMcpConnectionRecord
    , deleteMcpConnectionRecord
    , mcpConnectionCredentialProvider
    , mcpConnectionCredentialProviderWith
    , mcpConnectionCredentialProviderWithRefresh
    , withMcpConnectionRefreshLock
    ) where

import Agent.MCP (McpCredentialProvider(..))
import Agent.CLI.PrivateFileLock (withPrivateFileLock)
import Agent.MCP.OAuth
import Control.Applicative ((<|>))
import Control.Concurrent.MVar
import Control.Exception.Safe (tryAny)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Client.TLS (getGlobalManager)
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

-- | Application-owned credentials and coordination. Share one runtime across
-- all sessions using the same store. The store is fixed at construction, so a
-- consumer can never observe partially installed platform credentials.
data CredentialRuntime = CredentialRuntime
    { credentialStore :: !(Maybe McpCredentialStore)
    , connectionLocks :: !(MVar (Map.Map Text (MVar ())))
    , refreshLocks :: !(MVar (Map.Map Text (MVar ())))
    }

-- | 'Nothing' explicitly selects unavailable protected storage (no fallback).
newCredentialRuntime :: Maybe McpCredentialStore -> IO CredentialRuntime
newCredentialRuntime store =
    CredentialRuntime store <$> newMVar Map.empty <*> newMVar Map.empty

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
    :: CredentialRuntime -> Text -> (McpCredentialStore -> IO (Either Text a)) -> IO (Either Text a)
withConnectionStore runtime identifier action =
    withIdentifierLock runtime.connectionLocks identifier $
        case runtime.credentialStore of
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

loadMcpConnectionRecord :: CredentialRuntime -> Text
    -> IO (Either Text (Maybe (OAuthTokenFile, OAuthTokenFileExtra)))
loadMcpConnectionRecord runtime identifier =
    withConnectionStore runtime identifier \store -> loadRecord store identifier

saveMcpConnectionRecord :: CredentialRuntime -> Text -> OAuthTokenFile -> OAuthTokenFileExtra
    -> IO (Either Text ())
saveMcpConnectionRecord runtime identifier record extra =
    withConnectionStore runtime identifier \store ->
        store.credentialStoreSave identifier
            (LBS.toStrict (encodeOAuthTokenRecord record extra))

deleteMcpConnectionRecord :: CredentialRuntime -> Text -> IO (Either Text ())
deleteMcpConnectionRecord runtime identifier =
    withConnectionStore runtime identifier \store -> store.credentialStoreDelete identifier

mcpConnectionCredentialProvider :: CredentialRuntime -> Text -> McpCredentialProvider
mcpConnectionCredentialProvider runtime identifier =
    mcpConnectionCredentialProviderWith runtime identifier id

-- | The host gate validates the captured generation under the catalog lock.
-- It wraps only protected storage, never a network request. Lock ordering is
-- consistently catalog -> store; refresh serialization is independent.
mcpConnectionCredentialProviderWith
    :: CredentialRuntime -> Text
    -> (forall a. IO (Either Text a) -> IO (Either Text a))
    -> McpCredentialProvider
mcpConnectionCredentialProviderWith runtime identifier gate =
    mcpConnectionCredentialProviderWithRefresh runtime identifier gate \request -> do
        manager <- getGlobalManager
        refreshAccessTokenWith manager request

mcpConnectionCredentialProviderWithRefresh
    :: CredentialRuntime -> Text
    -> (forall a. IO (Either Text a) -> IO (Either Text a))
    -> (RefreshRequest -> IO OAuthTokenResponse)
    -> McpCredentialProvider
mcpConnectionCredentialProviderWithRefresh runtime identifier gate refresh = McpCredentialProvider
    { mcpCredentialAccessToken = accessToken False
    , mcpCredentialRefreshAccessToken =
        accessToken True >>= pure . (>>= maybe (Left "MCP authorization is required") Right)
    }
  where
    accessToken forceRefresh = withIdentifierLock runtime.refreshLocks identifier $
        gate (loadMcpConnectionRecord runtime identifier) >>= \case
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
                                gate (withConnectionStore runtime identifier \store ->
                                    loadRecord store identifier >>= \case
                                        Right (Just (latest, latestExtra))
                                            | encodeOAuthTokenRecord latest latestExtra
                                                == encodeOAuthTokenRecord current extra ->
                                                store.credentialStoreSave identifier
                                                    (LBS.toStrict (encodeOAuthTokenRecord updated updatedExtra))
                                        _ -> pure (Left "MCP authorization changed during refresh"))
                                    >>= pure . fmap (const (Just updated.tokenAccessToken))
