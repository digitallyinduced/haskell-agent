{-# LANGUAGE OverloadedStrings #-}
module Agent.CLI.McpOAuthStore
    ( loadMcpOAuth, loadMcpOAuthRecord, mcpOAuthStorePath
    , saveMcpOAuth, saveMcpOAuthRecord
    , withMcpOAuthRefreshLock
    ) where

import Agent.CLI.PrivateFileLock (withPrivateFileLock)
import Agent.FileRetry (writeLazyFileAtomically)
import Agent.MCP.OAuth
    ( OAuthTokenFile, OAuthTokenFileExtra, decodeOAuthTokenRecord
    , emptyOAuthTokenFileExtra, encodeOAuthTokenRecord
    )
import Agent.OsPath (unsafeToFilePath)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception.Safe (tryAny)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text as Text
import Numeric (showHex)
import System.Directory.OsPath (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.OsPath (OsPath, takeDirectory, unsafeEncodeUtf, (</>))
import System.Posix.Files (setFileMode)

mcpOAuthStorePath :: OsPath -> Text -> OsPath
mcpOAuthStorePath home server = home </> unsafeEncodeUtf ".haskell-agent"
    </> unsafeEncodeUtf "credentials" </> unsafeEncodeUtf "mcp"
    </> unsafeEncodeUtf (hexName server <> ".json")
  where
    hexName = Text.unpack . Text.concatMap (Text.pack . (`showHex` "") . ord)

loadMcpOAuth :: Text -> IO (Either Text (Maybe OAuthTokenFile))
loadMcpOAuth server = fmap (fmap fst) <$> loadMcpOAuthRecord server

-- | Load the token record together with the issuer, scope, resource, and
-- client binding fields persisted alongside it.
loadMcpOAuthRecord :: Text -> IO (Either Text (Maybe (OAuthTokenFile, OAuthTokenFileExtra)))
loadMcpOAuthRecord server = do
    home <- getHomeDirectory
    let path = mcpOAuthStorePath home server
    exists <- doesFileExist path
    if not exists then pure (Right Nothing) else do
        result <- tryAny (LBS.readFile (unsafeToFilePath path))
        pure $ case result of
            Left exception -> Left (Text.pack (show exception))
            Right bytes -> Just <$> decodeOAuthTokenRecord bytes

saveMcpOAuth :: Text -> OAuthTokenFile -> IO (Either Text ())
saveMcpOAuth server record = saveMcpOAuthRecord server record emptyOAuthTokenFileExtra

-- | Atomically persist a private (mode @0600@) token record.
saveMcpOAuthRecord :: Text -> OAuthTokenFile -> OAuthTokenFileExtra -> IO (Either Text ())
saveMcpOAuthRecord server record extra = do
    home <- getHomeDirectory
    let path = mcpOAuthStorePath home server
    result <- tryAny do
        createDirectoryIfMissing True (takeDirectory path)
        setFileMode (unsafeToFilePath (takeDirectory path)) 0o700
        writeLazyFileAtomically path 0o600 (encodeOAuthTokenRecord record extra)
    pure $ either (Left . Text.pack . show) Right result

withMcpOAuthRefreshLock :: Text -> IO a -> IO a
withMcpOAuthRefreshLock server action = withMVar refreshThreadLock $ const do
    home <- getHomeDirectory
    withPrivateFileLock
        (takeDirectory (mcpOAuthStorePath home server) </> unsafeEncodeUtf "refresh.lock")
        action

refreshThreadLock :: MVar ()
refreshThreadLock = unsafePerformIO (newMVar ())
{-# NOINLINE refreshThreadLock #-}
