{-# LANGUAGE OverloadedStrings #-}
module Agent.CLI.McpOAuthStore
    ( loadMcpOAuth, mcpOAuthStorePath, saveMcpOAuth
    , withMcpOAuthRefreshLock
    ) where

import Agent.CLI.PrivateFileLock (withPrivateFileLock)
import Agent.FileRetry (writeLazyFileAtomically)
import Agent.MCP.OAuth (OAuthTokenFile)
import Agent.OsPath (unsafeToFilePath)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception.Safe (tryAny)
import qualified Data.Aeson as Aeson
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
loadMcpOAuth server = do
    home <- getHomeDirectory
    let path = mcpOAuthStorePath home server
    exists <- doesFileExist path
    if not exists then pure (Right Nothing) else do
        result <- tryAny (LBS.readFile (unsafeToFilePath path))
        pure $ case result of
            Left exception -> Left (Text.pack (show exception))
            Right bytes -> either (Left . Text.pack) (Right . Just) (Aeson.eitherDecode bytes)

saveMcpOAuth :: Text -> OAuthTokenFile -> IO (Either Text ())
saveMcpOAuth server record = do
    home <- getHomeDirectory
    let path = mcpOAuthStorePath home server
    result <- tryAny do
        createDirectoryIfMissing True (takeDirectory path)
        setFileMode (unsafeToFilePath (takeDirectory path)) 0o700
        writeLazyFileAtomically path 0o600 (Aeson.encode record)
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
