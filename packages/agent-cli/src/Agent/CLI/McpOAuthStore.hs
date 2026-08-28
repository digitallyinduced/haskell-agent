{-# LANGUAGE OverloadedStrings #-}
module Agent.CLI.McpOAuthStore
    ( McpOAuthRecord(..), loadMcpOAuth, mcpOAuthStorePath
    , saveMcpOAuth, withMcpOAuthRefreshLock ) where

import Agent.CLI.PrivateFileLock (withPrivateFileLock)
import Agent.FileRetry (writeLazyFileAtomically)
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

data McpOAuthRecord = McpOAuthRecord
    { mcpOAuthServer :: !Text
    , mcpOAuthClientId :: !Text
    , mcpOAuthClientSecret :: !(Maybe Text)
    , mcpOAuthAccessToken :: !Text
    , mcpOAuthRefreshToken :: !(Maybe Text)
    , mcpOAuthExpiresAt :: !(Maybe Integer)
    } deriving (Eq)

instance Show McpOAuthRecord where
    show record = "McpOAuthRecord { mcpOAuthServer = " <> show record.mcpOAuthServer
        <> ", mcpOAuthClientId = <redacted>, mcpOAuthClientSecret = <redacted>"
        <> ", mcpOAuthAccessToken = <redacted>, mcpOAuthRefreshToken = "
        <> show (maybe False (const True) record.mcpOAuthRefreshToken)
        <> ", mcpOAuthExpiresAt = " <> show record.mcpOAuthExpiresAt <> " }"

instance Aeson.ToJSON McpOAuthRecord where
    toJSON record = Aeson.object
        [ "server" Aeson..= record.mcpOAuthServer, "client_id" Aeson..= record.mcpOAuthClientId
        , "client_secret" Aeson..= record.mcpOAuthClientSecret, "access_token" Aeson..= record.mcpOAuthAccessToken
        , "refresh_token" Aeson..= record.mcpOAuthRefreshToken, "expires_at" Aeson..= record.mcpOAuthExpiresAt ]

instance Aeson.FromJSON McpOAuthRecord where
    parseJSON = Aeson.withObject "McpOAuthRecord" $ \object ->
        McpOAuthRecord <$> object Aeson..: "server" <*> object Aeson..: "client_id"
            <*> object Aeson..:? "client_secret" <*> object Aeson..: "access_token"
            <*> object Aeson..:? "refresh_token" <*> object Aeson..:? "expires_at"

mcpOAuthStorePath :: OsPath -> Text -> OsPath
mcpOAuthStorePath home server = home </> unsafeEncodeUtf ".haskell-agent" </> unsafeEncodeUtf "credentials"
    </> unsafeEncodeUtf "mcp" </> unsafeEncodeUtf (hexName server <> ".json")
  where hexName = Text.unpack . Text.concatMap (Text.pack . (`showHex` "") . ord)

loadMcpOAuth :: Text -> IO (Either Text (Maybe McpOAuthRecord))
loadMcpOAuth server = do
    home <- getHomeDirectory
    let path = mcpOAuthStorePath home server
    exists <- doesFileExist path
    if not exists then pure (Right Nothing) else do
        result <- tryAny (LBS.readFile (unsafeToFilePath path))
        pure $ case result of
            Left exception -> Left (Text.pack (show exception))
            Right bytes -> either (Left . Text.pack) (Right . Just) (Aeson.eitherDecode bytes)

saveMcpOAuth :: McpOAuthRecord -> IO (Either Text ())
saveMcpOAuth record = do
    home <- getHomeDirectory
    let path = mcpOAuthStorePath home record.mcpOAuthServer
    result <- tryAny $ do
        createDirectoryIfMissing True (takeDirectory path)
        setFileMode (unsafeToFilePath (takeDirectory path)) 0o700
        writeLazyFileAtomically path 0o600 (Aeson.encode record)
    pure $ either (Left . Text.pack . show) Right result

withMcpOAuthRefreshLock :: Text -> IO a -> IO a
withMcpOAuthRefreshLock server action = withMVar refreshThreadLock $ const $ do
    home <- getHomeDirectory
    withPrivateFileLock (takeDirectory (mcpOAuthStorePath home server) </> unsafeEncodeUtf "refresh.lock") action

refreshThreadLock :: MVar ()
refreshThreadLock = unsafePerformIO (newMVar ())
{-# NOINLINE refreshThreadLock #-}
