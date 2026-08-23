-- | Machine-wide harness configuration under
-- @~/.haskell-agent/config.json@.
module Agent.CLI.Config
    ( HarnessConfig(..)
    , McpServerConfig(..)
    , defaultHarnessConfig
    , harnessConfigPath
    , loadHarnessConfig
    ) where

import Agent.FileRetry (retryOnFileBusy)
import Agent.OsPath (unsafeToFilePath)
import Control.Exception.Safe (displayException, tryIO)
import Control.Monad (unless, when)
import Data.Aeson
    ( FromJSON(..)
    , withObject
    , (.:)
    , (.:?)
    , (.!=)
    )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory.OsPath (doesFileExist)
import System.OsPath (OsPath, unsafeEncodeUtf, (</>))

harnessConfigSchemaVersion :: Int
harnessConfigSchemaVersion = 1

defaultMcpStartupTimeoutSeconds :: Int
defaultMcpStartupTimeoutSeconds = 30

defaultMcpRequestTimeoutSeconds :: Int
defaultMcpRequestTimeoutSeconds = 60

-- | One local stdio MCP server. Environment values are intentionally kept
-- opaque: callers must not include them in diagnostics.
data McpServerConfig = McpServerConfig
    { mcpEnabled :: !Bool
    , mcpCommand :: !Text
    , mcpArgs :: ![Text]
    , mcpCwd :: !(Maybe Text)
    , mcpEnv :: !(Map Text Text)
    , mcpStartupTimeoutSeconds :: !Int
    , mcpRequestTimeoutSeconds :: !Int
    }
    deriving (Eq)

instance Show McpServerConfig where
    show server =
        "McpServerConfig { mcpEnabled = "
            <> show server.mcpEnabled
            <> ", mcpCommand = "
            <> show server.mcpCommand
            <> ", mcpArgs = "
            <> show server.mcpArgs
            <> ", mcpCwd = "
            <> show server.mcpCwd
            <> ", mcpEnv = <redacted:"
            <> show (Map.size server.mcpEnv)
            <> " entries>, mcpStartupTimeoutSeconds = "
            <> show server.mcpStartupTimeoutSeconds
            <> ", mcpRequestTimeoutSeconds = "
            <> show server.mcpRequestTimeoutSeconds
            <> " }"

data HarnessConfig = HarnessConfig
    { configVersion :: !Int
    , configMcpServers :: !(Map Text McpServerConfig)
    }
    deriving (Eq, Show)

defaultHarnessConfig :: HarnessConfig
defaultHarnessConfig = HarnessConfig
    { configVersion = harnessConfigSchemaVersion
    , configMcpServers = Map.empty
    }

instance FromJSON McpServerConfig where
    parseJSON = withObject "McpServerConfig" \object ->
        McpServerConfig
            <$> object .:? "enabled" .!= True
            <*> object .: "command"
            <*> object .:? "args" .!= []
            <*> object .:? "cwd"
            <*> object .:? "env" .!= Map.empty
            <*> object .:? "startupTimeoutSeconds"
                .!= defaultMcpStartupTimeoutSeconds
            <*> object .:? "requestTimeoutSeconds"
                .!= defaultMcpRequestTimeoutSeconds

instance FromJSON HarnessConfig where
    parseJSON = withObject "HarnessConfig" \object ->
        HarnessConfig
            <$> object .:? "version" .!= harnessConfigSchemaVersion
            <*> object .:? "mcpServers" .!= Map.empty

-- | @~/.haskell-agent/config.json@ for a supplied home directory.
harnessConfigPath :: OsPath -> OsPath
harnessConfigPath home =
    home
        </> unsafeEncodeUtf ".haskell-agent"
        </> unsafeEncodeUtf "config.json"

-- | Load and validate the machine-wide configuration. A missing file is the
-- empty default. Decode, I/O, and semantic validation failures remain
-- distinguishable to the CLI through their error text.
loadHarnessConfig :: OsPath -> IO (Either Text HarnessConfig)
loadHarnessConfig home = do
    let path = harnessConfigPath home
    exists <- doesFileExist path
    if not exists
        then pure (Right defaultHarnessConfig)
        else do
            bytesResult <-
                tryIO (retryOnFileBusy (LBS.readFile (unsafeToFilePath path)))
            pure do
                bytes <- case bytesResult of
                    Left exception ->
                        Left
                            ( "Failed to read "
                                <> Text.pack (unsafeToFilePath path)
                                <> ": "
                                <> Text.pack (displayException exception)
                            )
                    Right bytes -> Right bytes
                config <- case Aeson.eitherDecode' bytes of
                    Left err ->
                        Left
                            ( "Invalid "
                                <> Text.pack (unsafeToFilePath path)
                                <> ": "
                                <> Text.pack err
                            )
                    Right parsed -> Right parsed
                validateHarnessConfig config

validateHarnessConfig :: HarnessConfig -> Either Text HarnessConfig
validateHarnessConfig config = do
    unless (config.configVersion == harnessConfigSchemaVersion) $
        Left
            ( "Unsupported harness config version "
                <> Text.pack (show config.configVersion)
                <> "; expected "
                <> Text.pack (show harnessConfigSchemaVersion)
            )
    _ <- Map.traverseWithKey validateServer config.configMcpServers
    pure config
  where
    validateServer label server = do
        when (Text.null (Text.strip label)) $
            Left "MCP server label must not be empty"
        when (Text.null (Text.strip server.mcpCommand)) $
            Left ("MCP server " <> quote label <> " has an empty command")
        when (server.mcpStartupTimeoutSeconds < 1) $
            Left
                ( "MCP server "
                    <> quote label
                    <> " startupTimeoutSeconds must be positive"
                )
        when (server.mcpRequestTimeoutSeconds < 1) $
            Left
                ( "MCP server "
                    <> quote label
                    <> " requestTimeoutSeconds must be positive"
                )
        pure server

    quote value = "'" <> value <> "'"
