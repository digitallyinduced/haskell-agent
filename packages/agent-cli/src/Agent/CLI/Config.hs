-- | Machine-wide harness configuration under
-- @~/.haskell-agent/config.json@.
module Agent.CLI.Config
    ( HarnessConfig(..)
    , WebFetchConfig(..)
    , LspConfig(..)
    , LspServerConfig(..)
    , McpInitStrategy(..)
    , McpServerConfig(..)
    , defaultHarnessConfig
    , harnessConfigPath
    , loadHarnessConfig
    , loadHarnessConfigSnapshot
    , modifyHarnessConfig
    , withHarnessConfigSnapshot
    , useProgressiveMcp
    ) where

import Agent.FileRetry (retryOnFileBusy, writeLazyFileAtomically)
import Agent.OsPath (unsafeToFilePath)
import Agent.CLI.PrivateFileLock (withPrivateFileLock)
import Control.Exception.Safe (displayException, tryIO)
import Control.Monad (unless, when)
import Data.Aeson
    ( FromJSON(..)
    , withText
    , withObject
    , (.:)
    , (.:?)
    , (.!=)
    )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Bits ((.|.), rotateL, shiftL, xor)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory.OsPath (createDirectoryIfMissing, doesFileExist)
import System.OsPath (OsPath, unsafeEncodeUtf, (</>))
import System.Posix.Files (setFileMode)
import Data.Word (Word64)
import System.IO (IOMode(ReadMode), withBinaryFile)

harnessConfigSchemaVersion :: Int
harnessConfigSchemaVersion = 1

defaultMcpStartupTimeoutSeconds :: Int
defaultMcpStartupTimeoutSeconds = 30

defaultMcpRequestTimeoutSeconds :: Int
defaultMcpRequestTimeoutSeconds = 60

defaultWebFetchTimeoutSeconds :: Int
defaultWebFetchTimeoutSeconds = 60

defaultWebFetchMaxContentBytes :: Int
defaultWebFetchMaxContentBytes = 10 * 1024 * 1024

defaultWebFetchMaxInlineBytes :: Int
defaultWebFetchMaxInlineBytes = 100000

defaultLspStartupTimeoutMilliseconds :: Int
defaultLspStartupTimeoutMilliseconds = 15000

defaultLspShutdownTimeoutMilliseconds :: Int
defaultLspShutdownTimeoutMilliseconds = 5000

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

data McpInitStrategy
    = McpInitAuto
    | McpInitProgressive
    | McpInitBlocking
    deriving (Eq, Show)

-- | Disabled-by-default client-side URL fetching. An empty domain list denies
-- all requests rather than becoming an unrestricted policy.
data WebFetchConfig = WebFetchConfig
    { webFetchEnabled :: !Bool
    , webFetchAllowedDomains :: ![Text]
    , webFetchTimeoutSeconds :: !Int
    , webFetchMaxContentBytes :: !Int
    , webFetchMaxInlineBytes :: !Int
    }
    deriving (Eq, Show)

-- | One configured local stdio language server. Environment values are
-- intentionally redacted from 'Show' and must not be included in diagnostics.
data LspServerConfig = LspServerConfig
    { lspCommand :: !Text
    , lspArgs :: ![Text]
    , lspEnv :: !(Map Text Text)
    , lspExtensionToLanguage :: !(Map Text Text)
    , lspInitializationOptions :: !(Maybe Aeson.Value)
    , lspSettings :: !(Maybe Aeson.Value)
    , lspWorkspaceFolder :: !(Maybe Text)
    , lspStartupTimeoutMilliseconds :: !Int
    , lspShutdownTimeoutMilliseconds :: !Int
    }
    deriving (Eq)

instance Show LspServerConfig where
    show server =
        "LspServerConfig { lspCommand = "
            <> show server.lspCommand
            <> ", lspArgs = "
            <> show server.lspArgs
            <> ", lspEnv = <redacted:"
            <> show (Map.size server.lspEnv)
            <> " entries>, lspExtensionToLanguage = "
            <> show server.lspExtensionToLanguage
            <> ", lspInitializationOptions = "
            <> show server.lspInitializationOptions
            <> ", lspSettings = "
            <> show server.lspSettings
            <> ", lspWorkspaceFolder = "
            <> show server.lspWorkspaceFolder
            <> ", lspStartupTimeoutMilliseconds = "
            <> show server.lspStartupTimeoutMilliseconds
            <> ", lspShutdownTimeoutMilliseconds = "
            <> show server.lspShutdownTimeoutMilliseconds
            <> " }"

data LspConfig = LspConfig
    { lspEnabled :: !Bool
    , lspServers :: !(Map Text LspServerConfig)
    }
    deriving (Eq, Show)

-- | Resolve the configured MCP startup policy for the current invocation.
-- Interactive sessions favor prompt availability, while one-shot commands
-- preserve deterministic startup unless explicitly overridden.
useProgressiveMcp :: McpInitStrategy -> Bool -> Bool
useProgressiveMcp strategy oneShot = case strategy of
    McpInitAuto -> not oneShot
    McpInitProgressive -> True
    McpInitBlocking -> False

instance FromJSON McpInitStrategy where
    parseJSON = withText "McpInitStrategy" \case
        "auto" -> pure McpInitAuto
        "progressive" -> pure McpInitProgressive
        "blocking" -> pure McpInitBlocking
        value ->
            fail
                ("unknown MCP initialization strategy: "
                    <> Text.unpack value)

instance Aeson.ToJSON McpInitStrategy where
    toJSON = Aeson.String . \case
        McpInitAuto -> "auto"
        McpInitProgressive -> "progressive"
        McpInitBlocking -> "blocking"

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

instance Aeson.ToJSON McpServerConfig where
    toJSON server =
        Aeson.object
            [ "enabled" Aeson..= server.mcpEnabled
            , "command" Aeson..= server.mcpCommand
            , "args" Aeson..= server.mcpArgs
            , "cwd" Aeson..= server.mcpCwd
            , "env" Aeson..= server.mcpEnv
            , "startupTimeoutSeconds"
                Aeson..= server.mcpStartupTimeoutSeconds
            , "requestTimeoutSeconds"
                Aeson..= server.mcpRequestTimeoutSeconds
            ]

instance Aeson.ToJSON WebFetchConfig where
    toJSON config =
        Aeson.object
            [ "enabled" Aeson..= config.webFetchEnabled
            , "allowedDomains" Aeson..= config.webFetchAllowedDomains
            , "timeoutSeconds" Aeson..= config.webFetchTimeoutSeconds
            , "maxContentBytes" Aeson..= config.webFetchMaxContentBytes
            , "maxInlineBytes" Aeson..= config.webFetchMaxInlineBytes
            ]

instance Aeson.ToJSON LspServerConfig where
    toJSON server =
        Aeson.object
            [ "command" Aeson..= server.lspCommand
            , "args" Aeson..= server.lspArgs
            , "env" Aeson..= server.lspEnv
            , "extensionToLanguage"
                Aeson..= server.lspExtensionToLanguage
            , "initializationOptions"
                Aeson..= server.lspInitializationOptions
            , "settings" Aeson..= server.lspSettings
            , "workspaceFolder" Aeson..= server.lspWorkspaceFolder
            , "startupTimeoutMilliseconds"
                Aeson..= server.lspStartupTimeoutMilliseconds
            , "shutdownTimeoutMilliseconds"
                Aeson..= server.lspShutdownTimeoutMilliseconds
            ]

instance Aeson.ToJSON LspConfig where
    toJSON config =
        Aeson.object
            [ "enabled" Aeson..= config.lspEnabled
            , "servers" Aeson..= config.lspServers
            ]

data HarnessConfig = HarnessConfig
    { configVersion :: !Int
    , configMcpInitStrategy :: !McpInitStrategy
    , configMcpServers :: !(Map Text McpServerConfig)
    , configWebFetch :: !WebFetchConfig
    , configLsp :: !LspConfig
    , configMaxConcurrentAgents :: !(Maybe Int)
    }
    deriving (Eq, Show)

instance Aeson.ToJSON HarnessConfig where
    toJSON config =
        Aeson.object
            [ "version" Aeson..= config.configVersion
            , "mcpInitStrategy" Aeson..= config.configMcpInitStrategy
            , "mcpServers" Aeson..= config.configMcpServers
            , "webFetch" Aeson..= config.configWebFetch
            , "lsp" Aeson..= config.configLsp
            , "maxConcurrentAgents" Aeson..= config.configMaxConcurrentAgents
            ]

defaultHarnessConfig :: HarnessConfig
defaultHarnessConfig = HarnessConfig
    { configVersion = harnessConfigSchemaVersion
    , configMcpInitStrategy = McpInitAuto
    , configMcpServers = Map.empty
    , configWebFetch = WebFetchConfig
        { webFetchEnabled = False
        , webFetchAllowedDomains = []
        , webFetchTimeoutSeconds = defaultWebFetchTimeoutSeconds
        , webFetchMaxContentBytes = defaultWebFetchMaxContentBytes
        , webFetchMaxInlineBytes = defaultWebFetchMaxInlineBytes
        }
    , configLsp = LspConfig
        { lspEnabled = False
        , lspServers = Map.empty
        }
    , configMaxConcurrentAgents = Nothing
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

instance FromJSON WebFetchConfig where
    parseJSON = withObject "WebFetchConfig" \object ->
        WebFetchConfig
            <$> object .:? "enabled" .!= False
            <*> object .:? "allowedDomains" .!= []
            <*> object .:? "timeoutSeconds"
                .!= defaultWebFetchTimeoutSeconds
            <*> object .:? "maxContentBytes"
                .!= defaultWebFetchMaxContentBytes
            <*> object .:? "maxInlineBytes"
                .!= defaultWebFetchMaxInlineBytes

instance FromJSON LspServerConfig where
    parseJSON = withObject "LspServerConfig" \object -> do
        transport <- object .:? "transport" .!= ("stdio" :: Text)
        unless (Text.toLower (Text.strip transport) == "stdio") $
            fail
                "LSP transport is unsupported; this host currently supports stdio only"
        restartOnCrash <-
            object .:? "restartOnCrash" .!= False
        when restartOnCrash $
            fail
                "LSP restartOnCrash=true is unsupported by this host"
        maxRestarts <-
            object .:? "maxRestarts" .!= Aeson.Null
        unless (maxRestarts == Aeson.Null) $
            fail
                "LSP maxRestarts is unsupported by this host"
        LspServerConfig
            <$> object .: "command"
            <*> object .:? "args" .!= []
            <*> object .:? "env" .!= Map.empty
            <*> object .:? "extensionToLanguage" .!= Map.empty
            <*> object .:? "initializationOptions"
            <*> object .:? "settings"
            <*> object .:? "workspaceFolder"
            <*> object .:? "startupTimeoutMilliseconds"
                .!= defaultLspStartupTimeoutMilliseconds
            <*> object .:? "shutdownTimeoutMilliseconds"
                .!= defaultLspShutdownTimeoutMilliseconds

instance FromJSON LspConfig where
    parseJSON = withObject "LspConfig" \object ->
        LspConfig
            <$> object .:? "enabled" .!= False
            <*> object .:? "servers" .!= Map.empty

instance FromJSON HarnessConfig where
    parseJSON = withObject "HarnessConfig" \object ->
        HarnessConfig
            <$> object .:? "version" .!= harnessConfigSchemaVersion
            <*> object .:? "mcpInitStrategy" .!= McpInitAuto
            <*> object .:? "mcpServers" .!= Map.empty
            <*> object .:? "webFetch" .!= defaultHarnessConfig.configWebFetch
            <*> object .:? "lsp" .!= defaultHarnessConfig.configLsp
            <*> object .:? "maxConcurrentAgents"

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
loadHarnessConfig home =
    fmap (fmap snd) (loadHarnessConfigSnapshot home)

-- | Read one config and its keyed opaque revision under the same lock used by
-- every writer. The owner-only key prevents the public token from becoming a
-- dictionary oracle for write-only configuration secrets.
loadHarnessConfigSnapshot
    :: OsPath -> IO (Either Text (Word64, HarnessConfig))
loadHarnessConfigSnapshot home =
    withPrivateFileLock (harnessConfigLockPath home) $
        loadHarnessConfigUnlocked home

-- | Run an effect against one locked config snapshot. This is used when an
-- external side effect must linearize with revision validation.
withHarnessConfigSnapshot
    :: OsPath
    -> (Word64 -> HarnessConfig -> IO a)
    -> IO (Either Text a)
withHarnessConfigSnapshot home action =
    withPrivateFileLock (harnessConfigLockPath home) do
        loadHarnessConfigUnlocked home >>= \case
            Left err -> pure (Left err)
            Right (revision, config) ->
                Right <$> action revision config

loadHarnessConfigUnlocked
    :: OsPath -> IO (Either Text (Word64, HarnessConfig))
loadHarnessConfigUnlocked home = do
    let path = harnessConfigPath home
    exists <- doesFileExist path
    bytesResult <-
        if exists
            then tryIO
                (retryOnFileBusy (LBS.readFile (unsafeToFilePath path)))
            else pure (Right (Aeson.encode defaultHarnessConfig))
    case bytesResult of
        Left exception ->
            pure . Left $
                "Failed to read "
                    <> Text.pack (unsafeToFilePath path)
                    <> ": "
                    <> Text.pack (displayException exception)
        Right bytes ->
            case Aeson.eitherDecode' bytes of
                Left err ->
                    pure . Left $
                        "Invalid "
                            <> Text.pack (unsafeToFilePath path)
                            <> ": "
                            <> Text.pack err
                Right config ->
                    case validateHarnessConfig config of
                        Left err -> pure (Left err)
                        Right valid ->
                            loadHarnessRevisionKeyUnlocked home >>= \case
                                Left err -> pure (Left err)
                                Right key ->
                                    pure (Right
                                        (keyedConfigRevision key bytes, valid))

-- | Atomically read, transform, validate, and replace the config while holding
-- the process-shared config lock. The returned revision is for the exact bytes
-- written by this transaction.
modifyHarnessConfig
    :: OsPath
    -> (Word64 -> HarnessConfig -> Either Text (HarnessConfig, a))
    -> IO (Either Text (Word64, HarnessConfig, a))
modifyHarnessConfig home change =
    withPrivateFileLock (harnessConfigLockPath home) do
        loadHarnessConfigUnlocked home >>= \case
            Left err -> pure (Left err)
            Right (revision, config) ->
                case change revision config of
                    Left err -> pure (Left err)
                    Right (updated, value) ->
                        writeHarnessConfigUnlocked home updated >>= \case
                            Left err -> pure (Left err)
                            Right nextRevision ->
                                pure (Right
                                    (nextRevision, updated, value))

writeHarnessConfigUnlocked
    :: OsPath -> HarnessConfig -> IO (Either Text Word64)
writeHarnessConfigUnlocked home config =
    case validateHarnessConfig config of
        Left err -> pure (Left err)
        Right valid -> do
            let directory =
                    home </> unsafeEncodeUtf ".haskell-agent"
                path = harnessConfigPath home
                bytes = Aeson.encode valid
            loadHarnessRevisionKeyUnlocked home >>= \case
                Left err -> pure (Left err)
                Right key -> do
                    result <- tryIO do
                        createDirectoryIfMissing True directory
                        setFileMode (unsafeToFilePath directory) 0o700
                        writeLazyFileAtomically path 0o600 bytes
                    case result of
                        Left exception ->
                            pure . Left $
                                ( "Failed to write "
                                    <> Text.pack (unsafeToFilePath path)
                                    <> ": "
                                    <> Text.pack (displayException exception)
                                )
                        Right () ->
                            pure (Right (keyedConfigRevision key bytes))

harnessConfigLockPath :: OsPath -> OsPath
harnessConfigLockPath home =
    home
        </> unsafeEncodeUtf ".haskell-agent"
        </> unsafeEncodeUtf "config.lock"

harnessRevisionKeyPath :: OsPath -> OsPath
harnessRevisionKeyPath home =
    home
        </> unsafeEncodeUtf ".haskell-agent"
        </> unsafeEncodeUtf "config.revision-key"

loadHarnessRevisionKeyUnlocked :: OsPath -> IO (Either Text BS.ByteString)
loadHarnessRevisionKeyUnlocked home = do
    let path = harnessRevisionKeyPath home
        directory = home </> unsafeEncodeUtf ".haskell-agent"
    exists <- doesFileExist path
    existing <-
        if exists
            then tryIO (BS.readFile (unsafeToFilePath path))
            else pure (Right BS.empty)
    case existing of
        Right key | BS.length key == 16 -> do
            secured <- tryIO
                (setFileMode (unsafeToFilePath path) 0o600)
            case secured of
                Left exception ->
                    pure (Left
                        ("Failed to secure config revision key: "
                            <> Text.pack (displayException exception)))
                Right () -> pure (Right key)
        _ -> do
            generated <- tryIO $
                withBinaryFile "/dev/urandom" ReadMode (`BS.hGet` 16)
            case generated of
                Left exception ->
                    pure (Left
                        ("Failed to generate config revision key: "
                            <> Text.pack (displayException exception)))
                Right key | BS.length key == 16 -> do
                    written <- tryIO do
                        createDirectoryIfMissing True directory
                        setFileMode (unsafeToFilePath directory) 0o700
                        writeLazyFileAtomically path 0o600
                            (LBS.fromStrict key)
                    case written of
                        Left exception ->
                            pure (Left
                                ("Failed to store config revision key: "
                                    <> Text.pack
                                        (displayException exception)))
                        Right () -> pure (Right key)
                Right _ ->
                    pure (Left "Failed to generate config revision key")

-- SipHash-2-4, used only to derive an opaque revision token from exact config
-- bytes. The key never crosses the native boundary.
keyedConfigRevision :: BS.ByteString -> LBS.ByteString -> Word64
keyedConfigRevision key lazyBytes =
    finalize (compressBlocks initial bytes)
  where
    bytes = LBS.toStrict lazyBytes
    k0 = word64LE (BS.take 8 key)
    k1 = word64LE (BS.drop 8 key)
    initial =
        ( 0x736f6d6570736575 `xor` k0
        , 0x646f72616e646f6d `xor` k1
        , 0x6c7967656e657261 `xor` k0
        , 0x7465646279746573 `xor` k1
        )
    compressBlocks state remaining
        | BS.length remaining >= 8 =
            let message = word64LE (BS.take 8 remaining)
                (v0, v1, v2, v3) = state
                mixed = sipRounds 2 (v0, v1, v2, v3 `xor` message)
                (r0, r1, r2, r3) = mixed
            in compressBlocks
                (r0 `xor` message, r1, r2, r3)
                (BS.drop 8 remaining)
        | otherwise =
            let finalBlock =
                    (fromIntegral (BS.length bytes) `shiftL` 56)
                        .|. word64LE remaining
                (v0, v1, v2, v3) = state
                mixed = sipRounds 2
                    (v0, v1, v2, v3 `xor` finalBlock)
                (r0, r1, r2, r3) = mixed
            in (r0 `xor` finalBlock, r1, r2, r3)
    finalize (v0, v1, v2, v3) =
        let (r0, r1, r2, r3) =
                sipRounds 4 (v0, v1, v2 `xor` 0xff, v3)
        in r0 `xor` r1 `xor` r2 `xor` r3

word64LE :: BS.ByteString -> Word64
word64LE =
    snd . BS.foldl' step (0 :: Int, 0)
  where
    step (offset, value) byte =
        (offset + 8, value .|. (fromIntegral byte `shiftL` offset))

sipRounds
    :: Int
    -> (Word64, Word64, Word64, Word64)
    -> (Word64, Word64, Word64, Word64)
sipRounds count state
    | count <= 0 = state
    | otherwise =
        let (v0, v1, v2, v3) = state
            a0 = v0 + v1
            a1 = rotateL v1 13 `xor` a0
            a2 = rotateL a0 32
            a3 = v2 + v3
            a4 = rotateL v3 16 `xor` a3
            b0 = a2 + a4
            b1 = rotateL a4 21 `xor` b0
            b2 = a3 + a1
            b3 = rotateL a1 17 `xor` b2
            b4 = rotateL b2 32
        in sipRounds (count - 1) (b0, b3, b4, b1)


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
    validateWebFetch config.configWebFetch
    _ <- Map.traverseWithKey validateLspServer config.configLsp.lspServers
    validateMaxConcurrentAgents config.configMaxConcurrentAgents
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

    validateMaxConcurrentAgents = \case
        Nothing -> pure ()
        Just limit
            | limit < 1 ->
                Left "maxConcurrentAgents must be at least 1"
            | otherwise -> pure ()

    validateWebFetch web = do
        when (web.webFetchTimeoutSeconds < 1) $
            Left "webFetch timeoutSeconds must be positive"
        when (web.webFetchTimeoutSeconds > 300) $
            Left "webFetch timeoutSeconds must not exceed 300"
        when (web.webFetchMaxContentBytes < 1) $
            Left "webFetch maxContentBytes must be positive"
        when (web.webFetchMaxContentBytes > 50 * 1024 * 1024) $
            Left "webFetch maxContentBytes must not exceed 52428800"
        when (web.webFetchMaxInlineBytes < 1) $
            Left "webFetch maxInlineBytes must be positive"
        when (web.webFetchMaxInlineBytes > 1024 * 1024) $
            Left "webFetch maxInlineBytes must not exceed 1048576"
        when
            (web.webFetchMaxInlineBytes > web.webFetchMaxContentBytes)
            (Left
                "webFetch maxInlineBytes must not exceed maxContentBytes")
        when
            (any (Text.null . Text.strip) web.webFetchAllowedDomains)
            (Left "webFetch allowedDomains must not contain empty entries")

    validateLspServer label server = do
        when (Text.null (Text.strip label)) $
            Left "LSP server label must not be empty"
        when (Text.null (Text.strip server.lspCommand)) $
            Left ("LSP server " <> quote label <> " has an empty command")
        when (Map.null server.lspExtensionToLanguage) $
            Left
                ( "LSP server "
                    <> quote label
                    <> " must configure extensionToLanguage"
                )
        when
            (any (Text.null . Text.strip) $
                Map.keys server.lspExtensionToLanguage
                    <> Map.elems server.lspExtensionToLanguage)
            (Left
                ( "LSP server "
                    <> quote label
                    <> " has an empty extension or language id"
                ))
        when (server.lspStartupTimeoutMilliseconds < 1) $
            Left
                ( "LSP server "
                    <> quote label
                    <> " startupTimeoutMilliseconds must be positive"
                )
        when (server.lspStartupTimeoutMilliseconds > 120000) $
            Left
                ( "LSP server "
                    <> quote label
                    <> " startupTimeoutMilliseconds must not exceed 120000"
                )
        when (server.lspShutdownTimeoutMilliseconds < 1) $
            Left
                ( "LSP server "
                    <> quote label
                    <> " shutdownTimeoutMilliseconds must be positive"
                )
        when (server.lspShutdownTimeoutMilliseconds > 120000) $
            Left
                ( "LSP server "
                    <> quote label
                    <> " shutdownTimeoutMilliseconds must not exceed 120000"
                )
        pure server

    quote value = "'" <> value <> "'"
