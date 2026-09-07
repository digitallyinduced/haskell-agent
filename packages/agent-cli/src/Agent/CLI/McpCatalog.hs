-- | Non-interactive MCP catalog commands: list, enable, and disable servers
-- in @~/.haskell-agent/config.json@. Environment values are never shown.
module Agent.CLI.McpCatalog
    ( McpCatalogChange(..)
    , McpCatalogEntry(..)
    , McpCatalogError(..)
    , formatMcpCatalogChange
    , formatMcpCatalogHuman
    , formatMcpCatalogJSON
    , listMcpCatalog
    , mcpCatalogEntries
    , mcpCatalogTransport
    , runMcpCommand
    , setMcpCatalogEnabled
    ) where

import Agent.CLI.Config
    ( HarnessConfig(..)
    , McpServerConfig(..)
    , loadHarnessConfig
    , modifyHarnessConfig
    )
import Agent.CLI.McpOAuth
    ( LoginOptions(..)
    , defaultLoginOptions
    , loginMcpWith
    , logoutMcp
    )
import Agent.CLI.Options
    ( McpCommand(..)
    , SessionOutputFormat(..)
    )
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Char (isAlphaNum)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory.OsPath (getHomeDirectory)
import System.Exit (die)
import System.OsPath (OsPath)

data McpCatalogError
    = McpCatalogNotFound !Text
    | McpCatalogInvalid !Text
    deriving (Eq, Show)

data McpCatalogEntry = McpCatalogEntry
    { mcpCatalogName :: !Text
    , mcpCatalogEnabled :: !Bool
    , mcpCatalogUrl :: !(Maybe Text)
    , mcpCatalogCommand :: !Text
    , mcpCatalogArgs :: ![Text]
    , mcpCatalogCwd :: !(Maybe Text)
    , mcpCatalogEnvKeys :: ![Text]
    }
    deriving (Eq, Show)

data McpCatalogChange = McpCatalogChange
    { mcpCatalogChanged :: !Bool
    , mcpCatalogEntry :: !McpCatalogEntry
    }
    deriving (Eq, Show)

runMcpCommand :: McpCommand -> IO ()
runMcpCommand = \case
    McpLogin url scopes ->
        loginMcpWith defaultLoginOptions { loginAdditionalScopes = scopes } url
    McpLogout url -> logoutMcp url
    McpList outputFormat -> runMcpList outputFormat
    McpEnable name -> runMcpSetEnabled True name
    McpDisable name -> runMcpSetEnabled False name

runMcpList :: SessionOutputFormat -> IO ()
runMcpList outputFormat = do
    home <- getHomeDirectory
    listMcpCatalog home >>= \case
        Left err -> die (Text.unpack (catalogErrorText err))
        Right entries ->
            case outputFormat of
                SessionHuman -> Text.putStr (formatMcpCatalogHuman entries)
                SessionJSON -> LBS8.putStrLn (formatMcpCatalogJSON entries)

runMcpSetEnabled :: Bool -> Text -> IO ()
runMcpSetEnabled enabled name = do
    home <- getHomeDirectory
    setMcpCatalogEnabled home name enabled >>= \case
        Left err -> die (Text.unpack (catalogErrorText err))
        Right change -> Text.putStrLn (formatMcpCatalogChange enabled change)

listMcpCatalog :: OsPath -> IO (Either McpCatalogError [McpCatalogEntry])
listMcpCatalog home =
    loadHarnessConfig home >>= \case
        Left err -> pure (Left (McpCatalogInvalid err))
        Right config -> pure (Right (mcpCatalogEntries config))

setMcpCatalogEnabled
    :: OsPath
    -> Text
    -> Bool
    -> IO (Either McpCatalogError McpCatalogChange)
setMcpCatalogEnabled home name enabled
    | Text.null trimmed =
        pure (Left (McpCatalogInvalid "MCP server name must not be empty"))
    | otherwise =
        modifyHarnessConfig home (\_ config -> change config) >>= \case
            Left err
                | Just missing <- Text.stripPrefix notFoundPrefix err ->
                    pure (Left (McpCatalogNotFound missing))
                | otherwise ->
                    pure (Left (McpCatalogInvalid err))
            Right (_, _, result) -> pure (Right result)
  where
    trimmed = Text.strip name
    change config =
        case Map.lookup trimmed config.configMcpServers of
            Nothing -> Left (notFoundPrefix <> trimmed)
            Just server ->
                let updated = server { mcpEnabled = enabled }
                    next =
                        config
                            { configMcpServers =
                                Map.insert trimmed updated
                                    config.configMcpServers
                            }
                in Right
                    ( next
                    , McpCatalogChange
                        { mcpCatalogChanged = server.mcpEnabled /= enabled
                        , mcpCatalogEntry = catalogEntry trimmed updated
                        }
                    )

mcpCatalogEntries :: HarnessConfig -> [McpCatalogEntry]
mcpCatalogEntries config =
    [ catalogEntry name server
    | (name, server) <- Map.toAscList config.configMcpServers
    ]

mcpCatalogTransport :: McpCatalogEntry -> Text
mcpCatalogTransport entry =
    case entry.mcpCatalogUrl of
        Just _ -> "http"
        Nothing -> "stdio"

formatMcpCatalogHuman :: [McpCatalogEntry] -> Text
formatMcpCatalogHuman [] = "No MCP servers configured\n"
formatMcpCatalogHuman entries =
    Text.unlines (map (formatHumanLine width) entries)
  where
    width = maximum (map (Text.length . catalogLabel) entries)

formatMcpCatalogJSON :: [McpCatalogEntry] -> LBS.ByteString
formatMcpCatalogJSON = Aeson.encode . map mcpCatalogEntryJSON

formatMcpCatalogChange :: Bool -> McpCatalogChange -> Text
formatMcpCatalogChange enabled change =
    if change.mcpCatalogChanged
        then verb <> " MCP server " <> name
        else "MCP server " <> name <> " is already " <> state
  where
    name = change.mcpCatalogEntry.mcpCatalogName
    verb = if enabled then "Enabled" else "Disabled"
    state = if enabled then "enabled" else "disabled"

catalogEntry :: Text -> McpServerConfig -> McpCatalogEntry
catalogEntry name server =
    McpCatalogEntry
        { mcpCatalogName = name
        , mcpCatalogEnabled = server.mcpEnabled
        , mcpCatalogUrl = server.mcpUrl
        , mcpCatalogCommand = server.mcpCommand
        , mcpCatalogArgs = server.mcpArgs
        , mcpCatalogCwd = server.mcpCwd
        , mcpCatalogEnvKeys = Map.keys server.mcpEnv
        }

catalogLabel :: McpCatalogEntry -> Text
catalogLabel entry =
    entry.mcpCatalogName
        <> if entry.mcpCatalogEnabled then "" else " (disabled)"

formatHumanLine :: Int -> McpCatalogEntry -> Text
formatHumanLine width entry =
    Text.justifyLeft width ' ' (catalogLabel entry)
        <> "  "
        <> mcpCatalogTransport entry
        <> "  "
        <> catalogTarget entry

catalogTarget :: McpCatalogEntry -> Text
catalogTarget entry =
    case entry.mcpCatalogUrl of
        Just url -> url
        Nothing ->
            Text.unwords
                (map shellQuote (entry.mcpCatalogCommand : entry.mcpCatalogArgs))

mcpCatalogEntryJSON :: McpCatalogEntry -> Aeson.Value
mcpCatalogEntryJSON entry =
    Aeson.object
        [ "name" .= entry.mcpCatalogName
        , "enabled" .= entry.mcpCatalogEnabled
        , "transport" .= mcpCatalogTransport entry
        , "url" .= entry.mcpCatalogUrl
        , "command" .= entry.mcpCatalogCommand
        , "args" .= entry.mcpCatalogArgs
        , "cwd" .= entry.mcpCatalogCwd
        , "envKeys" .= entry.mcpCatalogEnvKeys
        ]

catalogErrorText :: McpCatalogError -> Text
catalogErrorText = \case
    McpCatalogNotFound name ->
        "MCP server " <> name <> " is not configured"
    McpCatalogInvalid err -> err

notFoundPrefix :: Text
notFoundPrefix = "not-found:"

shellQuote :: Text -> Text
shellQuote value
    | not (Text.null value)
    , Text.all safe value = value
    | otherwise =
        "'" <> Text.replace "'" "'\\''" value <> "'"
  where
    safe char =
        isAlphaNum char || char `elem` ("-._/:@+=," :: String)
