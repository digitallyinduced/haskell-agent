-- | Parse an MCP add target (remote URL or local stdio command) and build
-- the catalog entry. Used by the TUI add form and @agent-cli mcp add@.
module Agent.CLI.McpAdd
    ( McpAddTarget(..)
    , McpTransportOption(..)
    , defaultMcpServerConfig
    , isHttpMcpUrl
    , mcpServerForTarget
    , parseMcpAddName
    , parseMcpCommand
    , parseMcpTarget
    , parseMcpTargetWithTransport
    , suggestMcpName
    , suggestMcpNameFromUrl
    , suggestMcpTargetName
    ) where

import Agent.CLI.Config (McpServerConfig(..))
import Agent.CLI.ExternalProgram (parseProgramWords)
import Agent.MCP (McpProtocolPreference(..))
import Control.Applicative ((<|>))
import Data.Char (isAlphaNum, isAscii, toLower)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath (dropExtension, takeFileName)

data McpAddTarget
    = McpAddHttpUrl !Text
    | McpAddStdioCommand !Text ![Text]
    deriving (Eq, Show)

data McpTransportOption
    = McpTransportStdio
    | McpTransportHttp
    deriving (Eq, Show)

defaultMcpServerConfig :: McpServerConfig
defaultMcpServerConfig =
    McpServerConfig
        { mcpEnabled = True
        , mcpUrl = Nothing
        , mcpCommand = ""
        , mcpArgs = []
        , mcpCwd = Nothing
        , mcpEnv = Map.empty
        , mcpStartupTimeoutSeconds = 30
        , mcpRequestTimeoutSeconds = 60
        , mcpOAuth = Nothing
        , mcpProtocol = McpProtocolAuto
        , mcpRoots = False
        , mcpSampling = False
        , mcpLogLevel = Nothing
        }

mcpServerForTarget :: McpAddTarget -> McpServerConfig
mcpServerForTarget = \case
    McpAddHttpUrl url ->
        defaultMcpServerConfig { mcpUrl = Just url }
    McpAddStdioCommand command arguments ->
        defaultMcpServerConfig
            { mcpCommand = command
            , mcpArgs = arguments
            }

isHttpMcpUrl :: Text -> Bool
isHttpMcpUrl input =
    let stripped = Text.strip input
        lower = Text.toLower stripped
    in "https://" `Text.isPrefixOf` lower
        || "http://" `Text.isPrefixOf` lower

parseMcpTarget :: Text -> Either Text McpAddTarget
parseMcpTarget = parseMcpTargetWithTransport Nothing

parseMcpTargetWithTransport
    :: Maybe McpTransportOption
    -> Text
    -> Either Text McpAddTarget
parseMcpTargetWithTransport transport input =
    let stripped = Text.strip input
    in if Text.null stripped
        then Left "MCP URL or command must not be empty"
        else case transport of
            Just McpTransportHttp ->
                if isHttpMcpUrl stripped
                    then Right (McpAddHttpUrl stripped)
                    else Left "HTTP MCP servers require an http(s) URL"
            Just McpTransportStdio
                | isHttpMcpUrl stripped ->
                    Left "stdio MCP servers require a local command, not a URL"
                | otherwise -> stdioTarget stripped
            Nothing
                | isHttpMcpUrl stripped -> Right (McpAddHttpUrl stripped)
                | otherwise -> stdioTarget stripped
  where
    stdioTarget raw = do
        (command, arguments) <- parseMcpCommand raw
        pure (McpAddStdioCommand command arguments)

parseMcpCommand :: Text -> Either Text (Text, [Text])
parseMcpCommand input = do
    arguments <- case parseProgramWords input of
        Left "program specification ends with an incomplete escape" ->
            Left "MCP command ends with an incomplete escape"
        Left "program specification contains an unterminated quote" ->
            Left "MCP command contains an unterminated quote"
        Left err -> Left ("MCP command " <> err)
        Right values -> Right values
    case arguments of
        command : rest
            | not (Text.null (Text.strip command)) ->
                Right (command, rest)
        _ -> Left "MCP command must not be empty"

parseMcpAddName :: Text -> Either Text Text
parseMcpAddName input =
    let trimmed = Text.strip input
    in if Text.null trimmed
        then Left "MCP server name must not be empty"
        else if Text.all allowed trimmed
            then Right trimmed
            else Left
                "MCP server names may only contain letters, numbers, hyphens, and underscores"
  where
    allowed char = isAscii char && (isAlphaNum char || char `elem` ['-', '_'])

suggestMcpTargetName :: McpAddTarget -> Text
suggestMcpTargetName = \case
    McpAddHttpUrl url -> suggestMcpNameFromUrl url
    McpAddStdioCommand command arguments -> suggestMcpName command arguments

suggestMcpNameFromUrl :: Text -> Text
suggestMcpNameFromUrl url =
    fromMaybe "mcp-server" (find usable candidates)
  where
    stripped = Text.strip url
    withoutScheme =
        fromMaybe stripped
            (stripSchemePrefix "https://" stripped
                <|> stripSchemePrefix "http://" stripped)
    host =
        Text.takeWhile (/= ':')
            (Text.takeWhile (\char -> char /= '/' && char /= '?') withoutScheme)
    labels = filter (not . Text.null) (Text.splitOn "." host)
    withoutMcp = case labels of
        first : rest
            | Text.toLower first == "mcp" -> rest
        _ -> labels
    candidates = map normalizeCandidate (withoutMcp <> labels <> ["mcp-server"])

stripSchemePrefix :: Text -> Text -> Maybe Text
stripSchemePrefix prefix value =
    let (head_, rest) = Text.splitAt (Text.length prefix) value
    in if Text.toLower head_ == prefix then Just rest else Nothing

suggestMcpName :: Text -> [Text] -> Text
suggestMcpName command arguments =
    fromMaybe "mcp-server" (find usable candidates)
  where
    candidates = map normalizeCandidate $
        case Text.toLower (baseName command) of
            "nix" ->
                dropLauncherOptions ["run"] arguments
                    <> [command]
            "npx" ->
                dropLauncherOptions [] arguments
                    <> [command]
            "node" ->
                dropLauncherOptions [] arguments
                    <> [command]
            _ -> command : arguments
    dropLauncherOptions subcommands =
        dropWhile
            (\value ->
                "-" `Text.isPrefixOf` value
                    || Text.toLower value `elem` subcommands)

normalizeCandidate :: Text -> Text
normalizeCandidate =
    Text.map normalizeChar
        . Text.pack . dropExtension . Text.unpack . baseName

baseName :: Text -> Text
baseName = Text.pack . takeFileName . Text.unpack

normalizeChar :: Char -> Char
normalizeChar char
    | isAlphaNum char || char `elem` ['-', '_'] = toLower char
    | otherwise = '-'

usable :: Text -> Bool
usable candidate =
    not (Text.null candidate)
        && candidate `notElem` reserved
        && not ("-" `Text.isPrefixOf` candidate)
        && Text.all (\char -> isAlphaNum char || char `elem` ['-', '_']) candidate
  where
    reserved = ["run", "exec", "npx", "node", "nix", "mcp", "www", "com"]
