-- | A private, typed planning boundary for natural-language configuration.
--
-- The meta console deliberately does not inherit the coding transcript and
-- cannot call tools.  Its only output is a small JSON action language which
-- the CLI validates before presenting or executing it.
module Agent.CLI.MetaConsole
    ( MetaPlan(..)
    , MetaAction(..)
    , MetaMcpServer(..)
    , MetaWebFetchUpdate(..)
    , MetaLspServer(..)
    , MetaError(..)
    , decodeMetaPlan
    , validateMetaPlan
    , metaActionPreview
    , metaPlanPreviews
    , metaPlanMutates
    , redactMetaContext
    , metaConsolePrompt
    , runMetaConsoleWithCancel
    , formatMetaError
    ) where

import Agent.Cancel (CancelFlag, newCancelFlag, waitCancel)
import Agent.CLI.Btw (BtwBackendFactory)
import Agent.CLI.Config (McpInitStrategy(..))
import Agent.CLI.Error (formatApiErrorInline)
import Agent.Error (ApiError)
import Agent.Loop
    ( Backend(..)
    , BackendResult(..)
    , TurnInput(..)
    , TurnOutput(..)
    )
import Agent.MCP (McpProtocolPreference(..))
import Agent.Provider (Provider(..), providerSlug)
import Agent.Responses.Types
    ( ResponseCreateParams(..)
    , ToolChoice(..)
    , ToolChoiceMode(..)
    )
import Control.Concurrent.Async (race)
import Control.Monad (unless, when)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.!=), (.:), (.:?))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Object, Parser)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum)
import Data.IORef (IORef, readIORef)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncodingError

-- | A model-proposed configuration plan.  The summary is display-only; only
-- the typed actions can be executed.
data MetaPlan = MetaPlan
    { metaSummary :: !Text
    , metaActions :: ![MetaAction]
    }
    deriving (Eq, Show)

-- | The complete executable language available to the meta planner.
--
-- Secret-bearing fields are intentionally absent.  OAuth credentials are
-- acquired later by the host-owned login flow rather than by the model.
data MetaAction
    = MetaSessionCommand !Text
    | MetaConnectAccount !Provider
    | MetaUpsertMcp !MetaMcpServer
    | MetaRemoveMcp !Text
    | MetaSetMcpEnabled !Text !Bool
    | MetaLoginMcpOAuth !Text
    | MetaSetMcpInitStrategy !McpInitStrategy
    | MetaSetWebFetch !MetaWebFetchUpdate
    | MetaSetLspEnabled !Bool
    | MetaUpsertLsp !MetaLspServer
    | MetaRemoveLsp !Text
    | MetaSetMaxConcurrentAgents !(Maybe Int)
    | MetaClarify !Text
    | MetaInform !Text
    deriving (Eq, Show)

-- | Public, non-secret MCP settings.  Existing environment and OAuth client
-- credentials must be preserved or managed by the host when applying this.
data MetaMcpServer = MetaMcpServer
    { metaMcpName :: !Text
    , metaMcpEnabled :: !Bool
    , metaMcpUrl :: !(Maybe Text)
    , metaMcpCommand :: !(Maybe Text)
    , metaMcpArgs :: ![Text]
    , metaMcpCwd :: !(Maybe Text)
    , metaMcpStartupTimeoutSeconds :: !Int
    , metaMcpRequestTimeoutSeconds :: !Int
    , metaMcpProtocol :: !McpProtocolPreference
    }
    deriving (Eq, Show)

-- | A partial update to the harness web-fetch policy.
data MetaWebFetchUpdate = MetaWebFetchUpdate
    { metaWebFetchEnabled :: !(Maybe Bool)
    , metaWebFetchAllowedDomains :: !(Maybe [Text])
    , metaWebFetchTimeoutSeconds :: !(Maybe Int)
    , metaWebFetchMaxContentBytes :: !(Maybe Int)
    , metaWebFetchMaxInlineBytes :: !(Maybe Int)
    }
    deriving (Eq, Show)

-- | Public, non-secret settings for one stdio language server.
data MetaLspServer = MetaLspServer
    { metaLspName :: !Text
    , metaLspCommand :: !Text
    , metaLspArgs :: ![Text]
    , metaLspExtensionToLanguage :: !(Map Text Text)
    , metaLspWorkspaceFolder :: !(Maybe Text)
    , metaLspStartupTimeoutMilliseconds :: !Int
    , metaLspShutdownTimeoutMilliseconds :: !Int
    }
    deriving (Eq, Show)

data MetaError
    = MetaTransport !ApiError
    | MetaCancelled
    | MetaEmptyResponse
    | MetaUnexpectedToolCall
    | MetaInvalidResponse
    | MetaInvalidPlan !Text
    deriving (Eq, Show)

instance Aeson.FromJSON MetaPlan where
    parseJSON = Aeson.withObject "MetaPlan" \object -> do
        rejectUnknownKeys "plan" ["summary", "actions"] object
        MetaPlan
            <$> object .: "summary"
            <*> object .: "actions"

instance Aeson.FromJSON MetaAction where
    parseJSON = Aeson.withObject "MetaAction" \object -> do
        actionType <- object .: "type"
        case (actionType :: Text) of
            "session_command" -> do
                rejectUnknownKeys "session_command"
                    ["type", "command"] object
                MetaSessionCommand <$> object .: "command"
            "connect_account" -> do
                rejectUnknownKeys "connect_account"
                    ["type", "provider"] object
                providerText <- object .: "provider"
                case parseMetaProvider providerText of
                    Nothing ->
                        fail
                            "unsupported provider (expected openai, grok/xai, openrouter, or claude)"
                    Just provider -> pure (MetaConnectAccount provider)
            "mcp_upsert" -> do
                rejectUnknownKeys "mcp_upsert"
                    [ "type", "name", "enabled", "url", "command", "args"
                    , "cwd", "startupTimeoutSeconds", "requestTimeoutSeconds"
                    , "protocol"
                    ]
                    object
                MetaUpsertMcp <$> parseMcpServer object
            "mcp_remove" -> do
                rejectUnknownKeys "mcp_remove" ["type", "name"] object
                MetaRemoveMcp <$> object .: "name"
            "mcp_set_enabled" -> do
                rejectUnknownKeys "mcp_set_enabled"
                    ["type", "name", "enabled"] object
                MetaSetMcpEnabled
                    <$> object .: "name"
                    <*> object .: "enabled"
            "mcp_oauth_login" -> do
                rejectUnknownKeys "mcp_oauth_login"
                    ["type", "name"] object
                MetaLoginMcpOAuth <$> object .: "name"
            "set_mcp_init_strategy" -> do
                rejectUnknownKeys "set_mcp_init_strategy"
                    ["type", "strategy"] object
                strategy <- object .: "strategy"
                MetaSetMcpInitStrategy <$> parseMcpInitStrategy strategy
            "set_web_fetch" -> do
                rejectUnknownKeys "set_web_fetch"
                    [ "type", "enabled", "allowedDomains", "timeoutSeconds"
                    , "maxContentBytes", "maxInlineBytes"
                    ]
                    object
                MetaSetWebFetch <$> parseWebFetchUpdate object
            "set_lsp_enabled" -> do
                rejectUnknownKeys "set_lsp_enabled"
                    ["type", "enabled"] object
                MetaSetLspEnabled <$> object .: "enabled"
            "lsp_upsert" -> do
                rejectUnknownKeys "lsp_upsert"
                    [ "type", "name", "command", "args"
                    , "extensionToLanguage", "workspaceFolder"
                    , "startupTimeoutMilliseconds"
                    , "shutdownTimeoutMilliseconds"
                    ]
                    object
                MetaUpsertLsp <$> parseLspServer object
            "lsp_remove" -> do
                rejectUnknownKeys "lsp_remove" ["type", "name"] object
                MetaRemoveLsp <$> object .: "name"
            "set_max_concurrent_agents" -> do
                rejectUnknownKeys "set_max_concurrent_agents"
                    ["type", "limit"] object
                unless (KeyMap.member "limit" object) $
                    fail "set_max_concurrent_agents requires limit (integer or null)"
                MetaSetMaxConcurrentAgents <$> object .: "limit"
            "clarify" -> do
                rejectUnknownKeys "clarify" ["type", "question"] object
                MetaClarify <$> object .: "question"
            "inform" -> do
                rejectUnknownKeys "inform" ["type", "message"] object
                MetaInform <$> object .: "message"
            other ->
                fail ("unknown meta action type: " <> Text.unpack other)

parseMcpServer :: Object -> Parser MetaMcpServer
parseMcpServer object =
    MetaMcpServer
        <$> object .: "name"
        <*> object .:? "enabled" .!= True
        <*> object .:? "url"
        <*> object .:? "command"
        <*> object .:? "args" .!= []
        <*> object .:? "cwd"
        <*> object .:? "startupTimeoutSeconds" .!= 30
        <*> object .:? "requestTimeoutSeconds" .!= 60
        <*> (object .:? "protocol" .!= ("auto" :: Text)
            >>= parseMcpProtocol)

parseWebFetchUpdate :: Object -> Parser MetaWebFetchUpdate
parseWebFetchUpdate object =
    MetaWebFetchUpdate
        <$> object .:? "enabled"
        <*> object .:? "allowedDomains"
        <*> object .:? "timeoutSeconds"
        <*> object .:? "maxContentBytes"
        <*> object .:? "maxInlineBytes"

parseLspServer :: Object -> Parser MetaLspServer
parseLspServer object =
    MetaLspServer
        <$> object .: "name"
        <*> object .: "command"
        <*> object .:? "args" .!= []
        <*> object .: "extensionToLanguage"
        <*> object .:? "workspaceFolder"
        <*> object .:? "startupTimeoutMilliseconds" .!= 15000
        <*> object .:? "shutdownTimeoutMilliseconds" .!= 5000

parseMetaProvider :: Text -> Maybe Provider
parseMetaProvider raw =
    case Text.toLower (Text.strip raw) of
        "openai" -> Just OpenAIProvider
        "xai" -> Just XAIProvider
        "grok" -> Just XAIProvider
        "openrouter" -> Just OpenRouterProvider
        "open-router" -> Just OpenRouterProvider
        "claude" -> Just ClaudeCodeProvider
        "claude-code" -> Just ClaudeCodeProvider
        _ -> Nothing

parseMcpProtocol :: Text -> Parser McpProtocolPreference
parseMcpProtocol raw =
    case Text.toLower (Text.strip raw) of
        "auto" -> pure McpProtocolAuto
        "modern" -> pure McpProtocolModern
        "legacy" -> pure McpProtocolLegacy
        other ->
            fail
                ("unknown MCP protocol: " <> Text.unpack other
                    <> " (expected auto, modern, or legacy)")

parseMcpInitStrategy :: Text -> Parser McpInitStrategy
parseMcpInitStrategy raw =
    case Text.toLower (Text.strip raw) of
        "auto" -> pure McpInitAuto
        "progressive" -> pure McpInitProgressive
        "blocking" -> pure McpInitBlocking
        other ->
            fail
                ("unknown MCP initialization strategy: "
                    <> Text.unpack other)

rejectUnknownKeys :: String -> [Text] -> Object -> Parser ()
rejectUnknownKeys label allowed object =
    case filter (`notElem` allowed) actual of
        [] -> pure ()
        unknown ->
            fail
                (label <> " contains unknown field(s): "
                    <> Text.unpack (Text.intercalate ", " unknown))
  where
    actual = map Key.toText (KeyMap.keys object)

-- | Decode plain JSON or one exact @```json ... ```@ fence, then run semantic
-- validation.  Surrounding prose is intentionally rejected.
decodeMetaPlan :: Text -> Either Text MetaPlan
decodeMetaPlan raw = do
    json <- stripJsonFence raw
    plan <-
        case Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 json) of
            Left err -> Left ("invalid meta plan JSON: " <> Text.pack err)
            Right parsed -> Right parsed
    validateMetaPlan plan

stripJsonFence :: Text -> Either Text Text
stripJsonFence raw =
    case Text.lines (Text.strip raw) of
        first : rest
            | Text.toLower (Text.strip first) == "```json" ->
                case reverse rest of
                    lastLine : body
                        | Text.strip lastLine == "```" ->
                            Right (Text.unlines (reverse body))
                    _ -> Left "invalid JSON fence: expected a closing ```"
        lines_
            | any (Text.isPrefixOf "```" . Text.strip) lines_ ->
                Left "invalid JSON fence: only a single ```json fence is allowed"
            | otherwise -> Right (Text.strip raw)

-- | Enforce safety and consistency constraints after structural decoding.
validateMetaPlan :: MetaPlan -> Either Text MetaPlan
validateMetaPlan plan = do
    requireNonBlank "plan summary" plan.metaSummary
    when (Text.length plan.metaSummary > 500) $
        Left "plan summary must be at most 500 characters"
    when (null plan.metaActions) $
        Left "meta plan must contain at least one action"
    when (length plan.metaActions > 12) $
        Left "meta plan must contain at most 12 actions"
    mapM_ validateAction plan.metaActions
    validateClarification plan.metaActions
    validateSingletonActions plan.metaActions
    validateMcpConflicts plan.metaActions
    validateLspConflicts plan.metaActions
    pure plan

validateAction :: MetaAction -> Either Text ()
validateAction = \case
    MetaSessionCommand command -> validateSessionCommand command
    MetaConnectAccount _ -> pure ()
    MetaUpsertMcp server -> validateMcpServer server
    MetaRemoveMcp name -> validateConfigName "MCP server" name
    MetaSetMcpEnabled name _ -> validateConfigName "MCP server" name
    MetaLoginMcpOAuth name -> validateConfigName "MCP server" name
    MetaSetMcpInitStrategy _ -> pure ()
    MetaSetWebFetch update -> validateWebFetchUpdate update
    MetaSetLspEnabled _ -> pure ()
    MetaUpsertLsp server -> validateLspServer server
    MetaRemoveLsp name -> validateConfigName "LSP server" name
    MetaSetMaxConcurrentAgents limit ->
        case limit of
            Just value | value < 1 ->
                Left "max concurrent agents must be at least 1"
            Just value | value > 256 ->
                Left "max concurrent agents must not exceed 256"
            _ -> pure ()
    MetaClarify question -> requireNonBlank "clarification question" question
    MetaInform message -> requireNonBlank "informational message" message

validateSessionCommand :: Text -> Either Text ()
validateSessionCommand raw =
    case Text.words (Text.strip raw) of
        ["/model", model] | nonBlank model -> pure ()
        ["/effort", effort]
            | Text.toLower effort
                `elem` ["none", "low", "medium", "high", "xhigh", "max"] ->
                    pure ()
        ["/fast"] -> pure ()
        ["/shell", mode]
            | Text.toLower mode `elem` ["ghci", "bash", "both", "none"] ->
                pure ()
        ["/codemod"] -> pure ()
        ["/always-approve"] -> pure ()
        ["/agents", "limit", limit]
            | [(value, "")] <- reads (Text.unpack limit)
            , value >= (1 :: Int)
            , value <= 256 ->
                pure ()
        ["/skills", "reload"] -> pure ()
        _ ->
            Left
                ( "unsafe or unsupported session command: "
                    <> raw
                    <> "; use a typed meta action or a supported configuration command"
                )

validateMcpServer :: MetaMcpServer -> Either Text ()
validateMcpServer server = do
    validateConfigName "MCP server" server.metaMcpName
    let url = Text.strip <$> server.metaMcpUrl
        command = Text.strip <$> server.metaMcpCommand
        hasUrl = maybe False nonBlank url
        hasCommand = maybe False nonBlank command
    when (hasUrl == hasCommand) $
        Left "MCP server must configure exactly one of url or command"
    when (hasUrl && maybe False (not . validHttpUrl) url) $
        Left "MCP URL must begin with http:// or https://"
    when (hasUrl && (not (null server.metaMcpArgs) || isJust server.metaMcpCwd)) $
        Left "remote MCP servers cannot configure command args or cwd"
    when (server.metaMcpStartupTimeoutSeconds < 1) $
        Left "MCP startup timeout must be positive"
    when (server.metaMcpRequestTimeoutSeconds < 1) $
        Left "MCP request timeout must be positive"
    mapM_ (requireNonBlank "MCP argument") server.metaMcpArgs
    case server.metaMcpCwd of
        Nothing -> pure ()
        Just cwd -> requireNonBlank "MCP cwd" cwd

validateWebFetchUpdate :: MetaWebFetchUpdate -> Either Text ()
validateWebFetchUpdate update = do
    unless
        (or
            [ isJust update.metaWebFetchEnabled
            , isJust update.metaWebFetchAllowedDomains
            , isJust update.metaWebFetchTimeoutSeconds
            , isJust update.metaWebFetchMaxContentBytes
            , isJust update.metaWebFetchMaxInlineBytes
            ])
        (Left "set_web_fetch must change at least one field")
    mapM_ (mapM_ (requireNonBlank "web-fetch domain"))
        update.metaWebFetchAllowedDomains
    validateRange "web-fetch timeout" 1 300
        update.metaWebFetchTimeoutSeconds
    validateRange "web-fetch max content bytes" 1 (50 * 1024 * 1024)
        update.metaWebFetchMaxContentBytes
    validateRange "web-fetch max inline bytes" 1 (1024 * 1024)
        update.metaWebFetchMaxInlineBytes
    case
        ( update.metaWebFetchMaxInlineBytes
        , update.metaWebFetchMaxContentBytes
        ) of
        (Just inlineBytes, Just contentBytes)
            | inlineBytes > contentBytes ->
                Left "web-fetch max inline bytes must not exceed max content bytes"
        _ -> pure ()

validateLspServer :: MetaLspServer -> Either Text ()
validateLspServer server = do
    validateConfigName "LSP server" server.metaLspName
    requireNonBlank "LSP command" server.metaLspCommand
    when (Map.null server.metaLspExtensionToLanguage) $
        Left "LSP extensionToLanguage must not be empty"
    mapM_ (requireNonBlank "LSP extension")
        (Map.keys server.metaLspExtensionToLanguage)
    mapM_ (requireNonBlank "LSP language id")
        (Map.elems server.metaLspExtensionToLanguage)
    mapM_ (requireNonBlank "LSP argument") server.metaLspArgs
    case server.metaLspWorkspaceFolder of
        Nothing -> pure ()
        Just folder -> requireNonBlank "LSP workspace folder" folder
    when (server.metaLspStartupTimeoutMilliseconds < 1) $
        Left "LSP startup timeout must be positive"
    when (server.metaLspShutdownTimeoutMilliseconds < 1) $
        Left "LSP shutdown timeout must be positive"

validateRange :: Text -> Int -> Int -> Maybe Int -> Either Text ()
validateRange _ _ _ Nothing = pure ()
validateRange label minimumValue maximumValue (Just value)
    | value < minimumValue =
        Left (label <> " must be at least " <> Text.pack (show minimumValue))
    | value > maximumValue =
        Left (label <> " must not exceed " <> Text.pack (show maximumValue))
    | otherwise = pure ()

validateConfigName :: Text -> Text -> Either Text ()
validateConfigName label name = do
    requireNonBlank (label <> " name") name
    when (Text.length name > 80) $
        Left (label <> " name must be at most 80 characters")
    unless (Text.all validNameCharacter name) $
        Left
            (label
                <> " name may contain only letters, digits, dot, underscore, and dash")
  where
    validNameCharacter character =
        isAlphaNum character || character `elem` (".-_" :: String)

requireNonBlank :: Text -> Text -> Either Text ()
requireNonBlank label value =
    when (not (nonBlank value)) (Left (label <> " must not be empty"))

nonBlank :: Text -> Bool
nonBlank = not . Text.null . Text.strip

validHttpUrl :: Text -> Bool
validHttpUrl value =
    let lowered = Text.toLower value
    in "https://" `Text.isPrefixOf` lowered
        || "http://" `Text.isPrefixOf` lowered

validateClarification :: [MetaAction] -> Either Text ()
validateClarification actions =
    when (any isClarification actions && length actions /= 1) $
        Left "a clarification must be the plan's only action"
  where
    isClarification MetaClarify{} = True
    isClarification _ = False

validateSingletonActions :: [MetaAction] -> Either Text ()
validateSingletonActions actions =
    mapM_ requireAtMostOne
        [ ("MCP initialization strategy", count isMcpInit)
        , ("web-fetch update", count isWebFetch)
        , ("LSP enabled update", count isLspEnabled)
        , ("maximum concurrent agents", count isMaxAgents)
        ]
  where
    count predicate = length (filter predicate actions)
    requireAtMostOne (label, amount) =
        when (amount > 1) (Left ("plan contains conflicting " <> label <> " actions"))
    isMcpInit MetaSetMcpInitStrategy{} = True
    isMcpInit _ = False
    isWebFetch MetaSetWebFetch{} = True
    isWebFetch _ = False
    isLspEnabled MetaSetLspEnabled{} = True
    isLspEnabled _ = False
    isMaxAgents MetaSetMaxConcurrentAgents{} = True
    isMaxAgents _ = False

validateMcpConflicts :: [MetaAction] -> Either Text ()
validateMcpConflicts actions =
    mapM_ validateName (nub (foldMap mcpTarget actions))
  where
    mcpTarget = \case
        MetaUpsertMcp server -> [server.metaMcpName]
        MetaRemoveMcp name -> [name]
        MetaSetMcpEnabled name _ -> [name]
        MetaLoginMcpOAuth name -> [name]
        _ -> []
    validateName name = do
        let targeting = filter ((name `elem`) . mcpTarget) actions
            removes = length (filter isRemove targeting)
            upserts = length (filter isUpsert targeting)
            enables = length (filter isEnable targeting)
            oauths = length (filter isOAuth targeting)
        when (removes > 0 && length targeting > 1) $
            Left ("MCP server " <> name <> " is removed and also modified")
        when (upserts > 1 || enables > 1 || oauths > 1) $
            Left ("MCP server " <> name <> " has duplicate actions")
    isRemove MetaRemoveMcp{} = True
    isRemove _ = False
    isUpsert MetaUpsertMcp{} = True
    isUpsert _ = False
    isEnable MetaSetMcpEnabled{} = True
    isEnable _ = False
    isOAuth MetaLoginMcpOAuth{} = True
    isOAuth _ = False

validateLspConflicts :: [MetaAction] -> Either Text ()
validateLspConflicts actions =
    mapM_ validateName (nub (foldMap lspTarget actions))
  where
    lspTarget = \case
        MetaUpsertLsp server -> [server.metaLspName]
        MetaRemoveLsp name -> [name]
        _ -> []
    validateName name =
        when (length (filter ((name `elem`) . lspTarget) actions) > 1) $
            Left ("LSP server " <> name <> " has conflicting actions")

-- | Human-readable, secret-free action preview for approval prompts.
metaActionPreview :: MetaAction -> Text
metaActionPreview = \case
    MetaSessionCommand command -> "Run session command " <> quote command
    MetaConnectAccount provider ->
        "Connect a " <> providerDisplayName provider <> " account"
    MetaUpsertMcp server ->
        "Add or update MCP server "
            <> quote server.metaMcpName
            <> " ("
            <> maybe
                ("command " <> maybe "<missing>" quote server.metaMcpCommand)
                (\url -> "remote " <> quote url)
                server.metaMcpUrl
            <> ")"
    MetaRemoveMcp name -> "Remove MCP server " <> quote name
    MetaSetMcpEnabled name enabled ->
        (if enabled then "Enable" else "Disable")
            <> " MCP server " <> quote name
    MetaLoginMcpOAuth name ->
        "Connect OAuth for MCP server " <> quote name
    MetaSetMcpInitStrategy strategy ->
        "Set MCP initialization strategy to " <> mcpInitStrategyText strategy
    MetaSetWebFetch update ->
        "Update web-fetch policy (" <> Text.intercalate ", " (webFetchChanges update) <> ")"
    MetaSetLspEnabled enabled ->
        (if enabled then "Enable" else "Disable") <> " LSP integration"
    MetaUpsertLsp server ->
        "Add or update LSP server " <> quote server.metaLspName
    MetaRemoveLsp name -> "Remove LSP server " <> quote name
    MetaSetMaxConcurrentAgents limit ->
        "Set maximum concurrent agents to "
            <> maybe "the default" (Text.pack . show) limit
    MetaClarify question -> "Ask for clarification: " <> question
    MetaInform message -> message

metaPlanPreviews :: MetaPlan -> [Text]
metaPlanPreviews = map metaActionPreview . (.metaActions)

metaPlanMutates :: MetaPlan -> Bool
metaPlanMutates = any actionMutates . (.metaActions)
  where
    actionMutates MetaClarify{} = False
    actionMutates MetaInform{} = False
    actionMutates _ = True

webFetchChanges :: MetaWebFetchUpdate -> [Text]
webFetchChanges update =
    concat
        [ maybe [] (\value -> [if value then "enabled" else "disabled"])
            update.metaWebFetchEnabled
        , maybe [] (const ["allowed domains"]) update.metaWebFetchAllowedDomains
        , maybe [] (const ["timeout"]) update.metaWebFetchTimeoutSeconds
        , maybe [] (const ["content limit"]) update.metaWebFetchMaxContentBytes
        , maybe [] (const ["inline limit"]) update.metaWebFetchMaxInlineBytes
        ]

providerDisplayName :: Provider -> Text
providerDisplayName XAIProvider = "Grok (xAI)"
providerDisplayName provider = providerSlug provider

mcpInitStrategyText :: McpInitStrategy -> Text
mcpInitStrategyText = \case
    McpInitAuto -> "auto"
    McpInitProgressive -> "progressive"
    McpInitBlocking -> "blocking"

quote :: Text -> Text
quote value = "'" <> value <> "'"

-- | Recursively redact values under secret-bearing keys before config context
-- crosses the model boundary.
redactMetaContext :: Aeson.Value -> Aeson.Value
redactMetaContext = go
  where
    go = \case
        Aeson.Object object ->
            Aeson.Object (KeyMap.mapWithKey redactField object)
        Aeson.Array values -> Aeson.Array (fmap go values)
        value -> value
    redactField key value
        | secretKey (Key.toText key) = Aeson.String "<redacted>"
        | otherwise = go value

secretKey :: Text -> Bool
secretKey key =
    let normalized =
            Text.filter isAlphaNum (Text.toLower key)
    in normalized == "env"
        || "token" `Text.isInfixOf` normalized
        || "secret" `Text.isInfixOf` normalized
        || "password" `Text.isInfixOf` normalized
        || "credential" `Text.isInfixOf` normalized
        || normalized == "apikey"

metaConsoleInstructions :: Text
metaConsoleInstructions =
    Text.unlines
        [ "You are the configuration planner for a coding-agent harness."
        , "Return exactly one JSON object matching the schema in the user message."
        , "Do not use Markdown except that a single ```json fence is accepted."
        , "Do not continue the coding task, request tools, or invent credentials."
        , "Never copy a token, secret, password, environment value, or OAuth credential into the output."
        , "Use clarify when required information is missing. Use inform for a no-op explanation."
        , "Prefer typed actions over session_command; session_command is restricted to the listed configuration commands."
        ]

-- | Build the single private planner request.  Context is redacted again here
-- so callers cannot accidentally pass config secrets through unchanged.
metaConsolePrompt :: Aeson.Value -> Text -> Text
metaConsolePrompt context request =
    Text.unlines
        [ "Meta Console request:"
        , request
        , ""
        , "Current configuration context (secret-bearing values redacted):"
        , renderJson (redactMetaContext context)
        , ""
        , "Output schema:"
        , "{"
        , "  \"summary\": \"short description\","
        , "  \"actions\": ["
        , "    {\"type\":\"session_command\",\"command\":\"/model MODEL\"},"
        , "    {\"type\":\"connect_account\",\"provider\":\"openai|grok|xai|openrouter|claude\"},"
        , "    {\"type\":\"mcp_upsert\",\"name\":\"NAME\",\"enabled\":true,\"url\":\"https://...\"|null,\"command\":\"PROGRAM\"|null,\"args\":[],\"cwd\":null,\"startupTimeoutSeconds\":30,\"requestTimeoutSeconds\":60,\"protocol\":\"auto|modern|legacy\"},"
        , "    {\"type\":\"mcp_remove\",\"name\":\"NAME\"},"
        , "    {\"type\":\"mcp_set_enabled\",\"name\":\"NAME\",\"enabled\":true},"
        , "    {\"type\":\"mcp_oauth_login\",\"name\":\"NAME\"},"
        , "    {\"type\":\"set_mcp_init_strategy\",\"strategy\":\"auto|progressive|blocking\"},"
        , "    {\"type\":\"set_web_fetch\",\"enabled\":true,\"allowedDomains\":[\"example.com\"],\"timeoutSeconds\":60,\"maxContentBytes\":10485760,\"maxInlineBytes\":100000},"
        , "    {\"type\":\"set_lsp_enabled\",\"enabled\":true},"
        , "    {\"type\":\"lsp_upsert\",\"name\":\"haskell\",\"command\":\"haskell-language-server-wrapper\",\"args\":[\"--lsp\"],\"extensionToLanguage\":{\"hs\":\"haskell\"},\"workspaceFolder\":null,\"startupTimeoutMilliseconds\":15000,\"shutdownTimeoutMilliseconds\":5000},"
        , "    {\"type\":\"lsp_remove\",\"name\":\"NAME\"},"
        , "    {\"type\":\"set_max_concurrent_agents\",\"limit\":4},"
        , "    {\"type\":\"clarify\",\"question\":\"...\"},"
        , "    {\"type\":\"inform\",\"message\":\"...\"}"
        , "  ]"
        , "}"
        , ""
        , "Omit optional fields rather than guessing. Never emit env, clientSecret, token, password, apiKey, or credential fields."
        , "Allowed session_command forms: /model MODEL, /effort LEVEL, /fast, /shell MODE, /codemod, /always-approve, /agents limit N, /skills reload."
        ]

renderJson :: Aeson.Value -> Text
renderJson =
    TextEncoding.decodeUtf8With TextEncodingError.lenientDecode
        . LBS.toStrict
        . Aeson.encode

-- | Run the private planner request.  It always starts from an empty backend
-- state, strips tools and continuation identifiers, disables persistence, and
-- performs at most one format-repair request.
runMetaConsoleWithCancel
    :: (CancelFlag
        -> IO (Either MetaError MetaPlan)
        -> IO (Either MetaError MetaPlan))
    -> BtwBackendFactory
    -> IORef ResponseCreateParams
    -> Aeson.Value
    -> Text
    -> IO (Either MetaError MetaPlan)
runMetaConsoleWithCancel withCancelScope makeBackend paramsRef context request = do
    params <- privateMetaParams <$> readIORef paramsRef
    cancel <- newCancelFlag
    let Backend submit = makeBackend params
        initialPrompt = metaConsolePrompt context request
        submitPrompt prompt =
            submit [] Nothing [UserMessage prompt] (\_ -> pure ())
        action = do
            result <- race (waitCancel cancel) do
                first <- submitPrompt initialPrompt
                case classifyTurn first of
                    Left err -> pure (Left err)
                    Right response ->
                        case decodeMetaPlan response of
                            Right plan -> pure (Right plan)
                            Left decodeError -> do
                                repaired <- submitPrompt
                                    (repairPrompt
                                        initialPrompt decodeError response)
                                pure do
                                    repairedText <- classifyTurn repaired
                                    either
                                        (Left . MetaInvalidPlan)
                                        Right
                                        (decodeMetaPlan repairedText)
            pure case result of
                Left () -> Left MetaCancelled
                Right outcome -> outcome
    withCancelScope cancel action

privateMetaParams :: ResponseCreateParams -> ResponseCreateParams
privateMetaParams ResponseCreateParams{..} =
    ResponseCreateParams
        { background = Just False
        , conversation = Nothing
        , input = Nothing
        , instructions = Just metaConsoleInstructions
        , maxToolCalls = Nothing
        , metadata = Nothing
        , parallelToolCalls = Just False
        , previousResponseId = Nothing
        , prompt = Nothing
        , store = Just False
        , text = Nothing
        , toolChoice = Just (ToolChoiceMode ToolChoiceNone)
        , tools = Nothing
        , ..
        }

classifyTurn :: Either ApiError BackendResult -> Either MetaError Text
classifyTurn result = do
    output <- either (Left . MetaTransport) (Right . (.backendOutput)) result
    classifyOutput output

classifyOutput :: TurnOutput -> Either MetaError Text
classifyOutput turn
    | Text.null turn.responseId = Left MetaInvalidResponse
    | not (null turn.toolCalls) = Left MetaUnexpectedToolCall
    | otherwise = case turn.assistantText of
        Just text | nonBlank text -> Right text
        _ -> Left MetaEmptyResponse

repairPrompt :: Text -> Text -> Text -> Text
repairPrompt originalPrompt decodeError response =
    Text.unlines
        [ originalPrompt
        , ""
        , "Your prior response was not a valid Meta Console plan."
        , "Return only a corrected JSON object. Do not add tools or prose."
        , "Validation error: " <> decodeError
        , ""
        , "Prior response:"
        , response
        ]

formatMetaError :: MetaError -> Text
formatMetaError = \case
    MetaTransport err ->
        "meta console failed: " <> formatApiErrorInline err
    MetaCancelled -> "meta console cancelled"
    MetaEmptyResponse -> "meta console returned an empty response"
    MetaUnexpectedToolCall ->
        "meta console attempted a tool call; no tools were run"
    MetaInvalidResponse -> "meta console returned an invalid response"
    MetaInvalidPlan err -> "meta console returned an invalid plan: " <> err
