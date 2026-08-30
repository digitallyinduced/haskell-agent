-- | Host-side execution helpers for the private Meta Console planner.
--
-- The planner can only describe changes.  This module is the typed boundary
-- which turns approved actions into a validated harness configuration; secret
-- values are supplied separately by trusted UI prompts and are never part of
-- the model-produced action language.
module Agent.CLI.Runtime.MetaConsole
    ( MetaSecretValue(..)
    , applyMetaConfigActions
    , buildMetaContext
    , isMetaConfigAction
    , metaConfigRequiresRestart
    , runMetaPlanner
    ) where

import Agent.Cancel (CancelFlag)
import Agent.CLI.CancelWatch (withEscCancel)
import Agent.CLI.Command
    ( ShellMode(..)
    , currentEffort
    , currentModel
    )
import Agent.CLI.Config
    ( HarnessConfig(..)
    , LspConfig(..)
    , LspServerConfig(..)
    , McpOAuthConfig(..)
    , McpServerConfig(..)
    , WebFetchConfig(..)
    )
import Agent.CLI.Interrupt (withTurnCancel)
import Agent.CLI.MetaConsole
    ( MetaAction(..)
    , MetaError
    , MetaLspServer(..)
    , MetaMcpServer(..)
    , MetaPlan
    , MetaWebFetchUpdate(..)
    , redactMetaContext
    , runMetaConsoleWithCancel
    )
import Agent.CLI.ModelConfig
    ( CatalogModel(..)
    , ModelCatalog(..)
    )
import Agent.CLI.Options (ApprovalPolicy(..))
import Agent.CLI.SessionEnv (SessionEnv(..))
import Agent.Provider (providerSlug)
import Agent.ReasoningEffort (reasoningEffortText)
import Agent.Responses.Types (ResponseCreateParams(..))
import Control.Applicative ((<|>))
import Control.Monad (foldM)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import Data.IORef (readIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

-- | A secret collected by trusted host UI after plan approval.
data MetaSecretValue
    = MetaMcpSecretValue !Text !Text !Text
    | MetaLspSecretValue !Text !Text !Text
    deriving (Eq)

-- Deliberately redact the value if this type ever reaches a diagnostic.
instance Show MetaSecretValue where
    show (MetaMcpSecretValue server key _) =
        "MetaMcpSecretValue "
            <> show server
            <> " "
            <> show key
            <> " <redacted>"
    show (MetaLspSecretValue server key _) =
        "MetaLspSecretValue "
            <> show server
            <> " "
            <> show key
            <> " <redacted>"

-- | Apply all persistent actions to one in-memory value.  The caller saves
-- the result once, so a bad action cannot leave a partially-written plan.
applyMetaConfigActions
    :: [MetaSecretValue]
    -> [MetaAction]
    -> HarnessConfig
    -> Either Text HarnessConfig
applyMetaConfigActions secrets actions initial =
    foldM (applyMetaConfigAction secrets) initial actions

applyMetaConfigAction
    :: [MetaSecretValue]
    -> HarnessConfig
    -> MetaAction
    -> Either Text HarnessConfig
applyMetaConfigAction secrets config = \case
    MetaUpsertMcp proposed -> do
        let name = proposed.metaMcpName
            existing = Map.lookup name config.configMcpServers
        remote <- resolveMetaMcpUrl name existing proposed.metaMcpUrl
        let
            oauth =
                case remote of
                    Nothing -> Nothing
                    Just _ ->
                        case proposed.metaMcpOAuthScopes of
                            Nothing -> existing >>= (.mcpOAuth)
                            Just scopes ->
                                Just $
                                    maybe
                                        McpOAuthConfig
                                            { mcpOAuthClientId = Nothing
                                            , mcpOAuthClientSecret = Nothing
                                            , mcpOAuthClientIdMetadataUrl = Nothing
                                            , mcpOAuthScopes = scopes
                                            }
                                        (\old -> old { mcpOAuthScopes = scopes })
                                        (existing >>= (.mcpOAuth))
            next = McpServerConfig
                { mcpEnabled = proposed.metaMcpEnabled
                , mcpUrl = remote
                , mcpCommand =
                    maybe "" Text.strip proposed.metaMcpCommand
                , mcpArgs =
                    case remote of
                        Just _ -> []
                        Nothing
                            | null proposed.metaMcpArgs ->
                                maybe [] (.mcpArgs) existing
                            | otherwise -> proposed.metaMcpArgs
                , mcpCwd =
                    case remote of
                        Just _ -> Nothing
                        Nothing ->
                            proposed.metaMcpCwd
                                <|> (existing >>= (.mcpCwd))
                , mcpEnv = maybe Map.empty (.mcpEnv) existing
                , mcpStartupTimeoutSeconds =
                    proposed.metaMcpStartupTimeoutSeconds
                , mcpRequestTimeoutSeconds =
                    proposed.metaMcpRequestTimeoutSeconds
                , mcpOAuth = oauth
                , mcpProtocol = proposed.metaMcpProtocol
                }
        Right config
            { configMcpServers =
                Map.insert name next config.configMcpServers
            }
    MetaRemoveMcp name -> do
        requireMember "MCP server" name config.configMcpServers
        Right config
            { configMcpServers =
                Map.delete name config.configMcpServers
            }
    MetaSetMcpEnabled name enabled -> do
        server <- requireLookup "MCP server" name config.configMcpServers
        Right config
            { configMcpServers =
                Map.insert
                    name
                    (server { mcpEnabled = enabled })
                    config.configMcpServers
            }
    MetaSetMcpSecretEnv name key -> do
        server <- requireLookup "MCP server" name config.configMcpServers
        value <- requireMcpSecret name key secrets
        Right config
            { configMcpServers =
                Map.insert
                    name
                    (server { mcpEnv = Map.insert key value server.mcpEnv })
                    config.configMcpServers
            }
    MetaSetMcpInitStrategy strategy ->
        Right config { configMcpInitStrategy = strategy }
    MetaSetWebFetch update ->
        let current = config.configWebFetch
            next = current
                { webFetchEnabled =
                    fromMaybe current.webFetchEnabled
                        update.metaWebFetchEnabled
                , webFetchAllowedDomains =
                    fromMaybe current.webFetchAllowedDomains
                        update.metaWebFetchAllowedDomains
                , webFetchTimeoutSeconds =
                    fromMaybe current.webFetchTimeoutSeconds
                        update.metaWebFetchTimeoutSeconds
                , webFetchMaxContentBytes =
                    fromMaybe current.webFetchMaxContentBytes
                        update.metaWebFetchMaxContentBytes
                , webFetchMaxInlineBytes =
                    fromMaybe current.webFetchMaxInlineBytes
                        update.metaWebFetchMaxInlineBytes
                }
        in Right config { configWebFetch = next }
    MetaSetLspEnabled enabled ->
        Right config
            { configLsp = config.configLsp { lspEnabled = enabled }
            }
    MetaUpsertLsp proposed ->
        let name = proposed.metaLspName
            existing =
                Map.lookup name config.configLsp.lspServers
            next = LspServerConfig
                { lspCommand = proposed.metaLspCommand
                , lspArgs =
                    if null proposed.metaLspArgs
                        then maybe [] (.lspArgs) existing
                        else proposed.metaLspArgs
                , lspEnv = maybe Map.empty (.lspEnv) existing
                , lspExtensionToLanguage =
                    proposed.metaLspExtensionToLanguage
                , lspInitializationOptions =
                    existing >>= (.lspInitializationOptions)
                , lspSettings = existing >>= (.lspSettings)
                , lspWorkspaceFolder =
                    proposed.metaLspWorkspaceFolder
                        <|> (existing >>= (.lspWorkspaceFolder))
                , lspStartupTimeoutMilliseconds =
                    proposed.metaLspStartupTimeoutMilliseconds
                , lspShutdownTimeoutMilliseconds =
                    proposed.metaLspShutdownTimeoutMilliseconds
                }
            lsp = config.configLsp
        in Right config
            { configLsp =
                lsp
                    { lspServers =
                        Map.insert name next lsp.lspServers
                    }
            }
    MetaRemoveLsp name -> do
        requireMember "LSP server" name config.configLsp.lspServers
        let lsp = config.configLsp
        Right config
            { configLsp =
                lsp { lspServers = Map.delete name lsp.lspServers }
            }
    MetaSetLspSecretEnv name key -> do
        server <-
            requireLookup "LSP server" name config.configLsp.lspServers
        value <- requireLspSecret name key secrets
        let lsp = config.configLsp
        Right config
            { configLsp =
                lsp
                    { lspServers =
                        Map.insert
                            name
                            (server
                                { lspEnv =
                                    Map.insert key value server.lspEnv
                                })
                            lsp.lspServers
                    }
            }
    MetaSetMaxConcurrentAgents limit ->
        Right config { configMaxConcurrentAgents = limit }
    MetaSessionCommand{} -> Right config
    MetaConnectAccount{} -> Right config
    MetaSelectAccount{} -> Right config
    MetaLoginMcpOAuth{} -> Right config
    MetaClarify{} -> Right config
    MetaInform{} -> Right config

requireLookup :: Text -> Text -> Map.Map Text value -> Either Text value
requireLookup kind name values =
    maybe
        (Left (kind <> " '" <> name <> "' is not configured"))
        Right
        (Map.lookup name values)

requireMember :: Text -> Text -> Map.Map Text value -> Either Text ()
requireMember kind name values =
    () <$ requireLookup kind name values

resolveMetaMcpUrl
    :: Text
    -> Maybe McpServerConfig
    -> Maybe Text
    -> Either Text (Maybe Text)
resolveMetaMcpUrl name existing proposed =
    case proposed of
        Just redacted
            | "<redacted>" `Text.isInfixOf` redacted ->
                case existing >>= (.mcpUrl) of
                    Just original -> Right (Just original)
                    Nothing ->
                        Left
                            ("MCP server '"
                                <> name
                                <> "' contains a redacted URL but has no existing URL to preserve")
        _ -> Right proposed

requireMcpSecret
    :: Text
    -> Text
    -> [MetaSecretValue]
    -> Either Text Text
requireMcpSecret server key secrets =
    maybe
        (Left ("secret input for MCP server '" <> server <> "' was cancelled"))
        Right
        (listToMaybe
            [ value
            | MetaMcpSecretValue target targetKey value <- secrets
            , target == server
            , targetKey == key
            ])

requireLspSecret
    :: Text
    -> Text
    -> [MetaSecretValue]
    -> Either Text Text
requireLspSecret server key secrets =
    maybe
        (Left ("secret input for LSP server '" <> server <> "' was cancelled"))
        Right
        (listToMaybe
            [ value
            | MetaLspSecretValue target targetKey value <- secrets
            , target == server
            , targetKey == key
            ])

isMetaConfigAction :: MetaAction -> Bool
isMetaConfigAction = \case
    MetaUpsertMcp{} -> True
    MetaRemoveMcp{} -> True
    MetaSetMcpEnabled{} -> True
    MetaSetMcpSecretEnv{} -> True
    MetaSetMcpInitStrategy{} -> True
    MetaSetWebFetch{} -> True
    MetaSetLspEnabled{} -> True
    MetaUpsertLsp{} -> True
    MetaRemoveLsp{} -> True
    MetaSetLspSecretEnv{} -> True
    MetaSetMaxConcurrentAgents{} -> True
    _ -> False

metaConfigRequiresRestart :: [MetaAction] -> Bool
metaConfigRequiresRestart = any isMetaConfigAction

-- | Construct the only session context visible to the private planner.
-- Account credentials, transcript text, tool results, and environment values
-- are intentionally absent.
buildMetaContext :: SessionEnv -> HarnessConfig -> IO Aeson.Value
buildMetaContext env config = do
    params <- readIORef env.sessionParams
    policy <- readIORef env.sessionPolicy
    shellMode <- env.sessionShellMode
    pure $
        Aeson.object
            [ "session" .= Aeson.object
                [ "provider" .= providerSlug env.sessionProvider
                , "connection" .= env.sessionConnection
                , "model" .= currentModel params
                , "effort" .= reasoningEffortText (currentEffort params)
                , "fastMode" .= (params.serviceTier == Just "priority")
                , "shellTools" .= shellModeText shellMode
                , "approvalPolicy" .= approvalPolicyText policy
                ]
            , "availableModels" .=
                [ Aeson.object
                    [ "id" .= model.catalogModelId
                    , "connection" .= model.catalogModelConnectionId
                    ]
                | model <- env.sessionModelCatalog.catalogModels
                ]
            , "harness" .= redactMetaContext (Aeson.toJSON config)
            ]

runMetaPlanner
    :: SessionEnv
    -> Aeson.Value
    -> Text
    -> IO (Either MetaError MetaPlan)
runMetaPlanner env context request =
    runMetaConsoleWithCancel
        (metaCancelScope env)
        env.sessionBtwBackend
        env.sessionParams
        context
        request

metaCancelScope
    :: SessionEnv
    -> CancelFlag
    -> IO (Either MetaError MetaPlan)
    -> IO (Either MetaError MetaPlan)
metaCancelScope env cancel action =
    withTurnCancel env.sessionInterrupt cancel $
        case env.sessionFullscreen of
            Nothing
                | not env.sessionBackground ->
                    withEscCancel cancel env.sessionEscPaused action
            _ -> action

shellModeText :: ShellMode -> Text
shellModeText = \case
    ShellGhci -> "ghci"
    ShellBash -> "bash"
    ShellBoth -> "both"
    ShellNone -> "none"

approvalPolicyText :: ApprovalPolicy -> Text
approvalPolicyText = \case
    ApproveAll -> "always-approve"
    PromptMutating -> "prompt"
    DenyMutating -> "deny-mutations"
