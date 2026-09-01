-- | Authoritative model aliases exposed by a connected organization gateway.
module Agent.CLI.GatewayModels
    ( catalogForGatewayState
    , catalogForGatewayModels
    , catalogUsesGateway
    , gatewayConnectionId
    , gatewayAnthropicConnectionId
    , gatewayDefaultModelId
    , gatewayModelIds
    , isGatewayConnectionId
    , isLegacyGatewayModelId
    , loadGatewayModelCatalogAt
    ) where

import Agent.CLI.GatewayClient
    ( GatewayModel(..)
    , GatewayModelCatalog(..)
    , GatewayModelProtocol(..)
    , fetchGatewayModelCatalog
    , loadGatewayCredentialAt
    )
import Agent.CLI.ModelConfig
    ( CatalogModel(..)
    , ConnectionKind(..)
    , ModelCatalog(..)
    , ModelConnection(..)
    , loadModelCatalogAt
    )
import Agent.Dialect
    ( DialectId (ClaudeCodeDialect, CodexDialect) )
import Agent.Provider
    ( Provider (ClaudeCodeProvider, OpenAIProvider) )
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import System.OsPath (OsPath)

gatewayConnectionId :: Text
gatewayConnectionId = "gateway"

gatewayAnthropicConnectionId :: Text
gatewayAnthropicConnectionId = "gateway-anthropic"

gatewayDefaultModelId :: Text
gatewayDefaultModelId = "gpt-5.6-sol"

-- Kept as the canonical defaults for callers/tests that need a disconnected
-- fixture. A connected catalog is always populated from discovery.
gatewayModelIds :: [Text]
gatewayModelIds =
    [ gatewayDefaultModelId
    , "gpt-5.6-terra"
    , "gpt-5.6-luna"
    ]

isGatewayConnectionId :: Text -> Bool
isGatewayConnectionId connectionId =
    connectionId == gatewayConnectionId
        || connectionId == gatewayAnthropicConnectionId

isLegacyGatewayModelId :: Text -> Bool
isLegacyGatewayModelId modelId =
    "router-" `Text.isPrefixOf` modelId
        || modelId == "traumimmo-translation"

catalogUsesGateway :: ModelCatalog -> Bool
catalogUsesGateway catalog =
    not (null catalog.catalogModels)
        && Map.lookup gatewayConnectionId catalog.catalogConnections
            == Just gatewayResponsesConnection
        && all
            (isGatewayConnectionId . (.catalogModelConnectionId))
            catalog.catalogModels

-- Compatibility helper used by pure tests. Runtime connection uses discovery.
catalogForGatewayState :: Bool -> ModelCatalog -> ModelCatalog
catalogForGatewayState connected catalog
    | connected =
        catalogForGatewayModels
            GatewayModelCatalog
                { gatewayModels =
                    fmap
                        (\modelId ->
                            GatewayModel modelId GatewayResponsesProtocol)
                        gatewayModelIds
                }
            catalog
    | otherwise = disconnectedCatalog catalog

catalogForGatewayModels
    :: GatewayModelCatalog
    -> ModelCatalog
    -> ModelCatalog
catalogForGatewayModels discovered catalog =
    catalog
        { catalogConnections =
            Map.fromList $
                (gatewayConnectionId, gatewayResponsesConnection)
                    : [ (gatewayAnthropicConnectionId, gatewayAnthropicConnection)
                      | not (null anthropicIds)
                      ]
        , catalogModels =
            zipWith
                (gatewayModel gatewayConnectionId CodexDialect)
                responseIds
                (defaultFlags responseIds anthropicIds)
                <> zipWith
                    (gatewayModel
                        gatewayAnthropicConnectionId
                        ClaudeCodeDialect)
                    anthropicIds
                    (drop (length responseIds)
                        (defaultFlags responseIds anthropicIds))
        }
  where
    responseIds =
        sanitize
            [ model.gatewayModelId
            | model <- discovered.gatewayModels
            , model.gatewayModelProtocol == GatewayResponsesProtocol
            ]
    anthropicIds =
        filter (`notElem` responseIds) $
            sanitize
                [ model.gatewayModelId
                | model <- discovered.gatewayModels
                , model.gatewayModelProtocol == GatewayAnthropicProtocol
                ]
    sanitize =
        nub
            . filter
                (\modelId ->
                    not (Text.null (Text.strip modelId))
                        && not (isLegacyGatewayModelId modelId))

defaultFlags :: [Text] -> [Text] -> [Bool]
defaultFlags responseIds anthropicIds =
    [ Just modelId == selected
    | modelId <- responseIds <> anthropicIds
    ]
  where
    selected
        | gatewayDefaultModelId `elem` responseIds =
            Just gatewayDefaultModelId
        | otherwise = listToMaybe (responseIds <> anthropicIds)

loadGatewayModelCatalogAt
    :: OsPath
    -> OsPath
    -> IO (Either Text ModelCatalog)
loadGatewayModelCatalogAt home cwd =
    loadModelCatalogAt home cwd >>= \case
        Left err -> pure (Left err)
        Right catalog ->
            loadGatewayCredentialAt home >>= \case
                Left err ->
                    pure (Left ("cannot load gateway credential: " <> err))
                Right Nothing ->
                    pure (Right (disconnectedCatalog catalog))
                Right (Just credential) ->
                    fetchGatewayModelCatalog credential >>= \case
                        Left err -> pure (Left err)
                        Right discovered ->
                            let active = catalogForGatewayModels discovered catalog
                             in if null active.catalogModels
                                    then
                                        pure $
                                            Left
                                                "The connected gateway returned an empty model catalog."
                                    else pure (Right active)

disconnectedCatalog :: ModelCatalog -> ModelCatalog
disconnectedCatalog catalog =
    catalog
        { catalogModels =
            filter
                (not . isLegacyGatewayModelId . (.catalogModelId))
                catalog.catalogModels
        }

gatewayResponsesConnection :: ModelConnection
gatewayResponsesConnection =
    ModelConnection
        { connectionId = gatewayConnectionId
        , connectionKind = BuiltinConnection OpenAIProvider
        }

gatewayAnthropicConnection :: ModelConnection
gatewayAnthropicConnection =
    ModelConnection
        { connectionId = gatewayAnthropicConnectionId
        , connectionKind = BuiltinConnection ClaudeCodeProvider
        }

gatewayModel
    :: Text
    -> DialectId
    -> Text
    -> Bool
    -> CatalogModel
gatewayModel connection dialect modelId isDefault =
    CatalogModel
        { catalogModelId = modelId
        , catalogModelConnectionId = connection
        , catalogModelWireId = modelId
        , catalogModelDialect = dialect
        , catalogModelContextWindow = Nothing
        , catalogModelLabel = Just "Gateway"
        , catalogModelReasoningEfforts =
            Just ["low", "medium", "high", "xhigh", "max"]
        , catalogModelDefaultReasoningEffort = Just "medium"
        , catalogModelDefault = isDefault
        , catalogModelFallbackPriority = Nothing
        }
