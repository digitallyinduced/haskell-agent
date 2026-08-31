-- | Canonical model aliases exposed by the Digitally Induced LLM gateway.
--
-- Gateway credentials are process-wide, so the active catalog is deliberately
-- exclusive: direct provider models disappear while connected and the same
-- model ids are attached to a dedicated gateway connection. Legacy router
-- aliases are always removed from the visible catalog.
module Agent.CLI.GatewayModels
    ( catalogForGatewayState
    , catalogUsesGateway
    , gatewayConnectionId
    , gatewayDefaultModelId
    , gatewayModelIds
    , isGatewayConnectionId
    , isLegacyGatewayModelId
    , loadGatewayModelCatalogAt
    ) where

import Agent.CLI.GatewayClient (loadGatewayCredentialAt)
import Agent.CLI.ModelConfig
    ( CatalogModel(..)
    , ConnectionKind(..)
    , ModelCatalog(..)
    , ModelConnection(..)
    , loadModelCatalogAt
    )
import Agent.Dialect (DialectId (CodexDialect))
import Agent.Provider (Provider (OpenAIProvider))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import System.OsPath (OsPath)

gatewayConnectionId :: Text
gatewayConnectionId = "gateway"

gatewayDefaultModelId :: Text
gatewayDefaultModelId = "gpt-5.6-sol"

gatewayModelIds :: [Text]
gatewayModelIds =
    [ gatewayDefaultModelId
    , "gpt-5.6-terra"
    , "gpt-5.6-luna"
    ]

isGatewayConnectionId :: Text -> Bool
isGatewayConnectionId = (== gatewayConnectionId)

isLegacyGatewayModelId :: Text -> Bool
isLegacyGatewayModelId modelId =
    modelId `elem` ["router-default", "router-codex", "router-grok"]

catalogUsesGateway :: ModelCatalog -> Bool
catalogUsesGateway catalog =
    Map.lookup gatewayConnectionId catalog.catalogConnections
        == Just gatewayConnection
        && not (null catalog.catalogModels)
        && all
            (isGatewayConnectionId . (.catalogModelConnectionId))
            catalog.catalogModels

catalogForGatewayState :: Bool -> ModelCatalog -> ModelCatalog
catalogForGatewayState connected catalog
    | connected =
        catalog
            { catalogConnections =
                Map.singleton gatewayConnectionId gatewayConnection
            , catalogModels = canonicalGatewayModels
            }
    | otherwise =
        catalog
            { catalogModels =
                filter
                    (not . isLegacyGatewayModelId . (.catalogModelId))
                    catalog.catalogModels
            }

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
                Right credential ->
                    pure
                        (Right
                            (catalogForGatewayState
                                (maybe False (const True) credential)
                                catalog))

canonicalGatewayModels :: [CatalogModel]
canonicalGatewayModels =
    [ gatewayModel gatewayDefaultModelId "Gateway · Frontier" True
    , gatewayModel "gpt-5.6-terra" "Gateway · Balanced" False
    , gatewayModel "gpt-5.6-luna" "Gateway · Fast · Low cost" False
    ]

gatewayConnection :: ModelConnection
gatewayConnection =
    ModelConnection
        { connectionId = gatewayConnectionId
        , connectionKind = BuiltinConnection OpenAIProvider
        }

gatewayModel :: Text -> Text -> Bool -> CatalogModel
gatewayModel modelId label isDefault =
    CatalogModel
        { catalogModelId = modelId
        , catalogModelConnectionId = gatewayConnectionId
        , catalogModelWireId = modelId
        , catalogModelDialect = CodexDialect
        , catalogModelContextWindow = Nothing
        , catalogModelLabel = Just label
        , catalogModelReasoningEfforts =
            Just ["low", "medium", "high", "xhigh", "max"]
        , catalogModelDefaultReasoningEffort = Just "medium"
        , catalogModelDefault = isDefault
        , catalogModelFallbackPriority = Nothing
        }
