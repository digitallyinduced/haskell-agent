-- | Canonical model aliases exposed by the Digitally Induced LLM gateway.
--
-- Gateway credentials are process-wide, so the active catalog is deliberately
-- exclusive: direct provider models disappear while connected and gateway
-- aliases disappear while disconnected. This prevents a direct model id from
-- being sent to the gateway (or a router alias from reaching a provider).
module Agent.CLI.GatewayModels
    ( catalogForGatewayState
    , catalogUsesGateway
    , gatewayDefaultModelId
    , gatewayModelIds
    , isGatewayModelId
    , loadGatewayModelCatalogAt
    ) where

import Agent.CLI.GatewayClient (loadGatewayCredentialAt)
import Agent.CLI.ModelConfig
    ( CatalogModel(..)
    , ModelCatalog(..)
    , builtinConnectionId
    , loadModelCatalogAt
    )
import Agent.Dialect (DialectId (CodexDialect))
import Agent.Provider (Provider (OpenAIProvider))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import System.OsPath (OsPath)

gatewayDefaultModelId :: Text
gatewayDefaultModelId = "router-default"

gatewayModelIds :: [Text]
gatewayModelIds =
    [ gatewayDefaultModelId
    , "router-codex"
    , "router-grok"
    ]

isGatewayModelId :: Text -> Bool
isGatewayModelId modelId = modelId `elem` gatewayModelIds

catalogUsesGateway :: ModelCatalog -> Bool
catalogUsesGateway catalog =
    not (null catalog.catalogModels)
        && all (isGatewayModelId . (.catalogModelId)) catalog.catalogModels

catalogForGatewayState :: Bool -> ModelCatalog -> ModelCatalog
catalogForGatewayState connected catalog
    | connected = withModels canonicalGatewayModels
    | otherwise =
        withModels $
            filter
                (not . isGatewayModelId . (.catalogModelId))
                catalog.catalogModels
  where
    withModels models =
        catalog
            { catalogModels = models
            , catalogModelsById =
                Map.fromList
                    [ (model.catalogModelId, model)
                    | model <- models
                    ]
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
    [ gatewayModel gatewayDefaultModelId "Gateway · Default" True
    , gatewayModel "router-codex" "Gateway · Codex" False
    , gatewayModel "router-grok" "Gateway · Grok" False
    ]

gatewayModel :: Text -> Text -> Bool -> CatalogModel
gatewayModel modelId label isDefault =
    CatalogModel
        { catalogModelId = modelId
        , catalogModelConnectionId = builtinConnectionId OpenAIProvider
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
