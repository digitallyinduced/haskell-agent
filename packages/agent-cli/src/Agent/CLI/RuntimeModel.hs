-- | Ephemeral model connections supplied by a trusted runtime host.
--
-- These models are deliberately separate from the persisted catalog: bearer
-- tokens stay in memory, and a host-provided connection cannot be redefined by
-- a user overlay.
module Agent.CLI.RuntimeModel
    ( RuntimeModelTransport(..)
    , RuntimeResponsesModel(..)
    , appleFoundationModelsConnectionId
    , appleFoundationModelsModelId
    , mkAppleFoundationModelsRuntime
    , applyRuntimeResponsesModel
    , runtimeResponsesModelMatches
    ) where

import Agent.CLI.ModelConfig
    ( CatalogModel(..)
    , ConnectionKind(..)
    , ModelCatalog(..)
    , ModelConnection(..)
    , ResponsesConnection(..)
    )
import Agent.Dialect (DialectId(GenericResponsesDialect))
import Control.Monad (unless)
import Data.Char (isDigit)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

-- | Wire protocol used by a host-managed local model.
data RuntimeModelTransport
    = RuntimeResponsesTransport
    | RuntimeChatCompletionsTransport
    deriving (Eq, Show)

data RuntimeResponsesModel = RuntimeResponsesModel
    { runtimeResponsesConnectionId :: !Text
    , runtimeResponsesConnection :: !ResponsesConnection
    , runtimeResponsesCatalogModel :: !CatalogModel
    , runtimeResponsesTransport :: !RuntimeModelTransport
    , runtimeResponsesBearerToken :: !Text
    , runtimeResponsesAllowedTools :: !(Maybe [Text])
    }
    deriving (Eq)

appleFoundationModelsConnectionId :: Text
appleFoundationModelsConnectionId = "apple-foundationmodels"

appleFoundationModelsModelId :: Text
appleFoundationModelsModelId = "apple-foundationmodel"

-- | Build the one host-managed Apple Intelligence model. The endpoint is
-- deliberately restricted to an authenticated IPv4 loopback server; the
-- native app supplies the random port and in-memory bearer token.
mkAppleFoundationModelsRuntime
    :: Text
    -> Text
    -> Int
    -> Either Text RuntimeResponsesModel
mkAppleFoundationModelsRuntime rawBaseUrl rawBearerToken contextWindow = do
    baseUrl <- validateLoopbackBaseUrl rawBaseUrl
    let bearerToken = Text.strip rawBearerToken
    unless (not (Text.null bearerToken)) $
        Left "Apple Foundation Models requires a bearer token"
    unless (contextWindow `elem` [4096, 8192]) $
        Left "Apple Foundation Models reported an unsupported context window"
    pure RuntimeResponsesModel
        { runtimeResponsesConnectionId = appleFoundationModelsConnectionId
        , runtimeResponsesConnection = ResponsesConnection
            { responsesBaseUrl = baseUrl
            , responsesApiKeyEnv = Nothing
            , responsesApiKeyOptional = False
            , responsesRequestTimeoutSeconds = 600
            }
        , runtimeResponsesCatalogModel = CatalogModel
            { catalogModelId = appleFoundationModelsModelId
            , catalogModelConnectionId = appleFoundationModelsConnectionId
            , catalogModelWireId = appleFoundationModelsModelId
            , catalogModelDialect = GenericResponsesDialect
            , catalogModelContextWindow = Just contextWindow
            , catalogModelLabel =
                Just "Apple Intelligence · On-device (Experimental)"
            -- An explicit empty list makes the native picker offer Default
            -- only: Foundation Models has no reasoning-effort control.
            , catalogModelReasoningEfforts = Just []
            , catalogModelDefaultReasoningEffort = Nothing
            , catalogModelDefault = False
            , catalogModelFallbackPriority = Nothing
            }
        , runtimeResponsesTransport = RuntimeChatCompletionsTransport
        , runtimeResponsesBearerToken = bearerToken
        , runtimeResponsesAllowedTools =
            Just
                [ "read_file"
                , "list_dir"
                , "grep"
                , "search_replace"
                , "run_terminal_cmd"
                ]
        }

-- | Add one trusted runtime model, replacing anything that attempts to reuse
-- its connection or stable model id.
applyRuntimeResponsesModel
    :: RuntimeResponsesModel
    -> ModelCatalog
    -> ModelCatalog
applyRuntimeResponsesModel runtime catalog =
    let connectionId = runtime.runtimeResponsesConnectionId
        model = runtime.runtimeResponsesCatalogModel
        retained =
            filter
                (\candidate ->
                    candidate.catalogModelId /= model.catalogModelId
                        && candidate.catalogModelConnectionId /= connectionId)
                catalog.catalogModels
    in ModelCatalog
        { catalogConnections =
            Map.insert
                connectionId
                ModelConnection
                    { connectionId
                    , connectionKind =
                        CustomResponsesConnection
                            runtime.runtimeResponsesConnection
                    }
                catalog.catalogConnections
        , catalogModels = retained <> [model]
        }

runtimeResponsesModelMatches
    :: RuntimeResponsesModel
    -> Text
    -> Text
    -> Bool
runtimeResponsesModelMatches runtime connectionId modelId =
    runtime.runtimeResponsesConnectionId == connectionId
        && runtime.runtimeResponsesCatalogModel.catalogModelId == modelId

validateLoopbackBaseUrl :: Text -> Either Text Text
validateLoopbackBaseUrl raw =
    case Text.stripPrefix "http://127.0.0.1:" normalized of
        Nothing -> Left "Apple Foundation Models must use a 127.0.0.1 endpoint"
        Just portAndPath ->
            let (port, path) = Text.breakOn "/" portAndPath
            in if
                Text.null port
                    || not (Text.all isDigit port)
                    || path /= "/v1"
               then
                    Left
                        "Apple Foundation Models endpoint must end in /v1"
               else Right normalized
  where
    normalized = Text.dropWhileEnd (== '/') (Text.strip raw)
