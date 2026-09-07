module Agent.Integrations.Registry
    ( IntegrationRegistry
    , createIntegrationRegistry
    , integrationRegistryInstructions
    , integrationRegistryTools
    , lookupIntegrationTool
    ) where

import Agent.Integrations.Types
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

data IntegrationRegistry = IntegrationRegistry
    { registryModules :: ![IntegrationModule]
    }

createIntegrationRegistry
    :: [IntegrationModule]
    -> Either Text IntegrationRegistry
createIntegrationRegistry modules = do
    ensureUnique
        "integration id"
        (map (integrationIdText . (.integrationModuleId)) modules)
    pure IntegrationRegistry
        { registryModules = modules
        }

integrationRegistryInstructions :: IntegrationRegistry -> Text
integrationRegistryInstructions registry =
    Text.intercalate "\n\n"
        [ Text.strip integrationModule.integrationModuleInstructions
        | integrationModule <- registry.registryModules
        , not
            (Text.null
                (Text.strip integrationModule.integrationModuleInstructions))
        ]

integrationRegistryTools
    :: IntegrationRegistry
    -> IO (Either Text [SomeIntegrationTool])
integrationRegistryTools registry = do
    tools <- concat <$> traverse
        (.integrationModuleTools)
        registry.registryModules
    pure (ensureUniqueTools tools)

lookupIntegrationTool
    :: Text
    -> IntegrationRegistry
    -> IO (Either Text (Maybe SomeIntegrationTool))
lookupIntegrationTool name registry =
    fmap (fmap (findByName name)) (integrationRegistryTools registry)

findByName :: Text -> [SomeIntegrationTool] -> Maybe SomeIntegrationTool
findByName _ [] = Nothing
findByName name (packed@(SomeIntegrationTool tool) : rest)
    | integrationToolNameText tool.integrationToolNameValue == name =
        Just packed
    | otherwise = findByName name rest

ensureUniqueTools
    :: [SomeIntegrationTool]
    -> Either Text [SomeIntegrationTool]
ensureUniqueTools tools = do
    ensureUnique
        "integration tool name"
        [ integrationToolNameText tool.integrationToolNameValue
        | SomeIntegrationTool tool <- tools
        ]
    pure tools

ensureUnique :: Text -> [Text] -> Either Text ()
ensureUnique label = go Map.empty
  where
    go _ [] = Right ()
    go seen (value : rest)
        | Map.member value seen =
            Left ("duplicate " <> label <> ": " <> value)
        | otherwise =
            go (Map.insert value () seen) rest
