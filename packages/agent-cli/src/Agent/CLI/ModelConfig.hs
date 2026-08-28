-- | Versioned model catalog configuration.
--
-- The application ships a default catalog and merges an optional user overlay
-- from @~/.haskell-agent/models.json@. Model ids in the overlay replace
-- shipped entries with the same id; new entries are appended in file order.
module Agent.CLI.ModelConfig
    ( CatalogModel(..)
    , ConnectionKind(..)
    , ModelCatalog(..)
    , ModelConnection(..)
    , ResponsesConnection(..)
    , builtinConnectionId
    , catalogConnection
    , catalogContextWindowFor
    , catalogContextWindowForTransport
    , catalogDefaultForProvider
    , catalogModelById
    , catalogModelsForConnection
    , connectionBuiltinProvider
    , decodeModelConfig
    , loadModelCatalog
    , loadModelCatalogAt
    , loadModelCatalogWith
    , mergeModelConfigs
    , modelCatalogUserPath
    , packagedModelCatalogPath
    ) where

import Agent.Dialect
    ( DialectId
    , parseDialect
    , providerSupportsDialect
    )
import Agent.FileRetry (retryOnFileBusy)
import Agent.CLI.Json (decodeLazy)
import Agent.Json.Decode (defaultKey, optionalKey)
import Agent.Json.Decode qualified as Hermes
import Agent.OsPath (toText, unsafeToFilePath)
import Agent.Provider (Provider(..), parseProvider, providerSlug)
import Control.Exception.Safe (tryIO)
import Control.Monad (unless, when)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Paths_agent_cli (getDataFileName)
import qualified System.Directory as Directory
import System.Directory.OsPath (doesFileExist)
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath
import System.OsPath (OsPath, unsafeEncodeUtf, (</>))

data ResponsesConnection = ResponsesConnection
    { responsesBaseUrl :: !Text
    , responsesApiKeyEnv :: !(Maybe Text)
    , responsesApiKeyOptional :: !Bool
    , responsesRequestTimeoutSeconds :: !Int
    }
    deriving (Eq, Show)

data ConnectionKind
    = BuiltinConnection !Provider
    | CustomResponsesConnection !ResponsesConnection
    deriving (Eq, Show)

data ModelConnection = ModelConnection
    { connectionId :: !Text
    , connectionKind :: !ConnectionKind
    }
    deriving (Eq, Show)

data CatalogModel = CatalogModel
    { catalogModelId :: !Text
    , catalogModelConnectionId :: !Text
    , catalogModelWireId :: !Text
    , catalogModelDialect :: !DialectId
    , catalogModelContextWindow :: !(Maybe Int)
    , catalogModelLabel :: !(Maybe Text)
    , catalogModelDefault :: !Bool
    , catalogModelFallbackPriority :: !(Maybe Int)
    }
    deriving (Eq, Show)

data ModelCatalog = ModelCatalog
    { catalogConnections :: !(Map Text ModelConnection)
    , catalogModels :: ![CatalogModel]
    , catalogModelsById :: !(Map Text CatalogModel)
    }
    deriving (Eq, Show)

data ConfigFile = ConfigFile
    { configVersion :: !Int
    , configConnections :: !(Map Text ConnectionFile)
    , configModels :: ![ModelFile]
    }
    deriving (Eq, Show)

data ConnectionFile = ConnectionFile
    { connectionApi :: !Text
    , connectionProvider :: !(Maybe Text)
    , connectionBaseUrl :: !(Maybe Text)
    , connectionApiKeyEnv :: !(Maybe Text)
    , connectionApiKeyOptional :: !Bool
    , connectionRequestTimeoutSeconds :: !Int
    }
    deriving (Eq, Show)

data ModelFile = ModelFile
    { modelFileId :: !Text
    , modelFileConnection :: !Text
    , modelFileWireId :: !(Maybe Text)
    , modelFileDialect :: !Text
    , modelFileContextWindow :: !(Maybe Int)
    , modelFileLabel :: !(Maybe Text)
    , modelFileDefault :: !Bool
    , modelFileFallbackPriority :: !(Maybe Int)
    }
    deriving (Eq, Show)

configFileDecoder :: Hermes.Decoder ConfigFile
configFileDecoder =
    Hermes.object $
        ConfigFile
            <$> Hermes.atKey "version" Hermes.int
            <*> defaultKey Map.empty "connections"
                (Hermes.objectAsMap pure connectionFileDecoder)
            <*> defaultKey [] "models" (Hermes.list modelFileDecoder)

connectionFileDecoder :: Hermes.Decoder ConnectionFile
connectionFileDecoder =
    Hermes.object $
        ConnectionFile
            <$> Hermes.atKey "api" Hermes.text
            <*> optionalKey "provider" Hermes.text
            <*> optionalKey "base_url" Hermes.text
            <*> optionalKey "api_key_env" Hermes.text
            <*> defaultKey False "api_key_optional" Hermes.bool
            <*> defaultKey 600 "request_timeout_seconds" Hermes.int

modelFileDecoder :: Hermes.Decoder ModelFile
modelFileDecoder =
    Hermes.object $
        ModelFile
            <$> Hermes.atKey "id" Hermes.text
            <*> Hermes.atKey "connection" Hermes.text
            <*> optionalKey "model" Hermes.text
            <*> Hermes.atKey "dialect" Hermes.text
            <*> optionalKey "context_window" Hermes.int
            <*> optionalKey "label" Hermes.text
            <*> defaultKey False "default" Hermes.bool
            <*> optionalKey "fallback_priority" Hermes.int

modelCatalogUserPath :: OsPath -> OsPath
modelCatalogUserPath home =
    home </> unsafeEncodeUtf ".haskell-agent" </> unsafeEncodeUtf "models.json"

builtinConnectionId :: Provider -> Text
builtinConnectionId = providerSlug

catalogConnection :: ModelCatalog -> Text -> Maybe ModelConnection
catalogConnection catalog connectionId =
    Map.lookup connectionId catalog.catalogConnections

catalogContextWindowFor :: ModelCatalog -> Text -> Text -> Maybe Int
catalogContextWindowFor catalog connectionId modelId = do
    model <- catalogModelById catalog modelId
    if model.catalogModelConnectionId == connectionId
        then model.catalogModelContextWindow
        else Nothing

-- | Resolve the selected model's context window against the model actually
-- sent to the provider. Environment-backed model maps may redirect a stable
-- catalog id to another configured wire model; in that case, using the source
-- model's limit could permit an oversized request.
catalogContextWindowForTransport
    :: ModelCatalog
    -> Text
    -> Text
    -> Text
    -> Maybe Int
catalogContextWindowForTransport catalog connectionId modelId wireModelId = do
    selected <- catalogModelById catalog modelId
    if selected.catalogModelConnectionId /= connectionId
        then Nothing
        else
            let effective
                    | selected.catalogModelWireId == wireModelId =
                        Just selected
                    | otherwise =
                        Map.lookup wireModelId
                            (Map.fromList
                                [ ( candidate.catalogModelWireId
                                  , candidate
                                  )
                                | candidate <-
                                    catalogModelsForConnection
                                        connectionId
                                        catalog
                                ])
            in effective >>= (.catalogModelContextWindow)

catalogModelById :: ModelCatalog -> Text -> Maybe CatalogModel
catalogModelById catalog modelId =
    Map.lookup modelId catalog.catalogModelsById

catalogModelsForConnection :: Text -> ModelCatalog -> [CatalogModel]
catalogModelsForConnection wanted =
    filter ((== wanted) . (.catalogModelConnectionId)) . (.catalogModels)

connectionBuiltinProvider :: ModelConnection -> Maybe Provider
connectionBuiltinProvider connection = case connection.connectionKind of
    BuiltinConnection provider -> Just provider
    CustomResponsesConnection _ -> Nothing

catalogDefaultForProvider :: ModelCatalog -> Provider -> Maybe CatalogModel
catalogDefaultForProvider catalog provider =
    case
        [ model
        | model <- catalogModelsForConnection (builtinConnectionId provider) catalog
        , model.catalogModelDefault
        ] of
        model : _ -> Just model
        [] -> Nothing

-- | Decode and validate one standalone file. This is mainly useful for tests;
-- normal startup should use 'mergeModelConfigs' so defaults can be overlaid.
decodeModelConfig :: Text -> LBS.ByteString -> Either Text ModelCatalog
decodeModelConfig source bytes = do
    config <- decodeConfigFile source bytes
    validateConfig source config

-- | Merge already-decoded JSON files using the public overlay semantics.
mergeModelConfigs
    :: (Text, LBS.ByteString)
    -> Maybe (Text, LBS.ByteString)
    -> Either Text ModelCatalog
mergeModelConfigs (defaultSource, defaultBytes) user = do
    defaults <- decodeConfigFile defaultSource defaultBytes
    overlay <- traverse (uncurry decodeConfigFile) user
    merged <- mergeConfigFiles defaults overlay
    validateConfig (maybe defaultSource fst user) merged

loadModelCatalog :: OsPath -> IO (Either Text ModelCatalog)
loadModelCatalog home = do
    cwd <- unsafeEncodeUtf <$> Directory.getCurrentDirectory
    loadModelCatalogAt home cwd

loadModelCatalogAt :: OsPath -> OsPath -> IO (Either Text ModelCatalog)
loadModelCatalogAt home cwd = do
    defaultPath <- packagedModelCatalogPathAt cwd
    loadModelCatalogWith defaultPath home

packagedModelCatalogPath :: IO FilePath
packagedModelCatalogPath = do
    cwd <- unsafeEncodeUtf <$> Directory.getCurrentDirectory
    packagedModelCatalogPathAt cwd

packagedModelCatalogPathAt :: OsPath -> IO FilePath
packagedModelCatalogPathAt cwd = do
    installed <- getDataFileName "config/models.default.json"
    executable <- Environment.getExecutablePath
    let roots =
            take 16 (iterate FilePath.takeDirectory executable)
                <> take 8
                    (iterate FilePath.takeDirectory (unsafeToFilePath cwd))
        sourceCandidates =
            [ root FilePath.</> "packages/agent-cli/config/models.default.json"
            | root <- roots
            ]
    firstExisting
        ( [installed, "config/models.default.json"]
            <> sourceCandidates
        ) >>= \case
            Just path -> pure path
            Nothing -> pure installed
  where
    firstExisting = \case
        [] -> pure Nothing
        path : rest ->
            Directory.doesFileExist path >>= \case
                True -> pure (Just path)
                False -> firstExisting rest

-- | Load a caller-supplied default file and the standard user overlay. Tests
-- use this to avoid depending on Cabal's installed data directory.
loadModelCatalogWith :: FilePath -> OsPath -> IO (Either Text ModelCatalog)
loadModelCatalogWith defaultPath home = do
    let userPath = modelCatalogUserPath home
    defaultResult <- readConfigFile (Text.pack defaultPath) defaultPath
    case defaultResult of
        Left err -> pure (Left err)
        Right defaultBytes -> do
            userExists <- doesFileExist userPath
            if not userExists
                then pure $
                    mergeModelConfigs
                        (Text.pack defaultPath, defaultBytes)
                        Nothing
                else do
                    userResult <-
                        readConfigFile (toText userPath) (unsafeToFilePath userPath)
                    pure do
                        userBytes <- userResult
                        mergeModelConfigs
                            (Text.pack defaultPath, defaultBytes)
                            (Just (toText userPath, userBytes))

readConfigFile :: Text -> FilePath -> IO (Either Text LBS.ByteString)
readConfigFile source path =
    tryIO (retryOnFileBusy (LBS.readFile path)) >>= \case
        Left exception ->
            pure $ Left
                ("could not read model config " <> source <> ": "
                    <> Text.pack (show exception))
        Right bytes -> pure (Right bytes)

decodeConfigFile :: Text -> LBS.ByteString -> Either Text ConfigFile
decodeConfigFile source bytes =
    case decodeLazy configFileDecoder bytes of
        Left err ->
            Left ("invalid model config " <> source <> ": " <> err)
        Right config
            | config.configVersion /= 1 ->
                Left
                    ( "unsupported model config version "
                        <> Text.pack (show config.configVersion)
                        <> " in " <> source <> "; expected version 1"
                    )
            | otherwise -> Right config

mergeConfigFiles :: ConfigFile -> Maybe ConfigFile -> Either Text ConfigFile
mergeConfigFiles defaults Nothing = Right defaults
mergeConfigFiles defaults (Just user) = do
    let reserved = map builtinConnectionId allBuiltinProviders
        overriddenReserved =
            filter (`Map.member` user.configConnections) reserved
    unless (null overriddenReserved) $
        Left
            ( "user model config cannot redefine reserved connection"
                <> plural overriddenReserved <> ": "
                <> Text.intercalate ", " overriddenReserved
            )
    ensureUniqueModelIds "shipped model config" defaults.configModels
    ensureUniqueModelIds "user model config" user.configModels
    let defaultIds = map (.modelFileId) defaults.configModels
        userById = Map.fromList
            [ (model.modelFileId, model)
            | model <- user.configModels
            ]
        replacedDefaults =
            map
                (\model ->
                    fromMaybe model
                        (Map.lookup model.modelFileId userById))
                defaults.configModels
        appendedModels =
            filter
                ((`notElem` defaultIds) . (.modelFileId))
                user.configModels
    pure ConfigFile
        { configVersion = 1
        , configConnections =
            defaults.configConnections <> user.configConnections
        , configModels = replacedDefaults <> appendedModels
        }

validateConfig :: Text -> ConfigFile -> Either Text ModelCatalog
validateConfig source config = do
    ensureUniqueModelIds source config.configModels
    connections <- Map.traverseWithKey validateConnection
        config.configConnections
    models <- traverse (validateModel connections) config.configModels
    traverse_ (validateBuiltinDefault models) allBuiltinProviders
    pure ModelCatalog
        { catalogConnections = connections
        , catalogModels = models
        , catalogModelsById =
            Map.fromList
                [ (model.catalogModelId, model)
                | model <- models
                ]
        }
  where
    validateConnection connectionId raw = do
        validateConnectionId connectionId
        kind <- case Text.toLower (Text.strip raw.connectionApi) of
            "builtin" -> do
                providerText <- maybe
                    (Left ("connection " <> connectionId
                        <> " with api=builtin requires provider"))
                    Right
                    raw.connectionProvider
                provider <- maybe
                    (Left ("connection " <> connectionId
                        <> " has unknown provider " <> providerText))
                    Right
                    (parseProvider (Text.toLower (Text.strip providerText)))
                when (connectionId /= builtinConnectionId provider) $
                    Left
                        ( "builtin connection " <> connectionId
                            <> " must use its provider id "
                            <> builtinConnectionId provider
                        )
                pure (BuiltinConnection provider)
            "responses" -> do
                baseUrl <- maybe
                    (Left ("connection " <> connectionId
                        <> " with api=responses requires base_url"))
                    (validateBaseUrl connectionId)
                    raw.connectionBaseUrl
                when
                    ( raw.connectionApiKeyEnv == Nothing
                        && not raw.connectionApiKeyOptional
                    ) $
                    Left
                        ( "connection " <> connectionId
                            <> " requires api_key_env unless "
                            <> "api_key_optional is true"
                        )
                when (raw.connectionRequestTimeoutSeconds <= 0) $
                    Left ("connection " <> connectionId
                        <> " request_timeout_seconds must be positive")
                traverse_ (validateEnvName connectionId)
                    raw.connectionApiKeyEnv
                pure $ CustomResponsesConnection ResponsesConnection
                    { responsesBaseUrl = baseUrl
                    , responsesApiKeyEnv =
                        nonEmptyText =<< raw.connectionApiKeyEnv
                    , responsesApiKeyOptional =
                        raw.connectionApiKeyOptional
                    , responsesRequestTimeoutSeconds =
                        raw.connectionRequestTimeoutSeconds
                    }
            other ->
                Left ("connection " <> connectionId
                    <> " has unsupported api " <> other)
        pure ModelConnection{connectionId, connectionKind = kind}

    validateModel connections raw = do
        let modelId = Text.strip raw.modelFileId
            connectionId = Text.strip raw.modelFileConnection
            wireId = Text.strip (fromMaybe modelId raw.modelFileWireId)
        when (Text.null modelId || Text.any isSpace modelId) $
            Left ("model id must be nonempty and contain no whitespace: "
                <> raw.modelFileId)
        when (Text.null wireId) $
            Left ("model " <> modelId <> " has an empty wire model name")
        connection <- maybe
            (Left ("model " <> modelId
                <> " references unknown connection " <> connectionId))
            Right
            (Map.lookup connectionId connections)
        dialect <- maybe
            (Left ("model " <> modelId <> " has unknown dialect "
                <> raw.modelFileDialect))
            Right
            (parseDialect raw.modelFileDialect)
        case connection.connectionKind of
            BuiltinConnection provider -> do
                when (wireId /= modelId) $
                    Left
                        ( "model " <> modelId
                            <> " cannot override its wire model on built-in connection "
                            <> connectionId
                            <> "; use the wire model as id or define a custom responses connection"
                        )
                unless (providerSupportsDialect provider dialect) $
                    Left
                        ( "model " <> modelId <> " uses dialect "
                            <> raw.modelFileDialect
                            <> " which is incompatible with connection "
                            <> connectionId
                        )
            CustomResponsesConnection _ -> pure ()
        traverse_
            (\priority -> when (priority < 0) $
                Left ("model " <> modelId
                    <> " fallback_priority must not be negative"))
            raw.modelFileFallbackPriority
        traverse_
            (\contextWindow -> when (contextWindow <= 0) $
                Left ("model " <> modelId
                    <> " context_window must be positive"))
            raw.modelFileContextWindow
        pure CatalogModel
            { catalogModelId = modelId
            , catalogModelConnectionId = connectionId
            , catalogModelWireId = wireId
            , catalogModelDialect = dialect
            , catalogModelContextWindow =
                raw.modelFileContextWindow
            , catalogModelLabel =
                nonEmptyText =<< raw.modelFileLabel
            , catalogModelDefault = raw.modelFileDefault
            , catalogModelFallbackPriority =
                raw.modelFileFallbackPriority
            }

validateBuiltinDefault :: [CatalogModel] -> Provider -> Either Text ()
validateBuiltinDefault models provider =
    case filter
        (\model ->
            model.catalogModelConnectionId == builtinConnectionId provider
                && model.catalogModelDefault)
        models of
        [_] -> Right ()
        [] ->
            Left ("connection " <> builtinConnectionId provider
                <> " must have exactly one default model")
        _ ->
            Left ("connection " <> builtinConnectionId provider
                <> " has more than one default model")

validateConnectionId :: Text -> Either Text ()
validateConnectionId connectionId
    | Text.null connectionId =
        Left "connection id must not be empty"
    | Text.all isConnectionChar connectionId = Right ()
    | otherwise =
        Left ("invalid connection id " <> connectionId
            <> "; use letters, digits, '.', '_', or '-'")
  where
    isConnectionChar character =
        isAlphaNum character || character `elem` ['.', '_', '-']

validateBaseUrl :: Text -> Text -> Either Text Text
validateBaseUrl connectionId raw
    | "http://" `Text.isPrefixOf` normalized = Right trimmed
    | "https://" `Text.isPrefixOf` normalized = Right trimmed
    | otherwise =
        Left ("connection " <> connectionId
            <> " base_url must start with http:// or https://")
  where
    trimmed = Text.dropWhileEnd (== '/') (Text.strip raw)
    normalized = Text.toLower trimmed

validateEnvName :: Text -> Text -> Either Text ()
validateEnvName connectionId raw =
    case Text.unpack (Text.strip raw) of
        [] ->
            Left ("connection " <> connectionId
                <> " api_key_env must not be empty")
        first : rest
            | (isAlpha first || first == '_')
            , all (\character -> isAlphaNum character || character == '_') rest ->
                Right ()
        _ ->
            Left ("connection " <> connectionId
                <> " has invalid api_key_env " <> raw)

ensureUniqueModelIds :: Text -> [ModelFile] -> Either Text ()
ensureUniqueModelIds source models =
    case duplicateIds of
        [] -> Right ()
        duplicates ->
            Left ("duplicate model id" <> plural duplicates <> " in "
                <> source <> ": " <> Text.intercalate ", " duplicates)
  where
    counts = foldl'
        (\current model ->
            Map.insertWith (+) (Text.strip model.modelFileId) (1 :: Int) current)
        Map.empty
        models
    duplicateIds =
        Map.keys (Map.filter (> 1) counts)

allBuiltinProviders :: [Provider]
allBuiltinProviders =
    [OpenAIProvider, XAIProvider, OpenRouterProvider]

nonEmptyText :: Text -> Maybe Text
nonEmptyText value
    | Text.null stripped = Nothing
    | otherwise = Just stripped
  where
    stripped = Text.strip value

plural :: [a] -> Text
plural [_] = ""
plural _ = "s"
