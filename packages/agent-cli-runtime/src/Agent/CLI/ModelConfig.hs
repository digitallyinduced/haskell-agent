-- | Versioned model catalog configuration.
--
-- The application ships a default catalog and merges an optional user overlay
-- from @~/.haskell-agent/models.json@. Model ids in the overlay replace
-- shipped entries with the same id; new entries are appended in file order.
module Agent.CLI.ModelConfig
    ( CatalogModel(..)
    , ConnectionKind(..)
    , ModelCatalog
    , catalogModels
    , ModelConnection(..)
    , ResponsesConnection(..)
    , builtinConnectionId
    , organizationGatewayConnectionId
    , catalogConnection
    , catalogContextWindowFor
    , catalogContextWindowForTransport
    , catalogDefaultForProvider
    , catalogGatewayModelById
    , catalogModelById
    , catalogModelForConnection
    , catalogModelsForConnection
    , catalogSupportsAsyncToolCallsForTransport
    , connectionSupportsDialect
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
    ( DialectId(..)
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
import Control.Monad (unless)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Paths_agent_cli_runtime (getDataFileName)
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
    | OrganizationGatewayConnection
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
    , catalogModelReasoningEfforts :: !(Maybe [Text])
    , catalogModelDefaultReasoningEffort :: !(Maybe Text)
    , catalogModelSupportsAsyncToolCalls :: !Bool
    , catalogModelDefault :: !Bool
    , catalogModelFallbackPriority :: !(Maybe Int)
    }
    deriving (Eq, Show)

data ModelCatalog = ModelCatalog
    { catalogConnections :: !(Map Text ModelConnection)
    , modelEntries :: ![CatalogModel]
    , catalogDefaults :: !ProviderDefaults
    , catalogModelsById :: !(Map Text CatalogModel)
    , catalogGatewayModelsById :: !(Map Text CatalogModel)
    }
    deriving (Eq, Show)

-- Every supported provider has a default after configuration validation.
-- Keeping a product here makes lookup exhaustive without a partial Map lookup.
data ProviderDefaults = ProviderDefaults
    { openAiDefault :: !CatalogModel
    , xaiDefault :: !CatalogModel
    , openRouterDefault :: !CatalogModel
    , geminiDefault :: !CatalogModel
    , claudeDefault :: !CatalogModel
    }
    deriving (Eq, Show)

-- | Models in their configured presentation order. Catalog internals cannot
-- be updated independently of the indexes and validated defaults.
catalogModels :: ModelCatalog -> [CatalogModel]
catalogModels catalog = catalog.modelEntries

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
    , modelFileReasoningEfforts :: !(Maybe [Text])
    , modelFileDefaultReasoningEffort :: !(Maybe Text)
    , modelFileSupportsAsyncToolCalls :: !Bool
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
            <*> optionalKey "reasoning_efforts"
                (Hermes.list Hermes.text)
            <*> optionalKey "default_reasoning_effort" Hermes.text
            <*> defaultKey False "supports_async_tool_calls" Hermes.bool
            <*> defaultKey False "default" Hermes.bool
            <*> optionalKey "fallback_priority" Hermes.int

modelCatalogUserPath :: OsPath -> OsPath
modelCatalogUserPath home =
    home </> unsafeEncodeUtf ".haskell-agent" </> unsafeEncodeUtf "models.json"

builtinConnectionId :: Provider -> Text
builtinConnectionId = providerSlug

-- | Reserved persisted routing identity for organization-gateway sessions.
--
-- The target provider distinguishes Responses from Anthropic while the
-- reserved connection keeps both protocols separate from direct accounts.
organizationGatewayConnectionId :: Text
organizationGatewayConnectionId = "organization-gateway"

catalogConnection :: ModelCatalog -> Text -> Maybe ModelConnection
catalogConnection catalog connectionId =
    Map.lookup connectionId catalog.catalogConnections

catalogContextWindowFor :: ModelCatalog -> Text -> Text -> Maybe Int
catalogContextWindowFor catalog connectionId modelId = do
    model <- catalogModelForConnection catalog connectionId modelId
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
    selected <- catalogModelForConnection catalog connectionId modelId
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

catalogGatewayModelById :: ModelCatalog -> Text -> Maybe CatalogModel
catalogGatewayModelById catalog modelId =
    Map.lookup modelId catalog.catalogGatewayModelsById

catalogModelForConnection
    :: ModelCatalog
    -> Text
    -> Text
    -> Maybe CatalogModel
catalogModelForConnection catalog connectionId modelId
    | connectionId == organizationGatewayConnectionId =
        catalogGatewayModelById catalog modelId
    | otherwise =
        catalogModelById catalog modelId

catalogModelsForConnection :: Text -> ModelCatalog -> [CatalogModel]
catalogModelsForConnection wanted =
    filter ((== wanted) . (.catalogModelConnectionId)) . catalogModels

-- | Resolve async-tool support against the exact routing connection and
-- transport model. Ambiguous custom aliases fail closed unless every matching
-- catalog entry explicitly opts in. Gateway aliases never inherit capability
-- merely by resembling a direct model name.
catalogSupportsAsyncToolCallsForTransport
    :: ModelCatalog
    -> Text
    -> Text
    -> Bool
catalogSupportsAsyncToolCallsForTransport catalog connectionId transportModel
    | connectionId == organizationGatewayConnectionId = False
    | otherwise =
        case
            [ model
            | model <- catalogModelsForConnection connectionId catalog
            , model.catalogModelId == transportModel
                || model.catalogModelWireId == transportModel
            ] of
            [] -> False
            matches -> all (.catalogModelSupportsAsyncToolCalls) matches

connectionBuiltinProvider :: ModelConnection -> Maybe Provider
connectionBuiltinProvider connection = case connection.connectionKind of
    BuiltinConnection provider -> Just provider
    CustomResponsesConnection _ -> Nothing
    OrganizationGatewayConnection -> Nothing

-- | Validate persisted model-facing protocol identity against its routing
-- connection. Organization gateways use the OpenAI transport while supporting
-- any Responses-hostable agent dialect.
connectionSupportsDialect :: Text -> Provider -> DialectId -> Bool
connectionSupportsDialect connection provider dialect
    | connection == organizationGatewayConnectionId =
        (provider == OpenAIProvider && gatewaySupportsDialect dialect)
            || (provider == ClaudeCodeProvider
                && dialect == ClaudeCodeDialect)
    | otherwise = providerSupportsDialect provider dialect

catalogDefaultForProvider :: ModelCatalog -> Provider -> CatalogModel
catalogDefaultForProvider catalog = \case
    OpenAIProvider -> catalog.catalogDefaults.openAiDefault
    XAIProvider -> catalog.catalogDefaults.xaiDefault
    OpenRouterProvider -> catalog.catalogDefaults.openRouterDefault
    GeminiProvider -> catalog.catalogDefaults.geminiDefault
    ClaudeCodeProvider -> catalog.catalogDefaults.claudeDefault

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
            [ root
                FilePath.</> "packages/agent-cli-runtime/config/models.default.json"
            | root <- roots
            ]
    firstExisting
        (installed : sourceCandidates) >>= \case
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
    let reserved =
            organizationGatewayConnectionId
                : map builtinConnectionId allBuiltinProviders
        overriddenReserved =
            filter (`Map.member` user.configConnections) reserved
    unless (null overriddenReserved) $
        Left
            ( "user model config cannot redefine reserved connection"
                <> plural overriddenReserved <> ": "
                <> Text.intercalate ", " overriddenReserved
            )
    ensureUniqueModelRoutes "shipped model config" defaults.configModels
    ensureUniqueModelRoutes "user model config" user.configModels
    let defaultKeys = map modelMergeKey defaults.configModels
        userByKey = Map.fromList
            [ (modelMergeKey model, model)
            | model <- user.configModels
            ]
        replacedDefaults =
            map
                (\model ->
                    fromMaybe model
                        (Map.lookup (modelMergeKey model) userByKey))
                defaults.configModels
        appendedModels =
            filter
                ((`notElem` defaultKeys) . modelMergeKey)
                user.configModels
    pure ConfigFile
        { configVersion = 1
        , configConnections =
            defaults.configConnections <> user.configConnections
        , configModels = replacedDefaults <> appendedModels
        }

-- | A deliberately small applicative validator. Configuration syntax and
-- references still use 'Either' for dependent, fail-fast resolution; this is
-- only for independent constraints that can be reported together.
data Validation errors value
    = ValidationFailure !errors
    | ValidationSuccess !value

instance Functor (Validation errors) where
    fmap transform = \case
        ValidationFailure errors -> ValidationFailure errors
        ValidationSuccess value -> ValidationSuccess (transform value)

instance Semigroup errors => Applicative (Validation errors) where
    pure = ValidationSuccess
    ValidationFailure left <*> ValidationFailure right =
        ValidationFailure (left <> right)
    ValidationFailure errors <*> _ = ValidationFailure errors
    _ <*> ValidationFailure errors = ValidationFailure errors
    ValidationSuccess transform <*> ValidationSuccess value =
        ValidationSuccess (transform value)

validationFailure :: Text -> Validation [Text] value
validationFailure = ValidationFailure . pure

validationFromEither :: Either Text value -> Validation [Text] value
validationFromEither = \case
    Left err -> validationFailure err
    Right value -> pure value

validationToEither :: Validation [Text] value -> Either Text value
validationToEither = \case
    ValidationFailure errors -> Left (Text.intercalate "\n" errors)
    ValidationSuccess value -> Right value

validationCheck :: Bool -> Text -> Validation [Text] ()
validationCheck valid err
    | valid = pure ()
    | otherwise = validationFailure err

validateConfig :: Text -> ConfigFile -> Either Text ModelCatalog
validateConfig source config = do
    ensureUniqueModelRoutes source config.configModels
    connections <- validationToEither $
        Map.traverseWithKey validateModelConnection config.configConnections
    models <- validationToEither $
        traverse (validateCatalogModel connections) config.configModels
    defaults <- ProviderDefaults
        <$> validateBuiltinDefault connections models OpenAIProvider
        <*> validateBuiltinDefault connections models XAIProvider
        <*> validateBuiltinDefault connections models OpenRouterProvider
        <*> validateBuiltinDefault connections models GeminiProvider
        <*> validateBuiltinDefault connections models ClaudeCodeProvider
    pure ModelCatalog
        { catalogConnections = connections
        , modelEntries = models
        , catalogDefaults = defaults
        , catalogModelsById =
            Map.fromList
                [ (model.catalogModelId, model)
                | model <- models
                , model.catalogModelConnectionId
                    /= organizationGatewayConnectionId
                ]
        , catalogGatewayModelsById =
            Map.fromList
                [ (model.catalogModelId, model)
                | model <- models
                , model.catalogModelConnectionId
                    == organizationGatewayConnectionId
                ]
        }

validateModelConnection
    :: Text
    -> ConnectionFile
    -> Validation [Text] ModelConnection
validateModelConnection connectionId raw =
        case validateConnectionId connectionId of
            Left err -> validationFailure err
            Right () ->
                ModelConnection connectionId
                    <$> validateConnectionKind connectionId raw

  where
    validateConnectionKind connectionId raw =
        case Text.toLower (Text.strip raw.connectionApi) of
            "builtin" ->
                case raw.connectionProvider of
                    Nothing ->
                        validationFailure ("connection " <> connectionId
                            <> " with api=builtin requires provider")
                    Just providerText ->
                        case parseProvider
                            (Text.toLower (Text.strip providerText)) of
                            Nothing ->
                                validationFailure ("connection " <> connectionId
                                    <> " has unknown provider " <> providerText)
                            Just provider ->
                                BuiltinConnection provider
                                    <$ validationCheck
                                        (connectionId == builtinConnectionId provider)
                                        ( "builtin connection " <> connectionId
                                            <> " must use its provider id "
                                            <> builtinConnectionId provider
                                        )
            "responses" ->
                CustomResponsesConnection
                    <$> ( ResponsesConnection
                            <$> maybe
                                (validationFailure
                                    ("connection " <> connectionId
                                        <> " with api=responses requires base_url"))
                                (validationFromEither . validateBaseUrl connectionId)
                                raw.connectionBaseUrl
                            <*> pure (nonEmptyText =<< raw.connectionApiKeyEnv)
                            <*> pure raw.connectionApiKeyOptional
                            <*> ( raw.connectionRequestTimeoutSeconds
                                <$ ( validationCheck
                                        ( raw.connectionApiKeyEnv /= Nothing
                                            || raw.connectionApiKeyOptional
                                        )
                                        ( "connection " <> connectionId
                                            <> " requires api_key_env unless "
                                            <> "api_key_optional is true"
                                        )
                                    *> validationCheck
                                        (raw.connectionRequestTimeoutSeconds > 0)
                                        ( "connection " <> connectionId
                                            <> " request_timeout_seconds must be positive"
                                        )
                                    *> maybe
                                        (pure ())
                                        (validationFromEither . validateEnvName connectionId)
                                        raw.connectionApiKeyEnv
                                )
                            )
                    )
            "gateway" ->
                OrganizationGatewayConnection
                    <$ validationCheck
                        (connectionId == organizationGatewayConnectionId)
                        ( "gateway connection " <> connectionId
                            <> " must use the reserved id "
                            <> organizationGatewayConnectionId
                        )
            other ->
                validationFailure ("connection " <> connectionId
                    <> " has unsupported api " <> other)

validateCatalogModel
    :: Map Text ModelConnection
    -> ModelFile
    -> Validation [Text] CatalogModel
validateCatalogModel connections raw =
        let modelId = Text.strip raw.modelFileId
            connectionId = Text.strip raw.modelFileConnection
            wireId = Text.strip (fromMaybe modelId raw.modelFileWireId)
            reasoningEfforts = fmap
                (map (Text.toLower . Text.strip))
                raw.modelFileReasoningEfforts
            defaultReasoningEffort =
                Text.toLower . Text.strip
                    <$> raw.modelFileDefaultReasoningEffort
        in if Text.null modelId || Text.any isSpace modelId
            then validationFailure
                ("model id must be nonempty and contain no whitespace: "
                    <> raw.modelFileId)
            else if Text.null wireId
                then validationFailure
                    ("model " <> modelId <> " has an empty wire model name")
            else case Map.lookup connectionId connections of
                Nothing ->
                    validationFailure ("model " <> modelId
                        <> " references unknown connection " <> connectionId)
                Just connection ->
                    case parseDialect raw.modelFileDialect of
                        Nothing ->
                            validationFailure ("model " <> modelId
                                <> " has unknown dialect "
                                <> raw.modelFileDialect)
                        Just dialect ->
                            validateResolvedModel
                                connection
                                dialect
                                modelId
                                connectionId
                                wireId
                                reasoningEfforts
                                defaultReasoningEffort
  where
        validateResolvedModel
            connection
            dialect
            modelId
            connectionId
            wireId
            reasoningEfforts
            defaultReasoningEffort =
            let connectionValidation =
                    case connection.connectionKind of
                        BuiltinConnection provider ->
                            validationCheck
                                (wireId == modelId)
                                ( "model " <> modelId
                                    <> " cannot override its wire model on built-in connection "
                                    <> connectionId
                                    <> "; use the wire model as id or define a custom responses connection"
                                )
                                *> validationCheck
                                    (providerSupportsDialect provider dialect)
                                    ( "model " <> modelId <> " uses dialect "
                                        <> raw.modelFileDialect
                                        <> " which is incompatible with connection "
                                        <> connectionId
                                    )
                        CustomResponsesConnection _ -> pure ()
                        OrganizationGatewayConnection ->
                            validationCheck
                                (wireId == modelId)
                                ( "model " <> modelId
                                    <> " cannot override its wire model on organization gateway connection "
                                    <> connectionId
                                )
                                *> validationCheck
                                    (connectionSupportsDialect
                                        connectionId
                                        OpenAIProvider
                                        dialect
                                        || connectionSupportsDialect
                                            connectionId
                                            ClaudeCodeProvider
                                            dialect)
                                    ( "model " <> modelId <> " uses dialect "
                                        <> raw.modelFileDialect
                                        <> " which is incompatible with connection "
                                        <> connectionId
                                    )
                modelValidation =
                    validationCheck
                        (maybe True (>= 0) raw.modelFileFallbackPriority)
                        ( "model " <> modelId
                            <> " fallback_priority must not be negative"
                        )
                    *> validationCheck
                        (maybe True (> 0) raw.modelFileContextWindow)
                        ( "model " <> modelId
                            <> " context_window must be positive"
                        )
                    *> validationCheck
                        (maybe True
                            (all (`elem` supportedReasoningEfforts))
                            reasoningEfforts)
                        ( "model " <> modelId
                            <> " has unsupported reasoning_efforts; expected "
                            <> Text.intercalate ", " supportedReasoningEfforts
                        )
                    *> validationCheck
                        (maybe True (not . null) reasoningEfforts)
                        ( "model " <> modelId
                            <> " reasoning_efforts must not be empty"
                        )
                    *> validationCheck
                        (maybe True
                            (\efforts -> length efforts == length (nub efforts))
                            reasoningEfforts)
                        ( "model " <> modelId
                            <> " reasoning_efforts must not contain duplicates"
                        )
                    *> validationCheck
                        (maybe True
                            (\effort ->
                                maybe False (effort `elem`) reasoningEfforts)
                            defaultReasoningEffort)
                        ( "model " <> modelId
                            <> " default_reasoning_effort must be listed in "
                            <> "reasoning_efforts"
                        )
            in CatalogModel
                { catalogModelId = modelId
                , catalogModelConnectionId = connectionId
                , catalogModelWireId = wireId
                , catalogModelDialect = dialect
                , catalogModelContextWindow =
                    raw.modelFileContextWindow
                , catalogModelLabel =
                    nonEmptyText =<< raw.modelFileLabel
                , catalogModelReasoningEfforts = reasoningEfforts
                , catalogModelDefaultReasoningEffort =
                    defaultReasoningEffort
                , catalogModelSupportsAsyncToolCalls =
                    raw.modelFileSupportsAsyncToolCalls
                        && connectionId /= organizationGatewayConnectionId
                , catalogModelDefault = raw.modelFileDefault
                , catalogModelFallbackPriority =
                    raw.modelFileFallbackPriority
                }
                <$ (connectionValidation *> modelValidation)

supportedReasoningEfforts :: [Text]
supportedReasoningEfforts =
    ["none", "low", "medium", "high", "xhigh", "max"]

validateBuiltinDefault
    :: Map Text ModelConnection
    -> [CatalogModel]
    -> Provider
    -> Either Text CatalogModel
validateBuiltinDefault connections models provider = do
    case Map.lookup (builtinConnectionId provider) connections of
        Just ModelConnection{connectionKind = BuiltinConnection configured}
            | configured == provider -> pure ()
        _ -> Left ("connection " <> builtinConnectionId provider
            <> " must be a builtin connection for its provider")
    case filter
        (\model ->
            model.catalogModelConnectionId == builtinConnectionId provider
                && model.catalogModelDefault)
        models of
        [model] -> Right model
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

ensureUniqueModelRoutes :: Text -> [ModelFile] -> Either Text ()
ensureUniqueModelRoutes source models =
    case duplicateIds of
        [] -> Right ()
        duplicates ->
            Left ("duplicate model id" <> plural duplicates <> " in "
                <> source <> ": " <> Text.intercalate ", " duplicates)
  where
    counts = foldl'
        (\current model ->
            Map.insertWith (+) (modelMergeKey model) (1 :: Int) current)
        Map.empty
        models
    duplicateIds =
        map snd (Map.keys (Map.filter (> 1) counts))

modelMergeKey :: ModelFile -> (Bool, Text)
modelMergeKey model =
    ( Text.strip model.modelFileConnection
        == organizationGatewayConnectionId
    , Text.strip model.modelFileId
    )

allBuiltinProviders :: [Provider]
allBuiltinProviders =
    [OpenAIProvider, XAIProvider, OpenRouterProvider, GeminiProvider, ClaudeCodeProvider]

gatewaySupportsDialect :: DialectId -> Bool
gatewaySupportsDialect = \case
    CodexDialect -> True
    GrokBuildDialect -> True
    GenericResponsesDialect -> True
    ClaudeCodeDialect -> False

nonEmptyText :: Text -> Maybe Text
nonEmptyText value
    | Text.null stripped = Nothing
    | otherwise = Just stripped
  where
    stripped = Text.strip value

plural :: [a] -> Text
plural [_] = ""
plural _ = "s"
