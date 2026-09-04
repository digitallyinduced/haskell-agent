module Agent.CLI.ModelConfigSpec (spec) where

import Agent.CLI.ModelConfig
import Agent.Dialect (DialectId(..))
import Agent.OsPath (fromText, unsafeToFilePath)
import Agent.Provider (Provider(..))
import Control.Exception.Safe (bracket)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Either (isRight)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory
    ( createDirectory
    , createDirectoryIfMissing
    , getTemporaryDirectory
    , removePathForcibly
    )
import System.IO (hClose, openTempFile)
import System.OsPath (OsPath, (</>))
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.ModelConfig" do
    it "decodes the shipped catalog and derives every builtin default" do
        bytes <- readPackagedDefaults
        let decoded = decodeModelConfig "models.default.json" bytes
        catalog <- expectRight decoded
        fmap (.catalogModelId)
            (catalogDefaultForProvider catalog OpenAIProvider)
            `shouldBe` Just "gpt-5.6-sol"
        fmap (.catalogModelId)
            (catalogDefaultForProvider catalog XAIProvider)
            `shouldBe` Just "grok-4.6"
        fmap (.catalogModelId)
            (catalogDefaultForProvider catalog GeminiProvider)
            `shouldBe` Just "gemini-3.7-flash"
        fmap (.catalogModelId)
            (catalogDefaultForProvider catalog OpenRouterProvider)
            `shouldBe` Just "stealth/ox-alpha"
        catalogContextWindowFor catalog "xai" "grok-4.6"
            `shouldBe` Just 500_000
        map (catalogContextWindowFor catalog "gemini")
            [ "gemini-3.7-flash"
            , "gemini-3.1-pro-preview"
            , "gemini-3.5-flash-lite"
            ]
            `shouldBe` replicate 3 (Just 1_048_576)
        catalogContextWindowFor catalog "openrouter" "stealth/ox-alpha"
            `shouldBe` Just 1_048_576
        fmap (.catalogModelId)
            (catalogModelsForConnection "openai" catalog)
            `shouldBe`
                [ "gpt-5.6-sol"
                , "gpt-5.6-terra"
                , "gpt-5.6-luna"
                ]
        fmap (.catalogModelId)
            (catalogModelsForConnection "gemini" catalog)
            `shouldBe`
                [ "gemini-3.7-flash"
                , "gemini-3.1-pro-preview"
                , "gemini-3.5-flash-lite"
                ]
        Map.lookup organizationGatewayConnectionId catalog.catalogConnections
            `shouldBe`
                Just ModelConnection
                    { connectionId = organizationGatewayConnectionId
                    , connectionKind = OrganizationGatewayConnection
                    }
        fmap
            (\model ->
                ( model.catalogModelReasoningEfforts
                , model.catalogModelDefaultReasoningEffort
                ))
            (findModel "opus" catalog.catalogModels)
            `shouldBe`
                Just
                    ( Just ["low", "medium", "high", "xhigh", "max"]
                    , Just "xhigh"
                    )
        fmap
            (\model ->
                ( model.catalogModelReasoningEfforts
                , model.catalogModelDefaultReasoningEffort
                ))
            (Map.lookup "gpt-5.6-sol" catalog.catalogModelsById)
            `shouldBe`
                Just
                    ( Just ["low", "medium", "high", "xhigh", "max"]
                    , Just "low"
                    )

    it "loads protocol metadata for organization gateway aliases" do
        defaults <- readPackagedDefaults
        let overlay = LBS.pack $ unlines
                [ "{"
                , "  \"version\": 1,"
                , "  \"models\": [{"
                , "    \"id\": \"company-coder\","
                , "    \"connection\": \"organization-gateway\","
                , "    \"dialect\": \"generic-responses\","
                , "    \"context_window\": 131072,"
                , "    \"label\": \"company\""
                , "  }]"
                , "}"
                ]
        catalog <- expectRight
            (mergeModelConfigs
                ("models.default.json", defaults)
                (Just ("models.json", overlay)))
        catalogModelsForConnection organizationGatewayConnectionId catalog
            `shouldBe`
                [ CatalogModel
                    { catalogModelId = "company-coder"
                    , catalogModelConnectionId =
                        organizationGatewayConnectionId
                    , catalogModelWireId = "company-coder"
                    , catalogModelDialect = GenericResponsesDialect
                    , catalogModelContextWindow = Just 131_072
                    , catalogModelLabel = Just "company"
                    , catalogModelReasoningEfforts = Nothing
                    , catalogModelDefaultReasoningEffort = Nothing
                    , catalogModelDefault = False
                    , catalogModelFallbackPriority = Nothing
                    }
                ]
        connectionSupportsDialect
            organizationGatewayConnectionId
            OpenAIProvider
            GenericResponsesDialect
            `shouldBe` True
        connectionSupportsDialect
            organizationGatewayConnectionId
            ClaudeCodeProvider
            ClaudeCodeDialect
            `shouldBe` True

        let remapped =
                "{\"version\":1,\"models\":[{\"id\":\"company-remapped\",\"connection\":\"organization-gateway\",\"model\":\"private-upstream\",\"dialect\":\"codex\"}]}"
            ownedTools =
                "{\"version\":1,\"models\":[{\"id\":\"company-claude\",\"connection\":\"organization-gateway\",\"dialect\":\"claude-code\"}]}"
        mergeModelConfigs
            ("models.default.json", defaults)
            (Just ("models.json", remapped))
            `shouldSatisfy` leftContains "cannot override its wire model"
        mergeModelConfigs
            ("models.default.json", defaults)
            (Just ("models.json", ownedTools))
            `shouldSatisfy` isRight

    it "adds a custom Responses connection and model" do
        defaults <- readPackagedDefaults
        let overlay = LBS.pack $ unlines
                [ "{"
                , "  \"version\": 1,"
                , "  \"connections\": {"
                , "    \"ollama\": {"
                , "      \"api\": \"responses\","
                , "      \"base_url\": \"http://localhost:11434/v1/\","
                , "      \"api_key_env\": \"OLLAMA_API_KEY\","
                , "      \"api_key_optional\": true"
                , "    }"
                , "  },"
                , "  \"models\": [{"
                , "    \"id\": \"qwen-local\","
                , "    \"connection\": \"ollama\","
                , "    \"model\": \"qwen2.5-coder:32b\","
                , "    \"dialect\": \"generic-responses\","
                , "    \"context_window\": 32768,"
                , "    \"label\": \"local\""
                , "  }]"
                , "}"
                ]
        catalog <- expectRight
            (mergeModelConfigs
                ("models.default.json", defaults)
                (Just ("models.json", overlay)))
        Map.lookup "ollama" catalog.catalogConnections
            `shouldBe`
                Just ModelConnection
                    { connectionId = "ollama"
                    , connectionKind =
                        CustomResponsesConnection ResponsesConnection
                            { responsesBaseUrl =
                                "http://localhost:11434/v1"
                            , responsesApiKeyEnv = Just "OLLAMA_API_KEY"
                            , responsesApiKeyOptional = True
                            , responsesRequestTimeoutSeconds = 600
                            }
                    }
        let custom =
                filter ((== "qwen-local") . (.catalogModelId))
                    catalog.catalogModels
        fmap (.catalogModelWireId) custom
            `shouldBe` ["qwen2.5-coder:32b"]
        fmap (.catalogModelDialect) custom
            `shouldBe` [GenericResponsesDialect]
        fmap (.catalogModelContextWindow) custom
            `shouldBe` [Just 32_768]

    it "uses the effective configured wire model after a transport remap" do
        defaults <- readPackagedDefaults
        let overlay =
                "{\"version\":1,\"models\":[{\"id\":\"source-model\",\"connection\":\"openrouter\",\"dialect\":\"generic-responses\",\"context_window\":32000},{\"id\":\"provider/target-model\",\"connection\":\"openrouter\",\"dialect\":\"generic-responses\",\"context_window\":128000}]}"
        catalog <- expectRight
            (mergeModelConfigs
                ("models.default.json", defaults)
                (Just ("models.json", overlay)))
        catalogContextWindowForTransport
            catalog
            "openrouter"
            "source-model"
            "source-model"
            `shouldBe` Just 32_000
        catalogContextWindowForTransport
            catalog
            "openrouter"
            "source-model"
            "provider/target-model"
            `shouldBe` Just 128_000
        catalogContextWindowForTransport
            catalog
            "openrouter"
            "source-model"
            "provider/missing-model"
            `shouldBe` Nothing

    it "replaces shipped model metadata by stable id without moving entries" do
        defaults <- readPackagedDefaults
        let overlay = LBS.pack $ unlines
                [ "{"
                , "  \"version\": 1,"
                , "  \"models\": [{"
                , "    \"id\": \"gpt-5.6-sol\","
                , "    \"connection\": \"openai\","
                , "    \"model\": \"gpt-5.6-sol\","
                , "    \"dialect\": \"codex\","
                , "    \"label\": \"overridden\","
                , "    \"default\": true"
                , "  }]"
                , "}"
                ]
        catalog <- expectRight
            (mergeModelConfigs
                ("models.default.json", defaults)
                (Just ("models.json", overlay)))
        let matching =
                filter ((== "gpt-5.6-sol") . (.catalogModelId))
                    catalog.catalogModels
        length matching `shouldBe` 1
        fmap (.catalogModelWireId) matching
            `shouldBe` ["gpt-5.6-sol"]
        fmap (.catalogModelLabel) matching
            `shouldBe` [Just "overridden"]

    it "rejects user attempts to redefine reserved connections" do
        defaults <- readPackagedDefaults
        let overlay =
                "{\"version\":1,\"connections\":{\"openai\":{\"api\":\"responses\",\"base_url\":\"http://localhost:8000/v1\"}}}"
        mergeModelConfigs
            ("models.default.json", defaults)
            (Just ("models.json", overlay))
            `shouldSatisfy` leftContains "cannot redefine reserved connection"

        let gatewayOverlay =
                "{\"version\":1,\"connections\":{\"organization-gateway\":{\"api\":\"responses\",\"base_url\":\"http://localhost:8000/v1\"}}}"
        mergeModelConfigs
            ("models.default.json", defaults)
            (Just ("models.json", gatewayOverlay))
            `shouldSatisfy` leftContains "cannot redefine reserved connection"

    it "reports invalid custom references and authentication variables" do
        defaults <- readPackagedDefaults
        let overlay =
                "{\"version\":1,\"connections\":{\"local\":{\"api\":\"responses\",\"base_url\":\"http://localhost/v1\",\"api_key_env\":\"BAD-NAME\"}},\"models\":[{\"id\":\"local\",\"connection\":\"missing\",\"dialect\":\"generic-responses\"}]}"
        mergeModelConfigs
            ("models.default.json", defaults)
            (Just ("models.json", overlay))
            `shouldSatisfy` leftContains "invalid api_key_env"

    it "reports independent connection validation errors together in check order" do
        defaults <- readPackagedDefaults
        let overlay =
                "{\"version\":1,\"connections\":{\"local\":{\"api\":\"responses\",\"base_url\":\"ftp://localhost/v1\",\"api_key_env\":\"BAD-NAME\",\"request_timeout_seconds\":0}}}"
        err <- expectLeft
            (mergeModelConfigs
                ("models.default.json", defaults)
                (Just ("models.json", overlay)))
        err `shouldBe` Text.intercalate "\n"
            [ "connection local base_url must start with http:// or https://"
            , "connection local request_timeout_seconds must be positive"
            , "connection local has invalid api_key_env BAD-NAME"
            ]

    it "reports independent model validation errors together in check order" do
        defaults <- readPackagedDefaults
        let overlay =
                "{\"version\":1,\"connections\":{\"local\":{\"api\":\"responses\",\"base_url\":\"http://localhost/v1\",\"api_key_optional\":true}},\"models\":[{\"id\":\"local-model\",\"connection\":\"local\",\"dialect\":\"generic-responses\",\"fallback_priority\":-1,\"context_window\":0,\"reasoning_efforts\":[\"turbo\",\"turbo\"],\"default_reasoning_effort\":\"high\"}]}"
        err <- expectLeft
            (mergeModelConfigs
                ("models.default.json", defaults)
                (Just ("models.json", overlay)))
        err `shouldBe` Text.intercalate "\n"
            [ "model local-model fallback_priority must not be negative"
            , "model local-model context_window must be positive"
            , "model local-model has unsupported reasoning_efforts; expected none, low, medium, high, xhigh, max"
            , "model local-model reasoning_efforts must not contain duplicates"
            , "model local-model default_reasoning_effort must be listed in reasoning_efforts"
            ]

    it "requires an API-key environment variable unless auth is optional" do
        defaults <- readPackagedDefaults
        let overlay =
                "{\"version\":1,\"connections\":{\"local\":{\"api\":\"responses\",\"base_url\":\"http://localhost/v1\"}}}"
        mergeModelConfigs
            ("models.default.json", defaults)
            (Just ("models.json", overlay))
            `shouldSatisfy` leftContains "requires api_key_env"

    it "rejects non-positive model context windows" do
        defaults <- readPackagedDefaults
        let overlay =
                "{\"version\":1,\"connections\":{\"local\":{\"api\":\"responses\",\"base_url\":\"http://localhost/v1\",\"api_key_optional\":true}},\"models\":[{\"id\":\"local\",\"connection\":\"local\",\"dialect\":\"generic-responses\",\"context_window\":0}]}"
        mergeModelConfigs
            ("models.default.json", defaults)
            (Just ("models.json", overlay))
            `shouldSatisfy` leftContains "context_window must be positive"

    it "rejects invalid or duplicate reasoning efforts" do
        defaults <- readPackagedDefaults
        let invalid effortFields =
                LBS.pack $
                    "{\"version\":1,\"models\":[{\"id\":\"gpt-5.6-sol\","
                    <> "\"connection\":\"openai\",\"dialect\":\"codex\","
                    <> "\"default\":true,"
                    <> effortFields
                    <> "}]}"
        mergeModelConfigs
            ("models.default.json", defaults)
            (Just
                ( "models.json"
                , invalid "\"reasoning_efforts\":[\"medium\",\"turbo\"]"
                ))
            `shouldSatisfy` leftContains "unsupported reasoning_efforts"
        mergeModelConfigs
            ("models.default.json", defaults)
            (Just
                ( "models.json"
                , invalid "\"reasoning_efforts\":[\"high\",\"high\"]"
                ))
            `shouldSatisfy` leftContains "must not contain duplicates"

    it "requires a model reasoning default to be advertised" do
        defaults <- readPackagedDefaults
        let overlay =
                "{\"version\":1,\"models\":[{\"id\":\"gpt-5.6-sol\",\"connection\":\"openai\",\"dialect\":\"codex\",\"reasoning_efforts\":[\"low\",\"medium\"],\"default_reasoning_effort\":\"high\",\"default\":true}]}"
        mergeModelConfigs
            ("models.default.json", defaults)
            (Just ("models.json", overlay))
            `shouldSatisfy`
                leftContains
                    "default_reasoning_effort must be listed"

    it "loads ~/.haskell-agent/models.json over an explicit default path" $
        withTempDirectory "agent-model-config-" \root -> do
            defaults <- readPackagedDefaults
            let defaultPath =
                    unsafeToFilePath (root </> fromText "defaults.json")
                configDir = root </> fromText ".haskell-agent"
                userPath = configDir </> fromText "models.json"
            createDirectoryIfMissing True (unsafeToFilePath configDir)
            LBS.writeFile defaultPath defaults
            LBS.writeFile (unsafeToFilePath userPath) $
                "{\"version\":1,\"connections\":{\"local\":{\"api\":\"responses\",\"base_url\":\"http://localhost:8000/v1\",\"api_key_optional\":true}},\"models\":[{\"id\":\"local-model\",\"connection\":\"local\",\"dialect\":\"generic-responses\"}]}"
            loaded <- loadModelCatalogWith defaultPath root
            catalog <- expectRight loaded
            fmap (.catalogModelWireId)
                (filter ((== "local-model") . (.catalogModelId))
                    catalog.catalogModels)
                `shouldBe` ["local-model"]

leftContains :: Text -> Either Text value -> Bool
leftContains needle = \case
    Left err -> needle `Text.isInfixOf` err
    Right _ -> False

findModel :: Text -> [CatalogModel] -> Maybe CatalogModel
findModel modelId = go
  where
    go = \case
        [] -> Nothing
        model : rest
            | model.catalogModelId == modelId -> Just model
            | otherwise -> go rest

expectRight :: Either Text value -> IO value
expectRight = \case
    Left err -> do
        expectationFailure (Text.unpack err)
        fail "expected Right"
    Right value -> pure value

expectLeft :: Either Text value -> IO Text
expectLeft = \case
    Left err -> pure err
    Right _ -> do
        expectationFailure "expected Left"
        fail "expected Left"

readPackagedDefaults :: IO LBS.ByteString
readPackagedDefaults =
    packagedModelCatalogPath >>= LBS.readFile

withTempDirectory :: String -> (OsPath -> IO a) -> IO a
withTempDirectory template action = do
    base <- getTemporaryDirectory
    bracket
        (do
            (path, handle) <- openTempFile base template
            hClose handle
            removePathForcibly path
            createDirectory path
            pure (fromText (Text.pack path)))
        (removePathForcibly . unsafeToFilePath)
        action
