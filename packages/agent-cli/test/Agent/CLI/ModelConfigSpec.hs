module Agent.CLI.ModelConfigSpec (spec) where

import Agent.CLI.ModelConfig
import Agent.Dialect (DialectId(..))
import Agent.OsPath (fromText, unsafeToFilePath)
import Agent.Provider (Provider(..))
import Control.Exception.Safe (bracket)
import qualified Data.ByteString.Lazy.Char8 as LBS
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
            `shouldBe` Just "gpt-5.6-luna"
        fmap (.catalogModelId)
            (catalogDefaultForProvider catalog XAIProvider)
            `shouldBe` Just "grok-4.6"
        fmap (.catalogModelId)
            (catalogDefaultForProvider catalog OpenRouterProvider)
            `shouldBe` Just "openai/gpt-5.1"

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

    it "replaces shipped model metadata by stable id without moving entries" do
        defaults <- readPackagedDefaults
        let overlay = LBS.pack $ unlines
                [ "{"
                , "  \"version\": 1,"
                , "  \"models\": [{"
                , "    \"id\": \"gpt-5.6-luna\","
                , "    \"connection\": \"openai\","
                , "    \"model\": \"gpt-5.6-luna\","
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
                filter ((== "gpt-5.6-luna") . (.catalogModelId))
                    catalog.catalogModels
        length matching `shouldBe` 1
        fmap (.catalogModelWireId) matching
            `shouldBe` ["gpt-5.6-luna"]
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

    it "reports invalid custom references and authentication variables" do
        defaults <- readPackagedDefaults
        let overlay =
                "{\"version\":1,\"connections\":{\"local\":{\"api\":\"responses\",\"base_url\":\"http://localhost/v1\",\"api_key_env\":\"BAD-NAME\"}},\"models\":[{\"id\":\"local\",\"connection\":\"missing\",\"dialect\":\"generic-responses\"}]}"
        mergeModelConfigs
            ("models.default.json", defaults)
            (Just ("models.json", overlay))
            `shouldSatisfy` leftContains "invalid api_key_env"

    it "requires an API-key environment variable unless auth is optional" do
        defaults <- readPackagedDefaults
        let overlay =
                "{\"version\":1,\"connections\":{\"local\":{\"api\":\"responses\",\"base_url\":\"http://localhost/v1\"}}}"
        mergeModelConfigs
            ("models.default.json", defaults)
            (Just ("models.json", overlay))
            `shouldSatisfy` leftContains "requires api_key_env"

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

expectRight :: Either Text value -> IO value
expectRight = \case
    Left err -> do
        expectationFailure (Text.unpack err)
        fail "expected Right"
    Right value -> pure value

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
