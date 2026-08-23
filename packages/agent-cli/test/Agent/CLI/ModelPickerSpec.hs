module Agent.CLI.ModelPickerSpec (spec) where

import Agent.CLI.ModelPicker
import Agent.CLI.Models
import Agent.CLI.ModelConfig
    ( ModelCatalog
    , decodeModelConfig
    , packagedModelCatalogPath
    )
import Agent.Dialect (DialectId(..))
import Agent.Provider (Provider(..))
import Control.Exception.Safe (bracket)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec

spec :: Spec
spec = do
    catalog <- runIO readPackagedCatalog
    describe "decodePickerKey" do
        it "maps arrows and vim keys" do
            decodePickerKey "\ESC[A" `shouldBe` Just PickerUp
            decodePickerKey "\ESC[B" `shouldBe` Just PickerDown
            decodePickerKey "k" `shouldBe` Just PickerUp
            decodePickerKey "j" `shouldBe` Just PickerDown

        it "maps confirm and cancel" do
            decodePickerKey "\n" `shouldBe` Just PickerConfirm
            decodePickerKey "\r" `shouldBe` Just PickerConfirm
            decodePickerKey "\ESC" `shouldBe` Just PickerCancel
            decodePickerKey "q" `shouldBe` Just PickerCancel

        it "maps filter editing" do
            decodePickerKey "\DEL" `shouldBe` Just PickerBackspace
            decodePickerKey "g" `shouldBe` Just (PickerType 'g')

    describe "renderPickerFrame" do
        it "mentions all providers, current model, and controls" do
            let frame =
                    renderPickerFrame False $
                        initialPickerState
                            catalog
                            "xai"
                            XAIProvider
                            "grok-4.6"
                            GrokBuildDialect
            frame `shouldSatisfy` Text.isInfixOf "xai"
            frame `shouldSatisfy` Text.isInfixOf "openai"
            frame `shouldSatisfy` Text.isInfixOf "openrouter"
            frame `shouldSatisfy` Text.isInfixOf "claude-code"
            defaultModelFor catalog XAIProvider
                `shouldSatisfy` maybe False (\model -> Text.isInfixOf model frame)
            frame `shouldSatisfy` Text.isInfixOf "grok-4.6"
            frame `shouldSatisfy` Text.isInfixOf "enter"
            frame `shouldSatisfy` Text.isInfixOf "filter"

    describe "formatCatalogListing" do
        it "lists the current model and entries from every provider" do
            listing <-
                formatCatalogListing
                    catalog
                    False
                    "openai"
                    OpenAIProvider
                    "gpt-5.6-sol"
                    CodexDialect
            listing `shouldSatisfy` Text.isInfixOf "gpt-5.6-sol"
            listing `shouldSatisfy` Text.isInfixOf "gpt-5.6-terra"
            listing `shouldSatisfy` Text.isInfixOf "gpt-5.6-luna"
            listing `shouldSatisfy` Text.isInfixOf "openai"
            listing `shouldSatisfy` Text.isInfixOf "grok-4.6"
            listing `shouldSatisfy` Text.isInfixOf "openrouter"
            listing `shouldSatisfy` Text.isInfixOf "claude-code"

        it "shows the dialect of OpenRouter's mapped transport model" do
            withEnv
                "OPENROUTER_MODEL_MAP"
                (Just "openai/gpt-5.1=x-ai/grok-4") do
                    listing <-
                        formatCatalogListing
                            catalog
                            False
                            "openrouter"
                            OpenRouterProvider
                            "openai/gpt-5.1"
                            GrokBuildDialect
                    listing `shouldSatisfy`
                        Text.isInfixOf
                            "openrouter · openai/gpt-5.1 · grok-build"

withEnv :: String -> Maybe String -> IO a -> IO a
withEnv name value action =
    bracket (lookupEnv name) restore \_ -> set value >> action
  where
    set = \case
        Nothing -> unsetEnv name
        Just x -> setEnv name x
    restore = \case
        Nothing -> unsetEnv name
        Just x -> setEnv name x

readPackagedCatalog :: IO ModelCatalog
readPackagedCatalog = do
    path <- packagedModelCatalogPath
    bytes <- LBS.readFile path
    case decodeModelConfig "models.default.json" bytes of
        Left err -> fail (Text.unpack err)
        Right catalog -> pure catalog
