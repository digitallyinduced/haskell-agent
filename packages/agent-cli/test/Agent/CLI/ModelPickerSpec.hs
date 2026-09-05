module Agent.CLI.ModelPickerSpec (spec) where

import Agent.CLI.ModelPicker
import Agent.CLI.Models
import Agent.CLI.ModelConfig
    ( ModelCatalog
    , decodeModelConfig
    , organizationGatewayConnectionId
    , packagedModelCatalogPath
    )
import Agent.Dialect (DialectId(..))
import Agent.Provider (Provider(..))
import Agent.ReasoningEffort (ReasoningEffort(..))
import Control.Exception.Safe (bracket)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec

spec :: Spec
spec = do
    catalog <- runIO readPackagedCatalog
    describe "decodePickerKey" do
        it "maps arrows while keeping printable keys available to search" do
            decodePickerKey "\ESC[A" `shouldBe` Just PickerUp
            decodePickerKey "\ESC[B" `shouldBe` Just PickerDown
            decodePickerKey "\ESC[D" `shouldBe` Just PickerLeft
            decodePickerKey "\ESC[C" `shouldBe` Just PickerRight
            decodePickerKey "k" `shouldBe` Just (PickerType 'k')
            decodePickerKey "j" `shouldBe` Just (PickerType 'j')

        it "maps confirm and cancel" do
            decodePickerKey "\n" `shouldBe` Just PickerConfirm
            decodePickerKey "\r" `shouldBe` Just PickerConfirm
            decodePickerKey "\ESC" `shouldBe` Just PickerCancel
            decodePickerKey "q" `shouldBe` Just (PickerType 'q')

        it "maps filter editing" do
            decodePickerKey "\DEL" `shouldBe` Just PickerBackspace
            decodePickerKey [toEnum 127] `shouldBe` Just PickerBackspace
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
            frame `shouldSatisfy` Text.isInfixOf "confirm"
            frame `shouldSatisfy` Text.isInfixOf "reasoning effort"
            frame `shouldSatisfy` Text.isInfixOf "Type to search"

        it "keeps large live catalogs inside a scrolling viewport" do
            let options =
                    [ rawModelOption
                        OpenRouterProvider
                        ("vendor/model-" <> Text.pack (show index))
                    | index <- [0 .. 29 :: Int]
                    ]
                state =
                    (initialPickerState
                        catalog
                        "openrouter"
                        OpenRouterProvider
                        "vendor/model-20"
                        GenericResponsesDialect)
                        { pickerAll = options
                        , pickerIndex = 20
                        }
                frame = renderPickerFrame False state
            length (Text.lines frame) `shouldBe` 21
            frame `shouldSatisfy` Text.isInfixOf "vendor/model-20"
            frame `shouldNotSatisfy` Text.isInfixOf "vendor/model-0 "

        it "adjusts effort for the selected model without changing models" do
            let models =
                    initialPickerState
                        catalog
                        "xai"
                        XAIProvider
                        "grok-4.6"
                        GrokBuildDialect
                state0 = initialModelPickerState EffortMedium models
            state1 <- rightState (applyModelPickerEvent PickerRight state0)
            case applyModelPickerEvent PickerConfirm state1 of
                Left (Just selection) -> do
                    selection.modelPickerOption.modelTarget.targetModelId
                        `shouldBe` "grok-4.6"
                    selection.modelPickerEffort `shouldBe` EffortHigh
                other ->
                    expectationFailure
                        ("expected confirmed selection, got " <> show other)

        it "renders effort gauges and selected model metadata" do
            let models =
                    initialPickerState
                        catalog
                        "xai"
                        XAIProvider
                        "grok-4.6"
                        GrokBuildDialect
                frame =
                    renderModelPickerFrame False
                        (initialModelPickerState EffortHigh models)
            frame `shouldSatisfy` Text.isInfixOf "■■■□□ high"
            frame `shouldSatisfy` Text.isInfixOf "←"
            frame `shouldSatisfy` Text.isInfixOf "→"
            frame `shouldSatisfy` Text.isInfixOf "context"

        it "shows gateway aliases without the internal connection prefix" do
            let baseOption =
                    rawModelOption OpenAIProvider "gpt-5.6-sol"
                option =
                    baseOption
                        { modelTarget =
                            baseOption.modelTarget
                                { targetConnectionId =
                                    organizationGatewayConnectionId
                                }
                        }
            models <-
                initialPickerStateForOptions
                    "organization gateway"
                    [option]
                    organizationGatewayConnectionId
                    OpenAIProvider
                    "gpt-5.6-sol"
                    CodexDialect
            let frame = renderPickerFrame False models
            frame `shouldSatisfy` Text.isInfixOf "gpt-5.6-sol"
            frame `shouldNotSatisfy`
                Text.isInfixOf
                    (organizationGatewayConnectionId <> "/gpt-5.6-sol")

        it "makes untrusted catalog control characters inert" do
            let hostile =
                    (rawModelOption
                        OpenRouterProvider
                        "vendor/evil\n\ESC]8;;https://example.test\BEL")
                        { modelLabel = Just "label\r\ESC[2J" }
                models =
                    (initialPickerState
                        catalog
                        "openrouter"
                        OpenRouterProvider
                        hostile.modelTarget.targetModelId
                        GenericResponsesDialect)
                        { pickerAll = [hostile] }
                frame =
                    renderPickerFrame False models
            frame `shouldNotSatisfy` Text.isInfixOf "\ESC"
            frame `shouldNotSatisfy` Text.isInfixOf "\BEL"
            length (Text.lines frame) `shouldBe` 21

        it "does not display a stale current model outside an authoritative scope" do
            state <-
                initialPickerStateForOptions
                    "organization gateway"
                    [rawModelOption OpenAIProvider "company-model"]
                    "openai"
                    OpenAIProvider
                    "revoked-model"
                    CodexDialect
            let frame = renderPickerFrame False state
            frame `shouldSatisfy` Text.isInfixOf "organization gateway"
            frame `shouldSatisfy` Text.isInfixOf "company-model"
            frame `shouldNotSatisfy` Text.isInfixOf "revoked-model"

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
            listing `shouldSatisfy` Text.isInfixOf "gpt-6-astra"
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

rightState
    :: Either result ModelPickerState
    -> IO ModelPickerState
rightState = \case
    Right state -> pure state
    Left _ -> expectationFailure "expected picker state" >> fail "unreachable"
