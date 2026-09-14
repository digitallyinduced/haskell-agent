module Agent.Runtime.Startup.ModelSpec (spec) where

import Agent.Dialect (DialectId(..))
import Agent.Provider (Provider(..))
import Agent.ReasoningEffort (ReasoningEffort(..))
import Agent.Runtime.ModelConfig
    ( ModelCatalog, decodeModelConfig, mergeModelConfigs, packagedModelCatalogPath, builtinConnectionId )
import Agent.Runtime.Models (ModelOption(..), ModelTarget(..), defaultModelFor, rawModelOption)
import Agent.Runtime.Startup.Model
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "startup model resolution" do
    catalog <- runIO do
        path <- packagedModelCatalogPath
        bytes <- LBS.readFile path
        either (fail . Text.unpack) pure (decodeModelConfig "models.default.json" bytes)
    let inputs = defaults catalog
        base = (rawModelOption OpenAIProvider "selected").modelTarget
        resumed = ResumedModel OpenAIProvider (builtinConnectionId OpenAIProvider)
            "selected" CodexDialect (Just "selected") "high"
        resuming = inputs{startupTargetHint = Just base, startupResumedModel = Just resumed}
    it "uses the catalog default without an explicit model or hint" do
        (resolveStartupModel inputs).resolvedModel `shouldBe` defaultModelFor catalog OpenAIProvider
    it "uses a resolved target hint before the catalog default" do
        (resolveStartupModel inputs{startupTargetHint = Just base}).resolvedModel `shouldBe` "selected"
    it "uses an explicit model before the target hint" do
        (resolveStartupModel inputs{startupRequestedModel = Just "explicit", startupTargetHint = Just base}).resolvedModel
            `shouldBe` "explicit"
    it "uses the admitted gateway target and dialect before local overrides" do
        let gateway = (rawModelOption OpenAIProvider "gateway")
                {modelTarget = base{targetModelId = "gateway", targetWireModelId = "gateway", targetDialect = GenericResponsesDialect}}
            result = resolveStartupModel resuming
                { startupRequestedModel = Just "explicit"
                , startupGatewaySelection = Just gateway
                , startupTransitionTarget = Just base
                }
        result.resolvedModel `shouldBe` "gateway"
        result.resolvedTarget `shouldBe` gateway.modelTarget
        result.resolvedDialect `shouldBe` GenericResponsesDialect
    it "retains persisted dialect and effort when resuming an unchanged target" do
        let result = resolveStartupModel resuming
        result.resolvedDialect `shouldBe` CodexDialect
        result.resolvedEffort `shouldBe` EffortHigh
        result.resolvedResumeTargetChanged `shouldBe` False
        result.resolvedRefreshDialectContext `shouldBe` False
    it "prefers a transition dialect over persisted dialect" do
        let result = resolveStartupModel resuming
                {startupTransitionTarget = Just base{targetDialect = GenericResponsesDialect}}
        result.resolvedDialect `shouldBe` GenericResponsesDialect
        result.resolvedResumeTargetChanged `shouldBe` True
        result.resolvedRefreshDialectContext `shouldBe` True
    it "invalidates a resumed target when its wire mapping changes" do
        let result = resolveStartupModel resuming
                {startupTargetHint = Just base{targetWireModelId = "new-wire", targetDialect = GenericResponsesDialect}}
        result.resolvedDialect `shouldBe` GenericResponsesDialect
        result.resolvedResumeTargetChanged `shouldBe` True
        result.resolvedRefreshDialectContext `shouldBe` True
    it "invalidates a connection change without unnecessarily refreshing dialect context" do
        let result = resolveStartupModel resuming
                {startupTargetHint = Just base{targetConnectionId = "other"}}
        result.resolvedResumeTargetChanged `shouldBe` True
        result.resolvedRefreshDialectContext `shouldBe` False
    it "only inherits a remembered project's dialect for the same provider" do
        let remembered = base{targetDialect = GenericResponsesDialect}
            result target = resolveStartupModel inputs
                {startupTargetHint = Just base, startupRememberedTarget = Just target}
        (result remembered).resolvedDialect `shouldBe` GenericResponsesDialect
        (result remembered{targetProvider = XAIProvider}).resolvedDialect `shouldBe` CodexDialect
    it "an explicit selection replaces the remembered dialect" do
        (resolveStartupModel resuming
            { startupRequestedModel = Just "selected"
            , startupResumedModel = Just resumed{resumedDialect = GenericResponsesDialect}
            }).resolvedDialect `shouldBe` CodexDialect
    it "an invalid saved effort falls back to the active provider default" do
        (resolveStartupModel resuming
            {startupResumedModel = Just resumed{resumedEffort = "invalid"}}).resolvedEffort
            `shouldBe` EffortMedium
    it "explicit effort overrides valid resumed effort" do
        (resolveStartupModel resuming{startupRequestedEffort = Just EffortLow}).resolvedEffort
            `shouldBe` EffortLow
    mapM_ (\(provider, expected) ->
        it ("invalid saved effort uses the new default after switching to " <> show provider) do
            let result = resolveStartupModel inputs
                    { startupProvider = provider
                    , startupTargetHint = Just (rawModelOption provider "switched").modelTarget
                    , startupResumedModel = Just resumed{resumedEffort = "invalid"}
                    }
            result.resolvedEffort `shouldBe` expected
            result.resolvedResumeTargetChanged `shouldBe` True)
        [(ClaudeCodeProvider, EffortXHigh), (XAIProvider, EffortHigh)]
    it "resumed dialect wins over a remembered project dialect" do
        (resolveStartupModel resuming
            {startupRememberedTarget = Just base{targetDialect = GenericResponsesDialect}}).resolvedDialect
            `shouldBe` CodexDialect
    it "normalizes an explicitly inherited max effort for Grok" do
        (resolveStartupModel inputs
            { startupProvider = XAIProvider
            , startupRequestedEffort = Just EffortMax
            , startupTargetHint = Just (rawModelOption XAIProvider "grok").modelTarget{targetDialect = GrokBuildDialect}
            }).resolvedEffort `shouldBe` EffortHigh
    it "maps built-in OpenRouter wire targets without changing their public model id" do
        let target = (rawModelOption OpenRouterProvider "alias").modelTarget
            result = resolveStartupModel inputs
                {startupProvider = OpenRouterProvider, startupTargetHint = Just target, startupOpenRouterMap = const "wire"}
        result.resolvedModel `shouldBe` "alias"
        result.resolvedTarget.targetWireModelId `shouldBe` "wire"
        result.resolvedTransportModel "child" `shouldBe` "wire"
    it "preserves an already resolved OpenRouter wire target" do
        let target = (rawModelOption OpenRouterProvider "alias").modelTarget{targetWireModelId = "pinned"}
        (resolveStartupModel inputs
            {startupProvider = OpenRouterProvider, startupTargetHint = Just target, startupOpenRouterMap = const "changed"}
            ).resolvedTarget.targetWireModelId `shouldBe` "pinned"
    it "custom response mappings retain unknown child names and the selected exact wire id" do
        let target = base{targetConnectionId = "custom", targetWireModelId = "exact-wire"}
            result = resolveStartupModel inputs{startupTargetHint = Just target, startupCustomResponses = True}
        result.resolvedTransportModel "selected" `shouldBe` "exact-wire"
        result.resolvedTransportModel "unknown-child" `shouldBe` "unknown-child"
    it "maps configured children only within the selected custom connection" do
        path <- packagedModelCatalogPath
        bytes <- LBS.readFile path
        let overlay = "{\"version\":1,\"connections\":{\"custom\":{\"api\":\"responses\",\"base_url\":\"http://localhost:8000/v1\",\"api_key_optional\":true},\"other\":{\"api\":\"responses\",\"base_url\":\"http://localhost:8001/v1\",\"api_key_optional\":true}},\"models\":[{\"id\":\"child\",\"connection\":\"custom\",\"model\":\"child-wire\",\"dialect\":\"generic-responses\"},{\"id\":\"foreign\",\"connection\":\"other\",\"model\":\"foreign-wire\",\"dialect\":\"generic-responses\"}]}"
        customCatalog <- either (fail . Text.unpack) pure
            (mergeModelConfigs ("defaults.json", bytes) (Just ("custom.json", overlay)))
        let target = base{targetConnectionId = "custom", targetWireModelId = "exact-wire"}
            result = resolveStartupModel inputs
                {startupCatalog = customCatalog, startupTargetHint = Just target, startupCustomResponses = True}
        result.resolvedTransportModel "child" `shouldBe` "child-wire"
        result.resolvedTransportModel "foreign" `shouldBe` "foreign"

defaults :: ModelCatalog -> ModelStartupInputs
defaults catalog = ModelStartupInputs
    { startupProvider = OpenAIProvider
    , startupCatalog = catalog
    , startupRequestedModel = Nothing
    , startupTargetHint = Nothing
    , startupGatewaySelection = Nothing
    , startupTransitionTarget = Nothing
    , startupResumedModel = Nothing
    , startupRememberedTarget = Nothing
    , startupRequestedEffort = Nothing
    , startupCustomResponses = False
    , startupOpenRouterMap = id
    }
