module Agent.CLI.GatewayModelsSpec (spec) where

import Agent.CLI.GatewayModels
import Agent.CLI.GatewayClient
    ( GatewayModel(..)
    , GatewayModelProtocol(..)
    , GatewayModelProvider(..)
    , GatewayModelAccess
    , newGatewayModelAccessWithUsage
    , newGatewayModelAccessWith
    , cachedGatewayModels
    , refreshGatewayModels
    , fetchGatewayUsage
    )
import Agent.CLI.AgentViewport (AgentTarget(AgentRoot))
import Agent.CLI.Interrupt (CtrlCDecision(WarnExit))
import Agent.CLI.ModelConfig
import Agent.CLI.ModelPicker
    ( ModelPickerSelection(..), ModelPickerState(..), initialModelPickerState
    , refreshModelPickerState, applyModelPickerEvent )
import Agent.CLI.Models
    ( ModelOption(..), ModelTarget(..), PickerState(..), PickerEvent(..)
    , initialPickerStateForOptions, selectedOption )
import Agent.CLI.Session.Choices (modelChoiceWithEffort)
import Agent.CLI.TUI.App (newFullscreenInputBuffer, newFullscreenRuntime)
import Agent.CLI.TUI.Types
    ( AppEvent(..)
    , AppEventMailbox(..)
    , AppEventMailboxState(..)
    , FullscreenRuntime(..)
    , PendingAppEvent(..)
    )
import Agent.TUI.Model (initialUiState)
import Agent.TUI.Motion (MotionMode(MotionFull))
import Agent.Dialect (DialectId(..))
import Agent.OpenAI.Usage (UsageSnapshot(..), UsageLimit(..), UsageWindow(..))
import Agent.Provider (Provider(ClaudeCodeProvider, GeminiProvider, OpenAIProvider, XAIProvider))
import Agent.ReasoningEffort (ReasoningEffort(EffortHigh))
import Control.Concurrent.Async (withAsync, wait)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Concurrent.STM (atomically, putTMVar, readTVar, retry)
import Control.Exception.Safe (finally)
import Data.Aeson qualified as Aeson
import Data.Aeson ((.=))
import Data.Either (isLeft)
import Data.Foldable (toList)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import System.Timeout (timeout)
import Test.Hspec

newPickerRuntime :: IO FullscreenRuntime
newPickerRuntime = do
    input <- newFullscreenInputBuffer
    newFullscreenRuntime
        input
        (pure ())
        (const (pure ()))
        (pure WarnExit)
        (const (pure True))
        (const (pure ()))
        (const (pure ()))
        (pure (AgentRoot, []))
        (const (pure ()))
        (pure ())
        (const (pure ()))
        MotionFull
        False
        initialUiState

openGatewayPicker
    :: FullscreenRuntime
    -> GatewayModelAccess
    -> IO (Either Text (Maybe ModelPickerSelection))
openGatewayPicker runtime access =
    modelChoiceWithEffort
        testCatalog (Just access) (Just runtime) False
        organizationGatewayConnectionId OpenAIProvider "company-a"
        CodexDialect EffortHigh

awaitPickerAction :: IO a -> IO a
awaitPickerAction action =
    timeout 1_000_000 action >>= maybe
        (fail "Model picker operation did not complete while network requests were blocked")
        pure

awaitPickerEvent :: FullscreenRuntime -> (AppEvent -> Maybe a) -> IO a
awaitPickerEvent runtime project =
    awaitPickerAction $ atomically do
        let AppEventMailbox mailbox = runtime.runtimeMailbox
        pending <- readTVar mailbox
        case
            [ value
            | PendingEvent event <- toList pending.mailboxPendingEvents
            , Just value <- [project event]
            ] of
            value : _ -> pure value
            [] -> retry

pickerModel :: Text -> GatewayModel
pickerModel model = GatewayModel model GatewayResponsesProtocol GatewayOpenAIProvider

pickerUsage :: UsageSnapshot
pickerUsage = UsageSnapshot
    { planType = "plus"
    , rateLimit = Just UsageLimit
        { allowed = True
        , limitReached = False
        , primaryWindow = Just UsageWindow
            { usedPercent = 25
            , limitWindowSeconds = 18000
            , resetAfterSeconds = 9000
            , resetAt = 1783880000
            }
        , secondaryWindow = Nothing
        }
    , additionalRateLimits = []
    }

spec :: Spec
spec = describe "Agent.CLI.GatewayModels" do
    describe "cached gateway startup" do
        it "enters the runtime before warm refresh completes and joins it on exit" do
            let models = [pickerModel "company-a"]
            fetch <- newIORef (pure (Right models))
            started <- newEmptyMVar
            stopped <- newEmptyMVar
            gate <- newEmptyMVar
            access <- newGatewayModelAccessWith (readIORef fetch >>= id)
            refreshGatewayModels access `shouldReturn` Right models
            writeIORef fetch $
                (putMVar started () >> takeMVar gate)
                    `finally` putMVar stopped ()
            awaitPickerAction $
                withGatewayModelsForStartup access Right \selected -> do
                    selected `shouldBe` Right models
                    takeMVar started
                    tryTakeMVar stopped `shouldReturn` Nothing
            tryTakeMVar stopped `shouldReturn` Just ()

        it "waits for the first authoritative catalog on a cold start" do
            let models = [pickerModel "company-a"]
            started <- newEmptyMVar
            entered <- newEmptyMVar
            gate <- newEmptyMVar
            access <- newGatewayModelAccessWith (putMVar started () >> takeMVar gate)
            withAsync
                (withGatewayModelsForStartup access Right \selected ->
                    putMVar entered selected >> pure selected) \worker -> do
                    awaitPickerAction (takeMVar started)
                    tryTakeMVar entered `shouldReturn` Nothing
                    putMVar gate (Right models)
                    awaitPickerAction (wait worker) `shouldReturn` Right models
                    takeMVar entered `shouldReturn` Right models

        it "refreshes before rejecting an explicit alias absent from the cache" do
            let original = [pickerModel "company-a"]
                updated = original <> [pickerModel "company-new"]
                select models =
                    fmap (.modelTarget.targetModelId) $
                        selectGatewayModelOption
                            (modelOptionsForGatewayModels testCatalog models)
                            (Just "company-new") Nothing []
            fetch <- newIORef (pure (Right original))
            access <- newGatewayModelAccessWith (readIORef fetch >>= id)
            refreshGatewayModels access `shouldReturn` Right original
            writeIORef fetch (pure (Right updated))
            withGatewayModelsForStartup access select pure
                `shouldReturn` Right "company-new"
            cachedGatewayModels access `shouldReturn` Just updated

        it "refreshes an empty cache before deciding that no models are available" do
            let updated = [pickerModel "company-a"]
                select models =
                    fmap (.modelTarget.targetModelId) $
                        selectGatewayModelOption
                            (modelOptionsForGatewayModels testCatalog models)
                            Nothing Nothing []
            fetch <- newIORef (pure (Right []))
            access <- newGatewayModelAccessWith (readIORef fetch >>= id)
            refreshGatewayModels access `shouldReturn` Right []
            writeIORef fetch (pure (Right updated))
            withGatewayModelsForStartup access select pure
                `shouldReturn` Right "company-a"

        it "reports cold-start transport failure without selecting a model" do
            access <- newGatewayModelAccessWith (pure (Left "gateway unavailable"))
            withGatewayModelsForStartup access Right pure
                `shouldReturn` Left "gateway unavailable"
            cachedGatewayModels access `shouldReturn` Nothing

        it "keeps the warm selection and cache when background transport fails" do
            let models = [pickerModel "company-a"]
            fetch <- newIORef (pure (Right models))
            started <- newEmptyMVar
            access <- newGatewayModelAccessWith (readIORef fetch >>= id)
            refreshGatewayModels access `shouldReturn` Right models
            writeIORef fetch (putMVar started () >> pure (Left "gateway unavailable"))
            awaitPickerAction
                (withGatewayModelsForStartup access Right \selected ->
                    takeMVar started >> pure selected)
                `shouldReturn` Right models
            cachedGatewayModels access `shouldReturn` Just models

    describe "cached model picker" do
        it "preserves minimal-picker filter, focus, and effort during a catalog update" do
            let options = modelOptionsForGatewayModels testCatalog
                    [pickerModel "company-a", pickerModel "company-b"]
            models <- initialPickerStateForOptions "organization gateway" options
                organizationGatewayConnectionId OpenAIProvider "company-a" CodexDialect
            let original = initialModelPickerState EffortHigh
                    models { pickerFilter = "company", pickerIndex = 1 }
                adjusted = either (error . show) id (applyModelPickerEvent PickerRight original)
                updated = refreshModelPickerState EffortHigh
                    models { pickerAll = reverse models.pickerAll }
                    (Map.singleton "company-b" "5h 75% left") adjusted
            updated.modelPickerModels.pickerFilter `shouldBe` "company"
            fmap (.modelTarget.targetModelId) (selectedOption updated.modelPickerModels)
                `shouldBe` Just "company-b"
            updated.modelPickerEfforts `shouldBe` adjusted.modelPickerEfforts
            updated.modelPickerUsage `shouldBe` Map.singleton "company-b" "5h 75% left"

        it "includes cached usage in the first rows without awaiting usage refresh" do
            runtime <- newPickerRuntime
            usageFetch <- newIORef (pure (Right pickerUsage))
            gate <- newEmptyMVar
            access <- newGatewayModelAccessWithUsage
                (pure (Right [pickerModel "company-a"]))
                (\_ -> readIORef usageFetch >>= id)
            refreshGatewayModels access `shouldReturn` Right [pickerModel "company-a"]
            _ <- fetchGatewayUsage access "company-a"
            writeIORef usageFetch (takeMVar gate)
            withAsync (openGatewayPicker runtime access) \worker -> do
                (rows, reply) <- awaitPickerEvent runtime \case
                    AppAskDynamicAdjustableFilterChoice _ _ _ rows reply -> Just (rows, reply)
                    _ -> Nothing
                [label | (_, label, _, _, _) <- rows]
                    `shouldBe` ["company-a ✓  5h 75% left"]
                atomically (putTMVar reply Nothing)
                awaitPickerAction (wait worker) `shouldReturn` Right Nothing

        it "opens cached rows while both network requests are blocked and joins them on cancel" do
            runtime <- newPickerRuntime
            fetch <- newIORef (pure (Right [pickerModel "company-a"]))
            modelStarted <- newEmptyMVar
            modelStopped <- newEmptyMVar
            usageStarted <- newEmptyMVar
            usageStopped <- newEmptyMVar
            modelGate <- newEmptyMVar
            usageGate <- newEmptyMVar
            access <- newGatewayModelAccessWithUsage
                (readIORef fetch >>= id)
                (\_ ->
                    (putMVar usageStarted () >> takeMVar usageGate)
                        `finally` putMVar usageStopped ())
            refreshGatewayModels access `shouldReturn` Right [pickerModel "company-a"]
            writeIORef fetch $
                (putMVar modelStarted () >> takeMVar modelGate)
                    `finally` putMVar modelStopped ()
            withAsync (openGatewayPicker runtime access) \worker -> do
                (body, rows, reply) <- awaitPickerEvent runtime \case
                    AppAskDynamicAdjustableFilterChoice _ body _ rows reply ->
                        Just (body, rows, reply)
                    _ -> Nothing
                body `shouldBe` ""
                [label | (_, label, _, _, _) <- rows] `shouldBe` ["company-a ✓"]
                awaitPickerAction (takeMVar modelStarted)
                awaitPickerAction (takeMVar usageStarted)
                atomically (putTMVar reply Nothing)
                awaitPickerAction (wait worker) `shouldReturn` Right Nothing
                awaitPickerAction (takeMVar modelStopped)
                awaitPickerAction (takeMVar usageStopped)

        it "opens a cancellable loading picker before the first catalog request completes" do
            runtime <- newPickerRuntime
            gate <- newEmptyMVar
            access <- newGatewayModelAccessWithUsage (takeMVar gate)
                (const (pure (Left "usage unavailable")))
            withAsync (openGatewayPicker runtime access) \worker -> do
                (body, rows, reply) <- awaitPickerEvent runtime \case
                    AppAskDynamicAdjustableFilterChoice _ body _ rows reply ->
                        Just (body, rows, reply)
                    _ -> Nothing
                body `shouldBe` "Loading models…"
                rows `shouldBe` []
                atomically (putTMVar reply Nothing)
                awaitPickerAction (wait worker) `shouldReturn` Right Nothing

        it "publishes refreshed model rows without waiting for their usage" do
            runtime <- newPickerRuntime
            gate <- newEmptyMVar
            usageGate <- newEmptyMVar
            access <- newGatewayModelAccessWithUsage (takeMVar gate)
                (const (takeMVar usageGate))
            withAsync (openGatewayPicker runtime access) \worker -> do
                reply <- awaitPickerEvent runtime \case
                    AppAskDynamicAdjustableFilterChoice _ _ _ _ reply -> Just reply
                    _ -> Nothing
                putMVar gate (Right [pickerModel "company-new"])
                rows <- awaitPickerEvent runtime \case
                    AppUpdateDynamicAdjustableFilterChoice _ _ rows
                        | not (null rows) -> Just rows
                    _ -> Nothing
                [label | (_, label, _, _, _) <- rows] `shouldBe` ["company-new"]
                case rows of
                    (key, _, _, _, effort) : _ -> do
                        atomically (putTMVar reply (Just (key, effort)))
                        selected <- awaitPickerAction (wait worker)
                        fmap (fmap (.modelPickerOption.modelTarget.targetModelId)) selected
                            `shouldBe` Right (Just "company-new")
                    _ -> expectationFailure "Expected refreshed model row"

        it "publishes usage in one group while the model catalog refresh is blocked" do
            runtime <- newPickerRuntime
            let models = [pickerModel "company-a", pickerModel "company-b"]
            fetch <- newIORef (pure (Right models))
            modelGate <- newEmptyMVar
            usageGate <- newEmptyMVar
            access <- newGatewayModelAccessWithUsage
                (readIORef fetch >>= id) (const (takeMVar usageGate))
            refreshGatewayModels access `shouldReturn` Right models
            writeIORef fetch (takeMVar modelGate)
            withAsync (openGatewayPicker runtime access) \worker -> do
                reply <- awaitPickerEvent runtime \case
                    AppAskDynamicAdjustableFilterChoice _ _ _ _ reply -> Just reply
                    _ -> Nothing
                putMVar usageGate (Right pickerUsage)
                putMVar usageGate (Right pickerUsage)
                rows <- awaitPickerEvent runtime \case
                    AppUpdateDynamicAdjustableFilterChoice _ _ rows
                        | any (\(_, label, _, _, _) -> "75% left" `Text.isInfixOf` label) rows ->
                            Just rows
                    _ -> Nothing
                [label | (_, label, _, _, _) <- rows]
                    `shouldBe` ["company-a ✓  5h 75% left", "company-b  5h 75% left"]
                atomically (putTMVar reply Nothing)
                awaitPickerAction (wait worker) `shouldReturn` Right Nothing

    it "uses only the aliases advertised by the connected gateway" do
        let options =
                modelOptionsForGatewayState
                    testCatalog
                    (Just
                        [ GatewayModel "company-b" GatewayResponsesProtocol GatewayOpenAIProvider
                        , GatewayModel "company-a" GatewayResponsesProtocol GatewayXAIProvider
                        , GatewayModel "company-b" GatewayResponsesProtocol GatewayOpenAIProvider
                        ])
        map (.modelTarget.targetModelId) options
            `shouldBe` ["company-b", "company-a"]
        map (.modelTarget.targetConnectionId) options
            `shouldBe` replicate 2 organizationGatewayConnectionId
        map (.modelTarget.targetWireModelId) options
            `shouldBe` ["company-b", "company-a"]
        map (.modelTarget.targetDialect) options
            `shouldBe` [CodexDialect, GrokBuildDialect]
        map (.modelLabel) options
            `shouldBe` [Nothing, Just "Company A"]

    it "uses only direct catalog entries while disconnected" do
        let options = modelOptionsForGatewayState testCatalog Nothing
        map (.modelTarget.targetModelId) options
            `shouldBe` ["router-default", "grok", "gemini", "router", "sonnet"]
        map (.modelTarget.targetConnectionId) options
            `shouldBe` ["openai", "xai", "gemini", "openrouter", "claude-code"]

    it "maps shared Responses models to their distinct native transports" do
        let options =
                modelOptionsForGatewayModels
                    testCatalog
                    [ GatewayModel "company-a" GatewayResponsesProtocol GatewayOpenAIProvider
                    , GatewayModel "company-grok" GatewayResponsesProtocol GatewayXAIProvider
                    , GatewayModel "sonnet" GatewayAnthropicProtocol GatewayAnthropicProvider
                    , GatewayModel "router-default" GatewayResponsesProtocol GatewayOpenAIProvider
                    ]
        map (.modelTarget.targetProvider) options
            `shouldBe` [OpenAIProvider, XAIProvider, ClaudeCodeProvider]
        map (.modelTarget.targetModelId) options
            `shouldBe` ["company-a", "company-grok", "sonnet"]
        map (.modelTarget.targetConnectionId) options
            `shouldBe` replicate 3 organizationGatewayConnectionId
        map (.modelTarget.targetDialect) options
            `shouldBe` [CodexDialect, GrokBuildDialect, ClaudeCodeDialect]

    it "uses provider metadata rather than model names or local dialect overrides" do
        let options = modelOptionsForGatewayModels testCatalog
                [ GatewayModel "gpt-company" GatewayResponsesProtocol GatewayXAIProvider
                , GatewayModel "grok-company" GatewayResponsesProtocol GatewayOpenAIProvider
                , GatewayModel "company-a" GatewayResponsesProtocol GatewayXAIProvider
                ]
        map (.modelTarget.targetProvider) options
            `shouldBe` [XAIProvider, OpenAIProvider, XAIProvider]
        map (.modelTarget.targetDialect) options
            `shouldBe` [GrokBuildDialect, CodexDialect, GrokBuildDialect]
        map (.modelTarget.targetWireModelId) options
            `shouldBe` ["gpt-company", "grok-company", "company-a"]
        map (.modelLabel) options
            `shouldBe` [Nothing, Nothing, Just "Company A"]

    describe "selectGatewayModelOption" do
        let options = modelOptionsForGatewayModels testCatalog
                [ GatewayModel "company-openai" GatewayResponsesProtocol GatewayOpenAIProvider
                , GatewayModel "company-xai" GatewayResponsesProtocol GatewayXAIProvider
                , GatewayModel "company-claude" GatewayAnthropicProtocol GatewayAnthropicProvider
                ]
            savedTarget provider model =
                ModelTarget provider organizationGatewayConnectionId model model CodexDialect
            selectedProvider model provider hints =
                (.modelTarget.targetProvider)
                    <$> selectGatewayModelOption options model provider hints

        it "selects an explicit alias using its advertised provider" $
            selectedProvider (Just "company-xai") Nothing []
                `shouldBe` Right XAIProvider

        it "rejects an explicit alias absent from the authorized catalog" $
            selectGatewayModelOption options (Just "unlisted") Nothing []
                `shouldSatisfy` isLeft

        it "rejects an explicit provider that conflicts with the explicit alias" $
            selectGatewayModelOption options
                (Just "company-xai") (Just OpenAIProvider) []
                `shouldSatisfy` isLeft

        it "accepts an explicit provider matching the explicit alias" $
            selectedProvider (Just "company-xai") (Just XAIProvider) []
                `shouldBe` Right XAIProvider

        it "resolves a saved Grok alias without trusting its old OpenAI provider" $
            selectedProvider Nothing Nothing
                [savedTarget OpenAIProvider "company-xai"]
                `shouldBe` Right XAIProvider

        it "uses the first available saved alias preference" $
            selectedProvider Nothing Nothing
                [ savedTarget OpenAIProvider "removed-alias"
                , savedTarget OpenAIProvider "company-xai"
                , savedTarget ClaudeCodeProvider "company-claude"
                ]
                `shouldBe` Right XAIProvider

        it "filters saved alias preferences and defaults by explicit provider" $
            selectedProvider Nothing (Just XAIProvider)
                [savedTarget OpenAIProvider "company-openai"]
                `shouldBe` Right XAIProvider

        it "defaults to the first authorized model when no preference resolves" $
            selectedProvider Nothing Nothing
                [savedTarget OpenAIProvider "removed-alias"]
                `shouldBe` Right OpenAIProvider

        it "rejects a provider with no authorized models" $
            selectGatewayModelOption options Nothing (Just GeminiProvider) []
                `shouldSatisfy` isLeft

        it "rejects an empty authorized catalog" $
            selectGatewayModelOption [] Nothing Nothing []
                `shouldSatisfy` isLeft

-- Exercise the same validated construction boundary as production. A catalog
-- now always includes a default for each builtin provider.
testCatalog :: ModelCatalog
testCatalog = either (error . Text.unpack) id $
    decodeModelConfig "gateway-test.json" $ Aeson.encode $ Aeson.object
        [ "version" .= (1 :: Int)
        , "connections" .= Aeson.object
            [ "openai" .= builtin "openai"
            , "xai" .= builtin "xai"
            , "gemini" .= builtin "gemini"
            , "openrouter" .= builtin "openrouter"
            , "claude-code" .= builtin "claude-code"
            , "organization-gateway" .= Aeson.object ["api" .= ("gateway" :: Text)]
            ]
        , "models" .=
            ( [ Aeson.object
                    [ "id" .= model
                    , "connection" .= provider
                    , "dialect" .= dialect
                    , "default" .= True
                    ]
              | (provider, model, dialect) <- builtinModels
              ]
                <> [ Aeson.object
                        [ "id" .= ("company-a" :: Text)
                        , "connection" .= organizationGatewayConnectionId
                        , "dialect" .= ("generic-responses" :: Text)
                        , "context_window" .= (131_072 :: Int)
                        , "label" .= ("Company A" :: Text)
                        , "reasoning_efforts" .= ["high" :: Text]
                        , "default_reasoning_effort" .= ("high" :: Text)
                        ]
                   ]
            )
        ]
  where
    builtin :: Text -> Aeson.Value
    builtin provider = Aeson.object
        [ "api" .= ("builtin" :: Text), "provider" .= provider ]
    builtinModels :: [(Text, Text, Text)]
    builtinModels =
        [ ("openai", "router-default", "codex")
        , ("xai", "grok", "grok-build")
        , ("gemini", "gemini", "generic-responses")
        , ("openrouter", "router", "generic-responses")
        , ("claude-code", "sonnet", "claude-code")
        ]
