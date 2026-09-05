module Agent.CLI.CompactionSpec.ManualOpenAI (spec) where

import Agent.CLI.Compaction (CompactOutcome(..), compactOpenAIWith)
import Agent.CLI.CompactionSpec.Fixtures
import Agent.Error (ApiError(..), ErrorType(..))
import Agent.OpenAI.Compaction
    ( assistantSummaryItem, compactionTriggerItem, hasCompactionCheckpoint
    , userTextItem )
import Agent.OpenAI.ModelMetadata (codexEffectiveContextWindowFor)
import Agent.Provider (BillingMode(..), tokenProvider)
import Agent.Responses.Types
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = do
    describe "compactOpenAIWith" do
        it "uses remote compaction v2 on normal Responses" do
            requests <- newIORef []
            let provider = tokenProvider SubscriptionBilled \_ ->
                    error "remote compaction unexpectedly requested credentials"
                send _ request = do
                    modifyIORef' requests (<> [request])
                    pure (Right remoteCompactionResponse)
                history = [userTextItem "old context"]
                params = defaultResponseCreateParams
                    { instructions = Just "keep these instructions"
                    , tools = Just []
                    , stream = Just False
                    }
            result <- runExceptT $
                compactOpenAIWith send
                    (Just provider)
                    params
                    history
                    100
                    Nothing
            case result of
                Left err -> expectationFailure (show err)
                Right outcome -> do
                    outcome.compactSummary
                        `shouldBe` "Context compacted remotely."
                    outcome.compactHistory
                        `shouldSatisfy` hasCompactionCheckpoint
            seen <- readIORef requests
            length seen `shouldBe` 1
            map (.instructions) seen `shouldBe` [Just "keep these instructions"]
            map (.tools) seen `shouldBe` [Just []]
            map (.parallelToolCalls) seen `shouldBe` [Just True]
            map (.previousResponseId) seen `shouldBe` [Nothing]
            map (.store) seen `shouldBe` [Just False]
            map (.stream) seen `shouldBe` [Just True]
            map (.toolChoice) seen
                `shouldBe` [Just (ToolChoiceMode ToolChoiceAuto)]
            map requestItems seen
                `shouldBe` [history <> [compactionTriggerItem]]

        it "disables parallel tool calls for Responses Lite remote compaction" do
            requests <- newIORef []
            let provider = tokenProvider SubscriptionBilled \_ ->
                    error "remote compaction unexpectedly requested credentials"
                send _ request = do
                    modifyIORef' requests (<> [request])
                    pure (Right remoteCompactionResponse)
                history = [userTextItem "old context"]
                params = defaultResponseCreateParams
                    { model = Just "gpt-5.6-sol"
                    , instructions = Just "keep these instructions"
                    , store = Just True
                    , tools = Just []
                    }
            result <- runExceptT $
                compactOpenAIWith send
                    (Just provider)
                    params
                    history
                    100
                    Nothing
            case result of
                Left err -> expectationFailure (show err)
                Right outcome ->
                    outcome.compactSummary
                        `shouldBe` "Context compacted remotely."
            map (.parallelToolCalls) <$> readIORef requests
                `shouldReturn` [Just False]

        it "rejects remote checkpoints that cannot fit the installed snapshot" do
            let provider = tokenProvider SubscriptionBilled \_ ->
                    error "remote compaction unexpectedly requested credentials"
                contextWindow =
                    codexEffectiveContextWindowFor
                        defaultResponseCreateParams.model
                oversizedResponse =
                    responseWithOutput
                        [ Aeson.object
                            [ "type" .= ("compaction" :: Text)
                            , "encrypted_content" .=
                                Text.replicate (contextWindow * 4 + 10_000) "x"
                            ]
                        ]
                send _ _ = pure (Right oversizedResponse)
                history = [userTextItem "old context"]
            result <- runExceptT $
                compactOpenAIWith send
                    (Just provider)
                    defaultResponseCreateParams
                    history
                    100
                    Nothing
            result `shouldSatisfy` \case
                Left message ->
                    "remote compacted snapshot request cannot fit"
                        `Text.isInfixOf` message
                Right _ -> False

        it "keeps focused manual compaction on local summarization" do
            requests <- newIORef []
            let provider = tokenProvider SubscriptionBilled \_ ->
                    error "local summarization unexpectedly requested credentials"
                send _ request = do
                    modifyIORef' requests (<> [request])
                    pure (Right (summaryResponse "local summary"))
                history = [userTextItem "old context"]
            result <- runExceptT $
                compactOpenAIWith send
                    (Just provider)
                    defaultResponseCreateParams
                    history
                    100
                    (Just "focus on auth")
            case result of
                Left err -> expectationFailure (show err)
                Right outcome -> do
                    outcome.compactSummary `shouldBe` "local summary"
                    outcome.compactHistory
                        `shouldBe`
                            [ userTextItem "old context"
                            , assistantSummaryItem "local summary"
                            ]
            seen <- readIORef requests
            map (.tools) seen `shouldBe` [Nothing]
            map (.parallelToolCalls) seen `shouldBe` [Just False]
            map (.stream) seen `shouldBe` [Just True]

        it "returns friendly provider errors from manual compaction" do
            let provider = tokenProvider SubscriptionBilled \_ ->
                    error "compaction unexpectedly requested credentials"
                send _ _ =
                    pure $ Left $
                        ProviderError UsageLimitReached
                            "quota exhausted"
                            (Just 120)
                history = [userTextItem "old context"]
            result <- runExceptT $
                compactOpenAIWith send
                    (Just provider)
                    defaultResponseCreateParams
                    history
                    100
                    Nothing
            case result of
                Left err -> do
                    err `shouldSatisfy`
                        Text.isInfixOf "Usage limit reached"
                    err `shouldSatisfy`
                        Text.isInfixOf "Try again in 2m"
                    err `shouldNotSatisfy`
                        Text.isInfixOf "ProviderError"
                Right _ ->
                    expectationFailure "expected compaction to fail"
