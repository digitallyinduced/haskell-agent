module Agent.CLI.CompactionSpec (spec) where

import Agent.CLI.Compaction
    ( CompactOutcome(..)
    , autoCompactOpenAiBackendWith
    , codexAutoCompactTokenLimit
    , runProviderCompact
    )
import Agent.Loop
import Agent.OpenAI.Compaction (userTextItem)
import Agent.Responses.Types (defaultResponseCreateParams)
import Agent.Provider (Provider(..), TokenProvider(..))
import Data.IORef
import Test.Hspec

spec :: Spec
spec = do
    describe "runProviderCompact" do
        it "reports a provider-specific error when credentials are unavailable" do
            params <- newIORef defaultResponseCreateParams
            transcript <- newIORef []
            runProviderCompact OpenAIProvider Nothing params transcript Nothing
                `shouldReturn` Left "openai compact requires a token provider"
            runProviderCompact XAIProvider Nothing params transcript Nothing
                `shouldReturn` Left "xai compact requires a token provider"
            runProviderCompact OpenRouterProvider Nothing params transcript Nothing
                `shouldReturn` Left "openrouter compact requires a token provider"

        it "short-circuits an empty transcript before using credentials" do
            params <- newIORef defaultResponseCreateParams
            transcript <- newIORef []
            let provider = TokenProvider \_ ->
                    error "empty compaction unexpectedly requested credentials"
            runProviderCompact OpenAIProvider (Just provider) params transcript Nothing
                `shouldReturn` Left "nothing to compact"

    describe "autoCompactOpenAiBackendWith" do
        it "compacts before the next request at the Codex token limit" do
            let oldHistory = [userTextItem "old"]
                compactedHistory = [userTextItem "compacted"]
            transcript <- newIORef oldHistory
            contextState <- newIORef
                (Just (codexAutoCompactTokenLimit, length oldHistory))
            compactCalls <- newIORef (0 :: Int)
            seenPrevious <- newIORef []
            events <- newIORef []
            let compactAction = do
                    modifyIORef' compactCalls (+ 1)
                    pure $ Right CompactOutcome
                        { compactBeforeTokens = codexAutoCompactTokenLimit
                        , compactAfterTokens = 10
                        , compactHistory = compactedHistory
                        , compactSummary = "checkpoint"
                        }
                base = Backend \previous _ _ -> do
                    modifyIORef' seenPrevious (<> [previous])
                    pure $ Right TurnOutput
                        { responseId = "resp-new"
                        , toolCalls = []
                        , assistantText = Just "ok"
                        , tokenUsage = TokenUsage 20 5 0
                        }
                backend = autoCompactOpenAiBackendWith compactAction
                    transcript contextState base
            result <- backend.submitTurn (Just "resp-old") [UserMessage "new"]
                (\event -> modifyIORef' events (<> [event]))
            result `shouldSatisfy` either (const False) (const True)
            readIORef compactCalls `shouldReturn` 1
            readIORef seenPrevious `shouldReturn` [Nothing]
            readIORef transcript `shouldReturn` compactedHistory
            readIORef contextState `shouldReturn`
                Just (25, length compactedHistory)
            readIORef events `shouldReturn`
                [ActivityUpdated "Compacting context…"]
