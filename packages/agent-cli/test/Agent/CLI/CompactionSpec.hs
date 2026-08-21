module Agent.CLI.CompactionSpec (spec) where

import Agent.CLI.Compaction (runProviderCompact)
import Agent.OpenAI.Responses.Types (defaultResponseCreateParams)
import Agent.Provider (Provider(..), TokenProvider(..))
import Data.IORef (newIORef)
import Test.Hspec

spec :: Spec
spec =
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
