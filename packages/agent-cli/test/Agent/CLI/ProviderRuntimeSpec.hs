module Agent.CLI.ProviderRuntimeSpec (spec) where

import Agent.CLI.Compaction (CompactionInstall(..), reportedOccupancy)
import Agent.CLI.ProviderRuntime
import Agent.CLI.Session.ConversationStore (newConversationStore)
import Agent.CLI.Session.History (readLivePreviousResponseId, readLiveTranscript)
import Agent.Provider (BillingMode(..), TokenProvider, tokenProvider)
import Agent.Responses.Types (defaultResponseCreateParams)
import qualified Agent.Responses.Types as Responses (ResponseCreateParams(model))
import Control.Exception.Safe (throwString)
import Data.IORef (newIORef, readIORef, writeIORef)
import Test.Hspec
import qualified Agent.OpenRouter.Options as OpenRouter

spec :: Spec
spec = describe "provider runtime composition" do
    mapM_ checkProvider
        [ ("Gemini", GeminiProviderConfig noNetworkTokens)
        , ("xAI", XaiProviderConfig noNetworkTokens False)
        , ("OpenRouter", OpenRouterProviderConfig OpenRouterConfig
            { tokenProvider = noNetworkTokens
            , clientOptions = OpenRouter.defaultClientOptions
            , genericOptions = Nothing
            , model = "small"
            , transportModel = id
            })
        ]
  where
    checkProvider (name, config) = describe name do
        it "observes model changes after the runtime is constructed" do
            host <- newHost
            result <- withProviderRuntime config host \runtime -> do
                runtime.currentContextWindow `shouldReturn` Just 32_768
                writeIORef host.compaction.paramsRef
                    defaultResponseCreateParams{Responses.model = Just "large"}
                runtime.currentContextWindow `shouldReturn` Just 65_536
                pure "consumer result"
            result `shouldBe` ("consumer result" :: String)

        it "leaves live session state intact when manual compaction fails" do
            host <- newHost
            let beforeOccupancy = Just (reportedOccupancy 1234 0)
            writeIORef host.compaction.contextTokensRef beforeOccupancy
            withProviderRuntime config host \runtime ->
                runtime.compactRunner Nothing `shouldReturn` Left "nothing to compact"
            readLivePreviousResponseId host.compaction.conversationRef
                `shouldReturn` Just "previous-response"
            readLiveTranscript host.compaction.conversationRef `shouldReturn` []
            readIORef host.compaction.contextTokensRef `shouldReturn` beforeOccupancy

-- Construct a real runtime without CLI startup, persistence, or a subagent
-- registry. Any accidental credential acquisition fails before network IO.
noNetworkTokens :: TokenProvider
noNetworkTokens = tokenProvider ApiBilled \_ ->
    throwString "runtime composition unexpectedly requested credentials"

newHost :: IO ProviderHost
newHost = do
    paramsRef <- newIORef defaultResponseCreateParams{Responses.model = Just "small"}
    contextTokensRef <- newIORef Nothing
    conversationRef <- newIORef =<< newConversationStore (Just "previous-response") [] []
    let contextWindow params = case params.model of
            Just "large" -> 65_536
            _ -> 32_768
    pure ProviderHost
        { networkRecovery = Nothing
        , compaction = ProviderCompaction
            { paramsRef
            , contextTokensRef
            , conversationRef
            , contextWindowForParams = \_ _ -> contextWindow
            , currentModelContextWindow = \_ -> Just . contextWindow <$> readIORef paramsRef
            , installAutomaticCompact = \_ _ -> pure CompactionNotInstalled
            , taskPlan = Nothing
            , recordCompactionUsage = \_ -> pure ()
            , compactThreshold = Nothing
            }
        }
