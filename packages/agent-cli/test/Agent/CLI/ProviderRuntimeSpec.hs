module Agent.CLI.ProviderRuntimeSpec (spec) where

import Agent.CLI.Session.Request (newSessionRequestState, readSessionRequestParams, setSessionRequestModel)
import Agent.CLI.Session (Persistence(..))
import Agent.CLI.Auth (LoadedAuth(..), gatewayLoadedAuthForProvider)
import Agent.CLI.GatewayClient (GatewayCredential(..))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy as LBS
import Agent.CLI.Compaction (CompactOutcome(..), CompactionInstall(..), reportedOccupancy)
import Agent.CLI.ProviderRuntime
import Agent.CLI.Session.ConversationStore (newConversationStore)
import Agent.CLI.Session.History (readLivePreviousResponseId, readLiveTranscript)
import Agent.Provider (Provider(..), BillingMode(..), TokenProvider, tokenProvider)
import Agent.Responses.Types (defaultResponseCreateParams)
import Agent.OpenAI.Compaction (userTextItem, compactionTriggerItem)
import qualified Agent.Responses.Types as Responses (ResponseCreateParams(model))
import Control.Exception.Safe (throwString)
import Data.IORef (newIORef, readIORef, writeIORef)
import Test.Hspec
import qualified Agent.OpenRouter.Options as OpenRouter
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.HTTP.Types (status200)

spec :: Spec
spec = describe "provider runtime composition" do
    mapM_ checkProvider
        [ ("Gemini", GeminiProviderConfig noNetworkTokens)
        , ("xAI", XaiProviderConfig noNetworkTokens False Nothing)
        , ("OpenRouter", OpenRouterProviderConfig OpenRouterConfig
            { tokenProvider = noNetworkTokens
            , clientOptions = OpenRouter.defaultClientOptions
            , genericOptions = Nothing
            , model = "small"
            , transportModel = id
            })
        ]
    it "compacts gateway xAI history with a native summary request, not a Codex trigger" do
        observed <- newIORef Nothing
        let application request respond = do
                body <- Wai.strictRequestBody request
                writeIORef observed (Just
                    (Wai.rawPathInfo request, Wai.requestHeaders request, body))
                respond $ Wai.responseLBS status200
                    [("Content-Type", "text/event-stream")]
                    "data: {\"type\":\"response.completed\",\"response\":{\"id\":\"summary-response\",\"created_at\":0,\"model\":\"organization-research\",\"status\":\"completed\",\"output\":[{\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":\"Retained summary\"}]}]}}\n\n"
        Warp.testWithApplication (pure application) \port -> do
            let origin = "http://127.0.0.1:" <> Text.pack (show port)
                gateway = GatewayCredential
                    { gatewayBaseUrl = origin
                    , gatewayWebSocketUrl =
                        "ws://127.0.0.1:" <> Text.pack (show port) <> "/v1/responses"
                    , gatewayAccessToken = "gateway-token"
                    }
            loaded <- either (fail . Text.unpack) pure $
                gatewayLoadedAuthForProvider (Just XAIProvider) gateway
            host <- newHost
            setSessionRequestModel host.compaction.paramsRef
                XAIProvider "organization-research"
            history <- newConversationStore (Just "previous-response")
                [userTextItem "Retain the current task", compactionTriggerItem] []
            writeIORef host.compaction.conversationRef history
            withProviderRuntime
                (XaiProviderConfig loaded.loadedTokenProvider False (Just gateway))
                host \runtime -> do
                    result <- runtime.compactRunner Nothing
                    case result of
                        Left message -> expectationFailure (Text.unpack message)
                        Right outcome ->
                            outcome.compactHistory `shouldSatisfy` (not . null)
            readLivePreviousResponseId host.compaction.conversationRef
                `shouldReturn` Nothing
        readIORef observed >>= \case
            Nothing -> expectationFailure "the gateway did not receive a summary request"
            Just (path, headers, body) -> do
                path `shouldBe` "/v1/responses"
                lookup "Authorization" headers `shouldBe` Just "Bearer gateway-token"
                lookup "X-XAI-Token-Auth" headers `shouldBe` Just "xai-grok-cli"
                let requestText = Text.decodeUtf8 (LBS.toStrict body)
                requestText `shouldSatisfy` Text.isInfixOf "\"model\":\"organization-research\""
                requestText `shouldSatisfy` (not . Text.isInfixOf "compaction_trigger")
                requestText `shouldSatisfy` (not . Text.isInfixOf "previous_response_id")
  where
    checkProvider (name, config) = describe name do
        it "observes model changes after the runtime is constructed" do
            host <- newHost
            result <- withProviderRuntime config host \runtime -> do
                runtime.currentContextWindow `shouldReturn` Just 32_768
                setSessionRequestModel host.compaction.paramsRef OpenAIProvider "large"
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
    paramsRef <- newSessionRequestState PersistenceDisabled
        defaultResponseCreateParams{Responses.model = Just "small"}
        >>= either (fail . Text.unpack) pure
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
            , currentModelContextWindow = \_ -> Just . contextWindow <$> readSessionRequestParams paramsRef
            , installAutomaticCompact = \_ _ -> pure CompactionNotInstalled
            , taskPlan = Nothing
            , recordCompactionUsage = \_ -> pure ()
            , compactThreshold = Nothing
            }
        }
