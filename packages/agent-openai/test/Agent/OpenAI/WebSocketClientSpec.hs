module Agent.OpenAI.WebSocketClientSpec (spec) where

import Test.Hspec
import Agent.Error
import Agent.Responses.Types
import Agent.OpenAI.WebSocketClient
import Control.Retry (constantDelay, limitRetries)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.IORef
import Data.Text (Text)

spec :: Spec
spec = do
  describe "buildWsPayloadWithOptions" do
    it "forces store=false for the Codex WebSocket contract" do
        let request = sampleRequest { store = Just True }
        field "store" (buildWsPayloadWithOptions defaultCodexWsOptions request Nothing)
            `shouldBe` Just (Aeson.Bool False)

    it "does not request server-managed compaction by default" do
        contextManagement defaultCodexWsOptions `shouldBe` Nothing

    it "serializes a positive server-side compaction threshold" do
        let options = CodexWsOptions { compactThreshold = Just 180000 }
        contextManagement options `shouldBe` Just (Aeson.toJSON
            [ Aeson.object
                [ "type" Aeson..= ("compaction" :: Text)
                , "compact_threshold" Aeson..= (180000 :: Int)
                ]
            ])

    it "omits non-positive thresholds" do
        let options = CodexWsOptions { compactThreshold = Just 0 }
        contextManagement options `shouldBe` Nothing

  describe "retryTransientWsResultWithPolicy" do
    it "retries overloads centrally before returning success" do
        let overload = ProviderError OverloadedError "server_is_overloaded" Nothing
        responses <- newIORef
            [ Left overload
            , Left overload
            , Right ("completed" :: Text)
            ]
        result <- retryTransientWsResultWithPolicy
            (constantDelay 0 <> limitRetries 3)
            (atomicModifyIORef' responses \case
                next : rest -> (rest, next)
                [] -> error "unexpected extra WebSocket request")

        result `shouldBe` Right "completed"
        readIORef responses `shouldReturn` []

    it "leaves connection, connection-limit, and quota failures to callers" do
        attempts <- newIORef (0 :: Int)
        let run err = retryTransientWsResultWithPolicy
                (constantDelay 0 <> limitRetries 3)
                (modifyIORef' attempts (+ 1) >> pure (Left err :: Either ApiError Text))

        run (ConnectionError "socket closed")
            `shouldReturn` Left (ConnectionError "socket closed")
        run (ProviderError WebSocketConnectionLimitReached
                "too many websocket connections" Nothing)
            `shouldReturn` Left (ProviderError WebSocketConnectionLimitReached
                "too many websocket connections" Nothing)
        run (ProviderError UsageLimitReached "quota" (Just 3600))
            `shouldReturn` Left (ProviderError UsageLimitReached "quota" (Just 3600))
        readIORef attempts `shouldReturn` 3

contextManagement :: CodexWsOptions -> Maybe Aeson.Value
contextManagement options =
    field "context_management" $
        buildWsPayloadWithOptions options sampleRequest (Just "previous-1")

field :: Key.Key -> Aeson.Value -> Maybe Aeson.Value
field name = \case
    Aeson.Object object -> KeyMap.lookup name object
    _ -> Nothing

sampleRequest :: ResponseCreateParams
sampleRequest = defaultResponseCreateParams
    { model = Just "gpt-test"
    , instructions = Just "test"
    , input = Just (ResponseInputItems [])
    , tools = Just []
    , reasoning = Just ReasoningConfig
        { context = Nothing
        , effort = Just "minimal"
        , generateSummary = Nothing
        , reasoningMode = Nothing
        , summary = Nothing
        , extraFields = mempty
        }
    , include = Just []
    , promptCacheKey = Just "cache-key"
    }
