module Agent.OpenAI.RequestSpec (spec) where

import Agent.OpenAI.Request (sanitizeCodexRequest)
import Agent.OpenAI.WebSocketClient
    ( buildWsPayloadWithOptions
    , defaultCodexWsOptions
    )
import Agent.Responses.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (toList)
import Data.Text (Text)
import Test.Hspec

-- | Both Codex transports must serialize the same request contract, however
-- the caller assembled its 'ResponseCreateParams'.
spec :: Spec
spec = describe "Codex wire contract" do
    mapM_ transportSpec
        [ ("HTTP body", Aeson.toJSON . sanitizeCodexRequest)
        , ("WebSocket envelope", \request ->
            buildWsPayloadWithOptions defaultCodexWsOptions request Nothing)
        ]

transportSpec :: (String, ResponseCreateParams -> Aeson.Value) -> Spec
transportSpec (transport, payloadFor) = describe transport do
    it "sets reasoning.context to all_turns on Responses Lite requests" do
        let reasoning = payloadFor sampleRequest `at` "reasoning"
        (reasoning >>= (`at` "context"))
            `shouldBe` Just (Aeson.String "all_turns")
        (reasoning >>= (`at` "effort"))
            `shouldBe` Just (Aeson.String "minimal")

    it "creates the Responses Lite reasoning context when reasoning is unset" do
        payloadFor (withReasoning Nothing sampleRequest) `at` "reasoning"
            `shouldBe` Just (Aeson.object ["context" Aeson..= ("all_turns" :: Text)])

    it "leaves reasoning unchanged outside Responses Lite" do
        let generic = withModel (Just "gpt-generic") sampleRequest
        (payloadFor generic `at` "reasoning" >>= (`at` "context"))
            `shouldBe` Nothing
        payloadFor (withReasoning Nothing generic) `at` "reasoning"
            `shouldBe` Nothing

    it "forces store=false" do
        payloadFor (withStore (Just True) sampleRequest) `at` "store"
            `shouldBe` Just (Aeson.Bool False)
        payloadFor (withStore Nothing sampleRequest) `at` "store"
            `shouldBe` Just (Aeson.Bool False)
        payloadFor
                (withStore (Just True)
                    (withModel (Just "gpt-generic") sampleRequest))
                `at` "store"
            `shouldBe` Just (Aeson.Bool False)

    it "drops input message ids that Codex did not issue" do
        let request = withInputItems
                [ userMessage (Just "msg_provider")
                , userMessage (Just "local-message-1")
                , userMessage Nothing
                ]
                sampleRequest
        case payloadFor request `at` "input" of
            Just (Aeson.Array items) ->
                map (`at` "id") (toList items)
                    `shouldBe` [Just (Aeson.String "msg_provider"), Nothing, Nothing]
            other ->
                expectationFailure
                    ("expected input array, got " <> show other)

at :: Aeson.Value -> Text -> Maybe Aeson.Value
at value name = case value of
    Aeson.Object object -> KeyMap.lookup (Key.fromText name) object
    _ -> Nothing

userMessage :: Maybe Text -> ResponseItem
userMessage messageId = MessageItem ResponseMessage
    { messageId
    , content = MessageContentParts [InputTextPart "hello" Nothing]
    , role = RoleUser
    , status = Nothing
    , phase = Nothing
    , passthrough = Nothing
    }

sampleRequest :: ResponseCreateParams
sampleRequest = defaultResponseCreateParams
    { model = Just "gpt-5.6-sol"
    , instructions = Just "test"
    , input = Just (ResponseInputItems [])
    , reasoning = Just ReasoningConfig
        { context = Nothing
        , effort = Just "minimal"
        , generateSummary = Nothing
        , reasoningMode = Nothing
        , summary = Nothing
        }
    }

withModel :: Maybe Text -> ResponseCreateParams -> ResponseCreateParams
withModel nextModel ResponseCreateParams { model = _, .. } =
    ResponseCreateParams { model = nextModel, .. }

withReasoning
    :: Maybe ReasoningConfig -> ResponseCreateParams -> ResponseCreateParams
withReasoning nextReasoning ResponseCreateParams { reasoning = _, .. } =
    ResponseCreateParams { reasoning = nextReasoning, .. }

withStore :: Maybe Bool -> ResponseCreateParams -> ResponseCreateParams
withStore nextStore ResponseCreateParams { store = _, .. } =
    ResponseCreateParams { store = nextStore, .. }

withInputItems
    :: [ResponseItem] -> ResponseCreateParams -> ResponseCreateParams
withInputItems items ResponseCreateParams { input = _, .. } =
    ResponseCreateParams { input = Just (ResponseInputItems items), .. }
