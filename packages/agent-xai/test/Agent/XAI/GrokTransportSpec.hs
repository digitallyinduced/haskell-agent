-- | Functional tests for the Grok transport against an in-process HTTP mock.
-- These cover direct REST requests, provider failover on rate limits, and
-- stateful session @previous_response_id@ emulation.
module Agent.XAI.GrokTransportSpec (spec) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.XAI.Grok
import Agent.Provider
import Agent.OpenAI.Responses.Types
import Control.Exception (bracket_)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import System.Environment (setEnv, unsetEnv)
import Test.Hspec

spec :: Spec
spec = do
    describe "createGrokMessageWith" do
        it "POSTs the mapped request with subscription headers and parses the SSE response" do
            recorded <- newIORef []
            let handler _request = do
                    pure $ sseResponse
                        [ outputItemDone (assistantMessage "hello world")
                        , completedEvent "resp-1" []
                        ]
            withMockGrok recorded handler \options -> do
                result <- createGrokMessageWith options (grokCredential "token-a") (helloRequest "hi")
                response <- expectRight result
                response.responseId `shouldBe` "resp-1"
                extractAssistantText response `shouldBe` Just "hello world"

            [request] <- readIORef recorded
            request.path `shouldBe` "/v1/responses"
            lookup "Authorization" request.headers `shouldBe` Just "Bearer token-a"
            lookup "X-XAI-Token-Auth" request.headers `shouldBe` Just "xai-grok-cli"
            requestModel request `shouldBe` Just "grok-4.5"
            -- instructions travel as the leading system item
            (inputRoles <$> requestBodyObject request) `shouldBe` Just ["system", "user"]

    describe "createGrokMessageWithProvider" do
        it "fails over to the next account on 429 and reports the Retry-After interval" do
            recorded <- newIORef []
            feedback <- newIORef []
            let handler :: RecordedRequest -> IO Wai.Response
                handler request
                    | lookup "Authorization" request.headers == Just "Bearer token-limited" =
                        pure $ Wai.responseLBS HTTP.status429
                            [("Retry-After", "7")]
                            "slow down"
                    | otherwise =
                        pure $ sseResponse
                            [ outputItemDone (assistantMessage "served by second account")
                            , completedEvent "resp-2" []
                            ]
            withMockGrok recorded handler \options ->
                withGrokBaseUrlEnv options do
                    provider <- rotatingProvider feedback
                        [ grokCredential "token-limited"
                        , grokCredential "token-healthy"
                        ]
                    result <- createGrokMessageWithProvider provider (helloRequest "hi")
                    response <- expectRight result
                    extractAssistantText response `shouldBe` Just "served by second account"

            reported <- readIORef feedback
            [ (failedCredential.credential.accessToken, failedCredential.failure)
                | Just failedCredential <- reported ] `shouldBe`
                [ ("token-limited", AccountRateLimited (Just 7)) ]

    describe "stateful Grok session" do
        it "emulates previous_response_id by replaying the local transcript" do
            recorded <- newIORef []
            counter <- newIORef (0 :: Int)
            let handler _request = do
                    turn <- atomicModifyIORef' counter \n -> (n + 1, n + 1)
                    pure $ sseResponse
                        [ outputItemDone (assistantMessage ("answer " <> Text.pack (show turn)))
                        , completedEvent ("resp-" <> Text.pack (show turn)) []
                        ]
            withMockGrok recorded handler \options -> do
                session <- newGrokSessionWith options (grokCredential "token-a")
                events <- newIORef (0 :: Int)

                first <- expectRight =<< runGrokSessionTurn session
                    (helloRequest "first question")
                    Nothing
                    (\_ _ -> modifyIORef' events (+ 1))
                first.responseId `shouldBe` "resp-1"

                second <- expectRight =<< runGrokSessionTurn session
                    (helloRequest "second question")
                    (Just first.responseId)
                    (\_ _ -> pure ())
                second.responseId `shouldBe` "resp-2"

                stale <- runGrokSessionTurn session
                    (helloRequest "third question")
                    (Just "resp-unknown")
                    (\_ _ -> pure ())
                case stale of
                    Left (ProviderError PreviousResponseNotFound _ _) -> pure ()
                    other -> expectationFailure
                        ("expected PreviousResponseNotFound, got " <> show other)

                streamed <- readIORef events
                streamed `shouldSatisfy` (>= 2)

            requests <- readIORef recorded
            -- the stale previous_response_id must not reach the proxy
            length requests `shouldBe` 2
            let turnInputs request = do
                    object <- requestBodyObject request
                    input <- arrayField "input" object
                    traverse itemSummary input
            (traverse turnInputs requests) `shouldBe` Just
                [ [ ("message", Just "system")
                  , ("message", Just "user")
                  ]
                , [ ("message", Just "system")
                  , ("message", Just "user")      -- first question
                  , ("message", Just "assistant") -- answer 1, from the transcript
                  , ("message", Just "user")      -- second question
                  ]
                ]

    describe "retry boundaries" do
        it "does not re-run a turn whose events were already delivered" do
            recorded <- newIORef []
            events <- newIORef ([] :: [Text])
            -- A stream that starts fine and then fails. Retrying would hand the
            -- caller a second generation's events for one logical turn.
            let handler _request = pure $ sseResponse
                    [ outputItemDone (assistantMessage "partial answer")
                    , sseEvent "response.failed" $ Aeson.object
                        [ "type" Aeson..= ("response.failed" :: Text)
                        , "response" Aeson..= Aeson.object
                            [ "status_details" Aeson..= Aeson.object
                                [ "reason" Aeson..= ("overloaded" :: Text) ]
                            ]
                        ]
                    ]
            withMockGrok recorded handler \options -> do
                result <- createGrokMessageWith options (grokCredential "token-a") (helloRequest "hi")
                case result of
                    Left ConnectionError{} -> pure ()
                    other -> expectationFailure ("expected ConnectionError, got " <> show other)

            requests <- readIORef recorded
            length requests `shouldBe` 1
            delivered <- readIORef events
            delivered `shouldBe` []

    describe "transcript budget" do
        it "bounds the replayed transcript instead of growing every turn" do
            recorded <- newIORef []
            counter <- newIORef (0 :: Int)
            -- Every turn answers with a chunk far bigger than the budget. The
            -- proxy keeps no conversation state, so this replay is the entire
            -- context the model sees; unbounded, it would grow each turn until
            -- it exceeded the model's window.
            let bulk = Text.replicate 2000 "grok "
                handler _request = do
                    turn <- atomicModifyIORef' counter \n -> (n + 1, n + 1)
                    pure $ sseResponse
                        [ outputItemDone (assistantMessage (bulk <> Text.pack (show turn)))
                        , completedEvent ("resp-" <> Text.pack (show turn)) []
                        ]
            withMockGrok recorded handler \options ->
                withGrokBaseUrlEnv options do
                    session <- newGrokSessionWith options (grokCredential "token-a")
                    let turn n previous = expectRight =<< runGrokSessionTurnWithBudget
                            session
                            (Just 1000)
                            (helloRequest ("question " <> Text.pack (show (n :: Int))))
                            previous
                            (\_ _ -> pure ())
                    first <- turn 1 Nothing
                    second <- turn 2 (Just first.responseId)
                    _third <- turn 3 (Just second.responseId)
                    pure ()

            requests <- readIORef recorded
            length requests `shouldBe` 3
            let inputLengths =
                    [ length input
                    | request <- requests
                    , Just object <- [requestBodyObject request]
                    , Just input <- [arrayField "input" object]
                    ]
            case inputLengths of
                [_, secondLength, thirdLength] ->
                    thirdLength `shouldSatisfy` (<= secondLength)
                other -> expectationFailure ("unexpected input lengths: " <> show other)

--------------------------------------------------------------------------------
-- Mock server
--------------------------------------------------------------------------------

data RecordedRequest = RecordedRequest
    { path :: !Text
    , headers :: ![(Text, Text)]
    , body :: !LBS.ByteString
    }

withMockGrok
    :: IORef [RecordedRequest]
    -> (RecordedRequest -> IO Wai.Response)
    -> (GrokOptions -> IO a)
    -> IO a
withMockGrok recorded handler action =
    Warp.testWithApplication (pure app) \port ->
        action defaultGrokOptions
            { baseUrl = "http://127.0.0.1:" <> show port <> "/v1"
            , requestTimeoutSeconds = 10
            }
  where
    app waiRequest respond = do
        requestBody <- Wai.strictRequestBody waiRequest
        let request = RecordedRequest
                { path = "/" <> Text.intercalate "/" (Wai.pathInfo waiRequest)
                , headers =
                    [ (Text.decodeUtf8 (CI.original name), Text.decodeUtf8 value)
                    | (name, value) <- Wai.requestHeaders waiRequest
                    ]
                , body = requestBody
                }
        atomicModifyIORef' recorded \requests -> (requests <> [request], ())
        respond =<< handler request

sseResponse :: [Text] -> Wai.Response
sseResponse events = Wai.responseLBS HTTP.status200
    [("Content-Type", "text/event-stream")]
    (LBS.fromStrict (Text.encodeUtf8 (Text.concat events)))

outputItemDone :: Aeson.Value -> Text
outputItemDone item = sseEvent "response.output_item.done" $ Aeson.object
    [ "type" Aeson..= ("response.output_item.done" :: Text)
    , "item" Aeson..= item
    ]

completedEvent :: Text -> [Aeson.Value] -> Text
completedEvent responseId output = sseEvent "response.completed" $ Aeson.object
    [ "type" Aeson..= ("response.completed" :: Text)
    , "response" Aeson..= Aeson.object
        [ "id" Aeson..= responseId
        , "created_at" Aeson..= (0 :: Int)
        , "model" Aeson..= ("grok-4.5" :: Text)
        , "status" Aeson..= ("completed" :: Text)
        , "output" Aeson..= output
        , "usage" Aeson..= Aeson.object
            [ "input_tokens" Aeson..= (10 :: Int)
            , "output_tokens" Aeson..= (5 :: Int)
            , "total_tokens" Aeson..= (15 :: Int)
            ]
        ]
    ]

sseEvent :: Text -> Aeson.Value -> Text
sseEvent eventType payload =
    "event: " <> eventType <> "\ndata: "
        <> Text.decodeUtf8 (LBS.toStrict (Aeson.encode payload))
        <> "\n\n"

assistantMessage :: Text -> Aeson.Value
assistantMessage text = Aeson.object
    [ "type" Aeson..= ("message" :: Text)
    , "role" Aeson..= ("assistant" :: Text)
    , "content" Aeson..= [Aeson.object
        [ "type" Aeson..= ("output_text" :: Text)
        , "text" Aeson..= text
        ]]
    ]

--------------------------------------------------------------------------------
-- Providers and credentials
--------------------------------------------------------------------------------

grokCredential :: Text -> Credential
grokCredential token = Credential
    { accessToken = token
    , accountId = "grok-" <> token
    , leaseId = Nothing
    , provider = XAIProvider
    }

-- | Serves credentials in order, recording every failure report. The same
-- credential keeps being served until a failure arrives, then the next one
-- takes over — a miniature of the broker's behaviour.
rotatingProvider :: IORef [Maybe FailedCredential] -> [Credential] -> IO TokenProvider
rotatingProvider feedback credentials = do
    remaining <- newIORef credentials
    pure $ TokenProvider \failed -> do
        modifyIORef' feedback (<> [failed])
        case failed of
            Nothing -> pure ()
            Just _ -> modifyIORef' remaining (drop 1)
        current <- readIORef remaining
        case current of
            (credential : _) -> pure (Right credential)
            [] -> pure (Left (ConnectionError "provider exhausted"))

-- The provider entry point constructs its options from the environment; point
-- it at the mock server.
withGrokBaseUrlEnv :: GrokOptions -> IO a -> IO a
withGrokBaseUrlEnv options = bracket_
    (setEnv "XAI_GROK_BASE_URL" options.baseUrl)
    (unsetEnv "XAI_GROK_BASE_URL")

--------------------------------------------------------------------------------
-- Request/response helpers
--------------------------------------------------------------------------------

helloRequest :: Text -> ResponseCreateParams
helloRequest prompt = defaultResponseCreateParams
    { model = Just "gpt-5.6-terra"
    , instructions = Just "You are a test agent."
    , input = Just (ResponseInputText prompt)
    , tools = Just []
    , reasoning = Just (ReasoningConfig Nothing (Just "low") Nothing Nothing Nothing mempty)
    , include = Just [ResponseInclude "reasoning.encrypted_content"]
    }

extractAssistantText :: Response -> Maybe Text
extractAssistantText response = case
    [ value
    | MessageItem message <- response.output
    , message.role == RoleAssistant
    , value <- case message.content of
        MessageContentText text -> [text]
        MessageContentParts parts -> [text | OutputTextPart { text } <- parts]
    ] of
        [] -> Nothing
        values -> Just (Text.intercalate "\n" values)

requestBodyObject :: RecordedRequest -> Maybe Aeson.Object
requestBodyObject request = case Aeson.decode request.body of
    Just (Aeson.Object object) -> Just object
    _ -> Nothing

requestModel :: RecordedRequest -> Maybe Text
requestModel request = do
    object <- requestBodyObject request
    case KeyMap.lookup "model" object of
        Just (Aeson.String model) -> Just model
        _ -> Nothing

inputRoles :: Aeson.Object -> [Text]
inputRoles object = case KeyMap.lookup "input" object of
    Just (Aeson.Array items) ->
        [ role
        | Aeson.Object item <- foldr (:) [] items
        , Just (Aeson.String role) <- [KeyMap.lookup "role" item]
        ]
    _ -> []

arrayField :: Text -> Aeson.Object -> Maybe [Aeson.Value]
arrayField name object = case KeyMap.lookup (Key.fromText name) object of
    Just (Aeson.Array values) -> Just (foldr (:) [] values)
    _ -> Nothing

itemSummary :: Aeson.Value -> Maybe (Text, Maybe Text)
itemSummary = \case
    Aeson.Object item -> do
        Aeson.String itemType <- KeyMap.lookup "type" item
        let role = case KeyMap.lookup "role" item of
                Just (Aeson.String value) -> Just value
                _ -> Nothing
        pure (itemType, role)
    _ -> Nothing

expectRight :: Show e => Either e a -> IO a
expectRight = \case
    Left err -> expectationFailure ("expected Right, got Left " <> show err) >> fail "unreachable"
    Right value -> pure value
