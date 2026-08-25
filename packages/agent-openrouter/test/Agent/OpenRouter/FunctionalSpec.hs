-- | Live functional test against the real OpenRouter Responses API.
--
-- Opt-in: without OPENROUTER_API_KEY the test is pending. To run it:
--
-- > OPENROUTER_API_KEY=... cabal test agent-openrouter
--
-- @OPENROUTER_TEST_MODEL@ overrides the request model (default
-- @openai/gpt-5.1@).
module Agent.OpenRouter.FunctionalSpec (spec) where

import Agent.OpenRouter.Client
import Agent.OpenRouter.Credential
import Agent.OpenRouter.Options
import Agent.Responses.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Environment (lookupEnv)
import Test.Hspec

spec :: Spec
spec = describe "OpenRouter functional (live)" do
    it "answers and drives a function tool using explicit Responses input" do
        loaded <- credentialFromEnv
        case loaded of
            Nothing -> pendingWith
                "set OPENROUTER_API_KEY to run the live OpenRouter functional test"
            Just credential -> do
                model <- fmap (Text.pack . fromMaybe "openai/gpt-5.1")
                    (lookupEnv "OPENROUTER_TEST_MODEL")
                options <- clientOptionsFromEnv
                events <- newIORef (0 :: Int)

                first <- runTurn options credential
                    (helloRequest model "Reply with exactly: hello world") events
                assistantTextOf first `shouldSatisfy` Text.isInfixOf "hello world"

                toolCall <- runTurn options credential (toolRequest model) events
                call <- case [functionCall | FunctionCallItem functionCall <- toolCall.output
                                           , functionCall.name == "echo_text"] of
                    (functionCall : _) -> pure functionCall
                    [] -> expectationFailure
                        ("expected an echo_text function_call, got: " <> show toolCall.output)
                        >> fail "unreachable"
                functionCallArgumentText "text" call.arguments
                    `shouldBe` "openrouter functional tool ok"

                let toolHistory =
                        [userMessage "Call the echo_text tool with the text 'openrouter functional tool ok'."]
                            <> toolCall.output
                final <- runTurn options credential
                    (toolOutputRequest model toolHistory call) events
                assistantTextOf final `shouldSatisfy` Text.isInfixOf "done"

                streamed <- readIORef events
                streamed `shouldSatisfy` (> 0)
  where
    runTurn options credential request events = do
        result <- createResponseWithEvents options credential request
            (const (modifyIORef' events (+ 1)))
        case result of
            Left err -> expectationFailure ("OpenRouter turn failed: " <> show err) >> fail "unreachable"
            Right response -> pure response

    assistantTextOf response =
        Text.toLower (fromMaybe "" (assistantText response))

helloRequest :: Text -> Text -> ResponseCreateParams
helloRequest model prompt = defaultResponseCreateParams
    { model = Just model
    , instructions = Just "You are a concise test assistant. Follow the user's instruction literally."
    , input = Just (ResponseInputItems [userMessage prompt])
    , tools = Just []
    }

toolRequest :: Text -> ResponseCreateParams
toolRequest model = defaultResponseCreateParams
    { model = Just model
    , instructions = Just "You are a test assistant. Use the echo_text tool exactly as instructed."
    , input = Just (ResponseInputItems [userMessage "Call the echo_text tool with the text 'openrouter functional tool ok'."])
    , tools = Just [echoTool]
    }

toolOutputRequest :: Text -> [ResponseItem] -> FunctionCall -> ResponseCreateParams
toolOutputRequest model history call = defaultResponseCreateParams
    { model = Just model
    , instructions = Just "You are a test assistant."
    , input = Just (ResponseInputItems
        (history <>
        [ FunctionCallOutputItem FunctionCallOutput
            { itemId = Nothing
            , callId = call.callId
            , name = Nothing
            , namespace = Nothing
            , output = Aeson.object ["echoed" Aeson..= ("openrouter functional tool ok" :: Text)]
            , status = Nothing
            , extraFields = mempty
            }
        , userMessage "The tool ran. Reply with exactly: done"
        ]))
    , tools = Just [echoTool]
    }

echoTool :: ResponseTool
echoTool = FunctionToolValue FunctionTool
    { name = "echo_text"
    , description = Just "Echo the given text back to the caller."
    , parameters = Just (Aeson.object
        [ "type" Aeson..= ("object" :: Text)
        , "properties" Aeson..= Aeson.object
            [ "text" Aeson..= Aeson.object [ "type" Aeson..= ("string" :: Text) ] ]
        , "required" Aeson..= [ "text" :: Text ]
        , "additionalProperties" Aeson..= False
        ])
    , strict = Just True
    , extraFields = mempty
    }

userMessage :: Text -> ResponseItem
userMessage text = MessageItem ResponseMessage
    { messageId = Nothing
    , role = RoleUser
    , content = MessageContentParts [InputTextPart text Nothing mempty]
    , status = Nothing
    , phase = Nothing
    , passthrough = Nothing
    , extraFields = mempty
    }

assistantText :: Response -> Maybe Text
assistantText response = case
    [ value
    | MessageItem message <- response.output
    , message.role == RoleAssistant
    , value <- case message.content of
        MessageContentText text -> [text]
        MessageContentParts parts -> [text | OutputTextPart { text } <- parts]
    ] of
        [] -> Nothing
        values -> Just (Text.intercalate "\n" values)

functionCallArgumentText :: Text -> Text -> Text
functionCallArgumentText key arguments = case Aeson.decodeStrict' (Text.encodeUtf8 arguments) of
    Just (Aeson.Object object) -> case KeyMap.lookup (Key.fromText key) object of
        Just (Aeson.String value) -> value
        _ -> ""
    _ -> ""
