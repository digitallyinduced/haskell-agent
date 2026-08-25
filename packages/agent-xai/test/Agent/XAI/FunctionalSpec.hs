-- | Live functional test against the real Grok subscription proxy.
--
-- Opt-in: without credentials in the environment
-- the test is pending. To run it:
--
-- > GROK_AUTH_JSON="$(cat ~/.grok/auth.json)" cabal test
--
-- 'GROK_AUTH_JSON' accepts the grok CLI's @auth.json@ (the object keyed by
-- @issuer::client_id@), a single credential entry from it, or a plain
-- @{"access_token": "..."}@ object. Alternatively set @GROK_ACCESS_TOKEN@.
-- @GROK_TEST_MODEL@ overrides the request model (default @gpt-5.6-terra@ —
-- deliberately an OpenAI name, proving the mapping).
module Agent.XAI.FunctionalSpec (spec) where

import Agent.XAI.Auth (accountIdFromAccessToken)
import Agent.XAI.Client
import Agent.XAI.Options
import Agent.Provider (Credential(..), Provider(..))
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
spec = describe "xAI functional (live)" do
    it "answers and drives a function tool using explicit Responses input" do
        loaded <- loadGrokCredential
        case loaded of
            Nothing -> pendingWith
                "set GROK_AUTH_JSON (contents of ~/.grok/auth.json) or GROK_ACCESS_TOKEN to run the live Grok functional test"
            Just credential -> do
                model <- fmap (Text.pack . fromMaybe "gpt-5.6-terra") (lookupEnv "GROK_TEST_MODEL")
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
                    `shouldBe` "grok functional tool ok"

                let toolHistory =
                        [userMessage "Call the echo_text tool with the text 'grok functional tool ok'."]
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
            Left err -> expectationFailure ("grok turn failed: " <> show err) >> fail "unreachable"
            Right response -> pure response

    assistantTextOf response =
        Text.toLower (fromMaybe "" (assistantText response))

--------------------------------------------------------------------------------
-- Requests
--------------------------------------------------------------------------------

helloRequest :: Text -> Text -> ResponseCreateParams
helloRequest model prompt = defaultResponseCreateParams
    { model = Just model
    , instructions = Just "You are a concise test assistant. Follow the user's instruction literally."
    , input = Just (ResponseInputItems [userMessage prompt])
    , tools = Just []
    , reasoning = Just lowReasoning
    , include = Just [ResponseInclude "reasoning.encrypted_content"]
    }

toolRequest :: Text -> ResponseCreateParams
toolRequest model = defaultResponseCreateParams
    { model = Just model
    , instructions = Just "You are a test assistant. Use the echo_text tool exactly as instructed."
    , input = Just (ResponseInputItems [userMessage "Call the echo_text tool with the text 'grok functional tool ok'."])
    , tools = Just [echoTool]
    , reasoning = Just lowReasoning
    , include = Just [ResponseInclude "reasoning.encrypted_content"]
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
            , output = Aeson.object ["echoed" Aeson..= ("grok functional tool ok" :: Text)]
            , status = Nothing
            , extraFields = mempty
            }
        , userMessage "The tool ran. Reply with exactly: done"
        ]))
    , tools = Just [echoTool]
    , reasoning = Just lowReasoning
    , include = Just [ResponseInclude "reasoning.encrypted_content"]
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

lowReasoning :: ReasoningConfig
lowReasoning = ReasoningConfig Nothing (Just "low") Nothing Nothing Nothing mempty

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

--------------------------------------------------------------------------------
-- Credential loading
--------------------------------------------------------------------------------

loadGrokCredential :: IO (Maybe Credential)
loadGrokCredential = do
    fromJson <- lookupEnv "GROK_AUTH_JSON"
    fromToken <- lookupEnv "GROK_ACCESS_TOKEN"
    let accessToken = case fromJson of
            Just raw -> accessTokenFromAuthJson (Text.pack raw)
            Nothing -> Nothing
        chosen = accessToken `orElse` fmap Text.pack fromToken
    pure $ fmap credentialFromToken chosen
  where
    orElse (Just a) _ = Just a
    orElse Nothing b = b

    credentialFromToken token = Credential
        { accessToken = token
        , accountId = fromMaybe "grok-functional" (accountIdFromAccessToken token)
        , leaseId = Nothing
        , provider = XAIProvider
        }

-- | Accepts the auth.json map, one entry of it, or @{"access_token": ...}@.
accessTokenFromAuthJson :: Text -> Maybe Text
accessTokenFromAuthJson raw = do
    value <- Aeson.decodeStrict (Text.encodeUtf8 raw)
    entryToken value `orElse` firstNestedToken value
  where
    orElse (Just a) _ = Just a
    orElse Nothing b = b

    entryToken (Aeson.Object object) =
        textField "key" object `orElse` textField "access_token" object
    entryToken _ = Nothing

    firstNestedToken (Aeson.Object object) =
        case [token | nested <- KeyMap.elems object, Just token <- [entryToken nested]] of
            (token : _) -> Just token
            [] -> Nothing
    firstNestedToken _ = Nothing

    textField name object = case KeyMap.lookup name object of
        Just (Aeson.String value) | not (Text.null value) -> Just value
        _ -> Nothing
