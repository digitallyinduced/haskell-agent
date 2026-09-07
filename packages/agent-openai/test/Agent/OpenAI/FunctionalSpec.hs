module Agent.OpenAI.FunctionalSpec where

import Data.List.NonEmpty (NonEmpty((:|)))
import Test.Hspec

import qualified Agent.OpenAI.Auth as Auth
import Agent.Json (rawJsonFromEncoding)
import qualified Agent.Json.Decode as Json
import qualified Agent.Responses.Codec as ResponsesCodec
import qualified Agent.Responses.Types as OpenAI
import Agent.OpenAI.WebSocketClient (CodexConn, sendWsRequestWithRawEvents, withCodexWs)

import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.IORef
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, getCurrentTime)
import System.Environment (lookupEnv)

spec :: Spec
spec = describe "Codex functional streaming" do
    it "streams and decodes hello-world, function-tool, and web-search turns without unknown events" do
        loaded <- loadFunctionalAuth
        case loaded of
            Nothing ->
                pendingWith "set CODEX_AUTH_JSON or CODEX_ACCESS_TOKEN (+ CODEX_ACCOUNT_ID or CODEX_ID_TOKEN) with a fresh access token to run the live Codex functional test"
            Just authState -> do
                model <- fmap (Text.pack . fromMaybe "gpt-5.5") (lookupEnv "CODEX_TEST_MODEL")
                pool <- Auth.newPool (authState :| []) \state -> pure (Right state)
                withCodexWs pool \conn _accountId -> do
                    first <- runHelloTurn conn model Nothing "Reply with exactly: hello world" "hello world"
                    _second <- runHelloTurn conn model (Just first.responseId) "Reply with exactly: hello again" "hello again"
                    runFunctionToolLoop conn model
                    runWebSearchTurn conn model
                    pure ()

runHelloTurn ::
    CodexConn ->
    Text ->
    Maybe Text ->
    Text ->
    Text ->
    IO OpenAI.Response
runHelloTurn conn model previousResponseId prompt expectedText = do
    (response, decodedEvents) <- runCodexTurn conn (helloRequest model prompt) previousResponseId
    decodedEvents `shouldSatisfy` any isOutputTextDelta
    Text.toLower (fromMaybe "" (extractAssistantText response))
        `shouldSatisfy` Text.isInfixOf expectedText
    pure response

runFunctionToolLoop :: CodexConn -> Text -> IO ()
runFunctionToolLoop conn model = do
    (toolResponse, _events) <- runCodexTurn conn (functionToolRequest model) Nothing
    call <- case [functionCall | OpenAI.FunctionCallItem functionCall <- toolResponse.output
                              , functionCall.name == "echo_text"] of
        (functionCall : _) -> pure functionCall
        [] -> expectationFailure ("expected echo_text function_call, got: " <> show toolResponse.output) >> fail "unreachable"
    let toolText = functionCallArgumentText "text" call.arguments
    toolText `shouldBe` "codex functional tool ok"

    (finalResponse, decodedEvents) <- runCodexTurn
        conn
        (functionToolOutputRequest model call.callId ("TOOL_RESULT:" <> toolText))
        (Just toolResponse.responseId)
    decodedEvents `shouldSatisfy` any isOutputTextDelta
    Text.toLower (fromMaybe "" (extractAssistantText finalResponse))
        `shouldSatisfy` Text.isInfixOf "tool_result:codex functional tool ok"

runWebSearchTurn :: CodexConn -> Text -> IO ()
runWebSearchTurn conn model = do
    (response, decodedEvents) <- runCodexTurn conn (webSearchRequest model) Nothing
    decodedEvents `shouldSatisfy` any isOutputItemDone
    response.output `shouldSatisfy` any isWebSearchCall
    Text.toLower (fromMaybe "" (extractAssistantText response))
        `shouldSatisfy` Text.isInfixOf "openai.com"
  where
    isWebSearchCall OpenAI.WebSearchCallItem{} = True
    isWebSearchCall _ = False

runCodexTurn ::
    CodexConn ->
    OpenAI.ResponseCreateParams ->
    Maybe Text ->
    IO (OpenAI.Response, [OpenAI.ResponseStreamEvent])
runCodexTurn conn request previousResponseId = do
    eventsRef <- newIORef ([] :: [(Text, Either Text OpenAI.ResponseStreamEvent)])
    result <- sendWsRequestWithRawEvents conn request previousResponseId \eventType value ->
        modifyIORef' eventsRef
            (( eventType
             , ResponsesCodec.decodeResponseStreamEventWithType eventType
                (LBS8.toStrict (Aeson.encode value))
             ) :)
    events <- reverse <$> readIORef eventsRef
    let decodeFailures = [err | (_, Left err) <- events]
        decodedEvents = [event | (_, Right event) <- events]
        eventTypes = map fst events
        unknowns =
            [ OpenAI.responseStreamEventType event
            | event <- decodedEvents
            , OpenAI.StreamEventUnknown{} <- [OpenAI.responseStreamEventType event]
            ]
    decodeFailures `shouldBe` []
    unknowns `shouldBe` []
    case result of
        Left err -> expectationFailure ("Codex request failed: " <> show err) >> fail "unreachable"
        Right response -> do
            eventTypes `shouldSatisfy` elem "response.completed"
            decodedEvents `shouldSatisfy` any isOutputItemDone
            pure (response, decodedEvents)

isOutputTextDelta :: OpenAI.ResponseStreamEvent -> Bool
isOutputTextDelta event = OpenAI.responseStreamEventType event == OpenAI.EventOutputTextDelta

isOutputItemDone :: OpenAI.ResponseStreamEvent -> Bool
isOutputItemDone event = OpenAI.responseStreamEventType event == OpenAI.EventOutputItemDone

helloRequest :: Text -> Text -> OpenAI.ResponseCreateParams
helloRequest model prompt =
    OpenAI.defaultResponseCreateParams
        { OpenAI.model = Just model
        , OpenAI.instructions = Just "You are a functional test responder. Do not use tools. Return only the exact requested text."
        , OpenAI.input = Just
            (OpenAI.ResponseInputItems [userMessage prompt])
        , OpenAI.tools = Just []
        , OpenAI.reasoning = Just (reasoning "low")
        , OpenAI.include = Just []
        , OpenAI.promptCacheKey = Just "codex-hs-functional-test"
        , OpenAI.text = Just lowVerbosity
        }

functionToolRequest :: Text -> OpenAI.ResponseCreateParams
functionToolRequest model =
    OpenAI.defaultResponseCreateParams
        { OpenAI.model = Just model
        , OpenAI.instructions = Just "You are a functional test responder. You must call echo_text exactly once with text='codex functional tool ok'. Do not answer directly before the tool result."
        , OpenAI.input = Just (OpenAI.ResponseInputItems
            [userMessage "Call echo_text now."])
        , OpenAI.tools = Just [echoTextTool]
        , OpenAI.reasoning = Just (reasoning "low")
        , OpenAI.include = Just []
        , OpenAI.promptCacheKey = Just "codex-hs-functional-test-tools"
        , OpenAI.text = Just lowVerbosity
        }

functionToolOutputRequest :: Text -> Text -> Text -> OpenAI.ResponseCreateParams
functionToolOutputRequest model callId output =
    OpenAI.defaultResponseCreateParams
        { OpenAI.model = Just model
        , OpenAI.instructions = Just "You are a functional test responder. After receiving a tool result, reply with exactly the tool result text and nothing else."
        , OpenAI.input = Just (OpenAI.ResponseInputItems [functionOutput callId output])
        , OpenAI.tools = Just [echoTextTool]
        , OpenAI.reasoning = Just (reasoning "low")
        , OpenAI.include = Just []
        , OpenAI.promptCacheKey = Just "codex-hs-functional-test-tools"
        , OpenAI.text = Just lowVerbosity
        }

echoTextTool :: OpenAI.ResponseTool
echoTextTool =
    OpenAI.FunctionToolValue OpenAI.FunctionTool
        { OpenAI.name = "echo_text"
        , OpenAI.description = Just "Echoes the provided text."
        , OpenAI.parameters = Just (rawJsonFromEncoding (Aeson.toEncoding (Aeson.object
            [ "type" Aeson..= ("object" :: Text)
            , "properties" Aeson..= Aeson.object
                [ "text" Aeson..= Aeson.object
                    [ "type" Aeson..= ("string" :: Text)
                    , "description" Aeson..= ("Text to echo." :: Text)
                    ]
                ]
            , "required" Aeson..= (["text"] :: [Text])
            , "additionalProperties" Aeson..= False
            ])))
        , OpenAI.strict = Just True
        , OpenAI.async = Nothing
        }

webSearchRequest :: Text -> OpenAI.ResponseCreateParams
webSearchRequest model =
    OpenAI.defaultResponseCreateParams
        { OpenAI.model = Just model
        , OpenAI.instructions = Just "You are a functional test responder. You must use the web_search tool before answering. Answer exactly with the official domain openai.com."
        , OpenAI.input = Just (OpenAI.ResponseInputItems
            [userMessage
                "Use web search to confirm the official OpenAI website domain, then answer exactly with the domain."])
        , OpenAI.tools = Just
            [OpenAI.KnownResponseTool OpenAI.ToolWebSearch]
        , OpenAI.reasoning = Just (reasoning "low")
        , OpenAI.include = Just []
        , OpenAI.promptCacheKey = Just "codex-hs-functional-test-web-search"
        , OpenAI.text = Just lowVerbosity
        }

reasoning :: Text -> OpenAI.ReasoningConfig
reasoning effort =
    OpenAI.ReasoningConfig Nothing (Just effort) Nothing Nothing Nothing

lowVerbosity :: OpenAI.ResponseTextConfig
lowVerbosity = OpenAI.ResponseTextConfig Nothing (Just "low")

functionOutput :: Text -> Text -> OpenAI.ResponseItem
functionOutput callId output = OpenAI.FunctionCallOutputItem OpenAI.FunctionCallOutput
    { OpenAI.localOutcome = Nothing
    , OpenAI.itemId = Nothing
    , OpenAI.callId = callId
    , OpenAI.name = Nothing
    , OpenAI.namespace = Nothing
    , OpenAI.provider = Nothing
    , OpenAI.output =
        rawJsonFromEncoding (Aeson.toEncoding (Aeson.String output))
    , OpenAI.status = Nothing
    , OpenAI.async = Nothing
    }

userMessage :: Text -> OpenAI.ResponseItem
userMessage content =
    OpenAI.MessageItem OpenAI.ResponseMessage
        { OpenAI.messageId = Nothing
        , OpenAI.content = OpenAI.MessageContentText content
        , OpenAI.role = OpenAI.RoleUser
        , OpenAI.status = Nothing
        , OpenAI.phase = Nothing
        , OpenAI.passthrough = Nothing
        }

functionCallArgumentText :: Text -> Text -> Text
functionCallArgumentText key arguments =
    either (const "") id $
        Json.decodeText (Json.object (Json.atKey key Json.text)) arguments

extractAssistantText :: OpenAI.Response -> Maybe Text
extractAssistantText response = case
    [ value
    | OpenAI.MessageItem message <- response.output
    , message.role == OpenAI.RoleAssistant
    , value <- case message.content of
        OpenAI.MessageContentText text -> [text]
        OpenAI.MessageContentParts parts ->
            [text | OpenAI.OutputTextPart { OpenAI.text } <- parts]
    ] of
        [] -> Nothing
        values -> Just (Text.intercalate "\n" values)

loadFunctionalAuth :: IO (Maybe Auth.AuthState)
loadFunctionalAuth = do
    now <- getCurrentTime
    authJson <- lookupEnv "CODEX_AUTH_JSON"
    loaded <- case authJson >>= parseAuthJson . LBS8.pack of
        Just authFile -> pure (Just (authStateFromFile now authFile))
        Nothing -> loadDirectAccessToken now
    pure (loaded >>= freshOnly now)

loadDirectAccessToken :: UTCTime -> IO (Maybe Auth.AuthState)
loadDirectAccessToken now = do
    accessToken <- fmap Text.pack <$> lookupEnv "CODEX_ACCESS_TOKEN"
    accountIdEnv <- fmap Text.pack <$> lookupEnv "CODEX_ACCOUNT_ID"
    idToken <- fmap Text.pack <$> lookupEnv "CODEX_ID_TOKEN"
    case accessToken of
        Nothing -> pure Nothing
        Just token -> do
            let accountId = accountIdEnv <|> (idToken >>= Auth.deriveAccountId)
            pure $ Auth.AuthState token "unused" <$> accountId <*> pure idToken <*> pure now

freshOnly :: UTCTime -> Auth.AuthState -> Maybe Auth.AuthState
freshOnly now state
    | Auth.needsRefresh state now = Nothing
    | otherwise = Just state

parseAuthJson :: LBS8.ByteString -> Maybe AuthFile
parseAuthJson bytes =
    case Json.decodeEither (Json.list authFileDecoder) (LBS8.toStrict bytes) of
        Right authFiles -> listToMaybe authFiles
        Left _ -> either (const Nothing) Just
            (Json.decodeEither authFileDecoder (LBS8.toStrict bytes))

authStateFromFile :: UTCTime -> AuthFile -> Auth.AuthState
authStateFromFile now AuthFile{tokens = AuthFileTokens{accessToken, refreshToken, accountId, idToken}} =
    Auth.AuthState
        { Auth.accessToken = accessToken
        , Auth.refreshToken = refreshToken
        , Auth.accountId = accountId
        , Auth.idToken = idToken
        , Auth.lastRefresh = now
        }

data AuthFile = AuthFile {tokens :: !AuthFileTokens}

data AuthFileTokens = AuthFileTokens
    { accessToken :: !Text
    , refreshToken :: !Text
    , accountId :: !Text
    , idToken :: !(Maybe Text)
    }

authFileDecoder :: Json.Decoder AuthFile
authFileDecoder =
    Json.object $ AuthFile <$> Json.atKey "tokens" authFileTokensDecoder

authFileTokensDecoder :: Json.Decoder AuthFileTokens
authFileTokensDecoder = Json.object $
    AuthFileTokens
        <$> Json.atKey "access_token" Json.text
        <*> Json.atKey "refresh_token" Json.text
        <*> Json.atKey "account_id" Json.text
        <*> Json.atKeyOptional "id_token" Json.text
