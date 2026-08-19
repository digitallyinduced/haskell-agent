-- | xAI Grok transport for Responses-API-shaped requests.
--
-- Application code keeps building canonical 'ResponseCreateParams' values;
-- this module maps them onto the dialect spoken by
-- xAI's subscription proxy (@cli-chat-proxy.grok.com@) and performs a plain
-- HTTP SSE round trip instead of a ChatGPT WebSocket turn.
--
-- The wire dialect matches what the open-source @xai-org/grok-build@ CLI
-- sends: the system prompt travels as a leading @role: system@ input item
-- (not the @instructions@ field), @reasoning.summary@ is @concise@,
-- @store: false@, and the whole transcript is replayed each turn — the proxy
-- is not asked to keep @previous_response_id@ state. Statefulness expected by
-- WebSocket call sites is emulated locally by 'GrokSession'.
module Agent.XAI.Grok
    ( -- * Options
      GrokOptions(..)
    , defaultGrokOptions
    , grokOptionsFromEnv
      -- * Request mapping (pure)
    , mapGrokModel
    , grokRequestValue
      -- * One-shot transport
    , createGrokMessage
    , createGrokMessageWith
    , createGrokMessageWithProvider
      -- * Stateful session (previous_response_id emulation)
    , GrokSession
    , newGrokSession
    , newGrokSessionWith
    , runGrokSessionTurn
    , runGrokSessionTurnWithBudget
      -- * Failure classification and SSE parsing (exposed for tests)
    , classifyGrokFailure
    , parseSseDataEvents
    , buildResponseFromSse
    , grokFreeLimitBody
    ) where

import Agent.Error (ApiError(..), ErrorType(..), errorTypeFromText)
import Agent.OpenAI.Error
    ( classifyHttpFailure
    , isPreviousResponseIdError
    , mkOpenAIError
    )
import qualified Agent.XAI.ContextTrim as ContextTrim
import Agent.OpenAI.ResponseMerge (mergeCompletedResponseOutput)
import Agent.Provider
    ( Credential(..)
    , Provider(..)
    , TokenProvider
    , runWithTokenProvider
    )
import Agent.OpenAI.Responses.Types
import Control.Concurrent (threadDelay)
import qualified Control.Exception.Safe as Exception
import Control.Exception.Safe (tryAny)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import qualified Data.Maybe as Maybe
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text (lenientDecode)
import qualified Network.HTTP.Client as HttpClient
import Network.HTTP.Simple hiding (Response)
import System.Environment (lookupEnv)

--------------------------------------------------------------------------------
-- Options
--------------------------------------------------------------------------------

data GrokOptions = GrokOptions
    { baseUrl :: !String
      -- ^ Subscription proxy base URL including the @/v1@ segment.
    , modelOverrides :: ![(Text, Text)]
      -- ^ Exact-match request-model → grok-model overrides.
    , defaultModel :: !Text
      -- ^ Target for any non-grok model name without an explicit override.
    , requestTimeoutSeconds :: !Int
      -- ^ Full-response timeout. Grok turns stream for minutes on high
      -- reasoning effort, so this must be far above http-client's 30s default.
    , transcriptTokenBudget :: !Int
      -- ^ Fallback cap for a session transcript when the caller sets no
      -- compaction threshold. The proxy keeps no conversation state, so every
      -- turn resends the whole transcript: without a bound, a long agent
      -- session grows until it exceeds the model's context window, and its
      -- token cost grows with the square of the turn count.
    , clientVersion :: !Text
      -- ^ Sent as @x-grok-client-version@. The proxy enforces a minimum and
      -- rejects the request with HTTP 426 otherwise — including when the
      -- header is absent, which it reports as version @none@. That floor
      -- rises over time, so keep this overridable without a release.
    } deriving (Eq, Show)

defaultGrokOptions :: GrokOptions
defaultGrokOptions = GrokOptions
    { baseUrl = "https://cli-chat-proxy.grok.com/v1"
    , modelOverrides = []
    , defaultModel = "grok-4.5"
    , requestTimeoutSeconds = 600
    -- Well under grok-4.5's 500k window, leaving room for the system prompt,
    -- tool schemas, and the turn's own output.
    , transcriptTokenBudget = 200_000
    , clientVersion = "0.2.118"
    }

-- | Environment overrides, all optional:
--
-- * @XAI_GROK_BASE_URL@ — e.g. a mock server in tests
-- * @XAI_GROK_MODEL_MAP@ — @"gpt-5.6-terra=grok-4.5,gpt-5.6-sol=grok-4.5"@
-- * @XAI_GROK_DEFAULT_MODEL@
-- * @XAI_GROK_TIMEOUT_SECONDS@
grokOptionsFromEnv :: IO GrokOptions
grokOptionsFromEnv = do
    baseUrl <- lookupEnv "XAI_GROK_BASE_URL"
    modelMap <- lookupEnv "XAI_GROK_MODEL_MAP"
    defaultModel <- lookupEnv "XAI_GROK_DEFAULT_MODEL"
    timeoutSeconds <- lookupEnv "XAI_GROK_TIMEOUT_SECONDS"
    transcriptBudget <- lookupEnv "XAI_GROK_TRANSCRIPT_TOKEN_BUDGET"
    clientVersion <- lookupEnv "XAI_GROK_CLIENT_VERSION"
    pure GrokOptions
        { baseUrl = Maybe.fromMaybe defaultGrokOptions.baseUrl (nonEmpty baseUrl)
        , modelOverrides = maybe [] (parseModelMap . Text.pack) (nonEmpty modelMap)
        , defaultModel = maybe defaultGrokOptions.defaultModel Text.pack (nonEmpty defaultModel)
        , requestTimeoutSeconds = Maybe.fromMaybe defaultGrokOptions.requestTimeoutSeconds
            (nonEmpty timeoutSeconds >>= readMaybeInt)
        , transcriptTokenBudget = Maybe.fromMaybe defaultGrokOptions.transcriptTokenBudget
            (nonEmpty transcriptBudget >>= readMaybeInt)
        , clientVersion = maybe defaultGrokOptions.clientVersion Text.pack (nonEmpty clientVersion)
        }
  where
    nonEmpty (Just value) | not (null value) = Just value
    nonEmpty _ = Nothing

    readMaybeInt value = case reads value of
        [(number, "")] -> Just number
        _ -> Nothing

parseModelMap :: Text -> [(Text, Text)]
parseModelMap raw = Maybe.mapMaybe parseEntry (Text.splitOn "," raw)
  where
    parseEntry entry = case Text.breakOn "=" entry of
        (source, target)
            | not (Text.null (Text.strip source))
            , Just stripped <- Text.stripPrefix "=" target
            , not (Text.null (Text.strip stripped)) ->
                Just (Text.strip source, Text.strip stripped)
        _ -> Nothing

--------------------------------------------------------------------------------
-- Request mapping
--------------------------------------------------------------------------------

-- | Map an OpenAI model name onto the grok model that should serve it.
-- Explicit overrides win; names that already target grok pass through; any
-- other name falls back to 'defaultModel'.
mapGrokModel :: GrokOptions -> Text -> Text
mapGrokModel options model = case lookup model options.modelOverrides of
    Just target -> target
    Nothing
        | "grok" `Text.isPrefixOf` model -> model
        | otherwise -> options.defaultModel

-- | Build the JSON body for the grok proxy from canonical Responses params.
--
-- Deliberate differences from the public Responses API JSON:
--
-- * @model@ is mapped via 'mapGrokModel'.
-- * @instructions@ becomes a leading @role: system@ message item — grok-build
--   never sends the @instructions@ field, so this is the shape the proxy
--   demonstrably accepts.
-- * @reasoning.summary@ is @concise@ (@auto@ is a ChatGPT value).
-- * @text.verbosity@ is dropped (GPT-5.x only).
-- * @tool_choice@/@parallel_tool_calls@ are omitted like grok-build does.
-- * The @web_search@ tool loses @external_web_access@; the @computer@ tool
--   has no grok equivalent and is dropped entirely.
grokRequestValue :: GrokOptions -> ResponseCreateParams -> Aeson.Value
grokRequestValue options request = Aeson.object $ Maybe.catMaybes
    [ ("model" .=) . mapGrokModel options <$> request.model
    , Just ("input" .= (systemItems <> map Aeson.toJSON (requestInputItems request)))
    , ("tools" .=) . Maybe.mapMaybe grokTool <$> request.tools
    , Just ("store" .= False)
    , Just ("stream" .= True)
    , Just ("reasoning" .= Aeson.object
        [ "effort" .= grokReasoningEffort (request.reasoning >>= (.effort))
        , "summary" .= ("concise" :: Text)
        ])
    , ("include" .=) <$> request.include
    , ("prompt_cache_key" .=) <$> request.promptCacheKey
    ]
  where
    -- Built as raw JSON because this is a transport-specific projection of
    -- the canonical request's @instructions@ field, not a response item.
    systemItems
        | Nothing <- request.instructions = []
        | Just instructions <- request.instructions
        , Text.null (Text.strip instructions) = []
        | Just instructions <- request.instructions =
            [ Aeson.object
                [ "type" .= ("message" :: Text)
                , "role" .= ("system" :: Text)
                , "content" .= [Aeson.object
                    [ "type" .= ("input_text" :: Text)
                    , "text" .= instructions
                    ]]
                ]
            ]

    grokTool tool = case tool of
        FunctionToolValue {} -> Just (Aeson.toJSON tool)
        KnownResponseTool ToolWebSearch _ -> Just (Aeson.object ["type" .= ("web_search" :: Text)])
        KnownResponseTool ToolComputer _ -> Nothing
        _ -> Just (Aeson.toJSON tool)

-- | grok-4.5 accepts only low/medium/high; reasoning cannot be disabled.
grokReasoningEffort :: Maybe Text -> Text
grokReasoningEffort = \case
    Just "medium" -> "medium"
    Just "high" -> "high"
    Just "xhigh" -> "high"
    Just "max" -> "high"
    _ -> "low"

requestInputItems :: ResponseCreateParams -> [ResponseItem]
requestInputItems request = case request.input of
    Just (ResponseInputItems items) -> items
    Just (ResponseInputText inputText) ->
        [ MessageItem ResponseMessage
            { messageId = Nothing
            , content = MessageContentText inputText
            , role = RoleUser
            , status = Nothing
            , phase = Nothing
            , extraFields = KeyMap.empty
            }
        ]
    Nothing -> []

--------------------------------------------------------------------------------
-- One-shot transport
--------------------------------------------------------------------------------

-- | Send one canonical Responses request to the Grok proxy using
-- environment-derived 'GrokOptions'.
createGrokMessage :: Credential -> ResponseCreateParams -> IO (Either ApiError Response)
createGrokMessage credential request = do
    options <- grokOptionsFromEnv
    createGrokMessageWith options credential request

createGrokMessageWith
    :: GrokOptions
    -> Credential
    -> ResponseCreateParams
    -> IO (Either ApiError Response)
createGrokMessageWith options credential request =
    performGrokTurn options credential (grokRequestValue options request) (\_ _ -> pure ())

-- | Acquire XAI credentials through the shared provider boundary and retry
-- account-scoped failures with the provider's replacement credential.
createGrokMessageWithProvider
    :: TokenProvider
    -> ResponseCreateParams
    -> IO (Either ApiError Response)
createGrokMessageWithProvider provider request =
    runWithTokenProvider provider \credential ->
        case credential.provider of
            XAIProvider -> createGrokMessage credential request
            OpenAIProvider -> pure $ Left $ ProviderError ApiErrorType
                "agent-xai requires an XAI credential"
                Nothing

-- | POST the payload to @{baseUrl}/responses@ and assemble the final response
-- from the SSE stream.
--
-- Only failures raised before the request reached the server are retried.
-- Two things must not be retried even though both surface as a
-- 'ConnectionError':
--
-- * A stream that ends in @response.failed@ (or an untyped error event). By
--   then this turn's events have already been handed to @onEvent@, so
--   re-POSTing would deliver a second, unrelated generation's events for what
--   the caller believes is one turn.
-- * A response timeout. It fires only after the body was fully sent, so the
--   server is plausibly still working on it; retrying spends the quota again
--   and would stack four 600s attempts into a ~40 minute call.
performGrokTurn
    :: GrokOptions
    -> Credential
    -> Aeson.Value
    -> (Text -> Aeson.Value -> IO ())
    -> IO (Either ApiError Response)
performGrokTurn options credential payload onEvent = attempt (0 :: Int)
  where
    attempt n = requestOnce >>= \case
        Left (Unsent _err)
            | n < connectionRetries -> do
                threadDelay (backoffMicros n)
                attempt (n + 1)
        Left (Unsent err) -> pure (Left err)
        Left (Delivered err) -> pure (Left err)
        Right response -> pure (Right response)

    connectionRetries = 3
    backoffMicros n = 1_000_000 * (2 ^ n)

    requestOnce = tryAny performRequest >>= \case
        Left exception
            | isTimeoutException exception -> pure $ Left $ Delivered $ ConnectionError
                ("Grok request timed out after " <> Text.pack (show options.requestTimeoutSeconds)
                    <> "s: " <> Text.pack (show exception))
            | otherwise -> pure $ Left $ Unsent $ ConnectionError
                ("Grok request failed: " <> Text.pack (show exception))
        Right response -> mapLeft Delivered <$> handleResponse response

    performRequest = do
        request <- parseRequest ("POST " <> trimSlash options.baseUrl <> "/responses")
        httpLBS
            $ setRequestBodyLBS (Aeson.encode payload)
            $ setRequestHeader "Authorization"
                ["Bearer " <> Text.encodeUtf8 credential.accessToken]
            -- Marks the bearer as an xAI OAuth session token (as opposed to a
            -- management/API key); required by the subscription proxy.
            $ setRequestHeader "X-XAI-Token-Auth" ["xai-grok-cli"]
            -- The proxy gates on the client version and answers HTTP 426 when
            -- it is missing or below the floor.
            $ setRequestHeader "x-grok-client-version" [Text.encodeUtf8 options.clientVersion]
            $ setRequestHeader "x-grok-client-identifier" ["grok-shell"]
            $ setRequestHeader "x-grok-client-mode" ["interactive"]
            $ setRequestHeader "Content-Type" ["application/json"]
            $ setRequestHeader "Accept" ["text/event-stream"]
            $ setRequestHeader "User-Agent" ["codex-hs"]
            $ setTimeout request

    setTimeout request = request
        { HttpClient.responseTimeout =
            HttpClient.responseTimeoutMicro (options.requestTimeoutSeconds * 1_000_000)
        }

    handleResponse response = do
        let status = getResponseStatusCode response
            bodyText = Text.decodeUtf8With Text.lenientDecode (LBS.toStrict (getResponseBody response))
        if status >= 200 && status < 300
            then do
                let events = parseSseDataEvents bodyText
                mapM_ (uncurry onEvent) events
                pure (buildResponseFromSse events)
            else pure $ Left $ classifyGrokFailure status (retryAfterSeconds response) bodyText

    retryAfterSeconds response =
        case getResponseHeader "Retry-After" response of
            (value : _) -> case reads (BS8.unpack value) of
                [(seconds, "")] -> Just (max 1 seconds)
                _ -> Nothing
            [] -> Nothing

-- | Whether a failed attempt is safe to repeat. 'Unsent' means the request
-- never reached the server and delivered nothing to the caller; 'Delivered'
-- means it did one or both, so repeating it is observable.
data TurnFailure = Unsent !ApiError | Delivered !ApiError

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right

isTimeoutException :: Exception.SomeException -> Bool
isTimeoutException exception =
    case Exception.fromException exception of
        Just (HttpClient.HttpExceptionRequest _ content) -> case content of
            HttpClient.ResponseTimeout -> True
            HttpClient.ConnectionTimeout -> True
            _ -> False
        _ -> False

--------------------------------------------------------------------------------
-- Failure classification
--------------------------------------------------------------------------------

-- | Classify a non-2xx proxy response. The proxy speaks the OpenAI error
-- envelope, so 'classifyHttpFailure' does the heavy lifting; on top of that:
--
-- * the free-quota upsell body becomes 'UsageLimitReached' so accounts cool
--   down for the server-suggested interval instead of being hammered,
-- * a bare 429 becomes a typed 'RateLimitError',
-- * a @Retry-After@ header fills in the cooldown when the body carries none.
classifyGrokFailure :: Int -> Maybe Int -> Text -> ApiError
classifyGrokFailure status retryAfterHeader body
    -- 426 is the proxy refusing this client build, not an account problem.
    -- Surfacing it as an invalid request keeps the failover layer from
    -- cooling down a perfectly healthy account (and every replacement in
    -- turn) over a header we control.
    | status == 426 =
        ProviderError InvalidRequestError
            ("Grok proxy rejected the client version: " <> Text.take 500 body)
            Nothing
    | grokFreeLimitBody body =
        ProviderError UsageLimitReached (Text.take 500 body) retryAfterHeader
    | otherwise = case classifyHttpFailure status body of
        ProviderError errType message retryAfter ->
            ProviderError errType message (orElseMaybe retryAfter retryAfterHeader)
        HttpError s b
            | s == 429 -> ProviderError RateLimitError (Text.take 500 b) retryAfterHeader
        other -> other
  where
    orElseMaybe (Just a) _ = Just a
    orElseMaybe Nothing b = b

-- | The proxy reports an exhausted subscription window with an upsell text
-- rather than a typed error. Match the same markers grok-build matches.
grokFreeLimitBody :: Text -> Bool
grokFreeLimitBody body =
    "grok.com/supergrok" `Text.isInfixOf` lowered
        || "upgrade to a grok subscription" `Text.isInfixOf` lowered
  where
    lowered = Text.toLower body

--------------------------------------------------------------------------------
-- SSE parsing
--------------------------------------------------------------------------------

-- | Parse an SSE body into @(event type, data object)@ pairs, in order.
-- The event type comes from the @event:@ line when present, otherwise from
-- the data object's own @type@ field. Non-object data payloads (for example
-- a @[DONE]@ sentinel) are skipped.
parseSseDataEvents :: Text -> [(Text, Aeson.Value)]
parseSseDataEvents sseText = Maybe.mapMaybe parseBlock (Text.splitOn "\n\n" normalized)
  where
    normalized = Text.replace "\r\n" "\n" sseText

    parseBlock block = do
        let blockLines = Text.lines block
            eventLine = Maybe.listToMaybe
                [ Text.strip (Text.drop 6 line)
                | line <- blockLines
                , "event:" `Text.isPrefixOf` line
                ]
            dataText = Text.intercalate "\n"
                [ Text.strip (Text.drop 5 line)
                | line <- blockLines
                , "data:" `Text.isPrefixOf` line
                ]
        value <- case Aeson.eitherDecodeStrict' (Text.encodeUtf8 dataText) of
            Right object@(Aeson.Object _) -> Just object
            _ -> Nothing
        let typeField = case value of
                Aeson.Object object -> case KeyMap.lookup "type" object of
                    Just (Aeson.String t) -> Just t
                    _ -> Nothing
                _ -> Nothing
        eventType <- eventLine `orElse` typeField
        pure (eventType, value)

    orElse (Just a) _ = Just a
    orElse Nothing b = b

-- | Assemble the final 'Response' from parsed SSE events, mirroring the
-- WebSocket client's accumulation: output items arrive in
-- @response.output_item.done@ events and are merged into the final
-- @response.completed@ payload, which may itself carry a partial output
-- array. Failure events take precedence over a missing completion.
buildResponseFromSse :: [(Text, Aeson.Value)] -> Either ApiError Response
buildResponseFromSse events =
    case completedResponse of
        Just responseValue ->
            let patched = mergeCompletedResponseOutput doneItems responseValue
            in case Aeson.fromJSON patched of
                Aeson.Success response -> Right response
                Aeson.Error err -> Left $ JsonDecodeError
                    (Text.pack err)
                    (Text.take 2000 (Text.decodeUtf8With Text.lenientDecode (LBS.toStrict (Aeson.encode patched))))
        Nothing -> Left failureError
  where
    completedResponse = lastMaybe
        [ responseValue
        | ("response.completed", Aeson.Object object) <- events
        , Just responseValue <- [KeyMap.lookup "response" object]
        ]

    doneItems =
        [ item
        | ("response.output_item.done", Aeson.Object object) <- events
        , Just item <- [KeyMap.lookup "item" object]
        ]

    failureError = case Maybe.listToMaybe (Maybe.mapMaybe failureEvent events) of
        Just err -> err
        Nothing -> JsonDecodeError
            "No response.completed event found in SSE stream"
            (Text.take 2000 (Text.decodeUtf8With Text.lenientDecode (LBS.toStrict (Aeson.encode (map snd events)))))

    failureEvent (eventType, value) = case eventType of
        "error" -> Just (parseErrorEvent value)
        "response.failed" -> Just (parseFailedEvent value)
        _ -> Nothing

    parseErrorEvent value = case value of
        Aeson.Object object ->
            let errObject = case KeyMap.lookup "error" object of
                    Just (Aeson.Object nested) -> nested
                    _ -> object
                typedError = case KeyMap.lookup "type" errObject of
                    Just (Aeson.String t) | t /= "error" -> Just (errorTypeFromText t)
                    _ -> Nothing
                message = case KeyMap.lookup "message" errObject of
                    Just (Aeson.String t) -> t
                    _ -> Text.decodeUtf8With Text.lenientDecode (LBS.toStrict (Aeson.encode value))
                code = case KeyMap.lookup "code" errObject of
                    Just (Aeson.String t) -> Just t
                    _ -> Nothing
                retryAfter = case KeyMap.lookup "resets_in_seconds" errObject of
                    Just (Aeson.Number n) -> toBoundedInteger n
                    _ -> Nothing
            in if grokFreeLimitBody message
                then ProviderError UsageLimitReached message retryAfter
                else case typedError of
                    Just errType -> mkOpenAIError errType message code retryAfter
                    -- Mirror the WebSocket client: a typeless error event is a
                    -- transport-level failure unless it identifies a missing
                    -- previous_response_id via code or message.
                    Nothing ->
                        let parsed = mkOpenAIError (UnknownErrorType "") message code retryAfter
                        in if isPreviousResponseIdError parsed
                            then parsed
                            else ConnectionError ("Grok stream error: " <> message)
        _ -> ConnectionError "Grok stream error event"

    parseFailedEvent value = case value of
        Aeson.Object object -> case KeyMap.lookup "response" object of
            Just (Aeson.Object responseObject) -> case KeyMap.lookup "status_details" responseObject of
                Just details -> ConnectionError
                    (Text.decodeUtf8With Text.lenientDecode (LBS.toStrict (Aeson.encode details)))
                Nothing -> ConnectionError "response.failed (no details)"
            _ -> ConnectionError "response.failed"
        _ -> ConnectionError "response.failed"


    lastMaybe [] = Nothing
    lastMaybe [x] = Just x
    lastMaybe (_ : rest) = lastMaybe rest

--------------------------------------------------------------------------------
-- Stateful session
--------------------------------------------------------------------------------

-- | Emulates the ChatGPT WebSocket's @previous_response_id@ contract over the
-- stateless grok proxy. The session records the full item transcript of the
-- last successful turn; a follow-up turn that references that response id is
-- replayed as stored transcript + new delta items. A reference to any other
-- response id yields 'PreviousResponseNotFound', which callers already treat
-- as "rebuild the full input and retry" on the ChatGPT path.
data GrokSession = GrokSession
    { sessionOptions :: !GrokOptions
    , sessionCredential :: !Credential
    , sessionTranscript :: !(IORef (Maybe (Text, [ResponseItem])))
    }

newGrokSession :: Credential -> IO GrokSession
newGrokSession credential = do
    options <- grokOptionsFromEnv
    newGrokSessionWith options credential

newGrokSessionWith :: GrokOptions -> Credential -> IO GrokSession
newGrokSessionWith options credential = do
    transcript <- newIORef Nothing
    pure GrokSession
        { sessionOptions = options
        , sessionCredential = credential
        , sessionTranscript = transcript
        }

-- | Run one turn. Matches the shape of
-- WebSocket-style request handling: the callback receives
-- every SSE event (type + JSON object) before the final response is returned.
-- Events are currently delivered after the stream has completed, not live.
runGrokSessionTurn
    :: GrokSession
    -> ResponseCreateParams
    -> Maybe Text
    -> (Text -> Aeson.Value -> IO ())
    -> IO (Either ApiError Response)
runGrokSessionTurn session = runGrokSessionTurnWithBudget session Nothing

-- | Like 'runGrokSessionTurn', with the caller's context bound for this turn.
-- On the WebSocket transport that number is the server-side compaction
-- threshold; here it caps the transcript this session replays, which is the
-- only thing standing between a long agent session and its own context
-- window.
runGrokSessionTurnWithBudget
    :: GrokSession
    -> Maybe Int
    -> ResponseCreateParams
    -> Maybe Text
    -> (Text -> Aeson.Value -> IO ())
    -> IO (Either ApiError Response)
runGrokSessionTurnWithBudget session turnBudget request previousResponseId onEvent = do
    stored <- readIORef session.sessionTranscript
    case resolveInput stored of
        Left err -> pure (Left err)
        Right resolvedInput -> do
            let payload = grokRequestValue session.sessionOptions
                    (setRequestInput (Just (ResponseInputItems resolvedInput)) request)
            result <- performGrokTurn session.sessionOptions session.sessionCredential payload onEvent
            case result of
                Right response -> do
                    -- The system prompt is not part of the transcript: it is
                    -- re-derived from the request's instructions every turn,
                    -- exactly like the instructions field on the ChatGPT path.
                    --
                    -- Trimming here rather than at send time keeps what this
                    -- session believes it sent equal to what it actually sent
                    -- on the next turn. The spine is forced before storing:
                    -- each turn appends to the previous transcript, so lazy
                    -- appends would nest one level deeper per turn and make a
                    -- long session quadratic in its own history.
                    let nextTranscript = ContextTrim.trimResponsesItemHistory
                            (Maybe.fromMaybe session.sessionOptions.transcriptTokenBudget turnBudget)
                            (resolvedInput <> response.output)
                    length nextTranscript `seq` writeIORef session.sessionTranscript
                        (Just (response.responseId, nextTranscript))
                Left _ ->
                    -- Keep the previous transcript: a transient failure does
                    -- not invalidate the response id the caller will retry
                    -- against.
                    pure ()
            pure result
  where
    resolveInput stored = case previousResponseId of
        Nothing -> Right (requestInputItems request)
        Just wanted -> case stored of
            Just (lastResponseId, transcript)
                | lastResponseId == wanted -> Right (transcript <> requestInputItems request)
            _ -> Left $ ProviderError
                PreviousResponseNotFound
                ("Grok transport has no local transcript for previous_response_id " <> wanted)
                Nothing

setRequestInput :: Maybe ResponseInput -> ResponseCreateParams -> ResponseCreateParams
setRequestInput newInput ResponseCreateParams { input = _, .. } =
    ResponseCreateParams { input = newInput, .. }

trimSlash :: String -> String
trimSlash = reverse . dropWhile (== '/') . reverse
