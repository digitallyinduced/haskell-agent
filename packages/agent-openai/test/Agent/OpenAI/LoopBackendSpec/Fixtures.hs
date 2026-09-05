module Agent.OpenAI.LoopBackendSpec.Fixtures
    ( isToolStartedEvent
    , submitWithState
    , loopConfig
    , emptyRegistry
    , baseParams
    , withModel
    , withPromptCacheRetention
    , withEffort
    , reasoningEffort
    , recordingSend
    , scriptedStatelessSend
    , functionResult
    , customResult
    , functionCallItem
    , functionCallItemWithExtras
    , customCallItem
    , assistantItem
    , reasoningItem
    , reasoningIncompleteResponse
    , compactionItem
    , deltaEvent
    , codexRateLimitsEvent
    , testResponse
    , testResponseWithUsage
    , itemType
    , inputItems
    ) where

import Agent.Cancel (newCancelFlag)
import Agent.Error (ApiError(..))
import qualified Agent.Responses.Codec as ResponsesCodec
import Agent.Loop
import Agent.Responses.Types
import Agent.ToolDispatch
import Agent.Tools.Types (ToolRegistry, mkToolRegistry)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text

isToolStartedEvent :: LoopEvent -> Bool
isToolStartedEvent = \case
    ToolStarted _ -> True
    _ -> False

submitWithState
    :: IORef [ResponseItem]
    -> Backend
    -> Maybe Text
    -> [TurnInput]
    -> (LoopEvent -> IO ())
    -> IO (Either ApiError TurnOutput)
submitWithState stateRef backend previous inputs onEvent = do
    state <- readIORef stateRef
    result <- backend.submitTurn
        (initialBackendSnapshot state) previous inputs onEvent
    case result of
        Left err -> pure (Left err)
        Right BackendResult{..} -> do
            writeIORef stateRef backendState.backendItems
            pure (Right backendOutput)

loopConfig :: Backend -> IO LoopConfig
loopConfig backend = do
    state <- newIORef emptyBackendSnapshot
    cancel <- newCancelFlag
    pure LoopConfig
        { loopBackend = backend
        , loopBackendState = BackendStateStore
            { readBackendState = readIORef state
            , commitBackendState = \snapshot -> do
                writeIORef state snapshot
                pure snapshot
            }
        , loopTools = emptyRegistry
        , loopDispatch = defaultLoopDispatch
        , loopMaxTurns = defaultLoopMaxTurns
        , loopOnEvent = const (pure ())
        , loopApprove = const (pure (Right True))
        , loopReadSteering = pure []
        , loopCommitSteering = const (pure ())
        , loopInterrupt = pure ()
        , loopCancel = cancel
        }

emptyRegistry :: ToolRegistry
emptyRegistry =
    either (error . Text.unpack) id (mkToolRegistry [])

baseParams :: ResponseCreateParams
baseParams = withModel (Just "gpt-5.6-luna") defaultResponseCreateParams

withModel :: Maybe Text -> ResponseCreateParams -> ResponseCreateParams
withModel nextModel ResponseCreateParams { model = _, .. } =
    ResponseCreateParams { model = nextModel, .. }

withPromptCacheRetention
    :: Maybe Text -> ResponseCreateParams -> ResponseCreateParams
withPromptCacheRetention nextRetention
        ResponseCreateParams { promptCacheRetention = _, .. } =
    ResponseCreateParams { promptCacheRetention = nextRetention, .. }

withEffort :: Text -> ResponseCreateParams -> ResponseCreateParams
withEffort effort ResponseCreateParams { reasoning = _, .. } =
    ResponseCreateParams
        { reasoning = Just ReasoningConfig
            { context = Nothing
            , effort = Just effort
            , generateSummary = Nothing
            , reasoningMode = Nothing
            , summary = Nothing
            }
        , ..
        }

reasoningEffort :: ResponseCreateParams -> Maybe Text
reasoningEffort request = request.reasoning >>= (.effort)

recordingSend
    :: IORef [(ResponseCreateParams, Maybe Text)]
    -> ResponseCreateParams
    -> Maybe Text
    -> (ResponseStreamEvent -> IO ())
    -> IO (Either ApiError Response)
recordingSend seen request previous onEvent = do
    modifyIORef' seen (++ [(request, previous)])
    onEvent (deltaEvent EventOutputTextDelta "ok")
    pure $ Right (testResponse "resp-1" [assistantItem "ok"])

scriptedStatelessSend
    :: IORef [ResponseCreateParams]
    -> IORef [Response]
    -> ResponseCreateParams
    -> (ResponseStreamEvent -> IO ())
    -> IO (Either ApiError Response)
scriptedStatelessSend seen remaining request onEvent = do
    modifyIORef' seen (++ [request])
    next <- atomicModifyIORef' remaining \case
        [] -> ([], Nothing)
        response : rest -> (rest, Just response)
    case next of
        Nothing -> pure (Left (ConnectionError "scripted backend exhausted"))
        Just response -> do
            onEvent (deltaEvent EventOutputTextDelta "call")
            pure (Right response)

functionResult :: Text -> Text -> ToolCallResult
functionResult callId output = ToolCallResult
    { callId
    , output
    , callKind = FunctionCallKind
    , toolResultMode = BlockingToolCall
    , toolResultImages = []
    , toolResultOutcome = Nothing
    }

customResult :: Text -> Text -> ToolCallResult
customResult callId output = ToolCallResult
    { callId
    , output
    , callKind = CustomCallKind
    , toolResultMode = BlockingToolCall
    , toolResultImages = []
    , toolResultOutcome = Nothing
    }

functionCallItem :: Text -> Text -> Text -> ResponseItem
functionCallItem callId name arguments =
    functionCallItemWithExtras callId name arguments KeyMap.empty

functionCallItemWithExtras
    :: Text
    -> Text
    -> Text
    -> Aeson.Object
    -> ResponseItem
functionCallItemWithExtras callId name arguments metadataFields =
    FunctionCallItem FunctionCall
    { itemId = Nothing
    , callId
    , name
    , namespace = case KeyMap.lookup "namespace" metadataFields of
        Just (Aeson.String value) -> Just value
        _ -> Nothing
    , provider = case KeyMap.lookup "provider" metadataFields of
        Just (Aeson.String value) -> Just value
        _ -> Nothing
    , arguments
    , encryptedFunctionArgs =
        case KeyMap.lookup "encrypted_function_args" metadataFields of
            Just (Aeson.Array _) -> Just []
            _ -> Nothing
    , status = Just ItemCompleted
    , async = Nothing
    }

customCallItem :: Text -> Text -> Text -> ResponseItem
customCallItem callId name input = CustomToolCallItem CustomToolCall
    { itemId = Nothing
    , callId
    , name
    , namespace = Nothing
    , input
    , status = Just ItemCompleted
    , async = Nothing
    }

assistantItem :: Text -> ResponseItem
assistantItem text = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentParts [OutputTextPart text Nothing Nothing]
    , role = RoleAssistant
    , status = Just ItemCompleted
    , phase = Nothing
    , passthrough = Nothing
    }

reasoningItem :: Text -> ResponseItem
reasoningItem itemId = ReasoningItemValue ReasoningItem
    { itemId = Just itemId
    , summary = []
    , content = Nothing
    , encryptedContent = Just "opaque"
    , status = Just ItemCompleted
    }

reasoningIncompleteResponse :: Response
reasoningIncompleteResponse =
    (testResponseWithUsage
        "resp-reasoning-incomplete"
        [reasoningItem "rs-1"]
        (Aeson.object
            [ "input_tokens" Aeson..= (64000 :: Int)
            , "output_tokens" Aeson..= (128000 :: Int)
            , "total_tokens" Aeson..= (192000 :: Int)
            , "output_tokens_details" Aeson..= Aeson.object
                [ "reasoning_tokens" Aeson..= (53 :: Int)
                ]
            ]))
        { status = ResponseIncomplete
        , incompleteDetails = Just IncompleteDetails
            { reason = "max_output_tokens"
            }
        }

compactionItem :: Text -> ResponseItem
compactionItem _ = CompactionItemValue CompactionItem
    { itemId = Nothing
    , encryptedContent = Nothing
    }

deltaEvent :: StreamEventType -> Text -> ResponseStreamEvent
deltaEvent otherEventType delta = OtherResponseStreamEvent
    { otherEventType
    , sequenceNumber = Nothing
    , eventDelta = Just delta
    , streamItemId = Nothing
    , streamOutputIndex = Nothing
    , summaryIndex = Nothing
    , turnState = Nothing
    }

codexRateLimitsEvent :: Aeson.Value -> ResponseStreamEvent
codexRateLimitsEvent rateLimits = ResponseCodexRateLimitsEvent
    { rateLimits = CodexRateLimits
        { allowed = boolField "allowed" rateLimits
        , limitReached = boolField "limit_reached" rateLimits
        , primaryUsedPercent = percentField "primary" rateLimits
        , secondaryUsedPercent = percentField "secondary" rateLimits
        }
    , sequenceNumber = Nothing
    }
  where
    boolField name (Aeson.Object object) =
        case KeyMap.lookup name object of
            Just (Aeson.Bool value) -> Just value
            _ -> Nothing
    boolField _ _ = Nothing
    percentField name (Aeson.Object object) =
        case KeyMap.lookup name object of
            Just (Aeson.Object window) ->
                case KeyMap.lookup "used_percent" window of
                    Just (Aeson.Number value) -> Just (realToFrac value)
                    _ -> Nothing
            _ -> Nothing
    percentField _ _ = Nothing

testResponse :: Text -> [ResponseItem] -> Response
testResponse responseId output = testResponseWithUsage responseId output Aeson.Null

testResponseWithUsage :: Text -> [ResponseItem] -> Aeson.Value -> Response
testResponseWithUsage responseId output usage =
    case ResponsesCodec.decodeResponse
        (LBS.toStrict (Aeson.encode (Aeson.object $
    [ "id" Aeson..= responseId
    , "created_at" Aeson..= (0 :: Int)
    , "model" Aeson..= ("test-model" :: Text)
    , "status" Aeson..= ("completed" :: Text)
    , "output" Aeson..= output
    ] <> usageField))) of
        Right response -> response
        Left err -> error (Text.unpack err)
  where
    usageField = case usage of
        Aeson.Null -> []
        value -> ["usage" Aeson..= value]

itemType :: Aeson.ToJSON a => a -> Text
itemType value = case Aeson.toJSON value of
    Aeson.Object object -> case KeyMap.lookup "type" object of
        Just (Aeson.String tag) -> tag
        _ -> ""
    _ -> ""

inputItems :: ResponseCreateParams -> [ResponseItem]
inputItems request = case request.input of
    Just (ResponseInputItems items) -> items
    _ -> []
