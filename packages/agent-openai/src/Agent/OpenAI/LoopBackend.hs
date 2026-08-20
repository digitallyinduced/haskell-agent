-- | Map the provider-neutral loop onto the OpenAI Responses WebSocket transport.
--
-- Shared helpers convert 'TurnInput' / 'Response' / stream events so the xAI
-- adapter can reuse the same function_call and custom_tool_call encoding.
module Agent.OpenAI.LoopBackend
    ( openAiBackend
    , openAiBackendWith
    , turnInputsToItems
    , responseToTurnOutput
    , streamEventToLoopEvent
    , assistantTextFromResponse
    , withRequestInput
    ) where

import Agent.Error (ApiError)
import Agent.OpenAI.Error (isPreviousResponseIdError)
import Agent.Loop
    ( Backend(..)
    , ImageAttachment(..)
    , LoopEvent(..)
    , TurnInput(..)
    , TurnOutput(..)
    )
import Agent.OpenAI.Responses.Types
import Agent.OpenAI.WebSocketClient
    ( CodexConn
    , sendWsRequestWithEvents
    )
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , ToolCallResult(..)
    )
import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString (ByteString)
import qualified "base64-bytestring" Data.ByteString.Base64 as Base64
import Data.IORef
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

-- | Close over a live Codex WebSocket and the request fields the loop does
-- not own (model, instructions, tools, reasoning). The params action is
-- re-run each turn so the REPL can change reasoning effort in place.
--
-- The wire protocol still sends only new items plus @previous_response_id@.
-- The shared 'IORef' mirrors the full transcript locally so sessions can be
-- persisted and resumed when the server-side chain is gone.
openAiBackend
    :: CodexConn
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> Backend
openAiBackend conn =
    openAiBackendWith \request previousResponseId onEvent ->
        sendWsRequestWithEvents conn request previousResponseId onEvent

-- | Same mapping as 'openAiBackend', with an injectable transport for tests.
openAiBackendWith
    :: (ResponseCreateParams
        -> Maybe Text
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> Backend
openAiBackendWith send getParams transcript = Backend \previousResponseId inputs onEvent -> do
    baseParams <- getParams
    history <- readIORef transcript
    let newItems = turnInputsToItems inputs
        deltaRequest = withRequestInput baseParams newItems
        fullRequest = withRequestInput baseParams (history <> newItems)
        emit event = mapM_ onEvent (streamEventToLoopEvent event)
    result <- send deltaRequest previousResponseId emit
    recovered <- case result of
        Left err
            | isPreviousResponseIdError err && not (null history) ->
                -- Server forgot the chain; replay the local transcript as a
                -- fresh turn so resumed sessions keep working.
                send fullRequest Nothing emit
            | otherwise -> pure (Left err)
        Right response -> pure (Right response)
    case recovered of
        Left err -> pure (Left err)
        Right response -> do
            writeIORef transcript (history <> newItems <> response.output)
            pure (Right (responseToTurnOutput response))

-- | 'input' is also a field on 'CustomToolCall', so a record update is
-- ambiguous. Rebuild from the constructor instead.
withRequestInput :: ResponseCreateParams -> [ResponseItem] -> ResponseCreateParams
withRequestInput ResponseCreateParams{..} items =
    ResponseCreateParams { input = Just (ResponseInputItems items), .. }

turnInputsToItems :: [TurnInput] -> [ResponseItem]
turnInputsToItems = map turnInputToItem

turnInputToItem :: TurnInput -> ResponseItem
turnInputToItem = \case
    UserMessage text -> userMessageItem text
    UserMultimodal{userText, userImages} -> multimodalUserItem userText userImages
    CompletedTool result -> toolResultToItem result

userMessageItem :: Text -> ResponseItem
userMessageItem text = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentParts [InputTextPart text Nothing KeyMap.empty]
    , role = RoleUser
    , status = Nothing
    , phase = Nothing
    , extraFields = KeyMap.empty
    }

multimodalUserItem :: Text -> [ImageAttachment] -> ResponseItem
multimodalUserItem text images = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentParts
        ( InputTextPart text Nothing KeyMap.empty
        : map imageAttachmentPart images
        )
    , role = RoleUser
    , status = Nothing
    , phase = Nothing
    , extraFields = KeyMap.empty
    }

imageAttachmentPart :: ImageAttachment -> ResponseContentPart
imageAttachmentPart ImageAttachment{imageMime, imageBytes} =
    InputImagePart
        { detail = Just "auto"
        , fileId = Nothing
        , imageUrl = Just (imageDataUrl imageMime imageBytes)
        , promptCacheBreakpoint = Nothing
        , extraFields = KeyMap.empty
        }

imageDataUrl :: Text -> ByteString -> Text
imageDataUrl mime bytes =
    "data:" <> mime <> ";base64," <> Text.decodeUtf8 (Base64.encode bytes)

toolResultToItem :: ToolCallResult -> ResponseItem
toolResultToItem result = case result.callKind of
    FunctionCallKind -> FunctionCallOutputItem FunctionCallOutput
        { itemId = Nothing
        , callId = result.callId
        , output = Aeson.String result.output
        , status = Nothing
        , extraFields = KeyMap.empty
        }
    CustomCallKind -> CustomToolCallOutputItem CustomToolCallOutput
        { itemId = Nothing
        , callId = result.callId
        , name = Nothing
        , output = Aeson.String result.output
        , status = Nothing
        , extraFields = KeyMap.empty
        }

responseToTurnOutput :: Response -> TurnOutput
responseToTurnOutput response = TurnOutput
    { responseId = response.responseId
    , toolCalls = mapMaybe responseItemToToolCall response.output
    , assistantText = assistantTextFromResponse response
    }

responseItemToToolCall :: ResponseItem -> Maybe ToolCall
responseItemToToolCall = \case
    FunctionCallItem call -> Just ToolCall
        { callId = call.callId
        , name = call.name
        , arguments = call.arguments
        , callKind = FunctionCallKind
        }
    CustomToolCallItem call -> Just ToolCall
        { callId = call.callId
        , name = call.name
        , arguments = call.input
        , callKind = CustomCallKind
        }
    _ -> Nothing

-- | Concatenate assistant message text the same way the live functional tests do.
assistantTextFromResponse :: Response -> Maybe Text
assistantTextFromResponse response = case
    [ value
    | MessageItem message <- response.output
    , message.role == RoleAssistant
    , value <- case message.content of
        MessageContentText text -> [text]
        MessageContentParts parts -> [text | OutputTextPart { text } <- parts]
    ] of
        [] -> Nothing
        values -> Just (Text.intercalate "\n" values)

streamEventToLoopEvent :: ResponseStreamEvent -> Maybe LoopEvent
streamEventToLoopEvent = \case
    OtherResponseStreamEvent { otherEventType, eventExtraFields } ->
        case extraDeltaText eventExtraFields of
            Just text -> case otherEventType of
                EventOutputTextDelta -> Just (TextDelta text)
                EventReasoningTextDelta -> Just (ReasoningDelta text)
                EventReasoningSummaryTextDelta -> Just (ReasoningDelta text)
                _ -> Nothing
            Nothing -> Nothing
    _ -> Nothing

extraDeltaText :: Aeson.Object -> Maybe Text
extraDeltaText extras = nonEmptyText extras "delta" <|> nonEmptyText extras "text"

nonEmptyText :: Aeson.Object -> Text -> Maybe Text
nonEmptyText extras key = case KeyMap.lookup (Key.fromText key) extras of
    Just (Aeson.String text) | not (Text.null text) -> Just text
    _ -> Nothing
