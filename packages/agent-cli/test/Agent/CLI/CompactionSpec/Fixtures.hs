-- | Response fixtures shared by manual, automatic, and task-plan compaction.
module Agent.CLI.CompactionSpec.Fixtures
    ( responseItemHasTaskPlan
    , taskPlanMessage
    , responseMessageHasText
    , responseMessageTexts
    , successful
    , requestItems
    , remoteCompactionResponse
    , responseWithoutCompaction
    , responseWithOutput
    , decodeResponseFixture
    , compactionUsage
    , withModel
    , summaryResponse
    , summaryResponseWithStatus
    , testRequestState
    ) where

import Agent.CLI.Session.Request (SessionRequestState, newSessionRequestState)
import Agent.CLI.Session (Persistence(..))
import Agent.Error (ApiError)
import Agent.Json.Decode qualified as Hermes
import Agent.Loop
import Agent.Responses.Types
import Agent.Tools.TaskPlan (isTaskPlanContextText)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text

responseItemHasTaskPlan :: ResponseItem -> Bool
responseItemHasTaskPlan = \case
    MessageItem message ->
        any isTaskPlanContextText (responseMessageTexts message)
    _ -> False

taskPlanMessage :: ResponseRole -> Text -> ResponseItem
taskPlanMessage role text =
    MessageItem ResponseMessage
        { messageId = Nothing
        , content = MessageContentParts [InputTextPart text Nothing]
        , role = role
        , status = Nothing
        , phase = Nothing
        , passthrough = Nothing
        }

responseMessageHasText :: Text -> ResponseMessage -> Bool
responseMessageHasText expected =
    elem expected . responseMessageTexts

responseMessageTexts :: ResponseMessage -> [Text]
responseMessageTexts message =
    case message.content of
        MessageContentText text -> [text]
        MessageContentParts parts ->
            [ text
            | part <- parts
            , text <- case part of
                InputTextPart{text} -> [text]
                OutputTextPart{text} -> [text]
                PlainTextPart{text} -> [text]
                _ -> []
            ]

successful
    :: BackendSnapshot
    -> TurnOutput
    -> Either ApiError BackendResult
successful state output =
    Right BackendResult
        { backendOutput = output
        , backendState = state
        }

requestItems :: ResponseCreateParams -> [ResponseItem]
requestItems request = case request.input of
    Just (ResponseInputItems items) -> items
    _ -> []

remoteCompactionResponse :: Response
remoteCompactionResponse =
    responseWithOutput
        [ Aeson.object
            [ "type" .= ("compaction" :: Text)
            , "encrypted_content" .= ("opaque" :: Text)
            ]
        ]

responseWithoutCompaction :: Response
responseWithoutCompaction =
    responseWithOutput []

responseWithOutput :: [Aeson.Value] -> Response
responseWithOutput output =
    decodeResponseFixture $ Aeson.object
        [ "id" .= ("resp-compact" :: Text)
        , "created_at" .= (0 :: Int)
        , "status" .= ("completed" :: Text)
        , "model" .= ("gpt-test" :: Text)
        , "output" .= output
        , "usage" .= Aeson.object
            [ "input_tokens" .= compactionUsage.inputTokens
            , "output_tokens" .= compactionUsage.outputTokens
            , "total_tokens" .=
                (compactionUsage.inputTokens + compactionUsage.outputTokens)
            , "input_tokens_details" .= Aeson.object
                [ "cached_tokens" .= compactionUsage.cachedTokens
                ]
            ]
        ]

decodeResponseFixture :: Aeson.Value -> Response
decodeResponseFixture fixture =
    case Hermes.decodeEither responseDecoder
            (LBS.toStrict (Aeson.encode fixture)) of
        Right response -> response
        Left err -> error (Text.unpack (Hermes.jsonErrorMessage err))

compactionUsage :: TokenUsage
compactionUsage = TokenUsage
    { inputTokens = 80
    , outputTokens = 6
    , cachedTokens = 40
    }

withModel :: Maybe Text -> ResponseCreateParams -> ResponseCreateParams
withModel nextModel ResponseCreateParams { model = _, .. } =
    ResponseCreateParams { model = nextModel, .. }

summaryResponse :: Text -> Response
summaryResponse summary =
    summaryResponseWithStatus "completed" summary

summaryResponseWithStatus :: Text -> Text -> Response
summaryResponseWithStatus responseStatus summary =
    decodeResponseFixture $ Aeson.object
        [ "id" .= ("resp-summary" :: Text)
        , "created_at" .= (0 :: Int)
        , "status" .= responseStatus
        , "model" .= ("gpt-test" :: Text)
        , "output" .=
            [ Aeson.object
                [ "type" .= ("message" :: Text)
                , "role" .= ("assistant" :: Text)
                , "content" .=
                    [ Aeson.object
                        [ "type" .= ("output_text" :: Text)
                        , "text" .= summary
                        ]
                    ]
                ]
            ]
        ]

-- Compaction now receives the same validated request state as live sessions.
testRequestState :: ResponseCreateParams -> IO SessionRequestState
testRequestState params =
    newSessionRequestState PersistenceDisabled
        (case params.model of
            Nothing -> params { model = Just "gpt-5.6-sol" }
            Just _ -> params)
        >>= either (fail . Text.unpack) pure
