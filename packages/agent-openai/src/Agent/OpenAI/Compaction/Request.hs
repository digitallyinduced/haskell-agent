-- | Remote compaction request construction and serialized-size estimation.
module Agent.OpenAI.Compaction.Request
    ( buildRemoteCompactionRequest
    , estimateRequestTokensWithItems
    , estimateResponseCreateParamsTokens
    , estimateEncodedValue
    , resizedImageBytesEstimate
    ) where

import Agent.OpenAI.ModelMetadata (isCodexResponsesLiteModel)
import Agent.Responses.LoopBackend (withRequestInput)
import Agent.Responses.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

buildRemoteCompactionRequest
    :: ResponseCreateParams
    -> [ResponseItem]
    -> ResponseCreateParams
buildRemoteCompactionRequest params history =
    case withRequestInput params (history <> [compactionTriggerItem]) of
        ResponseCreateParams{..} ->
            ResponseCreateParams
                { parallelToolCalls =
                    Just (not (maybe False isCodexResponsesLiteModel model))
                , previousResponseId = Nothing
                , store = Just False
                , stream = Just True
                , toolChoice = Just (ToolChoiceMode ToolChoiceAuto)
                , ..
                }

compactionTriggerItem :: ResponseItem
compactionTriggerItem =
    CompactionTriggerItemValue CompactionTriggerItem

estimateRequestTokensWithItems
    :: ResponseCreateParams
    -> [ResponseItem]
    -> Int
estimateRequestTokensWithItems params items =
    estimateEncodedValue (withRequestInput params items)

estimateResponseCreateParamsTokens :: ResponseCreateParams -> Int
estimateResponseCreateParamsTokens = estimateEncodedValue

estimateEncodedValue :: Aeson.ToJSON value => value -> Int
estimateEncodedValue value =
    estimateAdjustedJsonTokens (Aeson.toJSON value)

estimateAdjustedJsonTokens :: Aeson.Value -> Int
estimateAdjustedJsonTokens json =
    let encoded =
            TextEncoding.decodeUtf8 (LBS.toStrict (Aeson.encode json))
        (payloadBytes, replacementBytes) = mediaEstimateAdjustment json
        adjusted =
            max 0 (Text.length encoded - payloadBytes) + replacementBytes
    in max 1 (adjusted `div` 4)

resizedImageBytesEstimate :: Int
resizedImageBytesEstimate = 7_373

mediaEstimateAdjustment :: Aeson.Value -> (Int, Int)
mediaEstimateAdjustment = \case
    Aeson.Array values ->
        foldl'
            (\acc value -> addPair acc (mediaEstimateAdjustment value))
            (0, 0)
            values
    Aeson.Object fields ->
        let isInputImage =
                KeyMap.lookup "type" fields == Just (Aeson.String "input_image")
        in foldl'
            (\acc (key, value) ->
                addPair acc (fieldAdjustment isInputImage key value))
            (0, 0)
            (KeyMap.toList fields)
    _ ->
        (0, 0)
  where
    addPair (payloadAcc, replacementAcc) (payload, replacement) =
        (payloadAcc + payload, replacementAcc + replacement)

    fieldAdjustment isInputImage key value
        | isInputImage && key == "image_url" =
            imageUrlAdjustment value
        | otherwise =
            mediaEstimateAdjustment value

imageUrlAdjustment :: Aeson.Value -> (Int, Int)
imageUrlAdjustment = \case
    Aeson.String text ->
        case parseBase64ImageDataUrl text of
            Just payload ->
                (Text.length payload, resizedImageBytesEstimate)
            Nothing ->
                (0, 0)
    _ ->
        (0, 0)

parseBase64ImageDataUrl :: Text -> Maybe Text
parseBase64ImageDataUrl url
    | not (hasInsensitivePrefix "data:" url) = Nothing
    | otherwise =
        case Text.break (== ',') (Text.drop 5 url) of
            (_, payload)
                | Text.null payload -> Nothing
            (metadata, payload) ->
                let parts = Text.splitOn ";" metadata
                    mime = case parts of
                        (value : _) -> value
                        [] -> Text.empty
                    hasBase64 =
                        any (\part -> Text.toLower part == "base64") parts
                in if hasBase64 && hasInsensitivePrefix "image/" mime
                    then Just (Text.drop 1 payload)
                    else Nothing

hasInsensitivePrefix :: Text -> Text -> Bool
hasInsensitivePrefix prefix text =
    Text.toLower (Text.take (Text.length prefix) text) == Text.toLower prefix
