-- | Remote compaction request construction and serialized-size estimation.
module Agent.OpenAI.Compaction.Request
    ( buildRemoteCompactionRequest
    , estimateRequestTokensWithItems
    , estimateResponseCreateParamsTokens
    , estimateEncodedValue
    , resizedImageBytesEstimate
    , pdfPageBytesEstimate
    ) where

import Agent.OpenAI.ModelMetadata (isCodexResponsesLiteModel)
import Agent.OpenAI.ToolCatalog.Request (isCatalogContextItem)
import Agent.Responses.LoopBackend (withRequestInput)
import Agent.Responses.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

buildRemoteCompactionRequest
    :: ResponseCreateParams
    -> [ResponseItem]
    -> ResponseCreateParams
buildRemoteCompactionRequest params history =
    -- Compaction starts a new context window: rebase onto the current complete
    -- template instead of replaying obsolete catalog declarations.
    case withRequestInput params (filter (not . isCatalogContextItem) history <> [compactionTriggerItem]) of
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

-- | OpenAI gives the model the extracted text and an image of every PDF
-- page, so a page costs about one resized image plus its text, however many
-- bytes its scans and fonts take.
pdfPageBytesEstimate :: Int
pdfPageBytesEstimate = resizedImageBytesEstimate + 2_000

mediaEstimateAdjustment :: Aeson.Value -> (Int, Int)
mediaEstimateAdjustment = \case
    Aeson.Array values ->
        foldl'
            (\acc value -> addPair acc (mediaEstimateAdjustment value))
            (0, 0)
            values
    Aeson.Object fields ->
        let partType = KeyMap.lookup "type" fields
        in foldl'
            (\acc (key, value) ->
                addPair acc (fieldAdjustment partType key value))
            (0, 0)
            (KeyMap.toList fields)
    _ ->
        (0, 0)
  where
    addPair (payloadAcc, replacementAcc) (payload, replacement) =
        (payloadAcc + payload, replacementAcc + replacement)

    fieldAdjustment partType key value
        | partType == Just (Aeson.String "input_image") && key == "image_url" =
            imageUrlAdjustment value
        | partType == Just (Aeson.String "input_file") && key == "file_data" =
            pdfFileDataAdjustment value
        | otherwise =
            mediaEstimateAdjustment value

imageUrlAdjustment :: Aeson.Value -> (Int, Int)
imageUrlAdjustment = \case
    Aeson.String text ->
        case parseBase64DataUrl ("image/" `Text.isPrefixOf`) text of
            Just payload ->
                (Text.length payload, resizedImageBytesEstimate)
            Nothing ->
                (0, 0)
    _ ->
        (0, 0)

pdfFileDataAdjustment :: Aeson.Value -> (Int, Int)
pdfFileDataAdjustment = \case
    Aeson.String text ->
        case parseBase64DataUrl (== "application/pdf") text of
            Just payload ->
                ( Text.length payload
                , pdfPageBytesEstimate
                    * pdfPageCountEstimate
                        (Base64.decodeLenient (TextEncoding.encodeUtf8 payload))
                )
            Nothing ->
                (0, 0)
    _ ->
        (0, 0)

-- | Count the page objects of a PDF (@/Type /Page@, but not @/Pages@ or
-- @/PageLabel@). Files that compress their objects into object streams show
-- none, so assume one page per 100 KiB for them.
pdfPageCountEstimate :: ByteString -> Int
pdfPageCountEstimate bytes =
    case countPageObjects 0 bytes of
        0 -> max 1 (Char8.length bytes `div` 102_400)
        pages -> pages
  where
    countPageObjects count remaining =
        case Char8.breakSubstring "/Type" remaining of
            (_, match)
                | Char8.null match -> count
                | otherwise ->
                    let afterKey = Char8.drop 5 match
                    in countPageObjects
                        (if isPageType afterKey then count + 1 else count)
                        afterKey

    isPageType afterKey =
        let value = Char8.dropWhile isPdfWhitespace afterKey
        in "/Page" `Char8.isPrefixOf` value
            && maybe True isPdfNameEnd (Char8.indexMaybe value 5)

    isPdfWhitespace = (`elem` ("\0\t\n\f\r " :: String))
    isPdfNameEnd char = isPdfWhitespace char || char `elem` ("()<>[]{}/%" :: String)

parseBase64DataUrl :: (Text -> Bool) -> Text -> Maybe Text
parseBase64DataUrl acceptsMime url
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
                in if hasBase64 && acceptsMime (Text.toLower mime)
                    then Just (Text.drop 1 payload)
                    else Nothing

hasInsensitivePrefix :: Text -> Text -> Bool
hasInsensitivePrefix prefix text =
    Text.toLower (Text.take (Text.length prefix) text) == Text.toLower prefix
