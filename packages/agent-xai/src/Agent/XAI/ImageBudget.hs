-- | Request-body budget for inline images on xAI Responses calls.
--
-- The xAI inference proxy rejects bodies larger than 50 MiB. Token estimates
-- charge each image a small flat cost, so they do not observe this limit.
-- Eviction rewrites only the provider request. Stored history is unchanged.
module Agent.XAI.ImageBudget
    ( imageBudgetLimits
    , imageBudgetHeadroomBytes
    , defaultMaxRequestBytes
    , minimumMaxRequestBytes
    , applyImageBudget
    , applyImageBudgetWithLimits
    , omitInlineImages
    , requestJsonBytes
    , imageBudgetPlaceholder
    , toolImageBudgetNote
    , imageStripPlaceholder
    ) where

import Agent.Json (RawJson, rawJsonBytes, rawJsonFromEncoding)
import Agent.Responses.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector

-- | Hard request-body ceiling enforced by the xAI inference proxy.
defaultMaxRequestBytes :: Int
defaultMaxRequestBytes = 50 * 1024 * 1024

-- | Uncounted tool definitions and the request envelope sit outside the
-- conversation measurement grok-build uses. The same 3 MiB headroom is
-- reserved under an explicit cap.
imageBudgetHeadroomBytes :: Int
imageBudgetHeadroomBytes = 3 * 1024 * 1024

-- | Smaller caps would put the reclaim mark on the trigger. Grok Build raises
-- them to four headrooms so one eviction stays below the trigger.
minimumMaxRequestBytes :: Int
minimumMaxRequestBytes = 4 * imageBudgetHeadroomBytes

-- | Replaces an inline image evicted to keep the request under the proxy cap.
imageBudgetPlaceholder :: Text
imageBudgetPlaceholder =
    "[An earlier image was removed to keep the request within its size limit and is no longer visible. Do not describe or reason about its contents from memory; ask the user to re-share it if you need to see it again.]"

-- | Appended to a tool result when one of its images is evicted.
toolImageBudgetNote :: Text
toolImageBudgetNote =
    "[One or more images from this tool result were removed to keep the request within its size limit and are no longer visible. Do not describe or reason about their contents from memory.]"

-- | Replaces an image removed after the server rejected the request body.
imageStripPlaceholder :: Text
imageStripPlaceholder =
    "[image removed — the server could not process it; its contents are unavailable. Ask the user to re-attach the image if it is still needed.]"

-- | Trigger and reclaim target for a provider request-body cap.
-- 'Nothing' uses the 50 MiB proxy default. The trigger is one headroom under
-- the cap; eviction then reclaims to half the cap so a later turn does not
-- cross the trigger again.
imageBudgetLimits :: Maybe Int -> (Int, Int)
imageBudgetLimits maxRequestBytes =
    let cap =
            max minimumMaxRequestBytes $
                fromMaybe defaultMaxRequestBytes maxRequestBytes
    in (cap - imageBudgetHeadroomBytes, cap `div` 2)

-- | Drop oldest inline images once the encoded request reaches the trigger.
applyImageBudget :: Maybe Int -> ResponseCreateParams -> ResponseCreateParams
applyImageBudget maxRequestBytes request =
    let (trigger, reclaim) = imageBudgetLimits maxRequestBytes
    in applyImageBudgetWithLimits trigger reclaim request

-- | Evict oldest inline images while the encoded body is above @reclaim@,
-- but only after it has reached @trigger@. Below the trigger every image stays.
applyImageBudgetWithLimits
    :: Int
    -> Int
    -> ResponseCreateParams
    -> ResponseCreateParams
applyImageBudgetWithLimits trigger reclaim request
    | bytes < trigger = request
    | bytes <= reclaim = request
    | otherwise = evict request bytes
  where
    bytes = requestJsonBytes request

    evict current currentBytes
        | currentBytes <= reclaim = current
        | otherwise =
            case omitOldestInlineImage current of
                Nothing -> current
                Just (next, removed, replacement) ->
                    evict next
                        ( currentBytes
                            - jsonBytes removed
                            + jsonBytes replacement
                        )

-- | Remove every inline image from a request. 'Nothing' when it had none, so
-- a payload rejection with no images is not retried.
omitInlineImages :: ResponseCreateParams -> Maybe ResponseCreateParams
omitInlineImages request =
    case request.input of
        Just (ResponseInputItems items) ->
            case traverseEdited editEveryItem items of
                Nothing -> Nothing
                Just items' -> Just (replaceInput request items')
        _ -> Nothing

-- | Encoded JSON size of a request, in bytes. This is the provider body,
-- not the token estimate.
requestJsonBytes :: ResponseCreateParams -> Int
requestJsonBytes = jsonBytes

jsonBytes :: Aeson.ToJSON a => a -> Int
jsonBytes = fromIntegral . LBS.length . Aeson.encode

replaceInput :: ResponseCreateParams -> [ResponseItem] -> ResponseCreateParams
replaceInput ResponseCreateParams {..} items =
    ResponseCreateParams
        { input = Just (ResponseInputItems items)
        , ..
        }

data ImagePass
    = OldestBudget
    | StripEvery
    deriving (Eq)

omitOldestInlineImage
    :: ResponseCreateParams
    -> Maybe (ResponseCreateParams, ResponseItem, ResponseItem)
omitOldestInlineImage request =
    case request.input of
        Just (ResponseInputItems items) ->
            case editFirst OldestBudget items of
                Nothing -> Nothing
                Just (items', removed, replacement) ->
                    Just (replaceInput request items', removed, replacement)
        _ -> Nothing

editFirst
    :: ImagePass
    -> [ResponseItem]
    -> Maybe ([ResponseItem], ResponseItem, ResponseItem)
editFirst pass = go
  where
    go [] = Nothing
    go (item : rest) =
        case editItem pass item of
            Just item' -> Just (item' : rest, item, item')
            Nothing -> do
                (rest', removed, replacement) <- go rest
                pure (item : rest', removed, replacement)

traverseEdited
    :: (ResponseItem -> Maybe ResponseItem)
    -> [ResponseItem]
    -> Maybe [ResponseItem]
traverseEdited edit items
    | not (any isJust edited) = Nothing
    | otherwise = Just (zipWith fromMaybe items edited)
  where
    edited = map edit items

editEveryItem :: ResponseItem -> Maybe ResponseItem
editEveryItem = editItem StripEvery

editItem :: ImagePass -> ResponseItem -> Maybe ResponseItem
editItem pass = \case
    MessageItem ResponseMessage { content = MessageContentText _ } ->
        Nothing
    MessageItem ResponseMessage { content = MessageContentParts parts, .. } ->
        fmap
            (\parts' ->
                MessageItem ResponseMessage
                    { content = MessageContentParts parts'
                    , ..
                    })
            (editParts pass parts)
    AgentMessageItem ResponseAgentMessage { content = parts, .. } ->
        fmap
            (\parts' ->
                AgentMessageItem ResponseAgentMessage
                    { content = parts'
                    , ..
                    })
            (editParts pass parts)
    ReasoningItemValue ReasoningItem { content = Nothing } ->
        Nothing
    ReasoningItemValue ReasoningItem { content = Just parts, .. } ->
        fmap
            (\parts' ->
                ReasoningItemValue ReasoningItem
                    { content = Just parts'
                    , ..
                    })
            (editParts pass parts)
    FunctionCallOutputItem FunctionCallOutput { output = current, .. } ->
        fmap
            (\edited ->
                FunctionCallOutputItem FunctionCallOutput
                    { output = edited
                    , ..
                    })
            (editToolOutput pass current)
    CustomToolCallOutputItem CustomToolCallOutput { output = current, .. } ->
        fmap
            (\edited ->
                CustomToolCallOutputItem CustomToolCallOutput
                    { output = edited
                    , ..
                    })
            (editToolOutput pass current)
    ComputerCallOutputItem ComputerCallOutput { screenshotDataUrl = url, .. }
        | isInlineImagePayload url ->
            Just $ ComputerCallOutputItem ComputerCallOutput
                { screenshotDataUrl = partPlaceholder pass
                , ..
                }
        | otherwise -> Nothing
    FunctionCallItem _ -> Nothing
    CustomToolCallItem _ -> Nothing
    ComputerCallItem _ -> Nothing
    ItemReferenceValue _ -> Nothing
    AdditionalToolsItemValue _ -> Nothing
    LocalShellCallItem _ -> Nothing
    ToolSearchCallItem _ -> Nothing
    ToolSearchOutputItem _ -> Nothing
    WebSearchCallItem _ -> Nothing
    ImageGenerationCallItem _ -> Nothing
    CompactionItemValue _ -> Nothing
    CompactionTriggerItemValue _ -> Nothing
    ContextCompactionItemValue _ -> Nothing
    KnownResponseItem _ _ -> Nothing
    UnknownResponseItem _ -> Nothing

editParts
    :: ImagePass
    -> [ResponseContentPart]
    -> Maybe [ResponseContentPart]
editParts OldestBudget parts = replaceFirst parts
editParts StripEvery parts
    | any isImagePart parts =
        Just (map (replaceImage imageStripPlaceholder) parts)
    | otherwise = Nothing

replaceFirst :: [ResponseContentPart] -> Maybe [ResponseContentPart]
replaceFirst [] = Nothing
replaceFirst (part : rest)
    | isImagePart part =
        Just (replaceImage imageBudgetPlaceholder part : rest)
    | otherwise = (part :) <$> replaceFirst rest

isImagePart :: ResponseContentPart -> Bool
isImagePart = \case
    InputImagePart { imageUrl } ->
        maybe False isInlineImagePayload imageUrl
    _ -> False

replaceImage :: Text -> ResponseContentPart -> ResponseContentPart
replaceImage placeholder part@InputImagePart{} =
    InputTextPart
        { text = placeholder
        , promptCacheBreakpoint = part.promptCacheBreakpoint
        }
replaceImage _ part = part

partPlaceholder :: ImagePass -> Text
partPlaceholder = \case
    OldestBudget -> imageBudgetPlaceholder
    StripEvery -> imageStripPlaceholder

-- | Computer screenshots and data URLs are the inline payloads that dominate
-- the body. A short replacement must not be selected again.
isInlineImagePayload :: Text -> Bool
isInlineImagePayload url =
    Text.isPrefixOf "data:" (Text.toLower url)
        || Text.isInfixOf "base64," url
        || Text.length url > 2048

editToolOutput :: ImagePass -> RawJson -> Maybe RawJson
editToolOutput pass raw =
    case Aeson.eitherDecodeStrict' (rawJsonBytes raw) of
        Left _ -> Nothing
        Right value ->
            fmap
                (rawJsonFromEncoding . Aeson.toEncoding . finish pass)
                (dropImages pass value)

finish :: ImagePass -> Aeson.Value -> Aeson.Value
finish OldestBudget = ensureToolNote
finish StripEvery = ensureStripText

dropImages :: ImagePass -> Aeson.Value -> Maybe Aeson.Value
dropImages pass = \case
    Aeson.Array values ->
        fmap Aeson.Array (dropInVector pass values)
    Aeson.Object object
        | isImageObject (Aeson.Object object) ->
            Just (textPart (partPlaceholder pass))
        | otherwise ->
            fmap Aeson.Object (editObject pass (KeyMap.toList object))
    _ -> Nothing

editObject
    :: ImagePass
    -> [(Aeson.Key, Aeson.Value)]
    -> Maybe Aeson.Object
editObject StripEvery fields
    | any changed fields =
        Just (KeyMap.fromList (map edited fields))
    | otherwise = Nothing
  where
    changed (_, value) = isJust (dropImages StripEvery value)
    edited (key, value) =
        (key, fromMaybe value (dropImages StripEvery value))
editObject OldestBudget fields = go [] fields
  where
    go _ [] = Nothing
    go seen ((key, value) : rest) =
        case dropImages OldestBudget value of
            Just value' ->
                Just (KeyMap.fromList (reverse seen <> ((key, value') : rest)))
            Nothing -> go ((key, value) : seen) rest

dropInVector
    :: ImagePass
    -> Vector.Vector Aeson.Value
    -> Maybe (Vector.Vector Aeson.Value)
dropInVector pass values =
    case pass of
        OldestBudget -> dropFirst values
        StripEvery
            | Vector.any isImageObject values ->
                Just (Vector.filter (not . isImageObject) values)
            | otherwise -> Nothing

dropFirst
    :: Vector.Vector Aeson.Value
    -> Maybe (Vector.Vector Aeson.Value)
dropFirst values =
    case Vector.findIndex isImageObject values of
        Just index -> Just (Vector.ifilter (\i _ -> i /= index) values)
        Nothing -> Nothing

isImageObject :: Aeson.Value -> Bool
isImageObject = \case
    Aeson.Object object ->
        case KeyMap.lookup "type" object of
            Just (Aeson.String kind) ->
                kind == "input_image" || kind == "computer_screenshot"
            _ -> False
    _ -> False

ensureToolNote :: Aeson.Value -> Aeson.Value
ensureToolNote = \case
    Aeson.Array values
        | vectorHasNote values -> Aeson.Array values
        | otherwise -> Aeson.Array (appendNote values)
    value -> value
  where
    appendNote values =
        case Vector.findIndex isInputText values of
            Just index ->
                Vector.imap
                    (\i value ->
                        if i == index then appendToText value else value)
                    values
            Nothing ->
                values `Vector.snoc` textPart toolImageBudgetNote

    appendToText = \case
        Aeson.Object object ->
            case KeyMap.lookup "text" object of
                Just (Aeson.String text) ->
                    Aeson.Object $
                        KeyMap.insert
                            "text"
                            (Aeson.String (text <> "\n\n" <> toolImageBudgetNote))
                            object
                _ -> Aeson.Object object
        value -> value

ensureStripText :: Aeson.Value -> Aeson.Value
ensureStripText = \case
    Aeson.Array values
        | Vector.null values ->
            Aeson.Array (Vector.singleton (textPart imageStripPlaceholder))
        | otherwise -> Aeson.Array values
    value -> value

vectorHasNote :: Vector.Vector Aeson.Value -> Bool
vectorHasNote =
    Vector.any \case
        Aeson.Object object ->
            case KeyMap.lookup "text" object of
                Just (Aeson.String text) ->
                    toolImageBudgetNote `Text.isInfixOf` text
                _ -> False
        _ -> False

isInputText :: Aeson.Value -> Bool
isInputText = \case
    Aeson.Object object ->
        KeyMap.lookup "type" object == Just (Aeson.String "input_text")
    _ -> False

textPart :: Text -> Aeson.Value
textPart text =
    Aeson.object
        [ "type" Aeson..= ("input_text" :: Text)
        , "text" Aeson..= text
        ]
