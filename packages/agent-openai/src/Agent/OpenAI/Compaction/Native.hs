-- | Remote compaction for applications retaining their native Responses wire
-- representation. Unknown payloads are preserved, not decoded and re-encoded
-- through the narrower library item schema.
module Agent.OpenAI.Compaction.Native
    ( NativeRequestAdapter(..)
    , openAIRemoteCompactionStrategy
    , buildCompactionRequest
    , compactedHistory
    , estimateRequestTokens
    , responseOccupancy
    ) where

import Agent.OpenAI.Compaction.Manager
import qualified Agent.OpenAI.Compaction as Shared
import Agent.OpenAI.ModelMetadata (codexEffectiveContextWindowFor)
import qualified Agent.Responses.Types as Responses
import qualified Agent.Responses.Types.Items as Items
import qualified Agent.Json.Decode as Json
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.:?))
import qualified Data.ByteString.Lazy as LBS
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as Text

-- | Mechanical record access only; no application compaction policy.
data NativeRequestAdapter request item = NativeRequestAdapter
    { requestModel :: request -> Text
    , requestInput :: request -> [item]
    , withRequestInput :: request -> [item] -> request
    }

openAIRemoteCompactionStrategy
    :: (Aeson.ToJSON request, Aeson.ToJSON item, Aeson.FromJSON item, Aeson.ToJSON response)
    => NativeRequestAdapter request item
    -> CompactionStrategy request item response
openAIRemoteCompactionStrategy adapter = CompactionStrategy
    { estimateRequest = estimateRequestTokens
    , estimateItem = Shared.estimateEncodedValue
    , contextWindow = nativeContextWindow adapter
    , requestItems = adapter.requestInput
    , replaceItems = adapter.withRequestInput
    , compressHistory = \request history send ->
        checked (buildCompactionRequest adapter request history) >>= send
    , replacementHistory = \threshold request pending history response ->
        checked (compactedHistory adapter threshold request pending history response)
    }

estimateRequestTokens :: Aeson.ToJSON request => request -> Int
estimateRequestTokens = Shared.estimateEncodedValue

data ResponseUsage = ResponseUsage Int Int
instance Aeson.FromJSON ResponseUsage where
    parseJSON = Aeson.withObject "ResponseUsage" \object ->
        ResponseUsage <$> object .: "input_tokens" <*> object .: "output_tokens"

newtype UsageEnvelope = UsageEnvelope (Maybe ResponseUsage)
instance Aeson.FromJSON UsageEnvelope where
    parseJSON = Aeson.withObject "UsageEnvelope" \object ->
        UsageEnvelope <$> object .:? "usage"

responseOccupancy :: Aeson.ToJSON response => response -> Maybe Int
responseOccupancy response = case Aeson.fromJSON (Aeson.toJSON response) of
    Aeson.Success (UsageEnvelope (Just (ResponseUsage input output)))
        | input + output > 0 -> Just (input + output)
    _ -> Nothing

nativeContextWindow :: NativeRequestAdapter request item -> request -> Int
nativeContextWindow adapter = codexEffectiveContextWindowFor . Just . adapter.requestModel

buildCompactionRequest
    :: (Aeson.ToJSON request, Aeson.ToJSON item, Aeson.FromJSON item)
    => NativeRequestAdapter request item -> request -> [item] -> Either Text request
buildCompactionRequest adapter request history = do
    params <- decodeLibrary Responses.responseCreateParamsDecoder $
        adapter.withRequestInput request []
    let triggerRequest = Shared.buildRemoteCompactionRequest params []
    trigger <- case triggerRequest.input of
        Just (Responses.ResponseInputItems items) -> traverse nativeItem items
        _ -> Left "Compaction: library did not build a compaction trigger"
    let nativeRequest = adapter.withRequestInput request (history <> trigger)
    if estimateRequestTokens nativeRequest <= nativeContextWindow adapter request
        then Right nativeRequest
        else do
            represented <- traverse losslessLibraryItem history
            trimmed <- traverse nativeItem $
                Shared.trimRemoteCompactionRequestToFit (nativeContextWindow adapter request) params represented
            let trimmedRequest = adapter.withRequestInput request (trimmed <> trigger)
            if estimateRequestTokens trimmedRequest <= nativeContextWindow adapter request
                then Right trimmedRequest
                else Left "Compaction: compaction request exceeds the model context window"

compactedHistory
    :: (Aeson.ToJSON request, Aeson.ToJSON item, Aeson.FromJSON item, Aeson.ToJSON response)
    => NativeRequestAdapter request item -> Int -> request -> [item] -> [item]
    -> response -> Either Text [item]
compactedHistory adapter threshold request pending history response = do
    libraryResponse <- decodeLibrary Responses.responseDecoder response
    checkpoint <- Shared.extractRemoteCompactionItem libraryResponse
    checkpointNative <- nativeItem checkpoint
    retained <- traverse (decodeLibrary Items.responseItemDecoder) (filter isUserMessage history)
    let fixed = estimateRequestTokens (adapter.withRequestInput request ([checkpointNative] <> pending))
        reserve = max 1024 (threshold `div` 10)
        retainedBudget = max 0 $ minimum
            [ Shared.remoteCompactionRetainedTokenBudget
            , threshold - reserve - fixed
            , nativeContextWindow adapter request - reserve - fixed
            ]
    replacement <- traverse nativeItem $
        Shared.buildRemoteCompactedHistory retainedBudget retained checkpoint
    let replacementRequest = adapter.withRequestInput request (replacement <> pending)
        checkpointRequest = adapter.withRequestInput request [checkpointNative]
    if estimateRequestTokens replacementRequest > nativeContextWindow adapter request
        then Left "Compaction: checkpoint and current input exceed the model context window"
        else if estimateRequestTokens checkpointRequest >= threshold
            then Left "Compaction: checkpoint did not free enough context"
            else Right replacement

data ItemRole = ItemRole Text (Maybe Text)
instance Aeson.FromJSON ItemRole where
    parseJSON = Aeson.withObject "ItemRole" \object ->
        ItemRole <$> object .: "type" <*> object .:? "role"

isUserMessage :: Aeson.ToJSON item => item -> Bool
isUserMessage item = case Aeson.fromJSON (Aeson.toJSON item) of
    Aeson.Success (ItemRole "message" (Just "user")) -> True
    _ -> False

losslessLibraryItem
    :: forall item. (Aeson.ToJSON item, Aeson.FromJSON item)
    => item -> Either Text Responses.ResponseItem
losslessLibraryItem item = do
    converted <- decodeLibrary Items.responseItemDecoder item
    restored <- nativeItem converted :: Either Text item
    if Aeson.toJSON restored == Aeson.toJSON item
        then Right converted
        else Left "Compaction: oversized history contains an item the library cannot trim losslessly"

nativeItem :: Aeson.FromJSON item => Responses.ResponseItem -> Either Text item
nativeItem item = case Aeson.fromJSON (Aeson.toJSON item) of
    Aeson.Success value -> Right value
    Aeson.Error message -> Left (Text.pack message)

decodeLibrary :: Aeson.ToJSON a => Json.Decoder b -> a -> Either Text b
decodeLibrary decoder = first (.jsonErrorMessage) . Json.decodeEither decoder . LBS.toStrict . Aeson.encode

checked :: Either Text a -> IO a
checked = either (fail . Text.unpack) pure
