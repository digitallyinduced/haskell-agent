module Agent.Responses.Hermes
    ( TextDeltaFields(..)
    , textDeltaEventDecoder
    ) where

import Agent.Json
    ( Extensions
    , RawJson
    , emptyExtensions
    , insertExtension
    )
import Agent.Json.Decoder.Backend
    ( unsafeRawJsonFromValidatedBytes
    )
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import qualified Data.Hermes as Hermes
import Data.Text (Text)

data TextDeltaFields = TextDeltaFields
    { wireType :: !(Maybe Text)
    , sequenceNumber :: !(Maybe Int)
    , itemId :: !(Maybe Text)
    , outputIndex :: !(Maybe Int)
    , contentIndex :: !(Maybe Int)
    , delta :: !(Maybe Text)
    , logprobs :: !(Maybe RawJson)
    , extensions :: !Extensions
    }

textDeltaEventDecoder :: Text -> Hermes.Decoder TextDeltaFields
textDeltaEventDecoder expectedType = do
    fields <- Hermes.objectFold
        (TextDeltaFields
            Nothing Nothing Nothing Nothing Nothing Nothing Nothing
            emptyExtensions)
        \key state -> case key of
            "sequence_number" ->
                optional key Hermes.int
                    (\value -> state { sequenceNumber = Just value })
                    state
            "item_id" ->
                optional key Hermes.text
                    (\value -> state { itemId = Just value })
                    state
            "output_index" ->
                optional key Hermes.int
                    (\value -> state { outputIndex = Just value })
                    state
            "content_index" ->
                optional key Hermes.int
                    (\value -> state { contentIndex = Just value })
                    state
            "delta" ->
                optional key Hermes.text
                    (\value -> state { delta = Just value })
                    state
            "logprobs" ->
                optional key rawJsonValue
                    (\value -> state { logprobs = Just value })
                    state
            "type" ->
                (\value -> state { wireType = value })
                    <$> Hermes.nullable Hermes.text
            _ ->
                (\value ->
                    state
                        { extensions =
                            insertExtension key value state.extensions
                        })
                    <$> rawJsonValue
    case fields.wireType of
        Just actual
            | actual /= expectedType ->
                fail
                    ( "SSE event type " <> show expectedType
                    <> " disagrees with JSON type " <> show actual
                    )
        _ -> pure fields
  where
    optional key decoder setValue state = do
        value <- Hermes.nullable decoder
        pure $ case value of
            Just present -> setValue present
            Nothing ->
                state
                    { extensions =
                        insertExtension
                            key
                            rawNull
                            state.extensions
                    }

    rawNull =
        unsafeRawJsonFromValidatedBytes (pack "null")

rawJsonValue :: Hermes.Decoder RawJson
rawJsonValue =
    Hermes.withRawJsonByteString \bytes ->
        pure (unsafeRawJsonFromValidatedBytes (BS.copy bytes))
