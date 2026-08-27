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
import qualified Data.Hermes as Hermes
import Data.Text (Text)

data TextDeltaFields = TextDeltaFields
    { sequenceNumber :: !(Maybe Int)
    , itemId :: !(Maybe Text)
    , outputIndex :: !(Maybe Int)
    , contentIndex :: !(Maybe Int)
    , delta :: !(Maybe Text)
    , logprobs :: !(Maybe RawJson)
    , extensions :: !Extensions
    }

textDeltaEventDecoder :: Hermes.Decoder TextDeltaFields
textDeltaEventDecoder =
    Hermes.objectFold
        (TextDeltaFields
            Nothing Nothing Nothing Nothing Nothing Nothing
            emptyExtensions)
        \key state -> case key of
            "sequence_number" ->
                (\value -> state { sequenceNumber = Just value })
                    <$> Hermes.int
            "item_id" ->
                (\value -> state { itemId = Just value })
                    <$> Hermes.text
            "output_index" ->
                (\value -> state { outputIndex = Just value })
                    <$> Hermes.int
            "content_index" ->
                (\value -> state { contentIndex = Just value })
                    <$> Hermes.int
            "delta" ->
                (\value -> state { delta = Just value })
                    <$> Hermes.text
            "logprobs" ->
                (\value -> state { logprobs = Just value })
                    <$> rawJsonValue
            "type" -> state <$ Hermes.text
            _ ->
                (\value ->
                    state
                        { extensions =
                            insertExtension key value state.extensions
                        })
                    <$> rawJsonValue

rawJsonValue :: Hermes.Decoder RawJson
rawJsonValue =
    Hermes.withRawJsonByteString \bytes ->
        pure (unsafeRawJsonFromValidatedBytes (BS.copy bytes))
