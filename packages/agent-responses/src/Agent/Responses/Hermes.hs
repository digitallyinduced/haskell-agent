module Agent.Responses.Hermes
    ( TextDeltaFields(..)
    , textDeltaEventDecoder
    ) where

import Agent.Json
    ( Extensions
    , RawJson
    , emptyExtensions
    , deleteExtension
    , insertExtension
    )
import Agent.Json.Decoder.Backend
    ( unsafeRawJsonFromValidatedBytes
    )
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import qualified Data.Hermes as Hermes
import Data.Text (Text)
import Data.Scientific (toBoundedInteger)

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
                optional key integralInt
                    (\value -> state { sequenceNumber = value })
                    state
            "item_id" ->
                optional key Hermes.text
                    (\value -> state { itemId = value })
                    state
            "output_index" ->
                optional key integralInt
                    (\value -> state { outputIndex = value })
                    state
            "content_index" ->
                optional key integralInt
                    (\value -> state { contentIndex = value })
                    state
            "delta" ->
                optional key Hermes.text
                    (\value -> state { delta = value })
                    state
            "logprobs" ->
                if expectedType == "response.output_text.delta"
                    then optional key rawJsonValue
                        (\value -> state { logprobs = value })
                        state
                    else captureExtension key state
            "type" ->
                (\value -> state { wireType = value })
                    <$> Hermes.nullable Hermes.text
            _ ->
                captureExtension key state
    case fields.wireType of
        Just actual
            | actual /= expectedType ->
                fail
                    ( "SSE event type " <> show expectedType
                    <> " disagrees with JSON type " <> show actual
                    )
        _ -> pure fields
  where
    optional key decoder setValue _state = do
        value <- Hermes.nullable decoder
        pure $ case value of
            Just present ->
                (setValue (Just present))
                    { extensions =
                        deleteExtension
                            key
                            (setValue (Just present)).extensions
                    }
            Nothing ->
                (setValue Nothing)
                    { extensions =
                        insertExtension
                            key
                            rawNull
                            (setValue Nothing).extensions
                    }

    rawNull =
        unsafeRawJsonFromValidatedBytes (pack "null")

    integralInt = do
        value <- Hermes.scientific
        maybe
            (fail "expected an integral Int")
            pure
            (toBoundedInteger value)

    captureExtension key state =
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
