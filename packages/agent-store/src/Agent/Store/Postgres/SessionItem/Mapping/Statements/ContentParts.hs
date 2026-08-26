module Agent.Store.Postgres.SessionItem.Mapping.Statements.ContentParts
    ( ContentPartRow (..)
    , loadContentPartsStatement
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)

import Agent.Store.Postgres.Hasql (mkStatement)

data ContentPartRow = ContentPartRow
    { contentPartRowType :: !Text
    , contentPartRowText :: !(Maybe Text)
    , contentPartRowRefusal :: !(Maybe Text)
    , contentPartRowDetail :: !(Maybe Text)
    , contentPartRowFileData :: !(Maybe Text)
    , contentPartRowFileId :: !(Maybe Text)
    , contentPartRowFileUrl :: !(Maybe Text)
    , contentPartRowFilename :: !(Maybe Text)
    , contentPartRowImageUrl :: !(Maybe Text)
    , contentPartRowFileDataMimeType :: !(Maybe Text)
    , contentPartRowFileDataBytes :: !(Maybe ByteString)
    , contentPartRowImageMimeType :: !(Maybe Text)
    , contentPartRowImageBytes :: !(Maybe ByteString)
    , contentPartRowInputAudio :: !(Maybe Text)
    , contentPartRowPromptCacheBreakpoint :: !(Maybe Text)
    , contentPartRowAnnotations :: !(Maybe Text)
    , contentPartRowLogprobs :: !(Maybe Text)
    , contentPartRowExtraFields :: !Text
    }

loadContentPartsStatement :: Statement Text [ContentPartRow]
loadContentPartsStatement = mkStatement
    "SELECT part_type, text_value, refusal_text, detail, file_data, file_id,\
    \ file_url, filename, image_url, file_data_mime_type, file_data_bytes,\
    \ image_mime_type, image_bytes, input_audio_text,\
    \ prompt_cache_breakpoint_text, annotations_text, logprobs_text,\
    \ extra_fields_text\
    \ FROM harness.session_response_content_parts\
    \ WHERE response_item_id = $1::uuid\
    \ ORDER BY part_index"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowList $
        ContentPartRow
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.bytea)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.bytea)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text))
    True
