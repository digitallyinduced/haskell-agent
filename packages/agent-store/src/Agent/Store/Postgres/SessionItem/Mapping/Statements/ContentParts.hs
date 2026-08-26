module Agent.Store.Postgres.SessionItem.Mapping.Statements.ContentParts
    ( ContentPartRow (..)
    , loadContentPartsStatement
    , loadTurnContentPartsStatement
    ) where

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
    , contentPartRowInputAudio :: !(Maybe Text)
    , contentPartRowPromptCacheBreakpoint :: !(Maybe Text)
    , contentPartRowAnnotations :: !(Maybe Text)
    , contentPartRowLogprobs :: !(Maybe Text)
    , contentPartRowExtraFields :: !Text
    }

loadContentPartsStatement :: Statement Text [ContentPartRow]
loadContentPartsStatement = mkStatement
    "SELECT part_type, text_value, refusal_text, detail, file_data, file_id,\
    \ file_url, filename, image_url, input_audio_text,\
    \ prompt_cache_breakpoint_text, annotations_text, logprobs_text,\
    \ extra_fields_text\
    \ FROM harness.session_response_content_parts\
    \ WHERE response_item_id = $1::uuid\
    \ ORDER BY part_index"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowList contentPartRowDecoder)
    True

loadTurnContentPartsStatement :: Statement Text [(Text, ContentPartRow)]
loadTurnContentPartsStatement = mkStatement
    "SELECT child.response_item_id::text, child.part_type,\
    \ child.text_value, child.refusal_text, child.detail, child.file_data,\
    \ child.file_id, child.file_url, child.filename, child.image_url,\
    \ child.input_audio_text, child.prompt_cache_breakpoint_text,\
    \ child.annotations_text, child.logprobs_text, child.extra_fields_text\
    \ FROM harness.session_response_content_parts child\
    \ WHERE child.response_item_id = ANY (ARRAY(\
    \   SELECT item.response_item_id\
    \   FROM harness.session_response_items item\
    \   WHERE item.turn_id = $1::uuid\
    \ ))\
    \ ORDER BY child.response_item_id ASC, child.part_index ASC"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowList $
        (,)
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> contentPartRowDecoder)
    True

contentPartRowDecoder :: Decoders.Row ContentPartRow
contentPartRowDecoder =
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
        <*> Decoders.column (Decoders.nullable Decoders.text)
        <*> Decoders.column (Decoders.nullable Decoders.text)
        <*> Decoders.column (Decoders.nullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
