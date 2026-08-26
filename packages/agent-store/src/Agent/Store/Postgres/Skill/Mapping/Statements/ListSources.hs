module Agent.Store.Postgres.Skill.Mapping.Statements.ListSources
    ( listSourcesStatement
    ) where

import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)

import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.Postgres.Skill.Mapping.Types

listSourcesStatement
    :: Statement (ScopeSlugParams, Int64) [SourceRow]
listSourcesStatement = mkStatement
    "SELECT src.skill_source_id::text, r.revision_number,\
    \ src.source_session_key, src.source_turn_index,\
    \ src.source_response_item_id, src.evidence_text, src.created_at\
    \ FROM harness.skill_sources src\
    \ JOIN harness.skill_revisions r\
    \   ON r.skill_revision_id = src.skill_revision_id\
    \ JOIN harness.skills s ON s.skill_id = r.skill_id\
    \ WHERE s.scope_kind = $1 AND s.scope_id = $2::uuid AND s.slug = $3\
    \   AND r.revision_number = $4\
    \ ORDER BY src.created_at, src.skill_source_id"
    ( ((.scopeSlugKind) . fst >$< textParam)
        <> ((.scopeSlugId) . fst >$< textParam)
        <> ((.scopeSlug) . fst >$< textParam)
        <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    )
    (Decoders.rowList sourceRowDecoder)
    True
  where
    textParam = Encoders.param (Encoders.nonNullable Encoders.text)

sourceRowDecoder :: Decoders.Row SourceRow
sourceRowDecoder =
    SourceRow
        <$> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.int8)
        <*> Decoders.column (Decoders.nullable Decoders.text)
        <*> Decoders.column (Decoders.nullable Decoders.int8)
        <*> Decoders.column (Decoders.nullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.timestamptz)
