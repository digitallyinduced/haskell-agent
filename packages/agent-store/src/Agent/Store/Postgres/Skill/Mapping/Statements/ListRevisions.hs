module Agent.Store.Postgres.Skill.Mapping.Statements.ListRevisions
    ( listRevisionsStatement
    ) where

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)

import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.Postgres.Skill.Mapping.Types

listRevisionsStatement :: Statement ScopeSlugParams [RevisionRow]
listRevisionsStatement = mkStatement
    "SELECT r.skill_revision_id::text, r.revision_number, r.title,\
    \ r.description, r.applies_when, r.instructions_text,\
    \ r.activation_mode, r.priority, r.status, r.change_summary, r.created_at\
    \ FROM harness.skill_revisions r\
    \ JOIN harness.skills s ON s.skill_id = r.skill_id\
    \ WHERE s.scope_kind = $1 AND s.scope_id = $2::uuid AND s.slug = $3\
    \ ORDER BY r.revision_number DESC"
    scopeSlugEncoder
    (Decoders.rowList revisionRowDecoder)
    True

scopeSlugEncoder :: Encoders.Params ScopeSlugParams
scopeSlugEncoder =
    ((.scopeSlugKind) >$< textParam)
        <> ((.scopeSlugId) >$< textParam)
        <> ((.scopeSlug) >$< textParam)
  where
    textParam = Encoders.param (Encoders.nonNullable Encoders.text)

revisionRowDecoder :: Decoders.Row RevisionRow
revisionRowDecoder =
    RevisionRow
        <$> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.int8)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.int4)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.timestamptz)
