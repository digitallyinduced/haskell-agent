module Agent.Store.Postgres.Skill.Mapping.Statements.ListRevisions
    ( listRevisionsStatement
    , listRevisionsLimitedStatement
    ) where

import Data.Int (Int64)
import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)

import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.Postgres.Skill.Mapping.Codec
    ( scopeSlugEncoder
    , revisionRowDecoder
    , revisionColumns
    )
import Agent.Store.Postgres.Skill.Mapping.Types

listRevisionsStatement :: Statement ScopeSlugParams [RevisionRow]
listRevisionsStatement = mkStatement
    ("SELECT " <> revisionColumns "r."
        <> " FROM harness.skill_revisions r\
        \ JOIN harness.skills s ON s.skill_id = r.skill_id\
        \ WHERE s.scope_kind = $1 AND s.scope_id = $2::uuid AND s.slug = $3\
        \ ORDER BY r.revision_number DESC")
    scopeSlugEncoder
    (Decoders.rowList revisionRowDecoder)
    True

listRevisionsLimitedStatement
    :: Statement (ScopeSlugParams, Int64) [RevisionRow]
listRevisionsLimitedStatement = mkStatement
    ("SELECT " <> revisionColumns "r."
        <> " FROM harness.skill_revisions r\
        \ JOIN harness.skills s ON s.skill_id = r.skill_id\
        \ WHERE s.scope_kind = $1 AND s.scope_id = $2::uuid AND s.slug = $3\
        \ ORDER BY r.revision_number DESC LIMIT $4")
    ( (fst >$< scopeSlugEncoder)
        <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    )
    (Decoders.rowList revisionRowDecoder)
    True
