module Agent.Store.Postgres.Skill.Mapping.Statements.ReadSkill
    ( readSkillStatement
    , readRevisionStatement
    ) where

import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)

import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.Postgres.Skill.Mapping.Codec
    ( scopeSlugEncoder
    , skillSelectSql
    , skillRowDecoder
    , revisionRowDecoder
    , revisionColumns
    )
import Agent.Store.Postgres.Skill.Mapping.Types

readSkillStatement :: Statement ScopeSlugParams (Maybe SkillRow)
readSkillStatement = mkStatement
    (skillSelectSql
        <> " WHERE scope_kind = $1 AND scope_id = $2::uuid AND slug = $3")
    scopeSlugEncoder
    (Decoders.rowMaybe skillRowDecoder)
    True

readRevisionStatement
    :: Statement (ScopeSlugParams, Int64) (Maybe RevisionRow)
readRevisionStatement = mkStatement
    ("SELECT " <> revisionColumns "r."
        <> " FROM harness.skill_revisions r\
        \ JOIN harness.skills s ON s.skill_id = r.skill_id\
        \ WHERE s.scope_kind = $1 AND s.scope_id = $2::uuid AND s.slug = $3\
        \ AND r.revision_number = $4")
    ( (fst >$< scopeSlugEncoder)
        <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    )
    (Decoders.rowMaybe revisionRowDecoder)
    True
