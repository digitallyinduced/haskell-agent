module Agent.Store.Postgres.Skill.Mapping.Statements.LockSkill
    ( lockSkillStatement
    ) where

import qualified Hasql.Decoders as Decoders
import Hasql.Statement (Statement)

import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.Postgres.Skill.Mapping.Codec
    ( scopeSlugEncoder
    , skillSelectSql
    , skillRowDecoder
    )
import Agent.Store.Postgres.Skill.Mapping.Types

lockSkillStatement :: Statement ScopeSlugParams (Maybe SkillRow)
lockSkillStatement = mkStatement
    (skillSelectSql
        <> " WHERE scope_kind = $1 AND scope_id = $2::uuid AND slug = $3\
           \ FOR UPDATE")
    scopeSlugEncoder
    (Decoders.rowMaybe skillRowDecoder)
    True
