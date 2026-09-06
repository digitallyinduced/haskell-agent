module Agent.Store.Postgres.Skill.Mapping.Statements.ListSkills
    ( listSkillsStatement
    , listAllSkillsStatement
    , listAllSkillsLimitedStatement
    ) where

import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)

import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.Postgres.Skill.Mapping.Codec
    ( applicableScopesEncoder
    , skillSelectSql
    , skillRowDecoder
    )
import Agent.Store.Postgres.Skill.Mapping.Types

listSkillsStatement :: Statement ApplicableScopes [SkillRow]
listSkillsStatement = mkStatement
    (skillSelectSql
        <> applicableWhereSql
        <> " AND status = 'active'\
           \ ORDER BY CASE activation_mode\
           \   WHEN 'always' THEN 0 WHEN 'relevant' THEN 1 ELSE 2 END,\
           \ priority DESC,\
           \ CASE scope_kind\
           \   WHEN 'checkout' THEN 0 WHEN 'repository' THEN 1 ELSE 2 END,\
           \ title, slug")
    applicableScopesEncoder
    (Decoders.rowList skillRowDecoder)
    True

listAllSkillsStatement :: Statement ApplicableScopes [SkillRow]
listAllSkillsStatement = mkStatement
    (skillSelectSql
        <> applicableWhereSql
        <> " ORDER BY scope_kind, title, slug")
    applicableScopesEncoder
    (Decoders.rowList skillRowDecoder)
    True

listAllSkillsLimitedStatement :: Statement SkillListParams [SkillRow]
listAllSkillsLimitedStatement = mkStatement
    (skillSelectSql
        <> applicableWhereSql
        <> " AND ($4::text IS NULL OR scope_kind = $4)\
           \ ORDER BY scope_kind, title, slug LIMIT $5")
    ( ((.skillListScopes) >$< applicableScopesEncoder)
        <> ((.skillListKind)
            >$< Encoders.param (Encoders.nullable Encoders.text))
        <> ((.skillListLimit)
            >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    )
    (Decoders.rowList skillRowDecoder)
    True

applicableWhereSql :: Text
applicableWhereSql =
    " WHERE (\
    \ (scope_kind = 'user' AND scope_id = $1::uuid)\
    \ OR (scope_kind = 'repository' AND scope_id = $2::uuid)\
    \ OR (scope_kind = 'checkout' AND scope_id = $3::uuid)\
    \ )"
