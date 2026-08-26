module Agent.Store.Postgres.Skill.Mapping.Statements.ListSkills
    ( listSkillsStatement
    ) where

import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)

import Agent.Store.Postgres.Hasql (mkStatement)
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

applicableScopesEncoder :: Encoders.Params ApplicableScopes
applicableScopesEncoder =
    ((.applicableUserScopeId) >$< textParam)
        <> ((.applicableRepositoryScopeId) >$< textParam)
        <> ((.applicableCheckoutScopeId) >$< textParam)
  where
    textParam = Encoders.param (Encoders.nonNullable Encoders.text)

skillSelectSql :: Text
skillSelectSql =
    "SELECT skill_id::text, scope_kind, scope_id::text, slug, title,\
    \ description, applies_when, instructions_text, activation_mode,\
    \ priority, status, current_revision, created_at, updated_at\
    \ FROM harness.skills"

applicableWhereSql :: Text
applicableWhereSql =
    " WHERE (\
    \ (scope_kind = 'user' AND scope_id = $1::uuid)\
    \ OR (scope_kind = 'repository' AND scope_id = $2::uuid)\
    \ OR (scope_kind = 'checkout' AND scope_id = $3::uuid)\
    \ )"

skillRowDecoder :: Decoders.Row SkillRow
skillRowDecoder =
    SkillRow
        <$> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.int4)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.int8)
        <*> Decoders.column (Decoders.nonNullable Decoders.timestamptz)
        <*> Decoders.column (Decoders.nonNullable Decoders.timestamptz)
