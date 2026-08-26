module Agent.Store.Postgres.Skill.Mapping.Statements.LockSkill
    ( lockSkillStatement
    ) where

import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)

import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.Postgres.Skill.Mapping.Types

lockSkillStatement :: Statement ScopeSlugParams (Maybe SkillRow)
lockSkillStatement = mkStatement
    (skillSelectSql
        <> " WHERE scope_kind = $1 AND scope_id = $2::uuid AND slug = $3\
           \ FOR UPDATE")
    scopeSlugEncoder
    (Decoders.rowMaybe skillRowDecoder)
    True

scopeSlugEncoder :: Encoders.Params ScopeSlugParams
scopeSlugEncoder =
    ((.scopeSlugKind) >$< textParam)
        <> ((.scopeSlugId) >$< textParam)
        <> ((.scopeSlug) >$< textParam)
  where
    textParam = Encoders.param (Encoders.nonNullable Encoders.text)

skillSelectSql :: Text
skillSelectSql =
    "SELECT skill_id::text, scope_kind, scope_id::text, slug, title,\
    \ description, applies_when, instructions_text, activation_mode,\
    \ priority, status, current_revision, created_at, updated_at\
    \ FROM harness.skills"

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
