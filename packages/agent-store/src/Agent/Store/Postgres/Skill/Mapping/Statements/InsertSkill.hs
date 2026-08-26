module Agent.Store.Postgres.Skill.Mapping.Statements.InsertSkill
    ( insertSkillStatement
    ) where

import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)

import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.Postgres.Skill.Mapping.Types

insertSkillStatement :: Statement InsertSkillParams (Maybe Text)
insertSkillStatement = mkStatement
    "INSERT INTO harness.skills\
    \ (scope_kind, scope_id, slug, title, description, applies_when,\
    \ instructions_text, activation_mode, priority, status,\
    \ current_revision, created_at, updated_at)\
    \ VALUES ($1, $2::uuid, $3, $4, $5, $6, $7, $8, $9, $10, 1, $11, $11)\
    \ ON CONFLICT (scope_kind, scope_id, slug) DO NOTHING\
    \ RETURNING skill_id::text"
    ( ((.insertSkillScopeKind) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.insertSkillScopeId) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.insertSkillSlug) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.insertSkillTitle) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.insertSkillDescription) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.insertSkillAppliesWhen) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.insertSkillInstructions) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.insertSkillActivation) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.insertSkillPriority) >$< Encoders.param (Encoders.nonNullable Encoders.int4))
        <> ((.insertSkillStatus) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.insertSkillAt) >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz))
    )
    (Decoders.rowMaybe (Decoders.column (Decoders.nonNullable Decoders.text)))
    True
