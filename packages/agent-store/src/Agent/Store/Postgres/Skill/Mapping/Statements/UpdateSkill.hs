module Agent.Store.Postgres.Skill.Mapping.Statements.UpdateSkill
    ( updateSkillStatement
    ) where

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)

import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.Postgres.Skill.Mapping.Types

updateSkillStatement :: Statement UpdateSkillParams ()
updateSkillStatement = mkStatement
    "UPDATE harness.skills SET\
    \ title = $2, description = $3, applies_when = $4,\
    \ instructions_text = $5, activation_mode = $6, priority = $7,\
    \ status = $8, current_revision = $9, updated_at = $10\
    \ WHERE skill_id = $1::uuid"
    ( ((.updateSkillId) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.updateSkillTitle) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.updateSkillDescription) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.updateSkillAppliesWhen) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.updateSkillInstructions) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.updateSkillActivation) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.updateSkillPriority) >$< Encoders.param (Encoders.nonNullable Encoders.int4))
        <> ((.updateSkillStatus) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.updateSkillRevision) >$< Encoders.param (Encoders.nonNullable Encoders.int8))
        <> ((.updateSkillAt) >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz))
    )
    Decoders.noResult
    True
