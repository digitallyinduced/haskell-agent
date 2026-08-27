module Agent.Store.Postgres.Skill.Mapping.Statements.InsertRevision
    ( insertRevisionStatement
    ) where

import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)

import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.Postgres.Skill.Mapping.Types

insertRevisionStatement :: Statement InsertRevisionParams Text
insertRevisionStatement = mkStatement
    "INSERT INTO harness.skill_revisions\
    \ (skill_id, revision_number, title, description, applies_when,\
    \ instructions_text, activation_mode, priority, status, change_summary,\
    \ created_at)\
    \ VALUES ($1::uuid, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11)\
    \ RETURNING skill_revision_id::text"
    ( ((.insertRevisionSkillId) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.insertRevisionNumber) >$< Encoders.param (Encoders.nonNullable Encoders.int8))
        <> ((.insertRevisionTitle) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.insertRevisionDescription) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.insertRevisionAppliesWhen) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.insertRevisionInstructions) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.insertRevisionActivation) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.insertRevisionPriority) >$< Encoders.param (Encoders.nonNullable Encoders.int4))
        <> ((.insertRevisionStatus) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.insertRevisionSummary) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.insertRevisionAt) >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz))
    )
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))
    True
