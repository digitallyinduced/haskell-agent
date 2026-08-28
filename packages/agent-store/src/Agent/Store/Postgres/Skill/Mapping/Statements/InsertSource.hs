module Agent.Store.Postgres.Skill.Mapping.Statements.InsertSource
    ( insertSourceStatement
    ) where

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)

import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.Postgres.Skill.Mapping.Types

insertSourceStatement :: Statement InsertSourceParams ()
insertSourceStatement = mkStatement
    "INSERT INTO harness.skill_sources\
    \ (skill_revision_id, source_session_key, source_turn_index,\
    \ source_response_item_id, evidence_text, created_at)\
    \ VALUES ($1::uuid, $2, $3, $4, $5, $6)"
    ( ((.insertSourceRevisionId) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.insertSourceSessionKey) >$< Encoders.param (Encoders.nullable Encoders.text))
        <> ((.insertSourceTurnIndex) >$< Encoders.param (Encoders.nullable Encoders.int8))
        <> ((.insertSourceResponseItemId) >$< Encoders.param (Encoders.nullable Encoders.text))
        <> ((.insertSourceEvidence) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.insertSourceAt) >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz))
    )
    Decoders.noResult
    True
