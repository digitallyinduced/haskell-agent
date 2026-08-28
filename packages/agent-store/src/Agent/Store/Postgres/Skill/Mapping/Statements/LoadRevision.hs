module Agent.Store.Postgres.Skill.Mapping.Statements.LoadRevision
    ( loadRevisionStatement
    ) where

import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)

import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.Postgres.Skill.Mapping.Types

loadRevisionStatement :: Statement (Text, Int64) (Maybe RevisionRow)
loadRevisionStatement = mkStatement
    (revisionSelectSql
        <> " WHERE skill_id = $1::uuid AND revision_number = $2")
    ( (fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    )
    (Decoders.rowMaybe revisionRowDecoder)
    True

revisionSelectSql :: Text
revisionSelectSql =
    "SELECT skill_revision_id::text, revision_number, title, description,\
    \ applies_when, instructions_text, activation_mode, priority, status,\
    \ change_summary, created_at FROM harness.skill_revisions"

revisionRowDecoder :: Decoders.Row RevisionRow
revisionRowDecoder =
    RevisionRow
        <$> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.int8)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.int4)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.timestamptz)
