module Agent.Store.Postgres.Skill.Mapping.Codec
    ( scopeSlugEncoder
    , applicableScopesEncoder
    , skillSelectSql
    , skillColumns
    , revisionColumns
    , skillRowDecoder
    , revisionRowDecoder
    ) where

import Agent.Store.Postgres.Skill.Mapping.Types
import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders

scopeSlugEncoder :: Encoders.Params ScopeSlugParams
scopeSlugEncoder =
    ((.scopeSlugKind) >$< textParam)
        <> ((.scopeSlugId) >$< textParam)
        <> ((.scopeSlug) >$< textParam)
  where
    textParam = Encoders.param (Encoders.nonNullable Encoders.text)

applicableScopesEncoder :: Encoders.Params ApplicableScopes
applicableScopesEncoder =
    ((.applicableUserScopeId) >$< textParam)
        <> ((.applicableRepositoryScopeId) >$< textParam)
        <> ((.applicableCheckoutScopeId) >$< textParam)
  where
    textParam = Encoders.param (Encoders.nonNullable Encoders.text)

skillSelectSql :: Text
skillSelectSql = "SELECT " <> skillColumns "" <> " FROM harness.skills"

-- Keep each SQL projection beside its positional decoder. The prefix is a
-- fixed table alias supplied by the statement, including its trailing dot.
skillColumns :: Text -> Text
skillColumns prefix = Text.intercalate ", " $ map (prefix <>)
    [ "skill_id::text", "scope_kind", "scope_id::text", "slug", "title"
    , "description", "applies_when", "instructions_text", "activation_mode"
    , "priority", "status", "current_revision", "created_at", "updated_at"
    ]

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

revisionColumns :: Text -> Text
revisionColumns prefix = Text.intercalate ", " $ map (prefix <>)
    [ "skill_revision_id::text", "revision_number", "title", "description"
    , "applies_when", "instructions_text", "activation_mode", "priority"
    , "status", "change_summary", "created_at"
    ]

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
