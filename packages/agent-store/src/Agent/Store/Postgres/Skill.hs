{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Typed, versioned learned skills stored in the harness PostgreSQL schema.
--
-- Conversation history remains the immutable evidence layer.  Skills are the
-- smaller, reusable instructions promoted from that history for future
-- sessions.  The current projection is searchable while every accepted change
-- is retained as an immutable revision with explicit source evidence.
module Agent.Store.Postgres.Skill
    ( LearnedSkillActivation(..)
    , LearnedSkillStatus(..)
    , LearnedSkill(..)
    , LearnedSkillRevision(..)
    , LearnedSkillSource(..)
    , LearnedSkillSourceInput(..)
    , LearnedSkillCreate(..)
    , LearnedSkillPatch(..)
    , LearnedSkillUpdate(..)
    , LearnedSkillRollback(..)
    , LearnedSkillSearchResult(..)
    , LearnedSkillMutationResult(..)
    , learnedSkillSchemaStatements
    , learnedSkillRuntimeGrantStatements
    , learnedSkillActivationText
    , learnedSkillStatusText
    , createLearnedSkill
    , updateLearnedSkill
    , archiveLearnedSkill
    , rollbackLearnedSkill
    , readLearnedSkill
    , readLearnedSkillRevision
    , searchLearnedSkills
    , listApplicableLearnedSkills
    , listAllLearnedSkills
    , listAllLearnedSkillsLimited
    , listLearnedSkillRevisions
    , listLearnedSkillRevisionsLimited
    , listLearnedSkillSources
    ) where

import Data.ByteString (ByteString)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Session as HasqlSession
import Hasql.Statement (Statement)
import qualified Hasql.Transaction as Transaction
import qualified Hasql.Transaction.Sessions as Transactions

import Agent.Store.Postgres.Connection
    ( StorePool
    , withSession
    )
import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.Postgres.Scope
    ( Scope(..)
    , ScopeKind(..)
    , mkScopeId
    , scopeIdText
    , scopeKindText
    )
import Agent.Store.Types (StoreError(..))

data LearnedSkillActivation
    = SkillAlways
    | SkillRelevant
    | SkillManual
    deriving (Eq, Ord, Show)

data LearnedSkillStatus
    = SkillActive
    | SkillArchived
    deriving (Eq, Ord, Show)

data LearnedSkill = LearnedSkill
    { learnedSkillId :: !Text
    , learnedSkillScope :: !Scope
    , learnedSkillSlug :: !Text
    , learnedSkillTitle :: !Text
    , learnedSkillDescription :: !Text
    , learnedSkillAppliesWhen :: !Text
    , learnedSkillInstructions :: !Text
    , learnedSkillActivation :: !LearnedSkillActivation
    , learnedSkillPriority :: !Int32
    , learnedSkillStatus :: !LearnedSkillStatus
    , learnedSkillRevision :: !Int64
    , learnedSkillCreatedAt :: !UTCTime
    , learnedSkillUpdatedAt :: !UTCTime
    }
    deriving (Eq, Show)

data LearnedSkillRevision = LearnedSkillRevision
    { learnedSkillRevisionId :: !Text
    , learnedSkillRevisionNumber :: !Int64
    , learnedSkillRevisionTitle :: !Text
    , learnedSkillRevisionDescription :: !Text
    , learnedSkillRevisionAppliesWhen :: !Text
    , learnedSkillRevisionInstructions :: !Text
    , learnedSkillRevisionActivation :: !LearnedSkillActivation
    , learnedSkillRevisionPriority :: !Int32
    , learnedSkillRevisionStatus :: !LearnedSkillStatus
    , learnedSkillRevisionSummary :: !Text
    , learnedSkillRevisionCreatedAt :: !UTCTime
    }
    deriving (Eq, Show)

data LearnedSkillSource = LearnedSkillSource
    { learnedSkillSourceId :: !Text
    , learnedSkillSourceRevision :: !Int64
    , learnedSkillSourceSessionKey :: !(Maybe Text)
    , learnedSkillSourceTurnIndex :: !(Maybe Int64)
    , learnedSkillSourceResponseItemId :: !(Maybe Text)
    , learnedSkillSourceEvidence :: !Text
    , learnedSkillSourceCreatedAt :: !UTCTime
    }
    deriving (Eq, Show)

data LearnedSkillSourceInput = LearnedSkillSourceInput
    { learnedSkillSourceInputSessionKey :: !(Maybe Text)
    , learnedSkillSourceInputTurnIndex :: !(Maybe Int64)
    , learnedSkillSourceInputResponseItemId :: !(Maybe Text)
    , learnedSkillSourceInputEvidence :: !Text
    }
    deriving (Eq, Show)

data LearnedSkillCreate = LearnedSkillCreate
    { learnedSkillCreateScope :: !Scope
    , learnedSkillCreateSlug :: !Text
    , learnedSkillCreateTitle :: !Text
    , learnedSkillCreateDescription :: !Text
    , learnedSkillCreateAppliesWhen :: !Text
    , learnedSkillCreateInstructions :: !Text
    , learnedSkillCreateActivation :: !LearnedSkillActivation
    , learnedSkillCreatePriority :: !Int32
    , learnedSkillCreateStatus :: !LearnedSkillStatus
    , learnedSkillCreateSummary :: !Text
    , learnedSkillCreateSource :: !LearnedSkillSourceInput
    , learnedSkillCreateAt :: !UTCTime
    }
    deriving (Eq, Show)

data LearnedSkillPatch = LearnedSkillPatch
    { learnedSkillPatchTitle :: !(Maybe Text)
    , learnedSkillPatchDescription :: !(Maybe Text)
    , learnedSkillPatchAppliesWhen :: !(Maybe Text)
    , learnedSkillPatchInstructions :: !(Maybe Text)
    , learnedSkillPatchActivation :: !(Maybe LearnedSkillActivation)
    , learnedSkillPatchPriority :: !(Maybe Int32)
    , learnedSkillPatchStatus :: !(Maybe LearnedSkillStatus)
    }
    deriving (Eq, Show)

data LearnedSkillUpdate = LearnedSkillUpdate
    { learnedSkillUpdateScope :: !Scope
    , learnedSkillUpdateSlug :: !Text
    , learnedSkillUpdateExpectedRevision :: !Int64
    , learnedSkillUpdatePatch :: !LearnedSkillPatch
    , learnedSkillUpdateSummary :: !Text
    , learnedSkillUpdateSource :: !LearnedSkillSourceInput
    , learnedSkillUpdateAt :: !UTCTime
    }
    deriving (Eq, Show)

data LearnedSkillRollback = LearnedSkillRollback
    { learnedSkillRollbackScope :: !Scope
    , learnedSkillRollbackSlug :: !Text
    , learnedSkillRollbackExpectedRevision :: !Int64
    , learnedSkillRollbackTargetRevision :: !Int64
    , learnedSkillRollbackSummary :: !Text
    , learnedSkillRollbackSource :: !LearnedSkillSourceInput
    , learnedSkillRollbackAt :: !UTCTime
    }
    deriving (Eq, Show)

data LearnedSkillSearchResult = LearnedSkillSearchResult
    { learnedSkillSearchSkill :: !LearnedSkill
    , learnedSkillSearchRank :: !Double
    }
    deriving (Eq, Show)

data LearnedSkillMutationResult
    = LearnedSkillMutationApplied !LearnedSkill
    | LearnedSkillMutationAlreadyExists
    | LearnedSkillMutationNotFound
    | LearnedSkillMutationConflict !Int64
    | LearnedSkillMutationRevisionNotFound
    deriving (Eq, Show)

learnedSkillActivationText :: LearnedSkillActivation -> Text
learnedSkillActivationText = \case
    SkillAlways -> "always"
    SkillRelevant -> "relevant"
    SkillManual -> "manual"

learnedSkillStatusText :: LearnedSkillStatus -> Text
learnedSkillStatusText = \case
    SkillActive -> "active"
    SkillArchived -> "archived"

learnedSkillSchemaStatements :: [ByteString]
learnedSkillSchemaStatements =
    [ "CREATE TABLE IF NOT EXISTS harness.skills (\
      \ skill_id uuid PRIMARY KEY DEFAULT pg_catalog.uuidv7(),\
      \ scope_kind text NOT NULL\
      \   CHECK (scope_kind IN ('user', 'repository', 'checkout')),\
      \ scope_id uuid NOT NULL,\
      \ slug text NOT NULL\
      \   CHECK (slug ~ '^[a-z0-9]+(-[a-z0-9]+)*$' AND length(slug) <= 80),\
      \ title text NOT NULL\
      \   CHECK (length(btrim(title)) > 0 AND length(title) <= 200),\
      \ description text NOT NULL\
      \   CHECK (length(btrim(description)) > 0 AND length(description) <= 1000),\
      \ applies_when text NOT NULL CHECK (length(applies_when) <= 2000),\
      \ instructions_text text NOT NULL\
      \   CHECK (length(btrim(instructions_text)) > 0\
      \     AND length(instructions_text) <= 30000),\
      \ activation_mode text NOT NULL\
      \   CHECK (activation_mode IN ('always', 'relevant', 'manual')),\
      \ priority integer NOT NULL DEFAULT 0\
      \   CHECK (priority BETWEEN -100 AND 100),\
      \ status text NOT NULL\
      \   CHECK (status IN ('active', 'archived')),\
      \ current_revision bigint NOT NULL CHECK (current_revision >= 1),\
      \ created_at timestamptz NOT NULL,\
      \ updated_at timestamptz NOT NULL,\
      \ search_vector tsvector GENERATED ALWAYS AS (\
      \   setweight(to_tsvector('english', coalesce(title, '')), 'A') ||\
      \   setweight(to_tsvector('english', coalesce(description, '')), 'A') ||\
      \   setweight(to_tsvector('english', coalesce(applies_when, '')), 'A') ||\
      \   setweight(to_tsvector('english', coalesce(instructions_text, '')), 'B')\
      \ ) STORED,\
      \ UNIQUE (scope_kind, scope_id, slug),\
      \ CHECK (updated_at >= created_at)\
      \ )"
    , "CREATE INDEX IF NOT EXISTS skills_search_idx\
      \ ON harness.skills USING gin (search_vector)"
    , "CREATE INDEX IF NOT EXISTS skills_scope_active_idx\
      \ ON harness.skills\
      \ (scope_kind, scope_id, activation_mode, priority DESC, updated_at DESC)\
      \ WHERE status = 'active'"
    , "CREATE TABLE IF NOT EXISTS harness.skill_revisions (\
      \ skill_revision_id uuid PRIMARY KEY DEFAULT pg_catalog.uuidv7(),\
      \ skill_id uuid NOT NULL\
      \   REFERENCES harness.skills(skill_id) ON DELETE RESTRICT,\
      \ revision_number bigint NOT NULL CHECK (revision_number >= 1),\
      \ title text NOT NULL,\
      \ description text NOT NULL,\
      \ applies_when text NOT NULL,\
      \ instructions_text text NOT NULL,\
      \ activation_mode text NOT NULL\
      \   CHECK (activation_mode IN ('always', 'relevant', 'manual')),\
      \ priority integer NOT NULL CHECK (priority BETWEEN -100 AND 100),\
      \ status text NOT NULL\
      \   CHECK (status IN ('active', 'archived')),\
      \ change_summary text NOT NULL CHECK (length(btrim(change_summary)) > 0),\
      \ created_at timestamptz NOT NULL,\
      \ UNIQUE (skill_id, revision_number)\
      \ )"
    , "CREATE INDEX IF NOT EXISTS skill_revisions_skill_idx\
      \ ON harness.skill_revisions (skill_id, revision_number DESC)"
      -- Session ids are reserved before the first successful turn is persisted,
      -- so provenance uses a deliberate soft reference instead of a foreign key.
    , "CREATE TABLE IF NOT EXISTS harness.skill_sources (\
      \ skill_source_id uuid PRIMARY KEY DEFAULT pg_catalog.uuidv7(),\
      \ skill_revision_id uuid NOT NULL\
      \   REFERENCES harness.skill_revisions(skill_revision_id)\
      \   ON DELETE RESTRICT,\
      \ source_session_key text,\
      \ source_turn_index bigint CHECK (source_turn_index >= 0),\
      \ source_response_item_id text,\
      \ evidence_text text NOT NULL\
      \   CHECK (length(btrim(evidence_text)) > 0\
      \     AND length(evidence_text) <= 8000),\
      \ created_at timestamptz NOT NULL\
      \ )"
    , "CREATE INDEX IF NOT EXISTS skill_sources_revision_idx\
      \ ON harness.skill_sources (skill_revision_id, created_at)"
    , "CREATE OR REPLACE FUNCTION harness.reject_skill_fact_mutation()\
      \ RETURNS trigger\
      \ LANGUAGE plpgsql\
      \ AS $$ BEGIN\
      \ RAISE EXCEPTION 'skill revisions and sources are immutable';\
      \ END $$"
    , "DROP TRIGGER IF EXISTS skill_revisions_immutable\
      \ ON harness.skill_revisions"
    , "CREATE TRIGGER skill_revisions_immutable\
      \ BEFORE UPDATE OR DELETE ON harness.skill_revisions\
      \ FOR EACH ROW EXECUTE FUNCTION harness.reject_skill_fact_mutation()"
    , "DROP TRIGGER IF EXISTS skill_sources_immutable\
      \ ON harness.skill_sources"
    , "CREATE TRIGGER skill_sources_immutable\
      \ BEFORE UPDATE OR DELETE ON harness.skill_sources\
      \ FOR EACH ROW EXECUTE FUNCTION harness.reject_skill_fact_mutation()"
    ]

learnedSkillRuntimeGrantStatements :: [ByteString]
learnedSkillRuntimeGrantStatements =
    [ "GRANT SELECT ON harness.skills TO ha_runtime"
    , "GRANT INSERT\
      \ (scope_kind, scope_id, slug, title, description, applies_when,\
      \ instructions_text, activation_mode, priority, status,\
      \ current_revision, created_at, updated_at)\
      \ ON harness.skills TO ha_runtime"
    , "GRANT UPDATE\
      \ (title, description, applies_when, instructions_text, activation_mode,\
      \ priority, status, current_revision, updated_at)\
      \ ON harness.skills TO ha_runtime"
    , "GRANT SELECT ON harness.skill_revisions TO ha_runtime"
    , "GRANT INSERT\
      \ (skill_id, revision_number, title, description, applies_when,\
      \ instructions_text, activation_mode, priority, status, change_summary,\
      \ created_at)\
      \ ON harness.skill_revisions TO ha_runtime"
    , "GRANT SELECT ON harness.skill_sources TO ha_runtime"
    , "GRANT INSERT\
      \ (skill_revision_id, source_session_key, source_turn_index,\
      \ source_response_item_id, evidence_text, created_at)\
      \ ON harness.skill_sources TO ha_runtime"
    ]

createLearnedSkill
    :: StorePool
    -> LearnedSkillCreate
    -> IO (Either StoreError LearnedSkillMutationResult)
createLearnedSkill pool input =
    runSkillWrite pool do
        inserted <- Transaction.statement input insertSkillStatement
        case inserted of
            Nothing -> pure (Right LearnedSkillMutationAlreadyExists)
            Just skillId -> do
                let skill = skillFromCreate skillId input
                revisionId <- Transaction.statement
                    (skill, input.learnedSkillCreateSummary)
                    insertRevisionStatement
                Transaction.statement
                    ( revisionId
                    , 1
                    , input.learnedSkillCreateSource
                    , input.learnedSkillCreateAt
                    )
                    insertSourceStatement
                pure (Right (LearnedSkillMutationApplied skill))

updateLearnedSkill
    :: StorePool
    -> LearnedSkillUpdate
    -> IO (Either StoreError LearnedSkillMutationResult)
updateLearnedSkill pool input =
    runSkillWrite pool do
        currentRow <- Transaction.statement
            (input.learnedSkillUpdateScope, input.learnedSkillUpdateSlug)
            lockSkillStatement
        case currentRow of
            Nothing -> pure (Right LearnedSkillMutationNotFound)
            Just row -> case decodeSkillRow row of
                Left err -> pure (Left err)
                Right current
                    | current.learnedSkillRevision
                        /= input.learnedSkillUpdateExpectedRevision ->
                            pure (Right (LearnedSkillMutationConflict
                                current.learnedSkillRevision))
                    | otherwise -> do
                        let updated = applySkillPatch input current
                        if sameSkillContents current updated
                            then
                                pure
                                    (Left
                                        "learned skill update does not change any stored field")
                            else do
                                Transaction.statement updated updateSkillStatement
                                revisionId <- Transaction.statement
                                    (updated, input.learnedSkillUpdateSummary)
                                    insertRevisionStatement
                                Transaction.statement
                                    ( revisionId
                                    , updated.learnedSkillRevision
                                    , input.learnedSkillUpdateSource
                                    , input.learnedSkillUpdateAt
                                    )
                                    insertSourceStatement
                                pure
                                    (Right
                                        (LearnedSkillMutationApplied updated))

archiveLearnedSkill
    :: StorePool
    -> Scope
    -> Text
    -> Int64
    -> Text
    -> LearnedSkillSourceInput
    -> UTCTime
    -> IO (Either StoreError LearnedSkillMutationResult)
archiveLearnedSkill pool scope slug expectedRevision summary source occurredAt =
    updateLearnedSkill pool LearnedSkillUpdate
        { learnedSkillUpdateScope = scope
        , learnedSkillUpdateSlug = slug
        , learnedSkillUpdateExpectedRevision = expectedRevision
        , learnedSkillUpdatePatch = emptySkillPatch
            { learnedSkillPatchStatus = Just SkillArchived
            }
        , learnedSkillUpdateSummary = summary
        , learnedSkillUpdateSource = source
        , learnedSkillUpdateAt = occurredAt
        }

rollbackLearnedSkill
    :: StorePool
    -> LearnedSkillRollback
    -> IO (Either StoreError LearnedSkillMutationResult)
rollbackLearnedSkill pool input =
    runSkillWrite pool do
        currentRow <- Transaction.statement
            (input.learnedSkillRollbackScope, input.learnedSkillRollbackSlug)
            lockSkillStatement
        case currentRow of
            Nothing -> pure (Right LearnedSkillMutationNotFound)
            Just row -> case decodeSkillRow row of
                Left err -> pure (Left err)
                Right current
                    | current.learnedSkillRevision
                        /= input.learnedSkillRollbackExpectedRevision ->
                            pure (Right (LearnedSkillMutationConflict
                                current.learnedSkillRevision))
                    | input.learnedSkillRollbackTargetRevision
                        >= current.learnedSkillRevision ->
                            pure
                                (Left
                                    "learned skill rollback target must be earlier than the current revision")
                    | otherwise -> do
                        targetRow <- Transaction.statement
                            ( current.learnedSkillId
                            , input.learnedSkillRollbackTargetRevision
                            )
                            loadRevisionStatement
                        case targetRow of
                            Nothing ->
                                pure (Right LearnedSkillMutationRevisionNotFound)
                            Just rawRevision -> case decodeRevisionRow rawRevision of
                                Left err -> pure (Left err)
                                Right target -> do
                                    let restored = skillFromRevision
                                            input.learnedSkillRollbackAt
                                            current
                                            target
                                    Transaction.statement restored updateSkillStatement
                                    revisionId <- Transaction.statement
                                        ( restored
                                        , input.learnedSkillRollbackSummary
                                        )
                                        insertRevisionStatement
                                    Transaction.statement
                                        ( revisionId
                                        , restored.learnedSkillRevision
                                        , input.learnedSkillRollbackSource
                                        , input.learnedSkillRollbackAt
                                        )
                                        insertSourceStatement
                                    pure $ Right $
                                        LearnedSkillMutationApplied restored

readLearnedSkill
    :: StorePool
    -> Scope
    -> Text
    -> IO (Either StoreError (Maybe LearnedSkill))
readLearnedSkill pool scope slug =
    withSession pool
        (HasqlSession.statement (scope, slug) readSkillStatement)
        >>= pure . decodeMaybeSkillResult

readLearnedSkillRevision
    :: StorePool
    -> Scope
    -> Text
    -> Int64
    -> IO (Either StoreError (Maybe LearnedSkillRevision))
readLearnedSkillRevision pool scope slug revision =
    withSession pool
        (HasqlSession.statement
            (scope, slug, revision)
            readRevisionStatement)
        >>= pure . decodeMaybeRevisionResult

searchLearnedSkills
    :: StorePool
    -> [Scope]
    -> Text
    -> Int
    -> IO (Either StoreError [LearnedSkillSearchResult])
searchLearnedSkills pool scopes query limit =
    case applicableScopes scopes of
        Left err -> pure (Left (StoreDataError err))
        Right applicable ->
            withSession pool
                (HasqlSession.statement
                    SkillSearchParams
                        { skillSearchScopes = applicable
                        , skillSearchQuery = query
                        , skillSearchLimit =
                            fromIntegral (max 1 (min 50 limit))
                        }
                    searchSkillsStatement)
                >>= pure . decodeSearchResult

listApplicableLearnedSkills
    :: StorePool
    -> [Scope]
    -> IO (Either StoreError [LearnedSkill])
listApplicableLearnedSkills pool scopes =
    case applicableScopes scopes of
        Left err -> pure (Left (StoreDataError err))
        Right applicable ->
            withSession pool
                (HasqlSession.statement applicable listSkillsStatement)
                >>= pure . decodeSkillListResult

listAllLearnedSkills
    :: StorePool
    -> [Scope]
    -> IO (Either StoreError [LearnedSkill])
listAllLearnedSkills pool scopes =
    case applicableScopes scopes of
        Left err -> pure (Left (StoreDataError err))
        Right applicable ->
            withSession pool
                (HasqlSession.statement applicable listAllSkillsStatement)
                >>= pure . decodeSkillListResult

-- | List a bounded administration page, optionally restricted to one of the
-- three applicable scope kinds. Unlike the model-context list this includes
-- archived rows.
listAllLearnedSkillsLimited
    :: StorePool
    -> [Scope]
    -> Maybe ScopeKind
    -> Int
    -> IO (Either StoreError [LearnedSkill])
listAllLearnedSkillsLimited pool scopes selectedKind limit =
    case applicableScopes scopes of
        Left err -> pure (Left (StoreDataError err))
        Right applicable ->
            withSession pool
                (HasqlSession.statement
                    SkillListParams
                        { skillListScopes = applicable
                        , skillListKind =
                            scopeKindText <$> selectedKind
                        , skillListLimit =
                            fromIntegral (max 1 (min 1000 limit))
                        }
                    listAllSkillsLimitedStatement)
                >>= pure . decodeSkillListResult

listLearnedSkillRevisions
    :: StorePool
    -> Scope
    -> Text
    -> IO (Either StoreError [LearnedSkillRevision])
listLearnedSkillRevisions pool scope slug =
    withSession pool
        (HasqlSession.statement (scope, slug) listRevisionsStatement)
        >>= pure . decodeRevisionListResult

listLearnedSkillRevisionsLimited
    :: StorePool
    -> Scope
    -> Text
    -> Int
    -> IO (Either StoreError [LearnedSkillRevision])
listLearnedSkillRevisionsLimited pool scope slug limit =
    withSession pool
        (HasqlSession.statement
            (scope, slug, fromIntegral (max 1 (min 1000 limit)))
            listRevisionsLimitedStatement)
        >>= pure . decodeRevisionListResult

listLearnedSkillSources
    :: StorePool
    -> Scope
    -> Text
    -> Int64
    -> IO (Either StoreError [LearnedSkillSource])
listLearnedSkillSources pool scope slug revision =
    withSession pool
        (HasqlSession.statement
            (scope, slug, revision)
            listSourcesStatement)

emptySkillPatch :: LearnedSkillPatch
emptySkillPatch = LearnedSkillPatch
    { learnedSkillPatchTitle = Nothing
    , learnedSkillPatchDescription = Nothing
    , learnedSkillPatchAppliesWhen = Nothing
    , learnedSkillPatchInstructions = Nothing
    , learnedSkillPatchActivation = Nothing
    , learnedSkillPatchPriority = Nothing
    , learnedSkillPatchStatus = Nothing
    }

skillFromCreate :: Text -> LearnedSkillCreate -> LearnedSkill
skillFromCreate skillId input = LearnedSkill
    { learnedSkillId = skillId
    , learnedSkillScope = input.learnedSkillCreateScope
    , learnedSkillSlug = input.learnedSkillCreateSlug
    , learnedSkillTitle = input.learnedSkillCreateTitle
    , learnedSkillDescription = input.learnedSkillCreateDescription
    , learnedSkillAppliesWhen = input.learnedSkillCreateAppliesWhen
    , learnedSkillInstructions = input.learnedSkillCreateInstructions
    , learnedSkillActivation = input.learnedSkillCreateActivation
    , learnedSkillPriority = input.learnedSkillCreatePriority
    , learnedSkillStatus = input.learnedSkillCreateStatus
    , learnedSkillRevision = 1
    , learnedSkillCreatedAt = input.learnedSkillCreateAt
    , learnedSkillUpdatedAt = input.learnedSkillCreateAt
    }

applySkillPatch :: LearnedSkillUpdate -> LearnedSkill -> LearnedSkill
applySkillPatch input current =
    let patch = input.learnedSkillUpdatePatch
    in current
        { learnedSkillTitle =
            fromMaybe current.learnedSkillTitle patch.learnedSkillPatchTitle
        , learnedSkillDescription =
            fromMaybe
                current.learnedSkillDescription
                patch.learnedSkillPatchDescription
        , learnedSkillAppliesWhen =
            fromMaybe
                current.learnedSkillAppliesWhen
                patch.learnedSkillPatchAppliesWhen
        , learnedSkillInstructions =
            fromMaybe
                current.learnedSkillInstructions
                patch.learnedSkillPatchInstructions
        , learnedSkillActivation =
            fromMaybe
                current.learnedSkillActivation
                patch.learnedSkillPatchActivation
        , learnedSkillPriority =
            fromMaybe current.learnedSkillPriority patch.learnedSkillPatchPriority
        , learnedSkillStatus =
            fromMaybe current.learnedSkillStatus patch.learnedSkillPatchStatus
        , learnedSkillRevision = current.learnedSkillRevision + 1
        , learnedSkillUpdatedAt = input.learnedSkillUpdateAt
        }

sameSkillContents :: LearnedSkill -> LearnedSkill -> Bool
sameSkillContents left right =
    ( left.learnedSkillTitle
    , left.learnedSkillDescription
    , left.learnedSkillAppliesWhen
    , left.learnedSkillInstructions
    , left.learnedSkillActivation
    , left.learnedSkillPriority
    , left.learnedSkillStatus
    )
        == ( right.learnedSkillTitle
           , right.learnedSkillDescription
           , right.learnedSkillAppliesWhen
           , right.learnedSkillInstructions
           , right.learnedSkillActivation
           , right.learnedSkillPriority
           , right.learnedSkillStatus
           )

skillFromRevision
    :: UTCTime
    -> LearnedSkill
    -> LearnedSkillRevision
    -> LearnedSkill
skillFromRevision occurredAt current revision =
    current
        { learnedSkillTitle = revision.learnedSkillRevisionTitle
        , learnedSkillDescription = revision.learnedSkillRevisionDescription
        , learnedSkillAppliesWhen = revision.learnedSkillRevisionAppliesWhen
        , learnedSkillInstructions = revision.learnedSkillRevisionInstructions
        , learnedSkillActivation = revision.learnedSkillRevisionActivation
        , learnedSkillPriority = revision.learnedSkillRevisionPriority
        , learnedSkillStatus = revision.learnedSkillRevisionStatus
        , learnedSkillRevision = current.learnedSkillRevision + 1
        , learnedSkillUpdatedAt = occurredAt
        }

runSkillWrite
    :: StorePool
    -> Transaction.Transaction (Either Text a)
    -> IO (Either StoreError a)
runSkillWrite pool action =
    withSession pool
        (Transactions.transaction
            Transactions.Serializable
            Transactions.Write
            action)
        >>= pure . flattenDataResult

flattenDataResult
    :: Either StoreError (Either Text a)
    -> Either StoreError a
flattenDataResult = \case
    Left err -> Left err
    Right (Left err) -> Left (StoreDataError err)
    Right (Right value) -> Right value

data SkillRow = SkillRow
    { skillRowId :: !Text
    , skillRowScopeKind :: !Text
    , skillRowScopeId :: !Text
    , skillRowSlug :: !Text
    , skillRowTitle :: !Text
    , skillRowDescription :: !Text
    , skillRowAppliesWhen :: !Text
    , skillRowInstructions :: !Text
    , skillRowActivation :: !Text
    , skillRowPriority :: !Int32
    , skillRowStatus :: !Text
    , skillRowRevision :: !Int64
    , skillRowCreatedAt :: !UTCTime
    , skillRowUpdatedAt :: !UTCTime
    }

data RevisionRow = RevisionRow
    { revisionRowId :: !Text
    , revisionRowNumber :: !Int64
    , revisionRowTitle :: !Text
    , revisionRowDescription :: !Text
    , revisionRowAppliesWhen :: !Text
    , revisionRowInstructions :: !Text
    , revisionRowActivation :: !Text
    , revisionRowPriority :: !Int32
    , revisionRowStatus :: !Text
    , revisionRowSummary :: !Text
    , revisionRowCreatedAt :: !UTCTime
    }

decodeSkillRow :: SkillRow -> Either Text LearnedSkill
decodeSkillRow row = do
    kind <- scopeKindFromText row.skillRowScopeKind
    scopeId <- mkScopeId row.skillRowScopeId
    activation <- activationFromText row.skillRowActivation
    status <- statusFromText row.skillRowStatus
    pure LearnedSkill
        { learnedSkillId = row.skillRowId
        , learnedSkillScope = Scope kind scopeId
        , learnedSkillSlug = row.skillRowSlug
        , learnedSkillTitle = row.skillRowTitle
        , learnedSkillDescription = row.skillRowDescription
        , learnedSkillAppliesWhen = row.skillRowAppliesWhen
        , learnedSkillInstructions = row.skillRowInstructions
        , learnedSkillActivation = activation
        , learnedSkillPriority = row.skillRowPriority
        , learnedSkillStatus = status
        , learnedSkillRevision = row.skillRowRevision
        , learnedSkillCreatedAt = row.skillRowCreatedAt
        , learnedSkillUpdatedAt = row.skillRowUpdatedAt
        }

decodeRevisionRow :: RevisionRow -> Either Text LearnedSkillRevision
decodeRevisionRow row = do
    activation <- activationFromText row.revisionRowActivation
    status <- statusFromText row.revisionRowStatus
    pure LearnedSkillRevision
        { learnedSkillRevisionId = row.revisionRowId
        , learnedSkillRevisionNumber = row.revisionRowNumber
        , learnedSkillRevisionTitle = row.revisionRowTitle
        , learnedSkillRevisionDescription = row.revisionRowDescription
        , learnedSkillRevisionAppliesWhen = row.revisionRowAppliesWhen
        , learnedSkillRevisionInstructions = row.revisionRowInstructions
        , learnedSkillRevisionActivation = activation
        , learnedSkillRevisionPriority = row.revisionRowPriority
        , learnedSkillRevisionStatus = status
        , learnedSkillRevisionSummary = row.revisionRowSummary
        , learnedSkillRevisionCreatedAt = row.revisionRowCreatedAt
        }

scopeKindFromText :: Text -> Either Text ScopeKind
scopeKindFromText = \case
    "user" -> Right UserScope
    "repository" -> Right RepositoryScope
    "checkout" -> Right CheckoutScope
    value -> Left ("unknown learned skill scope kind: " <> value)

activationFromText :: Text -> Either Text LearnedSkillActivation
activationFromText = \case
    "always" -> Right SkillAlways
    "relevant" -> Right SkillRelevant
    "manual" -> Right SkillManual
    value -> Left ("unknown learned skill activation mode: " <> value)

statusFromText :: Text -> Either Text LearnedSkillStatus
statusFromText = \case
    "active" -> Right SkillActive
    "archived" -> Right SkillArchived
    value -> Left ("unknown learned skill status: " <> value)

decodeMaybeSkillResult
    :: Either StoreError (Maybe SkillRow)
    -> Either StoreError (Maybe LearnedSkill)
decodeMaybeSkillResult = \case
    Left err -> Left err
    Right Nothing -> Right Nothing
    Right (Just row) ->
        either (Left . StoreDataError) (Right . Just) (decodeSkillRow row)

decodeSkillListResult
    :: Either StoreError [SkillRow]
    -> Either StoreError [LearnedSkill]
decodeSkillListResult = \case
    Left err -> Left err
    Right rows ->
        either (Left . StoreDataError) Right (traverse decodeSkillRow rows)

decodeRevisionListResult
    :: Either StoreError [RevisionRow]
    -> Either StoreError [LearnedSkillRevision]
decodeRevisionListResult = \case
    Left err -> Left err
    Right rows ->
        either (Left . StoreDataError) Right (traverse decodeRevisionRow rows)

decodeMaybeRevisionResult
    :: Either StoreError (Maybe RevisionRow)
    -> Either StoreError (Maybe LearnedSkillRevision)
decodeMaybeRevisionResult = \case
    Left err -> Left err
    Right Nothing -> Right Nothing
    Right (Just row) ->
        either (Left . StoreDataError) (Right . Just) (decodeRevisionRow row)

decodeSearchResult
    :: Either StoreError [(SkillRow, Double)]
    -> Either StoreError [LearnedSkillSearchResult]
decodeSearchResult = \case
    Left err -> Left err
    Right rows ->
        either (Left . StoreDataError) Right $
            traverse
                (\(row, rank) ->
                    LearnedSkillSearchResult <$> decodeSkillRow row <*> pure rank)
                rows

data ApplicableScopes = ApplicableScopes
    { applicableUserScopeId :: !Text
    , applicableRepositoryScopeId :: !Text
    , applicableCheckoutScopeId :: !Text
    }

applicableScopes :: [Scope] -> Either Text ApplicableScopes
applicableScopes scopes = ApplicableScopes
    <$> findScope UserScope
    <*> findScope RepositoryScope
    <*> findScope CheckoutScope
  where
    findScope kind =
        case [scopeIdText scope.scopeId | scope <- scopes, scope.scopeKind == kind] of
            [value] -> Right value
            [] -> Left ("missing " <> scopeKindText kind <> " learned skill scope")
            _ -> Left ("duplicate " <> scopeKindText kind <> " learned skill scope")

data SkillSearchParams = SkillSearchParams
    { skillSearchScopes :: !ApplicableScopes
    , skillSearchQuery :: !Text
    , skillSearchLimit :: !Int64
    }

data SkillListParams = SkillListParams
    { skillListScopes :: !ApplicableScopes
    , skillListKind :: !(Maybe Text)
    , skillListLimit :: !Int64
    }

insertSkillStatement :: Statement LearnedSkillCreate (Maybe Text)
insertSkillStatement = mkStatement
    "INSERT INTO harness.skills\
    \ (scope_kind, scope_id, slug, title, description, applies_when,\
    \ instructions_text, activation_mode, priority, status,\
    \ current_revision, created_at, updated_at)\
    \ VALUES ($1, $2::uuid, $3, $4, $5, $6, $7, $8, $9, $10, 1, $11, $11)\
    \ ON CONFLICT (scope_kind, scope_id, slug) DO NOTHING\
    \ RETURNING skill_id::text"
    ( (scopeKindText . (.scopeKind) . (.learnedSkillCreateScope) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (scopeIdText . (.scopeId) . (.learnedSkillCreateScope) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.learnedSkillCreateSlug) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.learnedSkillCreateTitle) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.learnedSkillCreateDescription) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.learnedSkillCreateAppliesWhen) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.learnedSkillCreateInstructions) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (learnedSkillActivationText . (.learnedSkillCreateActivation)
            >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.learnedSkillCreatePriority) >$< Encoders.param (Encoders.nonNullable Encoders.int4))
        <> (learnedSkillStatusText . (.learnedSkillCreateStatus) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.learnedSkillCreateAt) >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz))
    )
    (Decoders.rowMaybe (Decoders.column (Decoders.nonNullable Decoders.text)))
    True

readRevisionStatement
    :: Statement (Scope, Text, Int64) (Maybe RevisionRow)
readRevisionStatement = mkStatement
    ("SELECT r.skill_revision_id::text, r.revision_number, r.title,\
    \ r.description, r.applies_when, r.instructions_text,\
    \ r.activation_mode, r.priority, r.status, r.change_summary, r.created_at\
    \ FROM harness.skill_revisions r\
    \ JOIN harness.skills s ON s.skill_id = r.skill_id\
    \ WHERE s.scope_kind = $1 AND s.scope_id = $2::uuid AND s.slug = $3\
    \   AND r.revision_number = $4")
    ( (scopeKindText . (.scopeKind) . first4
        >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (scopeIdText . (.scopeId) . first4
            >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (second4
            >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (third4
            >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    )
    (Decoders.rowMaybe revisionRowDecoder)
    True
  where
    first4 (scope, _, _) = scope
    second4 (_, slug, _) = slug
    third4 (_, _, revision) = revision

listRevisionsLimitedStatement
    :: Statement (Scope, Text, Int64) [RevisionRow]
listRevisionsLimitedStatement = mkStatement
    ("SELECT r.skill_revision_id::text, r.revision_number, r.title,\
    \ r.description, r.applies_when, r.instructions_text,\
    \ r.activation_mode, r.priority, r.status, r.change_summary, r.created_at\
    \ FROM harness.skill_revisions r\
    \ JOIN harness.skills s ON s.skill_id = r.skill_id\
    \ WHERE s.scope_kind = $1 AND s.scope_id = $2::uuid AND s.slug = $3\
    \ ORDER BY r.revision_number DESC LIMIT $4")
    ( (scopeKindText . (.scopeKind) . first3
        >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (scopeIdText . (.scopeId) . first3
            >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (second3
            >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (third3
            >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    )
    (Decoders.rowList revisionRowDecoder)
    True
  where
    first3 (scope, _, _) = scope
    second3 (_, slug, _) = slug
    third3 (_, _, limit) = limit

listAllSkillsLimitedStatement :: Statement SkillListParams [SkillRow]
listAllSkillsLimitedStatement = mkStatement
    (skillSelectSql
        <> applicableWhereSql
        <> " AND ($4::text IS NULL OR scope_kind = $4)\
           \ ORDER BY scope_kind, title, slug LIMIT $5")
    ( ((.applicableUserScopeId) . (.skillListScopes)
        >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.applicableRepositoryScopeId) . (.skillListScopes)
            >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.applicableCheckoutScopeId) . (.skillListScopes)
            >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.skillListKind)
            >$< Encoders.param (Encoders.nullable Encoders.text))
        <> ((.skillListLimit)
            >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    )
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

updateSkillStatement :: Statement LearnedSkill ()
updateSkillStatement = mkStatement
    "UPDATE harness.skills SET\
    \ title = $2, description = $3, applies_when = $4,\
    \ instructions_text = $5, activation_mode = $6, priority = $7,\
    \ status = $8, current_revision = $9, updated_at = $10\
    \ WHERE skill_id = $1::uuid"
    ( ((.learnedSkillId) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.learnedSkillTitle) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.learnedSkillDescription) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.learnedSkillAppliesWhen) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.learnedSkillInstructions) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (learnedSkillActivationText . (.learnedSkillActivation) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.learnedSkillPriority) >$< Encoders.param (Encoders.nonNullable Encoders.int4))
        <> (learnedSkillStatusText . (.learnedSkillStatus) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.learnedSkillRevision) >$< Encoders.param (Encoders.nonNullable Encoders.int8))
        <> ((.learnedSkillUpdatedAt) >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz))
    )
    Decoders.noResult
    True

insertRevisionStatement :: Statement (LearnedSkill, Text) Text
insertRevisionStatement = mkStatement
    "INSERT INTO harness.skill_revisions\
    \ (skill_id, revision_number, title, description, applies_when,\
    \ instructions_text, activation_mode, priority, status, change_summary,\
    \ created_at)\
    \ VALUES ($1::uuid, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11)\
    \ RETURNING skill_revision_id::text"
    ( ((.learnedSkillId) . fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.learnedSkillRevision) . fst >$< Encoders.param (Encoders.nonNullable Encoders.int8))
        <> ((.learnedSkillTitle) . fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.learnedSkillDescription) . fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.learnedSkillAppliesWhen) . fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.learnedSkillInstructions) . fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (learnedSkillActivationText . (.learnedSkillActivation) . fst
            >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.learnedSkillPriority) . fst >$< Encoders.param (Encoders.nonNullable Encoders.int4))
        <> (learnedSkillStatusText . (.learnedSkillStatus) . fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.learnedSkillUpdatedAt) . fst >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz))
    )
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))
    True

insertSourceStatement
    :: Statement (Text, Int64, LearnedSkillSourceInput, UTCTime) ()
insertSourceStatement = mkStatement
    "INSERT INTO harness.skill_sources\
    \ (skill_revision_id, source_session_key, source_turn_index,\
    \ source_response_item_id, evidence_text, created_at)\
    \ VALUES ($1::uuid, $2, $3, $4, $5, $6)"
    ( (first4 >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (sourceField (.learnedSkillSourceInputSessionKey)
            >$< Encoders.param (Encoders.nullable Encoders.text))
        <> (sourceField (.learnedSkillSourceInputTurnIndex)
            >$< Encoders.param (Encoders.nullable Encoders.int8))
        <> (sourceField (.learnedSkillSourceInputResponseItemId)
            >$< Encoders.param (Encoders.nullable Encoders.text))
        <> (sourceField (.learnedSkillSourceInputEvidence) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (fourth4 >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz))
    )
    Decoders.noResult
    True
  where
    first4 (value, _, _, _) = value
    fourth4 (_, _, _, value) = value
    sourceField field (_, _, source, _) = field source

lockSkillStatement :: Statement (Scope, Text) (Maybe SkillRow)
lockSkillStatement = mkStatement
    (skillSelectSql
        <> " WHERE scope_kind = $1 AND scope_id = $2::uuid AND slug = $3\
           \ FOR UPDATE")
    scopeSlugEncoder
    (Decoders.rowMaybe skillRowDecoder)
    True

readSkillStatement :: Statement (Scope, Text) (Maybe SkillRow)
readSkillStatement = mkStatement
    (skillSelectSql
        <> " WHERE scope_kind = $1 AND scope_id = $2::uuid AND slug = $3")
    scopeSlugEncoder
    (Decoders.rowMaybe skillRowDecoder)
    True

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

searchSkillsStatement
    :: Statement SkillSearchParams [(SkillRow, Double)]
searchSkillsStatement = mkStatement
    ("WITH query AS (SELECT websearch_to_tsquery('english', $4) AS value) "
        <> skillSelectSqlWithPrefix
        <> ", ts_rank_cd(s.search_vector, query.value)::float8"
        <> " FROM harness.skills s CROSS JOIN query"
        <> applicableWhereSqlWithPrefix
        <> " AND s.status = 'active' AND (\
           \ s.search_vector @@ query.value\
           \ OR s.slug ILIKE '%' || $4 || '%'\
           \ OR s.title ILIKE '%' || $4 || '%'\
           \ OR s.description ILIKE '%' || $4 || '%'\
           \ OR s.applies_when ILIKE '%' || $4 || '%'\
           \ OR s.instructions_text ILIKE '%' || $4 || '%'\
           \ )\
           \ ORDER BY ts_rank_cd(s.search_vector, query.value) DESC,\
           \ s.priority DESC, s.updated_at DESC\
           \ LIMIT $5")
    ( ((.applicableUserScopeId) . (.skillSearchScopes) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.applicableRepositoryScopeId) . (.skillSearchScopes) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.applicableCheckoutScopeId) . (.skillSearchScopes) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.skillSearchQuery) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.skillSearchLimit) >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    )
    (Decoders.rowList ((,) <$> skillRowDecoder <*> doubleColumn))
    True

loadRevisionStatement :: Statement (Text, Int64) (Maybe RevisionRow)
loadRevisionStatement = mkStatement
    (revisionSelectSql
        <> " WHERE skill_id = $1::uuid AND revision_number = $2")
    ( (fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    )
    (Decoders.rowMaybe revisionRowDecoder)
    True

listRevisionsStatement :: Statement (Scope, Text) [RevisionRow]
listRevisionsStatement = mkStatement
    ("SELECT r.skill_revision_id::text, r.revision_number, r.title,\
    \ r.description, r.applies_when, r.instructions_text,\
    \ r.activation_mode, r.priority, r.status, r.change_summary, r.created_at\
    \ FROM harness.skill_revisions r\
    \ JOIN harness.skills s ON s.skill_id = r.skill_id\
    \ WHERE s.scope_kind = $1 AND s.scope_id = $2::uuid AND s.slug = $3\
    \ ORDER BY r.revision_number DESC")
    scopeSlugEncoder
    (Decoders.rowList revisionRowDecoder)
    True

listSourcesStatement
    :: Statement (Scope, Text, Int64) [LearnedSkillSource]
listSourcesStatement = mkStatement
    "SELECT src.skill_source_id::text, r.revision_number,\
    \ src.source_session_key, src.source_turn_index,\
    \ src.source_response_item_id, src.evidence_text, src.created_at\
    \ FROM harness.skill_sources src\
    \ JOIN harness.skill_revisions r\
    \   ON r.skill_revision_id = src.skill_revision_id\
    \ JOIN harness.skills s ON s.skill_id = r.skill_id\
    \ WHERE s.scope_kind = $1 AND s.scope_id = $2::uuid AND s.slug = $3\
    \   AND r.revision_number = $4\
    \ ORDER BY src.created_at, src.skill_source_id"
    ( (scopeKindText . (.scopeKind) . first3 >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (scopeIdText . (.scopeId) . first3 >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (second3 >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (third3 >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    )
    (Decoders.rowList sourceRowDecoder)
    True
  where
    first3 (scope, _, _) = scope
    second3 (_, slug, _) = slug
    third3 (_, _, revision) = revision

scopeSlugEncoder :: Encoders.Params (Scope, Text)
scopeSlugEncoder =
    (scopeKindText . (.scopeKind) . fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (scopeIdText . (.scopeId) . fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.text))

applicableScopesEncoder :: Encoders.Params ApplicableScopes
applicableScopesEncoder =
    ((.applicableUserScopeId) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.applicableRepositoryScopeId) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.applicableCheckoutScopeId) >$< Encoders.param (Encoders.nonNullable Encoders.text))

skillSelectSql :: Text
skillSelectSql =
    "SELECT skill_id::text, scope_kind, scope_id::text, slug, title,\
    \ description, applies_when, instructions_text, activation_mode,\
    \ priority, status, current_revision, created_at, updated_at\
    \ FROM harness.skills"

skillSelectSqlWithPrefix :: Text
skillSelectSqlWithPrefix =
    "SELECT s.skill_id::text, s.scope_kind, s.scope_id::text, s.slug, s.title,\
    \ s.description, s.applies_when, s.instructions_text, s.activation_mode,\
    \ s.priority, s.status, s.current_revision, s.created_at, s.updated_at"

revisionSelectSql :: Text
revisionSelectSql =
    "SELECT skill_revision_id::text, revision_number, title, description,\
    \ applies_when, instructions_text, activation_mode, priority, status,\
    \ change_summary, created_at FROM harness.skill_revisions"

applicableWhereSql :: Text
applicableWhereSql =
    " WHERE (\
    \ (scope_kind = 'user' AND scope_id = $1::uuid)\
    \ OR (scope_kind = 'repository' AND scope_id = $2::uuid)\
    \ OR (scope_kind = 'checkout' AND scope_id = $3::uuid)\
    \ )"

applicableWhereSqlWithPrefix :: Text
applicableWhereSqlWithPrefix =
    " WHERE (\
    \ (s.scope_kind = 'user' AND s.scope_id = $1::uuid)\
    \ OR (s.scope_kind = 'repository' AND s.scope_id = $2::uuid)\
    \ OR (s.scope_kind = 'checkout' AND s.scope_id = $3::uuid)\
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

sourceRowDecoder :: Decoders.Row LearnedSkillSource
sourceRowDecoder =
    LearnedSkillSource
        <$> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.int8)
        <*> Decoders.column (Decoders.nullable Decoders.text)
        <*> Decoders.column (Decoders.nullable Decoders.int8)
        <*> Decoders.column (Decoders.nullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.timestamptz)

doubleColumn :: Decoders.Row Double
doubleColumn = Decoders.column (Decoders.nonNullable Decoders.float8)
