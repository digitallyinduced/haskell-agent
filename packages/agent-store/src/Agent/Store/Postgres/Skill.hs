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
    , searchLearnedSkills
    , listApplicableLearnedSkills
    , listLearnedSkillRevisions
    , listLearnedSkillSources
    ) where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Hasql.Session as HasqlSession
import qualified Hasql.Transaction as Transaction
import qualified Hasql.Transaction.Sessions as Transactions

import Agent.Store.Postgres.Connection
    ( StorePool
    , withSession
    )
import Agent.Store.Postgres.Scope
    ( Scope(..)
    , ScopeKind(..)
    , mkScopeId
    , scopeIdText
    , scopeKindText
    )
import Agent.Store.Postgres.Skill.Types
import Agent.Store.Postgres.Skill.Mapping.Statements.InsertRevision
import Agent.Store.Postgres.Skill.Mapping.Statements.InsertSkill
import Agent.Store.Postgres.Skill.Mapping.Statements.InsertSource
import Agent.Store.Postgres.Skill.Mapping.Statements.ListRevisions
import Agent.Store.Postgres.Skill.Mapping.Statements.ListSkills
import Agent.Store.Postgres.Skill.Mapping.Statements.ListSources
import Agent.Store.Postgres.Skill.Mapping.Statements.LoadRevision
import Agent.Store.Postgres.Skill.Mapping.Statements.LockSkill
import Agent.Store.Postgres.Skill.Mapping.Statements.ReadSkill
import Agent.Store.Postgres.Skill.Mapping.Statements.SearchSkills
import Agent.Store.Postgres.Skill.Mapping.Statements.UpdateSkill
import Agent.Store.Postgres.Skill.Mapping.Types
import Agent.Store.Types (StoreError(..))

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
        inserted <- Transaction.statement
            (insertSkillParams input)
            insertSkillStatement
        case inserted of
            Nothing -> pure (Right LearnedSkillMutationAlreadyExists)
            Just skillId -> do
                let skill = skillFromCreate skillId input
                revisionId <- Transaction.statement
                    (insertRevisionParams skill input.learnedSkillCreateSummary)
                    insertRevisionStatement
                Transaction.statement
                    (insertSourceParams
                        revisionId
                        input.learnedSkillCreateSource
                        input.learnedSkillCreateAt)
                    insertSourceStatement
                pure (Right (LearnedSkillMutationApplied skill))

updateLearnedSkill
    :: StorePool
    -> LearnedSkillUpdate
    -> IO (Either StoreError LearnedSkillMutationResult)
updateLearnedSkill pool input =
    runSkillWrite pool do
        currentRow <- Transaction.statement
            (scopeSlugParams
                input.learnedSkillUpdateScope
                input.learnedSkillUpdateSlug)
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
                                Transaction.statement
                                    (updateSkillParams updated)
                                    updateSkillStatement
                                revisionId <- Transaction.statement
                                    (insertRevisionParams
                                        updated
                                        input.learnedSkillUpdateSummary)
                                    insertRevisionStatement
                                Transaction.statement
                                    (insertSourceParams
                                        revisionId
                                        input.learnedSkillUpdateSource
                                        input.learnedSkillUpdateAt)
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
            (scopeSlugParams
                input.learnedSkillRollbackScope
                input.learnedSkillRollbackSlug)
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
                                    Transaction.statement
                                        (updateSkillParams restored)
                                        updateSkillStatement
                                    revisionId <- Transaction.statement
                                        (insertRevisionParams
                                            restored
                                            input.learnedSkillRollbackSummary)
                                        insertRevisionStatement
                                    Transaction.statement
                                        (insertSourceParams
                                            revisionId
                                            input.learnedSkillRollbackSource
                                            input.learnedSkillRollbackAt)
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
        (HasqlSession.statement
            (scopeSlugParams scope slug)
            readSkillStatement)
        >>= pure . decodeMaybeSkillResult

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

listLearnedSkillRevisions
    :: StorePool
    -> Scope
    -> Text
    -> IO (Either StoreError [LearnedSkillRevision])
listLearnedSkillRevisions pool scope slug =
    withSession pool
        (HasqlSession.statement
            (scopeSlugParams scope slug)
            listRevisionsStatement)
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
            (scopeSlugParams scope slug, revision)
            listSourcesStatement)
        >>= pure . fmap (map sourceFromRow)

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

sourceFromRow :: SourceRow -> LearnedSkillSource
sourceFromRow row = LearnedSkillSource
    { learnedSkillSourceId = row.sourceRowId
    , learnedSkillSourceRevision = row.sourceRowRevision
    , learnedSkillSourceSessionKey = row.sourceRowSessionKey
    , learnedSkillSourceTurnIndex = row.sourceRowTurnIndex
    , learnedSkillSourceResponseItemId = row.sourceRowResponseItemId
    , learnedSkillSourceEvidence = row.sourceRowEvidence
    , learnedSkillSourceCreatedAt = row.sourceRowCreatedAt
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

scopeSlugParams :: Scope -> Text -> ScopeSlugParams
scopeSlugParams scope slug = ScopeSlugParams
    { scopeSlugKind = scopeKindText scope.scopeKind
    , scopeSlugId = scopeIdText scope.scopeId
    , scopeSlug = slug
    }

insertSkillParams :: LearnedSkillCreate -> InsertSkillParams
insertSkillParams input = InsertSkillParams
    { insertSkillScopeKind =
        scopeKindText input.learnedSkillCreateScope.scopeKind
    , insertSkillScopeId =
        scopeIdText input.learnedSkillCreateScope.scopeId
    , insertSkillSlug = input.learnedSkillCreateSlug
    , insertSkillTitle = input.learnedSkillCreateTitle
    , insertSkillDescription = input.learnedSkillCreateDescription
    , insertSkillAppliesWhen = input.learnedSkillCreateAppliesWhen
    , insertSkillInstructions = input.learnedSkillCreateInstructions
    , insertSkillActivation =
        learnedSkillActivationText input.learnedSkillCreateActivation
    , insertSkillPriority = input.learnedSkillCreatePriority
    , insertSkillStatus =
        learnedSkillStatusText input.learnedSkillCreateStatus
    , insertSkillAt = input.learnedSkillCreateAt
    }

updateSkillParams :: LearnedSkill -> UpdateSkillParams
updateSkillParams skill = UpdateSkillParams
    { updateSkillId = skill.learnedSkillId
    , updateSkillTitle = skill.learnedSkillTitle
    , updateSkillDescription = skill.learnedSkillDescription
    , updateSkillAppliesWhen = skill.learnedSkillAppliesWhen
    , updateSkillInstructions = skill.learnedSkillInstructions
    , updateSkillActivation =
        learnedSkillActivationText skill.learnedSkillActivation
    , updateSkillPriority = skill.learnedSkillPriority
    , updateSkillStatus = learnedSkillStatusText skill.learnedSkillStatus
    , updateSkillRevision = skill.learnedSkillRevision
    , updateSkillAt = skill.learnedSkillUpdatedAt
    }

insertRevisionParams :: LearnedSkill -> Text -> InsertRevisionParams
insertRevisionParams skill summary = InsertRevisionParams
    { insertRevisionSkillId = skill.learnedSkillId
    , insertRevisionNumber = skill.learnedSkillRevision
    , insertRevisionTitle = skill.learnedSkillTitle
    , insertRevisionDescription = skill.learnedSkillDescription
    , insertRevisionAppliesWhen = skill.learnedSkillAppliesWhen
    , insertRevisionInstructions = skill.learnedSkillInstructions
    , insertRevisionActivation =
        learnedSkillActivationText skill.learnedSkillActivation
    , insertRevisionPriority = skill.learnedSkillPriority
    , insertRevisionStatus =
        learnedSkillStatusText skill.learnedSkillStatus
    , insertRevisionSummary = summary
    , insertRevisionAt = skill.learnedSkillUpdatedAt
    }

insertSourceParams
    :: Text
    -> LearnedSkillSourceInput
    -> UTCTime
    -> InsertSourceParams
insertSourceParams revisionId source occurredAt = InsertSourceParams
    { insertSourceRevisionId = revisionId
    , insertSourceSessionKey =
        source.learnedSkillSourceInputSessionKey
    , insertSourceTurnIndex =
        source.learnedSkillSourceInputTurnIndex
    , insertSourceResponseItemId =
        source.learnedSkillSourceInputResponseItemId
    , insertSourceEvidence = source.learnedSkillSourceInputEvidence
    , insertSourceAt = occurredAt
    }
