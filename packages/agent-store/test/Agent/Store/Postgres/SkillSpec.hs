{-# LANGUAGE OverloadedStrings #-}

module Agent.Store.Postgres.SkillSpec (spec) where

import Control.Exception.Safe (finally)
import qualified Data.ByteString as ByteString
import Data.Time.Clock (UTCTime)
import qualified Data.Text as Text
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Agent.Store.Postgres
    ( ManagedPostgresConfig
    , closeStore
    , defaultManagedPostgresConfig
    , openStore
    , trustedPool
    )
import Agent.Store.Postgres.Connection (StorePool)
import Agent.Store.Postgres.Managed (stopManagedPostgres)
import Agent.Store.Postgres.Scope
    ( Scope(..)
    , ScopeKind(..)
    , mkScopeId
    )
import Agent.Store.Postgres.Session
    ( SessionMetadata(..)
    , createSession
    )
import Agent.Store.Postgres.Skill
import Agent.Store.Types (StoreError(..))

spec :: Spec
spec = describe "PostgreSQL learned skill storage" do
    it "uses explicit current, revision, source, and search tables" do
        let ddl = ByteString.intercalate "\n" learnedSkillSchemaStatements
        ddl `shouldContainBytes` "CREATE TABLE IF NOT EXISTS harness.skills"
        ddl `shouldContainBytes`
            "CREATE TABLE IF NOT EXISTS harness.skill_revisions"
        ddl `shouldContainBytes`
            "CREATE TABLE IF NOT EXISTS harness.skill_sources"
        ddl `shouldContainBytes` "instructions_text text NOT NULL"
        ddl `shouldContainBytes` "search_vector tsvector GENERATED ALWAYS"
        ddl `shouldContainBytes` "USING gin (search_vector)"
        ddl `shouldContainBytes` "skill_revisions_immutable"
        ddl `shouldContainBytes` "skill_sources_immutable"
        ddl `shouldContainBytes` "ON DELETE RESTRICT"
        ddl `shouldNotContainBytes` "'draft'"
        ddl `shouldNotContainBytes` "jsonb"

    it "creates, searches, updates, archives, and rolls back scoped skills" $
        withSystemTempDirectory "ha" \stateDirectory -> do
            let
                config = defaultManagedPostgresConfig stateDirectory ""
                cleanup = do
                    _ <- stopManagedPostgres config
                    pure ()
            withOpenStore config exerciseStore `finally` cleanup

    it "keeps learned skills available after the store is reopened" $
        withSystemTempDirectory "ha" \stateDirectory -> do
            let
                config = defaultManagedPostgresConfig stateDirectory ""
                scope = testScope UserScope '4'
                now = read "2026-08-24 14:10:00 UTC"
                cleanup = do
                    _ <- stopManagedPostgres config
                    pure ()
                createInput = LearnedSkillCreate
                    { learnedSkillCreateScope = scope
                    , learnedSkillCreateSlug = "persistent-session-lesson"
                    , learnedSkillCreateTitle = "Persistent session lesson"
                    , learnedSkillCreateDescription =
                        "A reusable instruction loaded by later sessions."
                    , learnedSkillCreateAppliesWhen =
                        "Starting another agent session."
                    , learnedSkillCreateInstructions =
                        "Retain explicit learned skills across store reopen."
                    , learnedSkillCreateActivation = SkillAlways
                    , learnedSkillCreatePriority = 5
                    , learnedSkillCreateStatus = SkillActive
                    , learnedSkillCreateSummary = "Learn across sessions"
                    , learnedSkillCreateSource = LearnedSkillSourceInput
                        { learnedSkillSourceInputSessionKey =
                            Just "reserved-session-id"
                        , learnedSkillSourceInputTurnIndex = Nothing
                        , learnedSkillSourceInputResponseItemId = Nothing
                        , learnedSkillSourceInputEvidence =
                            "The user requested memory over multiple sessions."
                        }
                    , learnedSkillCreateAt = now
                    }
            ( (withOpenStore config \pool -> do
                    created <- createLearnedSkill pool createInput
                    _ <- expectApplied created
                    pure ())
                >> withOpenStore config \pool ->
                    fmap
                        (fmap (map (.learnedSkillSlug)))
                        (listApplicableLearnedSkills
                            pool
                            [ scope
                            , testScope RepositoryScope '5'
                            , testScope CheckoutScope '6'
                            ])
                        `shouldReturn`
                            Right ["persistent-session-lesson"]
                ) `finally` cleanup

exerciseStore :: StorePool -> IO ()
exerciseStore pool = do
    let
        now = read "2026-08-24 14:00:00 UTC"
        later = read "2026-08-24 14:01:00 UTC"
        latest = read "2026-08-24 14:02:00 UTC"
        rolledBackAt = read "2026-08-24 14:03:00 UTC"
        userScope = testScope UserScope '1'
        repositoryScope = testScope RepositoryScope '2'
        checkoutScope = testScope CheckoutScope '3'
        scopes = [userScope, repositoryScope, checkoutScope]
        source evidence = LearnedSkillSourceInput
            { learnedSkillSourceInputSessionKey = Just "skill-session"
            , learnedSkillSourceInputTurnIndex = Just 0
            , learnedSkillSourceInputResponseItemId = Nothing
            , learnedSkillSourceInputEvidence = evidence
            }
        createInput = LearnedSkillCreate
            { learnedSkillCreateScope = repositoryScope
            , learnedSkillCreateSlug = "postgres-session-storage"
            , learnedSkillCreateTitle = "PostgreSQL session storage"
            , learnedSkillCreateDescription =
                "How to change durable session persistence."
            , learnedSkillCreateAppliesWhen =
                "Changing session schemas, migrations, or retrieval."
            , learnedSkillCreateInstructions =
                "Use explicit tables. Store tool outputs as text."
            , learnedSkillCreateActivation = SkillAlways
            , learnedSkillCreatePriority = 10
            , learnedSkillCreateStatus = SkillActive
            , learnedSkillCreateSummary = "Initial learned skill"
            , learnedSkillCreateSource =
                source "The user requested explicit tables and text outputs."
            , learnedSkillCreateAt = now
            }
    createSession pool (testMetadata now) `shouldReturn` Right True
    created <- createLearnedSkill pool createInput
    createdSkill <- expectApplied created
    createdSkill.learnedSkillRevision `shouldBe` 1
    createLearnedSkill pool createInput
        `shouldReturn` Right LearnedSkillMutationAlreadyExists

    readLearnedSkill pool repositoryScope "postgres-session-storage"
        `shouldReturn` Right (Just createdSkill)
    searchLearnedSkills pool scopes "tool outputs text" 10 >>= \case
        Right [match] -> do
            match.learnedSkillSearchSkill.learnedSkillSlug
                `shouldBe` "postgres-session-storage"
            match.learnedSkillSearchRank `shouldSatisfy` (>= 0)
        other -> expectationFailure ("unexpected skill search: " <> show other)

    let updateInput = LearnedSkillUpdate
            { learnedSkillUpdateScope = repositoryScope
            , learnedSkillUpdateSlug = "postgres-session-storage"
            , learnedSkillUpdateExpectedRevision = 1
            , learnedSkillUpdatePatch = emptyPatch
                { learnedSkillPatchInstructions =
                    Just
                        "Use explicit tables. Store tool outputs as text. Test /new."
                , learnedSkillPatchActivation = Just SkillRelevant
                }
            , learnedSkillUpdateSummary = "Add live session validation"
            , learnedSkillUpdateSource =
                source "A live test found a timestamp race in /new."
            , learnedSkillUpdateAt = later
            }
    updated <- updateLearnedSkill pool updateInput
    updatedSkill <- expectApplied updated
    updatedSkill.learnedSkillRevision `shouldBe` 2
    updatedSkill.learnedSkillActivation `shouldBe` SkillRelevant
    updatedSkill.learnedSkillInstructions `shouldContainText` "Test /new"

    updateLearnedSkill pool updateInput
        { learnedSkillUpdateExpectedRevision = 2
        , learnedSkillUpdatePatch = emptyPatch
            { learnedSkillPatchInstructions =
                Just updatedSkill.learnedSkillInstructions
            , learnedSkillPatchActivation =
                Just updatedSkill.learnedSkillActivation
            }
        , learnedSkillUpdateSummary = "Repeat the same values"
        , learnedSkillUpdateSource =
            source "No stored field actually changed."
        , learnedSkillUpdateAt = latest
        }
        `shouldReturn`
            Left
                (StoreDataError
                    "learned skill update does not change any stored field")

    updateLearnedSkill pool updateInput
        `shouldReturn` Right (LearnedSkillMutationConflict 2)
    listLearnedSkillRevisions
        pool repositoryScope "postgres-session-storage" >>= \case
            Right revisions ->
                map (.learnedSkillRevisionNumber) revisions `shouldBe` [2, 1]
            other ->
                expectationFailure ("unexpected skill revisions: " <> show other)
    listLearnedSkillSources
        pool repositoryScope "postgres-session-storage" 2 >>= \case
            Right [storedSource] -> do
                storedSource.learnedSkillSourceSessionKey
                    `shouldBe` Just "skill-session"
                storedSource.learnedSkillSourceEvidence
                    `shouldContainText` "timestamp race"
            other ->
                expectationFailure ("unexpected skill sources: " <> show other)

    archived <- archiveLearnedSkill
        pool repositoryScope "postgres-session-storage" 2
        "Archive temporarily"
        (source "The procedure is temporarily inactive.")
        latest
    archivedSkill <- expectApplied archived
    archivedSkill.learnedSkillRevision `shouldBe` 3
    archivedSkill.learnedSkillStatus `shouldBe` SkillArchived
    fmap
        (fmap (map (.learnedSkillSlug)))
        (listApplicableLearnedSkills pool scopes)
        `shouldReturn` Right []

    rollbackLearnedSkill pool LearnedSkillRollback
        { learnedSkillRollbackScope = repositoryScope
        , learnedSkillRollbackSlug = "postgres-session-storage"
        , learnedSkillRollbackExpectedRevision = 3
        , learnedSkillRollbackTargetRevision = 3
        , learnedSkillRollbackSummary = "Invalid current revision rollback"
        , learnedSkillRollbackSource =
            source "The target must be an earlier immutable revision."
        , learnedSkillRollbackAt = rolledBackAt
        }
        `shouldReturn`
            Left
                (StoreDataError
                    "learned skill rollback target must be earlier than the current revision")

    restored <- rollbackLearnedSkill pool LearnedSkillRollback
        { learnedSkillRollbackScope = repositoryScope
        , learnedSkillRollbackSlug = "postgres-session-storage"
        , learnedSkillRollbackExpectedRevision = 3
        , learnedSkillRollbackTargetRevision = 2
        , learnedSkillRollbackSummary = "Restore revision 2"
        , learnedSkillRollbackSource =
            source "The repository still uses this procedure."
        , learnedSkillRollbackAt = rolledBackAt
        }
    restoredSkill <- expectApplied restored
    restoredSkill.learnedSkillRevision `shouldBe` 4
    restoredSkill.learnedSkillStatus `shouldBe` SkillActive
    restoredSkill.learnedSkillInstructions
        `shouldBe` updatedSkill.learnedSkillInstructions
    fmap
        (fmap (map (.learnedSkillSlug)))
        (listApplicableLearnedSkills pool scopes)
        `shouldReturn` Right ["postgres-session-storage"]

withOpenStore
    :: ManagedPostgresConfig
    -> (StorePool -> IO a)
    -> IO a
withOpenStore config action =
    openStore config >>= \case
        Left err -> do
            expectationFailure ("could not open store: " <> show err)
            fail "could not open store"
        Right store ->
            action (trustedPool store) `finally` closeStore store

expectApplied
    :: Show error
    => Either error LearnedSkillMutationResult
    -> IO LearnedSkill
expectApplied = \case
    Right (LearnedSkillMutationApplied skill) -> pure skill
    other -> do
        expectationFailure ("expected applied skill mutation: " <> show other)
        fail "expected applied skill mutation"

testScope :: ScopeKind -> Char -> Scope
testScope kind digit =
    Scope kind $
        either (error . Text.unpack) id $
            mkScopeId (Text.replicate 32 (Text.singleton digit))

emptyPatch :: LearnedSkillPatch
emptyPatch = LearnedSkillPatch
    { learnedSkillPatchTitle = Nothing
    , learnedSkillPatchDescription = Nothing
    , learnedSkillPatchAppliesWhen = Nothing
    , learnedSkillPatchInstructions = Nothing
    , learnedSkillPatchActivation = Nothing
    , learnedSkillPatchPriority = Nothing
    , learnedSkillPatchStatus = Nothing
    }

testMetadata :: UTCTime -> SessionMetadata
testMetadata now = SessionMetadata
    { sessionMetadataKey = "skill-session"
    , sessionMetadataVersion = 1
    , sessionMetadataCreatedAt = now
    , sessionMetadataUpdatedAt = now
    , sessionMetadataProvider = "openai"
    , sessionMetadataConnection = "openai"
    , sessionMetadataModel = "gpt-test"
    , sessionMetadataTransportModel = Just "gpt-test"
    , sessionMetadataDialect = "openai"
    , sessionMetadataLegacyTarget = Nothing
    , sessionMetadataCwd = "/tmp/project"
    , sessionMetadataEffort = "medium"
    , sessionMetadataTitle = "skill test"
    , sessionMetadataTitleIsManual = False
    , sessionMetadataTitleRefreshIndex = 0
    , sessionMetadataTitleUserTurns = 0
    , sessionMetadataLastResponseId = Nothing
    , sessionMetadataInputTokens = 0
    , sessionMetadataOutputTokens = 0
    , sessionMetadataCachedTokens = 0
    }

shouldContainBytes :: ByteString.ByteString -> ByteString.ByteString -> Expectation
shouldContainBytes actual expected =
    actual `shouldSatisfy` ByteString.isInfixOf expected

shouldNotContainBytes :: ByteString.ByteString -> ByteString.ByteString -> Expectation
shouldNotContainBytes actual expected =
    actual `shouldSatisfy` (not . ByteString.isInfixOf expected)

shouldContainText :: Text.Text -> Text.Text -> Expectation
shouldContainText actual expected =
    actual `shouldSatisfy` Text.isInfixOf expected
