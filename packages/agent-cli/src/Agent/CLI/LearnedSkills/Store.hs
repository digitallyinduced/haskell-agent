-- | Adapter from learned-skill tools to the Hasql-backed PostgreSQL store.
module Agent.CLI.LearnedSkills.Store
    ( learnedSkillToolsEnvForStore
    , loadApplicableLearnedSkillsForStore
    , loadLearnedSkillsWithPreload
    , successfulLearnedSkillsPreload
    ) where

import Agent.CLI.Database.Store
    ( DatabaseScopes
    , applicableDatabaseScopes
    , scopeForDatabase
    )
import Agent.CLI.LearnedSkills
    ( LearnedSkillArchiveRequest(..)
    , LearnedSkillCreateRequest(..)
    , LearnedSkillRollbackRequest(..)
    , LearnedSkillToolsEnv(..)
    , LearnedSkillDetails(..)
    , LearnedSkillMutationResponse(..)
    , LearnedSkillRevisionDetails(..)
    , LearnedSkillRevisionSummary(..)
    , LearnedSkillSearchMatch(..)
    , LearnedSkillSearchResponse(..)
    , LearnedSkillSourceDetails(..)
    , LearnedSkillSummary(..)
    , LearnedSkillView(..)
    , LearnedSkillUpdateRequest(..)
    )
import Agent.Store.Postgres
    ( Store
    , trustedPool
    )
import Agent.Store.Postgres.Scope
    ( Scope(..)
    , scopeKindText
    )
import Agent.Store.Postgres.Skill
    ( LearnedSkill(..)
    , LearnedSkillMutationResult(..)
    , LearnedSkillPatch(..)
    , LearnedSkillRevision(..)
    , LearnedSkillRollback(..)
    , LearnedSkillSearchResult(..)
    , LearnedSkillSource(..)
    , LearnedSkillSourceInput(..)
    , LearnedSkillStatus(..)
    , LearnedSkillCreate(..)
    , LearnedSkillUpdate(..)
    , archiveLearnedSkill
    , createLearnedSkill
    , learnedSkillActivationText
    , learnedSkillStatusText
    , listApplicableLearnedSkills
    , listLearnedSkillRevisions
    , listLearnedSkillSources
    , readLearnedSkill
    , rollbackLearnedSkill
    , searchLearnedSkills
    , updateLearnedSkill
    )
import Agent.Store.Types (StoreError, renderStoreError)
import Data.Bifunctor (first)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)

learnedSkillToolsEnvForStore
    :: Store
    -> DatabaseScopes
    -> IO (Maybe Text)
    -- ^ Reserved or persisted root session id, when available.
    -> LearnedSkillToolsEnv
learnedSkillToolsEnvForStore store scopes currentSessionId =
    LearnedSkillToolsEnv
        { learnedSkillSearch = \query limit ->
            storeResultIO
                (searchLearnedSkills pool applicableScopes query limit)
                >>= pure . fmap
                    (\results ->
                        LearnedSkillSearchResponse
                            (map searchResultValue results))
        , learnedSkillRead = \selected slug revision ->
            readSkillValue
                (scopeForDatabase scopes selected)
                slug
                revision
        , learnedSkillCreate = \request -> do
            now <- getCurrentTime
            source <- sourceInput currentSessionId request.createRequestEvidence
            storeResultIO
                (createLearnedSkill pool LearnedSkillCreate
                    { learnedSkillCreateScope =
                        scopeForDatabase scopes request.createRequestScope
                    , learnedSkillCreateSlug = request.createRequestSlug
                    , learnedSkillCreateTitle = request.createRequestTitle
                    , learnedSkillCreateDescription =
                        request.createRequestDescription
                    , learnedSkillCreateAppliesWhen =
                        request.createRequestAppliesWhen
                    , learnedSkillCreateInstructions =
                        request.createRequestInstructions
                    , learnedSkillCreateActivation =
                        request.createRequestActivation
                    , learnedSkillCreatePriority =
                        fromIntegral request.createRequestPriority
                    , learnedSkillCreateStatus = SkillActive
                    , learnedSkillCreateSummary =
                        request.createRequestChangeSummary
                    , learnedSkillCreateSource = source
                    , learnedSkillCreateAt = now
                    })
                >>= pure . (>>= mutationResultValue)
        , learnedSkillUpdate = \request -> do
            now <- getCurrentTime
            source <- sourceInput currentSessionId request.updateRequestEvidence
            storeResultIO
                (updateLearnedSkill pool LearnedSkillUpdate
                    { learnedSkillUpdateScope =
                        scopeForDatabase scopes request.updateRequestScope
                    , learnedSkillUpdateSlug = request.updateRequestSlug
                    , learnedSkillUpdateExpectedRevision =
                        fromIntegral request.updateRequestExpectedRevision
                    , learnedSkillUpdatePatch = LearnedSkillPatch
                        { learnedSkillPatchTitle =
                            request.updateRequestTitle
                        , learnedSkillPatchDescription =
                            request.updateRequestDescription
                        , learnedSkillPatchAppliesWhen =
                            request.updateRequestAppliesWhen
                        , learnedSkillPatchInstructions =
                            request.updateRequestInstructions
                        , learnedSkillPatchActivation =
                            request.updateRequestActivation
                        , learnedSkillPatchPriority =
                            fromIntegral
                                <$> request.updateRequestPriority
                        , learnedSkillPatchStatus = Nothing
                        }
                    , learnedSkillUpdateSummary =
                        request.updateRequestChangeSummary
                    , learnedSkillUpdateSource = source
                    , learnedSkillUpdateAt = now
                    })
                >>= pure . (>>= mutationResultValue)
        , learnedSkillArchive = \request -> do
            now <- getCurrentTime
            source <- sourceInput currentSessionId request.archiveRequestEvidence
            storeResultIO
                (archiveLearnedSkill
                    pool
                    (scopeForDatabase scopes request.archiveRequestScope)
                    request.archiveRequestSlug
                    (fromIntegral request.archiveRequestExpectedRevision)
                    request.archiveRequestChangeSummary
                    source
                    now)
                >>= pure . (>>= mutationResultValue)
        , learnedSkillRollback = \request -> do
            now <- getCurrentTime
            source <- sourceInput currentSessionId request.rollbackRequestEvidence
            storeResultIO
                (rollbackLearnedSkill pool LearnedSkillRollback
                    { learnedSkillRollbackScope =
                        scopeForDatabase scopes request.rollbackRequestScope
                    , learnedSkillRollbackSlug = request.rollbackRequestSlug
                    , learnedSkillRollbackExpectedRevision =
                        fromIntegral request.rollbackRequestExpectedRevision
                    , learnedSkillRollbackTargetRevision =
                        fromIntegral request.rollbackRequestTargetRevision
                    , learnedSkillRollbackSummary =
                        request.rollbackRequestChangeSummary
                    , learnedSkillRollbackSource = source
                    , learnedSkillRollbackAt = now
                    })
                >>= pure . (>>= mutationResultValue)
        }
  where
    pool = trustedPool store
    applicableScopes = applicableDatabaseScopes scopes

    readSkillValue scope slug requestedRevision =
        storeResultIO (readLearnedSkill pool scope slug) >>= \case
            Left err -> pure (Left err)
            Right Nothing ->
                pure
                    (Left
                        ("learned skill not found in "
                            <> scopeKindText scope.scopeKind
                            <> " scope: "
                            <> slug))
            Right (Just skill) -> do
                revisionsResult <-
                    storeResultIO
                        (listLearnedSkillRevisions pool scope slug)
                case revisionsResult of
                    Left err -> pure (Left err)
                    Right revisions ->
                        case selectRevision skill revisions requestedRevision of
                            Left err -> pure (Left err)
                            Right selectedRevision -> do
                                sourcesResult <-
                                    storeResultIO
                                        (listLearnedSkillSources
                                            pool
                                            scope
                                            slug
                                            selectedRevision.learnedSkillRevisionNumber)
                                pure do
                                    sources <- sourcesResult
                                    Right $ LearnedSkillView
                                        (skillValue skill)
                                        (revisionDetailValue selectedRevision)
                                        (map sourceValue sources)
                                        (map revisionValue revisions)

    selectRevision skill revisions requested =
        let revisionNumber =
                maybe
                    skill.learnedSkillRevision
                    fromIntegral
                    requested
        in case find
            ((== revisionNumber) . (.learnedSkillRevisionNumber))
            revisions of
                Just revision -> Right revision
                Nothing ->
                    Left
                        ("learned skill revision not found: "
                            <> Text.pack (show revisionNumber))

loadApplicableLearnedSkillsForStore
    :: Store
    -> DatabaseScopes
    -> IO (Either Text [LearnedSkill])
loadApplicableLearnedSkillsForStore store scopes =
    storeResultIO $
        listApplicableLearnedSkills
            (trustedPool store)
            (applicableDatabaseScopes scopes)

successfulLearnedSkillsPreload
    :: Either Text [LearnedSkill]
    -> Maybe [LearnedSkill]
successfulLearnedSkillsPreload =
    either (const Nothing) Just

loadLearnedSkillsWithPreload
    :: Maybe [LearnedSkill]
    -> IO (Either Text [LearnedSkill])
    -> IO (Either Text [LearnedSkill])
loadLearnedSkillsWithPreload (Just learnedSkills) _ =
    pure (Right learnedSkills)
loadLearnedSkillsWithPreload Nothing reload =
    reload

sourceInput
    :: IO (Maybe Text)
    -> Text
    -> IO LearnedSkillSourceInput
sourceInput currentSessionId evidence = do
    sessionId <- currentSessionId
    pure LearnedSkillSourceInput
        { learnedSkillSourceInputSessionKey = sessionId
        , learnedSkillSourceInputTurnIndex = Nothing
        , learnedSkillSourceInputResponseItemId = Nothing
        , learnedSkillSourceInputEvidence = evidence
        }

storeResultIO
    :: IO (Either StoreError value)
    -> IO (Either Text value)
storeResultIO action =
    first renderStoreError <$> action

mutationResultValue :: LearnedSkillMutationResult
    -> Either Text LearnedSkillMutationResponse
mutationResultValue = \case
    LearnedSkillMutationApplied skill ->
        Right $ LearnedSkillMutationResponse "applied" (skillValue skill)
    LearnedSkillMutationAlreadyExists ->
        Left
            "a learned skill with this scope and slug already exists; read it and use skill_update"
    LearnedSkillMutationNotFound ->
        Left "learned skill not found"
    LearnedSkillMutationConflict currentRevision ->
        Left
            ("learned skill revision conflict; current revision is "
                <> Text.pack (show currentRevision)
                <> ". Read the skill and retry with that expected_revision.")
    LearnedSkillMutationRevisionNotFound ->
        Left "target learned-skill revision not found"

searchResultValue :: LearnedSkillSearchResult -> LearnedSkillSearchMatch
searchResultValue result = LearnedSkillSearchMatch
    (skillSummaryValue result.learnedSkillSearchSkill)
    result.learnedSkillSearchRank

skillSummaryValue :: LearnedSkill -> LearnedSkillSummary
skillSummaryValue skill = LearnedSkillSummary
    (scopeKindText skill.learnedSkillScope.scopeKind)
    skill.learnedSkillSlug
    skill.learnedSkillTitle
    skill.learnedSkillDescription
    skill.learnedSkillAppliesWhen
    (learnedSkillActivationText skill.learnedSkillActivation)
    (fromIntegral skill.learnedSkillPriority)
    (learnedSkillStatusText skill.learnedSkillStatus)
    skill.learnedSkillRevision
    skill.learnedSkillUpdatedAt

skillValue :: LearnedSkill -> LearnedSkillDetails
skillValue skill = LearnedSkillDetails
    (scopeKindText skill.learnedSkillScope.scopeKind)
    skill.learnedSkillSlug
    skill.learnedSkillTitle
    skill.learnedSkillDescription
    skill.learnedSkillAppliesWhen
    skill.learnedSkillInstructions
    (learnedSkillActivationText skill.learnedSkillActivation)
    (fromIntegral skill.learnedSkillPriority)
    (learnedSkillStatusText skill.learnedSkillStatus)
    skill.learnedSkillRevision
    skill.learnedSkillCreatedAt
    skill.learnedSkillUpdatedAt

revisionValue :: LearnedSkillRevision -> LearnedSkillRevisionSummary
revisionValue revision = LearnedSkillRevisionSummary
    revision.learnedSkillRevisionNumber
    revision.learnedSkillRevisionTitle
    revision.learnedSkillRevisionDescription
    revision.learnedSkillRevisionAppliesWhen
    (learnedSkillActivationText revision.learnedSkillRevisionActivation)
    (fromIntegral revision.learnedSkillRevisionPriority)
    (learnedSkillStatusText revision.learnedSkillRevisionStatus)
    revision.learnedSkillRevisionSummary
    revision.learnedSkillRevisionCreatedAt

revisionDetailValue :: LearnedSkillRevision -> LearnedSkillRevisionDetails
revisionDetailValue revision = LearnedSkillRevisionDetails
    revision.learnedSkillRevisionNumber
    revision.learnedSkillRevisionTitle
    revision.learnedSkillRevisionDescription
    revision.learnedSkillRevisionAppliesWhen
    revision.learnedSkillRevisionInstructions
    (learnedSkillActivationText revision.learnedSkillRevisionActivation)
    (fromIntegral revision.learnedSkillRevisionPriority)
    (learnedSkillStatusText revision.learnedSkillRevisionStatus)
    revision.learnedSkillRevisionSummary
    revision.learnedSkillRevisionCreatedAt

sourceValue :: LearnedSkillSource -> LearnedSkillSourceDetails
sourceValue source = LearnedSkillSourceDetails
    source.learnedSkillSourceRevision
    source.learnedSkillSourceSessionKey
    source.learnedSkillSourceTurnIndex
    source.learnedSkillSourceResponseItemId
    source.learnedSkillSourceEvidence
    source.learnedSkillSourceCreatedAt
