-- | Adapter from learned-skill tools to the Hasql-backed PostgreSQL store.
module Agent.CLI.LearnedSkills.Store
    ( learnedSkillToolsEnvForStore
    , loadApplicableLearnedSkillsForStore
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
import Data.Aeson (Value, object, (.=))
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
                        object ["matches" .= map searchResultValue results])
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
                                    Right $ object
                                        [ "skill" .= skillValue skill
                                        , "selected_revision" .=
                                            revisionDetailValue selectedRevision
                                        , "sources" .= map sourceValue sources
                                        , "revisions" .=
                                            map revisionValue revisions
                                        ]

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

mutationResultValue :: LearnedSkillMutationResult -> Either Text Value
mutationResultValue = \case
    LearnedSkillMutationApplied skill ->
        Right $ object
            [ "status" .= ("applied" :: Text)
            , "skill" .= skillValue skill
            ]
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

searchResultValue :: LearnedSkillSearchResult -> Value
searchResultValue result = object
    [ "skill" .= skillSummaryValue result.learnedSkillSearchSkill
    , "rank" .= result.learnedSkillSearchRank
    ]

skillSummaryValue :: LearnedSkill -> Value
skillSummaryValue skill = object
    [ "scope" .= scopeKindText skill.learnedSkillScope.scopeKind
    , "slug" .= skill.learnedSkillSlug
    , "title" .= skill.learnedSkillTitle
    , "description" .= skill.learnedSkillDescription
    , "applies_when" .= skill.learnedSkillAppliesWhen
    , "activation" .=
        learnedSkillActivationText skill.learnedSkillActivation
    , "priority" .= skill.learnedSkillPriority
    , "status" .= learnedSkillStatusText skill.learnedSkillStatus
    , "revision" .= skill.learnedSkillRevision
    , "updated_at" .= skill.learnedSkillUpdatedAt
    ]

skillValue :: LearnedSkill -> Value
skillValue skill = object
    [ "scope" .= scopeKindText skill.learnedSkillScope.scopeKind
    , "slug" .= skill.learnedSkillSlug
    , "title" .= skill.learnedSkillTitle
    , "description" .= skill.learnedSkillDescription
    , "applies_when" .= skill.learnedSkillAppliesWhen
    , "instructions" .= skill.learnedSkillInstructions
    , "activation" .=
        learnedSkillActivationText skill.learnedSkillActivation
    , "priority" .= skill.learnedSkillPriority
    , "status" .= learnedSkillStatusText skill.learnedSkillStatus
    , "revision" .= skill.learnedSkillRevision
    , "created_at" .= skill.learnedSkillCreatedAt
    , "updated_at" .= skill.learnedSkillUpdatedAt
    ]

revisionValue :: LearnedSkillRevision -> Value
revisionValue revision = object
    [ "revision" .= revision.learnedSkillRevisionNumber
    , "title" .= revision.learnedSkillRevisionTitle
    , "description" .= revision.learnedSkillRevisionDescription
    , "applies_when" .= revision.learnedSkillRevisionAppliesWhen
    , "activation" .=
        learnedSkillActivationText
            revision.learnedSkillRevisionActivation
    , "priority" .= revision.learnedSkillRevisionPriority
    , "status" .=
        learnedSkillStatusText revision.learnedSkillRevisionStatus
    , "change_summary" .= revision.learnedSkillRevisionSummary
    , "created_at" .= revision.learnedSkillRevisionCreatedAt
    ]

revisionDetailValue :: LearnedSkillRevision -> Value
revisionDetailValue revision = object
    [ "revision" .= revision.learnedSkillRevisionNumber
    , "title" .= revision.learnedSkillRevisionTitle
    , "description" .= revision.learnedSkillRevisionDescription
    , "applies_when" .= revision.learnedSkillRevisionAppliesWhen
    , "instructions" .= revision.learnedSkillRevisionInstructions
    , "activation" .=
        learnedSkillActivationText
            revision.learnedSkillRevisionActivation
    , "priority" .= revision.learnedSkillRevisionPriority
    , "status" .=
        learnedSkillStatusText revision.learnedSkillRevisionStatus
    , "change_summary" .= revision.learnedSkillRevisionSummary
    , "created_at" .= revision.learnedSkillRevisionCreatedAt
    ]

sourceValue :: LearnedSkillSource -> Value
sourceValue source = object
    [ "revision" .= source.learnedSkillSourceRevision
    , "session_id" .= source.learnedSkillSourceSessionKey
    , "turn_index" .= source.learnedSkillSourceTurnIndex
    , "response_item_id" .= source.learnedSkillSourceResponseItemId
    , "evidence" .= source.learnedSkillSourceEvidence
    , "created_at" .= source.learnedSkillSourceCreatedAt
    ]
