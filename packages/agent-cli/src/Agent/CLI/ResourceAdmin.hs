{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Typed administration of the learned-skill store used as agent memory.
--
-- This module deliberately does not expose source evidence. Native
-- administration records a fixed, non-secret audit source instead.
module Agent.CLI.ResourceAdmin
    ( ResourceScope(..)
    , ResourceSkillDraft(..)
    , ResourceSkill(..)
    , ResourceSkillRevision(..)
    , ResourceAdminError(..)
    , validateResourceSlug
    , validateResourceSummary
    , validateResourceSkillDraft
    , listResourceSkills
    , readResourceSkill
    , createResourceSkill
    , updateResourceSkill
    , archiveResourceSkill
    , restoreResourceSkill
    , rollbackResourceSkill
    , historyResourceSkill
    ) where

import Agent.CLI.Database.Store
    ( DatabaseScopes
    , applicableDatabaseScopes
    , scopeForDatabase
    )
import Agent.CLI.Database (DatabaseScope(..))
import Agent.Store.Postgres (Store, trustedPool)
import Agent.Store.Postgres.Scope
    ( Scope(..)
    , ScopeKind(..)
    )
import Agent.Store.Postgres.Skill
    ( LearnedSkill(..)
    , LearnedSkillActivation(..)
    , LearnedSkillCreate(..)
    , LearnedSkillMutationResult(..)
    , LearnedSkillPatch(..)
    , LearnedSkillRevision(..)
    , LearnedSkillRollback(..)
    , LearnedSkillSourceInput(..)
    , LearnedSkillStatus(..)
    , LearnedSkillUpdate(..)
    , createLearnedSkill
    , listAllLearnedSkillsLimited
    , listLearnedSkillRevisionsLimited
    , readLearnedSkill
    , readLearnedSkillRevision
    , rollbackLearnedSkill
    , updateLearnedSkill
    )
import Agent.Store.Types (StoreError(..))
import Data.Char (isAsciiLower, isDigit)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, getCurrentTime)

data ResourceScope
    = ResourceUserScope
    | ResourceRepositoryScope
    | ResourceCheckoutScope
    deriving (Eq, Ord, Show)

data ResourceSkillDraft = ResourceSkillDraft
    { resourceDraftTitle :: !Text
    , resourceDraftDescription :: !Text
    , resourceDraftAppliesWhen :: !Text
    , resourceDraftInstructions :: !Text
    , resourceDraftActivation :: !LearnedSkillActivation
    , resourceDraftPriority :: !Int32
    }
    deriving (Eq, Show)

data ResourceSkill = ResourceSkill
    { resourceSkillScope :: !ResourceScope
    , resourceSkillSlug :: !Text
    , resourceSkillRevision :: !Int64
    , resourceSkillTitle :: !Text
    , resourceSkillDescription :: !Text
    , resourceSkillAppliesWhen :: !Text
    , resourceSkillInstructions :: !Text
    , resourceSkillActivation :: !LearnedSkillActivation
    , resourceSkillPriority :: !Int32
    , resourceSkillArchived :: !Bool
    , resourceSkillCreatedAt :: !UTCTime
    , resourceSkillUpdatedAt :: !UTCTime
    }
    deriving (Eq, Show)

data ResourceSkillRevision = ResourceSkillRevision
    { resourceRevisionNumber :: !Int64
    , resourceRevisionTitle :: !Text
    , resourceRevisionDescription :: !Text
    , resourceRevisionAppliesWhen :: !Text
    , resourceRevisionInstructions :: !Text
    , resourceRevisionActivation :: !LearnedSkillActivation
    , resourceRevisionPriority :: !Int32
    , resourceRevisionArchived :: !Bool
    , resourceRevisionSummary :: !Text
    , resourceRevisionCreatedAt :: !UTCTime
    }
    deriving (Eq, Show)

data ResourceAdminError
    = ResourceAdminInvalid !Text
    | ResourceAdminNotFound
    | ResourceAdminAlreadyExists
    | ResourceAdminConflict !Int64
    | ResourceAdminRevisionNotFound
    | ResourceAdminUnavailable
    deriving (Eq, Show)

validateResourceSlug :: Text -> Either ResourceAdminError Text
validateResourceSlug slug
    | Text.null slug =
        invalid "slug must not be empty"
    | Text.length slug > 80 =
        invalid "slug must contain at most 80 characters"
    | any invalidSegment (Text.splitOn "-" slug) =
        invalid "slug must use lower-case ASCII words separated by single hyphens"
    | otherwise = Right slug
  where
    invalidSegment segment =
        Text.null segment
            || Text.any (\char -> not (isAsciiLower char || isDigit char)) segment

validateResourceSummary :: Text -> Either ResourceAdminError Text
validateResourceSummary summary =
    validateRequired "change summary" 1000 summary

validateResourceSkillDraft
    :: ResourceSkillDraft
    -> Either ResourceAdminError ResourceSkillDraft
validateResourceSkillDraft draft = do
    title <- validateRequired "title" 200 draft.resourceDraftTitle
    description <-
        validateRequired "description" 1000 draft.resourceDraftDescription
    appliesWhen <-
        validateOptional "applies_when" 2000 draft.resourceDraftAppliesWhen
    instructions <-
        validateRequired "instructions" 30000 draft.resourceDraftInstructions
    if draft.resourceDraftPriority < -100
        || draft.resourceDraftPriority > 100
    then invalid "priority must be between -100 and 100"
    else Right draft
        { resourceDraftTitle = title
        , resourceDraftDescription = description
        , resourceDraftAppliesWhen = appliesWhen
        , resourceDraftInstructions = instructions
        }

listResourceSkills
    :: Store
    -> DatabaseScopes
    -> Maybe ResourceScope
    -> Int
    -> IO (Either ResourceAdminError [ResourceSkill])
listResourceSkills store scopes selected limit
    | limit < 1 || limit > 1000 =
        pure (invalid "list limit must be between 1 and 1000")
    | otherwise =
        fmap
            ( fmap
                ( map skillFromStored
                    . take limit
                    . maybe id
                        (\scope -> filter
                            ((== scope) . resourceScopeFromStored
                                . (.scopeKind) . (.learnedSkillScope)))
                        selected
                )
                . mapStoreError
            ) $
            listAllLearnedSkillsLimited
                (trustedPool store)
                (applicableDatabaseScopes scopes)
                (resourceScopeKind <$> selected)
                limit

readResourceSkill
    :: Store
    -> DatabaseScopes
    -> ResourceScope
    -> Text
    -> Maybe Int64
    -> IO (Either ResourceAdminError ResourceSkill)
readResourceSkill store scopes selected slug requestedRevision =
    case validateKey slug requestedRevision of
        Left err -> pure (Left err)
        Right () -> do
            let scope = selectScope scopes selected
                pool = trustedPool store
            mapStoreError <$> readLearnedSkill pool scope slug >>= \case
                Left err -> pure (Left err)
                Right Nothing -> pure (Left ResourceAdminNotFound)
                Right (Just current) ->
                    case requestedRevision of
                        Nothing -> pure (Right (skillFromStored current))
                        Just revision ->
                            mapStoreError
                                <$> readLearnedSkillRevision
                                    pool scope slug revision
                                >>= pure . (>>= \case
                                    Nothing ->
                                        Left ResourceAdminRevisionNotFound
                                    Just stored ->
                                        Right
                                            (historicalSkill current stored))

createResourceSkill
    :: Store
    -> DatabaseScopes
    -> ResourceScope
    -> Text
    -> ResourceSkillDraft
    -> Text
    -> IO (Either ResourceAdminError ResourceSkill)
createResourceSkill store scopes selected slug rawDraft rawSummary =
    case (,,)
        <$> validateResourceSlug slug
        <*> validateResourceSkillDraft rawDraft
        <*> validateResourceSummary rawSummary of
        Left err -> pure (Left err)
        Right (_, draft, summary) -> do
            now <- getCurrentTime
            finishMutation =<< mapStoreError
                <$> createLearnedSkill
                    (trustedPool store)
                    LearnedSkillCreate
                        { learnedSkillCreateScope = selectScope scopes selected
                        , learnedSkillCreateSlug = slug
                        , learnedSkillCreateTitle = draft.resourceDraftTitle
                        , learnedSkillCreateDescription =
                            draft.resourceDraftDescription
                        , learnedSkillCreateAppliesWhen =
                            draft.resourceDraftAppliesWhen
                        , learnedSkillCreateInstructions =
                            draft.resourceDraftInstructions
                        , learnedSkillCreateActivation =
                            draft.resourceDraftActivation
                        , learnedSkillCreatePriority =
                            draft.resourceDraftPriority
                        , learnedSkillCreateStatus = SkillActive
                        , learnedSkillCreateSummary = summary
                        , learnedSkillCreateSource = nativeAdminSource
                        , learnedSkillCreateAt = now
                        }

updateResourceSkill
    :: Store
    -> DatabaseScopes
    -> ResourceScope
    -> Text
    -> Int64
    -> ResourceSkillDraft
    -> Text
    -> IO (Either ResourceAdminError ResourceSkill)
updateResourceSkill store scopes selected slug expected rawDraft rawSummary =
    case (,,)
        <$> validateMutationKey slug expected
        <*> validateResourceSkillDraft rawDraft
        <*> validateResourceSummary rawSummary of
        Left err -> pure (Left err)
        Right (_, draft, summary) ->
            updateWithPatch store scopes selected slug expected summary
                LearnedSkillPatch
                    { learnedSkillPatchTitle =
                        Just draft.resourceDraftTitle
                    , learnedSkillPatchDescription =
                        Just draft.resourceDraftDescription
                    , learnedSkillPatchAppliesWhen =
                        Just draft.resourceDraftAppliesWhen
                    , learnedSkillPatchInstructions =
                        Just draft.resourceDraftInstructions
                    , learnedSkillPatchActivation =
                        Just draft.resourceDraftActivation
                    , learnedSkillPatchPriority =
                        Just draft.resourceDraftPriority
                    , learnedSkillPatchStatus = Nothing
                    }

archiveResourceSkill
    :: Store -> DatabaseScopes -> ResourceScope -> Text -> Int64 -> Text
    -> IO (Either ResourceAdminError ResourceSkill)
archiveResourceSkill =
    setResourceSkillArchived SkillArchived

restoreResourceSkill
    :: Store -> DatabaseScopes -> ResourceScope -> Text -> Int64 -> Text
    -> IO (Either ResourceAdminError ResourceSkill)
restoreResourceSkill =
    setResourceSkillArchived SkillActive

rollbackResourceSkill
    :: Store
    -> DatabaseScopes
    -> ResourceScope
    -> Text
    -> Int64
    -> Int64
    -> Text
    -> IO (Either ResourceAdminError ResourceSkill)
rollbackResourceSkill store scopes selected slug expected target rawSummary =
    case (,,)
        <$> validateMutationKey slug expected
        <*> validatePositiveRevision "target revision" target
        <*> validateResourceSummary rawSummary of
        Left err -> pure (Left err)
        Right (_, _, summary) -> do
            now <- getCurrentTime
            finishMutation =<< mapStoreError
                <$> rollbackLearnedSkill
                    (trustedPool store)
                    LearnedSkillRollback
                        { learnedSkillRollbackScope =
                            selectScope scopes selected
                        , learnedSkillRollbackSlug = slug
                        , learnedSkillRollbackExpectedRevision = expected
                        , learnedSkillRollbackTargetRevision = target
                        , learnedSkillRollbackSummary = summary
                        , learnedSkillRollbackSource = nativeAdminSource
                        , learnedSkillRollbackAt = now
                        }

historyResourceSkill
    :: Store
    -> DatabaseScopes
    -> ResourceScope
    -> Text
    -> Int
    -> IO (Either ResourceAdminError [ResourceSkillRevision])
historyResourceSkill store scopes selected slug limit
    | limit < 1 || limit > 1000 =
        pure (invalid "history limit must be between 1 and 1000")
    | otherwise = case validateResourceSlug slug of
        Left err -> pure (Left err)
        Right _ -> do
            result <- mapStoreError <$> listLearnedSkillRevisionsLimited
                (trustedPool store)
                (selectScope scopes selected)
                slug
                limit
            pure case result of
                Right [] -> Left ResourceAdminNotFound
                Right revisions ->
                    Right (map revisionFromStored revisions)
                Left err -> Left err

setResourceSkillArchived
    :: LearnedSkillStatus
    -> Store -> DatabaseScopes -> ResourceScope -> Text -> Int64 -> Text
    -> IO (Either ResourceAdminError ResourceSkill)
setResourceSkillArchived status store scopes selected slug expected rawSummary =
    case (,)
        <$> validateMutationKey slug expected
        <*> validateResourceSummary rawSummary of
        Left err -> pure (Left err)
        Right (_, summary) ->
            updateWithPatch store scopes selected slug expected summary
                emptyPatch
                    { learnedSkillPatchStatus = Just status
                    }

updateWithPatch
    :: Store
    -> DatabaseScopes
    -> ResourceScope
    -> Text
    -> Int64
    -> Text
    -> LearnedSkillPatch
    -> IO (Either ResourceAdminError ResourceSkill)
updateWithPatch store scopes selected slug expected summary patch = do
    now <- getCurrentTime
    finishMutation =<< mapStoreError
        <$> updateLearnedSkill
            (trustedPool store)
            LearnedSkillUpdate
                { learnedSkillUpdateScope = selectScope scopes selected
                , learnedSkillUpdateSlug = slug
                , learnedSkillUpdateExpectedRevision = expected
                , learnedSkillUpdatePatch = patch
                , learnedSkillUpdateSummary = summary
                , learnedSkillUpdateSource = nativeAdminSource
                , learnedSkillUpdateAt = now
                }

finishMutation
    :: Either ResourceAdminError LearnedSkillMutationResult
    -> IO (Either ResourceAdminError ResourceSkill)
finishMutation = pure . (>>= \case
    LearnedSkillMutationApplied skill -> Right (skillFromStored skill)
    LearnedSkillMutationAlreadyExists -> Left ResourceAdminAlreadyExists
    LearnedSkillMutationNotFound -> Left ResourceAdminNotFound
    LearnedSkillMutationConflict revision ->
        Left (ResourceAdminConflict revision)
    LearnedSkillMutationRevisionNotFound ->
        Left ResourceAdminRevisionNotFound)

validateKey :: Text -> Maybe Int64 -> Either ResourceAdminError ()
validateKey slug revision = do
    _ <- validateResourceSlug slug
    traverse_ (validatePositiveRevision "revision") revision

validateMutationKey :: Text -> Int64 -> Either ResourceAdminError ()
validateMutationKey slug revision =
    validateKey slug (Just revision)

validatePositiveRevision
    :: Text -> Int64 -> Either ResourceAdminError Int64
validatePositiveRevision label revision
    | revision < 1 = invalid (label <> " must be positive")
    | otherwise = Right revision

validateRequired
    :: Text -> Int -> Text -> Either ResourceAdminError Text
validateRequired label maximum value
    | Text.any (== '\NUL') value =
        invalid (label <> " must not contain NUL")
    | Text.null (Text.strip value) =
        invalid (label <> " must not be empty")
    | Text.length value > maximum =
        invalid
            (label <> " must contain at most "
                <> Text.pack (show maximum) <> " characters")
    | otherwise = Right value

validateOptional
    :: Text -> Int -> Text -> Either ResourceAdminError Text
validateOptional label maximum value
    | Text.any (== '\NUL') value =
        invalid (label <> " must not contain NUL")
    | Text.length value > maximum =
        invalid
            (label <> " must contain at most "
                <> Text.pack (show maximum) <> " characters")
    | otherwise = Right value

invalid :: Text -> Either ResourceAdminError value
invalid = Left . ResourceAdminInvalid

mapStoreError :: Either StoreError value -> Either ResourceAdminError value
mapStoreError = either (Left . mapOne) Right
  where
    mapOne = \case
        StoreDataError message
            | "does not change any stored field" `Text.isInfixOf` message ->
                ResourceAdminInvalid "mutation does not change the resource"
        _ -> ResourceAdminUnavailable

historicalSkill
    :: LearnedSkill
    -> LearnedSkillRevision
    -> ResourceSkill
historicalSkill current revision =
    ResourceSkill
        { resourceSkillScope =
            resourceScopeFromStored current.learnedSkillScope.scopeKind
        , resourceSkillSlug = current.learnedSkillSlug
        , resourceSkillRevision = revision.learnedSkillRevisionNumber
        , resourceSkillTitle = revision.learnedSkillRevisionTitle
        , resourceSkillDescription =
            revision.learnedSkillRevisionDescription
        , resourceSkillAppliesWhen =
            revision.learnedSkillRevisionAppliesWhen
        , resourceSkillInstructions =
            revision.learnedSkillRevisionInstructions
        , resourceSkillActivation =
            revision.learnedSkillRevisionActivation
        , resourceSkillPriority = revision.learnedSkillRevisionPriority
        , resourceSkillArchived =
            revision.learnedSkillRevisionStatus == SkillArchived
        , resourceSkillCreatedAt = current.learnedSkillCreatedAt
        , resourceSkillUpdatedAt = revision.learnedSkillRevisionCreatedAt
        }

skillFromStored :: LearnedSkill -> ResourceSkill
skillFromStored skill = ResourceSkill
    { resourceSkillScope =
        resourceScopeFromStored skill.learnedSkillScope.scopeKind
    , resourceSkillSlug = skill.learnedSkillSlug
    , resourceSkillRevision = skill.learnedSkillRevision
    , resourceSkillTitle = skill.learnedSkillTitle
    , resourceSkillDescription = skill.learnedSkillDescription
    , resourceSkillAppliesWhen = skill.learnedSkillAppliesWhen
    , resourceSkillInstructions = skill.learnedSkillInstructions
    , resourceSkillActivation = skill.learnedSkillActivation
    , resourceSkillPriority = skill.learnedSkillPriority
    , resourceSkillArchived = skill.learnedSkillStatus == SkillArchived
    , resourceSkillCreatedAt = skill.learnedSkillCreatedAt
    , resourceSkillUpdatedAt = skill.learnedSkillUpdatedAt
    }

revisionFromStored :: LearnedSkillRevision -> ResourceSkillRevision
revisionFromStored revision = ResourceSkillRevision
    { resourceRevisionNumber = revision.learnedSkillRevisionNumber
    , resourceRevisionTitle = revision.learnedSkillRevisionTitle
    , resourceRevisionDescription =
        revision.learnedSkillRevisionDescription
    , resourceRevisionAppliesWhen =
        revision.learnedSkillRevisionAppliesWhen
    , resourceRevisionInstructions =
        revision.learnedSkillRevisionInstructions
    , resourceRevisionActivation =
        revision.learnedSkillRevisionActivation
    , resourceRevisionPriority = revision.learnedSkillRevisionPriority
    , resourceRevisionArchived =
        revision.learnedSkillRevisionStatus == SkillArchived
    , resourceRevisionSummary = revision.learnedSkillRevisionSummary
    , resourceRevisionCreatedAt = revision.learnedSkillRevisionCreatedAt
    }

selectScope :: DatabaseScopes -> ResourceScope -> Scope
selectScope scopes = \case
    ResourceUserScope -> scopeForDatabase scopes DatabaseUserScope
    ResourceRepositoryScope ->
        scopeForDatabase scopes DatabaseRepositoryScope
    ResourceCheckoutScope ->
        scopeForDatabase scopes DatabaseCheckoutScope

resourceScopeFromStored :: ScopeKind -> ResourceScope
resourceScopeFromStored = \case
    UserScope -> ResourceUserScope
    RepositoryScope -> ResourceRepositoryScope
    CheckoutScope -> ResourceCheckoutScope

resourceScopeKind :: ResourceScope -> ScopeKind
resourceScopeKind = \case
    ResourceUserScope -> UserScope
    ResourceRepositoryScope -> RepositoryScope
    ResourceCheckoutScope -> CheckoutScope

nativeAdminSource :: LearnedSkillSourceInput
nativeAdminSource = LearnedSkillSourceInput
    { learnedSkillSourceInputSessionKey = Nothing
    , learnedSkillSourceInputTurnIndex = Nothing
    , learnedSkillSourceInputResponseItemId = Nothing
    , learnedSkillSourceInputEvidence =
        "Changed through typed native memory administration."
    }

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

traverse_
    :: Applicative f
    => (a -> f b)
    -> Maybe a
    -> f ()
traverse_ action = maybe (pure ()) ((() <$) . action)
