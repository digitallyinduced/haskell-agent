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
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
    ( ExceptT(..)
    , except
    , runExceptT
    , throwE
    )
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

type ResourceAdmin = ExceptT ResourceAdminError IO

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
listResourceSkills store scopes selected limit = runExceptT do
    _ <- except (validateLimit "list" limit)
    stored <- liftStore $
        listAllLearnedSkillsLimited
            (trustedPool store)
            (applicableDatabaseScopes scopes)
            (resourceScopeKind <$> selected)
            limit
    pure
        ( map skillFromStored
            . take limit
            . maybe id
                (\scope -> filter
                    ((== scope) . resourceScopeFromStored
                        . (.scopeKind) . (.learnedSkillScope)))
                selected
            $ stored
        )

readResourceSkill
    :: Store
    -> DatabaseScopes
    -> ResourceScope
    -> Text
    -> Maybe Int64
    -> IO (Either ResourceAdminError ResourceSkill)
readResourceSkill store scopes selected slug requestedRevision = runExceptT do
    _ <- except (validateKey slug requestedRevision)
    let scope = selectScope scopes selected
        pool = trustedPool store
    current <- require ResourceAdminNotFound
        =<< liftStore (readLearnedSkill pool scope slug)
    case requestedRevision of
        Nothing -> pure (skillFromStored current)
        Just revision -> do
            stored <- require ResourceAdminRevisionNotFound
                =<< liftStore
                    (readLearnedSkillRevision pool scope slug revision)
            pure (historicalSkill current stored)

createResourceSkill
    :: Store
    -> DatabaseScopes
    -> ResourceScope
    -> Text
    -> ResourceSkillDraft
    -> Text
    -> IO (Either ResourceAdminError ResourceSkill)
createResourceSkill store scopes selected slug rawDraft rawSummary =
    runExceptT do
        (_, draft, summary) <- except $
            (,,)
                <$> validateResourceSlug slug
                <*> validateResourceSkillDraft rawDraft
                <*> validateResourceSummary rawSummary
        now <- liftIO getCurrentTime
        result <- liftStore $
            createLearnedSkill
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
        finishMutation result

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
    runExceptT do
        (_, draft, summary) <- except $
            (,,)
                <$> validateMutationKey slug expected
                <*> validateResourceSkillDraft rawDraft
                <*> validateResourceSummary rawSummary
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
archiveResourceSkill store scopes selected slug expected summary =
    runExceptT $
        setResourceSkillArchived
            SkillArchived store scopes selected slug expected summary

restoreResourceSkill
    :: Store -> DatabaseScopes -> ResourceScope -> Text -> Int64 -> Text
    -> IO (Either ResourceAdminError ResourceSkill)
restoreResourceSkill store scopes selected slug expected summary =
    runExceptT $
        setResourceSkillArchived
            SkillActive store scopes selected slug expected summary

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
    runExceptT do
        (_, _, summary) <- except $
            (,,)
                <$> validateMutationKey slug expected
                <*> validatePositiveRevision "target revision" target
                <*> validateResourceSummary rawSummary
        now <- liftIO getCurrentTime
        result <- liftStore $
            rollbackLearnedSkill
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
        finishMutation result

historyResourceSkill
    :: Store
    -> DatabaseScopes
    -> ResourceScope
    -> Text
    -> Int
    -> IO (Either ResourceAdminError [ResourceSkillRevision])
historyResourceSkill store scopes selected slug limit = runExceptT do
    _ <- except $
        (,)
            <$> validateLimit "history" limit
            <*> validateResourceSlug slug
    revisions <- liftStore $
        listLearnedSkillRevisionsLimited
            (trustedPool store)
            (selectScope scopes selected)
            slug
            limit
    case revisions of
        [] -> throwE ResourceAdminNotFound
        _ -> pure (map revisionFromStored revisions)

setResourceSkillArchived
    :: LearnedSkillStatus
    -> Store -> DatabaseScopes -> ResourceScope -> Text -> Int64 -> Text
    -> ResourceAdmin ResourceSkill
setResourceSkillArchived status store scopes selected slug expected rawSummary = do
    (_, summary) <- except $
        (,)
            <$> validateMutationKey slug expected
            <*> validateResourceSummary rawSummary
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
    -> ResourceAdmin ResourceSkill
updateWithPatch store scopes selected slug expected summary patch = do
    now <- liftIO getCurrentTime
    result <- liftStore $
        updateLearnedSkill
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
    finishMutation result

finishMutation
    :: LearnedSkillMutationResult
    -> ResourceAdmin ResourceSkill
finishMutation = \case
    LearnedSkillMutationApplied skill -> pure (skillFromStored skill)
    LearnedSkillMutationAlreadyExists -> throwE ResourceAdminAlreadyExists
    LearnedSkillMutationNotFound -> throwE ResourceAdminNotFound
    LearnedSkillMutationConflict revision ->
        throwE (ResourceAdminConflict revision)
    LearnedSkillMutationRevisionNotFound ->
        throwE ResourceAdminRevisionNotFound

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

validateLimit :: Text -> Int -> Either ResourceAdminError Int
validateLimit label limit
    | limit < 1 || limit > 1000 =
        invalid (label <> " limit must be between 1 and 1000")
    | otherwise = Right limit

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

require :: ResourceAdminError -> Maybe value -> ResourceAdmin value
require err = maybe (throwE err) pure

liftStore :: IO (Either StoreError value) -> ResourceAdmin value
liftStore action = ExceptT (mapStoreError <$> action)

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
