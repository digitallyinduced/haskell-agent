{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}

module Agent.Store.Postgres.Skill.Types
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
    , learnedSkillActivationText
    , learnedSkillStatusText
    ) where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

import Agent.Store.Postgres.Scope (Scope)

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

learnedSkillActivationText :: LearnedSkillActivation -> Text
learnedSkillActivationText = \case
    SkillAlways -> "always"
    SkillRelevant -> "relevant"
    SkillManual -> "manual"

learnedSkillStatusText :: LearnedSkillStatus -> Text
learnedSkillStatusText = \case
    SkillActive -> "active"
    SkillArchived -> "archived"

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
