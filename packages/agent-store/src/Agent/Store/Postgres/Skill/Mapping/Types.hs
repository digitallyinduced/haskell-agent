{-# LANGUAGE NoFieldSelectors #-}

module Agent.Store.Postgres.Skill.Mapping.Types
    ( SkillRow(..)
    , RevisionRow(..)
    , SourceRow(..)
    , ScopeSlugParams(..)
    , ApplicableScopes(..)
    , SkillSearchParams(..)
    , SkillListParams(..)
    , InsertSkillParams(..)
    , UpdateSkillParams(..)
    , InsertRevisionParams(..)
    , InsertSourceParams(..)
    ) where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

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

data SourceRow = SourceRow
    { sourceRowId :: !Text
    , sourceRowRevision :: !Int64
    , sourceRowSessionKey :: !(Maybe Text)
    , sourceRowTurnIndex :: !(Maybe Int64)
    , sourceRowResponseItemId :: !(Maybe Text)
    , sourceRowEvidence :: !Text
    , sourceRowCreatedAt :: !UTCTime
    }

data ScopeSlugParams = ScopeSlugParams
    { scopeSlugKind :: !Text
    , scopeSlugId :: !Text
    , scopeSlug :: !Text
    }

data ApplicableScopes = ApplicableScopes
    { applicableUserScopeId :: !Text
    , applicableRepositoryScopeId :: !Text
    , applicableCheckoutScopeId :: !Text
    }

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

data InsertSkillParams = InsertSkillParams
    { insertSkillScopeKind :: !Text
    , insertSkillScopeId :: !Text
    , insertSkillSlug :: !Text
    , insertSkillTitle :: !Text
    , insertSkillDescription :: !Text
    , insertSkillAppliesWhen :: !Text
    , insertSkillInstructions :: !Text
    , insertSkillActivation :: !Text
    , insertSkillPriority :: !Int32
    , insertSkillStatus :: !Text
    , insertSkillAt :: !UTCTime
    }

data UpdateSkillParams = UpdateSkillParams
    { updateSkillId :: !Text
    , updateSkillTitle :: !Text
    , updateSkillDescription :: !Text
    , updateSkillAppliesWhen :: !Text
    , updateSkillInstructions :: !Text
    , updateSkillActivation :: !Text
    , updateSkillPriority :: !Int32
    , updateSkillStatus :: !Text
    , updateSkillRevision :: !Int64
    , updateSkillAt :: !UTCTime
    }

data InsertRevisionParams = InsertRevisionParams
    { insertRevisionSkillId :: !Text
    , insertRevisionNumber :: !Int64
    , insertRevisionTitle :: !Text
    , insertRevisionDescription :: !Text
    , insertRevisionAppliesWhen :: !Text
    , insertRevisionInstructions :: !Text
    , insertRevisionActivation :: !Text
    , insertRevisionPriority :: !Int32
    , insertRevisionStatus :: !Text
    , insertRevisionSummary :: !Text
    , insertRevisionAt :: !UTCTime
    }

data InsertSourceParams = InsertSourceParams
    { insertSourceRevisionId :: !Text
    , insertSourceSessionKey :: !(Maybe Text)
    , insertSourceTurnIndex :: !(Maybe Int64)
    , insertSourceResponseItemId :: !(Maybe Text)
    , insertSourceEvidence :: !Text
    , insertSourceAt :: !UTCTime
    }
