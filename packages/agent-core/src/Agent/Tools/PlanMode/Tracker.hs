-- | Pure state machine foundations for durable plan-mode tracking.
module Agent.Tools.PlanMode.Tracker
    ( PlanTrackerPhase(..)
    , PlanGeneration(..)
    , PendingPlanApproval(..)
    , ApprovedPlanContinuation(..)
    , PlanApprovalResolution(..)
    , PlanTracker(..)
    , PlanTrackerError(..)
    , initialPlanTracker
    , planTrackerRestrictsWrites
    , planTrackerCurrentRevision
    , planTrackerReminderCount
    , requestPlanActivation
    , activatePlanTracker
    , deactivatePlanTracker
    , beginPlanExit
    , resolvePlanApproval
    , notePlanReminder
    , resetPlanReminders
    , bufferPlanActivation
    , consumeBufferedPlanActivation
    , markPlanContinuationDelivered
    , restorePlanTrackerIfRevision
    , normalizePlanTrackerAfterRestart
    ) where

import Agent.Tools.PlanMode.File (PlanDigest)
import Data.Text (Text)
import Data.Word (Word64)

data PlanTrackerPhase
    = TrackerInactive
    | TrackerPending
    | TrackerActive
    | TrackerExitPending
    deriving (Eq, Ord, Show)

newtype PlanGeneration = PlanGeneration
    { unPlanGeneration :: Word64
    } deriving (Eq, Ord, Show)

data PendingPlanApproval = PendingPlanApproval
    { pendingPlanGeneration :: !PlanGeneration
    , pendingPlanRequestKey :: !Text
    , pendingPlanDigest :: !PlanDigest
    } deriving (Eq, Show)

data ApprovedPlanContinuation = ApprovedPlanContinuation
    { approvedPlanDigest :: !PlanDigest
    , approvedPlanVerification :: ![Text]
    , approvedPlanContinuation :: !Text
    } deriving (Eq, Show)

data PlanApprovalResolution
    = ApprovePlan !ApprovedPlanContinuation
    | RevisePlan
    | AbandonPlan
    deriving (Eq, Show)

-- | This value contains no handles or callbacks and is therefore suitable for
-- a versioned sidecar codec. The codec is intentionally owned by the session
-- layer so it can evolve and validate session-specific correlation fields.
data PlanTracker = PlanTracker
    { trackerSchemaVersion :: !Int
    , trackerPhase :: !PlanTrackerPhase
    , trackerRevision :: !Word64
    , trackerGeneration :: !PlanGeneration
    , trackerReminderCount :: !Word64
    , trackerEverActivated :: !Bool
    , trackerReentered :: !Bool
    , trackerBufferedActivation :: !Bool
    , trackerExitNoticePending :: !Bool
    , trackerPendingApproval :: !(Maybe PendingPlanApproval)
    , trackerApprovedContinuation :: !(Maybe ApprovedPlanContinuation)
    } deriving (Eq, Show)

data PlanTrackerError
    = PlanTrackerNotActive
    | PlanTrackerNoPendingApproval
    | PlanTrackerStaleResolution
    | PlanTrackerRevisionConflict !Word64 !Word64
    deriving (Eq, Show)

initialPlanTracker :: PlanTracker
initialPlanTracker = PlanTracker
    { trackerSchemaVersion = 1
    , trackerPhase = TrackerInactive
    , trackerRevision = 0
    , trackerGeneration = PlanGeneration 0
    , trackerReminderCount = 0
    , trackerEverActivated = False
    , trackerReentered = False
    , trackerBufferedActivation = False
    , trackerExitNoticePending = False
    , trackerPendingApproval = Nothing
    , trackerApprovedContinuation = Nothing
    }

planTrackerRestrictsWrites :: PlanTracker -> Bool
planTrackerRestrictsWrites tracker =
    tracker.trackerPhase `elem` [TrackerActive, TrackerExitPending]

planTrackerCurrentRevision :: PlanTracker -> Word64
planTrackerCurrentRevision = (.trackerRevision)

planTrackerReminderCount :: PlanTracker -> Word64
planTrackerReminderCount = (.trackerReminderCount)

requestPlanActivation :: PlanTracker -> PlanTracker
requestPlanActivation tracker =
    bumpRevision $
        case tracker.trackerPhase of
            TrackerInactive ->
                tracker
                    { trackerPhase = TrackerPending
                    , trackerReminderCount = 0
                    , trackerReentered = tracker.trackerEverActivated
                    }
            TrackerPending -> tracker
            TrackerActive ->
                tracker
                    { trackerReminderCount = 0
                    , trackerReentered = True
                    }
            TrackerExitPending ->
                tracker
                    { trackerPhase = TrackerActive
                    , trackerPendingApproval = Nothing
                    , trackerReminderCount = 0
                    , trackerReentered = True
                    }

activatePlanTracker :: PlanTracker -> PlanTracker
activatePlanTracker tracker =
    bumpRevision tracker
        { trackerPhase = TrackerActive
        , trackerReminderCount = 0
        , trackerEverActivated = True
        , trackerReentered = tracker.trackerEverActivated
        , trackerBufferedActivation = False
        , trackerPendingApproval = Nothing
        }

deactivatePlanTracker :: Bool -> PlanTracker -> PlanTracker
deactivatePlanTracker emitExitNotice tracker =
    bumpRevision tracker
        { trackerPhase = TrackerInactive
        , trackerReminderCount = 0
        , trackerReentered = False
        , trackerBufferedActivation = False
        , trackerExitNoticePending =
            emitExitNotice || tracker.trackerExitNoticePending
        , trackerPendingApproval = Nothing
        }

beginPlanExit
    :: Text
    -> PlanDigest
    -> PlanTracker
    -> Either PlanTrackerError PlanTracker
beginPlanExit requestKey digest tracker
    | tracker.trackerPhase /= TrackerActive =
        Left PlanTrackerNotActive
    | otherwise =
        let generation = successor tracker.trackerGeneration
        in Right $ bumpRevision tracker
            { trackerPhase = TrackerExitPending
            , trackerGeneration = generation
            , trackerPendingApproval = Just PendingPlanApproval
                { pendingPlanGeneration = generation
                , pendingPlanRequestKey = requestKey
                , pendingPlanDigest = digest
                }
            }

resolvePlanApproval
    :: PlanGeneration
    -> PlanDigest
    -> PlanApprovalResolution
    -> PlanTracker
    -> Either PlanTrackerError PlanTracker
resolvePlanApproval generation digest resolution tracker =
    case tracker.trackerPendingApproval of
        Nothing -> Left PlanTrackerNoPendingApproval
        Just pending
            | pending.pendingPlanGeneration /= generation
                || pending.pendingPlanDigest /= digest ->
                Left PlanTrackerStaleResolution
            | otherwise -> Right $
                bumpRevision $ case resolution of
                    ApprovePlan continuation ->
                        tracker
                            { trackerPhase = TrackerInactive
                            , trackerPendingApproval = Nothing
                            , trackerApprovedContinuation = Just continuation
                            , trackerReminderCount = 0
                            , trackerReentered = False
                            }
                    RevisePlan ->
                        tracker
                            { trackerPhase = TrackerActive
                            , trackerPendingApproval = Nothing
                            , trackerReminderCount = 0
                            , trackerReentered = True
                            }
                    AbandonPlan ->
                        tracker
                            { trackerPhase = TrackerInactive
                            , trackerPendingApproval = Nothing
                            , trackerApprovedContinuation = Nothing
                            , trackerReminderCount = 0
                            , trackerReentered = False
                            }

notePlanReminder :: PlanTracker -> PlanTracker
notePlanReminder tracker =
    bumpRevision tracker
        { trackerReminderCount = tracker.trackerReminderCount + 1 }

resetPlanReminders :: PlanTracker -> PlanTracker
resetPlanReminders tracker =
    bumpRevision tracker
        { trackerReminderCount = 0 }

bufferPlanActivation :: PlanTracker -> PlanTracker
bufferPlanActivation tracker =
    bumpRevision tracker
        { trackerBufferedActivation = True }

consumeBufferedPlanActivation :: PlanTracker -> PlanTracker
consumeBufferedPlanActivation tracker =
    bumpRevision tracker
        { trackerBufferedActivation = False }

markPlanContinuationDelivered :: PlanTracker -> PlanTracker
markPlanContinuationDelivered tracker =
    bumpRevision tracker
        { trackerApprovedContinuation = Nothing }

-- | Compare-and-restore a previously captured tracker snapshot. The caller
-- supplies the revision it expects after its own turn-scoped transitions. If
-- a user action or another owner has advanced the tracker, restoration is
-- rejected rather than overwriting the newer state. Generation remains
-- monotonic even when the older snapshot is restored.
restorePlanTrackerIfRevision
    :: Word64
    -> PlanTracker
    -> PlanTracker
    -> Either PlanTrackerError PlanTracker
restorePlanTrackerIfRevision expectedRevision snapshot current
    | current.trackerRevision /= expectedRevision =
        Left
            (PlanTrackerRevisionConflict
                expectedRevision
                current.trackerRevision)
    | otherwise =
        Right snapshot
            { trackerSchemaVersion = current.trackerSchemaVersion
            , trackerRevision = current.trackerRevision + 1
            , trackerGeneration =
                max snapshot.trackerGeneration current.trackerGeneration
            , trackerEverActivated =
                snapshot.trackerEverActivated || current.trackerEverActivated
            }

-- | Restart never relaxes an interrupted approval. A persisted pending
-- activation is kept pending; an exit remains write-restricted and replayable.
normalizePlanTrackerAfterRestart :: PlanTracker -> PlanTracker
normalizePlanTrackerAfterRestart tracker =
    let normalized = tracker
            { trackerBufferedActivation = False
            , trackerExitNoticePending = False
            }
    in if normalized == tracker
        then tracker
        else bumpRevision normalized

bumpRevision :: PlanTracker -> PlanTracker
bumpRevision tracker =
    tracker { trackerRevision = tracker.trackerRevision + 1 }

successor :: PlanGeneration -> PlanGeneration
successor (PlanGeneration generation) = PlanGeneration (generation + 1)
