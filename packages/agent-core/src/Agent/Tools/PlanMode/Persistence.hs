-- | Versioned, fail-closed persistence for the pure plan tracker.
module Agent.Tools.PlanMode.Persistence
    ( planTrackerStateFileName
    , planTrackerStateFilePath
    , encodePlanTracker
    , decodePlanTracker
    , validatePlanTracker
    , readPlanTrackerState
    , writePlanTrackerState
    ) where

import Agent.FileRetry (retryOnFileBusy, writeLazyFileAtomically)
import qualified Agent.Json.Decode as Json
import Agent.OsPath (toText, unsafeToFilePath)
import Agent.Tools.PlanMode.File (PlanDigest(..))
import Agent.Tools.PlanMode.Tracker
import Control.Exception.Safe
    ( IOException
    , bracket
    , bracketOnError
    , displayException
    , tryIO
    )
import Control.Monad (unless, when)
import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isHexDigit)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import System.Directory.OsPath (createDirectoryIfMissing)
import System.IO (Handle, hClose)
import System.IO.Error (isDoesNotExistError)
import System.OsPath (OsPath, takeDirectory, unsafeEncodeUtf, (</>))
import System.Posix.Files
    ( getSymbolicLinkStatus
    , FileStatus
    , isRegularFile
    , isSymbolicLink
    )
import System.Posix.IO
    ( OpenFileFlags(..)
    , OpenMode(ReadOnly)
    , closeFd
    , defaultFileFlags
    , fdToHandle
    , openFd
    )

planTrackerStateFileName :: OsPath
planTrackerStateFileName = unsafeEncodeUtf ".plan-mode.json"

planTrackerStateFilePath :: OsPath -> OsPath
planTrackerStateFilePath directory =
    directory </> planTrackerStateFileName

encodePlanTracker :: PlanTracker -> Either Text LBS.ByteString
encodePlanTracker tracker = do
    validatePlanTracker tracker
    pure (encode (trackerValue tracker))

decodePlanTracker :: BS.ByteString -> Either Text PlanTracker
decodePlanTracker bytes = do
    tracker <-
        case Json.decodeEither trackerDecoder bytes of
            Left err ->
                Left ("invalid plan-mode state JSON: " <> err.jsonErrorMessage)
            Right value -> Right value
    validatePlanTracker tracker
    pure tracker

validatePlanTracker :: PlanTracker -> Either Text ()
validatePlanTracker tracker = do
    unless (tracker.trackerSchemaVersion == 1) $
        Left
            ("unsupported plan-mode state version: "
                <> Text.pack (show tracker.trackerSchemaVersion))
    validateCounter "revision" tracker.trackerRevision
    validateCounter "generation" tracker.trackerGeneration.unPlanGeneration
    validateCounter "reminder count" tracker.trackerReminderCount
    case (tracker.trackerPhase, tracker.trackerPendingApproval) of
        (TrackerExitPending, Nothing) ->
            Left "exit-pending plan state has no pending approval"
        (TrackerExitPending, Just pending) -> do
            unless
                (pending.pendingPlanGeneration == tracker.trackerGeneration)
                (Left "pending approval generation does not match tracker generation")
            validateDigest pending.pendingPlanDigest
            when (Text.null (Text.strip pending.pendingPlanRequestKey)) $
                Left "pending approval request key is empty"
        (_, Just _) ->
            Left "pending approval is present outside exit-pending state"
        (_, Nothing) -> pure ()
    case tracker.trackerApprovedContinuation of
        Nothing -> pure ()
        Just continuation ->
            validateDigest continuation.approvedPlanDigest

readPlanTrackerState
    :: OsPath
    -> IO (Either Text (Maybe PlanTracker))
readPlanTrackerState directory = do
    let path = planTrackerStateFilePath directory
    inspectStateLeaf path >>= \case
        Left err
            | isDoesNotExistError err -> pure (Right Nothing)
            | otherwise -> pure (Left (stateIoError path err))
        Right status
            | isSymbolicLink status ->
                pure (Left ("refusing symbolic-link plan-mode state: " <> toText path))
            | not (isRegularFile status) ->
                pure (Left ("plan-mode state is not a regular file: " <> toText path))
            | otherwise ->
                readStateBytes path >>= \case
                    Left err -> pure (Left err)
                    Right bytes -> pure (Just <$> decodePlanTracker bytes)

writePlanTrackerState
    :: OsPath
    -> PlanTracker
    -> IO (Either Text ())
writePlanTrackerState directory tracker =
    case encodePlanTracker tracker of
        Left err -> pure (Left err)
        Right bytes -> do
            let path = planTrackerStateFilePath directory
            tryIO (createDirectoryIfMissing True (takeDirectory path)) >>= \case
                Left err -> pure (Left (stateIoError path err))
                Right () ->
                    inspectStateLeaf path >>= \case
                        Right status
                            | isSymbolicLink status ->
                                pure (Left
                                    ("refusing symbolic-link plan-mode state: "
                                        <> toText path))
                            | not (isRegularFile status) ->
                                pure (Left
                                    ("plan-mode state is not a regular file: "
                                        <> toText path))
                        Left err
                            | not (isDoesNotExistError err) ->
                                pure (Left (stateIoError path err))
                        _ ->
                            writeAndVerify path bytes tracker

writeAndVerify
    :: OsPath
    -> LBS.ByteString
    -> PlanTracker
    -> IO (Either Text ())
writeAndVerify path bytes tracker =
    tryIO (writeLazyFileAtomically path 0o600 bytes) >>= \case
        Left err -> pure (Left (stateIoError path err))
        Right () ->
            readPlanTrackerState (takeDirectory path) >>= \case
                Left err -> pure (Left err)
                Right Nothing ->
                    pure (Left
                        ("plan-mode state disappeared after write: "
                            <> toText path))
                Right (Just readback)
                    | readback == tracker -> pure (Right ())
                    | otherwise ->
                        pure (Left
                            ("plan-mode state changed during write verification: "
                                <> toText path))

trackerValue :: PlanTracker -> Value
trackerValue tracker = object
    [ "version" .= tracker.trackerSchemaVersion
    , "phase" .= phaseText tracker.trackerPhase
    , "revision" .= tracker.trackerRevision
    , "generation" .= tracker.trackerGeneration.unPlanGeneration
    , "reminder_count" .= tracker.trackerReminderCount
    , "ever_activated" .= tracker.trackerEverActivated
    , "reentered" .= tracker.trackerReentered
    , "buffered_activation" .= tracker.trackerBufferedActivation
    , "exit_notice_pending" .= tracker.trackerExitNoticePending
    , "pending_approval" .=
        fmap pendingApprovalValue tracker.trackerPendingApproval
    , "approved_continuation" .=
        fmap approvedContinuationValue tracker.trackerApprovedContinuation
    ]

pendingApprovalValue :: PendingPlanApproval -> Value
pendingApprovalValue pending = object
    [ "generation" .= pending.pendingPlanGeneration.unPlanGeneration
    , "request_key" .= pending.pendingPlanRequestKey
    , "digest" .= pending.pendingPlanDigest.unPlanDigest
    ]

approvedContinuationValue :: ApprovedPlanContinuation -> Value
approvedContinuationValue continuation = object
    [ "digest" .= continuation.approvedPlanDigest.unPlanDigest
    , "verification" .= continuation.approvedPlanVerification
    , "continuation" .= continuation.approvedPlanContinuation
    ]

trackerDecoder :: Json.Decoder PlanTracker
trackerDecoder = Json.object do
    trackerSchemaVersion <- Json.atKey "version" Json.int
    trackerPhase <- Json.atKey "phase" Json.text >>= decodePhase
    trackerRevision <- nonNegativeWord "revision"
    trackerGeneration <- PlanGeneration <$> nonNegativeWord "generation"
    trackerReminderCount <- nonNegativeWord "reminder_count"
    trackerEverActivated <- Json.atKey "ever_activated" Json.bool
    trackerReentered <- Json.atKey "reentered" Json.bool
    trackerBufferedActivation <- Json.atKey "buffered_activation" Json.bool
    trackerExitNoticePending <- Json.atKey "exit_notice_pending" Json.bool
    trackerPendingApproval <-
        Json.optionalKey "pending_approval" pendingApprovalDecoder
    trackerApprovedContinuation <-
        Json.optionalKey "approved_continuation" approvedContinuationDecoder
    pure PlanTracker{..}

pendingApprovalDecoder :: Json.Decoder PendingPlanApproval
pendingApprovalDecoder = Json.object do
    pendingPlanGeneration <-
        PlanGeneration <$> nonNegativeWord "generation"
    pendingPlanRequestKey <- Json.atKey "request_key" Json.text
    pendingPlanDigest <- PlanDigest <$> Json.atKey "digest" Json.text
    pure PendingPlanApproval{..}

approvedContinuationDecoder :: Json.Decoder ApprovedPlanContinuation
approvedContinuationDecoder = Json.object do
    approvedPlanDigest <- PlanDigest <$> Json.atKey "digest" Json.text
    approvedPlanVerification <-
        Json.atKey "verification" (Json.list Json.text)
    approvedPlanContinuation <- Json.atKey "continuation" Json.text
    pure ApprovedPlanContinuation{..}

nonNegativeWord :: Text -> Json.FieldsDecoder Word64
nonNegativeWord key = do
    value <- Json.atKey key Json.int
    if value < 0
        then fail (Text.unpack key <> " must not be negative")
        else pure (fromIntegral value)

decodePhase :: Text -> Json.FieldsDecoder PlanTrackerPhase
decodePhase = \case
    "inactive" -> pure TrackerInactive
    "pending" -> pure TrackerPending
    "active" -> pure TrackerActive
    "exit_pending" -> pure TrackerExitPending
    value -> fail ("unknown plan-mode phase: " <> Text.unpack value)

phaseText :: PlanTrackerPhase -> Text
phaseText = \case
    TrackerInactive -> "inactive"
    TrackerPending -> "pending"
    TrackerActive -> "active"
    TrackerExitPending -> "exit_pending"

validateDigest :: PlanDigest -> Either Text ()
validateDigest digest =
    unless
        (Text.length value == 64
            && Text.all isHexDigit value)
        (Left "plan digest must be 64 hexadecimal SHA-256 characters")
  where
    value = digest.unPlanDigest

validateCounter :: Text -> Word64 -> Either Text ()
validateCounter label value =
    when (value > fromIntegral (maxBound :: Int)) $
        Left (label <> " is too large to decode on this platform")

inspectStateLeaf
    :: OsPath
    -> IO (Either IOException FileStatus)
inspectStateLeaf =
    tryIO . getSymbolicLinkStatus . unsafeToFilePath

readStateBytes :: OsPath -> IO (Either Text BS.ByteString)
readStateBytes path =
    tryIO (bracket acquire hClose BS.hGetContents) >>= \case
        Left err -> pure (Left (stateIoError path err))
        Right bytes -> pure (Right bytes)
  where
    acquire :: IO Handle
    acquire =
        bracketOnError
            (retryOnFileBusy
                (openFd
                    (unsafeToFilePath path)
                    ReadOnly
                    defaultFileFlags
                        { nofollow = True
                        , cloexec = True
                        }))
            closeFd
            fdToHandle

stateIoError :: OsPath -> IOException -> Text
stateIoError path err =
    "could not access plan-mode state ("
        <> toText path
        <> "): "
        <> Text.pack (displayException err)
