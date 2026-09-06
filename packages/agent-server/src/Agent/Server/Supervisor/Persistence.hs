-- | Persistence boundary for supervised turns and human-input requests.
--
-- Scheduling and atomic live-state transitions remain owned by the supervisor.
module Agent.Server.Supervisor.Persistence
    ( HumanRequestPersistenceResolution(..)
    , HumanRequestResolutionError(..)
    , HumanRequestCleanup(..)
    , TurnPersistence(..)
    , inMemoryTurnPersistence
    , cancelledRecord
    , terminalRecord
    , boundedSupervisorText
    ) where

import Agent.Server.Types
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime)

data HumanRequestPersistenceResolution
    = HumanRequestResolvedDurably !HumanRequest
    | HumanRequestNotFoundDurably
    | HumanRequestAlreadyResolvedDurably
    | HumanRequestInvalidDecisionDurably
    | HumanRequestLocalOnly
    deriving (Eq, Show)

data HumanRequestResolutionError
    = HumanRequestResolutionNotFound
    | HumanRequestResolutionConflict !Text
    | HumanRequestResolutionStoreUnavailable !Text
    deriving (Eq, Show)

data HumanRequestCleanup
    = HumanRequestAbandoned
    | HumanResponseConsumed
    deriving (Eq, Show)

data TurnPersistence = TurnPersistence
    { turnPersistenceStarted ::
        !(TurnRecord -> UTCTime -> IO (Either Text ()))
    , turnPersistenceTerminal ::
        !( TurnRecord ->
           UTCTime ->
           TurnTerminalOutcome ->
           IO (Either Text TurnRecord)
         )
    , turnPersistenceShouldCancel ::
        !(TurnRecord -> IO (Either Text Bool))
    , turnPersistenceCreateHumanRequest ::
        !(TurnRecord -> HumanRequest -> IO (Either Text ()))
    , turnPersistenceListHumanRequests ::
        !(AccessBoundary -> Maybe TurnId -> IO (Either Text [HumanRequest]))
    , turnPersistenceResolveHumanRequest ::
        !( AccessBoundary ->
           RequestId ->
           HumanResponse ->
           IO (Either Text HumanRequestPersistenceResolution)
         )
    , turnPersistenceLoadHumanResponse ::
        !( TurnRecord ->
           RequestId ->
           IO (Either Text (Maybe HumanResponse))
         )
    , turnPersistenceDeleteHumanRequest ::
        !( TurnRecord ->
           RequestId ->
           HumanRequestCleanup ->
           IO (Either Text ())
         )
    }

inMemoryTurnPersistence :: TurnPersistence
inMemoryTurnPersistence =
    TurnPersistence
        { turnPersistenceStarted = \_ _ -> pure (Right ())
        , turnPersistenceTerminal = \record finishedAt outcome ->
            pure (Right (terminalRecord finishedAt outcome record))
        , turnPersistenceShouldCancel = \_ ->
            pure (Right False)
        , turnPersistenceCreateHumanRequest = \_ _ ->
            pure (Right ())
        , turnPersistenceListHumanRequests = \_ _ ->
            pure (Right [])
        , turnPersistenceResolveHumanRequest = \_ _ _ ->
            pure (Right HumanRequestLocalOnly)
        , turnPersistenceLoadHumanResponse = \_ _ ->
            pure (Right Nothing)
        , turnPersistenceDeleteHumanRequest = \_ _ _ ->
            pure (Right ())
        }

cancelledRecord
    :: UTCTime
    -> TurnRecord
    -> TurnRecord
cancelledRecord now record =
    record
        { turnRecordStatus = TurnCancelled
        , turnRecordFinishedAt = Just now
        , turnRecordError = Nothing
        }

terminalRecord
    :: UTCTime
    -> TurnTerminalOutcome
    -> TurnRecord
    -> TurnRecord
terminalRecord finishedAt outcome record =
    case outcome of
        TurnSucceeded _ ->
            record
                { turnRecordStatus = TurnCompleted
                , turnRecordFinishedAt = Just finishedAt
                , turnRecordError = Nothing
                }
        TurnErrored err ->
            record
                { turnRecordStatus = TurnFailed
                , turnRecordFinishedAt = Just finishedAt
                , turnRecordError = Just (boundedSupervisorText err)
                }
        TurnWasCancelled ->
            cancelledRecord finishedAt record

boundedSupervisorText :: Text -> Text
boundedSupervisorText value
    | Text.length value <= 16384 = value
    | otherwise = Text.take 16383 value <> "…"
