-- | Transport-independent durable/session operations used by the HTTP layer.
--
-- Every REST callback is run through 'backendAdmitBoundary'. Authentication
-- selects the tenant before production acquires the nested gateway credential
-- lease. Long-running turns use 'backendTurnBoundaryGuard'; SSE uses
-- 'backendContinueBoundary' before every write.
module Agent.Server.Backend
    ( Backend(..)
    , SessionMutationLease(..)
    ) where

import Agent.Server.Supervisor
    ( TurnBoundaryGuard
    , TurnControl
    , TurnPersistence
    )
import Agent.Server.Types
import Data.Aeson (Value)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

data SessionMutationLease = SessionMutationLease
    { runSessionMutationLease ::
        forall value. IO value -> IO (Either ApiError value)
    , releaseSessionMutationLease :: IO ()
    }

data Backend = Backend
    { backendAdmitBoundary
        :: forall value.
        Principal ->
        (AccessBoundary -> IO value) ->
        IO (Either ApiError value)
    , backendContinueBoundary
        :: forall value.
        AccessBoundary ->
        IO value ->
        IO (Either ApiError value)
    , backendTurnBoundaryGuard :: !TurnBoundaryGuard
    , backendCheckReady :: !(IO (Either ApiError ()))
    , backendListModels
        :: !(AccessBoundary -> IO (Either ApiError Value))
    , backendListSessions
        :: !( AccessBoundary
            -> SessionArchiveFilter
            -> Maybe Text
            -> Int
            -> IO (Either ApiError Value)
            )
    , backendCreateSession
        :: !( AccessBoundary
            -> CreateSessionRequest
            -> IO (Either ApiError Value)
            )
    , backendGetSession
        :: !(AccessBoundary -> Text -> IO (Either ApiError Value))
    , backendPatchSession
        :: !( AccessBoundary
            -> Text
            -> PatchSessionRequest
            -> IO (Either ApiError Value)
            )
    , backendDeleteSession
        :: !(AccessBoundary -> Text -> IO (Either ApiError ()))
    , backendSessionHistory
        :: !( AccessBoundary
            -> Text
            -> Maybe Integer
            -> Int
            -> IO (Either ApiError Value)
            )
    , backendForkSession
        :: !( AccessBoundary
            -> Text
            -> ForkSessionRequest
            -> IO (Either ApiError Value)
            )
    , backendReserveSessionMutation
        :: !( AccessBoundary
            -> Text
            -> UTCTime
            -> IO (Either ApiError (Maybe SessionMutationLease))
            )
    , backendReserveTurn
        :: !( AccessBoundary
            -> Text
            -> ClientRequestId
            -> Text
            -> TurnId
            -> UTCTime
            -> IO (Either ApiError TurnReservation)
            )
    , backendLookupTurn
        :: !( AccessBoundary
            -> TurnId
            -> IO (Either ApiError (Maybe TurnRecord))
            )
    , backendListTurns
        :: !( AccessBoundary
            -> Maybe Text
            -> IO (Either ApiError [TurnRecord])
            )
    , backendLookupTurnResult
        :: !( AccessBoundary
            -> TurnId
            -> IO (Either ApiError (Maybe TurnResult))
            )
    , backendRequestTurnCancellation
        :: !( AccessBoundary
            -> TurnId
            -> UTCTime
            -> IO (Either ApiError (Maybe (Bool, TurnRecord)))
            )
    , backendTurnPersistence :: !TurnPersistence
    , backendRunTurn
        :: !( TurnControl
            -> TurnSpec
            -> IO (Either Text TurnExecutionOutput)
            )
    }
