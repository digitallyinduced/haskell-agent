-- | Transport-independent durable/session operations used by the HTTP layer.
--
-- Every REST callback is run through 'backendAdmitBoundary'. Authentication
-- selects the tenant before production acquires the nested gateway credential
-- lease. Long-running turns use 'backendTurnBoundaryGuard'; SSE uses
-- 'backendContinueBoundary' before every write.
module Agent.Server.Backend
    ( Backend(..)
    ) where

import Agent.Server.Supervisor
    ( TurnBoundaryGuard
    , TurnControl
    )
import Agent.Server.Types
import Data.Aeson (Value)
import Data.Text (Text)

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
    , backendRunTurn
        :: !(TurnControl -> TurnSpec -> IO (Either Text ()))
    }
