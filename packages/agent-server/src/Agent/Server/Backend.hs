-- | Transport-independent durable/session operations used by the HTTP layer.
--
-- Every REST callback is run through 'backendAdmitBoundary'. Production keeps
-- the gateway credential read lease for that entire callback. Long-running
-- turns use 'backendTurnBoundaryGuard'; SSE uses
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
        (GatewayBoundary -> IO value) ->
        IO (Either ApiError value)
    , backendContinueBoundary
        :: forall value.
        GatewayBoundary ->
        IO value ->
        IO (Either ApiError value)
    , backendTurnBoundaryGuard :: !TurnBoundaryGuard
    , backendCheckReady :: !(IO (Either ApiError ()))
    , backendListModels
        :: !(GatewayBoundary -> IO (Either ApiError Value))
    , backendListSessions
        :: !( GatewayBoundary
            -> SessionArchiveFilter
            -> Maybe Text
            -> Int
            -> IO (Either ApiError Value)
            )
    , backendCreateSession
        :: !( GatewayBoundary
            -> CreateSessionRequest
            -> IO (Either ApiError Value)
            )
    , backendGetSession
        :: !(GatewayBoundary -> Text -> IO (Either ApiError Value))
    , backendPatchSession
        :: !( GatewayBoundary
            -> Text
            -> PatchSessionRequest
            -> IO (Either ApiError Value)
            )
    , backendDeleteSession
        :: !(GatewayBoundary -> Text -> IO (Either ApiError ()))
    , backendSessionHistory
        :: !( GatewayBoundary
            -> Text
            -> Maybe Integer
            -> Int
            -> IO (Either ApiError Value)
            )
    , backendForkSession
        :: !( GatewayBoundary
            -> Text
            -> ForkSessionRequest
            -> IO (Either ApiError Value)
            )
    , backendRunTurn
        :: !(TurnControl -> TurnSpec -> IO (Either Text ()))
    }
