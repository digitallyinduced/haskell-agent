-- | Scoped, one-use session controls. A store must commit consumption before
-- returning: an uncertain remote response must never replay an approval.
module Agent.Telegram.Connector.Controls
    ( ControlScope(..), ControlAction(..), PendingControl(..)
    , ControlInvocation(..), ControlStore(..), ControlOutcome(..)
    , validateControl, handleSessionControl
    ) where

import Agent.Telegram.Connector.Session
import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)

data ControlScope = ControlScope
    { controlOwner :: !Text
    , controlChat :: !Integer
    , controlBindingRevision :: !Text
    , controlSession :: !Text
    , controlExecution :: !Text
    , controlTurn :: !Text
    } deriving (Eq, Show)

data ControlAction = RespondToHuman !HumanRequest !Value | CancelSession
    deriving (Eq, Show)

data PendingControl = PendingControl
    { pendingScope :: !ControlScope
    , pendingMessage :: !Integer
    , pendingExpiresAt :: !UTCTime
    , pendingAction :: !ControlAction
    } deriving (Eq, Show)

-- | Scope comes from the currently authorized binding, not callback data.
data ControlInvocation = ControlInvocation
    { invocationToken :: !Text
    , invocationScope :: !ControlScope
    , invocationActor :: !Integer
    , invocationMessage :: !Integer
    } deriving (Eq, Show)

-- | Lock token AND current binding, validate, consume once, then commit before
-- returning. Never invoke remote IO inside a rollbackable inbox transaction.
-- Concurrent callers must receive at most one successful claim.
newtype ControlStore = ControlStore
    { claimControl :: ControlInvocation -> (PendingControl -> Bool) -> IO (Maybe PendingControl) }

data ControlOutcome = ControlApplied | ControlInvalid | ControlBackendFailure !SessionFailure
    deriving (Eq, Show)

validateControl :: UTCTime -> ControlInvocation -> PendingControl -> Bool
validateControl now invocation pending =
    not (Text.null invocation.invocationToken)
    && invocation.invocationScope == pending.pendingScope
    && invocation.invocationActor == pending.pendingScope.controlChat
    && pending.pendingScope.controlChat > 0
    && invocation.invocationMessage == pending.pendingMessage
    && pending.pendingMessage > 0
    && now < pending.pendingExpiresAt
    && all (not . Text.null)
        [ pending.pendingScope.controlOwner, pending.pendingScope.controlBindingRevision
        , pending.pendingScope.controlSession, pending.pendingScope.controlExecution
        , pending.pendingScope.controlTurn ]

handleSessionControl :: ControlStore -> SessionBackend -> UTCTime -> ControlInvocation -> IO ControlOutcome
handleSessionControl store backend now invocation =
    store.claimControl invocation (validateControl now invocation) >>= \case
        Just pending | validateControl now invocation pending -> do
            let scope = pending.pendingScope
                execution = SessionExecution scope.controlExecution scope.controlSession "" (Just scope.controlTurn)
            result <- case pending.pendingAction of
                RespondToHuman request value -> respondToSessionRequest backend execution request value
                CancelSession -> cancelSessionExecution backend execution
            pure $ either ControlBackendFailure (const ControlApplied) result
        _ -> pure ControlInvalid
