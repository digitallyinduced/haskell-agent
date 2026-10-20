-- | Adapt the shared session input queue to backend submissions.
module Agent.CLI.PendingInputs
    ( PendingInputs
    , PendingNoticeKind(..)
    , newPendingInputs
    , clearPendingInputs
    , enqueuePendingInput
    , enqueuePendingNotice
    , pendingInputByteLimit
    , pendingInputCountLimit
    , withPendingInputs
    ) where

import Agent.Loop
    ( Backend(..)
    , BackendMiddleware
    , backendWithCallbacks
    )
import Agent.Loop.InputQueue
    ( PendingInputs
    , PendingNoticeKind(..)
    , newPendingInputs
    , clearPendingInputs
    , enqueuePendingInput
    , enqueuePendingNotice
    , pendingInputByteLimit
    , pendingInputCountLimit
    , withPendingInputBatch
    )

withPendingInputs :: PendingInputs -> BackendMiddleware
withPendingInputs pending backend =
    backendWithCallbacks \state previous inputs callbacks ->
        withPendingInputBatch pending \queued ->
            backend.submitTurnWithCallbacks state previous (queued <> inputs) callbacks
