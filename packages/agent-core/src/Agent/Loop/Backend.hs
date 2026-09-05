{-# LANGUAGE PatternSynonyms #-}

-- | Provider submission and immutable checkpoint contracts.
module Agent.Loop.Backend
    ( Backend(Backend, submitTurn, submitTurnWithCallbacks)
    , BackendCallbacks(..)
    , BackendResult(..)
    , BackendRevision(..)
    , BackendContinuation(..)
    , BackendSnapshot(..)
    , BackendStateStore(..)
    , backendWithCallbacks
    , emptyBackendSnapshot
    , initialBackendSnapshot
    , advanceBackendSnapshot
    , clearBackendContinuation
    , backendContinuationToken
    ) where

import Agent.Error (ApiError)
import Agent.Loop.Input (TurnInput)
import Agent.Loop.Output (LoopEvent, TurnOutput)
import Agent.Responses.Types (ResponseItem)
import Agent.ToolDispatch (ToolCall)
import Data.Text (Text)
import Data.Word (Word64)

data BackendResult = BackendResult
    { backendOutput :: !TurnOutput
    -- | The provider candidate checkpoint. The state store assigns the
    -- authoritative revision when this response is committed.
    , backendState :: !BackendSnapshot
    } deriving (Eq, Show)

newtype BackendRevision = BackendRevision Word64
    deriving (Eq, Ord, Show)

-- | An opaque provider continuation. Namespacing prevents a session token
-- minted by one backend from accidentally being sent to another backend.
data BackendContinuation = BackendContinuation
    { continuationProvider :: !Text
    , continuationToken :: !Text
    } deriving (Eq, Show)

-- | An immutable, atomically publishable backend checkpoint.
data BackendSnapshot = BackendSnapshot
    { backendItems :: ![ResponseItem]
    , backendRevision :: !BackendRevision
    , backendContinuation :: !(Maybe BackendContinuation)
    } deriving (Eq, Show)

emptyBackendSnapshot :: BackendSnapshot
emptyBackendSnapshot = initialBackendSnapshot []

initialBackendSnapshot :: [ResponseItem] -> BackendSnapshot
initialBackendSnapshot items = BackendSnapshot
    { backendItems = items
    , backendRevision = BackendRevision 0
    , backendContinuation = Nothing
    }

-- | Build a provider result from the checkpoint it consumed. State stores
-- still normalize the revision at commit time, so concurrent writers cannot
-- publish duplicate or stale revisions.
advanceBackendSnapshot
    :: BackendSnapshot
    -> [ResponseItem]
    -> Maybe BackendContinuation
    -> BackendSnapshot
advanceBackendSnapshot snapshot items continuation = BackendSnapshot
    { backendItems = items
    , backendRevision = nextBackendRevision snapshot.backendRevision
    , backendContinuation = continuation
    }

clearBackendContinuation :: BackendSnapshot -> BackendSnapshot
clearBackendContinuation snapshot =
    snapshot { backendContinuation = Nothing }

backendContinuationToken :: Text -> BackendSnapshot -> Maybe Text
backendContinuationToken provider snapshot =
    case snapshot.backendContinuation of
        Just BackendContinuation
            { continuationProvider
            , continuationToken
            }
            | continuationProvider == provider -> Just continuationToken
        _ -> Nothing

nextBackendRevision :: BackendRevision -> BackendRevision
nextBackendRevision (BackendRevision revision) =
    BackendRevision (revision + 1)

type LegacySubmitTurn =
    BackendSnapshot
    -- | Legacy unnamespaced continuation for persisted sessions. New
    -- backends should prefer the namespaced token in 'BackendSnapshot'.
    -> Maybe Text
    -> [TurnInput]
    -> (LoopEvent -> IO ())
    -> IO (Either ApiError BackendResult)

type CallbackSubmitTurn =
    BackendSnapshot
    -> Maybe Text
    -> [TurnInput]
    -> BackendCallbacks
    -> IO (Either ApiError BackendResult)

data BackendCallbacks = BackendCallbacks
    { onLoopEvent :: !(LoopEvent -> IO ())
    , onAsyncToolCall :: !(ToolCall -> IO ())
    }

data Backend = BackendInternal
    { submitTurn :: LegacySubmitTurn
    , submitTurnWithCallbacks :: CallbackSubmitTurn
    }

-- | Compatibility constructor for event-only backends.
pattern Backend :: LegacySubmitTurn -> Backend
pattern Backend legacySubmit <- BackendInternal legacySubmit _
  where
    Backend legacySubmit =
        BackendInternal
            legacySubmit
            (\snapshot previous inputs callbacks ->
                legacySubmit snapshot previous inputs callbacks.onLoopEvent)

{-# COMPLETE Backend #-}

-- | Construct a backend that can announce async tool calls while streaming.
backendWithCallbacks :: CallbackSubmitTurn -> Backend
backendWithCallbacks callbackSubmit =
    BackendInternal legacySubmit callbackSubmit
  where
    legacySubmit snapshot previous inputs onEvent =
        callbackSubmit snapshot previous inputs BackendCallbacks
            { onLoopEvent = onEvent
            , onAsyncToolCall = const (pure ())
            }

data BackendStateStore = BackendStateStore
    { readBackendState :: !(IO BackendSnapshot)
      -- | Publish a completed provider response for live observers and later
      -- tool continuations. Higher-level turn policy may still deliberately
      -- roll this state back after cancellation or terminal failure.
      --
      -- The returned snapshot is the authoritative committed value, including
      -- the store-assigned monotonic revision.
    , commitBackendState :: !(BackendSnapshot -> IO BackendSnapshot)
    }
