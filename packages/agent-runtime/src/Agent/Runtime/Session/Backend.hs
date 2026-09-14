-- | Frontend-independent provider operations for a live session.
module Agent.Runtime.Session.Backend
    ( SessionBackend(..)
    ) where

import Agent.Loop (Backend)
import Agent.Responses.Types (ResponseCreateParams)

-- | These operations share the provider's resource scope. Neither the backend
-- nor side-question factories may escape the provider continuation.
data SessionBackend = SessionBackend
    { backend :: !Backend
    , btwBackend :: !(ResponseCreateParams -> Backend)
    , interruptBackend :: !(IO ())
    , resetBackendState :: !(IO ())
    }
