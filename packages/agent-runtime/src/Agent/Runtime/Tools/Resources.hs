-- | Session-owned tool domains. Process services and independent sessions
-- belong to their own owners, not these scopes.
module Agent.Runtime.Tools.Resources
    ( SessionResourceScopes(..)
    , withSessionResourceScopes
    ) where

import Agent.ResourceScope
    ( ResourceScope, closeResourceScopeChecked, newResourceScope )
import Data.Acquire (Acquire, mkAcquire)
import qualified Data.Acquire as Acquire

data SessionResourceScopes = SessionResourceScopes
    { scratchResources :: ResourceScope
    , codingResources :: ResourceScope
    , mcpResources :: ResourceScope
    , webFetchResources :: ResourceScope
    , lspResources :: ResourceScope
    , computerUseResources :: ResourceScope
    , sessionLockResources :: ResourceScope
    , codeModeResources :: ResourceScope
    , activityResources :: ResourceScope
    }

-- | Establish ownership before concurrent acquisition. Completion order must
-- not affect teardown: join activities, stop code mode, release the session
-- lock, close tools, then remove scratch storage. Existing resourcet machinery
-- runs every domain finalizer even when one fails.
withSessionResourceScopes :: (SessionResourceScopes -> IO a) -> IO a
withSessionResourceScopes = Acquire.with acquireSessionResourceScopes

acquireSessionResourceScopes :: Acquire SessionResourceScopes
acquireSessionResourceScopes = do
    scratchResources <- acquireScope
    codingResources <- acquireScope
    mcpResources <- acquireScope
    webFetchResources <- acquireScope
    lspResources <- acquireScope
    computerUseResources <- acquireScope
    sessionLockResources <- acquireScope
    codeModeResources <- acquireScope
    activityResources <- acquireScope
    pure SessionResourceScopes{..}
  where
    acquireScope = mkAcquire newResourceScope closeResourceScopeChecked
