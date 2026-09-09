-- | Ownership for one invocation of the tool/session runtime, including
-- conversation resets. Process-owned services and independent sessions do
-- not belong here.
module Agent.CLI.Runtime.Orchestration.Tools.Resources
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

-- | Establish ownership before starting concurrent acquisitions. Their
-- completion order cannot change dependency teardown order.
--
-- Acquire unwinds in reverse: join session activities, stop the code-mode
-- dispatcher, release the session lock, close tools, then remove scratch
-- storage. Each domain uses the existing resourcet finalizer machinery;
-- there is no separate cleanup registry or worker supervisor here.
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
