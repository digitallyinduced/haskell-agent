-- | Concurrent tool acquisition with deterministic session ownership.
-- Frontends supply concrete acquisitions and interaction hooks; this module
-- knows neither terminal state nor CLI options.
module Agent.Runtime.Tools.Startup
    ( ToolAcquisitions(..)
    , ToolStartupResources(..)
    , acquireToolStartup
    ) where

import Agent.ResourceScope (allocateAcquire)
import Agent.Runtime.Tools.Resources (SessionResourceScopes(..))
import Control.Concurrent.Async (Concurrently(..))
import Data.Acquire (Acquire)

-- | Each acquisition must clean up partial initialization on failure. Completed
-- resources are retained by their domain until the enclosing session ends.
-- Context preload must scope any workers/resources internally.
data ToolAcquisitions mcp coding web lsp computer context = ToolAcquisitions
    { acquireMcp :: Acquire mcp
    , acquireCoding :: Acquire coding
    , acquireWebFetch :: Acquire web
    , acquireLsp :: Acquire lsp
    , acquireComputerUse :: Acquire computer
    , preloadContext :: IO context
    }

data ToolStartupResources mcp coding web lsp computer context = ToolStartupResources
    { startupMcp :: mcp
    , startupLocalTools :: coding
    , startupWebFetch :: web
    , startupLsp :: lsp
    , startupComputerUse :: computer
    , startupInitialContext :: context
    }

-- | Run inside 'withSessionResourceScopes'. A failed/cancelled branch cancels
-- and joins its siblings before the caller's scopes unwind. In particular,
-- no acquisition worker can still use scratch storage during its teardown.
acquireToolStartup
    :: SessionResourceScopes
    -> ToolAcquisitions mcp coding web lsp computer context
    -> IO (ToolStartupResources mcp coding web lsp computer context)
acquireToolStartup scopes acquisitions = runConcurrently $
    ToolStartupResources
        <$> owned scopes.mcpResources acquisitions.acquireMcp
        <*> owned scopes.codingResources acquisitions.acquireCoding
        <*> owned scopes.webFetchResources acquisitions.acquireWebFetch
        <*> owned scopes.lspResources acquisitions.acquireLsp
        <*> owned scopes.computerUseResources acquisitions.acquireComputerUse
        <*> Concurrently acquisitions.preloadContext
  where
    owned scope acquisition =
        Concurrently (snd <$> allocateAcquire scope acquisition)
