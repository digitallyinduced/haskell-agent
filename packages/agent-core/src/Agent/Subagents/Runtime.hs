{-# LANGUAGE BlockArguments #-}
-- | A scoped runtime for agent trees whose children outlive individual root
-- turns. The runtime owns scheduling and shutdown; callers only borrow a tree
-- and supply persistence/ownership callbacks.
module Agent.Subagents.Runtime
    ( SubagentRuntime
    , RuntimeHandle (..)
    , RuntimeHooks (..)
    , withSubagentRuntime
    , withSubagentRuntimeHandle
    ) where

import Agent.Subagents (SubagentConfig)
import Agent.Subagents.Runtime.Internal
import Control.Exception.Safe (SomeException)
import Data.Text (Text)
import System.OsPath (OsPath)

-- | Scope this around the host's agent loop, not around one model response.
-- Returning or throwing stops the scheduler and joins all children before
-- releasing their ownership callbacks. Sleeping children retain their durable
-- suspension state for recovery in a subsequent runtime.
withSubagentRuntime
    :: SubagentConfig
    -> OsPath
    -> (Text -> SomeException -> IO ())
    -> (SubagentRuntime -> IO a)
    -> IO a
withSubagentRuntime = withSubagentRuntimeInterval 10000000
