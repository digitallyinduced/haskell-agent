-- | Process-local operation generations for managed MCP connections.
-- Acquire this lock before catalog or credential locks. Never retain it while
-- awaiting browser authorization. Generations prevent configuration ABA from
-- reviving a superseded authorization operation.
module Agent.CLI.McpConnectionLifecycle
    ( withMcpConnectionLifecycle
    , advanceMcpConnectionGeneration
    ) where

import Control.Concurrent.MVar
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

connectionLifecycles :: MVar (Map.Map Text (MVar Integer))
connectionLifecycles = unsafePerformIO (newMVar Map.empty)
{-# NOINLINE connectionLifecycles #-}

withMcpConnectionLifecycle :: Text -> (Integer -> IO (Integer, a)) -> IO a
withMcpConnectionLifecycle identifier action = do
    lock <- modifyMVar connectionLifecycles \current ->
        case Map.lookup identifier current of
            Just existing -> pure (current, existing)
            Nothing -> do
                created <- newMVar 0
                pure (Map.insert identifier created current, created)
    modifyMVar lock action

advanceMcpConnectionGeneration :: Text -> IO Integer
advanceMcpConnectionGeneration identifier =
    withMcpConnectionLifecycle identifier \current ->
        pure (current + 1, current + 1)
