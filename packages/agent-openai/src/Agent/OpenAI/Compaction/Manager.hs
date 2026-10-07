-- | Compaction lifecycle independent of the application's persistence model.
module Agent.OpenAI.Compaction.Manager
    ( CompactionSource(..)
    , CompactionStrategy(..)
    , prepareCompaction
    ) where

import Control.Monad (unless, when)
import Data.Text (Text)

-- | Loading freezes a source boundary. Installation must atomically publish
-- its replacement and invalidate any persisted provider continuation.
data CompactionSource item = CompactionSource
    { history :: [item]
    , install :: [item] -> IO ()
    }

-- | Applications select a strategy, not a second compaction lifecycle.
-- The strategy receives observed history only. Pending items are supplied
-- solely to the result validator, never to the compression operation.
data CompactionStrategy request item response = CompactionStrategy
    { estimateRequest :: request -> Int
    , estimateItem :: item -> Int
    , contextWindow :: request -> Int
    , requestItems :: request -> [item]
    , replaceItems :: request -> [item] -> request
    , compressHistory :: request -> [item] -> (request -> IO response) -> IO response
    , replacementHistory :: Int -> request -> [item] -> [item] -> response -> IO [item]
    }

-- | Prepare one submission, installing a validated checkpoint before returning
-- a request that uses it. Exceptions leave pending-input acknowledgement to
-- the caller's normal submission lifecycle; compaction never acknowledges it.
prepareCompaction
    :: CompactionStrategy request item response
    -> Int -> Maybe Int -> Maybe Text -> request -> [item]
    -> IO (CompactionSource item)
    -> (request -> IO response)
    -> IO (request, Maybe Text)
prepareCompaction strategy threshold occupancy previous request pending loadSource send = do
    when (threshold <= 0) $ fail "Compaction: threshold must be positive"
    case (previous, projectedTokens) of
        (Just _, Just tokens) | tokens < effectiveThreshold -> requireFits request >> pure (request, previous)
        _ -> do
            source <- loadSource
            let fullRequest = strategy.replaceItems request (source.history <> pending)
                fullTokens = strategy.estimateRequest fullRequest
                required = maybe fullTokens (max fullTokens) projectedTokens >= effectiveThreshold
            if not required || null source.history
                then requireFits fullRequest >> pure (request, previous)
                else do
                    response <- strategy.compressHistory
                        (strategy.replaceItems request []) source.history send
                    replacement <- strategy.replacementHistory effectiveThreshold request pending source.history response
                    when (null replacement) $ fail "Compaction: empty replacement context"
                    let nextRequest = strategy.replaceItems request (replacement <> pending)
                    requireFits nextRequest
                    source.install replacement
                    pure (nextRequest, Nothing)
  where
    effectiveThreshold = min threshold (strategy.contextWindow request)
    projectedTokens = case previous of
        Nothing -> Just (strategy.estimateRequest request)
        Just _ -> (+ sum (map strategy.estimateItem (strategy.requestItems request))) <$> occupancy
    requireFits candidate =
        unless (strategy.estimateRequest candidate <= strategy.contextWindow candidate) $
            fail "Compaction: current input exceeds the model context window"
