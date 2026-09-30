-- | Demand-driven Responses streaming, separate from transport ownership and
-- the agent's approval, execution, retry and checkpoint state machines.
module Agent.Responses.StreamPipeline
    ( consumeResponsesSse
    , assembleResponseC
    ) where

import Agent.Error (ApiError)
import Agent.Responses.SSE (decodeSseC)
import Agent.Responses.StreamAssembly
    ( StreamAssemblyConfig
    , StreamAssemblyStep(..)
    , emptyStreamAssemblyState
    , finishStreamWithoutTerminal
    , stepStreamResponse
    )
import Agent.Responses.Types (Response, ResponseStreamEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT)
import qualified Data.ByteString as BS
import Data.Conduit (ConduitT, await, runConduit, yield, (.|))
import Data.Text (Text)
import Data.Void (Void)

-- | Consume a body reader whose empty chunk denotes EOF. The caller must keep
-- the transport bracket open for this entire action and enforce read timeouts.
-- No worker or queue is introduced: callback failure/cancellation unwinds the
-- pipeline, and a terminal event stops further body reads. Exceptions are left
-- to the transport's existing classification boundary.
consumeResponsesSse
    :: StreamAssemblyConfig
    -> Maybe Text
    -> IO BS.ByteString
    -> (ResponseStreamEvent -> IO ())
    -> IO (Either ApiError Response)
consumeResponsesSse config modelHint readChunk emit =
    runExceptT $ runConduit $
        sourceChunks .| decodeSseC .| assembleResponseC config modelHint emit
  where
    sourceChunks = do
        chunk <- liftIO readChunk
        unless (BS.null chunk) do
            yield chunk
            sourceChunks

-- | Deliver each event before applying the existing assembly transition,
-- including terminal/error events. Stopping here prevents later callbacks in
-- the same decoded chunk. Tool admission remains the callback owner's job.
assembleResponseC
    :: Monad m
    => StreamAssemblyConfig
    -> Maybe Text
    -> (ResponseStreamEvent -> m ())
    -> ConduitT ResponseStreamEvent Void (ExceptT ApiError m) Response
assembleResponseC config modelHint emit = go emptyStreamAssemblyState
  where
    go state = await >>= \case
        Nothing -> lift (except (finishStreamWithoutTerminal config state))
        Just event -> do
            lift (lift (emit event))
            case stepStreamResponse config modelHint state event of
                StreamFinished result -> lift (except result)
                StreamContinue next -> go next
