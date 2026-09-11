-- | Scoped delegation worker. The supplied handler must submit through the
-- application's normal coding engine, never execute tools in the voice layer.
module Agent.OpenAI.Live.Delegation (withLiveDelegation) where

import Agent.OpenAI.Live
import Control.Concurrent.Async (race)
import Control.Concurrent.STM
import Control.Exception.Safe (tryAny)
import Control.Monad (forever, unless, when)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

-- | One coding task runs at a time, with at most eight pending tasks. Duplicate
-- identifiers are ignored for the entire call. Overflow fails closed rather
-- than silently dropping work or blocking the audio receive loop. The caller
-- supplies a bounded outbound sink and a nonblocking UI/audio event callback.
--
-- The handler receives task text and a progress callback and returns a final
-- speakable result. Both are correlated with the original delegation ID. Its
-- lifetime is scoped to the call; cancellation must detach/cancel through the
-- coding engine's own lifecycle policy. Raw exceptions are never sent upstream.
withLiveDelegation
    :: (Text -> (Text -> IO ()) -> IO Text)
    -> (LiveInput -> IO ())
    -> (LiveEvent -> IO ())
    -> ((LiveEvent -> IO ()) -> IO a)
    -> IO a
withLiveDelegation delegate send onEvent conversation = do
    pending <- newTBQueueIO 8
    seen <- newTVarIO Set.empty
    let receive event = case event of
            LiveDelegation identifier task -> do
                unless (Text.length identifier <= 512 && Text.length task <= 16_384)
                    (fail "Voice delegation exceeds size limit")
                accepted <- atomically do
                    identifiers <- readTVar seen
                    if Set.member identifier identifiers then pure False else do
                        full <- isFullTBQueue pending
                        when (full || Set.size identifiers >= 4096)
                            (throwSTM (userError "Voice delegation capacity exceeded"))
                        writeTVar seen (Set.insert identifier identifiers)
                        writeTBQueue pending (identifier, task)
                        pure True
                when accepted (onEvent event)
            _ -> onEvent event
        worker = forever do
            (identifier, task) <- atomically (readTBQueue pending)
            let report channel content = send (LiveContext (Just identifier) channel content)
            result <- tryAny (delegate task (report Commentary))
            report Speakable $ case result of
                Right output -> output
                Left _ -> "The coding task failed. Check the application for details."
    race worker (conversation receive) >>= \case
        Right value -> pure value
        Left () -> fail "Voice delegation worker stopped unexpectedly"
