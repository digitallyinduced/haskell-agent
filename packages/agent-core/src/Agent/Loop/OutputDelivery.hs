-- | Ordered delivery of assistant messages interleaved with tool batches.
module Agent.Loop.OutputDelivery (processOrderedOutput) where

import Control.Monad (unless)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text

-- | 'Just' marks a message boundary, including an empty assistant message.
-- Consecutive calls may execute concurrently inside the host's batch callback;
-- the next message is delivered only after that callback completes.
-- Message persistence belongs to 'deliver', after successful delivery.
processOrderedOutput
    :: (item -> Maybe Text)
    -> (item -> Bool)
    -> (item -> Text -> IO ())
    -> (item -> IO ())
    -> ([item] -> IO [item])
    -> [item]
    -> IO [item]
processOrderedOutput messageText isCall deliver persist execute = go
  where
    go [] = pure []
    go (item : rest)
        | Just text <- messageText item = do
            unless (Text.null (Text.strip text)) (deliver item text)
            go rest
    go items = do
        let (batch, rest) = break (isJust . messageText) items
            calls = filter isCall batch
        mapM_ persist batch
        outputs <- if null calls then pure [] else execute calls
        remaining <- go rest
        pure (outputs <> remaining)
