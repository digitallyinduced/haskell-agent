-- | Scoped Telegram progress notifications, independent of session ownership.
module Agent.Telegram.Progress (withTelegramProgressUsing) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Exception.Safe (tryAny)
import Control.Monad (void)

withTelegramProgressUsing :: IO () -> IO () -> IO a -> IO a
withTelegramProgressUsing sendTyping sendDraft action = do
    -- Seed once; the caller owns subsequent live draft updates.
    void (tryAny sendDraft)
    withAsync progressLoop (const action)
  where
    progressLoop = do
        void (tryAny sendTyping)
        threadDelay 4_000_000
        progressLoop
