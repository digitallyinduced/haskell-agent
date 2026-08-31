module Agent.OpenAI.TestSupport
    ( requireLoopbackListener
    , withLoopbackApplication
    ) where

import Control.Exception (IOException, try)
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import System.IO.Error (isPermissionError)
import Test.Hspec (pendingWith)

-- | Run a loopback-server test when the enclosing sandbox permits an
-- ephemeral listener. Other listener startup errors remain test failures.
withLoopbackApplication application action = do
    requireLoopbackListener
    testWithApplication application action

requireLoopbackListener :: IO ()
requireLoopbackListener = do
    result <- (try $ testWithApplication (pure probeApplication)
        (const $ pure ())) :: IO (Either IOException ())
    case result of
        Right () -> pure ()
        Left err
            | isPermissionError err ->
                pendingWith
                    "the test environment cannot bind a loopback listener"
            | otherwise -> ioError err

probeApplication :: Application
probeApplication _ respond =
    respond $ responseLBS status200 [] ""
