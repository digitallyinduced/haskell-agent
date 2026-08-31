module Agent.Responses.TestSupport
    ( requireLoopbackListener
    ) where

import Control.Exception (IOException, try)
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import System.IO.Error (isPermissionError)
import Test.Hspec (pendingWith)

-- | Skip listener-backed integration tests only when an enclosing sandbox
-- denies binding an ephemeral loopback port.
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
