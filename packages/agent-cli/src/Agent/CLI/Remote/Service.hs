module Agent.CLI.Remote.Service (runLocalServerCommand) where

import Agent.CLI.Options (LocalServerCommand(..))
import Agent.CLI.Remote.Transport (requireRemote, RemoteFailure(..))
import Agent.Server.Client.Local
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (throwIO)
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 qualified as ByteString
import Data.Text qualified as Text

runLocalServerCommand :: LocalServerCommand -> IO ()
runLocalServerCommand = \case
    ServerStart -> start
    ServerStatus -> connectLocalService >>= \case
        Left LocalServiceAbsent -> putStrLn "Local server is stopped."
        Left err -> failService err
        Right client -> localServiceStatus client >>= requireRemote >>= ByteString.putStrLn . encode
    ServerStop -> stop False
    ServerRestart -> stop False >> start
    ServerForceStop -> stop True
    ServerForceRestart -> stop True >> start
  where
    start = do
        client <- ensureLocalService >>= either failService pure
        localServiceStatus client >>= requireRemote >>= ByteString.putStrLn . encode
    stop force = connectLocalService >>= \case
        Left LocalServiceAbsent -> pure ()
        Left err -> failService err
        Right client -> do
            _ <- (if force then stopLocalServiceForced else stopLocalService) client >>= requireRemote
            awaitStop (200 :: Int)
    awaitStop attempts = connectLocalService >>= \case
        Right _ | attempts > 0 -> threadDelay 50_000 >> awaitStop (attempts - 1)
        Right _ -> throwIO (RemoteFailure "The local server has not finished shutting down.")
        Left _ -> pure ()
    failService = throwIO . RemoteFailure . Text.pack . show
