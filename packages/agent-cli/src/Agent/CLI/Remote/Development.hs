module Agent.CLI.Remote.Development (configureDevelopmentService, stopDevelopmentService) where

import Agent.Server.Client.Local
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (throwString)
import System.Directory (getTemporaryDirectory)
import System.Environment (lookupEnv, setEnv)
import System.FilePath ((</>))
import System.Posix.Process (getProcessID)

configureDevelopmentService :: IO ()
configureDevelopmentService = lookupEnv "HASKELL_AGENT_DEV_SERVER" >>= mapM_ \driver -> do
    owner <- show <$> getProcessID
    temporary <- getTemporaryDirectory
    setEnv "HASKELL_AGENT_SERVER" driver
    setEnv "HASKELL_AGENT_RUNTIME_DIR" (temporary </> ("agent-dev-" <> owner))
    setEnv "HASKELL_AGENT_DEV_OWNER" owner
    setEnv "HASKELL_AGENT_SERVER_STARTUP_TIMEOUT_SECONDS" "180"

stopDevelopmentService :: IO ()
stopDevelopmentService = lookupEnv "HASKELL_AGENT_DEV_OWNER" >>= mapM_ \_ ->
    connectLocalService >>= \case
        Left _ -> pure ()
        Right client -> do
            stopLocalServiceForced client >>= either (const (throwString "could not stop the development service")) (const (pure ()))
            wait (200 :: Int)
  where
    wait remaining = connectLocalService >>= \case
        Left _ -> pure ()
        Right _ | remaining > 0 -> threadDelay 50000 >> wait (remaining - 1)
        Right _ -> throwString "the development service did not stop before reload"
