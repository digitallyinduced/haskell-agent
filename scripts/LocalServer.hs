{-# LANGUAGE ScopedTypeVariables #-}
-- The development service interprets both server and runtime source.
-- Its lifetime is bounded by the GHCi frontend owning this private profile.
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Exception.Safe (tryIO)
import Control.Monad (void)
import System.Environment (getEnv, getEnvironment)
import System.Exit (exitWith)
import System.IO (hPutStrLn, hClose)
import System.FilePath ((</>))
import System.Posix.Signals (signalProcess, nullSignal)
import System.Process
import Text.Read (readMaybe)

main :: IO ()
main = do
    root <- getEnv "HASKELL_AGENT_DEV_ROOT"
    owner <- getEnv "HASKELL_AGENT_DEV_OWNER"
    inherited <- getEnvironment
    let dataDirectories =
            [(name <> "_datadir", root </> "packages" </> package)
            | (name, package) <- [("agent_server", "agent-server"), ("agent_runtime", "agent-runtime")
                , ("agent_tools", "agent-tools"), ("agent_core", "agent-core")]]
        environment = dataDirectories <> filter
            (\(name, _) -> name /= "GHCRTS" && name `notElem` map fst dataDirectories) inherited
    let command = (proc "cabal"
            [ "repl", "--offline", "agent-server:lib:agent-server"
            , "agent-runtime:lib:agent-runtime", "agent-mcp:lib:agent-mcp"
            , "--repl-options=-ignore-dot-ghci" ])
            { cwd = Just root, std_in = CreatePipe, env = Just environment }
    withCreateProcess command $ \input _ _ process -> do
        case input of
            Nothing -> fail "the development server requires a GHCi command pipe"
            Just handle -> do
                hPutStrLn handle "import Agent.Server"
                hPutStrLn handle "import System.Environment (withArgs)"
                hPutStrLn handle "withArgs [\"--local\"] runServer"
                hClose handle
        outcome <- race (waitForProcess process) (watchOwner owner)
        case outcome of
            Left code -> exitWith code
            Right () -> terminateProcess process >> void (waitForProcess process)

watchOwner :: String -> IO ()
watchOwner raw = case readMaybe raw of
    Nothing -> fail "invalid development owner process"
    Just owner -> loop owner
  where
    loop owner = do
        threadDelay 1000000
        tryIO (signalProcess nullSignal owner) >>= \result -> case result of
            Left _ -> pure ()
            Right () -> loop owner
