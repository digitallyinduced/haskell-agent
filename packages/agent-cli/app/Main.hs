module Main (main) where

import Agent.CLI (run)
import Control.Monad (when)
import System.Directory (canonicalizePath, doesFileExist)
import System.Environment (getExecutablePath, lookupEnv, setEnv)
import System.FilePath ((</>), searchPathSeparator, takeDirectory)
import System.IO.Error (catchIOError)

main :: IO ()
main = do
    configurePortableBundle
    run

-- | Configure resources shipped next to the standalone macOS executable.
--
-- Cabal's generated @Paths_*@ modules normally contain Nix store paths. The
-- release bundle carries a marker file and mirrors each package's data
-- directory under @share@, so point those modules and external tools at the
-- extracted bundle before the CLI starts. Canonicalising the executable makes
-- this work when @agent-cli@ is invoked through a symlink in a PATH directory.
configurePortableBundle :: IO ()
configurePortableBundle = do
    reportedExecutable <- getExecutablePath
    executable <-
        canonicalizePath reportedExecutable
            `catchIOError` const (pure reportedExecutable)
    let root = takeDirectory (takeDirectory executable)
        bin = root </> "bin"
        postgresBin = root </> "libexec" </> "postgresql" </> "bin"
        share = root </> "share"
        marker = share </> "haskell-agent" </> "portable"
    portable <- doesFileExist marker
    when portable do
        setDefault "agent_cli_datadir" (share </> "agent-cli")
        setDefault "agent_cli_runtime_datadir" (share </> "agent-cli-runtime")
        setDefault "agent_core_datadir" (share </> "agent-core")
        setDefault "AGENT_SYNTAX_DIR" (share </> "skylighting" </> "xml")
        setDefault "AGENT_POSTGRES_BIN" postgresBin
        setDefault "TZDIR" (share </> "zoneinfo")
        currentPath <- lookupEnv "PATH"
        let bundledTools = bin <> [searchPathSeparator] <> postgresBin
            bundledPath = case currentPath of
                Just value | not (null value) ->
                    bundledTools <> [searchPathSeparator] <> value
                _ -> bundledTools
        setEnv "PATH" bundledPath

setDefault :: String -> String -> IO ()
setDefault name value =
    lookupEnv name >>= \case
        Nothing -> setEnv name value
        Just _ -> pure ()
