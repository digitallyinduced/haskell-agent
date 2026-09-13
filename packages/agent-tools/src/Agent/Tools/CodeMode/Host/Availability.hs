module Agent.Tools.CodeMode.Host.Availability
    ( checkCodeModeAvailability
    , resolveBunExecutable
    , resolveWorkerScript
    , resolveWorkerCommand
    ) where

import Agent.Tools.CodeMode.Host.Types (CodeModeBackend(..), CodeModeConfig(..))
import Control.Exception.Safe
    ( SomeException
    , displayException
    , try
    )
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory
    ( canonicalizePath
    , doesFileExist
    , findExecutable
    )
import System.Exit (ExitCode(..))
import System.Environment (lookupEnv)
import System.FilePath (isPathSeparator)
import System.Info (os)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)

-- | Check the external runtime before exposing @exec@/@wait@ to the model.
checkCodeModeAvailability :: CodeModeConfig -> IO (Either Text ())
checkCodeModeAvailability config = do
    command <- resolveWorkerCommand config
    case command of
        Left err -> pure (Left err)
        Right (executable, []) -> inspectNative executable
        Right (executable, _) -> inspectBun executable
  where
    inspectNative executable = do
        checked <- try @_ @SomeException $
            timeout (max 1 (min 60000 config.startupTimeoutMs) * 1000) $
                readProcessWithExitCode executable ["--check"] ""
        pure $ case checked of
            Left err -> Left ("failed to inspect JavaScriptCore worker: "
                <> Text.pack (displayException err))
            Right Nothing -> Left "JavaScriptCore worker check timed out"
            Right (Just (ExitFailure code, _, stderrText)) ->
                Left ("JavaScriptCore worker check failed (exit "
                    <> Text.pack (show code) <> "): "
                    <> Text.strip (Text.pack stderrText))
            Right (Just (ExitSuccess, _, _)) -> Right ()

-- | Resolve exactly one backend before launching a worker. Failure never
-- triggers fallback to another runtime or replay of a cell.
resolveWorkerCommand :: CodeModeConfig -> IO (Either Text (FilePath, [String]))
resolveWorkerCommand config = do
    selected <- case config.codeModeBackend of
        AutomaticBackend -> lookupEnv "AGENT_CODE_MODE_BACKEND" >>= \case
            Nothing -> pure (Right (if os == "darwin" then JavaScriptCoreBackend else BunBackend))
            Just "bun" -> pure (Right BunBackend)
            Just "javascriptcore" -> pure (Right JavaScriptCoreBackend)
            Just value -> pure (Left ("invalid AGENT_CODE_MODE_BACKEND: " <> Text.pack value))
        explicit -> pure (Right explicit)
    case selected of
        Left err -> pure (Left err)
        Right JavaScriptCoreBackend -> do
            override <- lookupEnv "AGENT_CODE_MODE_WORKER"
            let requested = maybe config.nativeWorkerExecutable id override
            resolveBunExecutable requested >>= \case
                Nothing -> pure (Left ("JavaScriptCore worker executable was not found: "
                    <> Text.pack requested))
                Just executable -> pure (Right (executable, []))
        Right _ -> do
            executable <- resolveBunExecutable config.bunExecutable
            worker <- resolveWorkerScript config.workerScript
            pure $ case (executable, worker) of
                (Nothing, _) -> Left ("Bun runtime executable was not found: "
                    <> Text.pack config.bunExecutable)
                (_, Nothing) -> Left ("code-mode worker script was not found: "
                    <> Text.pack config.workerScript)
                (Just path, Just script) -> Right
                    (path, ["--smol", "--no-install", "--no-env-file", "--no-addons", script])

inspectBun :: FilePath -> IO (Either Text ())
inspectBun executable = do
    checked <- try @_ @SomeException $
        readProcessWithExitCode executable bunFeatureProbe ""
    pure $ case checked of
        Left err ->
            Left $
                "failed to inspect Bun runtime: "
                    <> Text.pack (displayException err)
        Right (ExitFailure code, _, stderrText) ->
            Left $
                "Bun lacks the vm sandbox features required by code mode (exit "
                    <> Text.pack (show code)
                    <> "): "
                    <> Text.strip (Text.pack stderrText)
        Right (ExitSuccess, _, _) -> Right ()

bunFeatureProbe :: [String]
bunFeatureProbe =
    [ "--smol"
    , "--no-install"
    , "--no-env-file"
    , "--no-addons"
    , "-e"
    , "import vm from 'node:vm';"
        <> "if (typeof vm.createContext !== 'function' || "
        <> "typeof vm.SourceTextModule !== 'function') process.exit(1);"
    ]

resolveBunExecutable :: FilePath -> IO (Maybe FilePath)
resolveBunExecutable executable
    | any isPathSeparator executable = do
        exists <- doesFileExist executable
        if exists then Just <$> canonicalizePath executable else pure Nothing
    | otherwise = findExecutable executable

resolveWorkerScript :: FilePath -> IO (Maybe FilePath)
resolveWorkerScript script = do
    exists <- doesFileExist script
    if exists
        then Just <$> canonicalizePath script
        else pure Nothing
