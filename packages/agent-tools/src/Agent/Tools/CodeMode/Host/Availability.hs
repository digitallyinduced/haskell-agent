module Agent.Tools.CodeMode.Host.Availability
    ( checkCodeModeAvailability
    , resolveBunExecutable
    , resolveWorkerScript
    ) where

import Agent.Tools.CodeMode.Host.Types (CodeModeConfig(..))
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
import System.FilePath (isPathSeparator)
import System.Process (readProcessWithExitCode)

-- | Check the external runtime before exposing @exec@/@wait@ to the model.
checkCodeModeAvailability :: CodeModeConfig -> IO (Either Text ())
checkCodeModeAvailability config = do
    executable <- resolveBunExecutable config.bunExecutable
    worker <- resolveWorkerScript config.workerScript
    runtime <- case executable of
        Nothing -> pure Nothing
        Just path -> Just <$> inspectBun path
    pure $ case (runtime, worker) of
        (Nothing, _) ->
            Left $
                "Bun runtime executable was not found: "
                    <> Text.pack config.bunExecutable
        (Just (Left err), _) -> Left err
        (_, Nothing) ->
            Left $
                "code-mode worker script was not found: "
                    <> Text.pack config.workerScript
        (Just (Right ()), Just _) -> Right ()
  where
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
        pure (if exists then Just executable else Nothing)
    | otherwise = findExecutable executable

resolveWorkerScript :: FilePath -> IO (Maybe FilePath)
resolveWorkerScript script = do
    exists <- doesFileExist script
    if exists
        then Just <$> canonicalizePath script
        else pure Nothing
