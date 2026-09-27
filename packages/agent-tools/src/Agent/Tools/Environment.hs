-- | Session-owned, transactional Nix development environments.
module Agent.Tools.Environment
    ( newEnvironmentTool
    , newEnvironmentToolWithNix
    , restoreShellEnvironment
    ) where

import Agent.Cancel (isCancelled)
import Agent.OsPath (unsafeToFilePath)
import Agent.ToolDispatch (streamingTextTool)
import Agent.Tools.IO (CommandResult(..), formatCommandResult, runShellCommandStreaming)
import Agent.Tools.Types
    ( AppTool, ApprovalRule(..), ShellEnvironment(..), ToolEnv(..)
    , ToolExecutionPolicy(..), freeformGrammarAppToolWithExecution
    )
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Exception.Safe (mask_, tryAny)
import Data.Aeson (eitherDecodeStrict', encode)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory
    ( canonicalizePath, createDirectoryIfMissing, doesPathExist
    , findExecutable, renameFile
    )
import System.FilePath ((</>), isAbsolute, takeFileName)
import System.OsPath (OsPath, unsafeEncodeUtf)
import System.Posix.Temp (mkdtemp)

-- | The lock serializes explicitly concurrent calls from code mode as well as
-- direct tool invocations. Each build uses the original host environment so a
-- broken active environment cannot prevent its replacement.
newEnvironmentTool :: ToolEnv -> IO AppTool
newEnvironmentTool env = newEnvironmentToolUsing env (findExecutable "nix")

-- | Supply a Nix executable explicitly, allowing deterministic process tests
-- without modifying the harness's process-global PATH.
newEnvironmentToolWithNix :: ToolEnv -> FilePath -> IO AppTool
newEnvironmentToolWithNix env executable =
    newEnvironmentToolUsing env (pure (Just executable))

newEnvironmentToolUsing :: ToolEnv -> IO (Maybe FilePath) -> IO AppTool
newEnvironmentToolUsing env resolveExecutable = do
    lock <- newMVar ()
    pure $ freeformGrammarAppToolWithExecution
        "set_environment"
        ( "Replace this session's shell environment. Pass plain Nix source, not JSON "
        <> "or Markdown: { pkgs }: pkgs.mkShell { packages = [ pkgs.python312 ]; }. "
        <> "The harness supplies pinned nixpkgs through a managed flake. Builds and "
        <> "shell hooks execute with normal sandbox restrictions and may take up to "
        <> "ten minutes. Only a successful build changes subsequent shell commands; "
        <> "running processes and the harness are unchanged. Each call replaces the "
        <> "complete expression. Files are retained under the session temporary directory."
        )
        "lark"
        "start: SOURCE\nSOURCE: /(.|\\n)+/\n"
        AlwaysPrompt
        TurnSequential
        (streamingTextTool "set_environment" \emit expression ->
            withMVar lock \() ->
                if Text.null (Text.strip expression)
                    then pure (Left "Provide a nonempty Nix expression.")
                    else do
                        executable <- resolveExecutable
                        attempted <- tryAny (replaceEnvironment env executable emit expression)
                        pure $ case attempted of
                            Left exception ->
                                Left ("Environment was not activated: " <> Text.pack (show exception))
                            Right result -> result)

replaceEnvironment :: ToolEnv -> Maybe FilePath -> (Text -> IO ()) -> Text -> IO (Either Text Text)
replaceEnvironment env executable emit expression = do
    directory <- readIORef env.toolSessionTmp
    case (directory, executable) of
        (Nothing, _) -> pure (Left "This session has no private temporary directory.")
        (_, Nothing) -> pure (Left "Nix is not available in the harness environment.")
        (Just sessionDirectory, Just executablePath) -> do
            nixExecutable <- canonicalizePath executablePath
            let root = environmentRoot sessionDirectory
            createDirectoryIfMissing True root
            revision <- mkdtemp (root </> "revision-")
            let profile = revision </> "environment"
            Text.writeFile (revision </> "expression.nix") expression
            Text.writeFile (revision </> "flake.nix") environmentFlake
            baseline <- newIORef Nothing
            let buildEnv = env { toolShellEnvironment = baseline }
                command = Text.unwords $
                    map quoteShell
                        [ Text.pack nixExecutable
                        , "--extra-experimental-features", "nix-command flakes"
                        , "develop", "--profile", Text.pack profile
                        , "path:" <> Text.pack revision <> "#default"
                        , "--command", "/bin/sh", "-c", "true"
                        ]
            emit "Building candidate Nix environment…"
            result <- runShellCommandStreaming buildEnv env.toolCwd command 600000
                (\out err -> emit (out <> err))
            currentDirectory <- readIORef env.toolSessionTmp
            profileExists <- doesPathExist profile
            if result.commandExitCode /= Just 0
                    || result.commandCancelled || result.commandTimedOut
                    || not profileExists
                then pure $ Left $
                    "Environment was not activated; the previous environment is unchanged.\n"
                    <> formatCommandResult result
                    <> if profileExists then "" else "\nNix did not produce an environment profile."
                else if currentDirectory /= Just sessionDirectory
                    then pure (Left "The session changed during the build; environment was not activated.")
                    else do
                        -- Rename within one directory makes the on-disk activation
                        -- atomic. Never overwrite a successful revision in place.
                        let metadata = root </> "active.json"
                            candidateMetadata = revision </> "activation.json"
                        LazyByteString.writeFile candidateMetadata $
                            encode (takeFileName revision, nixExecutable)
                        -- Disk and memory must publish the same revision. This
                        -- short two-resource commit cannot be expressed as a
                        -- resource lifetime: defer asynchronous exceptions only
                        -- across rename and the corresponding IORef publication.
                        mask_ do
                            cancelled <- isCancelled env.toolCancel
                            activationDirectory <- readIORef env.toolSessionTmp
                            if cancelled || activationDirectory /= Just sessionDirectory
                                then pure (Left "Environment activation was cancelled or the session changed; the previous environment is unchanged.")
                                else do
                                    renameFile candidateMetadata metadata
                                    writeIORef env.toolShellEnvironment $ Just ShellEnvironment
                                        { environmentDirectory = sessionDirectory
                                        , environmentNixExecutable = nixExecutable
                                        , environmentProfile = unsafeEncodeUtf profile
                                        }
                                    pure $ Right $
                                        "Environment activated for subsequent shell commands.\nRevision: "
                                        <> Text.pack revision
                                        <> "\nExisting processes are unchanged.\n"
                                        <> formatCommandResult result

-- | Reconnect to the last successfully activated profile on session resume.
-- Missing or malformed state does not prevent the session from opening.
restoreShellEnvironment :: ToolEnv -> IO ()
restoreShellEnvironment env = do
    directory <- readIORef env.toolSessionTmp
    restored <- tryAny $ case directory of
        Nothing -> pure Nothing
        Just sessionDirectory -> do
            let root = environmentRoot sessionDirectory
            encoded <- ByteString.readFile (root </> "active.json")
            case eitherDecodeStrict' encoded of
                Right (revision, executable)
                    | not (null revision)
                    , revision == takeFileName revision
                    , revision /= "."
                    , revision /= ".."
                    , isAbsolute executable -> do
                        let profile = root </> revision </> "environment"
                        exists <- doesPathExist profile
                        executableExists <- doesPathExist executable
                        pure $ if exists && executableExists
                            then Just ShellEnvironment
                                { environmentDirectory = sessionDirectory
                                , environmentNixExecutable = executable
                                , environmentProfile = unsafeEncodeUtf profile
                                }
                            else Nothing
                _ -> pure Nothing
    writeIORef env.toolShellEnvironment $ either (const Nothing) id restored

environmentRoot :: OsPath -> FilePath
environmentRoot directory = unsafeToFilePath directory </> "nix-environment"

quoteShell :: Text -> Text
quoteShell value = "'" <> Text.replace "'" "'\\''" value <> "'"

-- The revision matches the repository's nixpkgs lock when this experiment was
-- introduced. No channel lookup or ambient NIX_PATH participates in evaluation.
environmentFlake :: Text
environmentFlake = Text.unlines
    [ "{"
    , "  inputs.nixpkgs.url = \"github:NixOS/nixpkgs/afe3d8ac4395617bdcdac9f188ac8717a062e014\";"
    , "  outputs = { nixpkgs, ... }: {"
    , "    devShells = nixpkgs.lib.genAttrs"
    , "      [ \"aarch64-darwin\" \"x86_64-darwin\" \"aarch64-linux\" \"x86_64-linux\" ]"
    , "      (system: { default = import ./expression.nix {"
    , "        pkgs = import nixpkgs { inherit system; };"
    , "      }; });"
    , "  };"
    , "}"
    ]
