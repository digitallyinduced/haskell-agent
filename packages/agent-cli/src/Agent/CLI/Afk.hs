-- | Move an idle persisted session into a detached tmux, locally or over SSH.
module Agent.CLI.Afk
    ( AfkTarget(..)
    , parseAfkTarget
    , handoffLocal
    , handoffRemote
    ) where

import Agent.CLI.Session (SessionMeta(..), SessionTransfer(..))
import Agent.OsPath (unsafeToFilePath)
import Control.Concurrent.Async (concurrently)
import Control.Exception.Safe (tryAny)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Environment (getExecutablePath, getProgName, lookupEnv)
import System.Exit (ExitCode(..))
import System.IO (hClose)
import System.OsPath (OsPath)
import System.Process
    ( CreateProcess(..)
    , StdStream(CreatePipe, UseHandle)
    , createProcess
    , proc
    , readProcessWithExitCode
    , waitForProcess
    )

data AfkTarget
    = AfkLocal
    | AfkRemote !Text !Text
    deriving (Eq, Show)

parseAfkTarget :: Maybe Text -> Either Text AfkTarget
parseAfkTarget Nothing = Right AfkLocal
parseAfkTarget (Just raw) =
    let (host, suffix) = Text.breakOn ":" (Text.strip raw)
        path = Text.drop 1 suffix
    in if Text.null host
            || Text.isPrefixOf "-" host
            || Text.null suffix
            || Text.null path
        then Left "remote AFK target must be HOST:PATH"
        else Right (AfkRemote host path)

handoffLocal :: Text -> OsPath -> IO (Either Text Text)
handoffLocal sessionId cwd = do
    executable <- localExecutable
    let name = tmuxName sessionId
        script =
            quote executable
                <> " sessions wait " <> quote (Text.unpack sessionId)
                <> " && exec " <> quote executable
                <> " --resume " <> quote (Text.unpack sessionId)
                <> " --cwd " <> quote (unsafeToFilePath cwd)
    runChecked "tmux"
        ["new-session", "-d", "-s", Text.unpack name, "sh", "-lc", script]
        ("session moved to tmux " <> name
            <> "\nattach with: tmux attach -t " <> name)

handoffRemote
    :: Text
    -> Text
    -> OsPath
    -> SessionTransfer
    -> IO (Either Text Text)
handoffRemote host path sessionDir transfer = do
    let meta = transfer.transferMeta
        name = tmuxName meta.metaId
        remotePath = remotePathWord path
        sessionId = meta.metaId
        importCommand =
            "cd " <> remotePath
                <> " && agent-cli sessions import --cwd " <> remotePath
        sessionPath =
            "\"$HOME/.haskell-agent/sessions/"
                <> Text.unpack sessionId <> "\""
        stagingPath =
            "\"$HOME/.haskell-agent/tmp/afk-"
                <> Text.unpack sessionId <> ".tar\""
        uploadCommand =
            "mkdir -p \"$HOME/.haskell-agent/tmp\""
                <> " && cat > " <> stagingPath
        startCommand =
            "tar -C " <> sessionPath <> " -xf " <> stagingPath
                <> " && rm -f " <> stagingPath
                <> " && tmux new-session -d -s " <> quote (Text.unpack name)
                <> " -c " <> remotePath
                <> " agent-cli --resume " <> quote (Text.unpack sessionId)
                <> " --cwd " <> remotePath
    copyArtifacts host uploadCommand sessionDir >>= \case
        Left err -> pure (Left err)
        Right () ->
            runSshInput host importCommand (Aeson.encode transfer) >>= \case
                Left err -> pure (Left err)
                Right _ ->
                    fmap (\case
                        Left err -> Left err
                        Right _ -> Right
                            ("session moved to " <> host <> " tmux " <> name
                                <> "\nattach with: ssh -t " <> host
                                <> " tmux attach -t " <> name))
                        (runSshInput host startCommand mempty)

runSshInput :: Text -> String -> LBS.ByteString -> IO (Either Text Text)
runSshInput host remote inputBytes = do
    attempted <- tryAny do
        (Just input, Just output, Just errOutput, process) <-
            createProcess (proc "ssh" [Text.unpack host, "sh", "-lc", quote remote])
                { std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = CreatePipe
                }
        LBS.hPut input inputBytes
        hClose input
        (out, err) <- concurrently
            (LBS.hGetContents output)
            (LBS.hGetContents errOutput)
        code <- waitForProcess process
        pure (code, out, err)
    pure case attempted of
        Left exception -> Left (Text.pack (show exception))
        Right (ExitFailure _, _, err) ->
            Left ("remote AFK handoff failed: " <> decode err)
        Right (ExitSuccess, out, _) -> Right (Text.strip (decode out))

copyArtifacts :: Text -> String -> OsPath -> IO (Either Text ())
copyArtifacts host remote sessionDir = do
    attempted <- tryAny do
        (Nothing, Just archive, Just tarError, tarProcess) <-
            createProcess
                (proc "tar"
                    [ "-C", unsafeToFilePath sessionDir
                    , "--exclude=.agent-running.lock"
                    , "--exclude=.agent-prompt-*"
                    , "--exclude=.agent-ready-*"
                    , "-cf", "-", "."
                    ])
                    { std_out = CreatePipe
                    , std_err = CreatePipe
                    }
        (Nothing, Just sshOut, Just sshError, sshProcess) <-
            createProcess
                (proc "ssh" [Text.unpack host, "sh", "-lc", quote remote])
                    { std_in = UseHandle archive
                    , std_out = CreatePipe
                    , std_err = CreatePipe
                    }
        hClose archive
        ((tarErr, sshStdout), sshErr) <- concurrently
            (concurrently
                (LBS.hGetContents tarError)
                (LBS.hGetContents sshOut))
            (LBS.hGetContents sshError)
        tarCode <- waitForProcess tarProcess
        sshCode <- waitForProcess sshProcess
        pure (tarCode, sshCode, tarErr, sshStdout, sshErr)
    pure case attempted of
        Left exception -> Left (Text.pack (show exception))
        Right (ExitSuccess, ExitSuccess, _, _, _) -> Right ()
        Right (tarCode, sshCode, tarErr, _, sshErr) ->
            Left
                ("remote AFK artifact transfer failed (tar "
                    <> Text.pack (show tarCode) <> ", ssh "
                    <> Text.pack (show sshCode) <> "): "
                    <> Text.strip (decode (tarErr <> sshErr)))

localExecutable :: IO FilePath
localExecutable = do
    lookupEnv "HASKELL_AGENT_EXECUTABLE" >>= \case
        Just executable | not (null executable) -> pure executable
        _ -> do
            prog <- getProgName
            executable <- getExecutablePath
            pure $
                if prog `elem` ["<interactive>", "ghc", "ghci"]
                    then "agent-cli"
                    else executable

runChecked :: FilePath -> [String] -> Text -> IO (Either Text Text)
runChecked command args success = do
    attempted <- tryAny (readProcessWithExitCode command args "")
    pure case attempted of
        Left exception -> Left (Text.pack (show exception))
        Right (ExitSuccess, _, _) -> Right success
        Right (ExitFailure _, out, err) ->
            Left (Text.strip (Text.pack (if null err then out else err)))

tmuxName :: Text -> Text
tmuxName sessionId =
    "agent-" <> Text.take 40 (Text.map safe sessionId)
  where
    safe character
        | isAlphaNum character || character `elem` ("-_" :: String) = character
        | otherwise = '-'

quote :: String -> String
quote value = "'" <> concatMap escape value <> "'"
  where
    escape '\'' = "'\\''"
    escape character = [character]

remotePathWord :: Text -> String
remotePathWord path =
    case Text.stripPrefix "~/" path of
        Just rest -> "\"$HOME\"/" <> quote (Text.unpack rest)
        Nothing -> quote (Text.unpack path)

decode :: LBS.ByteString -> Text
decode = Text.decodeUtf8With (\_ _ -> Just '\xfffd') . LBS.toStrict
