-- | Crash-safe, cross-process admission for session waits.
module Agent.CLI.AgentSessions.WaitGraph
    ( withSessionWaitEdge
    ) where

import Agent.PrivateFileLock (withPrivateFileLock)
import Agent.OsPath (unsafeToFilePath)
import Control.Exception.Safe (bracket, finally, onException)
import Control.Monad (forM)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (isPrefixOf)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified System.Directory as Directory
import qualified System.FileLock as FileLock
import qualified System.FilePath as FilePath
import System.OsPath (OsPath, unsafeEncodeUtf, (</>))
import System.Posix.Temp (mkdtemp)

data WaitEdge = WaitEdge !FilePath !FileLock.FileLock

-- | Register one scoped wait. Admission is serialized across processes and
-- includes every live outgoing edge, not just one target per session. Session
-- identifiers are metadata only and never become filesystem paths.
--
-- Each registration owns a distinct OS lease. Crashes release that lease, so
-- later admissions can discard stale edges without PID/clock heuristics.
-- The caller supplies the wait deadline; cancelling the action removes only
-- this edge and never affects the session being waited on.
withSessionWaitEdge
    :: OsPath
    -> Text
    -> Text
    -> IO a
    -> IO (Either Text a)
withSessionWaitEdge root source target action
    | source == target = pure (Left "cannot wait for the current agent session")
    | otherwise =
        bracket
            (withPrivateFileLock gate acquire)
            release
            \case
                Left err -> pure (Left err)
                Right _ -> Right <$> action
  where
    directory = root </> unsafeEncodeUtf ".agent-session-waits"
    gate = directory </> unsafeEncodeUtf "admission.lock"

    acquire = do
        edges <- liveEdges (unsafeToFilePath directory)
        case edges of
            Left err -> pure (Left err)
            Right graph
                | reachable graph target source ->
                    pure (Left "circular agent session wait rejected")
                | otherwise -> do
                    path <- mkdtemp
                        (unsafeToFilePath directory FilePath.</> "edge-")
                    (do
                        lease <- FileLock.lockFile
                            (path FilePath.</> "lease") FileLock.Exclusive
                        (do
                            LBS.writeFile (path FilePath.</> "edge.json")
                                (Aeson.encode (source, target))
                            pure (Right (WaitEdge path lease)))
                            `onException` FileLock.unlockFile lease)
                        `onException` Directory.removeDirectoryRecursive path

    release (Left _) = pure ()
    release (Right (WaitEdge path lease)) =
        -- Remove the unique name while still leased and under the admission
        -- gate. Nobody can probe or recreate that lease during its removal.
        withPrivateFileLock gate (Directory.removeDirectoryRecursive path)
            `finally` FileLock.unlockFile lease

liveEdges :: FilePath -> IO (Either Text [(Text, Text)])
liveEdges directory = do
    names <- filter ("edge-" `isPrefixOf`) <$> Directory.listDirectory directory
    results <- forM names \name -> do
        let path = directory FilePath.</> name
        bracket
            (FileLock.tryLockFile (path FilePath.</> "lease") FileLock.Exclusive)
            (maybe (pure ()) FileLock.unlockFile)
            \case
                Just _ -> do
                    Directory.removeDirectoryRecursive path
                    pure (Right [])
                Nothing -> do
                    bytes <- BS.readFile (path FilePath.</> "edge.json")
                    pure $ case Aeson.eitherDecodeStrict' bytes of
                        Left err ->
                            Left ("invalid live session wait edge: " <> Text.pack err)
                        Right edge -> Right [edge]
    pure (concat <$> sequence results)

reachable :: [(Text, Text)] -> Text -> Text -> Bool
reachable graph start goal = visit Set.empty [start]
  where
    visit _ [] = False
    visit seen (node : remaining)
        | node == goal = True
        | Set.member node seen = visit seen remaining
        | otherwise =
            visit (Set.insert node seen)
                ([target | (source, target) <- graph, source == node] <> remaining)
