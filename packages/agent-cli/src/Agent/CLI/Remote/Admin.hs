-- | Common administration borrows the service's store and model boundary.
module Agent.CLI.Remote.Admin (runRemoteList, runRemoteShow, runRemoteWait) where

import Agent.CLI.Options (SessionOutputFormat(..), SessionPageRequest(..))
import Agent.CLI.Remote.Transport
import Agent.CLI.Remote.History
import Agent.CLI.SessionAdmin (renderFullSession, renderSessionPage)
import Agent.Runtime.Json (decodeLazy)
import Agent.Provider (providerSlug)
import Agent.Concurrent (mapConcurrentlyBounded)
import Agent.Runtime.Session
import Agent.Server.Client
import Agent.Server.Client.Local
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (throwIO)
import Control.Monad (when)
import Data.Aeson
import Data.Aeson.Types (parseEither, Parser)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy.Char8 qualified as Lazy
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Agent.OsPath (unsafeToFilePath)

service :: IO AgentServerClient
service = ensureLocalService >>= either (throwIO . RemoteFailure . Text.pack . show) pure

runRemoteList :: SessionOutputFormat -> IO ()
runRemoteList format = do
    client <- service
    sessions <- remoteSessions client
    case format of
        SessionHuman -> mapM_ (\session -> TextIO.putStrLn
            (session.remoteId <> "  " <> session.remoteTitle <> "  " <> session.remoteModel)) sessions
        SessionJSON -> do
          summaries <- mapConcurrentlyBounded 4 (\session -> do
            turns <- listAgentServerTurns client session.remoteId >>= requireRemote
            ownership <- remoteJson client "GET" ("/v1/local/sessions/" <> segment session.remoteId <> "/ownership") [200] Nothing
            let running = any (\turn -> turn.agentServerTurnStatus `elem`
                    [AgentServerTurnQueued, AgentServerTurnRunning, AgentServerTurnWaitingForInput]) turns.agentServerTurns
                locked = case ownership of
                    Object fields -> maybe False (/= Null) (KeyMap.lookup "clientId" fields)
                    _ -> False
            pure (object ["id" .= session.remoteId, "title" .= session.remoteTitle
                , "updatedAt" .= session.remoteUpdatedAt, "provider" .= providerSlug session.remoteProvider
                , "model" .= session.remoteModel, "effort" .= session.remoteEffort
                , "cwd" .= unsafeToFilePath session.remoteCwd
                , "isRunning" .= running, "isLocked" .= locked, "isArchived" .= False])) sessions
          Lazy.putStrLn (encode summaries)

runRemoteShow :: Text -> SessionOutputFormat -> Maybe SessionPageRequest -> IO ()
runRemoteShow identifier format page = do
    client <- service
    value <- remoteJson client "GET" ("/v1/sessions/" <> segment identifier) [200] Nothing
    encoded <- either (throwIO . RemoteFailure . Text.pack) pure $
        parseEither (withObject "local session" (\fields -> fields .: "localMeta" :: Parser Value)) value
    meta <- either (throwIO . RemoteFailure) pure (decodeLazy sessionMetaDecoder (encode encoded))
    case page of
        Nothing -> loadRemoteTranscript client identifier >>= renderFullSession format meta . map snd
        Just request -> do
            let (before, count) = case request of
                    SessionRecent n -> (Nothing, n)
                    SessionBefore cursor n -> (Just cursor, n)
            loadRemoteHistoryPage client identifier before Nothing count >>= renderSessionPage meta

runRemoteWait :: Text -> IO ()
runRemoteWait identifier = do
    client <- service
    _ <- remoteSession client identifier
    let wait = do
            turns <- listAgentServerTurns client identifier >>= requireRemote
            ownership <- remoteJson client "GET" ("/v1/local/sessions/" <> segment identifier <> "/ownership") [200] Nothing
            let owned = case ownership of
                    Object fields -> maybe False (/= Null) (KeyMap.lookup "clientId" fields)
                    _ -> False
            when (owned || any (\turn -> turn.agentServerTurnStatus `elem`
                [AgentServerTurnQueued, AgentServerTurnRunning, AgentServerTurnWaitingForInput])
                turns.agentServerTurns) (threadDelay 100000 >> wait)
    wait
