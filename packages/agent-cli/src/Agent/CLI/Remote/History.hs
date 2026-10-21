-- | Canonical, paginated local history, including tool results and resets.
module Agent.CLI.Remote.History (loadRemoteHistoryPage, loadRemoteTranscript) where

import Agent.CLI.Remote.Transport
import Agent.Concurrent (mapConcurrentlyBounded)
import Agent.Runtime.Json (decodeLazy)
import Agent.Runtime.Session
import Agent.Server.Client (AgentServerClient)
import Control.Exception.Safe (throwIO)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text

data Page = Page ![(Int64, Value)] !Int64 !Int64 !Bool !Bool

instance FromJSON Page where
    parseJSON = withObject "local history page" \fields -> do
        rows <- fields .: "data" >>= traverse (withObject "history row" \row -> do
            index <- row .: "index"
            envelope <- row .:? "localTurn" >>= maybe
                (object . pure . ("event" .=) <$> (row .: "turn" :: Parser Value)) pure
            pure (index, envelope))
        Page rows <$> fields .: "generationStart" <*> fields .: "total"
            <*> fields .: "hasOlder" <*> fields .: "hasNewer"

loadRemoteHistoryPage :: AgentServerClient -> Text -> Maybe Int64 -> Maybe Int64 -> Int -> IO SessionTurnPage
loadRemoteHistoryPage client identifier before after limit = do
    Page rows pageGenerationStart pageTotalTurns pageHasOlder pageHasNewer <-
        remoteJson client "GET" path [200] Nothing
    pageTurns <- mapConcurrentlyBounded 4 (\(index, envelope) -> do
        value <- remoteEnvelope client envelope >>= requireRemote
        turn <- either (throwIO . RemoteFailure) pure (decodeLazy sessionTurnDecoder (encode value))
        pure (index, turn)) rows
    pure SessionTurnPage{..}
  where
    path = "/v1/sessions/" <> segment identifier <> "/history?limit=" <> Text.pack (show limit)
        <> maybe "" (("&before=" <>) . Text.pack . show) before
        <> maybe "" (("&after=" <>) . Text.pack . show) after

loadRemoteTranscript :: AgentServerClient -> Text -> IO [(Int64, SessionTurn)]
loadRemoteTranscript client identifier = collect Nothing
  where
    collect cursor = do
        page <- loadRemoteHistoryPage client identifier cursor Nothing 100
        let next = fst <$> listToMaybe page.pageTurns
        older <- if page.pageHasOlder && next /= cursor && next /= Nothing
            then collect next else pure []
        pure (older <> page.pageTurns)
