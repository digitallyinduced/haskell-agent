module Agent.Store.Postgres.Session.PullRequests
    ( loadSessionPullRequests, saveSessionPullRequests ) where

import Data.Text (Text)
import Data.Int (Int64)
import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)
import Agent.Store.Postgres.Connection (StorePool, withSession)
import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.Types (StoreError)

-- The recency cache is separate from the legacy association cache. Its
-- presence records that history was indexed using current ordering semantics;
-- older running clients cannot overwrite it with prompt-first ordering.
loadSessionPullRequests :: StorePool -> Text -> IO (Either StoreError (Maybe (Int64, [Text])))
loadSessionPullRequests pool key = withSession pool (Session.statement key loadStatement)

saveSessionPullRequests :: StorePool -> Text -> Int64 -> [Text] -> IO (Either StoreError ())
saveSessionPullRequests pool key cursor urls = withSession pool (Session.statement (key, cursor, urls) saveStatement)

loadStatement :: Statement Text (Maybe (Int64, [Text]))
loadStatement = mkStatement
    "SELECT p.next_turn_index, p.urls FROM harness.session_pull_request_recency p JOIN harness.sessions s USING (session_id) WHERE s.session_key = $1 AND s.deleted_at IS NULL"
    (E.param (E.nonNullable E.text))
    (D.rowMaybe ((,) <$> D.column (D.nonNullable D.int8)
        <*> D.column (D.nonNullable (D.listArray (D.nonNullable D.text))))) True

saveStatement :: Statement (Text, Int64, [Text]) ()
saveStatement = mkStatement
    "INSERT INTO harness.session_pull_request_recency (session_id, next_turn_index, urls) SELECT session_id, $2, $3 FROM harness.sessions WHERE session_key = $1 AND deleted_at IS NULL ON CONFLICT (session_id) DO UPDATE SET next_turn_index = EXCLUDED.next_turn_index, urls = EXCLUDED.urls WHERE harness.session_pull_request_recency.next_turn_index <= EXCLUDED.next_turn_index"
    (((\(a,_,_) -> a) >$< E.param (E.nonNullable E.text))
    <> ((\(_,b,_) -> b) >$< E.param (E.nonNullable E.int8))
    <> ((\(_,_,c) -> c) >$< E.param (E.nonNullable (E.foldableArray (E.nonNullable E.text)))))
    D.noResult True
