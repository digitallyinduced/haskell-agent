{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Durable, provider-agnostic cache entries for account usage snapshots.
--
-- The payload is deliberately opaque text. Provider packages own the JSON
-- schema and decode it at the boundary; the store never receives credentials
-- or provider-specific types.
module Agent.Store.Postgres.UsageCache
    ( AccountUsageCacheEntry(..)
    , accountUsageCacheSchemaStatements
    , loadAccountUsageCache
    , upsertAccountUsageCache
    ) where

import Data.ByteString (ByteString)
import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)

import Agent.Store.Postgres.Connection
    ( StorePool
    , withSession
    )
import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.Types (StoreError)

-- | A cached provider response.  'accountUsageCacheFetchedAt' and
-- 'accountUsageCacheExpiresAt' are both retained so callers can show stale
-- data while an asynchronous refresh is in flight.
data AccountUsageCacheEntry = AccountUsageCacheEntry
    { accountUsageCacheProvider :: !Text
    , accountUsageCacheAccountId :: !Text
    , accountUsageCachePayload :: !Text
    , accountUsageCacheFetchedAt :: !UTCTime
    , accountUsageCacheExpiresAt :: !UTCTime
    }
    deriving (Eq, Show)

-- | DDL is kept with the cache API so migration and schema tests cannot drift.
accountUsageCacheSchemaStatements :: [ByteString]
accountUsageCacheSchemaStatements =
    [ "CREATE TABLE IF NOT EXISTS harness.account_usage_cache (\
      \ provider text NOT NULL,\
      \ account_id text NOT NULL,\
      \ payload_text text NOT NULL,\
      \ fetched_at timestamptz NOT NULL,\
      \ expires_at timestamptz NOT NULL,\
      \ PRIMARY KEY (provider, account_id)\
      \ )"
    ]

loadAccountUsageCache
    :: StorePool
    -> Text
    -> Text
    -> IO (Either StoreError (Maybe AccountUsageCacheEntry))
loadAccountUsageCache pool provider accountId =
    withSession pool (Session.statement
        (provider, accountId)
        loadAccountUsageCacheStatement)

upsertAccountUsageCache
    :: StorePool
    -> AccountUsageCacheEntry
    -> IO (Either StoreError ())
upsertAccountUsageCache pool entry =
    withSession pool (Session.statement entry upsertAccountUsageCacheStatement)

loadAccountUsageCacheStatement
    :: Statement (Text, Text) (Maybe AccountUsageCacheEntry)
loadAccountUsageCacheStatement = mkStatement
    "SELECT provider, account_id, payload_text, fetched_at, expires_at\
    \ FROM harness.account_usage_cache\
    \ WHERE provider = $1 AND account_id = $2"
    cacheKeyParams
    (Decoders.rowMaybe $
        AccountUsageCacheEntry
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.timestamptz)
            <*> Decoders.column (Decoders.nonNullable Decoders.timestamptz))
    True

upsertAccountUsageCacheStatement
    :: Statement AccountUsageCacheEntry ()
upsertAccountUsageCacheStatement = mkStatement
    "INSERT INTO harness.account_usage_cache\
    \ (provider, account_id, payload_text, fetched_at, expires_at)\
    \ VALUES ($1, $2, $3, $4, $5)\
    \ ON CONFLICT (provider, account_id) DO UPDATE SET\
    \ payload_text = EXCLUDED.payload_text,\
    \ fetched_at = EXCLUDED.fetched_at,\
    \ expires_at = EXCLUDED.expires_at"
    ( ((.accountUsageCacheProvider)
            >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.accountUsageCacheAccountId)
            >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.accountUsageCachePayload)
            >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.accountUsageCacheFetchedAt)
            >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz))
        <> ((.accountUsageCacheExpiresAt)
            >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz))
    )
    Decoders.noResult
    True

cacheKeyParams :: Encoders.Params (Text, Text)
cacheKeyParams =
    ((fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.text)))
