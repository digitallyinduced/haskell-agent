{-# LANGUAGE OverloadedStrings #-}

-- | Presentation-only model catalogs keyed by a non-secret connection identity.
module Agent.Store.Postgres.ModelCatalogCache
    ( modelCatalogCacheSchemaStatements
    , loadModelCatalogCache
    , upsertModelCatalogCache
    , deleteModelCatalogCache
    ) where

import Agent.Store.Postgres.Connection (StorePool, withSession)
import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.Types (StoreError)
import Data.ByteString (ByteString)
import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Session as Session

modelCatalogCacheSchemaStatements :: [ByteString]
modelCatalogCacheSchemaStatements =
    [ "CREATE TABLE IF NOT EXISTS harness.model_catalog_cache (\
      \ connection_identity text PRIMARY KEY,\
      \ payload_text text NOT NULL,\
      \ fetched_at timestamptz NOT NULL DEFAULT clock_timestamp())"
    , "GRANT SELECT, INSERT, UPDATE, DELETE ON harness.model_catalog_cache TO ha_runtime"
    ]

loadModelCatalogCache :: StorePool -> Text -> IO (Either StoreError (Maybe Text))
loadModelCatalogCache pool identity =
    withSession pool (Session.statement identity (mkStatement
        "SELECT payload_text FROM harness.model_catalog_cache WHERE connection_identity = $1"
        (Encoders.param (Encoders.nonNullable Encoders.text))
        (Decoders.rowMaybe (Decoders.column (Decoders.nonNullable Decoders.text)))
        True))

upsertModelCatalogCache :: StorePool -> Text -> Text -> IO (Either StoreError ())
upsertModelCatalogCache pool identity payload =
    withSession pool (Session.statement (identity, payload) (mkStatement
        "INSERT INTO harness.model_catalog_cache (connection_identity, payload_text)\
        \ VALUES ($1, $2) ON CONFLICT (connection_identity) DO UPDATE SET\
        \ payload_text = EXCLUDED.payload_text, fetched_at = clock_timestamp()"
        ((fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
            <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.text)))
        Decoders.noResult
        True))

deleteModelCatalogCache :: StorePool -> Text -> IO (Either StoreError ())
deleteModelCatalogCache pool identity =
    withSession pool (Session.statement identity (mkStatement
        "DELETE FROM harness.model_catalog_cache WHERE connection_identity = $1"
        (Encoders.param (Encoders.nonNullable Encoders.text))
        Decoders.noResult
        True))
