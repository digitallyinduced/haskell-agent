{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Agent.Store.Postgres.ManagedSpec (spec) where

import Control.Exception.Safe (finally)
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)
import qualified Hasql.Statement as Statement
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Agent.Store.Postgres
import Agent.Store.Postgres.Connection (withSession)
import Agent.Store.Postgres.Managed (stopManagedPostgres)

spec :: Spec
spec =
    describe "managed PostgreSQL" do
        it "starts on a private socket and applies the harness migrations" $
            -- Keep the prefix short because Darwin's Unix socket path limit
            -- also includes PostgreSQL's generated socket filename.
            withSystemTempDirectory "ha" \stateDirectory -> do
                let config =
                        defaultManagedPostgresConfig stateDirectory ""
                    cleanup = do
                        _ <- stopManagedPostgres config
                        pure ()
                ((withStore config \store -> do
                    withSession
                        (trustedPool store)
                        (Session.statement () serverStatement)
                        `shouldReturn`
                            Right
                                ( "haskell_agent"
                                , "ha_runtime"
                                , True
                                , True
                                , True
                                )
                    forbiddenResult <- withSession
                        (trustedPool store)
                        (Session.script
                            "CREATE SCHEMA runtime_must_not_create")
                    forbiddenResult `shouldSatisfy` isLeft
                    ) >>= \case
                        Left err ->
                            expectationFailure
                                ("could not open managed store: " <> show err)
                        Right () -> pure ())
                    `finally` cleanup

serverStatement :: Statement () (Text, Text, Bool, Bool, Bool)
serverStatement = Statement.preparable
    "SELECT current_database()::text, current_user::text,\
    \ inet_server_addr() IS NULL,\
    \ to_regclass('harness.sessions') IS NOT NULL,\
    \ current_setting('server_version_num')::integer >= 180000\
    \   AND substring(pg_catalog.uuidv7()::text, 15, 1) = '7'"
    Encoders.noParams
    (Decoders.singleRow $
        (,,,,)
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.bool)
            <*> Decoders.column (Decoders.nonNullable Decoders.bool)
            <*> Decoders.column (Decoders.nonNullable Decoders.bool))
