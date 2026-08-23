{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Agent.Store.Postgres.CustomSpec (spec) where

import qualified Data.Aeson as Aeson
import Test.Hspec

import Agent.Store.Postgres.Custom

spec :: Spec
spec = describe "custom PostgreSQL SQL normalization" do
    it "includes the durable audit id in successful execution results" do
        Aeson.toJSON CustomExecutionResult
            { customExecutionAuditId = "0198d1ac-7b48-7000-8000-000000000000"
            , customExecutionCatalogBefore = []
            , customExecutionCatalogAfter = []
            , customExecutionWarning = Nothing
            }
            `shouldBe` Aeson.object
                [ "audit_id" Aeson..=
                    ("0198d1ac-7b48-7000-8000-000000000000" :: String)
                , "catalog_before" Aeson..= ([] :: [CatalogObject])
                , "catalog_after" Aeson..= ([] :: [CatalogObject])
                , "warning" Aeson..= (Nothing :: Maybe String)
                ]

    describe "normalizeCustomQuery" do
        it "strips whitespace and trailing statement terminators" do
            normalizeCustomQuery "  select * from todos; \n"
                `shouldBe` Right "select * from todos"

        it "rejects empty and NUL-containing queries" do
            normalizeCustomQuery " ; \n" `shouldBe`
                Left "database query SQL must not be empty"
            normalizeCustomQuery "select '\NUL'"
                `shouldBe` Left "database query SQL contains a NUL byte"

    describe "normalizeCustomExecution" do
        it "accepts DDL/DML after nested leading comments" do
            normalizeCustomExecution
                " /* outer /* inner */ comment */ CREATE TABLE todos (id bigint); "
                `shouldBe`
                    Right
                        "/* outer /* inner */ comment */ CREATE TABLE todos (id bigint)"

        it "accepts batches and ignores semicolons inside PostgreSQL quotes" do
            normalizeCustomExecution
                "CREATE FUNCTION note() RETURNS text LANGUAGE sql \
                \AS $body$ SELECT ';'::text $body$; \
                \INSERT INTO todos (title) VALUES (E'one\\'s; todo');"
                `shouldBe`
                    Right
                        ( "CREATE FUNCTION note() RETURNS text LANGUAGE sql "
                            <> "AS $body$ SELECT ';'::text $body$; "
                            <> "INSERT INTO todos (title) VALUES (E'one\\'s; todo')"
                        )

        it "rejects transaction and session control" do
            normalizeCustomExecution "COMMIT" `shouldBe`
                Left
                    "database execution only accepts DDL/DML statements; \
                    \transaction and session control are not allowed"
            normalizeCustomExecution "-- no\n SET statement_timeout = 0"
                `shouldBe`
                    Left
                        "database execution only accepts DDL/DML statements; \
                        \transaction and session control are not allowed"

        it "rejects transaction control hidden after an allowed statement" do
            normalizeCustomExecution
                "CREATE TABLE todos (id bigint); /* nope */ COMMIT"
                `shouldBe`
                    Left
                        "database execution only accepts DDL/DML statements; \
                        \transaction and session control are not allowed"
