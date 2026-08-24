{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Agent.Store.Postgres.CustomSpec (spec) where

import Control.Exception.Safe (finally)
import qualified Data.Text as Text
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Agent.Store.Postgres
    ( closeStore
    , defaultManagedPostgresConfig
    , openStore
    , provisioningPool
    , scopePool
    , trustedPool
    )
import Agent.Store.Postgres.Connection (storePool)
import Agent.Store.Postgres.Custom
import Agent.Store.Postgres.Managed (stopManagedPostgres)
import Agent.Store.Postgres.Scope
    ( Scope(..)
    , ScopeDatabase(..)
    , ScopeKind(..)
    , mkScopeId
    , provisionScope
    )

spec :: Spec
spec = describe "custom PostgreSQL SQL normalization" do
    it "keeps successful execution results as typed storage records" do
        CustomExecutionResult
            { customExecutionAuditId = "0198d1ac-7b48-7000-8000-000000000000"
            , customExecutionCatalogBefore = []
            , customExecutionCatalogAfter = []
            , customExecutionWarning = Nothing
            }
            `shouldBe` CustomExecutionResult
                { customExecutionAuditId =
                    "0198d1ac-7b48-7000-8000-000000000000"
                , customExecutionCatalogBefore = []
                , customExecutionCatalogAfter = []
                , customExecutionWarning = Nothing
                }

    it "returns typed catalogs and JSON text only at the query boundary" $
        withSystemTempDirectory "ha" \stateDirectory -> do
            let
                config = defaultManagedPostgresConfig stateDirectory ""
                cleanup = do
                    _ <- stopManagedPostgres config
                    pure ()
            (openStore config >>= \case
                Left err -> expectationFailure ("could not open store: " <> show err)
                Right store ->
                    finally
                        (do
                            scopeId <- mkScopeId
                                "0123456789abcdef0123456789abcdef"
                                `shouldSatisfyRight` const True
                            let scope = Scope RepositoryScope scopeId
                            provisionScope
                                (storePool (provisioningPool store))
                                scope >>= \case
                                    Left err ->
                                        expectationFailure
                                            ("could not provision scope: "
                                                <> Text.unpack err)
                                    Right database ->
                                        scopePool
                                            store
                                            database.scopeDatabaseRole
                                            >>= \case
                                                Left err ->
                                                    expectationFailure
                                                        ("could not open scope pool: "
                                                            <> show err)
                                                Right scoped -> do
                                                    let rawScope = storePool scoped
                                                    execution <- executeCustom
                                                        (storePool (trustedPool store))
                                                        rawScope
                                                        database
                                                        CustomAuditContext
                                                            { customAuditSessionId =
                                                                Nothing
                                                            , customAuditAgentId =
                                                                Nothing
                                                            }
                                                        defaultQueryLimits
                                                        "test typed catalog"
                                                        "CREATE TABLE todos (\
                                                        \ id bigint PRIMARY KEY,\
                                                        \ title text NOT NULL);\
                                                        \ INSERT INTO todos VALUES\
                                                        \ (1, 'one')"
                                                    _ <- execution
                                                        `shouldSatisfyRight`
                                                            (const True)
                                                    catalog <- inspectCustomSchema
                                                        rawScope
                                                        database
                                                    _ <- catalog
                                                        `shouldSatisfyRight`
                                                            (any
                                                                (\object ->
                                                                    object.catalogObjectKind
                                                                        == "table"
                                                                        && object.catalogObjectName
                                                                            == "todos"))
                                                    result <- queryCustom
                                                        rawScope
                                                        database
                                                        defaultQueryLimits
                                                        "SELECT id, title FROM todos"
                                                    _ <- result
                                                        `shouldSatisfyRight`
                                                            (\queryResult ->
                                                                not
                                                                    queryResult.customQueryTruncated
                                                                    && "\"title\": \"one\""
                                                                        `Text.isInfixOf`
                                                                            queryResult.customQueryRows)
                                                    pure ()
                        )
                        (closeStore store)
                ) `finally` cleanup

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

shouldSatisfyRight
    :: (Show left, Show right)
    => Either left right
    -> (right -> Bool)
    -> IO right
shouldSatisfyRight value predicate =
    case value of
        Left err ->
            expectationFailure ("expected Right, got Left " <> show err)
                >> fail "unreachable"
        Right result -> do
            result `shouldSatisfy` predicate
            pure result
