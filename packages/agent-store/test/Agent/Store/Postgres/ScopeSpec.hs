{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Agent.Store.Postgres.ScopeSpec (spec) where

import qualified Data.ByteString.Char8 as ByteString
import Test.Hspec

import Agent.Store.Postgres.Scope

spec :: Spec
spec = describe "custom PostgreSQL scopes" do
    it "normalizes UUID identifiers before deriving safe database names" do
        let parsed =
                mkScopeId "01234567-89AB-CDEF-0123-456789ABCDEF"
        scopeId <- parsed `shouldSatisfyRight` const True
        let database = scopeDatabaseFor Scope
                { scopeKind = RepositoryScope
                , scopeId = scopeId
                }
        scopeIdText scopeId
            `shouldBe` "0123456789abcdef0123456789abcdef"
        database.scopeDatabaseRole
            `shouldBe` "ha_scope_r_0123456789abcdef0123456789abcdef"
        database.scopeDatabaseSchema
            `shouldBe` "custom_r_0123456789abcdef0123456789abcdef"

    it "uses distinct names for user, repository, and checkout scopes" do
        scopeId <- mkScopeId "0123456789abcdef0123456789abcdef"
            `shouldSatisfyRight` const True
        let databaseNames kind =
                let database = scopeDatabaseFor Scope
                        { scopeKind = kind
                        , scopeId = scopeId
                        }
                in (database.scopeDatabaseRole, database.scopeDatabaseSchema)
        databaseNames <$> [UserScope, RepositoryScope, CheckoutScope]
            `shouldMatchList`
                [ ( "ha_scope_u_0123456789abcdef0123456789abcdef"
                  , "custom_u_0123456789abcdef0123456789abcdef"
                  )
                , ( "ha_scope_r_0123456789abcdef0123456789abcdef"
                  , "custom_r_0123456789abcdef0123456789abcdef"
                  )
                , ( "ha_scope_c_0123456789abcdef0123456789abcdef"
                  , "custom_c_0123456789abcdef0123456789abcdef"
                  )
                ]

    it "rejects identifiers outside the UUID-shaped safe alphabet" do
        mkScopeId "not-a-scope" `shouldBe`
            Left "scope id must be a UUID (32 hex digits, with optional hyphens)"
        mkScopeId "01234567-89abcdef0123-4567-89abcdef" `shouldBe`
            Left "scope id must be a UUID (32 hex digits, with optional hyphens)"

    it "migrates a durable started-to-final audit lifecycle" do
        let migrationSql = ByteString.unlines customSchemaStatements
        migrationSql `shouldSatisfy`
            ByteString.isInfixOf
                "scope_id uuid PRIMARY KEY DEFAULT pg_catalog.uuidv7()"
        migrationSql `shouldSatisfy`
            ByteString.isInfixOf
                "audit_id uuid PRIMARY KEY DEFAULT pg_catalog.uuidv7()"
        migrationSql `shouldSatisfy`
            (not . ByteString.isInfixOf " jsonb")
        migrationSql `shouldSatisfy`
            ByteString.isInfixOf
                "status text NOT NULL DEFAULT 'started'"
        migrationSql `shouldSatisfy`
            ByteString.isInfixOf
                "finished_at timestamptz,"
        migrationSql `shouldSatisfy`
            ByteString.isInfixOf
                "custom SQL audit rows can only finalize once"

shouldSatisfyRight
    :: (Show left, Show right)
    => Either left right
    -> (right -> Bool)
    -> IO right
shouldSatisfyRight value predicate =
    case value of
        Left err -> expectationFailure ("expected Right, got Left " <> show err)
            >> fail "unreachable"
        Right result -> do
            result `shouldSatisfy` predicate
            pure result
