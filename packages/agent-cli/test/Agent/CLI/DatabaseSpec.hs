module Agent.CLI.DatabaseSpec (spec) where

import Agent.CLI.Database
import Agent.CLI.Database.Storage
import Agent.CLI.Database.Store
    ( DatabaseBrowsePage(..)
    , applicableDatabaseScopes
    , databaseToolsEnvForStore
    , deriveDatabaseScopes
    , listDatabaseObjects
    , loadDatabaseRows
    , scopeForDatabase
    )
import Agent.CLI.Options (StorageCommand(..))
import Agent.Store.Postgres
    ( ManagedPostgresConfig(..)
    , ManagedPostgresPaths(..)
    , Store
    , closeStore
    , defaultManagedPostgresConfig
    , openStore
    , provisioningPool
    , scopePool
    , trustedPool
    )
import Agent.Store.Postgres.Connection (StorePool, storePool)
import Agent.Store.Postgres.Custom
    ( CustomAuditContext(..)
    , CatalogColumn(..)
    , CatalogDefinition(..)
    , CatalogObject(..)
    , defaultQueryLimits
    , executeCustom
    )
import Agent.Store.Postgres.Managed (stopManagedPostgres)
import Agent.Store.Postgres.Scope
    ( ScopeDatabase(..)
    , lookupScopeDatabase
    , provisionScope
    )
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , ToolDispatchConfig(..)
    , functionToolCall
    )
import Agent.Tools.Types
    ( AppTool(..)
    , appToolHandlers
    )
import Agent.ToolDispatch (dispatchToolCall)
import Control.Exception.Safe (displayException, finally, onException)
import Data.Aeson ((.=), object)
import qualified Data.Aeson as Aeson
import Data.IORef
import Data.Either (isLeft)
import Agent.Json.Decode qualified as Hermes
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory
    ( createDirectory
    , getCurrentDirectory
    , getTemporaryDirectory
    , removePathForcibly
    )
import System.FilePath ((</>))
import System.Posix.Process (getProcessID)
import Test.Hspec

spec :: Spec
spec = do
    describe "custom database scopes" do
        it "decodes the existing custom wire strings" do
            Hermes.decodeEither customDatabaseScopeDecoder "\"user\""
                `shouldBe` Right DatabaseUserScope
            Hermes.decodeEither customDatabaseScopeDecoder "\"repository\""
                `shouldBe` Right DatabaseRepositoryScope
            Hermes.decodeEither customDatabaseScopeDecoder "\"checkout\""
                `shouldBe` Right DatabaseCheckoutScope

        it "cannot decode the runtime catalog as custom data" do
            Hermes.decodeEither customDatabaseScopeDecoder "\"harness\""
                `shouldSatisfy` isLeft

    describe "databaseTools" do
        mapM_ (\(wireScope, selected) ->
            it ("dispatches custom mutations for " <> Text.unpack wireScope) do
                seen <- newIORef Nothing
                let env = testEnv
                        { databaseRunExecute = \scope purpose sql -> do
                            writeIORef seen (Just (scope, purpose, sql))
                            pure (Right "ok")
                        }
                _ <- dispatchToolCall dispatchConfig
                    (appToolHandlers (databaseTools env))
                    (functionToolCall "custom-execute" "database_execute"
                        ("{\"scope\":\"" <> wireScope
                            <> "\",\"sql\":\"select 1\",\"purpose\":\"test\"}"))
                readIORef seen `shouldReturn`
                    Just (selected, "test", "select 1"))
            [ ("user", DatabaseUserScope)
            , ("repository", DatabaseRepositoryScope)
            , ("checkout", DatabaseCheckoutScope)
            ]

        it "dispatches a read-only query to the selected scope" do
            seen <- newIORef Nothing
            let env = testEnv
                    { databaseRunQuery = \scope sql -> do
                        writeIORef seen (Just (scope, sql))
                        pure (Right "row 1:\n  title: test\n\ntruncated: no")
                    }
            result <- dispatchToolCall dispatchConfig
                (appToolHandlers (databaseTools env))
                (functionToolCall "call-1" "database_query"
                    "{\"scope\":\"user\",\"sql\":\"select * from todos\"}")
            readIORef seen `shouldReturn`
                Just (DatabaseCustomScope DatabaseUserScope, "select * from todos")
            result.output `shouldContainText` "title: test"

        it "rejects empty mutating SQL before calling storage" do
            called <- newIORef False
            let env = testEnv
                    { databaseRunExecute = \_ _ _ -> do
                        writeIORef called True
                        pure (Right "ok")
                    }
            result <- dispatchToolCall dispatchConfig
                (appToolHandlers (databaseTools env))
                (functionToolCall "call-2" "database_execute"
                    "{\"scope\":\"repository\",\"sql\":\"  \",\"purpose\":\"todos\"}")
            readIORef called `shouldReturn` False
            result.output `shouldContainText` "must not be empty"

        it "rejects unknown scopes during argument decoding" do
            result <- dispatchToolCall dispatchConfig
                (appToolHandlers (databaseTools testEnv))
                (functionToolCall "call-3" "database_schema"
                    "{\"scope\":\"organization\"}")
            result.output `shouldContainText` "expected user, repository, or checkout"

        it "omits the harness catalog unless the embedding enables it" do
            result <- dispatchToolCall dispatchConfig
                (appToolHandlers (databaseTools testEnv))
                (functionToolCall "call-6" "database_schema"
                    "{\"scope\":\"harness\"}")
            result.output `shouldContainText`
                "the harness catalog is not available in this session"
            schemaToolDescriptionFromEnv testEnv
                `shouldContainText` "harness-internal schemas are never exposed."

        it "exposes the harness catalog and session key when enabled" do
            seen <- newIORef Nothing
            let env = testEnv
                    { databaseDescribeScope = \scope -> do
                        writeIORef seen (Just scope)
                        pure (Right "table sessions")
                    , databaseHarnessCatalogEnabled = True
                    , databaseHarnessSessionId = Just "2026-09-10-abc"
                    }
            result <- dispatchToolCall dispatchConfig
                (appToolHandlers (databaseTools env))
                (functionToolCall "call-7" "database_schema"
                    "{\"scope\":\"harness\"}")
            readIORef seen `shouldReturn` Just DatabaseHarnessScope
            result.output `shouldContainText` "table sessions"
            queryToolDescription env
                `shouldContainText` "This session's key is `2026-09-10-abc`."
            queryToolDescription env
                `shouldContainText` "Compaction replaces model context only"

        it "rejects mutating SQL against the harness catalog" do
            called <- newIORef False
            let env = testEnv
                    { databaseRunExecute = \_ _ _ -> do
                        writeIORef called True
                        pure (Right "ok")
                    , databaseHarnessCatalogEnabled = True
                    }
            result <- dispatchToolCall dispatchConfig
                (appToolHandlers (databaseTools env))
                (functionToolCall "call-8" "database_execute"
                    "{\"scope\":\"harness\",\"sql\":\"delete from sessions\",\"purpose\":\"wipe\"}")
            readIORef called `shouldReturn` False
            result.output `shouldContainText` "the harness catalog is read-only"

        it "dispatches full-text search over past conversations" do
            seen <- newIORef Nothing
            let env = testEnv
                    { databaseSearchConversations = \query limit -> do
                        writeIORef seen (Just (query, limit))
                        pure (Right
                            [ ConversationSearchMatch "session-1" 4
                                (Just "2026-08-24T09:30:00Z")
                                "How should we store this?"
                                (Just "Use PostgreSQL.\nKeep scopes explicit.")
                            ])
                    }
            result <- dispatchToolCall dispatchConfig
                (appToolHandlers (databaseTools env))
                (functionToolCall "call-4" "conversation_search"
                    "{\"query\":\"postgres memory\",\"limit\":5}")
            readIORef seen `shouldReturn` Just ("postgres memory", 5)
            result.output `shouldBe`
                "Match 1\n\
                \Session: session-1\n\
                \Turn: 4\n\
                \Occurred at: 2026-08-24T09:30:00Z\n\
                \User:\n\
                \  How should we store this?\n\
                \Assistant:\n\
                \  Use PostgreSQL.\n\
                \  Keep scopes explicit."

        it "renders an empty conversation search result as text" do
            let env = testEnv
                    { databaseSearchConversations = \_ _ ->
                        pure (Right [])
                    }
            result <- dispatchToolCall dispatchConfig
                (appToolHandlers (databaseTools env))
                (functionToolCall "call-5" "conversation_search"
                    "{\"query\":\"nothing\"}")
            result.output `shouldBe` "(no matching conversations)"

    describe "native data browser store" do
        it "does not provision on browse and returns typed table rows" $
            withBrowserDirectories \stateDirectory socketDirectory -> do
                let
                    baseConfig =
                        defaultManagedPostgresConfig stateDirectory ""
                    config = baseConfig
                        { postgresPaths = baseConfig.postgresPaths
                            { postgresSocketDirectory = socketDirectory
                            }
                        }
                    cleanup = do
                        _ <- stopManagedPostgres config
                        pure ()
                (openStore config >>= \case
                    Left err ->
                        expectationFailure (
                            "could not open store: " <> show err
                        )
                    Right store ->
                        finally
                            (exerciseDataBrowser store stateDirectory)
                            (closeStore store)
                    ) `finally` cleanup

    describe "runStorageCommand" do
        it "dispatches each administrative command to its store action" do
            let env = StorageCommandEnv
                    { storageStatusAction = pure (Right "status")
                    , storageStartAction = pure (Right "start")
                    , storageStopAction = pure (Right "stop")
                    , storageMigrateAction = pure (Right "migrate")
                    , storageDoctorAction = pure (Right "doctor")
                    }
            runStorageCommand env StorageStatus `shouldReturn` Right "status"
            runStorageCommand env StorageStart `shouldReturn` Right "start"
            runStorageCommand env StorageStop `shouldReturn` Right "stop"
            runStorageCommand env StorageMigrate `shouldReturn` Right "migrate"
            runStorageCommand env StorageDoctor `shouldReturn` Right "doctor"

    describe "deriveDatabaseScopes" do
        it "maps every custom scope to its corresponding durable scope" do
            directory <- getCurrentDirectory
            deriveDatabaseScopes directory directory >>= \case
                Left err -> expectationFailure (Text.unpack err)
                Right scopes ->
                    map (scopeForDatabase scopes)
                        [ DatabaseUserScope
                        , DatabaseRepositoryScope
                        , DatabaseCheckoutScope
                        ]
                        `shouldBe` applicableDatabaseScopes scopes

        it "is stable for one state directory and checkout" do
            first <- deriveDatabaseScopes
                "/tmp/haskell-agent-state"
                "/tmp/haskell-agent-checkout"
            second <- deriveDatabaseScopes
                "/tmp/haskell-agent-state"
                "/tmp/haskell-agent-checkout"
            first `shouldBe` second

        it "isolates different checkouts" do
            first <- deriveDatabaseScopes
                "/tmp/haskell-agent-state"
                "/tmp/haskell-agent-checkout-a"
            second <- deriveDatabaseScopes
                "/tmp/haskell-agent-state"
                "/tmp/haskell-agent-checkout-b"
            first `shouldNotBe` second

withBrowserDirectories
    :: (FilePath -> FilePath -> IO value)
    -> IO value
withBrowserDirectories action = do
    currentDirectory <- getCurrentDirectory
    temporaryDirectory <- getTemporaryDirectory
    processId <- getProcessID
    let
        suffix = show processId
        stateDirectory = currentDirectory </> (".ha-db-" <> suffix)
        socketDirectory = temporaryDirectory </> ("s" <> suffix)
    createDirectory stateDirectory
    createDirectory socketDirectory
        `onException` removePathForcibly stateDirectory
    action stateDirectory socketDirectory
        `finally`
            (removePathForcibly stateDirectory
                `finally` removePathForcibly socketDirectory)

exerciseDataBrowser :: Store -> FilePath -> IO ()
exerciseDataBrowser store stateDirectory =
    deriveDatabaseScopes stateDirectory stateDirectory >>= \case
        Left err ->
            expectationFailure (
                "could not derive database scopes: " <> Text.unpack err
            )
        Right scopes -> do
            let
                scope =
                    scopeForDatabase scopes DatabaseRepositoryScope
                provisionPool = storePool (provisioningPool store)
            lookupScopeDatabase provisionPool scope
                `shouldReturn` Right Nothing
            listDatabaseObjects
                store scopes DatabaseRepositoryScope
                `shouldReturn` Right []
            lookupScopeDatabase provisionPool scope
                `shouldReturn` Right Nothing

            provisionScope provisionPool scope >>= \case
                Left err ->
                    expectationFailure (
                        "could not provision scope: " <> Text.unpack err
                    )
                Right database ->
                    scopePool store database.scopeDatabaseRole >>= \case
                        Left err ->
                            expectationFailure (
                                "could not open scope pool: " <> show err
                            )
                        Right scoped -> do
                            let
                                audit = CustomAuditContext
                                    { customAuditSessionId = Nothing
                                    , customAuditAgentId = Nothing
                                    }
                            seedBrowserTable store scoped database audit

                            objects <- listDatabaseObjects
                                store scopes DatabaseRepositoryScope
                            assertBrowserCatalog objects

                            loadDatabaseRows
                                store
                                scopes
                                DatabaseRepositoryScope
                                "notes"
                                0
                                500
                                >>= assertDataPage
                            loadDatabaseRows
                                store
                                scopes
                                DatabaseRepositoryScope
                                "notes"
                                (-1)
                                1
                                `shouldReturn`
                                    Left "data preview offset must not be negative"
                            loadDatabaseRows
                                store
                                scopes
                                DatabaseRepositoryScope
                                "notes"
                                1
                                1
                                >>= assertOffsetDataPage

                            let harnessEnv =
                                    databaseToolsEnvForStore
                                        store
                                        scopes
                                        (pure Nothing)
                                        Nothing
                                        True
                                        (Just "session-test")
                            schemaResult <- dispatchToolCall dispatchConfig
                                (appToolHandlers (databaseTools harnessEnv))
                                (functionToolCall "call-9" "database_schema"
                                    "{\"scope\":\"harness\"}")
                            schemaResult.output `shouldContainText` "sessions"
                            queryResult <- dispatchToolCall dispatchConfig
                                (appToolHandlers (databaseTools harnessEnv))
                                (functionToolCall "call-10" "database_query"
                                    "{\"scope\":\"harness\",\"sql\":\"select count(*)::bigint as session_count from sessions\"}")
                            queryResult.output `shouldContainText` "session_count:"

seedBrowserTable
    :: Store
    -> StorePool
    -> ScopeDatabase
    -> CustomAuditContext
    -> IO ()
seedBrowserTable store scoped database audit =
    executeCustom
        (storePool (trustedPool store))
        (storePool scoped)
        database
        audit
        defaultQueryLimits
        "test native data browser"
        "CREATE TABLE notes (\
        \ id bigint PRIMARY KEY,\
        \ title text NOT NULL,\
        \ done boolean NOT NULL,\
        \ metadata jsonb);\
        \ INSERT INTO notes VALUES\
        \ (1, 'first', true, '{\"priority\":2}'::jsonb),\
        \ (2, 'second', false, '{\"priority\":1}'::jsonb)"
        >>= \case
            Left err ->
                expectationFailure (
                    "could not seed browser table: " <> Text.unpack err
                )
            Right _ -> pure ()

assertBrowserCatalog :: Either Text [CatalogObject] -> IO ()
assertBrowserCatalog = \case
    Left err ->
        expectationFailure (
            "could not list browser tables: " <> Text.unpack err
        )
    Right [object] -> do
        object.catalogObjectName `shouldBe` "notes"
        map (.columnName)
            object.catalogObjectDefinition.definitionColumns
            `shouldBe` ["id", "title", "done", "metadata"]
    Right unexpected ->
        expectationFailure (
            "unexpected browser catalog: " <> show unexpected
        )

assertDataPage :: Either Text DatabaseBrowsePage -> IO ()
assertDataPage = \case
    Left err ->
        expectationFailure (
            "could not load browser rows: " <> Text.unpack err
        )
    Right page -> do
        page.databaseBrowseHasMore `shouldBe` False
        page.databaseBrowseRows `shouldBe`
            [ [ Aeson.Number 1
              , Aeson.String "first"
              , Aeson.Bool True
              , object ["priority" .= (2 :: Int)]
              ]
            , [ Aeson.Number 2
              , Aeson.String "second"
              , Aeson.Bool False
              , object ["priority" .= (1 :: Int)]
              ]
            ]

assertOffsetDataPage :: Either Text DatabaseBrowsePage -> IO ()
assertOffsetDataPage = \case
    Left err ->
        expectationFailure (
            "could not load offset browser rows: " <> Text.unpack err
        )
    Right page -> do
        page.databaseBrowseHasMore `shouldBe` False
        page.databaseBrowseRows `shouldBe`
            [ [ Aeson.Number 2
              , Aeson.String "second"
              , Aeson.Bool False
              , object ["priority" .= (1 :: Int)]
              ]
            ]

testEnv :: DatabaseToolsEnv
testEnv = DatabaseToolsEnv
    { databaseDescribeScope = \_ -> pure (Right "ok")
    , databaseRunQuery = \_ _ -> pure (Right "(no rows)")
    , databaseRunExecute = \_ _ _ -> pure (Right "ok")
    , databaseSearchConversations = \_ _ ->
        pure (Right [])
    , databaseHarnessCatalogEnabled = False
    , databaseHarnessSessionId = Nothing
    }

queryToolDescription :: DatabaseToolsEnv -> Text
queryToolDescription = toolDescription "database_query"

schemaToolDescriptionFromEnv :: DatabaseToolsEnv -> Text
schemaToolDescriptionFromEnv = toolDescription "database_schema"

toolDescription :: Text -> DatabaseToolsEnv -> Text
toolDescription name env =
    case [tool.appToolDescription | tool <- databaseTools env, tool.appToolName == name] of
        description : _ -> description
        [] -> ""

dispatchConfig :: ToolDispatchConfig
dispatchConfig = ToolDispatchConfig
    { toolDispatchUnknownTool = \name -> "unknown: " <> name
    , toolDispatchFormatResult = either ("error: " <>) id
    , toolDispatchFormatException = \name exception ->
        name <> ": " <> Text.pack (displayException exception)
    , toolDispatchOnException = \_ _ -> pure ()
    , toolDispatchOnOutput = \_ _ -> pure ()
    , toolDispatchFinalizeOutput = \_call output -> pure output
    }

shouldContainText :: Text -> Text -> Expectation
shouldContainText actual expected =
    actual `shouldSatisfy` Text.isInfixOf expected
